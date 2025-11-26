from __future__ import annotations

from datetime import datetime, timezone
import csv
import json
from pathlib import Path

from fastapi import APIRouter, HTTPException
from fastapi.responses import FileResponse
from pydantic import BaseModel, Field

import config
from Files.dataset_manager import dataset_path
from Metrics.students import load_students_outliers
from Routers.utils import ensure_dataset_ready, resolve_dataset_or_http_error, resolve_relative_file

router = APIRouter(prefix="/reports", tags=["reports"])
REPORTS_SUBDIR = config.REPORTS_SUBDIR
STUDENTS_REPORT_FILENAME = config.STUDENTS_REPORT_FILENAME


def _students_report_path(dataset_name: str) -> Path:
    """
    @brief Compute the path to the students outliers report for a dataset.
    @param dataset_name Dataset name.
    @return Filesystem path to the report.
    """
    return dataset_path(dataset_name) / REPORTS_SUBDIR / STUDENTS_REPORT_FILENAME


def _latest_clustering_artifacts(dataset_name: str) -> tuple[Path | None, Path | None]:
    """
    @brief Locate the most recent clustering meta and csv cache (any theme).
    """
    root = dataset_path(dataset_name)
    metas = sorted(root.glob(f"{config.CLUSTERING_META_FILENAME.replace('.json', '')}*.json"), key=lambda p: p.stat().st_mtime)
    if not metas:
        return None, None
    meta_path = metas[-1]
    suffix = meta_path.stem.replace(config.CLUSTERING_META_FILENAME.replace(".json", ""), "")
    suffix = suffix if not suffix else suffix  # keep as-is (e.g., _theme)
    csv_name = config.CLUSTERING_CACHE_FILENAME
    if suffix:
        csv_name = config.CLUSTERING_CACHE_FILENAME.replace(".csv", f"{suffix}.csv")
    csv_path = root / csv_name
    return meta_path, csv_path if csv_path.exists() else (meta_path, None)[1]


def _load_clustering_snapshot(meta_path: Path | None, csv_path: Path | None) -> tuple[list[dict[str, object]], list[dict[str, object]]]:
    clusters: list[dict[str, object]] = []
    points: list[dict[str, object]] = []
    if meta_path and meta_path.exists():
        try:
            payload = json.loads(meta_path.read_text(encoding="utf-8"))
            clusters = payload.get("clusters") or []
        except Exception:
            clusters = []
    if csv_path and csv_path.exists():
        try:
            with csv_path.open("r", encoding="utf-8") as handle:
                reader = csv.DictReader(handle)
                for row in reader:
                    try:
                        cluster = int(row.get("cluster") or -1)
                    except ValueError:
                        cluster = -1
                    probs_raw = (row.get("probabilities") or "").strip()
                    probabilities = None
                    if probs_raw:
                        try:
                            probabilities = [float(val) for val in probs_raw.split(";") if val.strip()]
                        except ValueError:
                            probabilities = None
                    points.append(
                        {
                            "path": (row.get("path") or "").replace("\\", "/"),
                            "cluster": cluster,
                            "probabilities": probabilities,
                        }
                    )
        except Exception:
            points = []
    return clusters, points


def _build_membership_lookup(clusters: list[dict[str, object]], points: list[dict[str, object]]) -> dict[str, tuple[str, float]]:
    """
    @brief Build a mapping from file path to (cluster label, confidence).
    """
    label_lookup = {c.get("id"): c.get("label") for c in clusters if isinstance(c, dict) and c.get("id") is not None}
    lookup: dict[str, tuple[str, float]] = {}
    for point in points:
        path = (point.get("path") or "").replace("\\", "/")
        if not path:
            continue
        cluster_id = point.get("cluster")
        if not isinstance(cluster_id, int) or cluster_id < 0:
            continue
        probs = point.get("probabilities")
        confidence = 1.0
        if isinstance(probs, list) and len(probs) > cluster_id:
            try:
                confidence = float(probs[cluster_id])
            except (TypeError, ValueError):
                confidence = 1.0
        label = label_lookup.get(cluster_id) or f"Cluster {cluster_id + 1}"
        lookup[path] = (label, confidence)
    return lookup


def _top_members(points: list[dict[str, object]], cluster_id: int, limit: int = 5) -> list[tuple[str, float]]:
    relevant = [p for p in points if int(p.get("cluster") or -1) == cluster_id]
    scored: list[tuple[str, float]] = []
    for entry in relevant:
        probs = entry.get("probabilities")
        confidence = 1.0
        if isinstance(probs, list) and len(probs) > cluster_id:
            try:
                confidence = float(probs[cluster_id])
            except (TypeError, ValueError):
                confidence = 1.0
        scored.append((entry.get("path") or "-", confidence))
    scored.sort(key=lambda x: x[1], reverse=True)
    return scored[:limit]


def _write_students_report(dataset_name: str, outliers: dict[str, object]) -> tuple[Path, int, bool]:
    """
    @brief Render the students outliers markdown report to disk.
    @param dataset_name Dataset name.
    @param outliers Outliers payload previously computed.
    @return Tuple of (report_path, highlighted_metrics_count, has_combined_findings).
    """
    buckets = outliers.get("buckets") or []
    if not isinstance(buckets, list):
        buckets = []

    report_path = _students_report_path(dataset_name)
    report_path.parent.mkdir(parents=True, exist_ok=True)
    generated_at = datetime.now(timezone.utc).strftime("%Y-%m-%d %H:%M UTC")

    def _fmt_value(raw: object) -> str:
        try:
            number = float(raw)  # type: ignore[arg-type]
        except (TypeError, ValueError):
            return str(raw)
        return f"{number:.3f}"

    def _metric_explanation(key: object) -> str | None:
        if not isinstance(key, str):
            return None
        lowered = key.lower()
        if "if/ncss" in lowered or ("if" in lowered and "ncss" in lowered):
            return "High If/NCSS means many conditionals relative to file size—logic may be overly branched; simplify or merge predicates."
        if "loops/ncss" in lowered or ("loop" in lowered and "ncss" in lowered):
            return "High Loops/NCSS indicates many iterations per line of code—consider reducing loop count or extracting helpers."
        if "vars/functions" in lowered:
            return "Vars/Functions captures variable churn per function—high values hint at long functions or heavy state; split or reduce state."
        if "vars/ncss" in lowered:
            return "Vars/NCSS reflects variable density—high density can reduce readability; very low density may mean under-documented or terse code."
        if "total variables" in lowered or lowered == "variables":
            return "Total variables shows how much state the file manages; very high counts can signal complex or sprawling state handling."
        if "ncss" in lowered or "loc" in lowered or "lines" in lowered:
            return '''
            High NCSS/LOC suggests large files that may use decomposition into smaller units.
            Low NCSS/LOC could indicate better modularization; very low values might signal incomplete files.
            '''
        if "duplication" in lowered:
            return "High duplication indicates copy/paste which lead to redundant code and maintenance issues. Consider extracting shared functions instead of copy/pasting or review the logic"
        if "ccn" in lowered or "complex" in lowered:
            return "High cyclomatic complexity indicates complex logical sturcture, usually with lots of branches. Simplify logic and split the code into smaller reusable portions or functions"
        if "nest" in lowered:
            return "Deep nesting often signals hard-to-read code with lots of imbricated logic. Consider trying to flatten the structure by returning early or splitting logic."
        if "function" in lowered:
            return "High number of functions may indicate good modularization, while very high values could suggest over-fragmentation. Low function counts usually indicate monolithic code that would benefit from decomposition."
        return None

    lines: list[str] = []
    lines.append(f"# Dataset Report: {dataset_name}")
    lines.append("")
    lines.append(f"_Generated on {generated_at}_")
    lines.append("")
    lines.append(
        "This report highlights files whose metrics fall outside the interquartile range "
        "(very high: above Q3 + 1.5×IQR, high: above Q3, low: below Q1, very low: below Q1 – 1.5×IQR)."
    )
    lines.append("")

    meta_path, csv_path = _latest_clustering_artifacts(dataset_name)
    clusters, points = _load_clustering_snapshot(meta_path, csv_path)
    membership_lookup = _build_membership_lookup(clusters, points) if clusters and points else {}

    def _format_cluster_tag(filename: str) -> str:
        entry = membership_lookup.get(filename)
        if not entry:
            return ""
        label, confidence = entry
        return f" _(Aligned with {label}, confidence {confidence:.2f})_"

    highlighted_metrics = 0
    for bucket in buckets:
        # Buckets represent one metric with its outlier groups.
        groups = bucket.get("groups") or {}
        above_fence = groups.get("aboveFence") or []
        above_q3 = groups.get("aboveQ3") or []
        below_q1 = groups.get("belowQ1") or []
        below_fence = groups.get("belowFence") or []
        if not any([above_fence, above_q3, below_q1, below_fence]):
            continue

        highlighted_metrics += 1
        label = bucket.get("label") or bucket.get("key") or "Metric"
        lines.append(f"## {label}")
        lines.append("")
        q1 = bucket.get("q1")
        q3 = bucket.get("q3")
        median = bucket.get("median")
        lines.append(f"- Q1: {_fmt_value(q1)}")
        lines.append(f"- Median: {_fmt_value(median)}")
        lines.append(f"- Q3: {_fmt_value(q3)}")
        lines.append("")
        explanation = _metric_explanation(bucket.get("key"))
        if explanation:
            lines.append(f"Interpretation: {explanation}")
            lines.append("")

        def _write_group(title: str, entries: list[dict[str, object]], description: str) -> None:
            if not entries:
                return
            lines.append(f"### {title}")
            if description:
                lines.append(description)
            for entry in entries:
                filename = entry.get("filename") or entry.get("path") or "Unknown file"
                value = _fmt_value(entry.get("value"))
                lines.append(f"- `{filename}` — {value}{_format_cluster_tag(filename)}")
            lines.append("")

        _write_group("Very high (above Q3 + 1.5×IQR)", above_fence, "Prioritize investigating these files.")
        _write_group("High (above Q3)", above_q3, "")
        _write_group("Low (below Q1)", below_q1, "")
        _write_group("Very low (below Q1 − 1.5×IQR)", below_fence, "Check for incomplete or stub files.")

    files_payload = outliers.get("files") or []
    combined_findings: list[str] = []

    def _is_high_metric(entry: dict[str, object]) -> bool:
        return bool(entry.get("high") or entry.get("extra_high"))

    def _is_low_metric(entry: dict[str, object]) -> bool:
        return bool(entry.get("below_q1") or entry.get("below_fence"))

    for file_entry in files_payload:
        metrics_data = file_entry.get("metrics") or {}
        if not isinstance(metrics_data, dict):
            continue
        # Aggregate repeated signals so the report can highlight stronger patterns.
        duplication_high = any(
            _is_high_metric(entry)
            for key, entry in metrics_data.items()
            if isinstance(entry, dict) and "duplication" in str(key).lower()
        )
        ncss_high = any(
            _is_high_metric(entry)
            for key, entry in metrics_data.items()
            if isinstance(entry, dict) and any(token in str(key).lower() for token in ("ncss", "loc", "lines"))
        )
        functions_low = any(
            _is_low_metric(entry)
            for key, entry in metrics_data.items()
            if isinstance(entry, dict) and "function" in str(key).lower()
        )
        complexity_high = any(
            _is_high_metric(entry)
            for key, entry in metrics_data.items()
            if isinstance(entry, dict) and ("ccn" in str(key).lower() or "complex" in str(key).lower())
        )
        nesting_high = any(
            _is_high_metric(entry)
            for key, entry in metrics_data.items()
            if isinstance(entry, dict) and "nest" in str(key).lower()
        )

        filename = file_entry.get("filename") or file_entry.get("path") or "Unknown file"

        if duplication_high and ncss_high:
            combined_findings.append(
                f"- Heavy duplication in a large file: `{filename}` — high NCSS with duplicated blocks suggests weak structure."
            )

        if duplication_high and (functions_low or ncss_high):
            reasons = []
            if duplication_high:
                reasons.append("high duplication")
            if ncss_high:
                reasons.append("high NCSS/LOC")
            if functions_low:
                reasons.append("low function count")
            joined = ", ".join(reasons) if reasons else "multiple signals"
            combined_findings.append(
                f"- Poor factoring: `{filename}` — {joined}. "
                "Refactor to extract shared helpers and reduce copy/paste."
            )

        if complexity_high and nesting_high:
            combined_findings.append(
                f"- Complex logic: `{filename}` — high cyclomatic complexity with deep nesting. "
                "Simplify branching and split logic into smaller units."
            )

    if combined_findings:
        lines.append("## Combined patterns")
        lines.append("")
        lines.append("Multi-metric signals for students/files needing attention:")
        lines.extend(combined_findings)
        lines.append("")

    if highlighted_metrics == 0 and not combined_findings:
        lines.append("No outlier students detected for the current dataset.")

    # Append clustering insights if available
    meta_path, csv_path = _latest_clustering_artifacts(dataset_name)
    clusters, points = _load_clustering_snapshot(meta_path, csv_path)
    if clusters:
        lines.append("")
        lines.append("## Clustering insights")
        lines.append("")
        lines.append(
            "Summaries below are generated from the latest clustering run (LLM-provided label/description) "
            "alongside a few representative files per cluster."
        )
        for cluster in clusters:
            cid = cluster.get("id") if isinstance(cluster, dict) else None
            label = cluster.get("label") if isinstance(cluster, dict) else None
            desc = cluster.get("description") if isinstance(cluster, dict) else None
            comparison = cluster.get("comparison") if isinstance(cluster, dict) else None
            size = cluster.get("size") if isinstance(cluster, dict) else None
            lines.append(f"### {label or f'Cluster {cid}'}")
            if size is not None:
                lines.append(f"- Size: {size}")
            if desc:
                lines.append("")
                lines.append(desc if isinstance(desc, str) else str(desc))
            if isinstance(comparison, list) and comparison:
                lines.append("")
                lines.append("| Metric | Cluster | Dataset | Relation |")
                lines.append("| --- | --- | --- | --- |")
                for row in comparison:
                    if not isinstance(row, dict):
                        continue
                    metric = row.get("metric") or "-"
                    cl = row.get("cluster") or "-"
                    ds = row.get("dataset") or "-"
                    rel = row.get("relation") or "-"
                    lines.append(f"| {metric} | {cl} | {ds} | {rel} |")
            top_members = _top_members(points, cid, limit=5) if cid is not None else []
            if top_members:
                lines.append("")
                lines.append("Top members (by confidence):")
                for path, score in top_members:
                    lines.append(f"- {path} ({score:.2f})")
            lines.append("")

    report_path.write_text("\n".join(lines).strip() + "\n", encoding="utf-8")
    return report_path, highlighted_metrics, bool(combined_findings)
class ReportRequest(BaseModel):
    """
    @brief Request body for dataset-level report generation.
    """
    dataset: str | None = Field(default=None, description="Dataset name, defaults to current selection.")


class FileReportRequest(ReportRequest):
    """
    @brief Request body for file-level report generation.
    """
    filename: str = Field(description="Relative filename inside the dataset raw folder.")


@router.post("/generate-report", name="generate-report")
async def generate_report(payload: ReportRequest) -> dict[str, object]:
    """
    @brief Trigger report generation for a dataset.
    @param payload Request body containing dataset selection.
    @return Response containing the path to the generated markdown report.
    """
    dataset_name, _ = resolve_dataset_or_http_error(payload.dataset, require_raw=True)
    ensure_dataset_ready(dataset_name)

    try:
        outliers = load_students_outliers(dataset_name)
    except RuntimeError as exc:
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=str(exc)) from exc

    try:
        report_path, highlighted_metrics, combined = _write_students_report(dataset_name, outliers)
    except OSError as exc:
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=f"Unable to write report: {exc}"
        ) from exc

    return {
        "dataset": dataset_name,
        "report_path": report_path.as_posix(),
        "highlighted_metrics": highlighted_metrics,
        "has_combined_findings": combined,
        "status": "generated",
    }


@router.post("/generate-file-report", name="generate-file-report")
async def generate_file_report(payload: FileReportRequest) -> dict[str, object]:
    """
    @brief Trigger report generation for a specific file within a dataset.
    @param payload Request body containing dataset selection and filename.
    @return Placeholder response echoing the validated dataset and file.
    """
    dataset_name, raw_dir = resolve_dataset_or_http_error(payload.dataset, require_raw=True)
    _, filename = resolve_relative_file(raw_dir, payload.filename)
    return {
        "dataset": dataset_name,
        "file": filename,
        "status": "accepted",
        "message": "File report generation endpoint stub.",
    }


@router.get("/download-report", name="download-report")
async def download_report(dataset: str | None = None):
    """
    @brief Download the latest students outliers report as a markdown file.
    @param dataset Optional dataset override; defaults to current selection.
    @return FileResponse streaming the markdown report.
    """
    dataset_name, _ = resolve_dataset_or_http_error(dataset, require_raw=True)
    ensure_dataset_ready(dataset_name)

    # Always regenerate to keep descriptions and findings fresh before download.
    try:
        outliers = load_students_outliers(dataset_name)
    except RuntimeError as exc:
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=str(exc)) from exc

    try:
        report_path, _, _ = _write_students_report(dataset_name, outliers)
    except OSError as exc:
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=f"Unable to write report: {exc}"
        ) from exc

    if not report_path.exists():
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Report not found.")

    filename = f"{dataset_name}_students_report.md"
    return FileResponse(report_path, media_type="text/markdown", filename=filename)
