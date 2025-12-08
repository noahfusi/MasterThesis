from __future__ import annotations

import csv
import json
from pathlib import Path
from datetime import datetime, timezone

from fastapi import APIRouter, HTTPException, status
from fastapi.responses import FileResponse
from pydantic import BaseModel, Field

import config
from Routers.clustering import THEMES
from Files.dataset_manager import dataset_path
from Metrics.students import load_students_outliers
from LLM import generate_completion
from Routers.utils import ensure_dataset_ready, resolve_dataset_or_http_error, resolve_relative_file

router = APIRouter(prefix="/reports", tags=["reports"])
REPORTS_SUBDIR = config.REPORTS_SUBDIR
STUDENTS_REPORT_FILENAME = config.STUDENTS_REPORT_FILENAME


def _students_report_path(dataset_name: str, generated_at: str | None = None) -> Path:
    """
    @brief Compute the path to the students outliers report for a dataset.
    @param dataset_name Dataset name.
    @param generated_at Optional timestamp token (safe for filenames).
    @return Filesystem path to the report.
    """
    token = generated_at or datetime.now(timezone.utc).strftime("%Y-%m-%dT%H-%M-%SZ")
    return dataset_path(dataset_name) / REPORTS_SUBDIR / f"{token}_{STUDENTS_REPORT_FILENAME}"


def _latest_report_path(dataset_name: str) -> tuple[Path | None, str | None]:
    """
    @brief Locate the most recent students report (timestamped).
    @return Tuple of (path, generated_at_token) or (None, None).
    """
    root = dataset_path(dataset_name) / REPORTS_SUBDIR
    if not root.exists():
        return None, None
    candidates = sorted(root.glob(f"*_{STUDENTS_REPORT_FILENAME}"), key=lambda p: p.stat().st_mtime)
    if not candidates:
        legacy = root / STUDENTS_REPORT_FILENAME
        if legacy.exists():
            return legacy, None
        return None, None
    latest = candidates[-1]
    # Extract timestamp token from filename prefix
    token = latest.stem.replace(f"_{STUDENTS_REPORT_FILENAME.replace('.md','')}", "")
    return latest, token if token else None


def _collect_clustering_runs(dataset_name: str) -> list[dict[str, object]]:
    """
    @brief Load all clustering runs (per theme) with clusters and points.
    """
    root = dataset_path(dataset_name)
    metas = sorted(root.glob(f"{config.CLUSTERING_META_FILENAME.replace('.json', '')}*.json"))
    runs: list[dict[str, object]] = []

    def _load_points(csv_path: Path) -> list[dict[str, object]]:
        pts: list[dict[str, object]] = []
        if not csv_path.exists():
            return pts
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
                    pts.append(
                        {
                            "path": (row.get("path") or "").replace("\\", "/"),
                            "cluster": cluster,
                            "probabilities": probabilities,
                        }
                    )
        except Exception:
            return []
        return pts

    for meta_path in metas:
        csv_name = config.CLUSTERING_CACHE_FILENAME
        suffix = meta_path.stem.replace(config.CLUSTERING_META_FILENAME.replace(".json", ""), "")
        if suffix:
            csv_name = config.CLUSTERING_CACHE_FILENAME.replace(".csv", f"{suffix}.csv")
        csv_path = root / csv_name

        clusters: list[dict[str, object]] = []
        metadata: dict[str, object] = {}
        try:
            payload = json.loads(meta_path.read_text(encoding="utf-8"))
            clusters = payload.get("clusters") or []
            if isinstance(payload.get("metadata"), dict):
                metadata = payload["metadata"]
        except Exception:
            clusters = []

        theme = (metadata.get("theme") if isinstance(metadata, dict) else None) or None
        theme_label = (
            (metadata.get("theme_label") if isinstance(metadata, dict) else None)
            or (THEMES.get(theme, {}) if theme else {}).get("label")
            or theme
        )
        runs.append(
            {
                "theme": theme,
                "theme_label": theme_label,
                "clusters": clusters,
                "points": _load_points(csv_path),
            }
        )

    return runs


def _build_membership_lookup(runs: list[dict[str, object]]) -> dict[str, list[tuple[str, float]]]:
    """
    @brief Build a mapping from file path to list of (theme/cluster label, confidence).
    """
    lookup: dict[str, list[tuple[str, float]]] = {}
    for run in runs:
        clusters = run.get("clusters") or []
        points = run.get("points") or []
        theme_label = run.get("theme_label") or run.get("theme")
        label_lookup = {
            c.get("id"): c.get("label") for c in clusters if isinstance(c, dict) and c.get("id") is not None
        }
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
            full_label = f"{theme_label or 'Theme'}: {label}"
            lookup.setdefault(path, []).append((full_label, confidence))
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


def _write_students_report(dataset_name: str, outliers: dict[str, object]) -> tuple[Path, int, bool, str]:
    """
    @brief Render the students outliers markdown report to disk.
    @param dataset_name Dataset name.
    @param outliers Outliers payload previously computed.
    @return Tuple of (report_path, highlighted_metrics_count, has_combined_findings, generated_at_token).
    """
    buckets = outliers.get("buckets") or []
    if not isinstance(buckets, list):
        buckets = []

    generated_token = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H-%M-%SZ")
    report_path = _students_report_path(dataset_name, generated_token)
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
        for pattern, text in config.STUDENTS_METRIC_EXPLANATIONS:
            if pattern in lowered:
                return text
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

    clustering_runs = _collect_clustering_runs(dataset_name)
    membership_lookup = _build_membership_lookup(clustering_runs) if clustering_runs else {}

    def _format_cluster_tag(filename: str) -> str:
        entries = membership_lookup.get(filename) or []
        if not entries:
            return ""
        sorted_entries = sorted(entries, key=lambda x: x[1], reverse=True)
        parts = [f"{label} ({confidence:.2f})" for label, confidence in sorted_entries[:3]]
        return f" _(Aligned with {', '.join(parts)})_"

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

    SIGNAL_SCORES = {
        "really_low": -3,
        "low": -2,
        "normal_low": -1,
        "normal_high": 1,
        "high": 2,
        "really_high": 3,
    }

    def _score_signal(entry: dict[str, object]) -> int:
        signal = entry.get("signal")
        if isinstance(signal, str) and signal in SIGNAL_SCORES:
            return SIGNAL_SCORES[signal]
        # Fallback to legacy flags
        if entry.get("below_fence"):
            return SIGNAL_SCORES["really_low"]
        if entry.get("below_q1"):
            return SIGNAL_SCORES["low"]
        if entry.get("extra_high"):
            return SIGNAL_SCORES["really_high"]
        if entry.get("high"):
            return SIGNAL_SCORES["high"]
        return 0

    for file_entry in files_payload:
        metrics_data = file_entry.get("metrics") or {}
        if not isinstance(metrics_data, dict):
            continue
        # Aggregate repeated signals so the report can highlight stronger patterns.
        def _max_score(predicate_tokens: tuple[str, ...]) -> int:
            scores = []
            for key, entry in metrics_data.items():
                if not isinstance(entry, dict):
                    continue
                lowered = str(key).lower()
                if any(token in lowered for token in predicate_tokens):
                    scores.append(_score_signal(entry))
            return max(scores) if scores else 0

        duplication_score = _max_score(("duplication",))
        ncss_score = _max_score(("ncss", "loc", "lines"))
        functions_score = _max_score(("function",))
        complexity_score = _max_score(("ccn", "complex"))
        nesting_score = _max_score(("nest",))

        filename = file_entry.get("filename") or file_entry.get("path") or "Unknown file"

        if duplication_score >= 2 and ncss_score >= 2:
            combined_findings.append(
                f"- Heavy duplication in a large file: `{filename}` — high NCSS with duplicated blocks suggests weak structure."
            )

        if duplication_score >= 2 and (functions_score <= -2 or ncss_score >= 2):
            reasons = []
            if duplication_score >= 2:
                reasons.append("high duplication")
            if ncss_score >= 2:
                reasons.append("high NCSS/LOC")
            if functions_score <= -2:
                reasons.append("low function count")
            joined = ", ".join(reasons) if reasons else "multiple signals"
            trailing = config.STUDENTS_COMBINED_FINDINGS.get(
                "duplication_ncss", "Refactor to extract shared helpers and reduce copy/paste."
            )
            combined_findings.append(f"- Poor factoring: `{filename}` — {joined}. {trailing}")

        if complexity_score >= 2 and nesting_score >= 2:
            trailing = config.STUDENTS_COMBINED_FINDINGS.get(
                "complexity_nesting", "Simplify branching and split logic into smaller units."
            )
            combined_findings.append(
                f"- Complex logic: `{filename}` — high cyclomatic complexity with deep nesting. {trailing}"
            )

        if ncss_score >= 1 and functions_score <= -2:
            trailing = config.STUDENTS_COMBINED_FINDINGS.get(
                "ncss_high_functions_low", "Large monolithic structure — break into smaller functions."
            )
            combined_findings.append(f"- Large monolithic structure: `{filename}` — {trailing}")

        if complexity_score >= 2 and functions_score <= -2:
            trailing = config.STUDENTS_COMBINED_FINDINGS.get(
                "complexity_high_functions_low", "Under-factored logic — extract helpers to tame complexity."
            )
            combined_findings.append(f"- Under-factored logic: `{filename}` — {trailing}")

        if nesting_score >= 2 and duplication_score >= 2:
            trailing = config.STUDENTS_COMBINED_FINDINGS.get(
                "nesting_high_duplication_high", "Nested duplication blocks — de-duplicate and flatten."
            )
            combined_findings.append(f"- Nested duplication blocks: `{filename}` — {trailing}")

        if ncss_score <= -2 and complexity_score >= 2:
            trailing = config.STUDENTS_COMBINED_FINDINGS.get(
                "ncss_low_complexity_high", "Hard to read code — small size but complex; clarify and simplify."
            )
            combined_findings.append(f"- Hard to read code: `{filename}` — {trailing}")

        if duplication_score <= -1 and nesting_score >= 2 and complexity_score >= 2:
            trailing = config.STUDENTS_COMBINED_FINDINGS.get(
                "duplication_low_nesting_high_complexity_high", "Huge decision tree — flatten branches and clarify flow."
            )
            combined_findings.append(f"- Huge decision tree: `{filename}` — {trailing}")

        if ncss_score <= -2 and functions_score >= 2:
            trailing = config.STUDENTS_COMBINED_FINDINGS.get(
                "ncss_low_functions_high", "Overfactoring — too many functions for the file size."
            )
            combined_findings.append(f"- Overfactoring: `{filename}` — {trailing}")

        if complexity_score >= 2 and duplication_score >= 2 and nesting_score >= 2 and ncss_score >= 2:
            trailing = config.STUDENTS_COMBINED_FINDINGS.get(
                "priority_all_high", "Priority code to review — multiple red flags across complexity, duplication, nesting, and size."
            )
            combined_findings.append(f"- Priority code to review: `{filename}` — {trailing}")

        # Positive patterns
        if complexity_score < 0 and nesting_score < 0:
            trailing = config.STUDENTS_COMBINED_FINDINGS.get(
                "positive_simple", "Simple code that reads easily."
            )
            combined_findings.append(f"- Simple and readable: `{filename}` — {trailing}")

        if duplication_score < 0 and functions_score >= 1:
            trailing = config.STUDENTS_COMBINED_FINDINGS.get(
                "positive_factored", "No duplication with healthy function decomposition."
            )
            combined_findings.append(f"- Well factored: `{filename}` — {trailing}")

        if ncss_score <= -1 and complexity_score < 0:
            trailing = config.STUDENTS_COMBINED_FINDINGS.get(
                "positive_concise", "Concise and easy to read solution."
            )
            combined_findings.append(f"- Concise solution: `{filename}` — {trailing}")

        if (
            ncss_score <= 1
            and functions_score >= -1
            and complexity_score <= 1
            and duplication_score <= 1
            and nesting_score <= 1
        ):
            trailing = config.STUDENTS_COMBINED_FINDINGS.get(
                "positive_balanced", "Well-balanced solution with no concerning signals."
            )
            combined_findings.append(f"- Balanced solution: `{filename}` — {trailing}")

    if combined_findings:
        lines.append("## Combined patterns")
        lines.append("")
        lines.append("Multi-metric signals for students/files needing attention:")
        lines.extend(combined_findings)
        lines.append("")

    if highlighted_metrics == 0 and not combined_findings:
        lines.append("No outlier students detected for the current dataset.")

    # Append clustering insights if available (all themes)
    clustering_runs = _collect_clustering_runs(dataset_name)
    if clustering_runs:
        lines.append("")
        lines.append("## Clustering insights")
        lines.append("")
        lines.append(
            "Summaries below are generated from the latest clustering runs across themes "
            "(LLM-provided label/description) with representative files."
        )
        for run in clustering_runs:
            clusters = run.get("clusters") or []
            points = run.get("points") or []
            theme_label = run.get("theme_label") or run.get("theme") or "Theme"
            if not clusters:
                continue
            lines.append(f"### Theme: {theme_label}")
            lines.append("")
            for cluster in clusters:
                cid = cluster.get("id") if isinstance(cluster, dict) else None
                label = cluster.get("label") if isinstance(cluster, dict) else None
                desc = cluster.get("description") if isinstance(cluster, dict) else None
                comparison = cluster.get("comparison") if isinstance(cluster, dict) else None
                size = cluster.get("size") if isinstance(cluster, dict) else None
                good = cluster.get("good") if isinstance(cluster, dict) else None
                bad = cluster.get("bad") if isinstance(cluster, dict) else None
                lines.append(f"#### {label or f'Cluster {cid}'}")
                if size is not None:
                    lines.append(f"- Size: {size}")
                if desc:
                    lines.append("")
                    lines.append(desc if isinstance(desc, str) else str(desc))
                if isinstance(good, list) and good:
                    lines.append("")
                    lines.append("Good:")
                    for item in good:
                        lines.append(f"- {item}")
                if isinstance(bad, list) and bad:
                    lines.append("")
                    lines.append("Bad:")
                    for item in bad:
                        lines.append(f"- {item}")
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

    raw_report = "\n".join(lines).strip() + "\n"

    # Summarize/refine via LLM
    final_report = raw_report
    '''

    try:
        prompt = config.STUDENTS_REPORT_SUMMARY_PROMPT.format(report=raw_report)
        final_report = generate_completion(prompt)
    except Exception:
        final_report = raw_report
    '''

    report_path.write_text(final_report.strip() + "\n", encoding="utf-8")
    return report_path, highlighted_metrics, bool(combined_findings), generated_token
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
        report_path, highlighted_metrics, combined, generated_token = _write_students_report(dataset_name, outliers)
    except OSError as exc:
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=f"Unable to write report: {exc}"
        ) from exc

    return {
        "dataset": dataset_name,
        "report_path": report_path.as_posix(),
        "report_filename": report_path.name,
        "generated_at": generated_token,
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

    report_path, token = _latest_report_path(dataset_name)
    if not report_path or not report_path.exists():
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Report not found.")

    filename = report_path.name
    return FileResponse(report_path, media_type="text/markdown", filename=filename)


@router.get("/latest-report", name="latest-report")
async def latest_report(dataset: str | None = None) -> dict[str, object]:
    """
    @brief Return metadata about the most recent students report (if any).
    """
    dataset_name, _ = resolve_dataset_or_http_error(dataset, require_raw=True)
    ensure_dataset_ready(dataset_name)
    path, token = _latest_report_path(dataset_name)
    if not path or not path.exists():
        return {"dataset": dataset_name, "available": False}
    return {
        "dataset": dataset_name,
        "available": True,
        "report_path": path.as_posix(),
        "report_filename": path.name,
        "generated_at": token,
    }
