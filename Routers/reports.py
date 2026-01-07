from __future__ import annotations

import csv
import json
from pathlib import Path
from datetime import datetime, timezone

from fastapi import APIRouter, HTTPException, status
from pydantic import BaseModel, Field

import config
from Routers.clustering import THEMES
from Files.dataset_manager import dataset_path
from Metrics.students import load_students_outliers
from Routers.utils import (
    ensure_dataset_ready,
    resolve_dataset_or_http_error,
    resolve_relative_file,
    resolve_repository_or_http_error,
)

router = APIRouter(prefix="/reports", tags=["reports"])
REPORTS_SUBDIR = config.REPORTS_SUBDIR
STUDENTS_REPORT_FILENAME = config.STUDENTS_REPORT_FILENAME


def _students_report_path(dataset_name: str) -> Path:
    """
    Compute the path to the students outliers report for a dataset.
    """
    return dataset_path(dataset_name) / REPORTS_SUBDIR / STUDENTS_REPORT_FILENAME


def _latest_report_path(dataset_name: str) -> tuple[Path | None, str | None]:
    """
    Locate the students report JSON (single latest file).
    """
    path = _students_report_path(dataset_name)
    if not path.exists():
        return None, None
    try:
        token = datetime.fromtimestamp(path.stat().st_mtime, tz=timezone.utc).isoformat()
    except Exception:
        token = None
    return path, token


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


def _load_report(dataset_name: str) -> tuple[dict[str, object] | None, str | None]:
    """
    Load the latest JSON report for a dataset, returning (report, generated_token).
    """
    path, token = _latest_report_path(dataset_name)
    if not path or not path.exists():
        return None, None
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
        return payload, token
    except Exception:
        return None, token


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


def _write_students_report(
    dataset_name: str, outliers: dict[str, object]
) -> tuple[Path, int, bool, str, dict[str, object]]:
    """
    Build a JSON report capturing outliers, combined findings, and clustering context.
    """
    buckets = outliers.get("buckets") or []
    if not isinstance(buckets, list):
        buckets = []

    generated_token = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H-%M-%SZ")
    generated_at = datetime.now(timezone.utc).isoformat()
    report_path = _students_report_path(dataset_name)
    report_path.parent.mkdir(parents=True, exist_ok=True)

    clustering_runs = _collect_clustering_runs(dataset_name)
    membership_lookup = _build_membership_lookup(clustering_runs) if clustering_runs else {}

    highlighted_metrics = sum(
        1
        for bucket in buckets
        if any((bucket.get("groups") or {}).get(key) for key in ("aboveFence", "aboveQ3", "belowQ1", "belowFence"))
    )

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
        if entry.get("below_fence"):
            return SIGNAL_SCORES["really_low"]
        if entry.get("below_q1"):
            return SIGNAL_SCORES["low"]
        if entry.get("extra_high"):
            return SIGNAL_SCORES["really_high"]
        if entry.get("high"):
            return SIGNAL_SCORES["high"]
        return 0

    def _max_score(metrics_data: dict[str, object], tokens: tuple[str, ...]) -> int:
        scores = []
        for key, entry in metrics_data.items():
            if not isinstance(entry, dict):
                continue
            lowered = str(key).lower()
            if any(token in lowered for token in tokens):
                scores.append(_score_signal(entry))
        return max(scores) if scores else 0

    combined_flags: dict[str, list[dict[str, str]]] = {}

    def _add_flag(filename: str, label: str, tone: str = "bad") -> None:
        if not filename or not label:
            return
        combined_flags.setdefault(filename, []).append({"label": label, "tone": tone})

    for file_entry in files_payload:
        metrics_data = file_entry.get("metrics") or {}
        if not isinstance(metrics_data, dict):
            continue

        duplication_score = _max_score(metrics_data, ("duplication",))
        ncss_score = _max_score(metrics_data, ("ncss", "loc", "lines"))
        functions_score = _max_score(metrics_data, ("function",))
        complexity_score = _max_score(metrics_data, ("ccn", "complex"))
        nesting_score = _max_score(metrics_data, ("nest",))

        filename = file_entry.get("filename") or file_entry.get("path") or "Unknown file"

        if duplication_score >= 2 and ncss_score >= 2:
            text = f"Heavy duplication in a large file: `{filename}` — high NCSS with duplicated blocks suggests weak structure."
            combined_findings.append(text)
            _add_flag(filename, "Duplication + size", "bad")

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
            text = f"Poor factoring: `{filename}` — {joined}. {trailing}"
            combined_findings.append(text)
            _add_flag(filename, "Poor factoring", "bad")

        if complexity_score >= 2 and nesting_score >= 2:
            trailing = config.STUDENTS_COMBINED_FINDINGS.get(
                "complexity_nesting", "Simplify branching and split logic into smaller units."
            )
            text = f"Complex logic: `{filename}` — high cyclomatic complexity with deep nesting. {trailing}"
            combined_findings.append(text)
            _add_flag(filename, "Complex logic", "bad")

        if ncss_score >= 1 and functions_score <= -2:
            trailing = config.STUDENTS_COMBINED_FINDINGS.get(
                "ncss_high_functions_low", "Large monolithic structure — break into smaller functions."
            )
            combined_findings.append(f"Large monolithic structure: `{filename}` — {trailing}")
            _add_flag(filename, "Monolithic", "bad")

        if complexity_score >= 2 and functions_score <= -2:
            trailing = config.STUDENTS_COMBINED_FINDINGS.get(
                "complexity_high_functions_low", "Under-factored logic — extract helpers to tame complexity."
            )
            combined_findings.append(f"Under-factored logic: `{filename}` — {trailing}")
            _add_flag(filename, "Under-factored", "bad")

        if nesting_score >= 2 and duplication_score >= 2:
            trailing = config.STUDENTS_COMBINED_FINDINGS.get(
                "nesting_high_duplication_high", "Nested duplication blocks — de-duplicate and flatten."
            )
            combined_findings.append(f"Nested duplication blocks: `{filename}` — {trailing}")
            _add_flag(filename, "Nested duplication", "bad")

        if ncss_score <= -2 and complexity_score >= 2:
            trailing = config.STUDENTS_COMBINED_FINDINGS.get(
                "ncss_low_complexity_high", "Hard to read code — small size but complex; clarify and simplify."
            )
            combined_findings.append(f"Hard to read code: `{filename}` — {trailing}")
            _add_flag(filename, "Hard to read", "bad")

        if duplication_score <= -1 and nesting_score >= 2 and complexity_score >= 2:
            trailing = config.STUDENTS_COMBINED_FINDINGS.get(
                "duplication_low_nesting_high_complexity_high", "Huge decision tree — flatten branches and clarify flow."
            )
            combined_findings.append(f"Huge decision tree: `{filename}` — {trailing}")
            _add_flag(filename, "Huge decision tree", "bad")

        if ncss_score <= -2 and functions_score >= 2:
            trailing = config.STUDENTS_COMBINED_FINDINGS.get(
                "ncss_low_functions_high", "Overfactoring — too many functions for the file size."
            )
            combined_findings.append(f"Overfactoring: `{filename}` — {trailing}")
            _add_flag(filename, "Overfactored", "bad")

        if complexity_score >= 2 and duplication_score >= 2 and nesting_score >= 2 and ncss_score >= 2:
            trailing = config.STUDENTS_COMBINED_FINDINGS.get(
                "priority_all_high", "Priority code to review — multiple red flags across complexity, duplication, nesting, and size."
            )
            combined_findings.append(f"Priority code to review: `{filename}` — {trailing}")
            _add_flag(filename, "Priority review", "bad")

        if complexity_score < 0 and nesting_score < 0:
            trailing = config.STUDENTS_COMBINED_FINDINGS.get("positive_simple", "Simple code that reads easily.")
            combined_findings.append(f"Simple and readable: `{filename}` — {trailing}")
            _add_flag(filename, "Simple & readable", "good")

        if duplication_score < 0 and functions_score >= 1:
            trailing = config.STUDENTS_COMBINED_FINDINGS.get(
                "positive_factored", "No duplication with healthy function decomposition."
            )
            combined_findings.append(f"Well factored: `{filename}` — {trailing}")
            _add_flag(filename, "Well factored", "good")

        if ncss_score <= -1 and complexity_score < 0:
            trailing = config.STUDENTS_COMBINED_FINDINGS.get(
                "positive_concise", "Concise and easy to read solution."
            )
            combined_findings.append(f"Concise solution: `{filename}` — {trailing}")
            _add_flag(filename, "Concise", "good")

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
            combined_findings.append(f"Balanced solution: `{filename}` — {trailing}")
            _add_flag(filename, "Balanced", "good")

    report_data: dict[str, object] = {
        "dataset": dataset_name,
        "generated_at": generated_at,
        "generated_token": generated_token,
        "buckets": buckets,
        "metrics": outliers.get("metrics") or [],
        "files": files_payload,
        "combined_findings": combined_findings,
        "combined_flags": combined_flags,
        "highlighted_metrics": highlighted_metrics,
        "has_combined_findings": bool(combined_findings),
        "clusters": clustering_runs,
        "cluster_membership": membership_lookup,
    }

    report_path.write_text(json.dumps(report_data, ensure_ascii=False, indent=2), encoding="utf-8")
    return report_path, highlighted_metrics, bool(combined_findings), generated_token, report_data


def generate_students_report(dataset_name: str) -> dict[str, object]:
    """
    Generate (or regenerate) the students report JSON and return its contents.
    """
    try:
        outliers = load_students_outliers(dataset_name)
    except RuntimeError as exc:
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=str(exc)) from exc

    report_path, _, _, generated_token, report_data = _write_students_report(dataset_name, outliers)
    report_data = dict(report_data or {})
    report_data["report_path"] = report_path.as_posix()
    report_data["report_filename"] = report_path.name
    report_data["generated_at"] = report_data.get("generated_at") or generated_token
    return report_data
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
    @return Response containing the generated JSON report.
    """
    dataset_name, _ = resolve_dataset_or_http_error(payload.dataset, require_raw=True)
    ensure_dataset_ready(dataset_name)

    try:
        report_data = generate_students_report(dataset_name)
    except HTTPException:
        raise
    except Exception as exc:
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=str(exc)) from exc

    return {
        "dataset": dataset_name,
        "report": report_data,
        "status": "generated",
    }


@router.post("/generate-file-report", name="generate-file-report")
async def generate_file_report(payload: FileReportRequest) -> dict[str, object]:
    """
    @brief Trigger report generation for a specific file within a dataset.
    @param payload Request body containing dataset selection and filename.
    @return Placeholder response echoing the validated dataset and file.
    """
    dataset_name, repository = resolve_repository_or_http_error(payload.dataset, require_raw=True)
    _, filename = resolve_relative_file(repository.raw_dir, payload.filename)
    return {
        "dataset": dataset_name,
        "file": filename,
        "status": "accepted",
        "message": "File report generation endpoint stub.",
    }


@router.get("/download-report", name="download-report")
async def download_report(dataset: str | None = None) -> dict[str, object]:
    """
    Return the latest students report as JSON.
    """
    dataset_name, _ = resolve_dataset_or_http_error(dataset, require_raw=True)
    ensure_dataset_ready(dataset_name)

    report_data, _ = _load_report(dataset_name)
    if not report_data:
        # If missing, generate on the fly
        report_data = generate_students_report(dataset_name)
    return {
        "dataset": dataset_name,
        "generated_at": report_data.get("generated_at"),
        "report": report_data,
    }


@router.get("/latest-report", name="latest-report")
async def latest_report(dataset: str | None = None) -> dict[str, object]:
    """
    @brief Return metadata about the most recent students report (if any).
    """
    dataset_name, _ = resolve_dataset_or_http_error(dataset, require_raw=True)
    ensure_dataset_ready(dataset_name)
    report_data, token = _load_report(dataset_name)
    if not report_data:
        try:
            report_data = generate_students_report(dataset_name)
            token = report_data.get("generated_at")
        except HTTPException:
            raise
        except Exception:
            return {"dataset": dataset_name, "available": False}
    return {
        "dataset": dataset_name,
        "available": True,
        "generated_at": token,
        "report": report_data,
    }
