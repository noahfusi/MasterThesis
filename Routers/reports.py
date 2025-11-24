from __future__ import annotations

from datetime import datetime, timezone
from pathlib import Path

from fastapi import APIRouter, HTTPException
from fastapi.responses import FileResponse
from pydantic import BaseModel, Field

from Files.dataset_manager import dataset_path
from Metrics.students import load_students_outliers
from Routers.utils import ensure_dataset_ready, resolve_dataset_or_http_error, resolve_relative_file

router = APIRouter(prefix="/reports", tags=["reports"])
REPORTS_SUBDIR = "reports"
STUDENTS_REPORT_FILENAME = "students_outliers_report.md"


def _students_report_path(dataset_name: str) -> Path:
    return dataset_path(dataset_name) / REPORTS_SUBDIR / STUDENTS_REPORT_FILENAME


def _write_students_report(dataset_name: str, outliers: dict[str, object]) -> tuple[Path, int, bool]:
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

    highlighted_metrics = 0
    for bucket in buckets:
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
                lines.append(f"- `{filename}` — {value}")
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
