from __future__ import annotations

from fastapi import APIRouter, HTTPException, Request, status
from fastapi.templating import Jinja2Templates

from Files.dataset_manager import get_current_dataset, list_datasets
from Metrics import dataset_summary_path
from Metrics.other_metrics import extract_file_metrics, load_reference_metrics
from Metrics.students import THRESHOLD_DESCRIPTIONS, load_students_outliers
from Routers.utils import ensure_dataset_ready, read_utf8_or_error, resolve_dataset_or_http_error

router = APIRouter()
templates = Jinja2Templates(directory="Static/templates")


def _context(request: Request, page_title: str) -> dict[str, object]:
    """
    @brief Build template context for plagiarism views.
    @param request Incoming FastAPI request object.
    @param page_title Title to display on the page.
    @return Context dictionary for template rendering.
    """
    return {
        "request": request,
        "page_title": page_title,
        "active_page": "plagiarism",
        "current_dataset": get_current_dataset(),
        "datasets": list_datasets(),
    }


@router.get("/metrics/global", name="global-metrics")
async def read_global_metrics(dataset: str | None = None) -> dict[str, object]:
    """
    @brief Retrieve global metrics aggregated by Lizard for a dataset.
    @param dataset Optional dataset override.
    @return Metrics summary alongside per-file entries and references.
    @throws HTTPException If summary files are missing or unreadable.
    """
    dataset_name, _ = resolve_dataset_or_http_error(dataset, require_raw=True)
    ensure_dataset_ready(dataset_name)
    summary_path = dataset_summary_path(dataset_name)
    if not summary_path.exists():
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Lizard analysis not found.")

    summary_text = read_utf8_or_error(summary_path, detail="Unable to read summary file.")
    metrics, files = extract_file_metrics(dataset_name, summary_text)
    if not metrics or not files:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="No file metrics available.")
    reference = load_reference_metrics(dataset_name)
    return {"dataset": dataset_name, "metrics": metrics, "files": files, "reference": reference}


@router.get("/metrics/students", name="students-metrics")
async def read_students_metrics(dataset: str | None = None) -> dict[str, object]:
    """
    @brief Return precomputed student outlier data for a dataset.
    @param dataset Optional dataset override.
    @return JSON payload containing metric buckets and per-file flags.
    """
    dataset_name, _ = resolve_dataset_or_http_error(dataset, require_raw=True)
    ensure_dataset_ready(dataset_name)
    summary_path = dataset_summary_path(dataset_name)
    if not summary_path.exists():
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Lizard analysis not found.")
    try:
        payload = load_students_outliers(dataset_name)
    except RuntimeError as exc:
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=str(exc)) from exc
    except Exception as exc:  # pragma: no cover - unexpected decode errors
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail="Unable to load students metrics."
        ) from exc

    if not payload:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="No students metrics available.")
    return payload


@router.get("/plagiarism", name="plagiarism")
async def plagiarism_view(request: Request):
    """
    @brief Render the plagiarism analysis page.
    @param request Incoming FastAPI request object.
    @return Jinja2 template response for the page.
    """
    return templates.TemplateResponse("plagiarism.html", _context(request, "Plagiarism"))


@router.get("/metrics/threshold-descriptions", name="threshold-descriptions")
async def threshold_descriptions() -> dict[str, object]:
    """
    @brief Provide shared threshold descriptions for frontend display.
    @return Mapping of metric categories to bucket descriptions.
    """
    return {"descriptions": THRESHOLD_DESCRIPTIONS}
