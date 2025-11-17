from fastapi import APIRouter, Request
from fastapi.templating import Jinja2Templates

from Files.dataset_manager import get_current_dataset, list_datasets

templates = Jinja2Templates(directory="Static/templates")

router = APIRouter()


def _context(request: Request, page_title: str, active_page: str) -> dict[str, object]:
    return {
        "request": request,
        "page_title": page_title,
        "active_page": active_page,
        "current_dataset": get_current_dataset(),
        "datasets": list_datasets(),
    }


@router.get("/", name="dataset-management")
async def dataset_management(request: Request):
    """Landing page with dataset management placeholder."""
    return templates.TemplateResponse("dataset_management.html", _context(request, "Dataset management", "dataset"))


@router.get("/global-analysis", name="global-analysis")
async def global_analysis(request: Request):
    """Placeholder page for global analysis."""
    return templates.TemplateResponse("global_analysis.html", _context(request, "Global analysis", "global"))


@router.get("/file-analysis", name="file-analysis")
async def file_analysis(request: Request):
    """Placeholder page for per-file analysis."""
    return templates.TemplateResponse("file_analysis.html", _context(request, "File analysis", "file"))


@router.get("/file-explorer", name="file-explorer")
async def file_explorer(request: Request):
    """Page to explore dataset files."""
    return templates.TemplateResponse("file_explorer.html", _context(request, "File explorer", "explorer"))
