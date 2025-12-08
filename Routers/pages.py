import config
from fastapi import APIRouter, Request
from fastapi.templating import Jinja2Templates

from Files.dataset_manager import get_current_dataset, list_datasets

templates = Jinja2Templates(directory=config.TEMPLATES_DIR)

router = APIRouter()


def _context(request: Request, page_title: str, active_page: str) -> dict[str, object]:
    """
    @brief Build shared template context for navigation pages.
    @param request Incoming FastAPI request object.
    @param page_title Title to display on the page.
    @param active_page Identifier for the active navigation tab.
    @return Context dictionary for template rendering.
    """
    return {
        "request": request,
        "page_title": page_title,
        "active_page": active_page,
        "current_dataset": get_current_dataset(),
        "datasets": list_datasets(),
    }


@router.get("/", name="dataset-management")
async def dataset_management(request: Request):
    """
    @brief Render the dataset management landing page.
    @param request Incoming FastAPI request.
    @return Jinja2 response for the dataset management page.
    """
    return templates.TemplateResponse("dataset_management.html", _context(request, "Dataset management", "dataset"))


@router.get("/global-analysis", name="global-analysis")
async def global_analysis(request: Request):
    """
    @brief Render the global analysis placeholder page.
    @param request Incoming FastAPI request.
    @return Jinja2 response for the global analysis page.
    """
    return templates.TemplateResponse("global_analysis.html", _context(request, "Global analysis", "global"))


@router.get("/file-analysis", name="file-analysis")
async def file_analysis(request: Request):
    """
    @brief Render the per-file analysis placeholder page.
    @param request Incoming FastAPI request.
    @return Jinja2 response for the file analysis page.
    """
    return templates.TemplateResponse("file_analysis.html", _context(request, "File analysis", "file"))


@router.get("/file-explorer", name="file-explorer")
async def file_explorer(request: Request):
    """
    @brief Render the dataset file explorer page.
    @param request Incoming FastAPI request.
    @return Jinja2 response for the file explorer page.
    """
    return templates.TemplateResponse("file_explorer.html", _context(request, "File explorer", "explorer"))


@router.get("/solution-clustering", name="solution-clustering")
async def solution_clustering(request: Request):
    """
    @brief Render the solution clustering placeholder page.
    @param request Incoming FastAPI request.
    @return Jinja2 response for the solution clustering page.
    """
    context = _context(request, "Solution clustering", "clustering")
    context["clustering_themes"] = config.CLUSTERING_THEMES
    return templates.TemplateResponse("solution_clustering.html", context)


@router.get("/students", name="students")
async def students_page(request: Request):
    """
    @brief Render the students placeholder page.
    @param request Incoming FastAPI request.
    @return Jinja2 response for the students page.
    """
    return templates.TemplateResponse("students.html", _context(request, "Students", "students"))


@router.get("/autotest", name="autotest-page")
async def autotest_page(request: Request):
    """
    Render the AutoTest dashboard page.
    """
    return templates.TemplateResponse("autotest.html", _context(request, "AutoTest", "autotest"))
