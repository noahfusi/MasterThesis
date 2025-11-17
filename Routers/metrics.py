from __future__ import annotations

from fastapi import APIRouter, Request
from fastapi.templating import Jinja2Templates

from Files.dataset_manager import get_current_dataset, list_datasets

router = APIRouter()
templates = Jinja2Templates(directory="Static/templates")


def _context(request: Request, page_title: str) -> dict[str, object]:
    return {
        "request": request,
        "page_title": page_title,
        "active_page": "plagiarism",
        "current_dataset": get_current_dataset(),
        "datasets": list_datasets(),
    }


@router.get("/plagiarism", name="plagiarism")
async def plagiarism_view(request: Request):
    return templates.TemplateResponse("plagiarism.html", _context(request, "Plagiarism"))
