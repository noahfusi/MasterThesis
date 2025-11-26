from __future__ import annotations

from pathlib import Path

from fastapi import APIRouter, BackgroundTasks, HTTPException, Query, status
from pydantic import BaseModel, Field

import config
from Files.dataset_manager import dataset_path
from LLM import LLMError, generate_completion
from Routers.utils import resolve_dataset_or_http_error, resolve_relative_file

router = APIRouter(prefix="/feedback", tags=["feedback"])


class FeedbackRequest(BaseModel):
    dataset: str = Field(..., description="Dataset name.")
    filename: str = Field(..., description="Relative filename inside the raw folder.")


class DatasetFeedbackRequest(BaseModel):
    dataset: str = Field(..., description="Dataset name.")


def _feedback_dir(dataset: str) -> Path:
    """
    @brief Return the feedback output directory for a dataset.
    """
    return dataset_path(dataset) / config.FEEDBACK_OUTPUT_DIRNAME


def _read_requirements(dataset_root: Path) -> str | None:
    """
    @brief Read the first available requirements file, if any.
    """
    req_dir = dataset_root / "requirements"
    if not req_dir.exists():
        return None
    for candidate in sorted(req_dir.iterdir()):
        if candidate.is_file():
            try:
                return candidate.read_text(encoding="utf-8")
            except OSError:
                continue
    return None


def _format_prompt(filename: str, code: str, requirements: str | None) -> str:
    """
    @brief Build the feedback prompt based on presence of requirements.
    """
    if requirements:
        return config.FEEDBACK_PROMPT_WITH_REQUIREMENTS.format(
            requirements=requirements.strip(), code=code, filename=filename
        )
    return config.FEEDBACK_PROMPT_NO_REQUIREMENTS.format(code=code, filename=filename)


def _generate_file_feedback(dataset: str, raw_dir: Path, filename: str, output_root: Path) -> None:
    """
    @brief Generate feedback for a single file and persist it under the dataset feedback folder.
    """
    try:
        target_path, relative = resolve_relative_file(raw_dir, filename)
    except HTTPException:
        return
    try:
        code = target_path.read_text(encoding="utf-8")
    except OSError:
        return

    dataset_root = dataset_path(dataset)
    requirements = _read_requirements(dataset_root)
    prompt = _format_prompt(relative, code, requirements)
    try:
        feedback = generate_completion(prompt)
    except (LLMError, ValueError):
        return

    output_path = output_root / Path(relative).with_suffix(Path(relative).suffix + ".feedback.txt")
    output_path.parent.mkdir(parents=True, exist_ok=True)
    try:
        output_path.write_text(feedback, encoding="utf-8")
    except OSError:
        return


def _generate_dataset_feedback(dataset: str, raw_dir: Path, output_root: Path) -> None:
    """
    @brief Generate feedback for all files in a dataset (best-effort).
    """
    if not raw_dir.exists():
        return
    for path in raw_dir.rglob("*"):
        if not path.is_file():
            continue
        relative = path.relative_to(raw_dir).as_posix()
        _generate_file_feedback(dataset, raw_dir, relative, output_root)


@router.post("/file", status_code=status.HTTP_202_ACCEPTED, name="feedback-file")
async def request_file_feedback(payload: FeedbackRequest, background_tasks: BackgroundTasks) -> dict[str, object]:
    """
    @brief Enqueue feedback generation for a specific file.
    """
    dataset, raw_dir = resolve_dataset_or_http_error(payload.dataset, require_raw=True)
    _, relative = resolve_relative_file(raw_dir, payload.filename)
    output_root = _feedback_dir(dataset)

    background_tasks.add_task(_generate_file_feedback, dataset, raw_dir, relative, output_root)
    return {
        "dataset": dataset,
        "filename": relative,
        "status": "accepted",
        "message": config.MESSAGES["FEEDBACK_FILE_QUEUED"],
    }


@router.post("/dataset", status_code=status.HTTP_202_ACCEPTED, name="feedback-dataset")
async def request_dataset_feedback(payload: DatasetFeedbackRequest, background_tasks: BackgroundTasks) -> dict[str, object]:
    """
    @brief Enqueue feedback generation for the entire dataset.
    """
    dataset, _ = resolve_dataset_or_http_error(payload.dataset, require_raw=False)
    dataset_root = dataset_path(dataset)
    if not dataset_root.exists():
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail=config.MESSAGES["DATASET_NOT_FOUND"])
    output_root = _feedback_dir(dataset)
    raw_dir = dataset_root / config.RAW_FOLDER_NAME

    background_tasks.add_task(_generate_dataset_feedback, dataset, raw_dir, output_root)
    return {
        "dataset": dataset,
        "status": "accepted",
        "message": config.MESSAGES["FEEDBACK_DATASET_QUEUED"],
    }


@router.get("/files", name="feedback-files")
async def list_feedback_files(dataset: str = Query(...)) -> dict[str, object]:
    """
    @brief List available feedback files for a dataset.
    """
    dataset_name, _ = resolve_dataset_or_http_error(dataset, require_raw=False)
    root = _feedback_dir(dataset_name)
    if not root.exists():
        return {"dataset": dataset_name, "files": []}
    files = [str(path.relative_to(root)).replace("\\", "/") for path in root.rglob("*.feedback.txt") if path.is_file()]
    return {"dataset": dataset_name, "files": sorted(files)}


@router.get("/file", name="feedback-read")
async def read_feedback_file(dataset: str = Query(...), filename: str = Query(...)) -> dict[str, object]:
    """
    @brief Read a stored feedback file content.
    """
    dataset_name, _ = resolve_dataset_or_http_error(dataset, require_raw=False)
    root = _feedback_dir(dataset_name)
    target = (root / filename).resolve()
    if not str(target).startswith(str(root.resolve())) or not target.is_file():
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail=config.MESSAGES["FILE_NOT_FOUND"])
    try:
        content = target.read_text(encoding="utf-8")
    except OSError as exc:
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=str(exc)) from exc
    return {"dataset": dataset_name, "filename": filename, "content": content}
