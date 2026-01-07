from __future__ import annotations

from pathlib import Path
import logging

from fastapi import APIRouter, BackgroundTasks, HTTPException, Query, status
from pydantic import BaseModel, Field

import config
import asyncio
from LLM import LLMError, generate_completion, async_generate_completion
from Pipeline import ArtifactRepository
from Routers.utils import resolve_relative_file, resolve_repository_or_http_error, sanitize_for_llm_prompt
from Routers.tasks import notify_tasks_sync

router = APIRouter(prefix="/feedback", tags=["feedback"])
logger = logging.getLogger("uvicorn.error")
DATASET_INFLIGHT: set[str] = set()
_inflight_lock = asyncio.Lock()


class FeedbackRequest(BaseModel):
    dataset: str = Field(..., description="Dataset name.")
    filename: str = Field(..., description="Relative filename inside the raw folder.")


class DatasetFeedbackRequest(BaseModel):
    dataset: str = Field(..., description="Dataset name.")


def _format_prompt(filename: str, code: str, requirements: str | None) -> str:
    """
    @brief Build the feedback prompt based on presence of requirements.
    """
    # Sanitize user-provided content before inserting into LLM prompts
    sanitized_code = sanitize_for_llm_prompt(code, max_length=8000)
    sanitized_filename = sanitize_for_llm_prompt(filename, max_length=500)

    if requirements:
        sanitized_requirements = sanitize_for_llm_prompt(requirements, max_length=5000)
        return config.FEEDBACK_PROMPT_WITH_REQUIREMENTS.format(
            requirements=sanitized_requirements.strip(), code=sanitized_code, filename=sanitized_filename
        )
    return config.FEEDBACK_PROMPT_NO_REQUIREMENTS.format(code=sanitized_code, filename=sanitized_filename)


def _generate_file_feedback(dataset: str, repository: ArtifactRepository, filename: str) -> None:
    """
    @brief Generate feedback for a single file and persist it under the dataset feedback folder.
    """
    logger.info("[feedback] start file feedback dataset=%s file=%s", dataset, filename)
    try:
        _, relative = resolve_relative_file(repository.raw_dir, filename)
        code, _ = repository.read_raw_file(relative)
    except (HTTPException, ValueError, FileNotFoundError):
        logger.warning("[feedback] file feedback skipped (resolve/read error) dataset=%s file=%s", dataset, filename)
        return

    requirements = repository.read_first_requirement()
    prompt = _format_prompt(relative, code, requirements)
    try:
        feedback = generate_completion(prompt)
    except (LLMError, ValueError):
        logger.warning("[feedback] file feedback generation failed dataset=%s file=%s", dataset, filename)
        return

    try:
        output_path = repository.write_feedback(relative, feedback)
        logger.info("[feedback] file feedback completed dataset=%s file=%s", dataset, filename)
        notify_tasks_sync(
            {"type": "feedback-file-completed", "dataset": dataset, "filename": str(Path(relative).as_posix())}
        )
    except OSError:
        logger.warning("[feedback] file feedback write failed dataset=%s file=%s", dataset, filename)
        return


async def _generate_file_feedback_async(dataset: str, repository: ArtifactRepository, filename: str) -> None:
    logger.info("[feedback] start file feedback (async) dataset=%s file=%s", dataset, filename)
    try:
        _, relative = resolve_relative_file(repository.raw_dir, filename)
        code, _ = repository.read_raw_file(relative)
    except (HTTPException, ValueError, FileNotFoundError):
        logger.warning("[feedback] file feedback skipped (resolve/read error) dataset=%s file=%s", dataset, filename)
        return

    requirements = repository.read_first_requirement()
    prompt = _format_prompt(relative, code, requirements)
    try:
        feedback = await async_generate_completion(prompt)
    except (LLMError, ValueError):
        logger.warning("[feedback] file feedback generation failed dataset=%s file=%s", dataset, filename)
        return

    try:
        output_path = repository.write_feedback(relative, feedback)
        logger.info("[feedback] file feedback completed dataset=%s file=%s", dataset, filename)
        notify_tasks_sync(
            {"type": "feedback-file-completed", "dataset": dataset, "filename": str(Path(relative).as_posix())}
        )
    except OSError:
        logger.warning("[feedback] file feedback write failed dataset=%s file=%s", dataset, filename)
        return


async def _generate_dataset_feedback_async(dataset: str, repository: ArtifactRepository) -> None:
    """
    @brief Generate feedback for all files in a dataset (best-effort) concurrently.
    """
    if not repository.raw_dir.exists():
        return
    logger.info("[feedback] dataset feedback start dataset=%s", dataset)
    tasks = []
    sem = asyncio.Semaphore(16)  # Limit concurrency to avoid overwhelming the LLM server

    async def worker(rel: str) -> None:
        async with sem:
            await _generate_file_feedback_async(dataset, repository, rel)

    for relative in repository.list_raw_files():
        tasks.append(asyncio.create_task(worker(relative)))
    try:
        if tasks:
            await asyncio.gather(*tasks)
        status = "completed"
        error = None
    except Exception as exc:  # pragma: no cover - defensive
        status = "failed"
        error = str(exc)
        logger.warning("[feedback] dataset feedback failed dataset=%s error=%s", dataset, exc)
    else:
        logger.info("[feedback] dataset feedback completed dataset=%s", dataset)
    finally:
        notify_tasks_sync(
            {
                "type": "feedback-dataset-completed",
                "dataset": dataset,
                "status": status,
                "error": error,
            }
        )


@router.post("/file", status_code=status.HTTP_202_ACCEPTED, name="feedback-file")
async def request_file_feedback(payload: FeedbackRequest, background_tasks: BackgroundTasks) -> dict[str, object]:
    """
    @brief Enqueue feedback generation for a specific file.
    """
    dataset, repository = resolve_repository_or_http_error(payload.dataset, require_raw=True)
    _, relative = resolve_relative_file(repository.raw_dir, payload.filename)

    background_tasks.add_task(_generate_file_feedback, dataset, repository, relative)
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
    logger.info("Received dataset feedback request: %s", payload.dataset)
    dataset, repository = resolve_repository_or_http_error(payload.dataset, require_raw=True)

    # Atomic check-and-set to prevent race conditions
    async with _inflight_lock:
        if dataset in DATASET_INFLIGHT:
            logger.info("Dataset feedback already in flight: %s", dataset)
            return {
                "dataset": dataset,
                "status": "accepted",
                "message": config.MESSAGES["FEEDBACK_DATASET_QUEUED"],
            }
        DATASET_INFLIGHT.add(dataset)

    async def _run() -> None:
        try:
            await _generate_dataset_feedback_async(dataset, repository)
        finally:
            async with _inflight_lock:
                DATASET_INFLIGHT.discard(dataset)

    # Use add_task directly with the async function, not asyncio.run
    background_tasks.add_task(_run)
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
    dataset_name, repository = resolve_repository_or_http_error(dataset, require_raw=False)
    root = repository.feedback_dir()
    if not root.exists():
        return {"dataset": dataset_name, "files": []}
    files = [str(path.relative_to(root)).replace("\\", "/") for path in root.rglob("*.feedback.txt") if path.is_file()]
    return {"dataset": dataset_name, "files": sorted(files)}


@router.get("/file", name="feedback-read")
async def read_feedback_file(dataset: str = Query(...), filename: str = Query(...)) -> dict[str, object]:
    """
    @brief Read a stored feedback file content.
    """
    dataset_name, repository = resolve_repository_or_http_error(dataset, require_raw=False)
    root = repository.feedback_dir()
    target = (root / filename).resolve()
    if not str(target).startswith(str(root.resolve())) or not target.is_file():
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail=config.MESSAGES["FILE_NOT_FOUND"])
    try:
        content = target.read_text(encoding="utf-8")
    except OSError as exc:
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=str(exc)) from exc
    return {"dataset": dataset_name, "filename": filename, "content": content}
