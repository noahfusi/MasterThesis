from __future__ import annotations

import asyncio
from pathlib import Path
import logging

import config
from fastapi import APIRouter, BackgroundTasks, HTTPException, Query, status
from pydantic import BaseModel, Field

from AutoTest.run import load_results, run_autotest
from Routers.tasks import notify_tasks_sync
from Routers.utils import ensure_dataset_ready, resolve_dataset_or_http_error

logger = logging.getLogger("uvicorn.error")

router = APIRouter(prefix="/autotest", tags=["autotest"])


def _has_compile_error(stored: dict[str, object]) -> bool:
    """
    Detect compilation failures in a stored autotest result payload.
    """
    results = stored.get("results")
    if isinstance(results, list):
        for item in results:
            errors = item.get("errors") if isinstance(item, dict) else None
            if isinstance(errors, list) and "compile_error" in errors:
                return True
    return False


def _collect_autotest_results(dataset: str | None, filenames: list[str]) -> dict[str, object]:
    """
    Build a batch of AutoTest results for the given filenames without failing the entire request.
    """
    dataset_name, raw_dir = resolve_dataset_or_http_error(dataset, require_raw=True)
    ensure_dataset_ready(dataset_name)

    if not filenames:
        return {"dataset": dataset_name, "files": []}

    root = raw_dir.resolve()
    unique_filenames = list(dict.fromkeys(filenames))
    entries: list[dict[str, object]] = []

    for name in unique_filenames:
        normalized = Path(name)
        relative = normalized.as_posix()
        entry: dict[str, object] = {"filename": relative, "results": {"passed": 0, "total": 0}}

        if normalized.is_absolute() or ".." in normalized.parts:
            entry.update({"status": "invalid", "error": "Invalid filename."})
            entries.append(entry)
            continue

        target = (root / normalized).resolve()
        if not str(target).startswith(str(root)) or not target.is_file():
            entry.update({"status": "not_found", "error": "File not found."})
            entries.append(entry)
            continue

        stored = load_results(dataset_name, relative)
        if not stored:
            entry.update({"status": "pending"})
        else:
            summary = stored.get("summary") or {}
            entry.update(
                {
                    "status": "completed",
                    "results": {"passed": summary.get("passed", 0), "total": summary.get("total", 0)},
                    "compile_error": _has_compile_error(stored),
                    "raw": stored,
                }
            )
        entries.append(entry)

    logger.info("[autotest] results dataset=%s count=%d", dataset_name, len(entries))
    return {"dataset": dataset_name, "files": entries}


@router.get("/files", name="autotest-files")
async def list_autotest_files(dataset: str | None = Query(default=None)) -> dict[str, object]:
    """
    Return the list of dataset files eligible for AutoTest.
    """
    dataset_name, raw_dir = resolve_dataset_or_http_error(dataset, require_raw=True)
    ensure_dataset_ready(dataset_name)
    logger.info("[autotest] list files dataset=%s", dataset_name)
    files = [str(path.relative_to(raw_dir).as_posix()) for path in raw_dir.rglob("*") if path.is_file()]
    return {"dataset": dataset_name, "files": sorted(files)}


class AutoTestRunRequest(BaseModel):
    dataset: str = Field(..., description="Dataset name.")
    filename: str | None = Field(default=None, description="Optional relative filename to scope tests.")


@router.post("/run", status_code=status.HTTP_202_ACCEPTED, name="autotest-run")
async def run_autotest_endpoint(payload: AutoTestRunRequest, background_tasks: BackgroundTasks) -> dict[str, object]:
    """
    Trigger autotests for a dataset (optionally scoped to a single file) in the background.
    """
    dataset_name, raw_dir = resolve_dataset_or_http_error(payload.dataset, require_raw=True)
    ensure_dataset_ready(dataset_name)

    # Validate file path when provided.
    if payload.filename:
        normalized = Path(payload.filename)
        if normalized.is_absolute() or ".." in normalized.parts:
            raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail="Invalid filename.")
        target = (raw_dir / normalized).resolve()
        if not str(target).startswith(str(raw_dir.resolve())) or not target.is_file():
            raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="File not found.")

    autotest_file = (config.DATASETS_DIR / dataset_name / "autotest.yaml").resolve()
    if not autotest_file.exists():
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail="autotest.yaml not found in dataset root.")

    logger.info(
        "[autotest] run accepted dataset=%s target=%s", dataset_name, payload.filename or "<dataset-wide>"
    )
    # Run asynchronously in background and push websocket notifications on completion.
    async def _run() -> None:
        try:
            results = await run_autotest(dataset_name, payload.filename)
            for entry in results:
                notify_tasks_sync(
                    {
                        "type": "autotest-completed",
                        "dataset": dataset_name,
                        "filename": entry.get("target"),
                        "results": entry.get("summary"),
                        "error": entry.get("error"),
                    }
                )
        except Exception as exc:  # pragma: no cover - defensive
            logger.exception("[autotest] run failed dataset=%s target=%s", dataset_name, payload.filename)
            notify_tasks_sync(
                {
                    "type": "autotest-completed",
                    "dataset": dataset_name,
                    "filename": payload.filename,
                    "status": "failed",
                    "error": str(exc),
                }
            )

    background_tasks.add_task(asyncio.run, _run())
    return {"dataset": dataset_name, "filename": payload.filename, "status": "accepted"}


@router.get("/files/results", name="autotest-file-results")
async def get_autotest_results(
    dataset: str | None = Query(default=None), filenames: list[str] = Query(default_factory=list)
) -> dict[str, object]:
    """
    Return stored AutoTest results for multiple files. Accepts repeated `filenames` query parameters.
    """
    return _collect_autotest_results(dataset, filenames)


@router.get("/files/{filename:path}/results", include_in_schema=False)
async def get_autotest_results_legacy(dataset: str | None = Query(default=None), filename: str = "") -> dict[str, object]:
    """
    Legacy single-file path that now delegates to the batch endpoint.
    """
    return _collect_autotest_results(dataset, [filename])
