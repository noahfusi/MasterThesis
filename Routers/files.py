from __future__ import annotations

import logging
import shutil
import zipfile
from io import BytesIO
from pathlib import Path

from fastapi import APIRouter, BackgroundTasks, File, Form, HTTPException, Query, UploadFile, status
from pydantic import BaseModel

import config
from Files.dataset_manager import (
    dataset_exists,
    dataset_path,
    get_current_dataset,
    list_datasets,
    normalize_dataset_name,
    set_current_dataset,
)
from Files.dataset_repository import DATASET_REPOSITORY
from Files.dataset_status import dataset_lock, is_ready, load_status, mark_failed, update_status
from Files.excluded_files import load_excluded_files, prune_missing_exclusions, update_excluded_file
from Lizard.run_analysis import LIZARD_FOLDER, _run_lizard
from Metrics import SUMMARY_FILENAME, dataset_summary_path
from Metrics.other_metrics import build_reference_metrics
from Pipeline import (
    ArtifactRepository,
    EmbeddingsStep,
    EmbeddingsStepConfig,
    LizardMetricsStep,
    LizardMetricsStepConfig,
    MetricsStep,
    MetricsStepConfig,
    Pipeline,
    PipelineStep,
    StudentsOutliersStep,
    StudentsOutliersStepConfig,
)
from Routers.tasks import notify_tasks_sync
from Routers.utils import (
    ensure_dataset_ready,
    read_utf8_or_error,
    resolve_dataset_or_http_error,
    resolve_repository_or_http_error,
    resolve_relative_file,
    resolve_dataset_objects,
)
from Services import dataset_service

router = APIRouter(prefix="/datasets", tags=["datasets"])
logger = logging.getLogger("uvicorn.error")


def _safe_extract(archive: zipfile.ZipFile, destination: Path) -> None:
    """
    @brief Safely extract a ZIP archive while preventing path traversal.
    @param archive Opened ZIP archive to extract.
    @param destination Target directory where files should be extracted.
    @throws HTTPException If an entry attempts to escape the destination.
    """
    destination = destination.resolve()
    for member in archive.infolist():
        member_path = Path(member.filename)
        if member_path.is_absolute() or ".." in member_path.parts:
            raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail="Invalid archive.")
        target_path = (destination / member_path).resolve()

        # Use relative_to for robust path traversal protection
        try:
            target_path.relative_to(destination)
        except ValueError:
            raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail="Invalid archive.")
    archive.extractall(destination)


RAW_FOLDER = config.RAW_FOLDER_NAME
PROCESSING_PHASES = config.PROCESSING_PHASES


def _process_dataset_pipeline(dataset_name: str) -> None:
    """
    @brief Orchestrate the full dataset processing pipeline (lizard, metrics, outliers, code embeddings).
    @param dataset_name Dataset name to process.
    @note Guarded by a per-dataset lock to avoid concurrent runs.
    """
    repository = ArtifactRepository(dataset_name)
    steps_with_phases: list[tuple[str, PipelineStep]] = [
        ("analyzing_lizard", LizardMetricsStep(LizardMetricsStepConfig(repository=repository, lizard_bin="lizard"))),
        ("computing_metrics", MetricsStep(MetricsStepConfig(repository=repository))),
        ("computing_outliers", StudentsOutliersStep(StudentsOutliersStepConfig(repository=repository))),
        (
            "building_embeddings",
            EmbeddingsStep(
                EmbeddingsStepConfig(
                    repository=repository,
                    source_extensions=config.STRUCTURAL_SOURCE_EXTENSIONS,
                    max_tokens=max(0, int(getattr(config, "EMBEDDING_MAX_TOKENS", 0))),
                    model=config.DEFAULT_OLLAMA_EMBED_MODEL,
                )
            ),
        ),
    ]
    pipeline = Pipeline([step for _, step in steps_with_phases])

    with dataset_lock(dataset_name):
        # Avoid duplicate work when a ready status already exists (e.g., concurrent requests).
        if is_ready(load_status(dataset_name)):
            return
        try:
            artifact = repository.load()

            def _before_step(step: PipelineStep, current_artifact) -> None:
                phase = next((phase_name for phase_name, candidate in steps_with_phases if candidate is step), None)
                if not phase:
                    return
                status_payload = update_status(
                    dataset_name,
                    state="running",
                    phase=phase,
                    message=PROCESSING_PHASES.get(phase, ""),
                )
                notify_tasks_sync({"type": "dataset-status", "dataset": dataset_name, "status": status_payload})

            pipeline.run(artifact, before_step=_before_step)

            status_payload = update_status(dataset_name, state="ready", phase="ready", message=PROCESSING_PHASES["ready"])
            notify_tasks_sync({"type": "dataset-status", "dataset": dataset_name, "status": status_payload})
        except Exception as exc:  # pragma: no cover - background error path
            status_payload = mark_failed(dataset_name, phase="failed", error=str(exc))
            notify_tasks_sync({"type": "dataset-status", "dataset": dataset_name, "status": status_payload})
            logger.exception("Dataset processing failed for %s: %s", dataset_name, exc)


class ExcludedFileToggle(BaseModel):
    filename: str
    excluded: bool = True
    dataset: str | None = None


def _dataset_response(
    message: str | None = None, dataset: str | None = None, extra: dict[str, object] | None = None
) -> dict[str, object]:
    """
    @brief Build a standard dataset response payload.
    @param message Optional status or info message.
    @param dataset Dataset name to include in the payload.
    @param extra Additional key/value pairs to merge into the response.
    @return Structured response containing datasets and current selection.
    """
    dataset_summaries = dataset_service.list_dataset_summaries(list_datasets())
    current_name = get_current_dataset()
    current_summary = dataset_service.summarize_dataset(current_name) if current_name else None
    payload: dict[str, object] = {
        "datasets": dataset_summaries,
        "current_dataset": current_summary,
    }
    if message is not None:
        payload["message"] = message
    if dataset is not None:
        payload["dataset"] = dataset_service.summarize_dataset(dataset)
    if extra:
        payload.update(extra)
    return payload


@router.get("", name="list-datasets")
async def list_existing_datasets() -> dict[str, object]:
    """
    @brief List available datasets and current selection.
    @return Payload containing dataset list and current dataset.
    """
    return _dataset_response()


@router.get("/{dataset_name}/status", name="dataset-status")
async def dataset_status(dataset_name: str) -> dict[str, object]:
    """
    @brief Retrieve processing status for a dataset.
    @param dataset_name Name of the dataset.
    @return Status payload including current phase and state.
    """
    try:
        sanitized = normalize_dataset_name(dataset_name)
    except ValueError as exc:
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail=config.MESSAGES["DATASET_INVALID_NAME"]) from exc

    target_dir = dataset_path(sanitized)
    if not target_dir.exists():
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail=config.MESSAGES["DATASET_NOT_FOUND"])

    return dataset_service.summarize_dataset(sanitized)


@router.post("", status_code=status.HTTP_201_CREATED, name="create-dataset")
async def create_dataset(
    name: str = Form(...), file: UploadFile = File(...), background_tasks: BackgroundTasks = None
) -> dict[str, object]:
    """
    @brief Create a new dataset from an uploaded ZIP archive.
    @param name Desired dataset name.
    @param file Uploaded ZIP file containing raw sources.
    @return Response detailing creation status and analysis outcomes.
    @throws HTTPException On validation errors or extraction failures.
    """
    try:
        sanitized_name = normalize_dataset_name(name)
    except ValueError as exc:
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail=config.MESSAGES["DATASET_INVALID_NAME"]) from exc

    if dataset_exists(sanitized_name):
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail=config.MESSAGES["DATASET_ALREADY_EXISTS"])

    if not file.filename or not file.filename.lower().endswith(".zip"):
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail=config.MESSAGES["INVALID_ZIP"])

    # Check file size limit
    if file.size and file.size > config.MAX_DATASET_ZIP_SIZE:
        raise HTTPException(status_code=status.HTTP_413_REQUEST_ENTITY_TOO_LARGE, detail=config.MESSAGES["FILE_TOO_LARGE"])

    dataset_dir = dataset_path(sanitized_name)
    raw_dir = dataset_dir / RAW_FOLDER

    try:
        raw_dir.mkdir(parents=True, exist_ok=False)
    except FileExistsError as exc:
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail=config.MESSAGES["DATASET_ALREADY_EXISTS"]) from exc

    # Extract synchronously, then process in the background
    update_status(
        sanitized_name,
        state="running",
        phase="extracting",
        message=PROCESSING_PHASES["extracting"],
    )

    file_bytes = await file.read()
    # Validate actual size after reading
    if len(file_bytes) > config.MAX_DATASET_ZIP_SIZE:
        shutil.rmtree(dataset_dir, ignore_errors=True)
        raise HTTPException(status_code=status.HTTP_413_REQUEST_ENTITY_TOO_LARGE, detail=config.MESSAGES["FILE_TOO_LARGE"])
    try:
        with zipfile.ZipFile(BytesIO(file_bytes)) as archive:
            _safe_extract(archive, raw_dir)
    except zipfile.BadZipFile as exc:
        shutil.rmtree(dataset_dir, ignore_errors=True)
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail=config.MESSAGES["INVALID_ZIP"]) from exc
    except HTTPException:
        shutil.rmtree(dataset_dir, ignore_errors=True)
        raise
    except Exception as exc:  # pragma: no cover - unexpected errors
        shutil.rmtree(dataset_dir, ignore_errors=True)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Unexpected error while extracting archive.",
        ) from exc

    if get_current_dataset() is None:
        try:
            set_current_dataset(sanitized_name)
        except ValueError:
            pass

    # Queue the heavy processing pipeline; if background_tasks is missing, run inline as a fallback.
    update_status(
        sanitized_name,
        state="queued",
        phase="queued",
        message=config.MESSAGES["DATASET_CREATED_PROCESSING"].format(dataset=sanitized_name),
    )

    if background_tasks is not None:
        background_tasks.add_task(_process_dataset_pipeline, sanitized_name)
    else:
        _process_dataset_pipeline(sanitized_name)

    status_payload = load_status(sanitized_name)
    return _dataset_response(
        message=config.MESSAGES["DATASET_CREATED_PROCESSING"].format(dataset=sanitized_name),
        dataset=sanitized_name,
        extra={"status": status_payload} if status_payload else None,
    )


@router.delete("/{dataset_name}", status_code=status.HTTP_200_OK, name="delete-dataset")
async def delete_dataset(dataset_name: str) -> dict[str, object]:
    """
    @brief Delete a dataset and clear current selection if needed.
    @param dataset_name Name of the dataset to remove.
    @return Response containing updated dataset information.
    @throws HTTPException If the dataset is not found or invalid.
    """
    sanitized, target_dir = resolve_dataset_or_http_error(dataset_name, require_raw=False)

    shutil.rmtree(target_dir)

    if get_current_dataset() == sanitized:
        set_current_dataset(None)

    DATASET_REPOSITORY.clear_cache(sanitized)

    return _dataset_response(message=config.MESSAGES["DATASET_DELETED"].format(dataset=sanitized), dataset=sanitized)


@router.get("/files", name="list-files")
async def list_files(dataset: str | None = None) -> dict[str, object]:
    """
    @brief List raw files contained in a dataset.
    @param dataset Optional dataset name to override the current selection.
    @return Mapping with dataset name and relative file paths.
    """
    dataset_name, dataset_obj, _ = resolve_dataset_objects(dataset, require_raw=True)
    ensure_dataset_ready(dataset_obj)
    files = dataset_service.list_files(dataset_obj)
    return {"dataset": dataset_service.summarize_dataset(dataset_name), "files": files}


@router.get("/excluded-files", name="list-excluded-files")
async def list_excluded_files(dataset: str | None = None) -> dict[str, object]:
    """
    @brief List files excluded from clustering for a dataset.
    @param dataset Optional dataset name to override the current selection.
    @return Mapping with dataset name and excluded file paths.
    """
    dataset_name, dataset_obj, repository = resolve_dataset_objects(dataset, require_raw=True)
    ensure_dataset_ready(dataset_obj)
    raw_dir = repository.raw_dir
    excluded = prune_missing_exclusions(dataset_name, raw_dir)
    summary = dataset_service.summarize_dataset(dataset_name)
    summary["excluded_files"] = excluded
    return {"dataset": summary, "excluded_files": excluded}


@router.post("/excluded-files", name="update-excluded-file")
async def set_excluded_file(payload: ExcludedFileToggle) -> dict[str, object]:
    """
    @brief Toggle inclusion of a file in clustering for a dataset.
    @param payload Body containing filename, exclusion flag, and optional dataset override.
    @return Updated exclusion list for the dataset.
    """
    dataset_name, _, repository = resolve_dataset_objects(payload.dataset, require_raw=True)
    raw_dir = repository.raw_dir
    _, relative_name = resolve_relative_file(raw_dir, payload.filename)
    excluded_files = update_excluded_file(dataset_name, relative_name, excluded=payload.excluded)
    return {
        "dataset": dataset_service.summarize_dataset(dataset_name),
        "filename": relative_name,
        "excluded": payload.excluded,
        "excluded_files": excluded_files,
    }


@router.get("/file", name="file")
async def read_file(filename: str = Query(..., alias="filename"), dataset: str | None = None) -> dict[str, object]:
    """
    @brief Read the contents of a raw file within a dataset.
    @param filename Relative file path to read.
    @param dataset Optional dataset override.
    @return Payload including file content and encoding.
    @throws HTTPException If the path is invalid or file missing.
    """
    dataset_name, dataset_obj, repository = resolve_dataset_objects(dataset, require_raw=True)
    ensure_dataset_ready(dataset_obj)
    _, relative_name = resolve_relative_file(repository.raw_dir, filename)
    try:
        content, encoding = dataset_service.read_file(dataset_obj, relative_name)
    except FileNotFoundError as exc:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="File not found.") from exc

    return {
        "dataset": dataset_service.summarize_dataset(dataset_name),
        "file": {"path": relative_name, "encoding": encoding, "content": content},
    }


@router.get("/file/lizard", name="file-lizard")
async def read_file_lizard_analysis(
    filename: str | None = Query(None, alias="filename"),
    dataset: str | None = None,
    summary: bool = Query(False, alias="summary"),
) -> dict[str, object]:
    """
    @brief Fetch Lizard analysis for a file or dataset summary.
    @param filename Relative file path whose analysis is requested.
    @param dataset Optional dataset override.
    @param summary Whether to return the dataset summary instead of a file.
    @return Analysis payload and format metadata.
    @throws HTTPException On invalid paths or missing analysis files.
    """
    dataset_name, raw_dir = resolve_dataset_or_http_error(dataset, require_raw=True)
    ensure_dataset_ready(dataset_name)

    if summary:
        summary_filename = filename or SUMMARY_FILENAME
        summary_path = dataset_summary_path(dataset_name)
        if summary_filename != SUMMARY_FILENAME:
            summary_path = dataset_path(dataset_name) / summary_filename
        if not summary_path.exists():
            raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Lizard analysis not found.")

        analysis_data = read_utf8_or_error(summary_path, detail="Unable to read analysis file.")
        analysis_format = summary_path.suffix.replace(".", "") or "xml"
        return {
            "dataset": dataset_name,
            "filename": summary_path.name,
            "analysis": analysis_data,
            "analysis_format": analysis_format,
        }

    if not filename:
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail="Filename is required.")

    _, relative_name = resolve_relative_file(raw_dir, filename)
    relative_path = Path(relative_name)

    analysis_dir = dataset_path(dataset_name) / LIZARD_FOLDER
    candidates = [
        ((analysis_dir / relative_path).with_suffix(relative_path.suffix + ".lizard.xml"), "xml"),
        ((analysis_dir / relative_path).with_suffix(relative_path.suffix + ".lizard.csv"), "csv"),
        ((analysis_dir / relative_path).with_suffix(relative_path.suffix + ".lizard.json"), "json"),
    ]

    analysis_file = next((path for path, _ in candidates if path.exists()), None)
    if analysis_file is None:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Lizard analysis not found.")

    analysis_format = next(fmt for path, fmt in candidates if path == analysis_file)
    analysis_data = read_utf8_or_error(analysis_file, detail="Unable to read analysis file.")

    return {
        "dataset": dataset_name,
        "filename": relative_name,
        "analysis": analysis_data,
        "analysis_format": analysis_format,
    }


@router.post("/{dataset_name}/reference", name="upload-reference-file")
async def upload_reference_file(dataset_name: str, file: UploadFile = File(...)) -> dict[str, object]:
    """
    @brief Upload a reference solution and compute its metrics via Lizard.
    @param dataset_name Dataset to associate with the reference file.
    @param file Uploaded code file to analyze.
    @return Computed reference metrics for the uploaded file.
    @throws HTTPException On validation or analysis errors.
    """
    try:
        sanitized = normalize_dataset_name(dataset_name)
    except ValueError as exc:
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail=config.MESSAGES["DATASET_INVALID_NAME"]) from exc

    dataset_dir = dataset_path(sanitized)
    if not dataset_dir.exists():
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail=config.MESSAGES["DATASET_NOT_FOUND"])
    if not file.filename:
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail="Filename is required.")

    # Check file size limit
    if file.size and file.size > config.MAX_REFERENCE_FILE_SIZE:
        raise HTTPException(status_code=status.HTTP_413_REQUEST_ENTITY_TOO_LARGE, detail=config.MESSAGES["FILE_TOO_LARGE"])

    reference_dir = dataset_dir / "reference"
    reference_dir.mkdir(parents=True, exist_ok=True)
    reference_name = Path(file.filename).name
    target_path = reference_dir / reference_name
    try:
        contents = await file.read()
        if len(contents) > config.MAX_REFERENCE_FILE_SIZE:
            raise HTTPException(status_code=status.HTTP_413_REQUEST_ENTITY_TOO_LARGE, detail=config.MESSAGES["FILE_TOO_LARGE"])
        target_path.write_bytes(contents)
    except OSError as exc:
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=str(exc)) from exc

    try:
        analysis_output = _run_lizard("lizard", target_path)
    except RuntimeError as exc:
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=str(exc)) from exc

    try:
        code_text = target_path.read_text(encoding="utf-8")
    except OSError:
        code_text = None
    metrics = build_reference_metrics(sanitized, reference_name, analysis_output, code_text)

    return {"dataset": sanitized, "filename": reference_name, "metrics": metrics}


@router.post("/{dataset_name}/requirements", name="upload-requirements-file")
async def upload_requirements_file(dataset_name: str, file: UploadFile = File(...)) -> dict[str, object]:
    """
    @brief Upload a requirements file and store it inside the dataset folder.
    @param dataset_name Dataset to associate with the requirements file.
    @param file Uploaded requirements file.
    @return Basic payload confirming persistence.
    @throws HTTPException On validation or write errors.
    """
    try:
        sanitized = normalize_dataset_name(dataset_name)
    except ValueError as exc:
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail=str(exc)) from exc

    dataset_dir = dataset_path(sanitized)
    if not dataset_dir.exists():
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail=config.MESSAGES["DATASET_NOT_FOUND"])
    if not file.filename:
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail="Filename is required.")

    # Check file size limit
    if file.size and file.size > config.MAX_REQUIREMENTS_FILE_SIZE:
        raise HTTPException(status_code=status.HTTP_413_REQUEST_ENTITY_TOO_LARGE, detail=config.MESSAGES["FILE_TOO_LARGE"])

    requirements_dir = dataset_dir / "requirements"
    requirements_dir.mkdir(parents=True, exist_ok=True)
    requirements_name = Path(file.filename).name
    target_path = requirements_dir / requirements_name
    try:
        contents = await file.read()
        if len(contents) > config.MAX_REQUIREMENTS_FILE_SIZE:
            raise HTTPException(status_code=status.HTTP_413_REQUEST_ENTITY_TOO_LARGE, detail=config.MESSAGES["FILE_TOO_LARGE"])
        target_path.write_bytes(contents)
    except OSError as exc:
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=str(exc)) from exc

    return {"dataset": sanitized, "filename": requirements_name, "path": target_path.as_posix()}


@router.post("/{dataset_name}/autotest", name="upload-autotest-file")
async def upload_autotest_file(dataset_name: str, file: UploadFile = File(...)) -> dict[str, object]:
    """
    Upload an autotest definition file and persist it as autotest.yaml at the dataset root.
    """
    try:
        sanitized = normalize_dataset_name(dataset_name)
    except ValueError as exc:
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail=str(exc)) from exc

    dataset_dir = dataset_path(sanitized)
    if not dataset_dir.exists():
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail=config.MESSAGES["DATASET_NOT_FOUND"])
    if not file.filename:
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail="Filename is required.")
    if not file.filename.lower().endswith((".yml", ".yaml")):
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail="Autotest file must be YAML.")

    # Check file size limit
    if file.size and file.size > config.MAX_AUTOTEST_FILE_SIZE:
        raise HTTPException(status_code=status.HTTP_413_REQUEST_ENTITY_TOO_LARGE, detail=config.MESSAGES["FILE_TOO_LARGE"])

    target_path = dataset_dir / "autotest.yaml"
    try:
        contents = await file.read()
        if len(contents) > config.MAX_AUTOTEST_FILE_SIZE:
            raise HTTPException(status_code=status.HTTP_413_REQUEST_ENTITY_TOO_LARGE, detail=config.MESSAGES["FILE_TOO_LARGE"])
        target_path.write_bytes(contents)
    except OSError as exc:
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=str(exc)) from exc

    return {"dataset": sanitized, "filename": target_path.name, "path": target_path.as_posix()}
