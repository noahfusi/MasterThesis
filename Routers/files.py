from __future__ import annotations

import json
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
from Files.dataset_status import dataset_lock, is_ready, load_status, mark_failed, update_status
from Files.excluded_files import load_excluded_files, prune_missing_exclusions, update_excluded_file
from Metrics import SUMMARY_FILENAME, dataset_summary_path
from Metrics.other_metrics import build_reference_metrics, generate_other_metrics
from Metrics.students import build_students_outliers
from Lizard.run_analysis import LIZARD_FOLDER, analyze_dataset, _run_lizard
from LLM import request_embedding, LLMError
from Routers.tasks import notify_tasks_sync
from Routers.utils import (
    ensure_dataset_ready,
    read_utf8_or_error,
    resolve_dataset_or_http_error,
    resolve_relative_file,
)

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
        if not str(target_path).startswith(str(destination)):
            raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail="Invalid archive.")
    archive.extractall(destination)


STRUCTURAL_SOURCE_EXTENSIONS = config.STRUCTURAL_SOURCE_EXTENSIONS
STRUCTURAL_EMBEDDING_SUFFIX = config.STRUCTURAL_EMBEDDING_SUFFIX
RAW_FOLDER = config.RAW_FOLDER_NAME
PROCESSING_PHASES = config.PROCESSING_PHASES


def _chunk_for_embedding(text: str, max_tokens: int) -> list[str]:
    """
    Split text into token-sized chunks based on a simple whitespace token approximation.
    """
    if max_tokens <= 0:
        return [text]

    chunks: list[str] = []
    current_lines: list[str] = []
    current_tokens = 0

    def _count_tokens(line: str) -> int:
        return len(line.split())

    for line in text.splitlines(keepends=True):
        line_tokens = _count_tokens(line)
        if current_tokens and current_tokens + line_tokens > max_tokens:
            chunks.append("".join(current_lines))
            current_lines = []
            current_tokens = 0
        if line_tokens > max_tokens:
            # Fallback: chunk very long single lines by characters to avoid losing content.
            max_chars = max_tokens * 4  # rough char-per-token heuristic
            for start in range(0, len(line), max_chars):
                chunks.append(line[start : start + max_chars])
            continue
        current_lines.append(line)
        current_tokens += line_tokens

    if current_lines:
        chunks.append("".join(current_lines))

    return chunks or [text]


def _process_dataset_pipeline(dataset_name: str) -> None:
    """
    @brief Orchestrate the full dataset processing pipeline (lizard, metrics, outliers, code embeddings).
    @param dataset_name Dataset name to process.
    @note Guarded by a per-dataset lock to avoid concurrent runs.
    """
    raw_dir = dataset_path(dataset_name) / RAW_FOLDER
    with dataset_lock(dataset_name):
        # Avoid duplicate work when a ready status already exists (e.g., concurrent requests).
        if is_ready(load_status(dataset_name)):
            return
        try:
            update_status(
                dataset_name, state="running", phase="analyzing_lizard", message=PROCESSING_PHASES["analyzing_lizard"]
            )
            analyze_dataset(dataset_name, "lizard")

            update_status(
                dataset_name,
                state="running",
                phase="computing_metrics",
                message=PROCESSING_PHASES["computing_metrics"],
            )
            generate_other_metrics(dataset_name)

            update_status(
                dataset_name,
                state="running",
                phase="computing_outliers",
                message=PROCESSING_PHASES["computing_outliers"],
            )
            build_students_outliers(dataset_name)

            update_status(
                dataset_name,
                state="running",
                phase="building_embeddings",
                message="Generating code embeddings.",
            )
            _generate_code_embeddings(dataset_name, raw_dir)

            status_payload = update_status(
                dataset_name, state="ready", phase="ready", message=PROCESSING_PHASES["ready"]
            )
            notify_tasks_sync({"type": "dataset-status", "dataset": dataset_name, "status": status_payload})
        except Exception as exc:  # pragma: no cover - background error path
            status_payload = mark_failed(dataset_name, phase="failed", error=str(exc))
            notify_tasks_sync({"type": "dataset-status", "dataset": dataset_name, "status": status_payload})
            logger.exception("Dataset processing failed for %s: %s", dataset_name, exc)


def _generate_code_embeddings(dataset_name: str, raw_dir: Path | None = None) -> str:
    """
    @brief Build code embeddings directly from raw source files.
    @param dataset_name Name of the dataset to process.
    @param raw_dir Optional pre-resolved raw directory path.
    @return Status message indicating embedding generation outcome.
    """
    raw_dir = raw_dir or (dataset_path(dataset_name) / RAW_FOLDER)
    if not raw_dir.exists():
        return "Code embeddings skipped: no raw files."

    embeddings_root = dataset_path(dataset_name) / config.STRUCTURAL_EMBEDDINGS_FOLDER_NAME
    embeddings_root.mkdir(parents=True, exist_ok=True)

    generated_files = 0
    generated_segments = 0
    skipped_files = 0
    skipped_segments = 0
    candidate_files = 0
    max_tokens = max(0, int(getattr(config, "EMBEDDING_MAX_TOKENS", 0)))

    for file_path in raw_dir.rglob("*"):
        if not file_path.is_file():
            continue
        if file_path.suffix.lower() not in STRUCTURAL_SOURCE_EXTENSIONS:
            continue

        candidate_files += 1
        try:
            code = file_path.read_text(encoding="utf-8")
        except UnicodeDecodeError as exc:
            skipped_files += 1
            logger.warning("Code embedding skipped (decode error) for %s: %s", file_path, exc)
            continue

        if not code.strip():
            skipped_files += 1
            continue

        relative = file_path.relative_to(raw_dir)
        output_path = embeddings_root / relative
        output_path = output_path.with_suffix(output_path.suffix + STRUCTURAL_EMBEDDING_SUFFIX)
        segment_texts = _chunk_for_embedding(code, max_tokens)
        embeddings: list[list[float]] = []

        for index, segment_text in enumerate(segment_texts):
            if not segment_text.strip():
                skipped_segments += 1
                continue
            try:
                embedding = request_embedding(segment_text)
            except (LLMError, ValueError) as exc:
                logger.warning("Code embedding skipped for %s (chunk %s): %s", file_path, index, exc)
                skipped_segments += 1
                continue

            embeddings.append(embedding)
            generated_segments += 1

        try:
            if not embeddings:
                skipped_files += 1
                continue
            dim = len(embeddings[0])
            if dim == 0:
                skipped_files += 1
                continue
            filtered = [vec for vec in embeddings if len(vec) == dim]
            if not filtered:
                skipped_files += 1
                continue
            averaged_embedding = [sum(values) / len(filtered) for values in zip(*filtered)]
            output_path.parent.mkdir(parents=True, exist_ok=True)
            payload = {
                "source": str(relative).replace("\\", "/"),
                "model": config.DEFAULT_OLLAMA_EMBED_MODEL,
                "embedding": averaged_embedding,
                "chunk_count": len(filtered),
            }
            output_path.write_text(json.dumps(payload, ensure_ascii=False), encoding="utf-8")
            generated_files += 1
        except OSError as exc:
            logger.warning("Failed to persist embedding for %s: %s", file_path, exc)
            skipped_files += 1

    if generated_files:
        return f"Code embeddings generated for {generated_files} file(s) across {generated_segments} chunk(s)."
    if skipped_files or candidate_files:
        return "Code embeddings skipped: unable to process files."
    return "No source files available for embeddings."


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
    payload: dict[str, object] = {
        "datasets": list_datasets(),
        "current_dataset": get_current_dataset(),
    }
    if message is not None:
        payload["message"] = message
    if dataset is not None:
        payload["dataset"] = dataset
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

    return load_status(sanitized)


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

    return _dataset_response(message=config.MESSAGES["DATASET_DELETED"].format(dataset=sanitized), dataset=sanitized)


@router.get("/files", name="list-files")
async def list_files(dataset: str | None = None) -> dict[str, object]:
    """
    @brief List raw files contained in a dataset.
    @param dataset Optional dataset name to override the current selection.
    @return Mapping with dataset name and relative file paths.
    """
    dataset_name, raw_dir = resolve_dataset_or_http_error(dataset, require_raw=True)
    files = sorted(str(path.relative_to(raw_dir)).replace("\\", "/") for path in raw_dir.rglob("*") if path.is_file())
    return {"dataset": dataset_name, "files": files}


@router.get("/excluded-files", name="list-excluded-files")
async def list_excluded_files(dataset: str | None = None) -> dict[str, object]:
    """
    @brief List files excluded from clustering for a dataset.
    @param dataset Optional dataset name to override the current selection.
    @return Mapping with dataset name and excluded file paths.
    """
    dataset_name, raw_dir = resolve_dataset_or_http_error(dataset, require_raw=True)
    excluded = prune_missing_exclusions(dataset_name, raw_dir)
    return {"dataset": dataset_name, "excluded_files": excluded}


@router.post("/excluded-files", name="update-excluded-file")
async def set_excluded_file(payload: ExcludedFileToggle) -> dict[str, object]:
    """
    @brief Toggle inclusion of a file in clustering for a dataset.
    @param payload Body containing filename, exclusion flag, and optional dataset override.
    @return Updated exclusion list for the dataset.
    """
    dataset_name, raw_dir = resolve_dataset_or_http_error(payload.dataset, require_raw=True)
    _, relative_name = resolve_relative_file(raw_dir, payload.filename)
    excluded_files = update_excluded_file(dataset_name, relative_name, excluded=payload.excluded)
    return {
        "dataset": dataset_name,
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
    dataset_name, raw_dir = resolve_dataset_or_http_error(dataset, require_raw=True)
    target_path, relative_name = resolve_relative_file(raw_dir, filename)
    try:
        content = target_path.read_text(encoding="utf-8")
        encoding = "utf-8"
    except UnicodeDecodeError:
        content = target_path.read_text(encoding="utf-8", errors="replace")
        encoding = "utf-8"

    return {
        "dataset": dataset_name,
        "filename": relative_name,
        "encoding": encoding,
        "content": content,
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

    reference_dir = dataset_dir / "reference"
    reference_dir.mkdir(parents=True, exist_ok=True)
    reference_name = Path(file.filename).name
    target_path = reference_dir / reference_name
    try:
        contents = await file.read()
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

    requirements_dir = dataset_dir / "requirements"
    requirements_dir.mkdir(parents=True, exist_ok=True)
    requirements_name = Path(file.filename).name
    target_path = requirements_dir / requirements_name
    try:
        contents = await file.read()
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

    target_path = dataset_dir / "autotest.yaml"
    try:
        contents = await file.read()
        target_path.write_bytes(contents)
    except OSError as exc:
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=str(exc)) from exc

    return {"dataset": sanitized, "filename": target_path.name, "path": target_path.as_posix()}
