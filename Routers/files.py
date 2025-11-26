from __future__ import annotations

import json
import shutil
import zipfile
from io import BytesIO
from pathlib import Path

from fastapi import APIRouter, BackgroundTasks, File, Form, HTTPException, Query, UploadFile, status
import logging

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
from Metrics import SUMMARY_FILENAME, dataset_summary_path
from Metrics.other_metrics import build_reference_metrics, generate_other_metrics
from Metrics.students import build_students_outliers
from Lizard.run_analysis import LIZARD_FOLDER, analyze_dataset, _run_lizard
from Tree_Sitter.structural_ast import build_rich_structural_representation
from LLM import request_embedding, LLMError
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


def _process_dataset_pipeline(dataset_name: str) -> None:
    """
    @brief Orchestrate the full dataset processing pipeline (lizard, metrics, outliers, structure, embeddings).
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
                phase="building_structural",
                message=PROCESSING_PHASES["building_structural"],
            )
            _generate_structural_views(dataset_name, raw_dir)

            update_status(
                dataset_name,
                state="running",
                phase="building_embeddings",
                message=PROCESSING_PHASES["building_embeddings"],
            )
            _generate_structural_embeddings(dataset_name)

            update_status(dataset_name, state="ready", phase="ready", message=PROCESSING_PHASES["ready"])
        except Exception as exc:  # pragma: no cover - background error path
            mark_failed(dataset_name, phase="failed", error=str(exc))
def _generate_structural_views(dataset_name: str, raw_dir: Path) -> str:
    """
    @brief Generate structural AST representations for supported source files.
    @param dataset_name Name of the dataset being processed.
    @param raw_dir Path to the dataset raw files directory.
    @return Status message indicating generation outcome.
    """
    structural_root = dataset_path(dataset_name) / config.STRUCTURAL_FOLDER_NAME
    structural_root.mkdir(parents=True, exist_ok=True)
    generated = 0
    skipped = 0
    failures = 0

    for file_path in raw_dir.rglob("*"):
        if not file_path.is_file():
            continue
        if file_path.suffix.lower() not in STRUCTURAL_SOURCE_EXTENSIONS:
            skipped += 1
            continue
        try:
            relative = file_path.relative_to(raw_dir)
            output_path = structural_root / relative
            output_path = output_path.with_suffix(output_path.suffix + ".structural.txt")
            code = file_path.read_text(encoding="utf-8")
            representation = build_rich_structural_representation(code)
            output_path.parent.mkdir(parents=True, exist_ok=True)
            output_path.write_text(representation, encoding="utf-8")
            generated += 1
        except UnicodeDecodeError as exc:
            failures += 1
            logger.warning("Structural view skipped (decode error) for %s: %s", file_path, exc)
        except Exception as exc:  # pragma: no cover - unexpected parse errors
            failures += 1
            logger.exception("Structural view failed for %s: %s", file_path, exc)

    if generated:
        return f"Structural AST generated for {generated} file(s)."
    if failures:
        return f"Structural AST failed for {failures} file(s); see logs."
    if skipped:
        return "Structural AST skipped: unsupported file types."
    return "No files available for structural AST."


def _load_structural_segments(structural_file: Path) -> list[dict[str, object]]:
    """
    @brief Load structural segments from a structural representation file.
    @param structural_file Path to a structural file (text or JSON).
    @return List of segment dictionaries or a single entry with raw text.
    """
    try:
        content = structural_file.read_text(encoding="utf-8")
    except OSError:
        return []

    content = content.strip()
    if not content:
        return []

    suffix = structural_file.suffix.lower()
    if suffix.endswith(".json") or content.startswith("{") or content.startswith("["):
        try:
            parsed = json.loads(content)
        except json.JSONDecodeError:
            parsed = None
        if parsed is not None:
            segments: list[dict[str, object]] = []

            def visit(node: dict[str, object]) -> None:
                if not isinstance(node, dict):
                    return
                text = node.get("text")
                if isinstance(text, str) and text.strip():
                    segments.append(
                        {
                            "text": text,
                            "type": node.get("type"),
                            "start_line": node.get("start_line"),
                            "end_line": node.get("end_line"),
                        }
                    )
                for child in node.get("children", []) or []:
                    if isinstance(child, dict):
                        visit(child)

            if isinstance(parsed, list):
                for item in parsed:
                    if isinstance(item, dict):
                        visit(item)
            elif isinstance(parsed, dict):
                visit(parsed)

            if segments:
                return segments

    return [{"text": content}]


def _generate_structural_embeddings(dataset_name: str) -> str:
    """
    @brief Build embeddings for structural segments of a dataset.
    @param dataset_name Name of the dataset to process.
    @return Status message indicating embedding generation outcome.
    """
    structural_root = dataset_path(dataset_name) / config.STRUCTURAL_FOLDER_NAME
    if not structural_root.exists():
        return "Structural embeddings skipped: no structural files."

    embeddings_root = dataset_path(dataset_name) / config.STRUCTURAL_EMBEDDINGS_FOLDER_NAME
    embeddings_root.mkdir(parents=True, exist_ok=True)

    generated_segments = 0
    skipped_segments = 0
    for structural_file in structural_root.rglob("*"):
        if not structural_file.is_file():
            continue
        segments = _load_structural_segments(structural_file)
        if not segments:
            skipped_segments += 1
            continue

        relative = structural_file.relative_to(structural_root)
        output_path = embeddings_root / relative
        output_path = output_path.with_suffix(output_path.suffix + STRUCTURAL_EMBEDDING_SUFFIX)
        embedding_records: list[dict[str, object]] = []

        for index, segment in enumerate(segments):
            text = segment.get("text")
            if not isinstance(text, str) or not text.strip():
                skipped_segments += 1
                continue
            try:
                embedding = request_embedding(text)
            except (LLMError, ValueError) as exc:
                logger.warning("Embedding generation skipped for %s (segment %s): %s", structural_file, index, exc)
                skipped_segments += 1
                continue

            record = {
                "index": index,
                "text": text,
                "embedding": embedding,
                "type": segment.get("type"),
                "start_line": segment.get("start_line"),
                "end_line": segment.get("end_line"),
            }
            embedding_records.append(record)
            generated_segments += 1

        if not embedding_records:
            continue

        try:
            output_path.parent.mkdir(parents=True, exist_ok=True)
            payload = {
                "source": str(relative).replace("\\", "/"),
                "model": DEFAULT_EMBED_MODEL,
                "segments": embedding_records,
            }
            output_path.write_text(json.dumps(payload, ensure_ascii=False), encoding="utf-8")
        except OSError:
            skipped_segments += len(embedding_records)

    if generated_segments:
        return f"Structural embeddings generated for {generated_segments} segment(s)."
    if skipped_segments:
        return "Structural embeddings skipped: unable to process segments."
    return "No structural segments available for embeddings."


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
