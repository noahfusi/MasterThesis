from __future__ import annotations

import json
import shutil
import zipfile
from io import BytesIO
from pathlib import Path

from fastapi import APIRouter, File, Form, HTTPException, Query, UploadFile, status

from Files.dataset_manager import (
    dataset_exists,
    dataset_path,
    get_current_dataset,
    list_datasets,
    normalize_dataset_name,
    set_current_dataset,
)
from Metrics import SUMMARY_FILENAME, dataset_summary_path
from Metrics.other_metrics import (
    build_reference_metrics,
    extract_file_metrics,
    generate_other_metrics,
    load_reference_metrics,
)
from Lizard.run_analysis import LIZARD_FOLDER, analyze_dataset, _run_lizard
from Tree_Sitter.structural_ast import build_rich_structural_representation
from LLM.ollama import DEFAULT_EMBED_MODEL, OllamaError, request_embedding

router = APIRouter(prefix="/datasets", tags=["datasets"])


def _safe_extract(archive: zipfile.ZipFile, destination: Path) -> None:
    destination = destination.resolve()
    for member in archive.infolist():
        member_path = Path(member.filename)
        if member_path.is_absolute() or ".." in member_path.parts:
            raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail="Invalid archive.")
        target_path = (destination / member_path).resolve()
        if not str(target_path).startswith(str(destination)):
            raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail="Invalid archive.")
    archive.extractall(destination)


STRUCTURAL_SOURCE_EXTENSIONS = {".scala"}
STRUCTURAL_EMBEDDING_SUFFIX = ".embedding.json"
RAW_FOLDER = "raw"


def _generate_structural_views(dataset_name: str, raw_dir: Path) -> str:
    structural_root = dataset_path(dataset_name) / "structural"
    structural_root.mkdir(parents=True, exist_ok=True)
    generated = 0
    skipped = 0

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
        except Exception:
            skipped += 1

    if generated:
        return f"Structural AST generated for {generated} file(s)."
    if skipped:
        return "Structural AST skipped: unsupported file types."
    return "No files available for structural AST."


def _load_structural_segments(structural_file: Path) -> list[dict[str, object]]:
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
    structural_root = dataset_path(dataset_name) / "structural"
    if not structural_root.exists():
        return "Structural embeddings skipped: no structural files."

    embeddings_root = dataset_path(dataset_name) / "structural_embeddings"
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
            except (OllamaError, ValueError):
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
    return _dataset_response()


@router.post("", status_code=status.HTTP_201_CREATED, name="create-dataset")
async def create_dataset(name: str = Form(...), file: UploadFile = File(...)) -> dict[str, object]:
    try:
        sanitized_name = normalize_dataset_name(name)
    except ValueError as exc:
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail=str(exc)) from exc

    if dataset_exists(sanitized_name):
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail="Dataset already exists.")

    if not file.filename or not file.filename.lower().endswith(".zip"):
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail="File must be a .zip archive.")

    dataset_dir = dataset_path(sanitized_name)
    raw_dir = dataset_dir / "raw"

    try:
        raw_dir.mkdir(parents=True, exist_ok=False)
    except FileExistsError as exc:
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail="Dataset already exists.") from exc

    file_bytes = await file.read()
    try:
        with zipfile.ZipFile(BytesIO(file_bytes)) as archive:
            _safe_extract(archive, raw_dir)
    except zipfile.BadZipFile as exc:
        shutil.rmtree(dataset_dir, ignore_errors=True)
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail="Invalid .zip archive.") from exc
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

    analysis_status = None
    try:
        analyze_dataset(sanitized_name, "lizard")
        analysis_status = "Lizard analysis completed."
    except RuntimeError as exc:
        analysis_status = f"Lizard analysis failed: {exc}"

    other_metrics_status = None
    try:
        other_metrics_status = generate_other_metrics(sanitized_name)
    except Exception as exc:  # pragma: no cover - derived metrics errors
        other_metrics_status = f"Other metrics failed: {exc}"

    structural_status = None
    try:
        structural_status = _generate_structural_views(sanitized_name, raw_dir)
    except Exception as exc:  # pragma: no cover - optional dependency
        structural_status = f"Structural AST failed: {exc}"

    embedding_status = None
    try:
        embedding_status = _generate_structural_embeddings(sanitized_name)
    except Exception as exc:  # pragma: no cover - external dependency
        embedding_status = f"Structural embeddings failed: {exc}"

    extra = {}
    if analysis_status:
        extra["analysis_status"] = analysis_status
    if other_metrics_status:
        extra["other_metrics_status"] = other_metrics_status
    if structural_status:
        extra["structural_status"] = structural_status
    if embedding_status:
        extra["embedding_status"] = embedding_status

    return _dataset_response(message="Dataset created", dataset=sanitized_name, extra=extra or None)


@router.delete("/{dataset_name}", status_code=status.HTTP_200_OK, name="delete-dataset")
async def delete_dataset(dataset_name: str) -> dict[str, object]:
    try:
        sanitized = normalize_dataset_name(dataset_name)
    except ValueError as exc:
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail=str(exc)) from exc

    target_dir = dataset_path(sanitized)
    if not target_dir.exists():
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Dataset not found.")

    shutil.rmtree(target_dir)

    if get_current_dataset() == sanitized:
        set_current_dataset(None)

    return _dataset_response(message="Dataset deleted", dataset=sanitized)


def _resolve_dataset_folder(dataset: str | None) -> tuple[str, Path]:
    dataset_name = dataset or get_current_dataset()
    if not dataset_name:
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail="No dataset selected.")
    try:
        sanitized = normalize_dataset_name(dataset_name)
    except ValueError as exc:
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail=str(exc)) from exc

    base_dir = dataset_path(sanitized)
    if not base_dir.exists():
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Dataset not found.")

    raw_dir = base_dir / "raw"
    if not raw_dir.exists() or not raw_dir.is_dir():
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Raw folder not found.")
    return sanitized, raw_dir


@router.get("/files", name="list-files")
async def list_files(dataset: str | None = None) -> dict[str, object]:
    dataset_name, raw_dir = _resolve_dataset_folder(dataset)
    files = sorted(str(path.relative_to(raw_dir)).replace("\\", "/") for path in raw_dir.rglob("*") if path.is_file())
    return {"dataset": dataset_name, "files": files}


@router.get("/file", name="file")
async def read_file(filename: str = Query(..., alias="filename"), dataset: str | None = None) -> dict[str, object]:
    dataset_name, raw_dir = _resolve_dataset_folder(dataset)

    relative_path = Path(filename)
    if relative_path.is_absolute() or ".." in relative_path.parts:
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail="Invalid file path.")

    target_path = (raw_dir / relative_path).resolve()
    if not str(target_path).startswith(str(raw_dir.resolve())):
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail="Invalid file path.")

    if not target_path.is_file():
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="File not found.")

    try:
        content = target_path.read_text(encoding="utf-8")
        encoding = "utf-8"
    except UnicodeDecodeError:
        content = target_path.read_text(encoding="utf-8", errors="replace")
        encoding = "utf-8"

    return {
        "dataset": dataset_name,
        "filename": str(relative_path).replace("\\", "/"),
        "encoding": encoding,
        "content": content,
    }


@router.get("/file/lizard", name="file-lizard")
async def read_file_lizard_analysis(
    filename: str | None = Query(None, alias="filename"),
    dataset: str | None = None,
    summary: bool = Query(False, alias="summary"),
) -> dict[str, object]:
    dataset_name, raw_dir = _resolve_dataset_folder(dataset)

    if summary:
        summary_filename = filename or SUMMARY_FILENAME
        summary_path = dataset_summary_path(dataset_name)
        if summary_filename != SUMMARY_FILENAME:
            summary_path = dataset_path(dataset_name) / summary_filename
        if not summary_path.exists():
            raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Lizard analysis not found.")
        try:
            analysis_data = summary_path.read_text(encoding="utf-8")
        except OSError as exc:
            raise HTTPException(
                status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail="Unable to read analysis file."
            ) from exc
        analysis_format = summary_path.suffix.replace(".", "") or "xml"
        return {
            "dataset": dataset_name,
            "filename": summary_path.name,
            "analysis": analysis_data,
            "analysis_format": analysis_format,
        }

    if not filename:
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail="Filename is required.")

    relative_path = Path(filename)
    if relative_path.is_absolute() or ".." in relative_path.parts:
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail="Invalid file path.")

    raw_file = (raw_dir / relative_path).resolve()
    if not str(raw_file).startswith(str(raw_dir.resolve())):
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail="Invalid file path.")

    if not raw_file.is_file():
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="File not found.")

    analysis_dir = dataset_path(dataset_name) / "lizard"
    xml_path = (analysis_dir / relative_path).with_suffix(relative_path.suffix + ".lizard.xml")
    csv_path = (analysis_dir / relative_path).with_suffix(relative_path.suffix + ".lizard.csv")
    json_path = (analysis_dir / relative_path).with_suffix(relative_path.suffix + ".lizard.json")

    analysis_file = None
    analysis_format = None
    if xml_path.exists():
        analysis_file = xml_path
        analysis_format = "xml"
    elif csv_path.exists():
        analysis_file = csv_path
        analysis_format = "csv"
    elif json_path.exists():
        analysis_file = json_path
        analysis_format = "json"
    else:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Lizard analysis not found.")

    try:
        analysis_data = analysis_file.read_text(encoding="utf-8")
    except OSError as exc:
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail="Unable to read analysis file."
        ) from exc

    return {
        "dataset": dataset_name,
        "filename": str(relative_path).replace("\\", "/"),
        "analysis": analysis_data,
        "analysis_format": analysis_format,
    }


@router.get("/metrics/global", name="global-metrics")
async def read_global_metrics(dataset: str | None = None) -> dict[str, object]:
    dataset_name, _ = _resolve_dataset_folder(dataset)
    summary_path = dataset_summary_path(dataset_name)
    if not summary_path.exists():
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Lizard analysis not found.")
    try:
        summary_text = summary_path.read_text(encoding="utf-8")
    except OSError as exc:
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail="Unable to read summary file."
        ) from exc
    metrics, files = extract_file_metrics(dataset_name, summary_text)
    if not metrics or not files:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="No file metrics available.")
    reference = load_reference_metrics(dataset_name)
    return {"dataset": dataset_name, "metrics": metrics, "files": files, "reference": reference}


@router.post("/{dataset_name}/reference", name="upload-reference-file")
async def upload_reference_file(dataset_name: str, file: UploadFile = File(...)) -> dict[str, object]:
    try:
        sanitized = normalize_dataset_name(dataset_name)
    except ValueError as exc:
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail=str(exc)) from exc

    dataset_dir = dataset_path(sanitized)
    if not dataset_dir.exists():
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Dataset not found.")
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
