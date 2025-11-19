from __future__ import annotations

import json
import re
import shutil
import xml.etree.ElementTree as ET
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
from Lizard.run_analysis import LIZARD_FOLDER, analyze_dataset
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
DUPLICATE_RATE_PATTERN = re.compile(r"Total duplicate rate:\s*([0-9.]+)%", re.IGNORECASE)


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


def _safe_number(value: str | None) -> int | float | None:
    if value is None:
        return None
    text = value.strip()
    if not text:
        return None
    try:
        number = float(text)
    except ValueError:
        return None
    if number.is_integer():
        return int(number)
    return number


def _normalize_relative_path(path_str: str) -> str:
    if not path_str:
        return ""
    normalized = path_str.replace("\\", "/")
    marker = f"/{RAW_FOLDER}/"
    lower_normalized = normalized.lower()
    marker_idx = lower_normalized.rfind(marker)
    if marker_idx != -1:
        return normalized[marker_idx + len(marker) :]
    return normalized


def _sanitize_summary_xml(summary_text: str) -> str:
    closing_tag = "</cppncss>"
    idx = summary_text.find(closing_tag)
    if idx != -1:
        return summary_text[: idx + len(closing_tag)]
    return summary_text


def _extract_file_metrics(summary_text: str) -> tuple[list[dict[str, str]], list[dict[str, object]]]:
    xml_payload = _sanitize_summary_xml(summary_text)
    try:
        root = ET.fromstring(xml_payload)
    except ET.ParseError as exc:
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail="Invalid Lizard summary format."
        ) from exc

    measure = root.find(".//measure[@type='File']")
    if measure is None:
        return [], []

    labels_element = measure.find("labels")
    label_texts: list[str] = []
    if labels_element is not None:
        for label_node in labels_element.findall("label"):
            if label_node.text:
                label_texts.append(label_node.text.strip())

    metric_labels = [label for label in label_texts if label and not label.lower().startswith("nr")]
    average_values: dict[str, object] = {}
    capturing_averages = False
    for child in measure:
        if child.tag == "average":
            if not capturing_averages:
                capturing_averages = True
            label_attr = (child.attrib.get("label") or "").strip()
            value_attr = child.attrib.get("value")
            avg_value = _safe_number(value_attr)
            if label_attr and not label_attr.lower().startswith("nr") and avg_value is not None:
                average_values[label_attr] = avg_value
            continue
        if capturing_averages:
            break
    metric_definitions = []
    for label in metric_labels:
        definition: dict[str, object] = {"key": label, "label": label}
        if label in average_values:
            definition["average"] = average_values[label]
        metric_definitions.append(definition)

    if "NCSS" in metric_labels and "Functions" in metric_labels:
        ratio_definition: dict[str, object] = {"key": "NCSS/Functions", "label": "NCSS / Functions"}
        ncss_avg = average_values.get("NCSS")
        functions_avg = average_values.get("Functions")
        if isinstance(ncss_avg, (int, float)) and isinstance(functions_avg, (int, float)) and functions_avg:
            ratio_definition["average"] = ncss_avg / functions_avg
        metric_definitions.append(ratio_definition)

    files: list[dict[str, object]] = []
    for item in measure.findall("item"):
        raw_name = item.attrib.get("name", "")
        relative = _normalize_relative_path(raw_name)
        value_nodes = item.findall("value")
        metrics: dict[str, object] = {}
        for label, value_node in zip(label_texts, value_nodes):
            label_clean = (label or "").strip()
            if not label_clean or label_clean.lower().startswith("nr"):
                continue
            numeric = _safe_number(value_node.text if value_node is not None else None)
            if numeric is not None:
                metrics[label_clean] = numeric
        ncss_value = metrics.get("NCSS")
        functions_value = metrics.get("Functions")
        if isinstance(ncss_value, (int, float)) and isinstance(functions_value, (int, float)) and functions_value:
            metrics["NCSS/Functions"] = ncss_value / functions_value
        files.append(
            {
                "path": relative or raw_name,
                "raw_path": raw_name,
                "metrics": metrics,
            }
        )

    return metric_definitions, files


def _read_file_duplicate_rate(dataset: str, relative_path: str) -> float | None:
    analysis_dir = dataset_path(dataset) / LIZARD_FOLDER
    relative = Path(relative_path)
    analysis_file = analysis_dir / relative
    suffix = analysis_file.suffix
    if suffix:
        analysis_file = analysis_file.with_suffix(suffix + ".lizard.xml")
    else:
        analysis_file = analysis_file.with_suffix(".lizard.xml")
    if not analysis_file.exists():
        return None
    try:
        content = analysis_file.read_text(encoding="utf-8", errors="ignore")
    except OSError:
        return None
    match = DUPLICATE_RATE_PATTERN.search(content)
    if not match:
        return None
    try:
        return float(match.group(1))
    except ValueError:
        return None


def _attach_duplication_metrics(dataset: str, files: list[dict[str, object]], metrics: list[dict[str, object]]) -> None:
    if not files:
        return
    rates: list[float] = []
    for entry in files:
        relative_path = entry.get("path") or entry.get("raw_path")
        if not isinstance(relative_path, str):
            continue
        rate = _read_file_duplicate_rate(dataset, relative_path)
        if rate is None:
            continue
        entry_metrics = entry.get("metrics")
        if not isinstance(entry_metrics, dict):
            entry_metrics = {}
            entry["metrics"] = entry_metrics
        entry_metrics["Duplication (%)"] = rate
        rates.append(rate)
    if rates:
        average_rate = sum(rates) / len(rates)
        metrics.append({"key": "Duplication (%)", "label": "Duplication (%)", "average": average_rate})


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
    metrics, files = _extract_file_metrics(summary_text)
    _attach_duplication_metrics(dataset_name, files, metrics)
    if not metrics or not files:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="No file metrics available.")
    return {"dataset": dataset_name, "metrics": metrics, "files": files}
