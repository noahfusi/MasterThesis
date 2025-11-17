from __future__ import annotations

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
from Lizard.run_analysis import analyze_dataset

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

    extra = {"analysis_status": analysis_status} if analysis_status else None
    return _dataset_response(message="Dataset created", dataset=sanitized_name, extra=extra)


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
