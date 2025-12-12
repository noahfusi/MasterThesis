from __future__ import annotations

from pathlib import Path

from fastapi import HTTPException, status

from Files.dataset_manager import dataset_path, get_current_dataset, normalize_dataset_name
from Files.dataset_status import is_ready, load_status


def resolve_dataset_or_http_error(dataset: str | None, *, require_raw: bool = True) -> tuple[str, Path]:
    """
    Resolve dataset name from parameter or current selection, enforcing existence checks.

    Returns (dataset_name, raw_dir if require_raw else dataset_dir).
    Raises HTTPException with consistent messages on validation issues.
    """
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

    if require_raw:
        raw_dir = base_dir / "raw"
        if not raw_dir.exists() or not raw_dir.is_dir():
            raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Raw folder not found.")
        return sanitized, raw_dir

    return sanitized, base_dir


def ensure_dataset_ready(dataset: str) -> dict[str, object]:
    """
    @brief Ensure dataset processing is complete before serving dependent requests.
    @param dataset Name of the dataset to validate.
    @return Loaded status payload when ready.
    @throws HTTPException If the dataset processing is incomplete or failed.
    """
    status_payload = load_status(dataset)
    if is_ready(status_payload):
        return status_payload
    detail = status_payload.get("message") or "Dataset processing is not finished."
    if status_payload.get("state") == "failed":
        detail = status_payload.get("error") or detail
        raise HTTPException(status_code=status.HTTP_503_SERVICE_UNAVAILABLE, detail=detail)
    raise HTTPException(status_code=status.HTTP_409_CONFLICT, detail=detail)


def resolve_relative_file(
    raw_dir: Path, filename: str, *, not_found_detail: str = "File not found."
) -> tuple[Path, str]:
    """
    @brief Validate a relative filename and ensure it resolves inside the raw directory.
    @param raw_dir Root directory containing raw dataset files.
    @param filename User-supplied relative filename.
    @param not_found_detail Custom detail message for missing files.
    @return Tuple with resolved absolute path and normalized relative string.
    @throws HTTPException If the path is invalid or the file does not exist.
    """
    relative_path = Path(filename)
    # Block absolute paths and parent traversal attempts early.
    if relative_path.is_absolute() or ".." in relative_path.parts:
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail="Invalid file path.")

    resolved = (raw_dir / relative_path).resolve()
    raw_dir_resolved = raw_dir.resolve()

    # Use is_relative_to for robust path traversal protection (Python 3.9+)
    try:
        resolved.relative_to(raw_dir_resolved)
    except ValueError:
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail="Invalid file path.")

    if not resolved.is_file():
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail=not_found_detail)
    return resolved, relative_path.as_posix()


def read_utf8_or_error(path: Path, *, detail: str) -> str:
    """
    @brief Read a UTF-8 file or raise an HTTP 500 error.
    @param path File path to read.
    @param detail Error message to surface if reading fails.
    @return Text content read from the file.
    @throws HTTPException On OS-level errors during reading.
    """
    try:
        return path.read_text(encoding="utf-8")
    except OSError as exc:
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=detail) from exc


def sanitize_for_llm_prompt(text: str, max_length: int = 10000) -> str:
    """
    @brief Sanitize user-provided text before including in LLM prompts.
    @param text Text to sanitize.
    @param max_length Maximum allowed length (default 10000 characters).
    @return Sanitized text safe for LLM prompts.
    """
    if not text:
        return ""

    # Remove null bytes and control characters except newlines and tabs
    sanitized = "".join(char for char in text if char == "\n" or char == "\t" or (ord(char) >= 32 and ord(char) != 127))

    # Truncate to max length
    if len(sanitized) > max_length:
        sanitized = sanitized[:max_length] + "\n...[truncated for safety]..."

    return sanitized
