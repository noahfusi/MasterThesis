from __future__ import annotations

import json
from pathlib import Path
from typing import Iterable

import config
from Files.dataset_manager import dataset_path

EXCLUDED_FILENAME = config.CLUSTERING_EXCLUDED_FILENAME


def normalize_excluded_path(value: str | None) -> str | None:
    """
    Normalize a relative path for exclusion storage.
    """
    if value is None:
        return None
    normalized = str(value).strip().replace("\\", "/")
    if not normalized or normalized in {".", ".."}:
        return None
    while normalized.startswith("./"):
        normalized = normalized[2:]
    while normalized.startswith("/"):
        normalized = normalized[1:]
    normalized = normalized.replace("//", "/")
    return normalized or None


def exclusions_path(dataset: str) -> Path:
    return dataset_path(dataset) / EXCLUDED_FILENAME


def load_excluded_files(dataset: str) -> list[str]:
    """
    Load the list of clustering-excluded files for a dataset.
    """
    path = exclusions_path(dataset)
    if not path.exists():
        return []
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return []

    if isinstance(payload, dict):
        entries = payload.get("files") or payload.get("excluded")
    else:
        entries = payload
    if not isinstance(entries, list):
        return []

    normalized: set[str] = set()
    for entry in entries:
        normalized_path = normalize_excluded_path(str(entry))
        if normalized_path:
            normalized.add(normalized_path)
    return sorted(normalized)


def save_excluded_files(dataset: str, files: Iterable[str]) -> list[str]:
    """
    Persist the exclusion list for a dataset, returning the normalized entries.
    """
    normalized = sorted({path for path in (normalize_excluded_path(f) for f in files) if path})
    path = exclusions_path(dataset)
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp_path = path.with_suffix(".tmp")
    tmp_path.write_text(json.dumps(normalized, ensure_ascii=False, indent=2), encoding="utf-8")
    tmp_path.replace(path)
    return normalized


def update_excluded_file(dataset: str, filename: str, *, excluded: bool) -> list[str]:
    """
    Add or remove a file from the exclusion list.
    """
    normalized = normalize_excluded_path(filename)
    current = set(load_excluded_files(dataset))
    if not normalized:
        return sorted(current)
    if excluded:
        current.add(normalized)
    else:
        current.discard(normalized)
    return save_excluded_files(dataset, current)


def prune_missing_exclusions(dataset: str, raw_dir: Path | None = None) -> list[str]:
    """
    Drop exclusions that no longer correspond to files in the raw folder.
    """
    excluded = load_excluded_files(dataset)
    if not excluded:
        return []
    if raw_dir is None:
        raw_dir = dataset_path(dataset) / config.RAW_FOLDER_NAME
    if not raw_dir.exists():
        return excluded

    raw_root = raw_dir.resolve()
    filtered: list[str] = []
    for entry in excluded:
        candidate = (raw_root / entry).resolve()
        if str(candidate).startswith(str(raw_root)) and candidate.is_file():
            filtered.append(entry)
    if set(filtered) != set(excluded):
        return save_excluded_files(dataset, filtered)
    return filtered
