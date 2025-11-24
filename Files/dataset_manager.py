from __future__ import annotations

from pathlib import Path
from typing import Optional

import config

config.DATASETS_DIR.mkdir(parents=True, exist_ok=True)

_current_dataset: Optional[str] = None


def normalize_dataset_name(name: str) -> str:
    sanitized = name.strip()
    if not sanitized:
        raise ValueError("Dataset name is required.")
    if sanitized in {".", ".."} or any(sep in sanitized for sep in ("/", "\\")):
        raise ValueError("Dataset name contains invalid characters.")
    return sanitized


def dataset_path(name: str) -> Path:
    """Return the filesystem path for a dataset without creating it."""
    sanitized = normalize_dataset_name(name)
    return config.DATASETS_DIR / sanitized


def dataset_exists(name: str) -> bool:
    try:
        return dataset_path(name).is_dir()
    except ValueError:
        return False


def list_datasets() -> list[str]:
    return sorted(p.name for p in config.DATASETS_DIR.iterdir() if p.is_dir())


def get_current_dataset() -> Optional[str]:
    global _current_dataset
    if _current_dataset and not dataset_exists(_current_dataset):
        _current_dataset = None
    return _current_dataset


def set_current_dataset(name: Optional[str]) -> Optional[str]:
    global _current_dataset
    if name is None or name == "":
        _current_dataset = None
        return _current_dataset

    sanitized = normalize_dataset_name(name)
    if not dataset_exists(sanitized):
        raise ValueError("Dataset not found.")
    _current_dataset = sanitized
    return _current_dataset
