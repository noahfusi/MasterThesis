from __future__ import annotations

from pathlib import Path
from typing import Optional

_current_dataset: Optional[str] = None


def _repo():
    from Files.dataset_repository import DATASET_REPOSITORY

    return DATASET_REPOSITORY


def normalize_dataset_name(name: str) -> str:
    return _repo().normalize_dataset_name(name)


def dataset_path(name: str) -> Path:
    """Return the filesystem path for a dataset without creating it."""
    return _repo().dataset_path(name)


def dataset_exists(name: str) -> bool:
    return _repo().dataset_exists(name)


def list_datasets() -> list[str]:
    return _repo().list_datasets()


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
