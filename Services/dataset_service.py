from __future__ import annotations

from typing import Any, Iterable

from Files.dataset_repository import DATASET_REPOSITORY
from Pipeline import Dataset


def get_dataset(dataset_name: str) -> Dataset:
    """Load a dataset artifact with its code artifacts and metadata."""
    return DATASET_REPOSITORY.load_dataset(dataset_name)


def summarize_dataset(dataset_name: str) -> dict[str, Any]:
    """Return a lightweight summary for a dataset without loading file contents."""
    if not dataset_name:
        return {}
    metadata = DATASET_REPOSITORY.load_metadata(dataset_name)
    return {
        "name": dataset_name,
        "status": metadata.get("status"),
        "is_ready": bool(metadata.get("is_ready")),
        "excluded_files": metadata.get("excluded_files") or [],
    }


def list_dataset_summaries(names: Iterable[str]) -> list[dict[str, Any]]:
    return [summarize_dataset(name) for name in names]


def list_files(dataset: Dataset) -> list[str]:
    """List files contained in the first code artifact."""
    if not dataset.code_artifacts:
        return []
    artifact = dataset.code_artifacts[0]
    return sorted(artifact.files.keys())


def read_file(dataset: Dataset, filename: str) -> tuple[str, str]:
    """Read a file content from the code artifact; assumes UTF-8."""
    if not dataset.code_artifacts:
        raise FileNotFoundError(filename)
    artifact = dataset.code_artifacts[0]
    if filename not in artifact.files:
        raise FileNotFoundError(filename)
    return artifact.files[filename], "utf-8"
