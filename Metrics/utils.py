from __future__ import annotations

from pathlib import Path

from Files.dataset_manager import dataset_path

SUMMARY_FILENAME = "lizard_dataset.xml"


def dataset_summary_path(dataset: str) -> Path:
    """Return the path to the dataset-wide Lizard summary file."""
    return dataset_path(dataset) / SUMMARY_FILENAME
