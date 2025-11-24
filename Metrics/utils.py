from __future__ import annotations

from pathlib import Path

import config
from Files.dataset_manager import dataset_path

SUMMARY_FILENAME = config.SUMMARY_FILENAME


def dataset_summary_path(dataset: str) -> Path:
    """Return the path to the dataset-wide Lizard summary file."""
    return dataset_path(dataset) / SUMMARY_FILENAME
