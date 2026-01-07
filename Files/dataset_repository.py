from __future__ import annotations

import hashlib
from pathlib import Path

import config
from Pipeline import ArtifactRepository, Dataset, DatasetArtifact


class DatasetRepository:
    """
    Handle dataset-level I/O while leveraging CodeArtifact persistence.
    """

    def __init__(self, datasets_dir: Path | None = None) -> None:
        self.datasets_dir = datasets_dir or config.DATASETS_DIR
        self.datasets_dir.mkdir(parents=True, exist_ok=True)
        # Cache of in-memory artifacts keyed by dataset name with a content fingerprint.
        self._artifact_cache: dict[str, tuple[str, DatasetArtifact]] = {}

    def normalize_dataset_name(self, name: str) -> str:
        sanitized = name.strip()
        if not sanitized:
            raise ValueError("Dataset name is required.")
        if sanitized in {".", ".."} or any(sep in sanitized for sep in ("/", "\\")):
            raise ValueError("Dataset name contains invalid characters.")
        return sanitized

    def dataset_path(self, name: str) -> Path:
        """Return the filesystem path for a dataset without creating it."""
        sanitized = self.normalize_dataset_name(name)
        return self.datasets_dir / sanitized

    def dataset_exists(self, name: str) -> bool:
        try:
            return self.dataset_path(name).is_dir()
        except ValueError:
            return False

    def list_datasets(self) -> list[str]:
        return sorted(p.name for p in self.datasets_dir.iterdir() if p.is_dir())

    def artifact_repository(self, dataset: str) -> ArtifactRepository:
        return ArtifactRepository(dataset)

    def load_metadata(self, dataset: str) -> dict[str, object]:
        """Load dataset metadata without code content."""
        from Files.dataset_status import load_status, is_ready
        from Files.excluded_files import load_excluded_files

        name = self.normalize_dataset_name(dataset)
        status = load_status(name)
        return {
            "status": status,
            "is_ready": is_ready(status),
            "excluded_files": load_excluded_files(name),
        }

    def load_dataset(self, dataset: str) -> Dataset:
        """Load a dataset as a domain object with its code artifacts and metadata."""
        metadata = self.load_metadata(dataset)
        repo = self.artifact_repository(dataset)
        fingerprint = repo._fingerprint()
        cached_fp, cached_artifact = (self._artifact_cache.get(dataset) or (None, None))
        if cached_fp == fingerprint and cached_artifact:
            updated = cached_artifact if cached_artifact.metadata == metadata else cached_artifact.with_metadata(**metadata)
            self._artifact_cache[dataset] = (fingerprint, updated)
            return Dataset(updated)

        code_artifact = repo.load()
        artifact = DatasetArtifact(name=code_artifact.dataset, code_artifacts=(code_artifact,), metadata=metadata)
        self._artifact_cache[dataset] = (fingerprint, artifact)
        return Dataset(artifact)

    def list_files(self, dataset: str) -> list[str]:
        """List raw files for a dataset via its artifact repository."""
        return self.artifact_repository(dataset).list_raw_files()

    def read_file(self, dataset: str, filename: str) -> tuple[str, str]:
        """Read a single file content and encoding via its artifact repository."""
        return self.artifact_repository(dataset).read_raw_file(filename)

    # --- caching helpers -------------------------------------------------

    def _fingerprint_dataset(self, dataset: str) -> str:
        """
        Build a stable fingerprint for a dataset based on raw and embedding files (name + mtime + size).
        """
        return self.artifact_repository(dataset)._fingerprint()

    def clear_cache(self, dataset: str | None = None) -> None:
        """Clear cached artifacts for one dataset or all datasets."""
        if dataset is None:
            self._artifact_cache.clear()
        else:
            self._artifact_cache.pop(dataset, None)
        from Pipeline.repository import ArtifactRepository

        ArtifactRepository.clear_cache(dataset)


# Default singleton instance used by legacy helper functions.
DATASET_REPOSITORY = DatasetRepository()
