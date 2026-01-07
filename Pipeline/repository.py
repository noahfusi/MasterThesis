from __future__ import annotations

import hashlib
import json
import logging
from pathlib import Path
from typing import Any, Mapping

import config
from Lizard.run_analysis import analyze_dataset
from Metrics import dataset_summary_path
from Metrics.other_metrics import generate_other_metrics, load_metrics_entries
from Metrics.students import build_students_outliers

from .artifacts import CodeArtifact

logger = logging.getLogger(__name__)


class ArtifactRepository:
    """
    Handle persistence concerns for CodeArtifact while keeping steps I/O-free.
    """

    _artifact_cache: dict[str, tuple[str, CodeArtifact]] = {}

    def __init__(self, dataset: str) -> None:
        self.dataset = self._normalize_dataset_name(dataset)
        self.dataset_dir = config.DATASETS_DIR / self.dataset
        self.raw_dir = self.dataset_dir / config.RAW_FOLDER_NAME
        self._embedding_root = self.dataset_dir / config.STRUCTURAL_EMBEDDINGS_FOLDER_NAME

    def load(self) -> CodeArtifact:
        """Load the current artifact state from disk."""
        fingerprint = self._fingerprint()
        cached = self._artifact_cache.get(self.dataset)
        if cached and cached[0] == fingerprint:
            return cached[1]

        files = self._load_raw_files()
        embeddings = self._load_existing_embeddings()
        metadata: dict[str, Any] = {}
        artifact = CodeArtifact(dataset=self.dataset, files=files, embeddings=embeddings, metadata=metadata)
        self._artifact_cache[self.dataset] = (fingerprint, artifact)
        return artifact

    def list_raw_files(self) -> list[str]:
        """Return relative paths of raw files contained in the dataset."""
        if not self.raw_dir.exists():
            return []
        return sorted(str(path.relative_to(self.raw_dir).as_posix()) for path in self.raw_dir.rglob("*") if path.is_file())

    def read_raw_file(self, relative_path: str) -> tuple[str, str]:
        """
        Read a single raw file by relative path.

        Returns (content, encoding) using UTF-8 with replacement fallback.
        """
        resolved = self._resolve_raw_path(relative_path)
        try:
            content = resolved.read_text(encoding="utf-8")
            encoding = "utf-8"
        except UnicodeDecodeError:
            content = resolved.read_text(encoding="utf-8", errors="replace")
            encoding = "utf-8"
        return content, encoding

    def _load_raw_files(self) -> dict[str, str]:
        if not self.raw_dir.exists():
            raise RuntimeError(f"Dataset '{self.dataset}' has no '{config.RAW_FOLDER_NAME}' directory.")
        files: dict[str, str] = {}
        for path in sorted(self.raw_dir.rglob("*")):
            if not path.is_file():
                continue
            try:
                content = path.read_text(encoding="utf-8")
            except UnicodeDecodeError:
                logger.warning("Skipping non-text file during load: %s", path)
                continue
            relative = path.relative_to(self.raw_dir).as_posix()
            files[relative] = content
        return files

    def _load_existing_embeddings(self) -> dict[str, list[float]]:
        embeddings: dict[str, list[float]] = {}
        if not self._embedding_root.exists():
            return embeddings
        suffix = config.STRUCTURAL_EMBEDDING_SUFFIX
        for path in self._embedding_root.rglob(f"*{suffix}"):
            if not path.is_file():
                continue
            try:
                payload = json.loads(path.read_text(encoding="utf-8"))
            except (OSError, json.JSONDecodeError):
                continue
            relative = path.relative_to(self._embedding_root).as_posix()
            if relative.endswith(suffix):
                relative = relative[: -len(suffix)]
            embedding = payload.get("embedding")
            if isinstance(embedding, list):
                embeddings[relative] = embedding
        return embeddings

    def load_embeddings(self) -> dict[str, list[float]]:
        """Public accessor for stored embeddings."""
        return self._load_existing_embeddings()

    def _resolve_raw_path(self, relative_path: str) -> Path:
        normalized = Path(relative_path)
        if normalized.is_absolute() or ".." in normalized.parts:
            raise ValueError("Invalid file path.")
        resolved = (self.raw_dir / normalized).resolve()
        raw_root = self.raw_dir.resolve()
        try:
            resolved.relative_to(raw_root)
        except ValueError:
            raise ValueError("Invalid file path.")
        if not resolved.exists():
            raise FileNotFoundError(f"File not found: {relative_path}")
        return resolved

    def ensure_lizard_summary(self, lizard_bin: str) -> str:
        """
        Ensure the Lizard dataset summary exists and return its content.
        """
        summary_path = dataset_summary_path(self.dataset)
        if not summary_path.exists():
            analyze_dataset(self.dataset, lizard_bin)
        try:
            return summary_path.read_text(encoding="utf-8")
        except OSError as exc:
            raise RuntimeError(f"Unable to read Lizard summary for {self.dataset}: {exc}") from exc

    def persist_other_metrics(self) -> str:
        """Generate derived metrics CSV, returning the generation status."""
        return generate_other_metrics(self.dataset)

    def persist_students_outliers(self) -> None:
        """Compute and persist students outliers."""
        build_students_outliers(self.dataset)

    def list_metrics_entries(self) -> list[dict[str, object]]:
        """Return parsed metrics entries from the generated CSV."""
        return load_metrics_entries(self.dataset)

    def embedding_exists(self, relative_path: str) -> bool:
        """Check whether an embedding file already exists for a relative path."""
        output_path = self._embedding_output_path(relative_path)
        return output_path.exists()

    def persist_embedding(self, relative_path: str, payload: Mapping[str, Any]) -> None:
        """Write a single embedding payload to disk."""
        output_path = self._embedding_output_path(relative_path)
        output_path.parent.mkdir(parents=True, exist_ok=True)
        output_path.write_text(json.dumps(payload, ensure_ascii=False), encoding="utf-8")

    def _embedding_output_path(self, relative_path: str) -> Path:
        target = self._embedding_root / Path(relative_path)
        return target.with_suffix(target.suffix + config.STRUCTURAL_EMBEDDING_SUFFIX)

    def feedback_dir(self) -> Path:
        """Return the feedback output directory path for the dataset."""
        return self.dataset_dir / config.FEEDBACK_OUTPUT_DIRNAME

    def write_feedback(self, relative_filename: str, content: str) -> Path:
        """
        Persist feedback content under the feedback directory, mirroring the code path structure.
        """
        output_root = self.feedback_dir()
        relative = Path(relative_filename)
        target = output_root / relative
        target = target.with_suffix(target.suffix + ".feedback.txt")
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(content, encoding="utf-8")
        return target

    def read_first_requirement(self) -> str | None:
        """
        Return the contents of the first requirements file, if present.
        """
        req_dir = self.dataset_dir / "requirements"
        if not req_dir.exists():
            return None
        for candidate in sorted(req_dir.iterdir()):
            if candidate.is_file():
                try:
                    return candidate.read_text(encoding="utf-8")
                except OSError:
                    continue
        return None

    def _normalize_dataset_name(self, name: str) -> str:
        sanitized = name.strip()
        if not sanitized:
            raise ValueError("Dataset name is required.")
        if sanitized in {".", ".."} or any(sep in sanitized for sep in ("/", "\\")):
            raise ValueError("Dataset name contains invalid characters.")
        return sanitized

    def _fingerprint(self) -> str:
        """Compute a hash representing current raw and embedding files."""
        parts: list[str] = []
        for root in (self.raw_dir, self._embedding_root):
            if not root.exists():
                parts.append(f"{root.name}:missing")
                continue
            for path in sorted(root.rglob("*")):
                if not path.is_file():
                    continue
                try:
                    stat = path.stat()
                except OSError:
                    continue
                rel = path.relative_to(self.dataset_dir).as_posix()
                parts.append(f"{rel}:{stat.st_mtime_ns}:{stat.st_size}")
        payload = "|".join(parts)
        return hashlib.sha256(payload.encode("utf-8")).hexdigest()

    @classmethod
    def clear_cache(cls, dataset: str | None = None) -> None:
        """Clear cached CodeArtifacts."""
        if dataset is None:
            cls._artifact_cache.clear()
        else:
            cls._artifact_cache.pop(dataset, None)
