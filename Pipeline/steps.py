from __future__ import annotations

import logging
from dataclasses import dataclass
from pathlib import Path
from statistics import mean
from typing import Iterable, Mapping

import config
from LLM import LLMError, request_embedding
from Lizard.metrics import parse_summary_metrics

from .artifacts import CodeArtifact
from .base import PipelineStep
from .embeddings import chunk_for_embedding
from .repository import ArtifactRepository

logger = logging.getLogger(__name__)


@dataclass(frozen=True)
class LizardMetricsStepConfig:
    repository: ArtifactRepository
    lizard_bin: str = "lizard"


class LizardMetricsStep(PipelineStep):
    """Populate Lizard metrics in the artifact metadata, generating them on demand."""

    def __init__(self, config: LizardMetricsStepConfig) -> None:
        super().__init__(name="LizardMetricsStep")
        self.config = config

    def run(self, artifact: CodeArtifact) -> CodeArtifact:
        if artifact.metadata.get("lizard_metrics") is not None:
            return artifact
        summary_text = self.config.repository.ensure_lizard_summary(self.config.lizard_bin)
        try:
            metric_defs, files = parse_summary_metrics(summary_text)
        except ValueError as exc:
            logger.warning("Failed to parse Lizard summary for %s: %s", artifact.dataset, exc)
            return artifact
        lizard_metadata = {"definitions": metric_defs, "files": files}
        return artifact.with_metadata(lizard_metrics=lizard_metadata)


@dataclass(frozen=True)
class MetricsStepConfig:
    repository: ArtifactRepository


class MetricsStep(PipelineStep):
    """Generate derived metrics and attach them to the artifact metadata."""

    def __init__(self, config: MetricsStepConfig) -> None:
        super().__init__(name="MetricsStep")
        self.config = config

    def run(self, artifact: CodeArtifact) -> CodeArtifact:
        status = self.config.repository.persist_other_metrics()
        entries = self.config.repository.list_metrics_entries()
        return artifact.with_metadata(metrics_status=status, metrics=entries)


@dataclass(frozen=True)
class StudentsOutliersStepConfig:
    repository: ArtifactRepository


class StudentsOutliersStep(PipelineStep):
    """Compute students outliers and mark completion in metadata."""

    def __init__(self, config: StudentsOutliersStepConfig) -> None:
        super().__init__(name="StudentsOutliersStep")
        self.config = config

    def run(self, artifact: CodeArtifact) -> CodeArtifact:
        self.config.repository.persist_students_outliers()
        return artifact.with_metadata(students_outliers=True)


@dataclass(frozen=True)
class EmbeddingsStepConfig:
    repository: ArtifactRepository
    source_extensions: Iterable[str]
    max_tokens: int
    model: str


class EmbeddingsStep(PipelineStep):
    """Generate code embeddings for structural files and persist them."""

    def __init__(self, config: EmbeddingsStepConfig) -> None:
        super().__init__(name="EmbeddingsStep")
        self.config = config
        self._extensions = {ext.lower() for ext in config.source_extensions}

    def run(self, artifact: CodeArtifact) -> CodeArtifact:
        produced: dict[str, list[float]] = {}
        existing_embeddings = dict(artifact.embeddings or {})
        for relative, code in artifact.files.items():
            if not self._should_embed(relative):
                continue
            if not code.strip():
                continue
            if self.config.repository.embedding_exists(relative):
                continue
            payload = self._build_embedding_payload(relative, code)
            if payload is None:
                continue
            try:
                self.config.repository.persist_embedding(relative, payload)
            except OSError as exc:  # pragma: no cover - filesystem errors
                logger.warning("Failed to persist embedding for %s: %s", relative, exc)
                continue
            produced[relative] = payload.get("embedding", [])

        if not produced:
            return artifact
        merged_embeddings = dict(existing_embeddings)
        merged_embeddings.update(produced)
        return artifact.with_embeddings(merged_embeddings)

    def _should_embed(self, relative_path: str) -> bool:
        suffix = Path(relative_path).suffix.lower()
        return suffix in self._extensions

    def _build_embedding_payload(self, relative: str, code: str) -> Mapping[str, object] | None:
        segments = chunk_for_embedding(code, self.config.max_tokens)
        embeddings: list[list[float]] = []
        for index, segment in enumerate(segments):
            if not segment.strip():
                continue
            try:
                embedding = request_embedding(segment)
            except (LLMError, ValueError) as exc:
                logger.warning("Embedding skipped for %s (chunk %s): %s", relative, index, exc)
                continue
            if isinstance(embedding, list):
                embeddings.append(embedding)

        if not embeddings:
            return None

        dimension = len(embeddings[0])
        filtered = [vector for vector in embeddings if len(vector) == dimension]
        if not filtered or dimension == 0:
            return None

        averaged = [mean(values) for values in zip(*filtered)]
        return {
            "source": relative,
            "model": self.config.model,
            "embedding": averaged,
            "chunk_count": len(filtered),
        }
