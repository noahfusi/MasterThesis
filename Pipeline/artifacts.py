from __future__ import annotations

from dataclasses import dataclass, field, replace
from typing import Any, Mapping


@dataclass(frozen=True)
class CodeArtifact:
    """
    Immutable domain object representing the state of code during ingestion.

    The object carries source text, embeddings, and arbitrary metadata
    without performing any I/O itself.
    """

    dataset: str
    files: Mapping[str, str]
    embeddings: Mapping[str, list[float]] | None = None
    metadata: Mapping[str, Any] = field(default_factory=dict)

    def with_metadata(self, **metadata_updates: Any) -> "CodeArtifact":
        """Return a new artifact with merged metadata."""
        merged = dict(self.metadata)
        merged.update(metadata_updates)
        return replace(self, metadata=merged)

    def with_embeddings(self, embeddings: Mapping[str, list[float]] | None) -> "CodeArtifact":
        """Return a new artifact with updated embeddings."""
        return replace(self, embeddings=embeddings or {})
