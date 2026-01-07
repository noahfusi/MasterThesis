from __future__ import annotations

from dataclasses import dataclass, field, replace
from typing import Any, Mapping, Sequence, Tuple

from .artifacts import CodeArtifact


@dataclass(frozen=True)
class DatasetArtifact:
    """
    Immutable dataset-level snapshot containing one or more code artifacts plus metadata.
    """

    name: str
    code_artifacts: Tuple[CodeArtifact, ...] = field(default_factory=tuple)
    metadata: Mapping[str, Any] = field(default_factory=dict)

    def with_metadata(self, **metadata_updates: Any) -> "DatasetArtifact":
        merged = dict(self.metadata)
        merged.update(metadata_updates)
        return replace(self, metadata=merged)

    def add_artifacts(self, artifacts: Sequence[CodeArtifact]) -> "DatasetArtifact":
        return replace(self, code_artifacts=tuple(self.code_artifacts) + tuple(artifacts))

    @property
    def status(self) -> Mapping[str, Any]:
        return self.metadata.get("status", {})

    @property
    def is_ready(self) -> bool:
        ready_flag = self.status.get("state")
        return ready_flag == "ready"


class Dataset:
    """
    Lightweight dataset representation exposing code artifacts and metadata without I/O.
    """

    def __init__(self, artifact: DatasetArtifact) -> None:
        self.artifact = artifact

    @property
    def name(self) -> str:
        return self.artifact.name

    @property
    def code_artifacts(self) -> Tuple[CodeArtifact, ...]:
        return self.artifact.code_artifacts

    @property
    def metadata(self) -> Mapping[str, Any]:
        return self.artifact.metadata
