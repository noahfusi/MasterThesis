from __future__ import annotations

from abc import ABC, abstractmethod
from typing import Callable, Iterable, Sequence

from .artifacts import CodeArtifact


class PipelineStep(ABC):
    """Abstract base for stateless pipeline steps."""

    name: str

    def __init__(self, name: str | None = None) -> None:
        self.name = name or self.__class__.__name__

    @abstractmethod
    def run(self, artifact: CodeArtifact) -> CodeArtifact:
        """Transform the given artifact and return a new instance."""
        raise NotImplementedError


class Pipeline:
    """Simple orchestrator that runs steps sequentially."""

    def __init__(self, steps: Sequence[PipelineStep] | Iterable[PipelineStep]) -> None:
        self.steps: list[PipelineStep] = list(steps)

    def run(
        self,
        artifact: CodeArtifact,
        before_step: Callable[[PipelineStep, CodeArtifact], None] | None = None,
    ) -> CodeArtifact:
        current = artifact
        for step in self.steps:
            if before_step:
                before_step(step, current)
            current = step.run(current)
        return current
