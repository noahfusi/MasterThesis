from __future__ import annotations

from abc import abstractmethod
from dataclasses import dataclass, field, replace
from typing import Any, Mapping, Sequence, Tuple

from Clustering import (
    auto_gmm_with_bic,
    auto_hdbscan_with_dbcv,
    auto_kmeans_with_silhouette,
    auto_optics_with_silhouette,
    build_feature_dataset,
    project_to_components,
    run_gmm_clustering,
    run_hdbscan_clustering,
    run_kmeans_clustering,
    run_optics_clustering,
)
import config
from Pipeline.base import PipelineStep
from Pipeline.repository import ArtifactRepository


@dataclass(frozen=True)
class ClusteringArtifact:
    dataset: str
    base_files: Tuple[Mapping[str, Any], ...]
    embeddings: Mapping[str, Sequence[float]]
    metrics_lookup: Mapping[str, Mapping[str, Any]]
    metric_keys: Tuple[str, ...] = field(default_factory=tuple)
    normalized: Any | None = None
    result: Any | None = None
    projection: Tuple[Tuple[float, float], ...] | None = None
    axes: Tuple[str, str] | None = None
    metadata: Mapping[str, Any] = field(default_factory=dict)

    def with_normalized(self, normalized: Any, metric_keys: Sequence[str]) -> "ClusteringArtifact":
        return replace(self, normalized=normalized, metric_keys=tuple(metric_keys))

    def with_result(self, result: Any, parameters: Mapping[str, Any]) -> "ClusteringArtifact":
        merged = dict(self.metadata)
        merged["parameters"] = dict(parameters)
        return replace(self, result=result, metadata=merged)

    def with_projection(self, projection: Sequence[tuple[float, float]], axes: Sequence[str]) -> "ClusteringArtifact":
        return replace(self, projection=tuple(tuple(p) for p in projection), axes=tuple(axes[:2]))

    def with_metadata(self, **updates: Any) -> "ClusteringArtifact":
        merged = dict(self.metadata)
        merged.update(updates)
        return replace(self, metadata=merged)


@dataclass(frozen=True)
class BuildFeaturesStepConfig:
    metric_keys: Sequence[str]
    feature_mode: str
    embedding_dims: int


class BuildFeaturesStep(PipelineStep):
    """Build normalized feature dataset from metrics and embeddings."""

    def __init__(self, config: BuildFeaturesStepConfig) -> None:
        super().__init__("BuildFeaturesStep")
        self.config = config

    def run(self, artifact: ClusteringArtifact) -> ClusteringArtifact:
        normalized = build_feature_dataset(
            artifact.base_files,
            artifact.embeddings,
            feature_mode=self.config.feature_mode,
            embedding_dims=self.config.embedding_dims,
            metric_keys=list(self.config.metric_keys),
        )
        response_metric_keys: list[str] = list(normalized.metric_keys) if normalized.metric_keys else []
        if not response_metric_keys and artifact.metrics_lookup:
            sample_metrics = next(iter(artifact.metrics_lookup.values()))
            response_metric_keys = sorted(sample_metrics.keys())
        return artifact.with_normalized(normalized, response_metric_keys)


@dataclass(frozen=True)
class AlgorithmStepConfig:
    feature_mode: str
    embedding_dims: int


class AlgorithmStepBase(PipelineStep):
    """Abstract clustering algorithm step."""

    def __init__(self, name: str, config: AlgorithmStepConfig) -> None:
        super().__init__(name)
        self.config = config

    def run(self, artifact: ClusteringArtifact) -> ClusteringArtifact:
        normalized = artifact.normalized
        if normalized is None:
            raise ValueError("Normalized dataset is required before clustering.")
        result, params = self._run_algorithm(normalized)
        merged = dict(params or {})
        merged["feature_mode"] = self.config.feature_mode
        merged["embedding_dims"] = self.config.embedding_dims
        return artifact.with_result(result, merged)

    @abstractmethod
    def _run_algorithm(self, normalized) -> tuple[Any, Mapping[str, Any]]:
        raise NotImplementedError


@dataclass(frozen=True)
class KMeansStepConfig(AlgorithmStepConfig):
    cluster_count: int


class KMeansStep(AlgorithmStepBase):
    """Standard k-means clustering."""

    def __init__(self, config: KMeansStepConfig) -> None:
        super().__init__("KMeansStep", config)

    def _run_algorithm(self, normalized) -> tuple[Any, Mapping[str, Any]]:
        cc = self.config.cluster_count
        if not cc:
            raise ValueError("Cluster count is required for kmeans.")
        result = run_kmeans_clustering(normalized, cc)
        return result, {"cluster_count": cc}


@dataclass(frozen=True)
class AutoKMeansStepConfig(AlgorithmStepConfig):
    pass


class AutoKMeansStep(AlgorithmStepBase):
    """Automatic k-means clustering using silhouette scoring."""

    def __init__(self, config: AutoKMeansStepConfig) -> None:
        super().__init__("AutoKMeansStep", config)

    def _run_algorithm(self, normalized) -> tuple[Any, Mapping[str, Any]]:
        result, chosen = auto_kmeans_with_silhouette(normalized)
        return result, {"mode": "auto", **(chosen or {})}


@dataclass(frozen=True)
class GMMStepConfig(AlgorithmStepConfig):
    cluster_count: int
    covariance_type: str | None = "full"


class GMMStep(AlgorithmStepBase):
    """Gaussian Mixture Models clustering."""

    def __init__(self, config: GMMStepConfig) -> None:
        super().__init__("GMMStep", config)

    def _run_algorithm(self, normalized) -> tuple[Any, Mapping[str, Any]]:
        cc = self.config.cluster_count
        cov_type = self.config.covariance_type or "full"
        if not cc:
            raise ValueError("Component count is required for GMM.")
        result = run_gmm_clustering(normalized, cc, covariance_type=cov_type, reg_covar=1e-3, n_init=5)
        return result, {"cluster_count": cc, "covariance_type": cov_type}


@dataclass(frozen=True)
class AutoGMMStepConfig(AlgorithmStepConfig):
    cluster_count: int | None = None
    covariance_type: str | None = "full"


class AutoGMMStep(AlgorithmStepBase):
    """Automatic GMM clustering using BIC for model selection."""

    def __init__(self, config: AutoGMMStepConfig) -> None:
        super().__init__("AutoGMMStep", config)

    def _run_algorithm(self, normalized) -> tuple[Any, Mapping[str, Any]]:
        counts = [self.config.cluster_count] if self.config.cluster_count else None
        cov_type = self.config.covariance_type or "full"
        result, chosen = auto_gmm_with_bic(
            normalized,
            component_counts=counts,
            covariance_types=[cov_type] if cov_type else None,
            reg_covar=1e-3,
            n_init=5,
        )
        return result, {"mode": "auto", **(chosen or {})}


@dataclass(frozen=True)
class HDBSCANStepConfig(AlgorithmStepConfig):
    min_cluster_size: int | None = None
    min_samples: int | None = None


class HDBSCANStep(AlgorithmStepBase):
    """Manual HDBSCAN clustering with sensible defaults."""

    def __init__(self, config: HDBSCANStepConfig) -> None:
        super().__init__("HDBSCANStep", config)

    def _run_algorithm(self, normalized) -> tuple[Any, Mapping[str, Any]]:
        heuristic = max(3, min(len(normalized.entries) // 8 or 2, 25))
        min_cluster_size = self.config.min_cluster_size or heuristic
        min_cluster_size = max(2, min(min_cluster_size, len(normalized.entries)))
        min_samples = self.config.min_samples or max(1, min_cluster_size // 2)
        min_samples = max(1, min(min_samples, min_cluster_size))
        result = run_hdbscan_clustering(normalized, min_cluster_size=min_cluster_size, min_samples=min_samples)
        return result, {"min_cluster_size": min_cluster_size, "min_samples": min_samples}


@dataclass(frozen=True)
class AutoHDBSCANStepConfig(AlgorithmStepConfig):
    pass


class AutoHDBSCANStep(AlgorithmStepBase):
    """Automatic HDBSCAN clustering using DBCV scoring."""

    def __init__(self, config: AutoHDBSCANStepConfig) -> None:
        super().__init__("AutoHDBSCANStep", config)

    def _run_algorithm(self, normalized) -> tuple[Any, Mapping[str, Any]]:
        result, chosen = auto_hdbscan_with_dbcv(normalized)
        return result, {"mode": "auto", **(chosen or {})}


@dataclass(frozen=True)
class OpticsStepConfig(AlgorithmStepConfig):
    min_samples: int | None = None
    xi: float | None = None
    max_eps: float | None = None


class OpticsStep(AlgorithmStepBase):
    """Manual OPTICS clustering with defaults."""

    def __init__(self, config: OpticsStepConfig) -> None:
        super().__init__("OpticsStep", config)

    def _run_algorithm(self, normalized) -> tuple[Any, Mapping[str, Any]]:
        heuristic = max(2, min(len(normalized.entries) // 8 or 2, 25))
        selected_min_samples = max(2, self.config.min_samples or heuristic)
        xi_value = self.config.xi if self.config.xi is not None else 0.05
        eps_value = self.config.max_eps if self.config.max_eps not in (None, 0, 0.0) else None
        result = run_optics_clustering(normalized, min_samples=selected_min_samples, xi=xi_value, max_eps=eps_value)
        return result, {"min_samples": selected_min_samples, "xi": xi_value, "max_eps": eps_value}


@dataclass(frozen=True)
class AutoOpticsStepConfig(AlgorithmStepConfig):
    min_samples: int | None = None
    xi: float | None = None
    max_eps: float | None = None


class AutoOpticsStep(AlgorithmStepBase):
    """Automatic OPTICS clustering using silhouette scoring."""

    def __init__(self, config: AutoOpticsStepConfig) -> None:
        super().__init__("AutoOpticsStep", config)

    def _run_algorithm(self, normalized) -> tuple[Any, Mapping[str, Any]]:
        result, chosen = auto_optics_with_silhouette(
            normalized,
            min_samples_values=[self.config.min_samples] if self.config.min_samples else None,
            xi_values=[self.config.xi] if self.config.xi is not None else None,
            max_eps_values=[self.config.max_eps] if self.config.max_eps is not None else None,
        )
        return result, {"mode": "auto", **(chosen or {})}


@dataclass(frozen=True)
class ProjectionStepConfig:
    components: int = 2


class ProjectionStep(PipelineStep):
    """Project normalized data to lower dimensions."""

    def __init__(self, config: ProjectionStepConfig = ProjectionStepConfig()) -> None:
        super().__init__("ProjectionStep")
        self.config = config

    def run(self, artifact: ClusteringArtifact) -> ClusteringArtifact:
        normalized = artifact.normalized
        if normalized is None:
            raise ValueError("Normalized dataset is required for projection.")
        projection, axes = project_to_components(normalized, components=self.config.components)
        if not projection:
            projection = [(0.0, 0.0) for _ in normalized.entries]
        if len(axes) < 2:
            axes = ["Component 1", "Component 2"]
        return artifact.with_projection(projection, axes)


@dataclass(frozen=True)
class AssembleClustersStepConfig:
    repository: ArtifactRepository
    theme_key: str
    theme_config: Mapping[str, Any]
    algorithm: str
    feature_mode: str
    embedding_dims: int
    metrics_lookup: Mapping[str, Mapping[str, Any]]
    excluded_lookup: set[str]
    generate_descriptions: bool


class AssembleClustersStep(PipelineStep):
    """Assemble clustering payload and background jobs."""

    def __init__(self, config: AssembleClustersStepConfig) -> None:
        super().__init__("AssembleClustersStep")
        self.config = config

    def run(self, artifact: ClusteringArtifact) -> ClusteringArtifact:
        result = artifact.result
        normalized = artifact.normalized
        projection = artifact.projection or ()
        axes = artifact.axes or ("Component 1", "Component 2")
        if result is None or normalized is None:
            raise ValueError("Clustering result is required.")

        response_metric_keys = list(artifact.metric_keys)
        metric_keys = list(response_metric_keys)
        metrics_lookup = self.config.metrics_lookup
        excluded_lookup = self.config.excluded_lookup

        points_payload: list[dict[str, object]] = []
        cluster_metric_sums: dict[int, dict[str, float]] = {}
        cluster_counts: dict[int, int] = {}
        dataset_metric_averages: dict[str, float] = {}
        for key in response_metric_keys:
            values = [
                entry.metrics.get(key)
                for entry in normalized.entries
                if isinstance(entry.metrics.get(key), (int, float, float))
            ]
            if values:
                dataset_metric_averages[key] = sum(values) / len(values)

        rep_candidates: dict[int, list[tuple[str, float | None]]] = {}
        for idx, (entry, label, coords) in enumerate(zip(normalized.entries, result.labels, projection)):
            metrics_snapshot_base = metrics_lookup.get(entry.path, {})
            metrics_snapshot = {key: metrics_snapshot_base.get(key) for key in metric_keys} if metric_keys else metrics_snapshot_base
            probs = None
            if result.probabilities and idx < len(result.probabilities):
                probs = result.probabilities[idx]
            if label >= 0:
                cluster_counts[label] = cluster_counts.get(label, 0) + 1
                sums = cluster_metric_sums.setdefault(label, {})
                for key, value in (metrics_snapshot.items() if metrics_snapshot else []):
                    if isinstance(value, (int, float)):
                        sums[key] = sums.get(key, 0.0) + float(value)
                rep_candidates.setdefault(label, []).append(
                    (entry.path, probs[label] if isinstance(probs, list) and len(probs) > label else None)
                )
            x, y = coords
            points_payload.append(
                {
                    "path": entry.path,
                    "cluster": label,
                    "x": x,
                    "y": y,
                    "metrics": metrics_snapshot,
                    "probabilities": probs,
                }
            )

        clusters_payload: list[dict[str, object]] = []
        background_jobs: list[dict[str, object]] = []
        parameters = artifact.metadata.get("parameters", {})
        for info in result.clusters:
            averages: dict[str, float] = {}
            count = cluster_counts.get(info.id, 0)
            if count:
                sums = cluster_metric_sums.get(info.id, {})
                for key in metric_keys:
                    total = sums.get(key)
                    if total is not None:
                        averages[key] = total / count
            base_label = f"Cluster {info.id + 1}"
            prompt_text = None
            representative_paths: list[dict[str, object]] = []
            if self.config.generate_descriptions:
                try:
                    candidates = rep_candidates.get(info.id, [])
                    candidates_sorted = sorted(
                        candidates,
                        key=lambda item: (item[1] is not None, item[1] if item[1] is not None else 0.0),
                        reverse=True,
                    )
                    selected = candidates_sorted[:3] if candidates_sorted else []
                    reps_text_parts: list[str] = []
                    for path, conf in selected:
                        representative_paths.append({"path": path, "confidence": float(conf) if conf is not None else None})
                        try:
                            content, _ = self.config.repository.read_raw_file(path)
                        except Exception:
                            content = None
                        snippet = (content or "").strip()
                        sanitized_snippet = snippet[:2000] + ("\n...[truncated for safety]..." if len(snippet) > 2000 else "")
                        reps_text_parts.append(
                            f"File: {path} (confidence {conf:.2f} if available)\n{sanitized_snippet or 'Content unavailable.'}"
                        )
                    reps_text = "\n\n".join(reps_text_parts) if reps_text_parts else "N/A"
                    prompt_text = config.CLUSTER_LABEL_PROMPT.format(
                        cluster_metrics=averages,
                        dataset_metrics=dataset_metric_averages,
                        representatives=reps_text,
                    )
                except Exception:
                    prompt_text = None
            cluster_entry = {
                "id": info.id,
                "label": base_label,
                "size": info.size,
                "centroid": info.centroid,
                "metrics": averages,
                "description": None,
                "llm_output": None,
                "llm_prompt": prompt_text,
                "llm_pending": bool(prompt_text),
                "comparison": [],
                "good": [],
                "bad": [],
                "representative_paths": representative_paths,
            }
            clusters_payload.append(cluster_entry)
            if prompt_text and self.config.generate_descriptions:
                background_jobs.append(
                    {
                        "dataset": artifact.dataset,
                        "theme": self.config.theme_key,
                        "algorithm": self.config.algorithm,
                        "parameters": dict(parameters),
                        "cluster": dict(cluster_entry),
                        "label_prefix": base_label,
                    }
                )

        axes_payload = {"x": axes[0], "y": axes[1]}
        metadata_payload = {
            "dataset": artifact.dataset,
            "theme": self.config.theme_key,
            "theme_label": self.config.theme_config.get("label") or self.config.theme_key,
            "algorithm": self.config.algorithm,
            "feature_mode": self.config.feature_mode,
            "embedding_dims": self.config.embedding_dims,
            "axes": axes_payload,
            "metrics": response_metric_keys,
            "parameters": dict(parameters),
            "timestamp": None,
            "point_count": len(points_payload),
            "cluster_count": len(clusters_payload),
            "excluded_count": len(excluded_lookup),
            "excluded_files": sorted(excluded_lookup),
            "generate_descriptions": self.config.generate_descriptions,
        }

        response = {
            "dataset": artifact.dataset,
            "theme": self.config.theme_key,
            "theme_label": self.config.theme_config.get("label") or self.config.theme_key,
            "theme_metrics": list(self.config.theme_config.get("metrics") or ()),
            "algorithm": self.config.algorithm,
            "parameters": dict(parameters),
            "metrics": response_metric_keys,
            "points": points_payload,
            "clusters": clusters_payload,
            "axes": axes_payload,
            "metadata": metadata_payload,
            "noise": result.noise,
        }

        return artifact.with_metadata(
            response=response, background_jobs=background_jobs, clusters=clusters_payload, points=points_payload
        )
