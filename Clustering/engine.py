from __future__ import annotations

from dataclasses import dataclass
import math
from typing import Sequence

import config
try:  # pragma: no cover - required for k-means
    from sklearn.cluster import KMeans as _sklearn_KMeans  # type: ignore
except Exception as exc:  # pragma: no cover
    _sklearn_KMeans = None
    _SKLEARN_KMEANS_IMPORT_ERROR = exc
else:
    _SKLEARN_KMEANS_IMPORT_ERROR = None

try:  # pragma: no cover - optional dependency
    from cuml.cluster import HDBSCAN as _cuml_HDBSCAN  # type: ignore
except Exception:  # pragma: no cover - optional dependency
    _cuml_HDBSCAN = None
try:  # pragma: no cover - optional dependency
    import hdbscan as _hdbscan  # type: ignore
except Exception:  # pragma: no cover - optional dependency
    _hdbscan = None
    _HDBSCAN_IMPORT_ERROR = None
from Clustering.utils import (
    average_vector,
    center_vectors,
    collect_metric_vectors,
    coerce_numeric,
    default_axes,
    dot_product,
    euclidean_distance,
    pca_components,
    project_embeddings,
    robust_scale_matrix,
)
try:  # pragma: no cover - optional dependency for CH score
    from sklearn.metrics import calinski_harabasz_score as _calinski_harabasz_score  # type: ignore
except Exception:  # pragma: no cover
    _calinski_harabasz_score = None

METRIC_KEYS: list[str] = list(config.CLUSTERING_METRIC_KEYS)


@dataclass(slots=True)
class NormalizedEntry:
    """
    @brief Single normalized file entry containing metrics and normalized vector.
    """

    path: str
    metrics: dict[str, float]
    vector: list[float]


@dataclass(slots=True)
class NormalizedDataset:
    """
    @brief Container for metric keys and their normalized entries.
    """
    metric_keys: list[str]
    entries: list[NormalizedEntry]


@dataclass(slots=True)
class ClusterInfo:
    """
    @brief Summary information for a discovered cluster.
    """
    id: int
    size: int
    centroid: list[float]


@dataclass(slots=True)
class ClusteringResult:
    """
    @brief Result of a clustering run including labels and cluster summaries.
    """
    labels: list[int]
    clusters: list[ClusterInfo]
    noise: int = 0
    probabilities: list[list[float]] | None = None


def normalize_dataset(files: Sequence[dict[str, object]], metric_keys: Sequence[str] | None = None) -> NormalizedDataset:
    """
    @brief Extract and normalize metric vectors for clustering.
    @param files Raw file entries containing metrics.
    @param metric_keys Optional ordered metric keys to retain.
    @return NormalizedDataset with filtered entries and normalized vectors.
    """

    candidate_keys = list(metric_keys or METRIC_KEYS)
    if not candidate_keys:
        return NormalizedDataset(metric_keys=[], entries=[])

    def _collect(keys: list[str]) -> list[tuple[str, dict[str, float], list[float]]]:
        collected_local: list[tuple[str, dict[str, float], list[float]]] = []
        for index, entry in enumerate(files):
            metrics = entry.get("metrics")
            if not isinstance(metrics, dict):
                continue

            raw_values: list[float] = []
            raw_metrics: dict[str, float] = {}
            for key in keys:
                raw_value = coerce_numeric(metrics.get(key))
                if raw_value is None:
                    raw_values = []
                    raw_metrics = {}
                    break
                raw_metrics[key] = raw_value
                raw_values.append(raw_value)

            if len(raw_values) != len(keys):
                continue

            path = (
                entry.get("path")
                or entry.get("raw_path")
                or entry.get("filename")
                or entry.get("file")
                or f"entry-{index}"
            )
            collected_local.append((str(path).replace("\\", "/"), raw_metrics, raw_values))
        return collected_local

    collected = _collect(candidate_keys)

    # Fallback to intersection of available numeric keys to keep as many files as possible
    if not collected or len(collected) < len(files):
        numeric_key_sets: list[set[str]] = []
        for entry in files:
            metrics = entry.get("metrics")
            if not isinstance(metrics, dict):
                continue
            numeric_keys = {str(k) for k, v in metrics.items() if coerce_numeric(v) is not None}
            if numeric_keys:
                numeric_key_sets.append(numeric_keys)
        if numeric_key_sets:
            intersection_keys = set.intersection(*numeric_key_sets) if numeric_key_sets else set()
            ordered_keys = [k for k in METRIC_KEYS if k in intersection_keys] + [
                k for k in sorted(intersection_keys) if k not in METRIC_KEYS
            ]
            fallback_keys = [k for k in ordered_keys if k]
            if fallback_keys:
                fallback_collected = _collect(fallback_keys)
                if fallback_collected and len(fallback_collected) > len(collected):
                    candidate_keys = fallback_keys
                    collected = fallback_collected

    if not collected:
        return NormalizedDataset(metric_keys=candidate_keys, entries=[])

    raw_matrix = [raw_values for _, _, raw_values in collected]
    normalized_matrix = robust_scale_matrix(raw_matrix)

    normalized_entries: list[NormalizedEntry] = []
    for (path, metrics, _), normalized in zip(collected, normalized_matrix):
        normalized_entries.append(NormalizedEntry(path=path, metrics=metrics, vector=list(normalized)))

    return NormalizedDataset(metric_keys=candidate_keys, entries=normalized_entries)


def build_feature_dataset(
    files: Sequence[dict[str, object]],
    embeddings: dict[str, Sequence[float]],
    *,
    feature_mode: str,
    embedding_dims: int,
    metric_keys: Sequence[str] | None = None,
) -> NormalizedDataset:
    """
    @brief Assemble a normalized dataset using metrics, embeddings, or both.
    @param files Raw metric entries.
    @param embeddings Mapping of path to embedding vector.
    @param feature_mode One of ("metrics", "embeddings", "both").
    @param embedding_dims Number of PCA dimensions when embeddings are used.
    @param metric_keys Metric keys to retain for clustering and display.
    @return NormalizedDataset ready for clustering.
    """
    mode = (feature_mode or "metrics").lower()
    keys = list(metric_keys or METRIC_KEYS) if mode in {"metrics", "both"} else []

    metric_vectors: dict[str, list[float]] = {}
    metric_payloads: dict[str, dict[str, float]] = {}
    if keys:
        metric_vectors, metric_payloads = collect_metric_vectors(files, keys)
        initial_count = len(metric_vectors)
        if not metric_vectors or len(metric_vectors) < len(files):
            numeric_key_sets: list[set[str]] = []
            for entry in files:
                metrics = entry.get("metrics")
                if not isinstance(metrics, dict):
                    continue
                numeric_keys = {str(k) for k, v in metrics.items() if coerce_numeric(v) is not None}
                if numeric_keys:
                    numeric_key_sets.append(numeric_keys)
            if numeric_key_sets:
                intersection_keys = set.intersection(*numeric_key_sets) if numeric_key_sets else set()
                ordered_keys = [k for k in METRIC_KEYS if k in intersection_keys] + [
                    k for k in sorted(intersection_keys) if k not in METRIC_KEYS
                ]
                fallback_keys = [k for k in ordered_keys if k]
                if fallback_keys:
                    fallback_vectors, fallback_payloads = collect_metric_vectors(files, fallback_keys)
                    if len(fallback_vectors) > initial_count:
                        keys = fallback_keys
                        metric_vectors = fallback_vectors
                        metric_payloads = fallback_payloads

        if metric_vectors:
            scaled_metrics = robust_scale_matrix(list(metric_vectors.values()))
            metric_vectors = {path: vector for path, vector in zip(metric_vectors.keys(), scaled_metrics)}

    embedding_vectors: dict[str, list[float]] = {}
    if mode in {"embeddings", "both"} and embeddings:
        embedding_vectors = project_embeddings(embeddings, embedding_dims)

    # Determine eligible paths according to the chosen mode
    if mode == "metrics":
        paths = list(metric_vectors.keys())
    elif mode == "embeddings":
        paths = list(embedding_vectors.keys())
    else:  # both
        paths = [path for path in metric_vectors if path in embedding_vectors]
        available = len(paths)
        if available == 0:
            warnings.warn(
                "No overlapping entries between metrics and embeddings; clustering dataset will be empty.",
                RuntimeWarning,
            )
        elif available < max(len(metric_vectors), len(embedding_vectors)) * 0.25:
            warnings.warn(
                "Low overlap between metrics and embeddings; clustering will use a small subset of files.",
                RuntimeWarning,
            )

    combined_vectors: list[list[float]] = []
    normalized_entries: list[NormalizedEntry] = []
    for path in paths:
        metric_part = metric_vectors.get(path, [])
        embedding_part = embedding_vectors.get(path, [])
        if mode == "metrics":
            combined = metric_part
        elif mode == "embeddings":
            combined = embedding_part
        else:
            combined = embedding_part + metric_part
        combined_vectors.append(combined)
        normalized_entries.append(
            NormalizedEntry(
                path=path,
                metrics=metric_payloads.get(path, {}),
                vector=list(combined),
            )
        )

    if combined_vectors:
        scaled = robust_scale_matrix(combined_vectors)
        for entry, vector in zip(normalized_entries, scaled):
            entry.vector = vector
    return NormalizedDataset(metric_keys=keys, entries=normalized_entries)


def run_kmeans_clustering(dataset: NormalizedDataset, cluster_count: int, max_iterations: int = 100) -> ClusteringResult:
    """
    @brief Run a simple k-means clustering over normalized entries.
    @param dataset Normalized dataset to cluster.
    @param cluster_count Desired number of clusters.
    @param max_iterations Maximum iterations before convergence check.
    @return ClusteringResult with labels and cluster summaries.
    """

    if not dataset.entries:
        return ClusteringResult(labels=[], clusters=[], noise=0)

    if _sklearn_KMeans is None:
        raise RuntimeError("scikit-learn is required for k-means clustering.") from _SKLEARN_KMEANS_IMPORT_ERROR

    points = [entry.vector for entry in dataset.entries]
    k = max(1, min(cluster_count, len(points)))
    model = _sklearn_KMeans(n_clusters=k, random_state=42, n_init=10, max_iter=max_iterations)
    labels = model.fit_predict(points).tolist()
    centroids = model.cluster_centers_.tolist() if hasattr(model, "cluster_centers_") else None

    probabilities: list[list[float]] | None = None
    try:
        distances = model.transform(points)  # shape (n_samples, k)
        probabilities = _fuzzy_memberships(distances)
        labels = [int(max(range(len(probs)), key=lambda idx: probs[idx])) for probs in probabilities]
    except Exception:
        probabilities = None

    clusters = _build_cluster_summaries(labels, points, centroids_override=centroids)
    return ClusteringResult(labels=labels, clusters=clusters, noise=0, probabilities=probabilities)


def auto_kmeans_with_silhouette(dataset: NormalizedDataset, cluster_counts: Sequence[int] | None = None) -> tuple[ClusteringResult, dict[str, object]]:
    """
    @brief Explore several k-means cluster counts and pick the best using Calinski–Harabasz score.
    @param dataset Normalized dataset to cluster.
    @param cluster_counts Optional explicit list of candidate cluster counts.
    @return Best clustering result and associated parameters (including calinski_harabasz).
    """
    if not dataset.entries:
        return ClusteringResult(labels=[], clusters=[], noise=0), {}

    entry_count = len(dataset.entries)
    if entry_count <= 2:
        target_k = max(1, entry_count)
        result = run_kmeans_clustering(dataset, cluster_count=target_k)
        return result, {"cluster_count": target_k, "calinski_harabasz": None}

    max_clusters = min(entry_count - 1, max(10, int(math.sqrt(entry_count)) + 2))

    if cluster_counts is None:
        candidate_set = {
            2,
            3,
            4,
            5,
            6,
            min(8, max_clusters),
            min(10, max_clusters),
            max(2, min(int(math.sqrt(entry_count)), max_clusters)),
            max(2, min(entry_count // 4, max_clusters)),
            max(2, min(entry_count // 6, max_clusters)),
            max_clusters,
        }
    else:
        candidate_set = {
            max(2, min(int(count), max_clusters)) for count in cluster_counts if isinstance(count, (int, float))
        }

    candidates = sorted(count for count in candidate_set if 2 <= count <= max_clusters)
    if not candidates:
        candidates = [2]

    best_score: float | None = None
    best_result: ClusteringResult | None = None
    best_params: dict[str, object] = {}
    points = [entry.vector for entry in dataset.entries]

    for count in candidates:
        result = run_kmeans_clustering(dataset, cluster_count=count)
        ch_score = None
        if _calinski_harabasz_score is not None:
            try:
                ch_score = float(_calinski_harabasz_score(points, result.labels))
            except Exception:
                ch_score = None
        score = ch_score
        comparative_score = -float("inf") if score is None else score
        current_best_count = best_params.get("cluster_count")
        if (
            best_score is None
            or comparative_score > best_score
            or (comparative_score == best_score and (current_best_count is None or count < current_best_count))
        ):
            best_score = comparative_score
            best_result = result
            best_params = {"cluster_count": count, "calinski_harabasz": score}

    if best_result is None:
        fallback = candidates[0]
        best_result = run_kmeans_clustering(dataset, cluster_count=fallback)
        best_params = {"cluster_count": fallback, "calinski_harabasz": None}

    return best_result, best_params


def run_hdbscan_clustering(
    dataset: NormalizedDataset, min_cluster_size: int = 5, *, min_samples: int | None = None
) -> ClusteringResult:
    """
    @brief Perform density-based clustering using hdbscan when available.
    @param dataset Normalized dataset to cluster.
    @param min_cluster_size Minimum cluster size threshold.
    @param min_samples Minimum samples for core points (defaults to heuristic).
    @return ClusteringResult with labels, clusters, and noise count.
    """

    if not dataset.entries:
        return ClusteringResult(labels=[], clusters=[], noise=0)

    points = [entry.vector for entry in dataset.entries]
    min_cluster_size = max(2, min_cluster_size)
    min_samples = max(1, min_samples or min_cluster_size // 2 or 1)

    if _cuml_HDBSCAN is None and _hdbscan is None:
        raise RuntimeError("hdbscan (CPU or GPU) is required for clustering.")

    labels: list[int] | None = None
    if _cuml_HDBSCAN is not None:
        # Try GPU-accelerated HDBSCAN (fast path when CUDA/cuML is installed)
        try:
            clusterer = _cuml_HDBSCAN(min_cluster_size=min_cluster_size, min_samples=min_samples)
            labels = clusterer.fit_predict(points).tolist()
        except Exception:
            labels = None
    if labels is None and _hdbscan is not None:
        # Fallback to CPU HDBSCAN if GPU path is unavailable
        clusterer = _hdbscan.HDBSCAN(min_cluster_size=min_cluster_size, min_samples=min_samples, prediction_data=True)
        labels = clusterer.fit_predict(points).tolist()

    if not labels or not any(label >= 0 for label in labels):
        raise RuntimeError("HDBSCAN did not produce any clusters.")

    clusters = _build_cluster_summaries(labels, points)
    probabilities: list[list[float]] | None = None
    try:
        if _hdbscan is not None and "clusterer" in locals():
            membership = _hdbscan.all_points_membership_vectors(clusterer)
            probabilities = [[float(x) for x in row] for row in membership]
    except Exception:
        probabilities = None

    noise = sum(1 for label in labels if label == -1)
    return ClusteringResult(labels=labels, clusters=clusters, noise=noise, probabilities=probabilities)


def _compute_dbcv(points: list[list[float]], labels: list[int]) -> float | None:
    """
    @brief Compute Density-Based Clustering Validation (DBCV) score if available.
    @param points List of feature vectors.
    @param labels Cluster labels from HDBSCAN.
    @return DBCV score in [-1, 1] or None if unavailable.
    """
    if _hdbscan is None or not hasattr(_hdbscan, "validity"):
        return None
    try:
        return float(_hdbscan.validity.validity_index(points, labels))  # type: ignore[attr-defined]
    except Exception:
        return None


def auto_hdbscan_with_dbcv(
    dataset: NormalizedDataset,
    min_cluster_sizes: Sequence[int] | None = None,
    min_samples: Sequence[int] | None = None,
) -> tuple[ClusteringResult, dict[str, object]]:
    """
    @brief Search HDBSCAN parameters using DBCV score as a guide.
    @param dataset Normalized dataset to cluster.
    @param min_cluster_sizes Optional candidate min_cluster_size values.
    @param min_samples Optional candidate min_samples values.
    @return Tuple of best clustering result and chosen parameters (including dbcv score).
    """
    if not dataset.entries:
        return ClusteringResult(labels=[], clusters=[], noise=0), {}

    entry_count = len(dataset.entries)
    default_sizes = sorted(
        {
            max(2, entry_count // 10),
            max(2, entry_count // 6),
            max(2, entry_count // 4),
            max(2, int(math.sqrt(entry_count))),
        }
    )
    size_candidates = list(min_cluster_sizes or default_sizes) or [2]
    size_candidates = [max(2, min(size, entry_count)) for size in size_candidates]

    def sample_candidates(size: int) -> list[int]:
        return list(
            {
                max(1, size // 3),
                max(1, size // 2),
                size,
            }
        )

    sample_candidates_override = list(min_samples) if min_samples is not None else None

    best_score = None
    best_result: ClusteringResult | None = None
    best_params: dict[str, object] = {}
    # Grid search on min_cluster_size/min_samples with DBCV guidance
    for size in size_candidates:
        samples_for_size = (
            [max(1, min(sample, size)) for sample in sample_candidates_override]
            if sample_candidates_override is not None
            else [max(1, min(value, size)) for value in sample_candidates(size)]
        )
        for samples in samples_for_size:
            try:
                result = run_hdbscan_clustering(dataset, min_cluster_size=size, min_samples=samples)
            except RuntimeError:
                continue
            dbcv_score = _compute_dbcv([entry.vector for entry in dataset.entries], result.labels)
            comparative_score = -1.0 if dbcv_score is None else dbcv_score
            if best_score is None or comparative_score > best_score:
                best_score = comparative_score
                best_result = result
                best_params = {"min_cluster_size": size, "min_samples": samples, "dbcv": dbcv_score}

    if best_result is None:
        fallback_size = size_candidates[0]
        fallback_samples = sample_candidates(fallback_size)[0]
        try:
            best_result = run_hdbscan_clustering(dataset, min_cluster_size=fallback_size, min_samples=fallback_samples)
        except RuntimeError as exc:
            raise RuntimeError("HDBSCAN did not produce any clusters with tested parameters.") from exc
        best_params = {"min_cluster_size": fallback_size, "min_samples": fallback_samples, "dbcv": None}

    return best_result, best_params


def project_to_components(dataset: NormalizedDataset, components: int = 2) -> tuple[list[tuple[float, float]], list[str]]:
    """
    @brief Project normalized vectors to principal components for visualization.
    @param dataset Normalized dataset containing vectors.
    @param components Number of components to project onto.
    @return Tuple of 2D coordinates and axis labels.
    """

    if not dataset.entries or components <= 0:
        return [], []

    vectors = [entry.vector for entry in dataset.entries]
    dims = len(vectors[0]) if vectors else 0
    if dims == 0:
        return [(0.0, 0.0) for _ in vectors], []

    projection_vectors = pca_components(vectors, components)
    if not projection_vectors:
        projection_vectors = default_axes(dims, components)

    centered = center_vectors(vectors)
    coords: list[tuple[float, float]] = [
        (
            dot_product(vector, projection_vectors[0]) if projection_vectors else 0.0,
            dot_product(vector, projection_vectors[1]) if len(projection_vectors) > 1 else 0.0,
        )
        for vector in centered
    ]

    axis_labels = [f"Component {index + 1}" for index in range(min(components, len(projection_vectors)))]
    if len(axis_labels) < 2:
        axis_labels.append("Component 2")
    return coords, axis_labels[:2]


# ---- helpers ----------------------------------------------------------------


def _euclidean_distance(a: Sequence[float], b: Sequence[float]) -> float:
    """
    @brief Compute Euclidean distance between two vectors.
    @param a First vector.
    @param b Second vector.
    @return Euclidean distance.
    """
    return math.sqrt(sum((x - y) ** 2 for x, y in zip(a, b)))


def _average_vector(points: Sequence[Sequence[float]]) -> list[float]:
    """
    @brief Compute the centroid of a collection of vectors.
    @param points Sequence of vectors.
    @return Averaged vector or empty list when no points provided.
    """
    if not points:
        return []
    return [sum(values) / len(points) for values in zip(*points)]


def _build_cluster_summaries(
    labels: list[int], points: Sequence[Sequence[float]], centroids_override: Sequence[Sequence[float]] | None = None
) -> list[ClusterInfo]:
    """
    @brief Build cluster summaries from labels and points.
    @param labels Cluster label for each point.
    @param points Normalized vectors corresponding to labels.
    @return List of ClusterInfo describing non-noise clusters.
    """
    buffers: dict[int, list[list[float]]] = {}
    for index, label in enumerate(labels):
        if label < 0:
            continue
        buffers.setdefault(label, []).append(list(points[index]))

    clusters: list[ClusterInfo] = []
    for cluster_id in sorted(buffers):
        members = buffers[cluster_id]
        if centroids_override is not None and cluster_id < len(centroids_override):
            centroid = list(centroids_override[cluster_id])
        else:
            centroid = average_vector(members)
        clusters.append(ClusterInfo(id=cluster_id, size=len(members), centroid=centroid))
    return clusters


def _fuzzy_memberships(distances: Sequence[Sequence[float]], m: float = 2.0) -> list[list[float]]:
    """
    @brief Compute fuzzy c-means style memberships from distance matrix.
    @param distances Matrix of distances (n_samples x k).
    @param m Fuzziness parameter (m > 1).
    @return Membership probabilities per sample.
    """
    memberships: list[list[float]] = []
    for row in distances:
        values = [float(d) for d in row]
        if not values:
            memberships.append([])
            continue
        # Handle zero distance with crisp assignment.
        if any(d == 0 for d in values):
            probs = [0.0 for _ in values]
            for idx, d in enumerate(values):
                if d == 0:
                    probs[idx] = 1.0
            memberships.append(probs)
            continue
        memberships_row: list[float] = []
        for d_ik in values:
            denom = sum(((d_ik / d_jk) ** (2 / (m - 1))) for d_jk in values if d_jk != 0)
            denom = denom or 1.0
            memberships_row.append(1.0 / denom)
        total = sum(memberships_row) or 1.0
        memberships.append([v / total for v in memberships_row])
    return memberships
