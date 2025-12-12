from __future__ import annotations

from dataclasses import dataclass
import math
import warnings
from typing import Sequence

import config
try:  # pragma: no cover - required for k-means
    from sklearn.cluster import KMeans as _sklearn_KMeans  # type: ignore
except Exception as exc:  # pragma: no cover
    _sklearn_KMeans = None
    _SKLEARN_KMEANS_IMPORT_ERROR = exc
else:
    _SKLEARN_KMEANS_IMPORT_ERROR = None

try:  # pragma: no cover - optional dependency for OPTICS
    from sklearn.cluster import OPTICS as _sklearn_OPTICS  # type: ignore
except Exception as exc:  # pragma: no cover
    _sklearn_OPTICS = None
    _SKLEARN_OPTICS_IMPORT_ERROR = exc
else:
    _SKLEARN_OPTICS_IMPORT_ERROR = None

try:  # pragma: no cover - optional dependency for Gaussian mixtures
    from sklearn.mixture import GaussianMixture as _sklearn_GaussianMixture  # type: ignore
except Exception as exc:  # pragma: no cover
    _sklearn_GaussianMixture = None
    _SKLEARN_GMM_IMPORT_ERROR = exc
else:
    _SKLEARN_GMM_IMPORT_ERROR = None

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
    pca_components,
    project_embeddings,
    robust_scale_matrix,
    silhouette_score,
)
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
    @brief Run a k-means clustering over normalized entries.
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


def run_gmm_clustering(
    dataset: NormalizedDataset,
    cluster_count: int,
    max_iterations: int = 200,
    covariance_type: str = "full",
    reg_covar: float = 1e-3,
    n_init: int = 5,
) -> ClusteringResult:
    """
    @brief Run a Gaussian Mixture Model clustering over normalized entries.
    @param dataset Normalized dataset to cluster.
    @param cluster_count Desired number of mixture components.
    @param max_iterations Maximum EM iterations before convergence check.
    @param covariance_type Covariance structure for the mixture model.
    @return ClusteringResult with labels, clusters, and membership probabilities.
    """

    if not dataset.entries:
        return ClusteringResult(labels=[], clusters=[], noise=0)

    if _sklearn_GaussianMixture is None:
        raise RuntimeError("scikit-learn is required for Gaussian Mixture clustering.") from _SKLEARN_GMM_IMPORT_ERROR

    points = [entry.vector for entry in dataset.entries]
    k = max(1, min(cluster_count, len(points)))
    model = _sklearn_GaussianMixture(
        n_components=k,
        covariance_type=covariance_type,
        max_iter=max_iterations,
        random_state=42,
        reg_covar=reg_covar,
        n_init=max(1, n_init),
    )
    model.fit(points)
    labels = model.predict(points).tolist()
    unique_labels = {label for label in labels if label is not None}
    if len(unique_labels) <= 1:
        raise RuntimeError("GMM produced a single cluster; adjust component count or data distribution.")
    probabilities: list[list[float]] | None = None
    try:
        probabilities = [[float(x) for x in row] for row in model.predict_proba(points)]
    except Exception:
        probabilities = None

    means = model.means_.tolist() if hasattr(model, "means_") else None
    clusters = _build_cluster_summaries(labels, points, centroids_override=means)
    return ClusteringResult(labels=labels, clusters=clusters, noise=0, probabilities=probabilities)


def auto_gmm_with_bic(
    dataset: NormalizedDataset,
    component_counts: Sequence[int] | None = None,
    covariance_types: Sequence[str] | None = None,
    *,
    reg_covar: float = 1e-3,
    n_init: int = 5,
) -> tuple[ClusteringResult, dict[str, object]]:
    """
    @brief Search Gaussian Mixture hyperparameters using silhouette score (with multi-cluster constraint).
    @param dataset Normalized dataset to cluster.
    @param component_counts Optional candidate component counts.
    @param covariance_types Optional candidate covariance types.
    @return Best clustering result and chosen parameters (including silhouette score).
    """

    if not dataset.entries:
        return ClusteringResult(labels=[], clusters=[], noise=0), {}

    if _sklearn_GaussianMixture is None:
        raise RuntimeError("scikit-learn is required for Gaussian Mixture clustering.") from _SKLEARN_GMM_IMPORT_ERROR

    points = [entry.vector for entry in dataset.entries]
    entry_count = len(points)
    if entry_count <= 1:
        labels = [0] if entry_count == 1 else []
        clusters = _build_cluster_summaries(labels, points)
        result = ClusteringResult(labels=labels, clusters=clusters, noise=0, probabilities=None)
        return result, {"cluster_count": max(1, entry_count), "covariance_type": "full", "silhouette": None, "mode": "auto"}

    max_components = min(entry_count, max(10, int(math.sqrt(entry_count)) + 2))
    default_counts = {
        2,
        3,
        4,
        5,
        min(8, max_components),
        min(10, max_components),
        max(2, min(entry_count // 4, max_components)),
        max(2, min(entry_count // 6, max_components)),
        max_components,
    }
    candidates = {max(1, min(int(count), max_components)) for count in (component_counts or default_counts)}
    candidates = sorted(count for count in candidates if 1 <= count <= max_components)
    cov_types = list(covariance_types or ["full", "tied", "diag", "spherical"])
    if not candidates:
        candidates = [1]
    if not cov_types:
        cov_types = ["full"]

    best_score: float | None = None
    best_result: ClusteringResult | None = None
    best_params: dict[str, object] = {}

    for cov in cov_types:
        for count in candidates:
            try:
                model = _sklearn_GaussianMixture(
                    n_components=count,
                    covariance_type=cov,
                    max_iter=300,
                    random_state=42,
                    reg_covar=reg_covar,
                    n_init=max(1, n_init),
                )
                model.fit(points)
                labels = model.predict(points).tolist()
                unique_labels = {label for label in labels if label is not None}
                if len(unique_labels) <= 1:
                    continue
                probs = [[float(x) for x in row] for row in model.predict_proba(points)]
                means = model.means_.tolist() if hasattr(model, "means_") else None
                clusters = _build_cluster_summaries(labels, points, centroids_override=means)
                result = ClusteringResult(labels=labels, clusters=clusters, noise=0, probabilities=probs)
            except Exception:
                continue

            score = _safe_silhouette(points, labels)
            comparative_score = -float("inf") if score is None else score
            if best_score is None or comparative_score > best_score:
                best_score = comparative_score
                best_result = result
                best_params = {"cluster_count": count, "covariance_type": cov, "silhouette": score}

    if best_result is None:
        raise RuntimeError("GMM auto-tuning could not find a multi-cluster solution.")

    best_params["mode"] = "auto"
    return best_result, best_params


def auto_kmeans_with_silhouette(dataset: NormalizedDataset, cluster_counts: Sequence[int] | None = None) -> tuple[ClusteringResult, dict[str, object]]:
    """
    @brief Explore several k-means cluster counts and pick the best using silhouette score.
    @param dataset Normalized dataset to cluster.
    @param cluster_counts Optional explicit list of candidate cluster counts.
    @return Best clustering result and associated parameters (including silhouette).
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
        score = _safe_silhouette(points, result.labels)
        comparative_score = -float("inf") if score is None else score
        current_best_count = best_params.get("cluster_count")
        if (
            best_score is None
            or comparative_score > best_score
            or (comparative_score == best_score and (current_best_count is None or count < current_best_count))
        ):
            best_score = comparative_score
            best_result = result
            best_params = {"cluster_count": count, "silhouette": score}

    if best_result is None:
        fallback = candidates[0]
        best_result = run_kmeans_clustering(dataset, cluster_count=fallback)
        best_params = {"cluster_count": fallback, "silhouette": None}

    return best_result, best_params


def run_hdbscan_clustering(
    dataset: NormalizedDataset,
    min_cluster_size: int = 5,
    *,
    min_samples: int | None = None,
    cluster_selection_method: str | None = None,
    metric: str = "euclidean",
) -> ClusteringResult:
    """
    @brief Perform density-based clustering using hdbscan when available.
    @param dataset Normalized dataset to cluster.
    @param min_cluster_size Minimum cluster size threshold.
    @param min_samples Minimum samples for core points (defaults to heuristic).
    @param cluster_selection_method HDBSCAN cluster selection strategy.
    @param metric Distance metric to use.
    @return ClusteringResult with labels, clusters, and noise count.
    """

    if not dataset.entries:
        return ClusteringResult(labels=[], clusters=[], noise=0)

    points = [entry.vector for entry in dataset.entries]
    min_cluster_size = max(2, min_cluster_size)
    selected_min_samples = min_samples if min_samples is not None else max(1, min_cluster_size // 2 or 1)
    if selected_min_samples is not None:
        selected_min_samples = max(1, int(selected_min_samples))

    if _cuml_HDBSCAN is None and _hdbscan is None:
        raise RuntimeError("hdbscan (CPU or GPU) is required for clustering.")

    labels: list[int] | None = None
    if _cuml_HDBSCAN is not None:
        # Try GPU-accelerated HDBSCAN (fast path when CUDA/cuML is installed)
        try:
            clusterer_kwargs = {
                "min_cluster_size": min_cluster_size,
                "min_samples": selected_min_samples,
            }
            if cluster_selection_method:
                clusterer_kwargs["cluster_selection_method"] = cluster_selection_method
            if metric:
                clusterer_kwargs["metric"] = metric
            clusterer = _cuml_HDBSCAN(**clusterer_kwargs)
            labels = clusterer.fit_predict(points).tolist()
        except Exception:
            labels = None
    if labels is None and _hdbscan is not None:
        # Fallback to CPU HDBSCAN if GPU path is unavailable
        clusterer = _hdbscan.HDBSCAN(
            min_cluster_size=min_cluster_size,
            min_samples=selected_min_samples,
            prediction_data=True,
            cluster_selection_method=cluster_selection_method,
            metric=metric,
        )
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


def run_optics_clustering(
    dataset: NormalizedDataset,
    *,
    min_samples: int = 5,
    xi: float = 0.05,
    max_eps: float | None = None,
    min_cluster_size: int | None = None,
    metric: str = "euclidean",
) -> ClusteringResult:
    """
    @brief Perform density-based clustering using OPTICS (scikit-learn).
    @param dataset Normalized dataset to cluster.
    @param min_samples Minimum samples for core points.
    @param xi Steepness threshold for Xi cluster extraction.
    @param max_eps Maximum neighborhood radius (None/<=0 -> infinity).
    @param min_cluster_size Optional cluster size constraint.
    @param metric Distance metric to use.
    @return ClusteringResult with labels, clusters, and noise count.
    """

    if not dataset.entries:
        return ClusteringResult(labels=[], clusters=[], noise=0)

    if _sklearn_OPTICS is None:
        raise RuntimeError("scikit-learn is required for OPTICS clustering.") from _SKLEARN_OPTICS_IMPORT_ERROR

    points = [entry.vector for entry in dataset.entries]
    selected_min_samples = max(2, int(min_samples) if min_samples is not None else 5)
    xi_value = max(0.001, min(0.99, float(xi if xi is not None else 0.05)))
    max_eps_value = float(max_eps) if max_eps is not None else math.inf
    if max_eps_value <= 0:
        max_eps_value = math.inf
    min_cluster_size_value = None
    if min_cluster_size is not None:
        min_cluster_size_value = max(2, int(min_cluster_size))

    model = _sklearn_OPTICS(
        min_samples=selected_min_samples,
        xi=xi_value,
        max_eps=max_eps_value,
        min_cluster_size=min_cluster_size_value,
        metric=metric,
    )
    try:
        labels = model.fit_predict(points).tolist()
    except Exception as exc:  # noqa: BLE001
        raise RuntimeError("OPTICS failed to produce clusters.") from exc

    if not labels or not any(label >= 0 for label in labels):
        raise RuntimeError("OPTICS did not produce any clusters.")

    clusters = _build_cluster_summaries(labels, points)
    noise = sum(1 for label in labels if label == -1)
    return ClusteringResult(labels=labels, clusters=clusters, noise=noise, probabilities=None)


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
    @brief Search HDBSCAN parameters using silhouette score (ignoring noise labels).
    @param dataset Normalized dataset to cluster.
    @param min_cluster_sizes Optional candidate min_cluster_size values.
    @param min_samples Optional candidate min_samples values.
    @return Tuple of best clustering result and chosen parameters (including silhouette score).
    """
    if not dataset.entries:
        return ClusteringResult(labels=[], clusters=[], noise=0), {}

    entry_count = len(dataset.entries)
    points = [entry.vector for entry in dataset.entries]

    def _build_hdbscan_param_grid(X: Sequence[Sequence[float]], expected_k_range: tuple[int, int] = (2, 12)) -> dict:
        n = len(X)
        k_min, k_max = expected_k_range
        k_min = max(2, min(k_min, n)) if n else 2
        k_max = max(k_min, min(k_max, max(n, k_min)))
        ks = [k_min, (k_min + k_max) // 2, k_max]
        base_sizes = [max(1, n / k) for k in ks if k > 0]

        raw_mcs: list[int] = []
        for s in base_sizes:
            for factor in (0.5, 0.75, 1.0):
                raw_mcs.append(int(round(s * factor)) or 1)

        min_cluster_sizes_grid = sorted(
            {
                max(2, min(n, max(5, min(n // 2 if n > 1 else 2, m))))
                for m in raw_mcs
                if m > 0
            }
        ) or [max(2, min(n, 5))]

        min_samples_values = set()
        for mcs in min_cluster_sizes_grid:
            min_samples_values.add(None)  # library default behavior
            for factor in (0.3, 0.6):
                candidate = int(round(factor * mcs))
                candidate = max(1, min(max(20, mcs), candidate))
                min_samples_values.add(candidate)

        min_samples_grid = sorted(min_samples_values, key=lambda x: (x is not None, x if x is not None else -1))

        adjusted_min_cluster_sizes = [3,4,5,6] + min_cluster_sizes_grid
        adjusted_min_samples = [None, 1, 2, 3, 4, 5] + min_samples_grid

        return {
            "min_cluster_size": adjusted_min_cluster_sizes,
            "min_samples": adjusted_min_samples,
            "cluster_selection_method": ["eom", "leaf"],
            "metric": ["euclidean"],
        }

    grid = _build_hdbscan_param_grid(points)
    if min_cluster_sizes:
        grid["min_cluster_size"] = sorted(
            {max(2, min(entry_count, int(size))) for size in min_cluster_sizes if isinstance(size, (int, float))}
        ) or grid["min_cluster_size"]
    if min_samples is not None:
        grid["min_samples"] = sorted(
            {
                None if sample is None else max(1, min(entry_count, int(sample)))
                for sample in min_samples
                if isinstance(sample, (int, float)) or sample is None
            },
            key=lambda x: (x is not None, x if x is not None else -1),
        ) or grid["min_samples"]

    best_score = None
    best_result: ClusteringResult | None = None
    best_params: dict[str, object] = {}
    # Grid search on min_cluster_size/min_samples/selection/metric with DBCV guidance
    for size in grid["min_cluster_size"]:
        for samples in grid["min_samples"]:
            for selection_method in grid["cluster_selection_method"]:
                for metric in grid["metric"]:
                    try:
                        result = run_hdbscan_clustering(
                            dataset,
                            min_cluster_size=size,
                            min_samples=samples if samples is not None else None,
                            cluster_selection_method=selection_method,
                            metric=metric,
                        )
                    except RuntimeError:
                        continue
                    score = _safe_silhouette(points, result.labels)
                    comparative_score = -float("inf") if score is None else score
                    if best_score is None or comparative_score > best_score:
                        best_score = comparative_score
                        best_result = result
                        best_params = {
                            "min_cluster_size": size,
                            "min_samples": samples,
                            "cluster_selection_method": selection_method,
                            "metric": metric,
                            "silhouette": score,
                        }

    if best_result is None:
        fallback_size = grid["min_cluster_size"][0]
        fallback_samples = grid["min_samples"][0]
        try:
            best_result = run_hdbscan_clustering(
                dataset,
                min_cluster_size=fallback_size,
                min_samples=fallback_samples if fallback_samples is not None else None,
                cluster_selection_method=grid["cluster_selection_method"][0],
                metric=grid["metric"][0],
            )
        except RuntimeError as exc:
            raise RuntimeError("HDBSCAN did not produce any clusters with tested parameters.") from exc
        best_params = {
            "min_cluster_size": fallback_size,
            "min_samples": fallback_samples,
            "cluster_selection_method": grid["cluster_selection_method"][0],
            "metric": grid["metric"][0],
            "silhouette": None,
        }

    return best_result, best_params


def auto_optics_with_silhouette(
    dataset: NormalizedDataset,
    min_samples_values: Sequence[int] | None = None,
    xi_values: Sequence[float] | None = None,
    max_eps_values: Sequence[float] | None = None,
) -> tuple[ClusteringResult, dict[str, object]]:
    """
    @brief Search OPTICS parameters using silhouette score.
    @param dataset Normalized dataset to cluster.
    @param min_samples_values Optional candidate min_samples values.
    @param xi_values Optional candidate xi thresholds.
    @param max_eps_values Optional candidate max_eps values (<=0 treated as infinity).
    @return Tuple of best clustering result and chosen parameters (including silhouette score).
    """

    if not dataset.entries:
        return ClusteringResult(labels=[], clusters=[], noise=0), {}

    entry_count = len(dataset.entries)
    points = [entry.vector for entry in dataset.entries]
    if entry_count <= 1:
        labels = [0] if entry_count == 1 else []
        clusters = _build_cluster_summaries(labels, points)
        return ClusteringResult(labels=labels, clusters=clusters, noise=0), {
            "min_samples": max(2, entry_count),
            "xi": None,
            "max_eps": None,
            "mode": "auto",
            "silhouette": None,
        }

    default_min_samples = {
        3,
        4,
        5,
        max(2, int(math.sqrt(entry_count))),
        max(2, entry_count // 8),
        max(2, entry_count // 12),
    }
    min_samples_grid = sorted(
        {max(2, min(entry_count, int(value))) for value in (min_samples_values or default_min_samples) if value}
    )
    if not min_samples_grid:
        min_samples_grid = [max(2, min(entry_count, 5))]

    xi_grid = xi_values or [0.03, 0.05, 0.1, 0.2]
    xi_grid = [max(0.001, min(0.99, float(xi))) for xi in xi_grid if xi is not None]
    if not xi_grid:
        xi_grid = [0.05]

    max_eps_grid: list[float] = []
    for eps in max_eps_values or [math.inf, 1.5, 2.5]:
        try:
            val = float(eps)
        except (TypeError, ValueError):
            continue
        if val <= 0:
            val = math.inf
        max_eps_grid.append(val)
    if not max_eps_grid:
        max_eps_grid = [math.inf]

    best_score: float | None = None
    best_result: ClusteringResult | None = None
    best_params: dict[str, object] = {}

    for min_samples in min_samples_grid:
        for xi in xi_grid:
            for eps in max_eps_grid:
                try:
                    result = run_optics_clustering(
                        dataset,
                        min_samples=min_samples,
                        xi=xi,
                        max_eps=eps,
                    )
                except RuntimeError:
                    continue
                score = _safe_silhouette(points, result.labels)
                comparative_score = -float("inf") if score is None else score
                if best_score is None or comparative_score > best_score:
                    best_score = comparative_score
                    best_result = result
                    best_params = {
                        "min_samples": min_samples,
                        "xi": xi,
                        "max_eps": None if math.isinf(eps) else eps,
                        "silhouette": score,
                    }

    if best_result is None:
        fallback_samples = min_samples_grid[0]
        best_result = run_optics_clustering(dataset, min_samples=fallback_samples, xi=xi_grid[0], max_eps=max_eps_grid[0])
        best_params = {
            "min_samples": fallback_samples,
            "xi": xi_grid[0],
            "max_eps": None if math.isinf(max_eps_grid[0]) else max_eps_grid[0],
            "silhouette": None,
        }

    best_params["mode"] = "auto"
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


def _safe_silhouette(points: Sequence[Sequence[float]], labels: Sequence[int]) -> float | None:
    """
    @brief Compute silhouette score while swallowing errors and allowing noise labels.
    """
    try:
        return silhouette_score(points, labels)
    except Exception:
        return None




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
