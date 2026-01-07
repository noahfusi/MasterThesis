from __future__ import annotations

import math
from typing import Sequence

from Clustering.common import (
    ClusteringResult,
    NormalizedDataset,
    build_cluster_summaries,
    fuzzy_memberships,
    safe_silhouette,
)

try:  # pragma: no cover - required for k-means
    from sklearn.cluster import KMeans as _sklearn_KMeans  # type: ignore
except Exception as exc:  # pragma: no cover
    _sklearn_KMeans = None
    _SKLEARN_KMEANS_IMPORT_ERROR = exc
else:
    _SKLEARN_KMEANS_IMPORT_ERROR = None


def run_kmeans_clustering(dataset: NormalizedDataset, cluster_count: int, max_iterations: int = 100) -> ClusteringResult:
    """
    Run a k-means clustering over normalized entries.
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
        distances = model.transform(points)
        probabilities = fuzzy_memberships(distances)
        labels = [int(max(range(len(probs)), key=lambda idx: probs[idx])) for probs in probabilities]
    except Exception:
        probabilities = None

    clusters = build_cluster_summaries(labels, points, centroids_override=centroids)
    return ClusteringResult(labels=labels, clusters=clusters, noise=0, probabilities=probabilities)


def auto_kmeans_with_silhouette(
    dataset: NormalizedDataset, cluster_counts: Sequence[int] | None = None
) -> tuple[ClusteringResult, dict[str, object]]:
    """
    Explore several k-means cluster counts and pick the best using silhouette score.
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
        score = safe_silhouette(points, result.labels)
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


__all__ = ["run_kmeans_clustering", "auto_kmeans_with_silhouette"]
