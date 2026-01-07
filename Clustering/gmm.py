from __future__ import annotations

import math
from typing import Sequence

from Clustering.common import ClusteringResult, NormalizedDataset, build_cluster_summaries, safe_silhouette

try:  # pragma: no cover - optional dependency for Gaussian mixtures
    from sklearn.mixture import GaussianMixture as _sklearn_GaussianMixture  # type: ignore
except Exception as exc:  # pragma: no cover
    _sklearn_GaussianMixture = None
    _SKLEARN_GMM_IMPORT_ERROR = exc
else:
    _SKLEARN_GMM_IMPORT_ERROR = None


def run_gmm_clustering(
    dataset: NormalizedDataset,
    cluster_count: int,
    max_iterations: int = 200,
    covariance_type: str = "full",
    reg_covar: float = 1e-3,
    n_init: int = 5,
) -> ClusteringResult:
    """
    Run a Gaussian Mixture Model clustering over normalized entries.
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
    clusters = build_cluster_summaries(labels, points, centroids_override=means)
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
    Search Gaussian Mixture hyperparameters using silhouette score.
    """
    if not dataset.entries:
        return ClusteringResult(labels=[], clusters=[], noise=0), {}

    if _sklearn_GaussianMixture is None:
        raise RuntimeError("scikit-learn is required for Gaussian Mixture clustering.") from _SKLEARN_GMM_IMPORT_ERROR

    points = [entry.vector for entry in dataset.entries]
    entry_count = len(points)
    if entry_count <= 1:
        labels = [0] if entry_count == 1 else []
        clusters = build_cluster_summaries(labels, points)
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
                clusters = build_cluster_summaries(labels, points, centroids_override=means)
                result = ClusteringResult(labels=labels, clusters=clusters, noise=0, probabilities=probs)
            except Exception:
                continue

            score = safe_silhouette(points, labels)
            comparative_score = -float("inf") if score is None else score
            if best_score is None or comparative_score > best_score:
                best_score = comparative_score
                best_result = result
                best_params = {"cluster_count": count, "covariance_type": cov, "silhouette": score}

    if best_result is None:
        raise RuntimeError("GMM auto-tuning could not find a multi-cluster solution.")

    best_params["mode"] = "auto"
    return best_result, best_params


__all__ = ["run_gmm_clustering", "auto_gmm_with_bic"]
