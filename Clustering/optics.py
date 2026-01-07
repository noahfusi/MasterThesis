from __future__ import annotations

import math
from typing import Sequence

from Clustering.common import ClusteringResult, NormalizedDataset, build_cluster_summaries, safe_silhouette

try:  # pragma: no cover - optional dependency for OPTICS
    from sklearn.cluster import OPTICS as _sklearn_OPTICS  # type: ignore
except Exception as exc:  # pragma: no cover
    _sklearn_OPTICS = None
    _SKLEARN_OPTICS_IMPORT_ERROR = exc
else:
    _SKLEARN_OPTICS_IMPORT_ERROR = None


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
    Perform density-based clustering using OPTICS (scikit-learn).
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

    clusters = build_cluster_summaries(labels, points)
    noise = sum(1 for label in labels if label == -1)
    return ClusteringResult(labels=labels, clusters=clusters, noise=noise, probabilities=None)


def auto_optics_with_silhouette(
    dataset: NormalizedDataset,
    min_samples_values: Sequence[int] | None = None,
    xi_values: Sequence[float] | None = None,
    max_eps_values: Sequence[float] | None = None,
) -> tuple[ClusteringResult, dict[str, object]]:
    """
    Search OPTICS parameters using silhouette score.
    """
    if not dataset.entries:
        return ClusteringResult(labels=[], clusters=[], noise=0), {}

    entry_count = len(dataset.entries)
    points = [entry.vector for entry in dataset.entries]
    if entry_count <= 1:
        labels = [0] if entry_count == 1 else []
        clusters = build_cluster_summaries(labels, points)
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
                score = safe_silhouette(points, result.labels)
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


__all__ = ["run_optics_clustering", "auto_optics_with_silhouette"]
