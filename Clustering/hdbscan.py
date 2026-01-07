from __future__ import annotations

import math
from typing import Sequence

from Clustering.common import ClusteringResult, NormalizedDataset, build_cluster_summaries, safe_silhouette

try:  # pragma: no cover - optional dependency
    from cuml.cluster import HDBSCAN as _cuml_HDBSCAN  # type: ignore
except Exception:  # pragma: no cover
    _cuml_HDBSCAN = None
try:  # pragma: no cover - optional dependency
    import hdbscan as _hdbscan  # type: ignore
except Exception:  # pragma: no cover
    _hdbscan = None
    _HDBSCAN_IMPORT_ERROR = None


def run_hdbscan_clustering(
    dataset: NormalizedDataset,
    min_cluster_size: int = 5,
    *,
    min_samples: int | None = None,
    cluster_selection_method: str | None = None,
    metric: str = "euclidean",
) -> ClusteringResult:
    """
    Perform density-based clustering using hdbscan when available.
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

    clusters = build_cluster_summaries(labels, points)
    probabilities: list[list[float]] | None = None
    try:
        if _hdbscan is not None and "clusterer" in locals():
            membership = _hdbscan.all_points_membership_vectors(clusterer)
            probabilities = [[float(x) for x in row] for row in membership]
    except Exception:
        probabilities = None

    noise = sum(1 for label in labels if label == -1)
    return ClusteringResult(labels=labels, clusters=clusters, noise=noise, probabilities=probabilities)


def auto_hdbscan_with_dbcv(
    dataset: NormalizedDataset,
    min_cluster_sizes: Sequence[int] | None = None,
    min_samples: Sequence[int] | None = None,
) -> tuple[ClusteringResult, dict[str, object]]:
    """
    Search HDBSCAN parameters using silhouette score (ignoring noise labels).
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
            min_samples_values.add(None)
            for factor in (0.3, 0.6):
                candidate = int(round(factor * mcs))
                candidate = max(1, min(max(20, mcs), candidate))
                min_samples_values.add(candidate)

        min_samples_grid = sorted(min_samples_values, key=lambda x: (x is not None, x if x is not None else -1))

        adjusted_min_cluster_sizes = [3, 4, 5, 6] + min_cluster_sizes_grid
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
                    score = safe_silhouette(points, result.labels)
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


__all__ = ["run_hdbscan_clustering", "auto_hdbscan_with_dbcv"]
