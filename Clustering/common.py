from __future__ import annotations

from dataclasses import dataclass
from typing import Sequence

import config
from Clustering.utils import average_vector, silhouette_score

# Exposed metric keys for clustering flows
METRIC_KEYS: list[str] = list(config.CLUSTERING_METRIC_KEYS)


@dataclass(slots=True)
class NormalizedEntry:
    """
    Single normalized file entry containing metrics and normalized vector.
    """

    path: str
    metrics: dict[str, float]
    vector: list[float]


@dataclass(slots=True)
class NormalizedDataset:
    """
    Container for metric keys and their normalized entries.
    """

    metric_keys: list[str]
    entries: list[NormalizedEntry]


@dataclass(slots=True)
class ClusterInfo:
    """
    Summary information for a discovered cluster.
    """

    id: int
    size: int
    centroid: list[float]


@dataclass(slots=True)
class ClusteringResult:
    """
    Result of a clustering run including labels and cluster summaries.
    """

    labels: list[int]
    clusters: list[ClusterInfo]
    noise: int = 0
    probabilities: list[list[float]] | None = None


def build_cluster_summaries(
    labels: list[int], points: Sequence[Sequence[float]], centroids_override: Sequence[Sequence[float]] | None = None
) -> list[ClusterInfo]:
    """
    Build cluster summaries from labels and points.
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


def safe_silhouette(points: Sequence[Sequence[float]], labels: Sequence[int]) -> float | None:
    """
    Compute silhouette score while swallowing errors and allowing noise labels.
    """
    try:
        return silhouette_score(points, labels)
    except Exception:
        return None


def fuzzy_memberships(distances: Sequence[Sequence[float]], m: float = 2.0) -> list[list[float]]:
    """
    Compute fuzzy c-means style memberships from distance matrix.
    """
    memberships: list[list[float]] = []
    for row in distances:
        values = [float(d) for d in row]
        if not values:
            memberships.append([])
            continue
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


__all__ = [
    "METRIC_KEYS",
    "NormalizedEntry",
    "NormalizedDataset",
    "ClusterInfo",
    "ClusteringResult",
    "build_cluster_summaries",
    "safe_silhouette",
    "fuzzy_memberships",
]
