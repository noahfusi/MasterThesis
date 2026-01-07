"""
Backwards-compatible aggregator for clustering helpers and algorithms.

The implementation now lives in smaller modules (features, kmeans, gmm, hdbscan, optics, common).
"""

from Clustering.common import (
    METRIC_KEYS,
    ClusterInfo,
    ClusteringResult,
    NormalizedDataset,
    NormalizedEntry,
    build_cluster_summaries,
    fuzzy_memberships,
    safe_silhouette,
)
from Clustering.features import build_feature_dataset, normalize_dataset, project_to_components
from Clustering.gmm import auto_gmm_with_bic, run_gmm_clustering
from Clustering.hdbscan import auto_hdbscan_with_dbcv, run_hdbscan_clustering
from Clustering.kmeans import auto_kmeans_with_silhouette, run_kmeans_clustering
from Clustering.optics import auto_optics_with_silhouette, run_optics_clustering

__all__ = [
    "METRIC_KEYS",
    "ClusterInfo",
    "ClusteringResult",
    "NormalizedDataset",
    "NormalizedEntry",
    "build_cluster_summaries",
    "fuzzy_memberships",
    "safe_silhouette",
    "build_feature_dataset",
    "normalize_dataset",
    "project_to_components",
    "auto_gmm_with_bic",
    "run_gmm_clustering",
    "auto_hdbscan_with_dbcv",
    "run_hdbscan_clustering",
    "auto_kmeans_with_silhouette",
    "run_kmeans_clustering",
    "auto_optics_with_silhouette",
    "run_optics_clustering",
]
