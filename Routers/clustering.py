from __future__ import annotations

from typing import Literal

import csv
import json
from pathlib import Path

import config
from fastapi import APIRouter, HTTPException
from pydantic import BaseModel, Field

from Clustering import (
    METRIC_KEYS,
    normalize_dataset,
    project_to_components,
    run_hdbscan_clustering,
    run_kmeans_clustering,
    auto_kmeans_with_silhouette,
    auto_hdbscan_with_dbcv,
    build_feature_dataset,
)
from Files.dataset_manager import dataset_path
from Metrics.other_metrics import load_metrics_entries
from Routers.utils import ensure_dataset_ready, resolve_dataset_or_http_error

router = APIRouter(prefix="/clustering", tags=["clustering"])


def _average_vector(vectors: list[list[float]]) -> list[float]:
    """
    @brief Compute the average vector (centroid) for a collection of vectors.
    @param vectors List of numeric vectors.
    @return Averaged vector or empty list when no input.
    """
    if not vectors:
        return []
    return [sum(values) / len(vectors) for values in zip(*vectors)]


def _load_structural_embeddings(dataset: str) -> dict[str, list[float]]:
    """
    @brief Load precomputed structural embeddings and collapse per-file averages.
    @param dataset Dataset name.
    @return Mapping of normalized file path to embedding vector.
    """
    root = dataset_path(dataset) / "structural_embeddings"
    if not root.exists():
        return {}

    embeddings: dict[str, list[float]] = {}
    for path in root.rglob("*.embedding.json"):
        try:
            payload = json.loads(path.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError):
            continue
        segments = payload.get("segments") or []
        vectors: list[list[float]] = []
        for segment in segments:
            embedding = segment.get("embedding")
            if isinstance(embedding, list) and all(isinstance(x, (int, float)) for x in embedding):
                vectors.append([float(x) for x in embedding])
        if not vectors:
            continue
        dim = len(vectors[0])
        vectors = [vec for vec in vectors if len(vec) == dim]
        if not vectors:
            continue
        averaged = [sum(values) / len(vectors) for values in zip(*vectors)]
        source = payload.get("source") or path.relative_to(root).as_posix()
        normalized_source = str(source).replace("\\", "/")
        normalized_source = normalized_source.replace(".structural.txt", "")
        normalized_source = normalized_source.lstrip("/")
        embeddings[normalized_source] = averaged
    return embeddings


class ClusteringRequest(BaseModel):
    """
    @brief Request payload for triggering clustering.
    """
    dataset: str | None = None
    algorithm: Literal["kmeans", "hdbscan"] = "kmeans"
    cluster_count: int | None = Field(default=None, ge=2, le=200)
    min_cluster_size: int | None = Field(default=None, ge=2, le=500)
    min_samples: int | None = Field(default=None, ge=1, le=500)
    auto_hdbscan: bool = False
    auto_kmeans: bool = False
    feature_mode: Literal["metrics", "embeddings", "both"] = "metrics"
    embedding_dims: int = Field(default=16, ge=2, le=128)


@router.post("/run")
async def launch_clustering(payload: ClusteringRequest) -> dict[str, object]:
    """
    @brief Run clustering on a dataset using k-means or hdbscan.
    @param payload Request body configuring dataset and algorithm parameters.
    @return Clustering result including points, clusters, and projection axes.
    @throws HTTPException On validation errors or missing metrics.
    """
    dataset_name, _ = resolve_dataset_or_http_error(payload.dataset, require_raw=True)
    ensure_dataset_ready(dataset_name)
    feature_mode = payload.feature_mode or "metrics"
    embedding_dims = payload.embedding_dims or 16

    files = load_metrics_entries(dataset_name)
    embeddings = _load_structural_embeddings(dataset_name) if feature_mode in {"embeddings", "both"} else {}
    metrics_lookup: dict[str, dict[str, object]] = {
        entry.get("path", "").replace("\\", "/"): entry.get("metrics") or {} for entry in files
    }
    # Build the feature matrix for clustering (metrics, embeddings, or both).
    normalized = build_feature_dataset(
        files,
        embeddings,
        feature_mode=feature_mode,
        embedding_dims=embedding_dims,
        metric_keys=METRIC_KEYS,
    )
    if not normalized.entries:
        raise HTTPException(status_code=400, detail=config.MESSAGES["CLUSTERING_NO_DATA"])

    response_metric_keys = list(normalized.metric_keys) if normalized.metric_keys else []
    if not response_metric_keys and metrics_lookup:
        # fallback to all available metric keys for display when clustering ran without metrics
        sample_metrics = next(iter(metrics_lookup.values()))
        response_metric_keys = sorted(sample_metrics.keys())

    parameters: dict[str, object] = {}
    if payload.algorithm == "kmeans":
        if payload.auto_kmeans:
            result, chosen = auto_kmeans_with_silhouette(normalized)
            parameters.update({"mode": "auto", **chosen})
        else:
            if not payload.cluster_count:
                raise HTTPException(status_code=400, detail=config.MESSAGES["CLUSTERING_KMEANS_COUNT_REQUIRED"])
            result = run_kmeans_clustering(normalized, payload.cluster_count)
            parameters["cluster_count"] = payload.cluster_count
    else:
        try:
            if payload.auto_hdbscan:
                result, chosen = auto_hdbscan_with_dbcv(normalized)
                parameters.update({"mode": "auto", **chosen})
                min_cluster_size = chosen.get("min_cluster_size")
                min_samples = chosen.get("min_samples")
            else:
                # Heuristic defaults scale with dataset size to avoid degenerate clusters.
                heuristic = max(3, min(len(normalized.entries) // 8 or 2, 25))
                min_cluster_size = payload.min_cluster_size or heuristic
                min_cluster_size = max(2, min(min_cluster_size, len(normalized.entries)))
                min_samples = payload.min_samples or max(1, min_cluster_size // 2)
                min_samples = max(1, min(min_samples, min_cluster_size))
                result = run_hdbscan_clustering(
                    normalized, min_cluster_size=min_cluster_size, min_samples=min_samples
                )
                parameters["min_cluster_size"] = min_cluster_size
        except RuntimeError as exc:
            raise HTTPException(
                status_code=400,
                detail="HDBSCAN did not produce any clusters. Adjust min_cluster_size/min_samples or use auto mode.",
            ) from exc
        parameters["min_samples"] = min_samples if min_samples is not None else parameters.get("min_samples")
    parameters["feature_mode"] = feature_mode
    parameters["embedding_dims"] = embedding_dims
    print(
        f"[clustering] dataset={dataset_name} algo={payload.algorithm} mode={feature_mode} "
        f"dims={embedding_dims} params={parameters}"
    )

    # Reduce to 2D for visualization; fallback to trivial axes if PCA fails.
    projection, axes = project_to_components(normalized, components=2)
    if not projection:
        projection = [(0.0, 0.0) for _ in normalized.entries]
    if len(axes) < 2:
        axes = ["Component 1", "Component 2"]

    points_payload: list[dict[str, object]] = []
    cluster_metric_sums: dict[int, dict[str, float]] = {}
    cluster_counts: dict[int, int] = {}
    metric_keys = list(response_metric_keys)
    # Walk normalized entries alongside labels to build response payloads and cluster aggregates.
    for entry, label, coords in zip(normalized.entries, result.labels, projection):
        metrics_snapshot_base = metrics_lookup.get(entry.path, {})
        metrics_snapshot = {key: metrics_snapshot_base.get(key) for key in metric_keys} if metric_keys else metrics_snapshot_base
        if label >= 0:
            cluster_counts[label] = cluster_counts.get(label, 0) + 1
            sums = cluster_metric_sums.setdefault(label, {})
            for key, value in (metrics_snapshot.items() if metrics_snapshot else []):
                if isinstance(value, (int, float)):
                    sums[key] = sums.get(key, 0.0) + float(value)
        x, y = coords
        points_payload.append(
            {
                "path": entry.path,
                "cluster": label,
                "x": x,
                "y": y,
                "metrics": metrics_snapshot,
            }
        )

    _persist_clustering_csv(dataset_name, payload.algorithm, points_payload)

    clusters_payload = []
    for info in result.clusters:
        averages: dict[str, float] = {}
        count = cluster_counts.get(info.id, 0)
        if count:
            sums = cluster_metric_sums.get(info.id, {})
            for key in metric_keys:
                total = sums.get(key)
                if total is not None:
                    averages[key] = total / count
        clusters_payload.append(
            {
                "id": info.id,
                "label": f"Cluster {info.id + 1}",
                "size": info.size,
                "centroid": info.centroid,
                "metrics": averages,
            }
        )

    return {
        "dataset": dataset_name,
        "algorithm": payload.algorithm,
        "parameters": parameters,
        "metrics": response_metric_keys,
        "points": points_payload,
        "clusters": clusters_payload,
        "axes": {"x": axes[0], "y": axes[1]},
        "noise": result.noise,
    }


@router.get("/last", name="last-clustering")
async def read_last_clustering(dataset: str | None = None) -> dict[str, object]:
    """
    @brief Load the last saved clustering for a dataset.
    @param dataset Dataset name (optional, uses current selection).
    @return Cached clustering result.
    @throws HTTPException Si aucun clustering n'est disponible ou sur erreur d'E/S.
    """
    dataset_name, _ = resolve_dataset_or_http_error(dataset, require_raw=True)
    ensure_dataset_ready(dataset_name)
    return _load_cached_clustering(dataset_name)


def _persist_clustering_csv(dataset: str, algorithm: str, points: list[dict[str, object]]) -> None:
    """
    @brief Persist clustering projection to a CSV file for reuse.
    @param dataset Dataset name owning the clustering.
    @param algorithm Algorithm identifier stored alongside the results.
    @param points List of clustering point payloads to serialize.
    @throws HTTPException If the CSV cannot be written.
    """
    target = dataset_path(dataset) / config.CLUSTERING_CACHE_FILENAME
    fieldnames = ["path", "cluster", "x", "y", "algorithm"]
    try:
        target.parent.mkdir(parents=True, exist_ok=True)
        with target.open("w", encoding="utf-8", newline="") as handle:
            writer = csv.DictWriter(handle, fieldnames=fieldnames)
            writer.writeheader()
            for point in points:
                writer.writerow(
                    {
                        "path": point.get("path"),
                        "cluster": point.get("cluster"),
                        "x": point.get("x"),
                        "y": point.get("y"),
                        "algorithm": algorithm,
                    }
                )
    except OSError as exc:
        raise HTTPException(status_code=500, detail=f"Impossible d'enregistrer le clustering: {exc}") from exc


def _load_cached_clustering(dataset: str) -> dict[str, object]:
    """
    @brief Reload clustering results from the CSV cache and recompute summaries.
    @param dataset Dataset name.
    @return Clustering payload reconstructed from cached labels and fresh metrics.
    @throws HTTPException When cache or metrics are unavailable/invalid.
    """
    cache_path = dataset_path(dataset) / config.CLUSTERING_CACHE_FILENAME
    if not cache_path.exists():
        raise HTTPException(status_code=404, detail=config.MESSAGES["CLUSTERING_NO_METRICS"])

    try:
        with cache_path.open("r", encoding="utf-8") as handle:
            reader = csv.DictReader(handle)
            records = list(reader)
    except OSError as exc:
        raise HTTPException(status_code=500, detail=f"Unable to read clustering: {exc}") from exc

    if not records:
        raise HTTPException(status_code=404, detail=config.MESSAGES["CLUSTERING_NO_METRICS"])

    algorithm = records[0].get("algorithm") or "hdbscan"
    point_lookup: dict[str, dict[str, object]] = {}
    for row in records:
        raw_path = (row.get("path") or "").strip()
        if not raw_path:
            continue
        path = raw_path.replace("\\", "/")
        cluster = -1
        try:
            cluster = int(row.get("cluster") or -1)
        except ValueError:
            cluster = -1
        try:
            x = float(row.get("x") or 0.0)
            y = float(row.get("y") or 0.0)
        except ValueError:
            x, y = 0.0, 0.0
        point_lookup[path] = {"cluster": cluster, "coords": (x, y)}

    files = load_metrics_entries(dataset)
    if not files:
        raise HTTPException(status_code=404, detail=config.MESSAGES["CLUSTERING_NO_METRICS"])

    normalized = normalize_dataset(files, METRIC_KEYS)
    if not normalized.entries:
        raise HTTPException(status_code=400, detail="Unable to normalize metrics for clustering.")

    # Rebuild per-point payloads and cluster summaries using cached labels and fresh metrics.
    metric_keys = list(normalized.metric_keys)
    cluster_metric_sums: dict[int, dict[str, float]] = {}
    cluster_counts: dict[int, int] = {}
    clusters_vectors: dict[int, list[list[float]]] = {}
    points_payload: list[dict[str, object]] = []
    labels: list[int] = []

    for entry in normalized.entries:
        info = point_lookup.get(entry.path)
        label = int(info["cluster"]) if info and "cluster" in info else -1
        labels.append(label)
        metrics_snapshot = {key: entry.metrics.get(key) for key in metric_keys}
        if label >= 0:
            cluster_counts[label] = cluster_counts.get(label, 0) + 1
            sums = cluster_metric_sums.setdefault(label, {})
            for key in metric_keys:
                value = metrics_snapshot.get(key)
                if isinstance(value, (int, float)):
                    sums[key] = sums.get(key, 0.0) + float(value)
            clusters_vectors.setdefault(label, []).append(entry.vector)

        coords = info["coords"] if info and "coords" in info else (0.0, 0.0)
        points_payload.append(
            {
                "path": entry.path,
                "cluster": label,
                "x": coords[0],
                "y": coords[1],
                "metrics": metrics_snapshot,
            }
        )

    clusters_payload = []
    for cluster_id, count in sorted(cluster_counts.items()):
        sums = cluster_metric_sums.get(cluster_id, {})
        averages: dict[str, float] = {}
        for key in metric_keys:
            total = sums.get(key)
            if total is not None:
                averages[key] = total / count
        centroid = _average_vector(clusters_vectors.get(cluster_id, []))
        clusters_payload.append(
            {
                "id": cluster_id,
                "label": f"Cluster {cluster_id + 1}",
                "size": count,
                "centroid": centroid,
                "metrics": averages,
            }
        )

    noise = sum(1 for label in labels if label == -1)
    axes = {"x": "Component 1", "y": "Component 2"}
    parameters = {"mode": "cached", "source": "clustering.csv"}
    return {
        "dataset": dataset,
        "algorithm": algorithm,
        "parameters": parameters,
        "metrics": metric_keys,
        "points": points_payload,
        "clusters": clusters_payload,
        "axes": axes,
        "noise": noise,
    }
