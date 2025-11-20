from __future__ import annotations

from typing import Literal

import csv

from fastapi import APIRouter, HTTPException
from pydantic import BaseModel, Field

from Clustering import (
    METRIC_KEYS,
    normalize_dataset,
    project_to_components,
    run_hdbscan_clustering,
    run_kmeans_clustering,
)
from Files.dataset_manager import dataset_path, get_current_dataset, normalize_dataset_name
from Metrics.other_metrics import load_metrics_entries

router = APIRouter(prefix="/clustering", tags=["clustering"])


class ClusteringRequest(BaseModel):
    dataset: str | None = None
    algorithm: Literal["kmeans", "hdbscan"] = "kmeans"
    cluster_count: int | None = Field(default=None, ge=2, le=200)
    min_cluster_size: int | None = Field(default=None, ge=2, le=500)
    min_samples: int | None = Field(default=None, ge=1, le=500)


@router.post("/run")
async def launch_clustering(payload: ClusteringRequest) -> dict[str, object]:
    dataset_name = payload.dataset or get_current_dataset()
    if not dataset_name:
        raise HTTPException(status_code=400, detail="Sélectionnez un dataset avant de lancer le clustering.")
    try:
        dataset_name = normalize_dataset_name(dataset_name)
    except ValueError as exc:
        raise HTTPException(status_code=400, detail=str(exc)) from exc

    files = load_metrics_entries(dataset_name)
    if not files:
        raise HTTPException(
            status_code=404,
            detail="Aucune métrique disponible. Générez 'metrics.csv' pour ce dataset avant de lancer le clustering.",
        )
    normalized = normalize_dataset(files, METRIC_KEYS)
    if not normalized.entries:
        raise HTTPException(status_code=400, detail="Impossible de normaliser les métriques des fichiers.")

    parameters: dict[str, object] = {}
    if payload.algorithm == "kmeans":
        if not payload.cluster_count:
            raise HTTPException(
                status_code=400, detail="Le nombre de clusters est requis pour exécuter k-means."
            )
        result = run_kmeans_clustering(normalized, payload.cluster_count)
        parameters["cluster_count"] = payload.cluster_count
    else:
        heuristic = max(3, min(len(normalized.entries) // 8 or 2, 25))
        min_cluster_size = payload.min_cluster_size or heuristic
        min_cluster_size = max(2, min(min_cluster_size, len(normalized.entries)))
        min_samples = payload.min_samples or max(1, min_cluster_size // 2)
        min_samples = max(1, min(min_samples, min_cluster_size))
        result = run_hdbscan_clustering(
            normalized, min_cluster_size=min_cluster_size, min_samples=min_samples
        )
        parameters["min_cluster_size"] = min_cluster_size
        parameters["min_samples"] = min_samples

    projection, axes = project_to_components(normalized, components=2)
    if not projection:
        projection = [(0.0, 0.0) for _ in normalized.entries]
    if len(axes) < 2:
        axes = ["Component 1", "Component 2"]

    points_payload: list[dict[str, object]] = []
    cluster_metric_sums: dict[int, dict[str, float]] = {}
    cluster_counts: dict[int, int] = {}
    metric_keys = list(normalized.metric_keys)
    for entry, label, coords in zip(normalized.entries, result.labels, projection):
        metrics_snapshot = {key: entry.metrics.get(key) for key in metric_keys}
        if label >= 0:
            cluster_counts[label] = cluster_counts.get(label, 0) + 1
            sums = cluster_metric_sums.setdefault(label, {})
            for key in metric_keys:
                value = metrics_snapshot.get(key)
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
        "metrics": normalized.metric_keys,
        "points": points_payload,
        "clusters": clusters_payload,
        "axes": {"x": axes[0], "y": axes[1]},
        "noise": result.noise,
    }


def _persist_clustering_csv(dataset: str, algorithm: str, points: list[dict[str, object]]) -> None:
    target = dataset_path(dataset) / "clustering.csv"
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
