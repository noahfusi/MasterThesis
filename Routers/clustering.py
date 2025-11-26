from __future__ import annotations

from typing import Literal
import re

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
from LLM.ollama import generate_completion
from Routers.utils import ensure_dataset_ready, resolve_dataset_or_http_error

router = APIRouter(prefix="/clustering", tags=["clustering"])
THEMES = config.CLUSTERING_THEMES


def _resolve_theme(theme: str | None) -> tuple[str, dict[str, object]]:
    """
    @brief Validate and normalize a requested clustering theme.
    @param theme Theme identifier from the request (may be None).
    @return Tuple of normalized theme key and its configuration.
    @throws HTTPException When an unknown theme is requested.
    """
    if THEMES:
        if theme:
            normalized = str(theme).strip().lower().replace(" ", "_")
            for key in THEMES:
                if key.lower() == normalized:
                    return key, THEMES[key]
            raise HTTPException(status_code=400, detail=f"Unknown clustering theme '{theme}'.")
        default_key = next(iter(THEMES))
        return default_key, THEMES[default_key]
    return "default", {"label": "Default", "metrics": METRIC_KEYS}


def _extract_comparison(completion: str | None) -> list[dict[str, str]]:
    """
    @brief Parse a comparison table from an LLM completion in the expected format.
    """
    if not completion or not isinstance(completion, str):
        return []
    lines = [line.strip().strip('"').strip("'") for line in completion.splitlines() if line.strip()]
    start_idx = next((i for i, line in enumerate(lines) if line.lower().startswith("comparison")), None)
    if start_idx is None:
        return []
    rows: list[dict[str, str]] = []
    for line in lines[start_idx + 1 :]:
        if not line.startswith("|"):
            break
        cells = [cell.strip() for cell in line.split("|") if cell.strip()]
        if len(cells) < 4:
            continue
        if cells[0].lower() == "metric":
            # Header row
            continue
        rows.append(
            {
                "metric": cells[0],
                "cluster": cells[1],
                "dataset": cells[2],
                "relation": cells[3],
            }
        )
    return rows


def _cache_path(dataset: str, theme: str | None, filename: str) -> Path:
    """
    @brief Build a cache path that is namespaced per theme.
    """
    base = Path(filename)
    suffix = f"_{theme}" if theme else ""
    target_name = f"{base.stem}{suffix}{base.suffix}" if suffix else filename
    return dataset_path(dataset) / target_name


def _average_vector(vectors: list[list[float]]) -> list[float]:
    """
    @brief Compute the average vector (centroid) for a collection of vectors.
    @param vectors List of numeric vectors.
    @return Averaged vector or empty list when no input.
    """
    if not vectors:
        return []
    return [sum(values) / len(vectors) for values in zip(*vectors)]


def _parse_llm_label(completion: str, fallback: str) -> tuple[str, str | None]:
    """
    @brief Extract label/description from LLM completion text.
    """
    label = fallback
    description: str | None = None
    if not completion or not isinstance(completion, str):
        return label, description
    text = completion.replace("\r", "\n")
    match_label = re.search(r"label\s*:\s*(.+)", text, flags=re.IGNORECASE)
    if match_label:
        candidate = match_label.group(1).strip()
        candidate = candidate.split("Description", 1)[0].strip()
        if candidate:
            label = candidate
    match_desc = re.search(r"description\s*:\s*(.+)", text, flags=re.IGNORECASE | re.DOTALL)
    if match_desc:
        candidate = match_desc.group(1).strip()
        if candidate:
            description = candidate
    if description is None:
        lines = [line.strip() for line in text.splitlines() if line.strip()]
        if len(lines) > 1:
            description = " ".join(lines[1:])
    return label, description
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
    theme: str | None = None
    themes: list[ThemeClusteringConfig] | None = None
    algorithm: Literal["kmeans", "hdbscan"] = "kmeans"
    cluster_count: int | None = Field(default=None, ge=2, le=200)
    min_cluster_size: int | None = Field(default=None, ge=2, le=500)
    min_samples: int | None = Field(default=None, ge=1, le=500)
    auto_hdbscan: bool = False
    auto_kmeans: bool = False
    feature_mode: Literal["metrics", "embeddings", "both"] = "metrics"
    embedding_dims: int = Field(default=16, ge=2, le=128)


class ThemeClusteringConfig(BaseModel):
    """
    @brief Per-theme clustering configuration.
    """
    dataset: str | None = None
    theme: str | None = None
    algorithm: Literal["kmeans", "hdbscan"] = "kmeans"
    cluster_count: int | None = Field(default=None, ge=2, le=200)
    min_cluster_size: int | None = Field(default=None, ge=2, le=500)
    min_samples: int | None = Field(default=None, ge=1, le=500)
    auto_hdbscan: bool = False
    auto_kmeans: bool = False
    feature_mode: Literal["metrics", "embeddings", "both"] | None = None
    embedding_dims: int | None = Field(default=None, ge=2, le=128)


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
    base_files = load_metrics_entries(dataset_name)
    if not base_files:
        raise HTTPException(status_code=400, detail=config.MESSAGES["CLUSTERING_NO_DATA"])

    def run_for_theme(
        *,
        theme_key: str,
        theme_config: dict[str, object],
        algorithm: str,
        cluster_count: int | None,
        min_cluster_size: int | None,
        min_samples: int | None,
        auto_hdbscan: bool,
        auto_kmeans: bool,
        feature_mode: str,
        embedding_dims: int,
    ) -> dict[str, object]:
        metric_keys_for_theme = list(theme_config.get("metrics") or METRIC_KEYS)
        embeddings = _load_structural_embeddings(dataset_name) if feature_mode in {"embeddings", "both"} else {}
        metrics_lookup: dict[str, dict[str, object]] = {
            entry.get("path", "").replace("\\", "/"): entry.get("metrics") or {} for entry in base_files
        }
        normalized = build_feature_dataset(
            base_files,
            embeddings,
            feature_mode=feature_mode,
            embedding_dims=embedding_dims,
            metric_keys=metric_keys_for_theme,
        )
        if not normalized.entries:
            raise HTTPException(status_code=400, detail=config.MESSAGES["CLUSTERING_NO_DATA"])

        response_metric_keys = list(normalized.metric_keys) if normalized.metric_keys else []
        if not response_metric_keys and metrics_lookup:
            sample_metrics = next(iter(metrics_lookup.values()))
            response_metric_keys = sorted(sample_metrics.keys())

        parameters: dict[str, object] = {}
        if algorithm == "kmeans":
            if auto_kmeans:
                result, chosen = auto_kmeans_with_silhouette(normalized)
                parameters.update({"mode": "auto", **chosen})
            else:
                if not cluster_count:
                    raise HTTPException(status_code=400, detail=config.MESSAGES["CLUSTERING_KMEANS_COUNT_REQUIRED"])
                result = run_kmeans_clustering(normalized, cluster_count)
                parameters["cluster_count"] = cluster_count
        else:
            try:
                if auto_hdbscan:
                    result, chosen = auto_hdbscan_with_dbcv(normalized)
                    parameters.update({"mode": "auto", **chosen})
                    min_cluster_size = chosen.get("min_cluster_size")
                    min_samples = chosen.get("min_samples")
                else:
                    heuristic = max(3, min(len(normalized.entries) // 8 or 2, 25))
                    min_cluster_size = min_cluster_size or heuristic
                    min_cluster_size = max(2, min(min_cluster_size, len(normalized.entries)))
                    min_samples = min_samples or max(1, min_cluster_size // 2)
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
            f"[clustering] dataset={dataset_name} theme={theme_key} algo={algorithm} mode={feature_mode} "
            f"dims={embedding_dims} params={parameters}"
        )

        projection, axes = project_to_components(normalized, components=2)
        if not projection:
            projection = [(0.0, 0.0) for _ in normalized.entries]
        if len(axes) < 2:
            axes = ["Component 1", "Component 2"]

        points_payload: list[dict[str, object]] = []
        cluster_metric_sums: dict[int, dict[str, float]] = {}
        cluster_counts: dict[int, int] = {}
        dataset_metric_averages: dict[str, float] = {}
        for key in response_metric_keys:
            values = [
                entry.metrics.get(key)
                for entry in normalized.entries
                if isinstance(entry.metrics.get(key), (int, float, float))
            ]
            if values:
                dataset_metric_averages[key] = sum(values) / len(values)
        metric_keys = list(response_metric_keys)
        for idx, (entry, label, coords) in enumerate(zip(normalized.entries, result.labels, projection)):
            metrics_snapshot_base = metrics_lookup.get(entry.path, {})
            metrics_snapshot = {key: metrics_snapshot_base.get(key) for key in metric_keys} if metric_keys else metrics_snapshot_base
            probs = None
            if result.probabilities and idx < len(result.probabilities):
                probs = result.probabilities[idx]
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
                    "probabilities": probs,
                }
            )

        _persist_clustering_csv(dataset_name, algorithm, points_payload, theme_key)

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
            label_text = f"Cluster {info.id + 1}"
            description_text = None
            completion_text = None
            prompt_text = None
            try:
                representative = next((p for p in points_payload if p.get("cluster") == info.id), None)
                representative_content = None
                if representative:
                    rep_path = dataset_path(dataset_name) / config.RAW_FOLDER_NAME / (representative.get("path") or "")
                    if rep_path.is_file():
                        try:
                            representative_content = rep_path.read_text(encoding="utf-8")
                        except OSError:
                            representative_content = None
                prompt_text = config.CLUSTER_LABEL_PROMPT.format(
                    cluster_metrics=averages,
                    dataset_metrics=dataset_metric_averages,
                    representative=representative_content or (representative.get("path") if representative else "N/A"),
                )
                completion_text = generate_completion(prompt_text)
                parsed_label, parsed_description = _parse_llm_label(completion_text, label_text)
                label_text = parsed_label or label_text
                description_text = parsed_description or description_text
            except Exception:
                label_text = label_text
            clusters_payload.append(
                {
                    "id": info.id,
                    "label": f"Cluster {info.id + 1}: {label_text}",
                    "size": info.size,
                    "centroid": info.centroid,
                    "metrics": averages,
                    "description": description_text,
                    "llm_output": completion_text,
                    "llm_prompt": prompt_text,
                    "comparison": _extract_comparison(completion_text),
                }
            )
        _persist_clustering_meta(dataset_name, algorithm, parameters, clusters_payload, theme_key)

        return {
            "dataset": dataset_name,
            "theme": theme_key,
            "theme_label": theme_config.get("label") or theme_key,
            "theme_metrics": metric_keys_for_theme,
            "algorithm": algorithm,
            "parameters": parameters,
            "metrics": response_metric_keys,
            "points": points_payload,
            "clusters": clusters_payload,
            "axes": {"x": axes[0], "y": axes[1]},
            "noise": result.noise,
        }

    requests: list[dict[str, object]] = []
    if payload.themes:
        for theme_cfg in payload.themes:
            theme_key, resolved_config = _resolve_theme(theme_cfg.theme)
            requests.append(
                {
                    "theme_key": theme_key,
                    "theme_config": resolved_config,
                    "algorithm": theme_cfg.algorithm or payload.algorithm,
                    "cluster_count": theme_cfg.cluster_count or payload.cluster_count,
                    "min_cluster_size": theme_cfg.min_cluster_size or payload.min_cluster_size,
                    "min_samples": theme_cfg.min_samples or payload.min_samples,
                    "auto_hdbscan": bool(theme_cfg.auto_hdbscan or False),
                    "auto_kmeans": bool(theme_cfg.auto_kmeans or False),
                    "feature_mode": theme_cfg.feature_mode or payload.feature_mode or "metrics",
                    "embedding_dims": theme_cfg.embedding_dims or payload.embedding_dims or 16,
                }
            )
    else:
        theme_key, resolved_config = _resolve_theme(payload.theme)
        requests.append(
            {
                "theme_key": theme_key,
                "theme_config": resolved_config,
                "algorithm": payload.algorithm,
                "cluster_count": payload.cluster_count,
                "min_cluster_size": payload.min_cluster_size,
                "min_samples": payload.min_samples,
                "auto_hdbscan": payload.auto_hdbscan,
                "auto_kmeans": payload.auto_kmeans,
                "feature_mode": payload.feature_mode or "metrics",
                "embedding_dims": payload.embedding_dims or 16,
            }
        )

    results: dict[str, object] = {}
    for params in requests:
        result_payload = run_for_theme(**params)
        results[params["theme_key"]] = result_payload

    if len(results) == 1:
        return next(iter(results.values()))
    return {"dataset": dataset_name, "results": results}


@router.get("/last", name="last-clustering")
async def read_last_clustering(dataset: str | None = None, theme: str | None = None) -> dict[str, object]:
    """
    @brief Load the last saved clustering for a dataset.
    @param dataset Dataset name (optional, uses current selection).
    @param theme Theme identifier (optional, defaults to first theme).
    @return Cached clustering result.
    @throws HTTPException Si aucun clustering n'est disponible ou sur erreur d'E/S.
    """
    dataset_name, _ = resolve_dataset_or_http_error(dataset, require_raw=True)
    ensure_dataset_ready(dataset_name)
    theme_key, theme_config = _resolve_theme(theme)
    return _load_cached_clustering(dataset_name, theme_key, list(theme_config.get("metrics") or METRIC_KEYS))


def _persist_clustering_csv(dataset: str, algorithm: str, points: list[dict[str, object]], theme: str | None) -> None:
    """
    @brief Persist clustering projection to a CSV file for reuse.
    @param dataset Dataset name owning the clustering.
    @param algorithm Algorithm identifier stored alongside the results.
    @param points List of clustering point payloads to serialize.
    @param theme Optional theme key to namespace the cache.
    @throws HTTPException If the CSV cannot be written.
    """
    target = _cache_path(dataset, theme, config.CLUSTERING_CACHE_FILENAME)
    fieldnames = ["path", "cluster", "x", "y", "algorithm", "probabilities"]
    try:
        target.parent.mkdir(parents=True, exist_ok=True)
        with target.open("w", encoding="utf-8", newline="") as handle:
            writer = csv.DictWriter(handle, fieldnames=fieldnames)
            writer.writeheader()
            for point in points:
                probs = point.get("probabilities")
                probs_str = ""
                if isinstance(probs, list):
                    probs_str = ";".join(f"{float(p):.6f}" for p in probs if isinstance(p, (int, float)))
                writer.writerow(
                    {
                        "path": point.get("path"),
                        "cluster": point.get("cluster"),
                        "x": point.get("x"),
                        "y": point.get("y"),
                        "algorithm": algorithm,
                        "probabilities": probs_str,
                    }
                )
    except OSError as exc:
        raise HTTPException(status_code=500, detail=f"Impossible d'enregistrer le clustering: {exc}") from exc


def _persist_clustering_meta(
    dataset: str,
    algorithm: str,
    parameters: dict[str, object],
    clusters: list[dict[str, object]],
    theme: str | None,
) -> None:
    """
    @brief Persist clustering metadata (labels, descriptions, metrics) to JSON.
    """
    target = _cache_path(dataset, theme, config.CLUSTERING_META_FILENAME)
    payload = {
        "dataset": dataset,
        "algorithm": algorithm,
        "parameters": parameters,
        "clusters": clusters,
        "theme": theme,
    }
    try:
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(json.dumps(payload, ensure_ascii=False, indent=2), encoding="utf-8")
    except OSError:
        return


def _load_cached_clustering(dataset: str, theme: str | None, metric_keys: list[str] | None = None) -> dict[str, object]:
    """
    @brief Reload clustering results from the CSV cache and recompute summaries.
    @param dataset Dataset name.
    @param theme Theme identifier for the cached clustering.
    @return Clustering payload reconstructed from cached labels and fresh metrics.
    @throws HTTPException When cache or metrics are unavailable/invalid.
    """
    cache_path = _cache_path(dataset, theme, config.CLUSTERING_CACHE_FILENAME)
    if not cache_path.exists() and theme:
        legacy_cache = dataset_path(dataset) / config.CLUSTERING_CACHE_FILENAME
        if legacy_cache.exists():
            cache_path = legacy_cache
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
        probs_raw = (row.get("probabilities") or "").strip()
        probs: list[float] | None = None
        if probs_raw:
            try:
                probs = [float(val) for val in probs_raw.split(";") if val.strip() != ""]
            except ValueError:
                probs = None
        point_lookup[path] = {"cluster": cluster, "coords": (x, y), "probabilities": probs}

    files = load_metrics_entries(dataset)
    if not files:
        raise HTTPException(status_code=404, detail=config.MESSAGES["CLUSTERING_NO_METRICS"])

    metric_keys = list(metric_keys or METRIC_KEYS)
    normalized = normalize_dataset(files, metric_keys)
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
        probs = info.get("probabilities") if info else None
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
                "probabilities": probs,
            }
        )

    clusters_payload = []
    meta_path = _cache_path(dataset, theme, config.CLUSTERING_META_FILENAME)
    if not meta_path.exists() and theme:
        legacy_meta = dataset_path(dataset) / config.CLUSTERING_META_FILENAME
        if legacy_meta.exists():
            meta_path = legacy_meta
    meta_lookup: dict[int, dict[str, object]] = {}
    if meta_path.exists():
        try:
            meta_payload = json.loads(meta_path.read_text(encoding="utf-8"))
            for entry in meta_payload.get("clusters", []) or []:
                cid = entry.get("id")
                if isinstance(cid, int):
                    meta_lookup[cid] = entry
        except (OSError, json.JSONDecodeError):
            meta_lookup = {}

    for cluster_id, count in sorted(cluster_counts.items()):
        sums = cluster_metric_sums.get(cluster_id, {})
        averages: dict[str, float] = {}
        for key in metric_keys:
            total = sums.get(key)
            if total is not None:
                averages[key] = total / count
        centroid = _average_vector(clusters_vectors.get(cluster_id, []))
        entry = {
            "id": cluster_id,
            "label": f"Cluster {cluster_id + 1}",
            "size": count,
            "centroid": centroid,
            "metrics": averages,
        }
        if cluster_id in meta_lookup:
            meta_entry = meta_lookup[cluster_id]
            if meta_entry.get("label"):
                entry["label"] = meta_entry["label"]
            if meta_entry.get("description"):
                entry["description"] = meta_entry["description"]
            if meta_entry.get("llm_output"):
                entry["llm_output"] = meta_entry["llm_output"]
            if meta_entry.get("llm_prompt"):
                entry["llm_prompt"] = meta_entry["llm_prompt"]
            if meta_entry.get("comparison"):
                entry["comparison"] = meta_entry["comparison"]
        clusters_payload.append(entry)

    noise = sum(1 for label in labels if label == -1)
    axes = {"x": "Component 1", "y": "Component 2"}
    parameters = {"mode": "cached", "source": cache_path.name, "theme": theme}
    # Persist meta in case labels/descriptions need to survive reloads.
    _persist_clustering_meta(dataset, algorithm, parameters, clusters_payload, theme)
    return {
        "dataset": dataset,
        "theme": theme,
        "theme_label": (THEMES.get(theme, {}) if theme else {}).get("label") or theme,
        "algorithm": algorithm,
        "parameters": parameters,
        "metrics": metric_keys,
        "points": points_payload,
        "clusters": clusters_payload,
        "axes": axes,
        "noise": noise,
    }
