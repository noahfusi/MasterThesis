from __future__ import annotations

from typing import Literal
from datetime import datetime, timezone
import logging
import re

import csv
import json
from pathlib import Path

import config
from fastapi import APIRouter, BackgroundTasks, HTTPException
from pydantic import BaseModel, Field

from Clustering import (
    METRIC_KEYS,
    normalize_dataset,
    project_to_components,
    run_hdbscan_clustering,
    run_kmeans_clustering,
    run_gmm_clustering,
    run_optics_clustering,
    auto_gmm_with_bic,
    auto_kmeans_with_silhouette,
    auto_hdbscan_with_dbcv,
    auto_optics_with_silhouette,
    build_feature_dataset,
)
from Files.dataset_manager import dataset_path
from Files.excluded_files import load_excluded_files, normalize_excluded_path
from Metrics.other_metrics import load_metrics_entries
from LLM import generate_completion
from Routers.tasks import notify_tasks_sync
from Routers.utils import ensure_dataset_ready, resolve_dataset_or_http_error, sanitize_for_llm_prompt

router = APIRouter(prefix="/clustering", tags=["clustering"])
logger = logging.getLogger("uvicorn.error")
THEMES = config.CLUSTERING_THEMES


def _now_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


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


def _normalize_path_key(value: str | None) -> str:
    normalized = normalize_excluded_path(value)
    if normalized:
        return normalized
    return str(value).replace("\\", "/").strip() if value else ""


def _is_excluded_path(path: str | None, excluded: set[str]) -> bool:
    key = _normalize_path_key(path)
    return bool(key and key in excluded)


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


def _extract_good_bad(completion: str | None) -> tuple[list[str], list[str]]:
    """
    @brief Extract Good/Bad bullet lists from an LLM completion.
    """
    if not completion or not isinstance(completion, str):
        return ([], [])
    lines = [line.strip().strip('"').strip("'") for line in completion.splitlines() if line.strip()]

    def collect(label: str) -> list[str]:
        start_idx = next((i for i, line in enumerate(lines) if re.match(rf"^{label}\s*:", line, flags=re.IGNORECASE)), None)
        if start_idx is None:
            return []
        bullets: list[str] = []
        inline = re.sub(rf"^{label}\s*:\s*", "", lines[start_idx], flags=re.IGNORECASE).strip()
        if inline:
            bullets.append(inline)
        for i in range(start_idx + 1, len(lines)):
            row = lines[i]
            if re.match(r"^[A-Za-z]+\s*:", row) or row.startswith("|"):
                break
            candidate = re.sub(r"^[-*]\s*", "", row).strip()
            if candidate:
                bullets.append(candidate)
        return bullets

    return collect("good"), collect("bad")


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
        averaged: list[float] | None = None
        embedding = payload.get("embedding")
        if isinstance(embedding, list) and all(isinstance(x, (int, float)) for x in embedding):
            averaged = [float(x) for x in embedding]
        else:
            segments = payload.get("segments") or []
            vectors: list[list[float]] = []
            for segment in segments:
                segment_embedding = segment.get("embedding")
                if isinstance(segment_embedding, list) and all(isinstance(x, (int, float)) for x in segment_embedding):
                    vectors.append([float(x) for x in segment_embedding])
            if vectors:
                dim = len(vectors[0])
                vectors = [vec for vec in vectors if len(vec) == dim]
                if vectors:
                    averaged = [sum(values) / len(vectors) for values in zip(*vectors)]
        if not averaged:
            continue
        source = payload.get("source") or path.relative_to(root).as_posix()
        normalized_source = str(source).replace("\\", "/")
        normalized_source = normalized_source.replace(".structural.txt", "")
        normalized_source = normalized_source.lstrip("/")
        embeddings[normalized_source] = averaged
    return embeddings


def _merge_cluster_into_meta(
    dataset: str,
    theme: str | None,
    algorithm: str,
    parameters: dict[str, object],
    cluster: dict[str, object],
) -> None:
    """
    @brief Update (or append) a cluster entry in the meta cache while keeping existing fields.
    """
    meta_path = _cache_path(dataset, theme, config.CLUSTERING_META_FILENAME)
    existing_clusters: list[dict[str, object]] = []
    stored_algorithm = algorithm
    stored_parameters = dict(parameters)
    stored_metadata: dict[str, object] | None = None
    if meta_path.exists():
        try:
            payload = json.loads(meta_path.read_text(encoding="utf-8"))
            existing_clusters = list(payload.get("clusters") or [])
            stored_algorithm = payload.get("algorithm") or stored_algorithm
            stored_parameters = payload.get("parameters") or stored_parameters
            stored_metadata = payload.get("metadata") or stored_metadata
        except (OSError, json.JSONDecodeError):
            existing_clusters = []
    merged: list[dict[str, object]] = []
    replaced = False
    for entry in existing_clusters:
        if entry.get("id") == cluster.get("id"):
            merged.append({**entry, **cluster})
            replaced = True
        else:
            merged.append(entry)
    if not replaced:
        merged.append(cluster)
    _persist_clustering_meta(dataset, stored_algorithm, stored_parameters, merged, theme, stored_metadata)


def _generate_cluster_description(job: dict[str, object]) -> None:
    """
    @brief Background job to request an LLM description/label for a cluster and persist it.
    """
    dataset = job.get("dataset")
    theme = job.get("theme")
    algorithm = job.get("algorithm") or "hdbscan"
    parameters = job.get("parameters") or {}
    cluster = dict(job.get("cluster") or {})
    cluster_id = cluster.get("id")
    prompt = cluster.get("llm_prompt") or job.get("prompt")
    base_label = job.get("label_prefix") or cluster.get("label")
    if dataset is None or cluster_id is None or not prompt:
        return
    default_label = base_label or f"Cluster {cluster_id + 1}"
    logger.info("[clustering][llm] dataset=%s theme=%s cluster=%s requesting description", dataset, theme, cluster_id)
    completion_text = None
    comparison: list[dict[str, str]] = []
    description_text = cluster.get("description")
    label_text = default_label
    try:
        completion_text = generate_completion(prompt)
        parsed_label, parsed_description = _parse_llm_label(completion_text, default_label)
        label_text = parsed_label or default_label
        description_text = parsed_description or description_text or completion_text
        comparison = _extract_comparison(completion_text)
        good_points, bad_points = _extract_good_bad(completion_text)
    except Exception as exc:  # noqa: BLE001
        description_text = description_text or f"LLM generation failed: {exc}"
        good_points, bad_points = ([], [])
    updated_cluster = {
        **cluster,
        "label": f"{default_label}: {label_text}" if label_text and label_text != default_label else label_text,
        "description": description_text,
        "llm_output": completion_text,
        "llm_prompt": prompt,
        "llm_pending": False,
        "comparison": comparison,
        "good": good_points,
        "bad": bad_points,
    }
    _merge_cluster_into_meta(dataset, theme, algorithm, parameters, updated_cluster)
    notify_tasks_sync(
        {
            "type": "clustering-description-completed",
            "dataset": dataset,
            "theme": theme,
            "cluster_id": cluster_id,
        }
    )


class ClusteringRequest(BaseModel):
    """
    @brief Request payload for triggering clustering.
    """
    dataset: str | None = None
    theme: str | None = None
    themes: list[ThemeClusteringConfig] | None = None
    algorithm: Literal["kmeans", "hdbscan", "gmm", "optics"] = "kmeans"
    cluster_count: int | None = Field(default=None, ge=2, le=200)
    min_cluster_size: int | None = Field(default=None, ge=2, le=500)
    min_samples: int | None = Field(default=None, ge=1, le=500)
    optics_xi: float | None = Field(default=None, ge=0, le=1)
    optics_max_eps: float | None = Field(default=None, ge=0)
    auto_hdbscan: bool = False
    auto_kmeans: bool = False
    auto_gmm: bool = False
    auto_optics: bool = False
    gmm_covariance_type: str | None = None
    feature_mode: Literal["metrics", "embeddings", "both"] = "metrics"
    embedding_dims: int = Field(default=16, ge=2, le=128)
    generate_descriptions: bool = True


class ThemeClusteringConfig(BaseModel):
    """
    @brief Per-theme clustering configuration.
    """
    dataset: str | None = None
    theme: str | None = None
    algorithm: Literal["kmeans", "hdbscan", "gmm", "optics"] = "kmeans"
    cluster_count: int | None = Field(default=None, ge=2, le=200)
    min_cluster_size: int | None = Field(default=None, ge=2, le=500)
    min_samples: int | None = Field(default=None, ge=1, le=500)
    optics_xi: float | None = Field(default=None, ge=0, le=1)
    optics_max_eps: float | None = Field(default=None, ge=0)
    auto_hdbscan: bool = False
    auto_kmeans: bool = False
    auto_gmm: bool = False
    auto_optics: bool = False
    gmm_covariance_type: str | None = None
    feature_mode: Literal["metrics", "embeddings", "both"] | None = None
    embedding_dims: int | None = Field(default=None, ge=2, le=128)
    generate_descriptions: bool | None = None


@router.post("/run")
async def launch_clustering(payload: ClusteringRequest, background_tasks: BackgroundTasks) -> dict[str, object]:
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
    excluded_files = load_excluded_files(dataset_name)
    excluded_lookup = {_normalize_path_key(path) for path in excluded_files}
    if excluded_lookup:
        base_files = [
            entry
            for entry in base_files
            if not _is_excluded_path(entry.get("path") or entry.get("raw_path"), excluded_lookup)
        ]
    if not base_files:
        detail = config.MESSAGES.get("CLUSTERING_ALL_EXCLUDED") or "No files available for clustering after exclusions."
        raise HTTPException(status_code=400, detail=detail)
    background_jobs: list[dict[str, object]] = []

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
        auto_gmm: bool,
        auto_optics: bool,
        optics_xi: float | None,
        optics_max_eps: float | None,
        gmm_covariance_type: str | None,
        feature_mode: str,
        embedding_dims: int,
        generate_descriptions: bool,
    ) -> dict[str, object]:
        logger.info(
            "[clustering] triggering theme=%s dataset=%s algo=%s feature_mode=%s dims=%s",
            theme_key, dataset_name, algorithm, feature_mode, embedding_dims
        )
        metric_keys_for_theme = list(theme_config.get("metrics") or METRIC_KEYS)
        embeddings = _load_structural_embeddings(dataset_name) if feature_mode in {"embeddings", "both"} else {}
        if excluded_lookup and embeddings:
            embeddings = {path: vector for path, vector in embeddings.items() if not _is_excluded_path(path, excluded_lookup)}
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
        elif algorithm == "gmm":
            cov_type = gmm_covariance_type or "full"
            try:
                if auto_gmm:
                    counts = [cluster_count] if cluster_count else None
                    result, chosen = auto_gmm_with_bic(
                        normalized,
                        component_counts=counts,
                        covariance_types=[cov_type] if cov_type else None,
                        reg_covar=1e-3,
                        n_init=5,
                    )
                    parameters.update({"mode": "auto", **chosen})
                else:
                    if not cluster_count:
                        raise HTTPException(status_code=400, detail=config.MESSAGES["CLUSTERING_GMM_COUNT_REQUIRED"])
                    result = run_gmm_clustering(normalized, cluster_count, covariance_type=cov_type, reg_covar=1e-3, n_init=5)
                    parameters["cluster_count"] = cluster_count
                    parameters["covariance_type"] = cov_type
            except RuntimeError as exc:
                raise HTTPException(
                    status_code=400,
                    detail="GMM did not produce multiple clusters. Adjust components or enable auto mode.",
                ) from exc
        elif algorithm == "optics":
            try:
                if auto_optics:
                    result, chosen = auto_optics_with_silhouette(
                        normalized,
                        min_samples_values=[min_samples] if min_samples else None,
                        xi_values=[optics_xi] if optics_xi is not None else None,
                        max_eps_values=[optics_max_eps] if optics_max_eps is not None else None,
                    )
                    parameters.update({"mode": "auto", **chosen})
                    min_samples = chosen.get("min_samples") if isinstance(chosen, dict) else min_samples
                    optics_xi = chosen.get("xi") if isinstance(chosen, dict) else optics_xi
                    optics_max_eps = chosen.get("max_eps") if isinstance(chosen, dict) else optics_max_eps
                else:
                    heuristic = max(2, min(len(normalized.entries) // 8 or 2, 25))
                    selected_min_samples = max(2, min_samples or heuristic)
                    xi_value = optics_xi if optics_xi is not None else 0.05
                    eps_value = optics_max_eps if optics_max_eps not in (None, 0, 0.0) else None
                    result = run_optics_clustering(
                        normalized,
                        min_samples=selected_min_samples,
                        xi=xi_value,
                        max_eps=eps_value,
                    )
                    min_samples = selected_min_samples
                    parameters.update(
                        {
                            "min_samples": selected_min_samples,
                            "xi": xi_value,
                            "max_eps": eps_value,
                        }
                    )
            except RuntimeError as exc:
                raise HTTPException(
                    status_code=400,
                    detail="OPTICS did not produce any clusters. Adjust parameters or enable auto mode.",
                ) from exc
        elif algorithm == "hdbscan":
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
        else:
            raise HTTPException(status_code=400, detail=f"Unsupported clustering algorithm '{algorithm}'.")
        if algorithm in {"hdbscan", "optics"}:
            parameters["min_samples"] = min_samples if min_samples is not None else parameters.get("min_samples")
        parameters["feature_mode"] = feature_mode
        parameters["embedding_dims"] = embedding_dims
        parameters["generate_descriptions"] = generate_descriptions
        logger.info(
            "[clustering] dataset=%s theme=%s algo=%s mode=%s dims=%s params=%s",
            dataset_name, theme_key, algorithm, feature_mode, embedding_dims, parameters
        )

        projection, axes = project_to_components(normalized, components=2)
        if not projection:
            projection = [(0.0, 0.0) for _ in normalized.entries]
        if len(axes) < 2:
            axes = ["Component 1", "Component 2"]
        axes_payload = {"x": axes[0], "y": axes[1]}

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
        # Precompute per-cluster representatives (up to 3) using probabilities when available.
        rep_candidates: dict[int, list[tuple[str, float | None]]] = {}
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
                rep_candidates.setdefault(label, []).append((entry.path, probs[label] if isinstance(probs, list) and len(probs) > label else None))
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

        clusters_payload: list[dict[str, object]] = []
        for info in result.clusters:
            averages: dict[str, float] = {}
            count = cluster_counts.get(info.id, 0)
            if count:
                sums = cluster_metric_sums.get(info.id, {})
                for key in metric_keys:
                    total = sums.get(key)
                    if total is not None:
                        averages[key] = total / count
            base_label = f"Cluster {info.id + 1}"
            prompt_text = None
            representatives_for_prompt: list[str] = []
            representative_paths: list[dict[str, object]] = []
            if generate_descriptions:
                try:
                    candidates = rep_candidates.get(info.id, [])
                    # Sort by confidence desc when available, otherwise keep order.
                    candidates_sorted = sorted(
                        candidates,
                        key=lambda item: (item[1] is not None, item[1] if item[1] is not None else 0.0),
                        reverse=True,
                    )
                    selected = candidates_sorted[:3] if candidates_sorted else []
                    for path, conf in selected:
                        representative_paths.append(
                            {"path": path, "confidence": float(conf) if conf is not None else None}
                        )
                        rep_path = dataset_path(dataset_name) / config.RAW_FOLDER_NAME / path
                        content = None
                        if rep_path.is_file():
                            try:
                                content = rep_path.read_text(encoding="utf-8")
                            except OSError:
                                content = None
                        snippet = (content or "").strip()
                        # Sanitize code snippet before including in LLM prompt
                        sanitized_snippet = sanitize_for_llm_prompt(snippet, max_length=2000)
                        sanitized_path = sanitize_for_llm_prompt(path, max_length=500)
                        representatives_for_prompt.append(
                            f"File: {sanitized_path} (confidence {conf:.2f} if available)\n{sanitized_snippet or 'Content unavailable.'}"
                        )
                    reps_text = "\n\n".join(representatives_for_prompt) if representatives_for_prompt else "N/A"
                    prompt_text = config.CLUSTER_LABEL_PROMPT.format(
                        cluster_metrics=averages,
                        dataset_metrics=dataset_metric_averages,
                        representatives=reps_text,
                    )
                except Exception:
                    prompt_text = None
            # Defer LLM labeling/description to a background task.
            cluster_entry = {
                "id": info.id,
                "label": base_label,
                "size": info.size,
                "centroid": info.centroid,
                "metrics": averages,
                "description": None,
                "llm_output": None,
                "llm_prompt": prompt_text,
                "llm_pending": bool(prompt_text),
                "comparison": [],
                "good": [],
                "bad": [],
                "representative_paths": representative_paths,
            }
            clusters_payload.append(cluster_entry)
            if prompt_text and generate_descriptions:
                background_jobs.append(
                    {
                        "dataset": dataset_name,
                        "theme": theme_key,
                        "algorithm": algorithm,
                        "parameters": dict(parameters),
                        "cluster": dict(cluster_entry),
                        "label_prefix": base_label,
                    }
                )
        metadata = {
            "dataset": dataset_name,
            "theme": theme_key,
            "theme_label": theme_config.get("label") or theme_key,
            "algorithm": algorithm,
            "feature_mode": feature_mode,
            "embedding_dims": embedding_dims,
            "axes": axes_payload,
            "metrics": response_metric_keys,
            "parameters": dict(parameters),
            "timestamp": _now_iso(),
            "point_count": len(points_payload),
            "cluster_count": len(clusters_payload),
            "excluded_count": len(excluded_lookup),
            "excluded_files": sorted(excluded_lookup),
            "generate_descriptions": generate_descriptions,
        }

        _persist_clustering_meta(dataset_name, algorithm, parameters, clusters_payload, theme_key, metadata)

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
            "axes": axes_payload,
            "metadata": metadata,
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
                    "auto_gmm": bool(theme_cfg.auto_gmm or False),
                    "auto_optics": bool(theme_cfg.auto_optics or False),
                    "optics_xi": theme_cfg.optics_xi or payload.optics_xi,
                    "optics_max_eps": theme_cfg.optics_max_eps or payload.optics_max_eps,
                    "gmm_covariance_type": theme_cfg.gmm_covariance_type or payload.gmm_covariance_type,
                    "feature_mode": theme_cfg.feature_mode or payload.feature_mode or "metrics",
                    "embedding_dims": theme_cfg.embedding_dims or payload.embedding_dims or 16,
                    "generate_descriptions": (
                        theme_cfg.generate_descriptions
                        if theme_cfg.generate_descriptions is not None
                        else payload.generate_descriptions
                    ),
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
                    "auto_gmm": payload.auto_gmm,
                    "auto_optics": payload.auto_optics,
                    "optics_xi": payload.optics_xi,
                    "optics_max_eps": payload.optics_max_eps,
                    "gmm_covariance_type": payload.gmm_covariance_type,
                    "feature_mode": payload.feature_mode or "metrics",
                    "embedding_dims": payload.embedding_dims or 16,
                    "generate_descriptions": payload.generate_descriptions,
                }
            )

    results: dict[str, object] = {}
    for params in requests:
        result_payload = run_for_theme(**params)
        results[params["theme_key"]] = result_payload
    for job in background_jobs:
        background_tasks.add_task(_generate_cluster_description, job)

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
    @throws HTTPException If no clustering is available or on I/O error.
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
        raise HTTPException(status_code=500, detail=f"Unable to save clustering: {exc}") from exc


def _persist_clustering_meta(
    dataset: str,
    algorithm: str,
    parameters: dict[str, object],
    clusters: list[dict[str, object]],
    theme: str | None,
    metadata: dict[str, object] | None = None,
) -> None:
    """
    @brief Persist clustering metadata (labels, descriptions, metrics) to JSON.
    """
    target = _cache_path(dataset, theme, config.CLUSTERING_META_FILENAME)
    existing_metadata: dict[str, object] | None = None
    if target.exists():
        try:
            existing_payload = json.loads(target.read_text(encoding="utf-8"))
            if isinstance(existing_payload, dict):
                existing_metadata = existing_payload.get("metadata")
        except (OSError, json.JSONDecodeError):
            existing_metadata = None

    payload = {
        "dataset": dataset,
        "algorithm": algorithm,
        "parameters": parameters,
        "clusters": clusters,
        "theme": theme,
    }
    metadata_payload = metadata if metadata is not None else existing_metadata
    if metadata_payload is not None:
        payload["metadata"] = metadata_payload
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

    excluded_set = {_normalize_path_key(path) for path in load_excluded_files(dataset)}

    # Check file size before loading
    if cache_path.stat().st_size > config.MAX_CSV_FILE_SIZE:
        raise HTTPException(status_code=413, detail="Clustering cache file too large to load.")

    try:
        with cache_path.open("r", encoding="utf-8") as handle:
            reader = csv.DictReader(handle)
            # Limit number of rows to prevent memory exhaustion
            records = []
            for i, row in enumerate(reader):
                if i >= config.MAX_CSV_ROWS:
                    logger.warning("[clustering] CSV row limit reached: %s rows, truncating", config.MAX_CSV_ROWS)
                    break
                records.append(row)
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
        if excluded_set and _is_excluded_path(path, excluded_set):
            continue
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
        point_entry = {"cluster": cluster, "coords": (x, y), "probabilities": probs}
        point_lookup[path] = point_entry
        normalized_key = _normalize_path_key(path)
        if normalized_key and normalized_key != path:
            point_lookup[normalized_key] = point_entry

    files = [
        entry
        for entry in load_metrics_entries(dataset)
        if not _is_excluded_path(entry.get("path") or entry.get("raw_path"), excluded_set)
    ]
    if not files:
        detail = config.MESSAGES.get("CLUSTERING_ALL_EXCLUDED") or config.MESSAGES["CLUSTERING_NO_METRICS"]
        raise HTTPException(status_code=404, detail=detail)

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
    stored_parameters: dict[str, object] | None = None
    run_metadata: dict[str, object] | None = None
    if meta_path.exists():
        try:
            meta_payload = json.loads(meta_path.read_text(encoding="utf-8"))
            stored_parameters = meta_payload.get("parameters")
            meta_metadata = meta_payload.get("metadata")
            if isinstance(meta_metadata, dict):
                run_metadata = meta_metadata
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
            if meta_entry.get("good") is not None:
                entry["good"] = meta_entry.get("good")
            if meta_entry.get("bad") is not None:
                entry["bad"] = meta_entry.get("bad")
            if meta_entry.get("representative_paths") is not None:
                reps = meta_entry.get("representative_paths")
                if isinstance(reps, list):
                    filtered_reps = []
                    for rep in reps:
                        path_value = rep.get("path") if isinstance(rep, dict) else rep
                        if _is_excluded_path(path_value, excluded_set):
                            continue
                        filtered_reps.append(rep)
                    entry["representative_paths"] = filtered_reps
                else:
                    entry["representative_paths"] = reps
            if "llm_pending" in meta_entry:
                entry["llm_pending"] = bool(meta_entry.get("llm_pending"))
        entry.setdefault("llm_pending", False)
        clusters_payload.append(entry)

    noise = sum(1 for label in labels if label == -1)
    axes = {"x": "Component 1", "y": "Component 2"}
    if isinstance(run_metadata, dict) and isinstance(run_metadata.get("axes"), dict):
        stored_axes = run_metadata.get("axes") or {}
        axes = {
            "x": stored_axes.get("x") or axes["x"],
            "y": stored_axes.get("y") or axes["y"],
        }
    parameters = {
        **(stored_parameters if isinstance(stored_parameters, dict) else {}),
        "mode": "cached",
        "source": cache_path.name,
        "theme": theme,
    }
    run_metadata = run_metadata or {}
    run_metadata.setdefault("dataset", dataset)
    run_metadata.setdefault("theme", theme)
    run_metadata.setdefault("theme_label", (THEMES.get(theme, {}) if theme else {}).get("label") or theme)
    run_metadata.setdefault("algorithm", algorithm)
    run_metadata.setdefault("axes", axes)
    run_metadata.setdefault("metrics", metric_keys)
    run_metadata.setdefault("parameters", parameters)
    run_metadata.setdefault("timestamp", _now_iso())
    run_metadata["excluded_count"] = len(excluded_set)
    run_metadata["excluded_files"] = sorted(excluded_set)
    run_metadata.setdefault("generate_descriptions", True)
    run_metadata["point_count"] = len(points_payload)
    run_metadata["cluster_count"] = len(clusters_payload)
    # Persist meta in case labels/descriptions need to survive reloads.
    _persist_clustering_meta(dataset, algorithm, parameters, clusters_payload, theme, run_metadata)
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
        "metadata": run_metadata,
        "noise": noise,
    }
