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

from Clustering import METRIC_KEYS, normalize_dataset
from Files.dataset_manager import dataset_path
from Files.excluded_files import load_excluded_files, normalize_excluded_path
from Metrics.other_metrics import load_metrics_entries
from LLM import generate_completion
from Pipeline import (
    AssembleClustersStep,
    AssembleClustersStepConfig,
    BuildFeaturesStep,
    BuildFeaturesStepConfig,
    ClusteringArtifact,
    AutoGMMStep,
    AutoGMMStepConfig,
    AutoHDBSCANStep,
    AutoHDBSCANStepConfig,
    AutoKMeansStep,
    AutoKMeansStepConfig,
    AutoOpticsStep,
    AutoOpticsStepConfig,
    GMMStep,
    GMMStepConfig,
    HDBSCANStep,
    HDBSCANStepConfig,
    KMeansStep,
    KMeansStepConfig,
    OpticsStep,
    OpticsStepConfig,
    Pipeline,
    ProjectionStep,
)
from Routers.tasks import notify_tasks_sync
from Routers.utils import (
    ensure_dataset_ready,
    resolve_dataset_or_http_error,
    resolve_repository_or_http_error,
    resolve_dataset_objects,
    sanitize_for_llm_prompt,
)

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
def _load_structural_embeddings(repository) -> dict[str, list[float]]:
    """
    @brief Load precomputed structural embeddings and collapse per-file averages.
    @return Mapping of normalized file path to embedding vector.
    """
    return repository.load_embeddings()


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
    dataset_name, dataset_obj, repository = resolve_dataset_objects(payload.dataset, require_raw=True)
    ensure_dataset_ready(dataset_name)
    base_files = load_metrics_entries(dataset_name)
    if not base_files:
        raise HTTPException(status_code=400, detail=config.MESSAGES["CLUSTERING_NO_DATA"])
    excluded_files = list(dataset_obj.metadata.get("excluded_files") or []) or load_excluded_files(dataset_name)
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
            theme_key,
            dataset_name,
            algorithm,
            feature_mode,
            embedding_dims,
        )
        metric_keys_for_theme = list(theme_config.get("metrics") or METRIC_KEYS)
        embeddings = _load_structural_embeddings(repository) if feature_mode in {"embeddings", "both"} else {}
        if excluded_lookup and embeddings:
            embeddings = {path: vector for path, vector in embeddings.items() if not _is_excluded_path(path, excluded_lookup)}
        metrics_lookup: dict[str, dict[str, object]] = {
            entry.get("path", "").replace("\\", "/"): entry.get("metrics") or {} for entry in base_files
        }

        artifact = ClusteringArtifact(
            dataset=dataset_name,
            base_files=tuple(base_files),
            embeddings=embeddings,
            metrics_lookup=metrics_lookup,
        )
        if algorithm == "kmeans":
            if auto_kmeans:
                algorithm_step = AutoKMeansStep(
                    AutoKMeansStepConfig(feature_mode=feature_mode, embedding_dims=embedding_dims)
                )
            else:
                algorithm_step = KMeansStep(
                    KMeansStepConfig(
                        feature_mode=feature_mode,
                        embedding_dims=embedding_dims,
                        cluster_count=cluster_count or 0,
                    )
                )
        elif algorithm == "gmm":
            if auto_gmm:
                algorithm_step = AutoGMMStep(
                    AutoGMMStepConfig(
                        feature_mode=feature_mode,
                        embedding_dims=embedding_dims,
                        cluster_count=cluster_count,
                        covariance_type=gmm_covariance_type,
                    )
                )
            else:
                algorithm_step = GMMStep(
                    GMMStepConfig(
                        feature_mode=feature_mode,
                        embedding_dims=embedding_dims,
                        cluster_count=cluster_count or 0,
                        covariance_type=gmm_covariance_type,
                    )
                )
        elif algorithm == "optics":
            if auto_optics:
                algorithm_step = AutoOpticsStep(
                    AutoOpticsStepConfig(
                        feature_mode=feature_mode,
                        embedding_dims=embedding_dims,
                        min_samples=min_samples,
                        xi=optics_xi,
                        max_eps=optics_max_eps,
                    )
                )
            else:
                algorithm_step = OpticsStep(
                    OpticsStepConfig(
                        feature_mode=feature_mode,
                        embedding_dims=embedding_dims,
                        min_samples=min_samples,
                        xi=optics_xi,
                        max_eps=optics_max_eps,
                    )
                )
        elif algorithm == "hdbscan":
            if auto_hdbscan:
                algorithm_step = AutoHDBSCANStep(
                    AutoHDBSCANStepConfig(feature_mode=feature_mode, embedding_dims=embedding_dims)
                )
            else:
                algorithm_step = HDBSCANStep(
                    HDBSCANStepConfig(
                        feature_mode=feature_mode,
                        embedding_dims=embedding_dims,
                        min_cluster_size=min_cluster_size,
                        min_samples=min_samples,
                    )
                )
        else:
            raise HTTPException(status_code=400, detail=f"Unsupported clustering algorithm '{algorithm}'.")
        steps = [
            BuildFeaturesStep(
                BuildFeaturesStepConfig(
                    metric_keys=metric_keys_for_theme, feature_mode=feature_mode, embedding_dims=embedding_dims
                )
            ),
            algorithm_step,
            ProjectionStep(),
            AssembleClustersStep(
                AssembleClustersStepConfig(
                    repository=repository,
                    theme_key=theme_key,
                    theme_config=theme_config,
                    algorithm=algorithm,
                    feature_mode=feature_mode,
                    embedding_dims=embedding_dims,
                    metrics_lookup=metrics_lookup,
                    excluded_lookup=excluded_lookup,
                    generate_descriptions=generate_descriptions,
                )
            ),
        ]
        try:
            final_artifact = Pipeline(steps).run(artifact)
        except ValueError as exc:
            raise HTTPException(status_code=400, detail=str(exc)) from exc
        response = (final_artifact.metadata.get("response") or {}).copy()
        if not response:
            raise HTTPException(status_code=400, detail=config.MESSAGES["CLUSTERING_NO_DATA"])

        response_metadata = response.get("metadata") or {}
        response_metadata["timestamp"] = _now_iso()
        response["metadata"] = response_metadata

        points_payload = response.get("points") or []
        clusters_payload = response.get("clusters") or []
        parameters = response.get("parameters") or {}

        _persist_clustering_csv(dataset_name, algorithm, points_payload, theme_key)
        _persist_clustering_meta(dataset_name, algorithm, parameters, clusters_payload, theme_key, response_metadata)

        background_jobs.extend(final_artifact.metadata.get("background_jobs") or [])
        return response
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
