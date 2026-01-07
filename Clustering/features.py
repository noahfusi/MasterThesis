from __future__ import annotations

import warnings
from typing import Sequence

import config
from Clustering.common import METRIC_KEYS, NormalizedDataset, NormalizedEntry
from Clustering.utils import (
    center_vectors,
    collect_metric_vectors,
    coerce_numeric,
    default_axes,
    dot_product,
    pca_components,
    project_embeddings,
    robust_scale_matrix,
)


def normalize_dataset(files: Sequence[dict[str, object]], metric_keys: Sequence[str] | None = None) -> NormalizedDataset:
    """
    Extract and normalize metric vectors for clustering.
    """
    candidate_keys = list(metric_keys or METRIC_KEYS)
    if not candidate_keys:
        return NormalizedDataset(metric_keys=[], entries=[])

    def _collect(keys: list[str]) -> list[tuple[str, dict[str, float], list[float]]]:
        collected_local: list[tuple[str, dict[str, float], list[float]]] = []
        for index, entry in enumerate(files):
            metrics = entry.get("metrics")
            if not isinstance(metrics, dict):
                continue

            raw_values: list[float] = []
            raw_metrics: dict[str, float] = {}
            for key in keys:
                raw_value = coerce_numeric(metrics.get(key))
                if raw_value is None:
                    raw_values = []
                    raw_metrics = {}
                    break
                raw_metrics[key] = raw_value
                raw_values.append(raw_value)

            if len(raw_values) != len(keys):
                continue

            path = (
                entry.get("path")
                or entry.get("raw_path")
                or entry.get("filename")
                or entry.get("file")
                or f"entry-{index}"
            )
            collected_local.append((str(path).replace("\\", "/"), raw_metrics, raw_values))
        return collected_local

    collected = _collect(candidate_keys)

    if not collected or len(collected) < len(files):
        numeric_key_sets: list[set[str]] = []
        for entry in files:
            metrics = entry.get("metrics")
            if not isinstance(metrics, dict):
                continue
            numeric_keys = {str(k) for k, v in metrics.items() if coerce_numeric(v) is not None}
            if numeric_keys:
                numeric_key_sets.append(numeric_keys)
        if numeric_key_sets:
            intersection_keys = set.intersection(*numeric_key_sets) if numeric_key_sets else set()
            ordered_keys = [k for k in METRIC_KEYS if k in intersection_keys] + [
                k for k in sorted(intersection_keys) if k not in METRIC_KEYS
            ]
            fallback_keys = [k for k in ordered_keys if k]
            if fallback_keys:
                fallback_collected = _collect(fallback_keys)
                if fallback_collected and len(fallback_collected) > len(collected):
                    candidate_keys = fallback_keys
                    collected = fallback_collected

    if not collected:
        return NormalizedDataset(metric_keys=candidate_keys, entries=[])

    raw_matrix = [raw_values for _, _, raw_values in collected]
    normalized_matrix = robust_scale_matrix(raw_matrix)

    normalized_entries: list[NormalizedEntry] = []
    for (path, metrics, _), normalized in zip(collected, normalized_matrix):
        normalized_entries.append(NormalizedEntry(path=path, metrics=metrics, vector=list(normalized)))

    return NormalizedDataset(metric_keys=candidate_keys, entries=normalized_entries)


def build_feature_dataset(
    files: Sequence[dict[str, object]],
    embeddings: dict[str, Sequence[float]],
    *,
    feature_mode: str,
    embedding_dims: int,
    metric_keys: Sequence[str] | None = None,
) -> NormalizedDataset:
    """
    Assemble a normalized dataset using metrics, embeddings, or both.
    """
    mode = (feature_mode or "metrics").lower()
    keys = list(metric_keys or METRIC_KEYS) if mode in {"metrics", "both"} else []

    metric_vectors: dict[str, list[float]] = {}
    metric_payloads: dict[str, dict[str, float]] = {}
    if keys:
        metric_vectors, metric_payloads = collect_metric_vectors(files, keys)
        initial_count = len(metric_vectors)
        if not metric_vectors or len(metric_vectors) < len(files):
            numeric_key_sets: list[set[str]] = []
            for entry in files:
                metrics = entry.get("metrics")
                if not isinstance(metrics, dict):
                    continue
                numeric_keys = {str(k) for k, v in metrics.items() if coerce_numeric(v) is not None}
                if numeric_keys:
                    numeric_key_sets.append(numeric_keys)
            if numeric_key_sets:
                intersection_keys = set.intersection(*numeric_key_sets) if numeric_key_sets else set()
                ordered_keys = [k for k in METRIC_KEYS if k in intersection_keys] + [
                    k for k in sorted(intersection_keys) if k not in METRIC_KEYS
                ]
                fallback_keys = [k for k in ordered_keys if k]
                if fallback_keys:
                    fallback_vectors, fallback_payloads = collect_metric_vectors(files, fallback_keys)
                    if len(fallback_vectors) > initial_count:
                        keys = fallback_keys
                        metric_vectors = fallback_vectors
                        metric_payloads = fallback_payloads

        if metric_vectors:
            scaled_metrics = robust_scale_matrix(list(metric_vectors.values()))
            metric_vectors = {path: vector for path, vector in zip(metric_vectors.keys(), scaled_metrics)}

    embedding_vectors: dict[str, list[float]] = {}
    if mode in {"embeddings", "both"} and embeddings:
        embedding_vectors = project_embeddings(embeddings, embedding_dims)

    if mode == "metrics":
        paths = list(metric_vectors.keys())
    elif mode == "embeddings":
        paths = list(embedding_vectors.keys())
    else:
        paths = [path for path in metric_vectors if path in embedding_vectors]
        available = len(paths)
        if available == 0:
            warnings.warn(
                "No overlapping entries between metrics and embeddings; clustering dataset will be empty.",
                RuntimeWarning,
            )
        elif available < max(len(metric_vectors), len(embedding_vectors)) * 0.25:
            warnings.warn(
                "Low overlap between metrics and embeddings; clustering will use a small subset of files.",
                RuntimeWarning,
            )

    combined_vectors: list[list[float]] = []
    normalized_entries: list[NormalizedEntry] = []
    for path in paths:
        metric_part = metric_vectors.get(path, [])
        embedding_part = embedding_vectors.get(path, [])
        if mode == "metrics":
            combined = metric_part
        elif mode == "embeddings":
            combined = embedding_part
        else:
            combined = embedding_part + metric_part
        combined_vectors.append(combined)
        normalized_entries.append(
            NormalizedEntry(
                path=path,
                metrics=metric_payloads.get(path, {}),
                vector=list(combined),
            )
        )

    if combined_vectors:
        scaled = robust_scale_matrix(combined_vectors)
        for entry, vector in zip(normalized_entries, scaled):
            entry.vector = vector
    return NormalizedDataset(metric_keys=keys, entries=normalized_entries)


def project_to_components(dataset: NormalizedDataset, components: int = 2) -> tuple[list[tuple[float, float]], list[str]]:
    """
    Project normalized vectors to principal components for visualization.
    """
    if not dataset.entries or components <= 0:
        return [], []

    vectors = [entry.vector for entry in dataset.entries]
    dims = len(vectors[0]) if vectors else 0
    if dims == 0:
        return [(0.0, 0.0) for _ in vectors], []

    projection_vectors = pca_components(vectors, components)
    if not projection_vectors:
        projection_vectors = default_axes(dims, components)

    centered = center_vectors(vectors)
    coords: list[tuple[float, float]] = [
        (
            dot_product(vector, projection_vectors[0]) if projection_vectors else 0.0,
            dot_product(vector, projection_vectors[1]) if len(projection_vectors) > 1 else 0.0,
        )
        for vector in centered
    ]

    axis_labels = [f"Component {index + 1}" for index in range(min(components, len(projection_vectors)))]
    if len(axis_labels) < 2:
        axis_labels.append("Component 2")
    return coords, axis_labels[:2]


__all__ = ["normalize_dataset", "build_feature_dataset", "project_to_components"]
