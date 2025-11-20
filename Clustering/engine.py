from __future__ import annotations

from dataclasses import dataclass
import math
import random
from typing import Sequence

try:  # pragma: no cover - optional dependency
    from sklearn.preprocessing import RobustScaler as _RobustScaler  # type: ignore
except Exception:  # pragma: no cover - optional dependency
    _RobustScaler = None

try:  # pragma: no cover - optional dependency
    import hdbscan as _hdbscan  # type: ignore
except Exception:  # pragma: no cover - optional dependency
    _hdbscan = None

METRIC_KEYS: list[str] = [
    "NCSS",
    "CCN",
    "Functions",
    "Duplication (%)",
    "Max nesting depth",
    "NCSS/Functions",
    "CCN/Functions",
]


@dataclass(slots=True)
class NormalizedEntry:
    """Single normalized file entry."""

    path: str
    metrics: dict[str, float]
    vector: list[float]


@dataclass(slots=True)
class NormalizedDataset:
    metric_keys: list[str]
    entries: list[NormalizedEntry]


@dataclass(slots=True)
class ClusterInfo:
    id: int
    size: int
    centroid: list[float]


@dataclass(slots=True)
class ClusteringResult:
    labels: list[int]
    clusters: list[ClusterInfo]
    noise: int = 0


def normalize_dataset(files: Sequence[dict[str, object]], metric_keys: Sequence[str] | None = None) -> NormalizedDataset:
    """Extract and normalize metric vectors for clustering using sklearn's RobustScaler when available."""

    keys = list(metric_keys or METRIC_KEYS)
    if not keys:
        return NormalizedDataset(metric_keys=[], entries=[])

    collected: list[tuple[str, dict[str, float], list[float]]] = []
    columns: list[list[float]] = [[] for _ in keys]

    for index, entry in enumerate(files):
        metrics = entry.get("metrics") or {}
        if not isinstance(metrics, dict):
            continue
        raw_values: list[float] = []
        raw_metrics: dict[str, float] = {}
        valid = True
        for key in keys:
            raw_value = _coerce_numeric(metrics.get(key))
            if raw_value is None:
                valid = False
                break
            raw_values.append(raw_value)
            raw_metrics[key] = raw_value
        if not valid:
            continue
        for column, value in zip(columns, raw_values):
            column.append(value)
        path = (
            entry.get("path")
            or entry.get("raw_path")
            or entry.get("filename")
            or entry.get("file")
            or f"entry-{index}"
        )
        collected.append((str(path).replace("\\", "/"), raw_metrics, raw_values))

    if not collected:
        return NormalizedDataset(metric_keys=keys, entries=[])

    raw_matrix = [raw_values for _, _, raw_values in collected]
    normalized_matrix = _robust_scale_matrix(raw_matrix)

    normalized_entries: list[NormalizedEntry] = []
    for (path, metrics, _), normalized in zip(collected, normalized_matrix):
        normalized_entries.append(NormalizedEntry(path=path, metrics=metrics, vector=list(normalized)))

    return NormalizedDataset(metric_keys=keys, entries=normalized_entries)


def run_kmeans_clustering(dataset: NormalizedDataset, cluster_count: int, max_iterations: int = 100) -> ClusteringResult:
    """Simple k-means implementation tailored for normalized entries."""

    if not dataset.entries:
        return ClusteringResult(labels=[], clusters=[], noise=0)

    points = [entry.vector for entry in dataset.entries]
    k = max(1, min(cluster_count, len(points)))
    rand = random.Random(42)
    initial_indices = rand.sample(range(len(points)), k)
    centroids = [[value for value in points[idx]] for idx in initial_indices]
    labels = [0] * len(points)

    for _ in range(max_iterations):
        updated = False
        for idx, point in enumerate(points):
            distances = [_euclidean_distance(point, centroid) for centroid in centroids]
            closest = distances.index(min(distances))
            if labels[idx] != closest:
                labels[idx] = closest
                updated = True

        new_centroids: list[list[float]] = []
        for centroid_index in range(k):
            members = [points[i] for i, label in enumerate(labels) if label == centroid_index]
            if not members:
                replacement = points[rand.randrange(len(points))]
                new_centroids.append(replacement[:])
                continue
            new_centroids.append(_average_vector(members))

        deltas = [
            _euclidean_distance(old, new)
            for old, new in zip(centroids, new_centroids)
        ]
        centroids = new_centroids
        if not updated or max(deltas) <= 1e-4:
            break

    clusters = _build_cluster_summaries(labels, points)
    return ClusteringResult(labels=labels, clusters=clusters, noise=0)


def run_hdbscan_clustering(
    dataset: NormalizedDataset, min_cluster_size: int = 5, *, min_samples: int | None = None
) -> ClusteringResult:
    """Density-based clustering with optional reliance on the hdbscan package."""

    if not dataset.entries:
        return ClusteringResult(labels=[], clusters=[], noise=0)

    points = [entry.vector for entry in dataset.entries]
    min_cluster_size = max(2, min_cluster_size)
    min_samples = max(1, min_samples or min_cluster_size // 2 or 1)

    labels: list[int] | None = None
    if _hdbscan is not None:
        clusterer = _hdbscan.HDBSCAN(min_cluster_size=min_cluster_size, min_samples=min_samples)
        labels = clusterer.fit_predict(points).tolist()

    if not labels or not any(label >= 0 for label in labels):
        labels = _fallback_hdbscan(points, min_cluster_size)

    clusters = _build_cluster_summaries(labels, points)
    noise = sum(1 for label in labels if label == -1)
    return ClusteringResult(labels=labels, clusters=clusters, noise=noise)


def project_to_components(dataset: NormalizedDataset, components: int = 2) -> tuple[list[tuple[float, float]], list[str]]:
    """Project normalized vectors to a 2D plane using a lightweight PCA implementation."""

    if not dataset.entries or components <= 0:
        return [], []

    vectors = [entry.vector for entry in dataset.entries]
    dims = len(vectors[0]) if vectors else 0
    if dims == 0:
        return [(0.0, 0.0) for _ in vectors], []

    projection_vectors = _pca_components(vectors, components)
    if not projection_vectors:
        projection_vectors = _default_axes(dims, components)

    centered = _center_vectors(vectors)
    coords: list[tuple[float, float]] = []
    for vector in centered:
        x = _dot_product(vector, projection_vectors[0]) if projection_vectors else 0.0
        y = (
            _dot_product(vector, projection_vectors[1])
            if len(projection_vectors) > 1
            else 0.0
        )
        coords.append((x, y))

    axis_labels = [f"Component {index + 1}" for index in range(min(components, len(projection_vectors)))]
    if len(axis_labels) < 2:
        axis_labels.append("Component 2")
    return coords, axis_labels[:2]


# ---- helpers ----------------------------------------------------------------


def _euclidean_distance(a: Sequence[float], b: Sequence[float]) -> float:
    return math.sqrt(sum((x - y) ** 2 for x, y in zip(a, b)))


def _average_vector(points: Sequence[Sequence[float]]) -> list[float]:
    if not points:
        return []
    dims = len(points[0])
    return [
        sum(point[i] for point in points) / len(points)
        for i in range(dims)
    ]


def _build_cluster_summaries(labels: list[int], points: Sequence[Sequence[float]]) -> list[ClusterInfo]:
    buffers: dict[int, list[list[float]]] = {}
    for index, label in enumerate(labels):
        if label < 0:
            continue
        buffers.setdefault(label, []).append(list(points[index]))

    clusters: list[ClusterInfo] = []
    for cluster_id in sorted(buffers):
        members = buffers[cluster_id]
        centroid = _average_vector(members)
        clusters.append(ClusterInfo(id=cluster_id, size=len(members), centroid=centroid))
    return clusters


def _fallback_hdbscan(points: Sequence[Sequence[float]], min_cluster_size: int) -> list[int]:
    """Simple density-based clustering used when the hdbscan package is unavailable or fails."""

    count = len(points)
    if count == 0:
        return []
    if count <= min_cluster_size:
        return [0 for _ in points]

    distances: list[float] = []
    matrix: list[list[float]] = [[0.0] * count for _ in range(count)]
    for i in range(count):
        for j in range(i + 1, count):
            distance = _euclidean_distance(points[i], points[j])
            distances.append(distance)
            matrix[i][j] = matrix[j][i] = distance

    if not distances:
        return [0 for _ in points]

    quantiles = [0.4, 0.6, 0.8, 1.0]
    for quantile in quantiles:
        threshold = _percentile(distances, quantile)
        if threshold <= 0:
            threshold = min(distances) if distances else 0.1
        labels = _label_with_threshold(matrix, threshold, min_cluster_size)
        if any(label >= 0 for label in labels):
            return labels

    return [0 for _ in points]


def _percentile(values: Sequence[float], fraction: float) -> float:
    if not values:
        return 0.0
    ordered = sorted(values)
    return _percentile_sorted(ordered, fraction)


def _percentile_sorted(ordered: Sequence[float], fraction: float) -> float:
    if not ordered:
        return 0.0
    index = int(max(0, min(len(ordered) - 1, fraction * (len(ordered) - 1))))
    return ordered[index]


def _robust_scale_matrix(matrix: Sequence[Sequence[float]]) -> list[list[float]]:
    if not matrix:
        return []
    if _RobustScaler is not None:
        scaler = _RobustScaler()
        return scaler.fit_transform(matrix).tolist()
    # fallback implementation using medians and IQR
    dims = len(matrix[0])
    columns = [[row[i] for row in matrix] for i in range(dims)]
    scalers: list[tuple[float, float]] = []
    for values in columns:
        ordered = sorted(values)
        median = _percentile_sorted(ordered, 0.5)
        q1 = _percentile_sorted(ordered, 0.25)
        q3 = _percentile_sorted(ordered, 0.75)
        scalers.append((median, q3 - q1))
    normalized: list[list[float]] = []
    for row in matrix:
        vector: list[float] = []
        for value, (median, iqr) in zip(row, scalers):
            scale = iqr if iqr > 1e-12 else None
            vector.append((value - median) / scale if scale else 0.0)
        normalized.append(vector)
    return normalized


def _label_with_threshold(matrix: Sequence[Sequence[float]], threshold: float, min_cluster_size: int) -> list[int]:
    count = len(matrix)
    adjacency: list[list[int]] = [[] for _ in range(count)]
    for i in range(count):
        for j in range(i + 1, count):
            if matrix[i][j] <= threshold:
                adjacency[i].append(j)
                adjacency[j].append(i)

    labels = [-1] * count
    cluster_id = 0
    for node in range(count):
        if labels[node] != -1:
            continue
        queue = [node]
        component: list[int] = []
        while queue:
            current = queue.pop()
            if labels[current] != -1:
                continue
            labels[current] = cluster_id
            component.append(current)
            for neighbor in adjacency[current]:
                if labels[neighbor] == -1:
                    queue.append(neighbor)
        if len(component) < min_cluster_size:
            for idx in component:
                labels[idx] = -1
        else:
            cluster_id += 1
    return labels


def _center_vectors(vectors: Sequence[Sequence[float]]) -> list[list[float]]:
    if not vectors:
        return []
    dims = len(vectors[0])
    means = [0.0] * dims
    for vector in vectors:
        for idx, value in enumerate(vector):
            means[idx] += value
    means = [value / len(vectors) for value in means]
    centered = []
    for vector in vectors:
        centered.append([value - means[i] for i, value in enumerate(vector)])
    return centered


def _pca_components(vectors: Sequence[Sequence[float]], components: int) -> list[list[float]]:
    centered = _center_vectors(vectors)
    if not centered:
        return []
    dims = len(centered[0])
    covariance = [[0.0] * dims for _ in range(dims)]
    for vector in centered:
        for i in range(dims):
            for j in range(dims):
                covariance[i][j] += vector[i] * vector[j]
    normalization = max(1, len(centered) - 1)
    for i in range(dims):
        for j in range(dims):
            covariance[i][j] /= normalization

    eig_vectors: list[list[float]] = []
    working = [row[:] for row in covariance]
    rand = random.Random(1234)

    for _ in range(min(components, dims)):
        last = _random_unit_vector(dims, rand)
        for _ in range(100):
            next_vec = _matvec(working, last)
            norm = math.sqrt(sum(value * value for value in next_vec))
            if norm <= 1e-9:
                break
            next_vec = [value / norm for value in next_vec]
            if _euclidean_distance(next_vec, last) <= 1e-6:
                last = next_vec
                break
            last = next_vec
        eig_vectors.append(last)
        eigenvalue = _dot_product(last, _matvec(working, last))
        for i in range(dims):
            for j in range(dims):
                working[i][j] -= eigenvalue * last[i] * last[j]
    return eig_vectors


def _random_unit_vector(dims: int, rand: random.Random) -> list[float]:
    vector = [rand.random() for _ in range(dims)]
    norm = math.sqrt(sum(value * value for value in vector))
    if norm <= 1e-9:
        return [1.0 if i == 0 else 0.0 for i in range(dims)]
    return [value / norm for value in vector]


def _matvec(matrix: Sequence[Sequence[float]], vector: Sequence[float]) -> list[float]:
    return [
        sum(matrix[row][col] * vector[col] for col in range(len(vector)))
        for row in range(len(matrix))
    ]


def _dot_product(a: Sequence[float], b: Sequence[float]) -> float:
    return sum(x * y for x, y in zip(a, b))


def _default_axes(dims: int, components: int) -> list[list[float]]:
    axes: list[list[float]] = []
    for index in range(min(components, dims)):
        axis = [0.0] * dims
        axis[index] = 1.0
        axes.append(axis)
    return axes


def _coerce_numeric(value: object) -> float | None:
    if isinstance(value, (int, float)):
        if math.isnan(value) or math.isinf(value):
            return None
        return float(value)
    if isinstance(value, str):
        stripped = value.strip()
        if not stripped:
            return None
        try:
            parsed = float(stripped)
        except ValueError:
            return None
        if math.isnan(parsed) or math.isinf(parsed):
            return None
        return parsed
    return None
