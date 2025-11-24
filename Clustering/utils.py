from __future__ import annotations

import math
from typing import Sequence, Tuple, Dict

try:  # pragma: no cover - required dependency
    from sklearn.preprocessing import RobustScaler as _RobustScaler  # type: ignore
    from sklearn.decomposition import PCA as _sklearn_PCA  # type: ignore
    from sklearn.metrics import silhouette_score as _sklearn_silhouette_score  # type: ignore
except Exception as exc:  # pragma: no cover
    _RobustScaler = None
    _sklearn_PCA = None
    _sklearn_silhouette_score = None
    _IMPORT_ERROR = exc
else:
    _IMPORT_ERROR = None

try:  # pragma: no cover - optional dependency
    from cuml.decomposition import PCA as _cuml_PCA  # type: ignore
except Exception:  # pragma: no cover
    _cuml_PCA = None


def coerce_numeric(value: object) -> float | None:
    """
    @brief Convert a value to float when finite, otherwise return None.
    @param value Candidate numeric value (int, float, or string).
    @return Finite float or None if conversion fails.
    """
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


def euclidean_distance(a: Sequence[float], b: Sequence[float]) -> float:
    """
    @brief Compute Euclidean distance between two vectors.
    @param a First vector.
    @param b Second vector.
    @return Euclidean distance.
    """
    return math.sqrt(sum((x - y) ** 2 for x, y in zip(a, b)))


def average_vector(points: Sequence[Sequence[float]]) -> list[float]:
    """
    @brief Compute the centroid of a collection of vectors.
    @param points Sequence of vectors.
    @return Averaged vector or empty list when no points provided.
    """
    if not points:
        return []
    return [sum(values) / len(points) for values in zip(*points)]


def percentile_sorted(ordered: Sequence[float], fraction: float) -> float:
    """
    @brief Compute an approximate percentile for a pre-sorted list.
    @param ordered Sorted numeric values.
    @param fraction Percentile fraction between 0 and 1.
    @return Value at the requested percentile.
    """
    if not ordered:
        return 0.0
    index = int(max(0, min(len(ordered) - 1, fraction * (len(ordered) - 1))))
    return ordered[index]


def percentile(values: Sequence[float], fraction: float) -> float:
    """
    @brief Compute an approximate percentile for an unsorted list.
    @param values Collection of numeric values.
    @param fraction Percentile fraction between 0 and 1.
    @return Value at the requested percentile.
    """
    if not values:
        return 0.0
    ordered = sorted(values)
    return percentile_sorted(ordered, fraction)


def robust_scale_matrix(matrix: Sequence[Sequence[float]]) -> list[list[float]]:
    """
    @brief Normalize a matrix using RobustScaler or a median/IQR fallback.
    @param matrix Matrix of raw metric values.
    @return Scaled matrix with robust normalization applied.
    """
    if not matrix:
        return []
    if _RobustScaler is None:
        raise RuntimeError("scikit-learn is required for robust scaling.") from _IMPORT_ERROR
    scaler = _RobustScaler()
    return scaler.fit_transform(matrix).tolist()


def collect_metric_vectors(
    files: Sequence[dict[str, object]], metric_keys: Sequence[str]
) -> Tuple[Dict[str, list[float]], Dict[str, Dict[str, float]]]:
    """
    @brief Extract metric vectors and their rendered mapping per path.
    @param files Raw metric entries.
    @param metric_keys Metric keys to retain.
    @return Tuple of path->vector and path->metrics dict.
    """
    vectors: Dict[str, list[float]] = {}
    payloads: Dict[str, Dict[str, float]] = {}
    for index, entry in enumerate(files):
        metrics = entry.get("metrics")
        if not isinstance(metrics, dict):
            continue
        values: list[float] = []
        rendered: Dict[str, float] = {}
        valid = True
        for key in metric_keys:
            number = coerce_numeric(metrics.get(key))
            if number is None:
                valid = False
                break
            values.append(number)
            rendered[str(key)] = number
        if not valid:
            continue
        path = (
            entry.get("path")
            or entry.get("raw_path")
            or entry.get("filename")
            or entry.get("file")
            or f"entry-{index}"
        )
        norm_path = str(path).replace("\\", "/")
        vectors[norm_path] = values
        payloads[norm_path] = rendered
    return vectors, payloads


def project_embeddings(embeddings: dict[str, Sequence[float]], components: int) -> dict[str, list[float]]:
    """
    @brief Project embeddings to a lower dimension using PCA and robust scaling.
    @param embeddings Mapping of path to embedding vector.
    @param components Target number of PCA components.
    @return Mapping of path to projected embedding vector.
    """
    if not embeddings:
        return {}
    paths = list(embeddings.keys())
    vectors = [list(map(float, embeddings[path])) for path in paths]
    scaled = robust_scale_matrix(vectors)
    components = max(1, min(components, len(scaled[0]) if scaled else components))
    pca_basis = pca_components(scaled, components)
    if not pca_basis:
        pca_basis = default_axes(len(scaled[0]) if scaled else components, components)
    centered = center_vectors(scaled)
    coords: list[list[float]] = []
    for vector in centered:
        coords.append([dot_product(vector, basis) for basis in pca_basis[:components]])
    return {path: coord for path, coord in zip(paths, coords)}


def label_with_threshold(matrix: Sequence[Sequence[float]], threshold: float, min_cluster_size: int) -> list[int]:
    """
    @brief Label connected components using a distance threshold.
    @param matrix Pairwise distance matrix.
    @param threshold Maximum distance to consider points connected.
    @param min_cluster_size Minimum number of points required for a cluster.
    @return Labels list where -1 represents noise.
    """
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


def center_vectors(vectors: Sequence[Sequence[float]]) -> list[list[float]]:
    """
    @brief Center vectors by subtracting component-wise means.
    @param vectors Collection of vectors to center.
    @return Centered vectors with zero mean per dimension.
    """
    if not vectors:
        return []
    means = [sum(values) / len(vectors) for values in zip(*vectors)]
    return [[value - mean for value, mean in zip(vector, means)] for vector in vectors]


def pca_components(vectors: Sequence[Sequence[float]], components: int) -> list[list[float]]:
    """
    @brief Compute principal component directions using cuML, sklearn, or a fallback power iteration.
    @param vectors Centered vectors to decompose.
    @param components Number of components to extract.
    @return List of component vectors.
    """
    if not vectors:
        return []
    dims = len(vectors[0]) if vectors else 0
    target_components = max(1, min(components, dims))

    if _cuml_PCA is not None:
        try:
            model = _cuml_PCA(n_components=target_components)
            comps = model.fit(vectors).components_
            return comps.tolist()
        except Exception:
            pass

    if _sklearn_PCA is None:
        raise RuntimeError("scikit-learn is required for PCA.") from _IMPORT_ERROR

    model = _sklearn_PCA(n_components=target_components)
    comps = model.fit(vectors).components_
    return comps.tolist()


def dot_product(a: Sequence[float], b: Sequence[float]) -> float:
    """
    @brief Compute dot product of two vectors.
    @param a First vector.
    @param b Second vector.
    @return Dot product value.
    """
    return sum(x * y for x, y in zip(a, b))


def default_axes(dims: int, components: int) -> list[list[float]]:
    """
    @brief Provide default unit axes when PCA components are unavailable.
    @param dims Dimensionality of the vectors.
    @param components Number of axes requested.
    @return List of axis vectors.
    """
    return [[1.0 if idx == axis else 0.0 for idx in range(dims)] for axis in range(min(components, dims))]


def silhouette_score(points: Sequence[Sequence[float]], labels: Sequence[int]) -> float | None:
    """
    @brief Compute silhouette score using sklearn when available, ignoring noise labels (-1).
    @param points Normalized vectors.
    @param labels Cluster assignments (noise as -1).
    @return Average silhouette score or None when undefined.
    """
    if not points or not labels or len(points) != len(labels):
        return None
    if _sklearn_silhouette_score is None:
        raise RuntimeError("scikit-learn is required for silhouette score.") from _IMPORT_ERROR
    mask = [idx for idx, label in enumerate(labels) if label is not None and label >= 0]
    if len(mask) < 2:
        return None
    filtered_points = [points[idx] for idx in mask]
    filtered_labels = [labels[idx] for idx in mask]
    if len(set(filtered_labels)) < 2:
        return None
    try:
        return float(_sklearn_silhouette_score(filtered_points, filtered_labels))
    except Exception:
        return None
