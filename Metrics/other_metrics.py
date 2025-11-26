from __future__ import annotations

import csv
import json
import re
from pathlib import Path
import xml.etree.ElementTree as ET

import config
from Files.dataset_manager import dataset_path
from Metrics import dataset_summary_path
from Lizard.metrics import extract_duplicate_rate, parse_reference_metrics, parse_summary_metrics, sanitize_summary_xml
try:
    from Tree_Sitter.metrics import compute_max_nesting_depth
except Exception:  # pragma: no cover - optional dependency
    def compute_max_nesting_depth(code: str) -> int | None:
        return None

METRICS_FILENAME = config.METRICS_FILENAME
REFERENCE_METRICS_FILENAME = config.REFERENCE_METRICS_FILENAME
REQUIRED_COLUMNS = set(config.CLUSTERING_METRIC_KEYS)
_COMMENT_PATTERN = re.compile(r"//.*?$|/\*.*?\*/", re.DOTALL | re.MULTILINE)
_STRING_PATTERN = re.compile(r'"""(?:.|\n)*?"""|"(?:\\.|[^"\\])*"|\'(?:\\.|[^\'\\])*\'', re.DOTALL)


def metrics_csv_path(dataset: str) -> Path:
    """
    @brief Return the CSV path storing derived metrics for a dataset.
    @param dataset Dataset name.
    @return Filesystem path to the metrics CSV.
    """
    return dataset_path(dataset) / METRICS_FILENAME


def reference_metrics_path(dataset: str) -> Path:
    """
    @brief Return the JSON path storing reference metrics for a dataset.
    @param dataset Dataset name.
    @return Filesystem path to the reference metrics JSON.
    """
    return dataset_path(dataset) / REFERENCE_METRICS_FILENAME


def _load_metrics_csv(dataset: str) -> tuple[list[str], list[dict[str, object]]]:
    """
    @brief Load the metrics CSV (generating it on demand when missing/invalid).
    @param dataset Dataset name.
    @return Tuple of fieldnames and row dicts.
    """
    path = metrics_csv_path(dataset)

    def _read_rows() -> tuple[list[str], list[dict[str, object]]]:
        try:
            with path.open("r", encoding="utf-8", newline="") as handle:
                reader = csv.DictReader(handle)
                header = [name for name in (reader.fieldnames or []) if name and name != "path"]
                rows = list(reader)
                return header, rows
        except OSError:
            return [], []

    if not path.exists():
        generate_other_metrics(dataset)

    fieldnames, rows = _read_rows()
    required = REQUIRED_COLUMNS
    if not fieldnames or not rows or (required and not required.issubset(set(fieldnames))):
        generate_other_metrics(dataset)
        fieldnames, rows = _read_rows()
        if not fieldnames or not rows:
            return [], []

    return fieldnames, rows


def extract_file_metrics(dataset: str, summary_text: str) -> tuple[list[dict[str, object]], list[dict[str, object]]]:
    """
    @brief Merge base Lizard summary metrics with locally computed ones.
    @param dataset Dataset name.
    @param summary_text Raw Lizard summary XML content.
    @return Tuple of metric definitions and enriched per-file metrics.
    """
    metric_defs, files = parse_summary_metrics(summary_text)
    merge_other_metrics(dataset, metric_defs, files)
    return metric_defs, files


def _normalize_path(value: str | None) -> str | None:
    """
    @brief Normalize a reported path to a raw-relative path when possible.
    @param value Raw path string from reports/CSV.
    @return Normalized path or None.
    """
    if not value:
        return None
    normalized = value.replace("\\", "/")
    marker = "/raw/"
    idx = normalized.lower().rfind(marker)
    if idx != -1:
        return normalized[idx + len(marker) :]
    return normalized


def _strip_comments_and_strings(code: str) -> str:
    """
    @brief Remove comments and string literals for lightweight keyword counting.
    @param code Raw source code.
    @return Code without strings/comments to avoid noisy keyword matches.
    """
    without_strings = _STRING_PATTERN.sub(" ", code or "")
    return _COMMENT_PATTERN.sub(" ", without_strings)


def _count_keyword_occurrences(code: str) -> dict[str, int]:
    """
    @brief Count branching/loop/variable keywords in Scala-like code.
    @param code Raw source code.
    @return Mapping of keyword category to counts.
    """
    if not code:
        return {"if": 0, "loops": 0, "vars": 0}
    cleaned = _strip_comments_and_strings(code)
    return {
        "if": len(re.findall(r"\bif\b", cleaned, flags=re.IGNORECASE)),
        "loops": len(re.findall(r"\b(for|while|do)\b", cleaned, flags=re.IGNORECASE)),
        "vars": len(re.findall(r"\b(val|var)\b", cleaned)),
    }


def _safe_ratio(numerator: float, denominator: float | None) -> float:
    """Return a safe ratio, falling back to 0 when the denominator is invalid."""
    try:
        denom = float(denominator) if denominator is not None else 0.0
    except (TypeError, ValueError):
        denom = 0.0
    return float(numerator) / denom if denom else 0.0


def _round_float(value: float, digits: int = 3) -> float:
    """Round a float to the desired precision."""
    try:
        return round(float(value), digits)
    except (TypeError, ValueError):
        return value


def _compute_keyword_metrics(code: str | None, raw_ncss: float | None, functions_count: float | None) -> dict[str, float]:
    """
    @brief Compute lightweight metrics derived from keyword counts.
    @param code Source code (may be None).
    @param raw_ncss NCSS value to normalize per-line metrics.
    @param functions_count Number of functions to normalize variable density.
    @return Dictionary of keyword-derived metrics.
    """
    metrics = {
        "If/NCSS": 0.0,
        "Loops/NCSS": 0.0,
        "Total variables": 0.0,
        "Vars/Functions": 0.0,
        "Vars/NCSS": 0.0,
    }
    if not code:
        return metrics
    counts = _count_keyword_occurrences(code)
    variables_count = counts.get("vars", 0)
    metrics["Total variables"] = float(variables_count)
    metrics["If/NCSS"] = _round_float(_safe_ratio(counts.get("if", 0), raw_ncss))
    metrics["Loops/NCSS"] = _round_float(_safe_ratio(counts.get("loops", 0), raw_ncss))
    metrics["Vars/NCSS"] = _round_float(_safe_ratio(variables_count, raw_ncss))
    metrics["Vars/Functions"] = _round_float(_safe_ratio(variables_count, functions_count))
    return metrics


def merge_other_metrics(dataset: str, metric_definitions: list[dict[str, object]], files: list[dict[str, object]]) -> None:
    """
    @brief Enrich summary metrics with derived metrics from CSV (nesting, duplication, ratios).
    @param dataset Dataset name.
    @param metric_definitions Base metric definitions list to extend.
    @param files File metric entries to mutate with additional metrics.
    """
    fieldnames, data = _load_metrics_csv(dataset)
    if not fieldnames or not data:
        return

    averages: dict[str, list[float]] = {name: [] for name in fieldnames}
    per_file: dict[str, dict[str, str]] = {}
    for row in data:
        file_path = _normalize_path(row.get("path"))
        if not file_path:
            continue
        per_file[file_path] = row
        for name in fieldnames:
            value = row.get(name)
            if value in (None, ""):
                continue
            try:
                averages[name].append(_round_float(float(value)))
            except ValueError:
                continue

    existing_keys = {definition.get("key") for definition in metric_definitions}
    for name in fieldnames:
        if name in existing_keys:
            continue
        definition: dict[str, object] = {"key": name, "label": name}
        values = averages.get(name) or []
        if values:
            definition["average"] = sum(values) / len(values)
        metric_definitions.append(definition)

    for entry in files:
        original_path = entry.get("path") or entry.get("raw_path")
        path_key = _normalize_path(original_path)
        if not path_key:
            continue
        row = per_file.get(path_key)
        if not row:
            continue
        metrics = entry.setdefault("metrics", {})
        for name in fieldnames:
            value = row.get(name)
            if value in (None, ""):
                continue
            try:
                metrics[name] = float(value)
            except ValueError:
                metrics[name] = value


def load_metrics_entries(dataset: str) -> list[dict[str, object]]:
    """
    @brief Return clustering-ready entries sourced from the dataset metrics CSV.
    @param dataset Dataset name.
    @return List of entries with normalized paths and metrics.
    """

    fieldnames, rows = _load_metrics_csv(dataset)
    if not fieldnames or not rows:
        return []

    entries: list[dict[str, object]] = []
    for row in rows:
        original_path = row.get("path")
        if not isinstance(original_path, str) or not original_path:
            continue
        normalized_path = _normalize_path(original_path) or original_path
        metrics: dict[str, object] = {}
        for name in fieldnames:
            value = row.get(name)
            if value in (None, ""):
                continue
            try:
                number = float(value)
                metrics[name] = _round_float(number)
            except (TypeError, ValueError):
                metrics[name] = value
        if metrics:
            entries.append({"path": normalized_path, "raw_path": original_path, "metrics": metrics})
        else:
            entries.append({"path": normalized_path, "raw_path": original_path, "metrics": {}})

    return entries


def generate_other_metrics(dataset: str) -> str:
    """
    @brief Generate derived metrics CSV for a dataset using Lizard outputs and source code.
    @param dataset Dataset name.
    @return Status text describing generation outcome.
    """
    summary_path = dataset_summary_path(dataset)
    path = metrics_csv_path(dataset)
    if not summary_path.exists():
        path.unlink(missing_ok=True)
        return "Other metrics skipped: summary missing."
    try:
        summary_text = summary_path.read_text(encoding="utf-8")
    except OSError as exc:
        path.unlink(missing_ok=True)
        return f"Other metrics skipped: {exc}"

    try:
        _, files = parse_summary_metrics(summary_text)
    except ValueError as exc:
        path.unlink(missing_ok=True)
        return f"Other metrics skipped: {exc}"

    if not files:
        path.unlink(missing_ok=True)
        return "Other metrics skipped: no file entries."

    rows: list[dict[str, object]] = []
    metric_names: set[str] = set()
    raw_root = dataset_path(dataset) / "raw"

    for entry in files:
        file_path = entry.get("path") or entry.get("raw_path")
        if not isinstance(file_path, str) or not file_path:
            continue
        row: dict[str, object] = {"path": file_path}
        base_metrics = entry.get("metrics") or {}
        functions_count = None
        raw_ccn = None
        raw_ncss = None
        for key, value in base_metrics.items():
            if value in (None, ""):
                continue
            column = key if isinstance(key, str) else str(key)
            if isinstance(value, (int, float)):
                numeric_value = _round_float(float(value))
                row[column] = numeric_value
                if column.lower() == "functions":
                    functions_count = numeric_value
                elif column.lower() == "ccn":
                    raw_ccn = numeric_value
                elif column.lower() == "ncss":
                    raw_ncss = numeric_value
            else:
                row[column] = value
            metric_names.add(column)

        if functions_count == 0:
            functions_count = 1.0
            row["Functions"] = functions_count
        if functions_count is not None:
            if raw_ccn is not None:
                row["CCN/Functions"] = _round_float(raw_ccn / functions_count if functions_count else raw_ccn)
                metric_names.add("CCN/Functions")
            if raw_ncss is not None:
                row["NCSS/Functions"] = _round_float(raw_ncss / functions_count if functions_count else raw_ncss)
                metric_names.add("NCSS/Functions")

        default_keyword_metrics = {
            "Max nesting depth": 0.0,
            "If/NCSS": 0.0,
            "Loops/NCSS": 0.0,
            "Total variables": 0.0,
            "Vars/Functions": 0.0,
            "Vars/NCSS": 0.0,
        }
        for key, value in default_keyword_metrics.items():
            row.setdefault(key, value)
            metric_names.add(key)

        lizard_file = dataset_path(dataset) / config.LIZARD_FOLDER_NAME / Path(file_path)
        suffix = lizard_file.suffix
        lizard_file = lizard_file.with_suffix((suffix or "") + ".lizard.xml")
        if lizard_file.exists():
            try:
                lizard_text = lizard_file.read_text(encoding="utf-8", errors="ignore")
            except OSError:
                lizard_text = None
            if lizard_text:
                base_metrics = _parse_lizard_file_metrics(lizard_text)
                for key, value in base_metrics.items():
                    row[key] = _round_float(value)
                    metric_names.add(key)
                duplicate_rate = extract_duplicate_rate(lizard_text)
                if duplicate_rate is not None:
                    row["Duplication (%)"] = _round_float(duplicate_rate)
                    metric_names.add("Duplication (%)")
        row.setdefault("Duplication (%)", row.get("Duplication (%)", 0.0))
        metric_names.add("Duplication (%)")

        source_file = raw_root / Path(file_path)
        if source_file.exists():
            try:
                code = source_file.read_text(encoding="utf-8")
            except OSError:
                code = None
            keyword_metrics = _compute_keyword_metrics(code, raw_ncss, functions_count)
            row.update(keyword_metrics)
            if code:
                nesting = compute_max_nesting_depth(code)
                if isinstance(nesting, (int, float)):
                    row["Max nesting depth"] = _round_float(nesting)

        rows.append(row)

    if not metric_names:
        path.unlink(missing_ok=True)
        return "Other metrics skipped: no derived metrics."

    fieldnames = ["path"] + sorted(metric_names)
    try:
        path.parent.mkdir(parents=True, exist_ok=True)
        with path.open("w", encoding="utf-8", newline="") as handle:
            writer = csv.DictWriter(handle, fieldnames=fieldnames)
            writer.writeheader()
            for row in rows:
                sanitized: dict[str, object] = {}
                for field in fieldnames:
                    value = row.get(field, "")
                    if isinstance(value, float):
                        value = _round_float(value)
                    sanitized[field] = value
                writer.writerow(sanitized)
    except OSError as exc:
        path.unlink(missing_ok=True)
        return f"Other metrics failed: {exc}"

    return "Other metrics generated."


def load_reference_metrics(dataset: str) -> dict[str, object] | None:
    """
    @brief Load stored reference metrics for a dataset.
    @param dataset Dataset name.
    @return Metrics payload or None when unavailable/invalid.
    """
    path = reference_metrics_path(dataset)
    if not path.exists():
        return None
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return None


def store_reference_metrics(dataset: str, filename: str, metrics: dict[str, object]) -> None:
    """
    @brief Persist reference metrics to disk.
    @param dataset Dataset name.
    @param filename Reference filename analyzed.
    @param metrics Metrics payload to store.
    """
    payload = {"filename": filename, "metrics": metrics}
    path = reference_metrics_path(dataset)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload, ensure_ascii=False), encoding="utf-8")


def build_reference_metrics(dataset: str, filename: str, lizard_output: str, code_text: str | None) -> dict[str, object]:
    """
    @brief Build and store reference metrics combining Lizard output and nesting depth.
    @param dataset Dataset name.
    @param filename Reference filename analyzed.
    @param lizard_output Raw Lizard XML output.
    @param code_text Optional source code content for nesting computation.
    @return Metrics payload stored to disk.
    """
    metrics = parse_reference_metrics(lizard_output)
    if code_text:
        nesting = compute_max_nesting_depth(code_text)
        if isinstance(nesting, (int, float)):
            metrics["Max nesting depth"] = nesting
    store_reference_metrics(dataset, filename, metrics)
    return metrics


def _safe_float(value: str | None) -> float | None:
    """
    @brief Safely parse a string to float.
    @param value Raw string value.
    @return Parsed float or None when invalid.
    """
    if value is None:
        return None
    text = value.strip()
    if not text:
        return None
    try:
        number = float(text)
    except ValueError:
        return None
    return number


def _parse_lizard_file_metrics(report: str) -> dict[str, float]:
    """
    @brief Parse file-level metrics from a single Lizard XML report.
    @param report Raw XML text.
    @return Mapping of metric labels to numeric values.
    """
    report = sanitize_summary_xml(report)
    try:
        root = ET.fromstring(report)
    except ET.ParseError:
        return {}

    metrics: dict[str, float] = {}

    measure = root.find(".//measure[@type='File']")
    if measure is not None:
        labels: list[ET.Element] = []
        labels_element = measure.find("labels")
        if labels_element is not None:
            labels = labels_element.findall("label")
        items = measure.findall("item")
        if items:
            first_item = items[0]
            values = first_item.findall("value")
            for label_node, value_node in zip(labels, values):
                label = (label_node.text or "").strip()
                if not label or label.lower().startswith("nr"):
                    continue
                number = _safe_float(value_node.text if value_node is not None else None)
                if number is not None:
                    metrics[label] = number

    averages = _extract_function_averages(root)
    if averages:
        ncss_avg = averages.get("NCSS")
        if ncss_avg is not None:
            metrics["NCSS/Functions"] = ncss_avg
        ccn_avg = averages.get("CCN")
        if ccn_avg is not None:
            metrics["CCN/Functions"] = ccn_avg

    return metrics


def _extract_function_averages(root: ET.Element) -> dict[str, float]:
    """
    @brief Extract function-level average metrics from a Lizard XML root.
    @param root Parsed XML root element.
    @return Mapping of averages keyed by label.
    """
    measure = root.find(".//measure[@type='Function']")
    if measure is None:
        return {}
    values: dict[str, float] = {}
    for average in measure.findall("average"):
        label = (average.attrib.get("label") or "").strip()
        value_attr = average.attrib.get("value")
        if not label or value_attr is None:
            continue
        number = _safe_float(value_attr)
        if number is not None:
            values[label] = number
    return values
