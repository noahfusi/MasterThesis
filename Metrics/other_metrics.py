from __future__ import annotations

import csv
import json
from pathlib import Path

from Files.dataset_manager import dataset_path
from Metrics import dataset_summary_path
from Lizard.metrics import extract_duplicate_rate, parse_reference_metrics, parse_summary_metrics
from Lizard.run_analysis import LIZARD_FOLDER
from Tree_Sitter.metrics import compute_max_nesting_depth

OTHER_METRICS_FILENAME = "other_metrics.csv"
REFERENCE_METRICS_FILENAME = "reference_metrics.json"
DERIVED_COLUMNS = {"NCSS/Functions", "Duplication (%)", "Max nesting depth"}


def other_metrics_path(dataset: str) -> Path:
    return dataset_path(dataset) / OTHER_METRICS_FILENAME


def reference_metrics_path(dataset: str) -> Path:
    return dataset_path(dataset) / REFERENCE_METRICS_FILENAME


def extract_file_metrics(dataset: str, summary_text: str) -> tuple[list[dict[str, object]], list[dict[str, object]]]:
    metric_defs, files = parse_summary_metrics(summary_text)
    merge_other_metrics(dataset, metric_defs, files)
    return metric_defs, files


def _normalize_path(value: str | None) -> str | None:
    if not value:
        return None
    normalized = value.replace("\\", "/")
    marker = "/raw/"
    idx = normalized.lower().rfind(marker)
    if idx != -1:
        return normalized[idx + len(marker) :]
    return normalized


def merge_other_metrics(dataset: str, metric_definitions: list[dict[str, object]], files: list[dict[str, object]]) -> None:
    path = other_metrics_path(dataset)

    def read_rows():
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

    fieldnames, data = read_rows()
    if not fieldnames or not data or not DERIVED_COLUMNS.issubset(set(fieldnames)):
        generate_other_metrics(dataset)
        fieldnames, data = read_rows()
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
                averages[name].append(float(value))
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


def generate_other_metrics(dataset: str) -> str:
    summary_path = dataset_summary_path(dataset)
    path = other_metrics_path(dataset)
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
        metrics = entry.get("metrics") or {}
        row: dict[str, object] = {"path": file_path}
        ncss_value = metrics.get("NCSS")
        functions_value = metrics.get("Functions")
        if isinstance(ncss_value, (int, float)) and isinstance(functions_value, (int, float)) and functions_value:
            row["NCSS/Functions"] = ncss_value / functions_value
            metric_names.add("NCSS/Functions")

        lizard_file = dataset_path(dataset) / LIZARD_FOLDER / Path(file_path)
        suffix = lizard_file.suffix
        lizard_file = lizard_file.with_suffix((suffix or "") + ".lizard.xml")
        if lizard_file.exists():
            try:
                lizard_text = lizard_file.read_text(encoding="utf-8", errors="ignore")
            except OSError:
                lizard_text = None
            if lizard_text:
                duplicate_rate = extract_duplicate_rate(lizard_text)
                if duplicate_rate is not None:
                    row["Duplication (%)"] = duplicate_rate
                    metric_names.add("Duplication (%)")

        source_file = raw_root / Path(file_path)
        if source_file.exists():
            try:
                code = source_file.read_text(encoding="utf-8")
            except OSError:
                code = None
            if code:
                nesting = compute_max_nesting_depth(code)
                if isinstance(nesting, (int, float)):
                    row["Max nesting depth"] = nesting
                    metric_names.add("Max nesting depth")

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
                writer.writerow({field: row.get(field, "") for field in fieldnames})
    except OSError as exc:
        path.unlink(missing_ok=True)
        return f"Other metrics failed: {exc}"

    return "Other metrics generated."


def load_reference_metrics(dataset: str) -> dict[str, object] | None:
    path = reference_metrics_path(dataset)
    if not path.exists():
        return None
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return None


def store_reference_metrics(dataset: str, filename: str, metrics: dict[str, object]) -> None:
    payload = {"filename": filename, "metrics": metrics}
    path = reference_metrics_path(dataset)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload, ensure_ascii=False), encoding="utf-8")


def build_reference_metrics(dataset: str, filename: str, lizard_output: str, code_text: str | None) -> dict[str, object]:
    metrics = parse_reference_metrics(lizard_output)
    if code_text:
        nesting = compute_max_nesting_depth(code_text)
        if isinstance(nesting, (int, float)):
            metrics["Max nesting depth"] = nesting
    store_reference_metrics(dataset, filename, metrics)
    return metrics
