from __future__ import annotations

import json
import math
from pathlib import Path
from typing import Iterable

from Files.dataset_manager import dataset_path
from Metrics.other_metrics import extract_file_metrics
from Metrics.utils import dataset_summary_path

STUDENTS_OUTLIERS_FILENAME = "students_outliers.json"


def students_outliers_path(dataset: str) -> Path:
    return dataset_path(dataset) / STUDENTS_OUTLIERS_FILENAME


def _coerce_number(value: object) -> float | None:
    try:
        number = float(value)  # type: ignore[arg-type]
    except (TypeError, ValueError):
        return None
    if not math.isfinite(number):
        return None
    return number


def _percentile(sorted_values: list[float], percentile: float) -> float | None:
    if not sorted_values:
        return None
    pos = (len(sorted_values) - 1) * percentile
    base = int(math.floor(pos))
    rest = pos - base
    if base + 1 < len(sorted_values):
        return sorted_values[base] + rest * (sorted_values[base + 1] - sorted_values[base])
    return sorted_values[base]


def _compute_quartiles(values: Iterable[float]) -> tuple[float | None, float | None, float | None]:
    filtered = []
    for value in values:
        number = _coerce_number(value)
        if number is not None:
            filtered.append(number)
    if not filtered:
        return None, None, None
    filtered.sort()
    q1 = _percentile(filtered, 0.25)
    median = _percentile(filtered, 0.5)
    q3 = _percentile(filtered, 0.75)
    return q1, median, q3


def _normalize_filename(file_entry: dict[str, object]) -> str:
    path = file_entry.get("path") or file_entry.get("raw_path") or file_entry.get("filename")
    if isinstance(path, str) and path.strip():
        return path.replace("\\", "/")
    return "Fichier"


def _flag_file_thresholds(
    file_flags: dict[str, dict[str, object]],
    filename: str,
    metric_key: str,
    value: float,
    *,
    high: bool,
    extra_high: bool,
    below_q1: bool,
    below_fence: bool,
) -> None:
    record = file_flags.setdefault(filename, {"filename": filename, "metrics": {}, "flags": {}})
    metric_record = record["metrics"].setdefault(
        metric_key, {"value": value, "high": False, "extra_high": False, "below_q1": False, "below_fence": False}
    )
    metric_record["value"] = value
    metric_record["high"] = bool(metric_record.get("high")) or high or extra_high
    metric_record["extra_high"] = bool(metric_record.get("extra_high")) or extra_high
    metric_record["below_q1"] = bool(metric_record.get("below_q1")) or below_q1 or below_fence
    metric_record["below_fence"] = bool(metric_record.get("below_fence")) or below_fence

    flags = record.setdefault("flags", {})
    flags[f"{metric_key} high"] = bool(flags.get(f"{metric_key} high")) or high or extra_high
    flags[f"{metric_key} extra high"] = bool(flags.get(f"{metric_key} extra high")) or extra_high


def _build_metric_bucket(
    metric: dict[str, object], files: list[dict[str, object]], file_flags: dict[str, dict[str, object]]
) -> dict[str, object] | None:
    key = metric.get("key")
    if not key or not isinstance(key, str):
        return None

    label = metric.get("label") if isinstance(metric.get("label"), str) else key
    per_file_values: list[dict[str, object]] = []
    for file in files:
        metrics = file.get("metrics") or {}
        value = _coerce_number(metrics.get(key))
        if value is None:
            continue
        per_file_values.append({"filename": _normalize_filename(file), "value": value})
    if not per_file_values:
        return None

    values_only = [entry["value"] for entry in per_file_values]
    q1, median, q3 = _compute_quartiles(values_only)
    if q1 is None or q3 is None:
        return None

    iqr = q3 - q1
    upper_fence = q3 + 1.5 * iqr if iqr >= 0 else None
    lower_fence = q1 - 1.5 * iqr if iqr >= 0 else None

    groups = {"aboveFence": [], "aboveQ3": [], "belowQ1": [], "belowFence": []}
    comments = {
        group_key: _describe_threshold(key, group_key)
        for group_key in ("aboveFence", "aboveQ3", "belowQ1", "belowFence")
    }
    for entry in per_file_values:
        value = entry["value"]
        filename = entry["filename"]
        high = value > q3
        extra_high = upper_fence is not None and value > upper_fence
        below_q1 = value < q1
        extra_low = lower_fence is not None and value < lower_fence

        if extra_high:
            groups["aboveFence"].append(entry)
        elif high:
            groups["aboveQ3"].append(entry)
        elif extra_low:
            groups["belowFence"].append(entry)
        elif below_q1:
            groups["belowQ1"].append(entry)

        _flag_file_thresholds(
            file_flags,
            filename,
            key,
            value,
            high=high,
            extra_high=extra_high,
            below_q1=below_q1,
            below_fence=extra_low,
        )

    if not any(groups.values()):
        return None

    groups["aboveFence"].sort(key=lambda entry: entry["value"], reverse=True)
    groups["aboveQ3"].sort(key=lambda entry: entry["value"], reverse=True)
    groups["belowQ1"].sort(key=lambda entry: entry["value"])
    groups["belowFence"].sort(key=lambda entry: entry["value"])

    return {
        "key": key,
        "label": label,
        "q1": q1,
        "q3": q3,
        "median": median,
        "iqr": iqr,
        "lowerFence": lower_fence,
        "upperFence": upper_fence,
        "groups": groups,
        "comments": comments,
    }


def build_students_outliers(dataset: str) -> dict[str, object]:
    summary_path = dataset_summary_path(dataset)
    if not summary_path.exists():
        raise RuntimeError("Lizard analysis not found.")
    try:
        summary_text = summary_path.read_text(encoding="utf-8")
    except OSError as exc:
        raise RuntimeError(f"Unable to read summary: {exc}") from exc

    metrics, files = extract_file_metrics(dataset, summary_text)
    if not metrics or not files:
        raise RuntimeError("No metrics available for students analysis.")

    file_flags: dict[str, dict[str, object]] = {}
    buckets: list[dict[str, object]] = []
    metric_entries: list[dict[str, object]] = []

    for metric in metrics:
        bucket = _build_metric_bucket(metric, files, file_flags)
        metric_key = metric.get("key")
        metric_label = metric.get("label") or metric_key
        if isinstance(metric_key, str):
            metric_entries.append({"key": metric_key, "label": metric_label})
        if bucket:
            buckets.append(bucket)

    files_payload: list[dict[str, object]] = []
    for filename, record in sorted(file_flags.items(), key=lambda item: item[0]):
        metrics_payload = record.get("metrics") or {}
        flags_payload = {key: bool(value) for key, value in (record.get("flags") or {}).items()}
        file_entry = {
            "filename": filename,
            "metrics": metrics_payload,
            "flags": flags_payload,
        }
        for flag_key, flag_value in flags_payload.items():
            file_entry[flag_key] = flag_value
        files_payload.append(file_entry)

    payload: dict[str, object] = {
        "dataset": dataset,
        "metrics": metric_entries,
        "buckets": buckets,
        "files": files_payload,
    }

    output_path = students_outliers_path(dataset)
    output_path.write_text(json.dumps(payload, ensure_ascii=False, indent=2), encoding="utf-8")
    return payload


def load_students_outliers(dataset: str) -> dict[str, object]:
    path = students_outliers_path(dataset)
    if path.exists():
        try:
            return json.loads(path.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError):
            pass
    return build_students_outliers(dataset)


THRESHOLD_DESCRIPTIONS: dict[str, dict[str, str]] = {
    "generic": {
        "aboveFence": "Valeur très au-dessus du reste du dataset – investiguer en priorité.",
        "aboveQ3": "Valeur au-dessus de la plupart des fichiers – vérifier nécessité de cette différence.",
        "belowFence": "Valeur extrêmement basse – peut signaler un fichier incomplet ou atypique.",
        "belowQ1": "Valeur sous la majorité – confirmer que ce comportement est voulu.",
    },
    "duplication": {
        "aboveFence": "Duplication très élevée – risque fort de copier/coller massif ou fragments générés.",
        "aboveQ3": "Duplication notable – consolider les sections répétées ou mutualiser les helpers.",
        "belowFence": "Duplication faible – bon signe, mais vérifier que le fichier n'est pas trop minimal.",
        "belowQ1": "Duplication faible – bon signe, mais vérifier que le fichier n'est pas trop minimal.",
    },
    "complexity": {
        "aboveFence": "Complexité cyclomatique extrême – prioriser une refonte (fonctions trop ramifiées).",
        "aboveQ3": "Complexité élevée – envisager de scinder ou simplifier les branches conditionnelles.",
        "belowFence": "Complexité très basse – peut indiquer un stub ou un code incomplet.",
        "belowQ1": "Complexité très basse – peut indiquer un stub ou un code incomplet.",
    },
    "size": {
        "aboveFence": "Fichier volumineux – risque de God object; découper en modules plus ciblés.",
        "aboveQ3": "Taille supérieure à la majorité – vérifier si des responsabilités peuvent être extraites.",
        "belowFence": "Fichier très court – s'assurer qu'il ne manque pas de logique attendue.",
        "belowQ1": "Fichier très court – s'assurer qu'il ne manque pas de logique attendue.",
    },
    "functions": {
        "aboveFence": "Beaucoup de fonctions – possible mélange de responsabilités, penser à regrouper par domaine.",
        "aboveQ3": "Nombre de fonctions au-dessus de la norme – audit rapide pour regrouper ce qui peut l'être.",
        "belowFence": "Peu de fonctions – fichier peut être trop limité ou encore un squelette.",
        "belowQ1": "Peu de fonctions – fichier peut être trop limité ou encore un squelette.",
    },
    "nesting": {
        "aboveFence": "Imbrication très profonde – refactoriser en extrayant des fonctions pour réduire la profondeur.",
        "aboveQ3": "Imbrication élevée – simplifier les branches ou utiliser un early return.",
    },
}


def _describe_threshold(metric_key: str, bucket_key: str) -> str:
    key = metric_key.lower()
    is_duplication = "duplication" in key
    is_complexity = "ccn" in key or "complex" in key
    is_size = "ncss" in key or "loc" in key or "lines" in key
    is_functions = "functions" in key
    is_nesting = "nesting" in key

    bucket = bucket_key
    if is_duplication:
        return THRESHOLD_DESCRIPTIONS["duplication"].get(bucket, THRESHOLD_DESCRIPTIONS["generic"].get(bucket, ""))
    if is_complexity:
        return THRESHOLD_DESCRIPTIONS["complexity"].get(bucket, THRESHOLD_DESCRIPTIONS["generic"].get(bucket, ""))
    if is_size:
        return THRESHOLD_DESCRIPTIONS["size"].get(bucket, THRESHOLD_DESCRIPTIONS["generic"].get(bucket, ""))
    if is_functions:
        return THRESHOLD_DESCRIPTIONS["functions"].get(bucket, THRESHOLD_DESCRIPTIONS["generic"].get(bucket, ""))
    if is_nesting:
        return THRESHOLD_DESCRIPTIONS["nesting"].get(bucket, THRESHOLD_DESCRIPTIONS["generic"].get(bucket, ""))

    return THRESHOLD_DESCRIPTIONS["generic"].get(bucket, "")
