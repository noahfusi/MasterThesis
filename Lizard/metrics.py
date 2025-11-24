from __future__ import annotations

import re
from typing import List, Dict, Tuple

from lxml import etree

DUPLICATE_RATE_PATTERN = re.compile(r"Total duplicate rate:\s*([0-9.]+)%", re.IGNORECASE)


def sanitize_summary_xml(report: str) -> str:
    """
    @brief Remove trailing content after the closing cppncss tag.
    @param report Raw XML output string from Lizard.
    @return Sanitized XML text ready for parsing.
    """
    closing_tag = "</cppncss>"
    idx = report.find(closing_tag)
    if idx != -1:
        return report[: idx + len(closing_tag)]
    return report


def parse_summary_metrics(summary_text: str) -> Tuple[List[Dict[str, object]], List[Dict[str, object]]]:
    """
    @brief Parse dataset-wide Lizard summary metrics using lxml.
    @param summary_text Raw XML summary content.
    @return Tuple containing metric definitions and per-file metric entries.
    @throws ValueError When XML cannot be parsed.
    """
    xml_payload = sanitize_summary_xml(summary_text)
    try:
        root = etree.fromstring(xml_payload.encode("utf-8"))
    except etree.XMLSyntaxError as exc:
        raise ValueError("Invalid Lizard summary format.") from exc

    measure = root.find(".//measure[@type='File']")
    if measure is None:
        return [], []

    labels = [label.text.strip() for label in measure.findall(".//labels/label") if label.text]
    label_texts = [label for label in labels if label and not label.lower().startswith("nr")]

    average_values: Dict[str, object] = {}
    for average_node in measure.findall("average"):
        label_attr = (average_node.get("label") or "").strip()
        if label_attr and not label_attr.lower().startswith("nr"):
            avg_value = _safe_number(average_node.get("value"))
            if avg_value is not None:
                average_values[label_attr] = avg_value

    metric_definitions: List[Dict[str, object]] = []
    for label in label_texts:
        definition: Dict[str, object] = {"key": label, "label": label}
        if label in average_values:
            definition["average"] = average_values[label]
        metric_definitions.append(definition)

    files: List[Dict[str, object]] = []
    for item in measure.findall("item"):
        raw_name = item.get("name", "")
        relative_name = _normalize_summary_path(raw_name)
        metrics: Dict[str, object] = {}
        for label, value_node in zip(labels, item.findall("value")):
            label_clean = (label or "").strip()
            if not label_clean or label_clean.lower().startswith("nr"):
                continue
            numeric = _safe_number(value_node.text if value_node is not None else None)
            if numeric is not None:
                metrics[label_clean] = numeric
        files.append({"path": relative_name or raw_name, "raw_path": raw_name, "metrics": metrics})

    return metric_definitions, files


def parse_reference_metrics(report: str) -> Dict[str, object]:
    """
    @brief Extract metrics for a reference file from Lizard output.
    @param report Raw XML output for the analyzed file.
    @return Mapping of metric labels to numeric values.
    @throws ValueError When XML cannot be parsed.
    """
    xml_payload = sanitize_summary_xml(report)
    try:
        root = etree.fromstring(xml_payload.encode("utf-8"))
    except etree.XMLSyntaxError as exc:
        raise ValueError("Invalid Lizard output.") from exc

    measure = root.find(".//measure[@type='File']")
    if measure is None:
        return {}

    labels = [label.text.strip() for label in measure.findall(".//labels/label") if label.text]
    item = measure.find("item")
    if item is None:
        return {}

    metrics: Dict[str, object] = {}
    for label, value_node in zip(labels, item.findall("value")):
        label_clean = (label or "").strip()
        if not label_clean or label_clean.lower().startswith("nr"):
            continue
        numeric = _safe_number(value_node.text if value_node is not None else None)
        if numeric is not None:
            metrics[label_clean] = numeric

    duplicate_rate = extract_duplicate_rate(report)
    if duplicate_rate is not None:
        metrics["Duplication (%)"] = duplicate_rate
    return metrics


def _normalize_summary_path(path_value: str) -> str:
    """
    @brief Normalize a path reported by Lizard to a relative raw file path.
    @param path_value Raw path text from the report.
    @return Normalized relative path string.
    """
    normalized = (path_value or "").replace("\\", "/")
    marker = "/raw/"
    idx = normalized.lower().rfind(marker)
    if idx != -1:
        return normalized[idx + len(marker) :]
    return normalized


def extract_duplicate_rate(text: str) -> float | None:
    """
    @brief Extract duplication percentage from plain-text Lizard output.
    @param text Textual output to scan.
    @return Duplication rate as float or None if not found.
    """
    match = DUPLICATE_RATE_PATTERN.search(text)
    if not match:
        return None
    try:
        return float(match.group(1))
    except ValueError:
        return None


def _safe_number(value: str | None) -> int | float | None:
    """
    @brief Convert a textual value to a numeric type when possible.
    @param value String containing a potential number.
    @return Parsed int/float or None on failure.
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
    if number.is_integer():
        return int(number)
    return number
