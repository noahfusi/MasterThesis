from __future__ import annotations

import re
import xml.etree.ElementTree as ET

DUPLICATE_RATE_PATTERN = re.compile(r"Total duplicate rate:\s*([0-9.]+)%", re.IGNORECASE)


def sanitize_summary_xml(report: str) -> str:
    closing_tag = "</cppncss>"
    idx = report.find(closing_tag)
    if idx != -1:
        return report[: idx + len(closing_tag)]
    return report


def parse_summary_metrics(summary_text: str) -> tuple[list[dict[str, object]], list[dict[str, object]]]:
    xml_payload = sanitize_summary_xml(summary_text)
    try:
        root = ET.fromstring(xml_payload)
    except ET.ParseError as exc:
        raise ValueError("Invalid Lizard summary format.") from exc

    measure = root.find(".//measure[@type='File']")
    if measure is None:
        return [], []

    labels_element = measure.find("labels")
    label_texts: list[str] = []
    if labels_element is not None:
        for label_node in labels_element.findall("label"):
            if label_node.text:
                label_texts.append(label_node.text.strip())

    metric_labels = [label for label in label_texts if label and not label.lower().startswith("nr")]
    average_values: dict[str, object] = {}
    capturing_averages = False
    for child in measure:
        if child.tag == "average":
            capturing_averages = True
            label_attr = (child.attrib.get("label") or "").strip()
            value_attr = child.attrib.get("value")
            avg_value = _safe_number(value_attr)
            if label_attr and not label_attr.lower().startswith("nr") and avg_value is not None:
                average_values[label_attr] = avg_value
            continue
        if capturing_averages:
            break

    metric_definitions: list[dict[str, object]] = []
    for label in metric_labels:
        definition: dict[str, object] = {"key": label, "label": label}
        if label in average_values:
            definition["average"] = average_values[label]
        metric_definitions.append(definition)

    files: list[dict[str, object]] = []
    for item in measure.findall("item"):
        raw_name = item.attrib.get("name", "")
        relative_name = _normalize_summary_path(raw_name)
        value_nodes = item.findall("value")
        metrics: dict[str, object] = {}
        for label, value_node in zip(label_texts, value_nodes):
            label_clean = (label or "").strip()
            if not label_clean or label_clean.lower().startswith("nr"):
                continue
            numeric = _safe_number(value_node.text if value_node is not None else None)
            if numeric is not None:
                metrics[label_clean] = numeric
        files.append({"path": relative_name or raw_name, "raw_path": raw_name, "metrics": metrics})

    return metric_definitions, files


def parse_reference_metrics(report: str) -> dict[str, object]:
    xml_payload = sanitize_summary_xml(report)
    try:
        root = ET.fromstring(xml_payload)
    except ET.ParseError as exc:
        raise ValueError("Invalid Lizard output.") from exc
    measure = root.find(".//measure[@type='File']")
    if measure is None:
        return {}
    labels_element = measure.find("labels")
    label_texts: list[str] = []
    if labels_element is not None:
        for label_node in labels_element.findall("label"):
            if label_node.text:
                label_texts.append(label_node.text.strip())
    item = measure.find("item")
    if item is None:
        return {}
    metrics: dict[str, object] = {}
    for label, value_node in zip(label_texts, item.findall("value")):
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
    normalized = (path_value or "").replace("\\", "/")
    marker = "/raw/"
    idx = normalized.lower().rfind(marker)
    if idx != -1:
        return normalized[idx + len(marker) :]
    return normalized


def extract_duplicate_rate(text: str) -> float | None:
    match = DUPLICATE_RATE_PATTERN.search(text)
    if not match:
        return None
    try:
        return float(match.group(1))
    except ValueError:
        return None


def _safe_number(value: str | None) -> int | float | None:
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
