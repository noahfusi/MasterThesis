"""Utility to launch Lizard static analysis on dataset files.

Usage examples:
    python Lizard/run_analysis.py --dataset my_dataset
    python Lizard/run_analysis.py --all
"""
from __future__ import annotations

import argparse
import json
import subprocess
import sys
from pathlib import Path
from typing import Iterable

from Files.dataset_manager import dataset_path, list_datasets, normalize_dataset_name

RAW_FOLDER = "raw"
LIZARD_FOLDER = "lizard"


def _available_datasets() -> list[str]:
    return list_datasets()


def _iter_files(root: Path) -> Iterable[Path]:
    for path in sorted(root.rglob("*")):
        if path.is_file():
            yield path


def _run_lizard(lizard_bin: str, target: Path) -> dict:
    cmd = [
        lizard_bin,
        "--json",
        "-Eduplicate",
        "--warnings_only",
        str(target),
    ]
    try:
        completed = subprocess.run(cmd, capture_output=True, text=True, check=False)
    except FileNotFoundError as exc:
        raise RuntimeError(
            "Lizard executable not found. Install it with 'pip install lizard' or adjust --lizard-bin."
        ) from exc

    if completed.returncode not in {0, 1}:  # 1 is used for warning threshold breaches
        raise RuntimeError(f"Lizard failed for {target}: {completed.stderr.strip() or completed.stdout}")

    try:
        return json.loads(completed.stdout or "{}")
    except json.JSONDecodeError as exc:
        raise RuntimeError(f"Unable to decode Lizard output for {target}: {exc}") from exc


def _write_analysis(dataset: str, raw_root: Path, file_path: Path, data: dict) -> Path:
    relative = file_path.relative_to(raw_root)
    output_root = dataset_path(dataset) / LIZARD_FOLDER
    target = output_root / relative
    target = target.with_suffix(target.suffix + ".lizard.json")
    target.parent.mkdir(parents=True, exist_ok=True)

    payload = {
        "dataset": dataset,
        "raw_file": str(relative).replace("\\", "/"),
        "lizard": data,
    }
    target.write_text(json.dumps(payload, indent=2), encoding="utf-8")
    return target


def analyze_dataset(dataset: str, lizard_bin: str) -> None:
    try:
        normalized = normalize_dataset_name(dataset)
    except ValueError as exc:
        raise RuntimeError(str(exc)) from exc

    raw_dir = dataset_path(normalized) / RAW_FOLDER
    if not raw_dir.exists():
        raise RuntimeError(f"Dataset '{normalized}' has no '{RAW_FOLDER}' directory to scan.")

    files = list(_iter_files(raw_dir))
    if not files:
        print(f"[skip] No files to analyze in {normalized}/{RAW_FOLDER}")
        return

    print(f"[run] Lizard analysis for dataset '{normalized}' on {len(files)} file(s)...")
    for file_path in files:
        try:
            analysis = _run_lizard(lizard_bin, file_path)
            output_path = _write_analysis(normalized, raw_dir, file_path, analysis)
            print(f"  ✓ {file_path.relative_to(raw_dir)} -> {output_path.relative_to(dataset_path(normalized))}")
        except RuntimeError as exc:
            print(f"  ✗ Failed to analyze {file_path}: {exc}")


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Run Lizard analysis on dataset files.")
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument("--dataset", help="Name of the dataset to analyze.")
    group.add_argument("--all", action="store_true", help="Analyze every available dataset.")
    parser.add_argument("--lizard-bin", default="lizard", help="Path to the Lizard executable (default: lizard)")
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv or sys.argv[1:])
    datasets: Iterable[str]
    if args.all:
        datasets = _available_datasets()
        if not datasets:
            print("No datasets available.")
            return 0
    else:
        datasets = [args.dataset]

    exit_code = 0
    for dataset in datasets:
        try:
            analyze_dataset(dataset, args.lizard_bin)
        except RuntimeError as exc:
            print(f"[error] {exc}")
            exit_code = 1
    return exit_code


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
