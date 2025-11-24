"""Utility to launch Lizard static analysis on dataset files.

Usage examples:
    python Lizard/run_analysis.py --dataset my_dataset
    python Lizard/run_analysis.py --all
"""
from __future__ import annotations

import argparse
from concurrent.futures import ProcessPoolExecutor, as_completed
import multiprocessing
import subprocess
import sys
from pathlib import Path
import re
from typing import Iterable

from Files.dataset_manager import dataset_path, list_datasets, normalize_dataset_name

RAW_FOLDER = "raw"
LIZARD_FOLDER = "lizard"
SHOW_LIZARD_OUTPUT = False
MIN_DUPLICATE_LINES = 30


def _available_datasets() -> list[str]:
    """
    @brief Retrieve available datasets from the dataset manager.
    @return List of dataset names.
    """
    return list_datasets()


def _iter_files(root: Path) -> Iterable[Path]:
    """
    @brief Iterate over all files under a root directory.
    @param root Directory to recurse.
    @return Generator yielding file paths.
    """
    for path in sorted(root.rglob("*")):
        if path.is_file():
            yield path


def _run_lizard(lizard_bin: str, target: Path) -> str:
    """
    @brief Execute Lizard on a single target and return its output.
    @param lizard_bin Path to the Lizard executable.
    @param target File or directory to analyze.
    @return Raw stdout from Lizard.
    @throws RuntimeError When execution fails or Lizard is missing.
    """
    cmd = [
        lizard_bin,
        "-l",
        "scala",
        "--xml",
        "-Eduplicate",
        str(target),
    ]
    print(f"    └─ Running command: {' '.join(cmd)}")
    try:
        completed = subprocess.run(cmd, capture_output=True, text=True, check=False)
    except FileNotFoundError as exc:
        raise RuntimeError(
            "Lizard executable not found. Install it with 'pip install lizard' or adjust --lizard-bin."
        ) from exc

    if completed.returncode not in {0, 1}:  # 1 may signal warning threshold breaches
        raise RuntimeError(
            f"Lizard failed for {target}: {completed.stderr.strip() or completed.stdout}", completed.stdout
        )

    return completed.stdout


def _write_analysis(dataset: str, raw_root: Path, file_path: Path, raw_output: str) -> Path:
    """
    @brief Persist Lizard XML output alongside the analyzed file.
    @param dataset Dataset name for resolving output location.
    @param raw_root Root raw folder for the dataset.
    @param file_path File that was analyzed.
    @param raw_output Raw XML output produced by Lizard.
    @return Path to the written analysis file.
    """
    relative = file_path.relative_to(raw_root)
    output_root = dataset_path(dataset) / LIZARD_FOLDER
    target = output_root / relative
    target = target.with_suffix(target.suffix + ".lizard.xml")
    target.parent.mkdir(parents=True, exist_ok=True)

    target.write_text(raw_output, encoding="utf-8")
    return target


def _filter_duplicate_blocks(report: str) -> str:
    """
    @brief Filter duplicate detection blocks based on configured thresholds.
    @param report Combined textual report output.
    @return Report text with filtered duplicate blocks.
    """
    lines = report.splitlines()
    filtered: list[str] = []
    i = 0
    while i < len(lines):
        line = lines[i]
        if line.strip() == "Duplicate block:" and i + 2 < len(lines):
            divider = lines[i + 1]
            j = i + 2
            block_lines: list[str] = []
            start_of_block = i
            while j < len(lines) and not lines[j].strip().startswith("^"):
                block_lines.append(lines[j])
                j += 1
            end_line = lines[j] if j < len(lines) else None
            end_of_block = j
            files = set()
            spans: list[int] = []
            for entry in block_lines:
                if ":" not in entry or not entry.strip():
                    continue
                files.add(entry.split(":")[0].strip())
                match = re.search(r"(\d+)\s*~\s*(\d+)", entry)
                if match:
                    start, end = int(match.group(1)), int(match.group(2))
                    if end >= start:
                        spans.append(end - start)
            include_block = bool(files)
            if include_block and MIN_DUPLICATE_LINES > 0:
                include_block = any(span >= MIN_DUPLICATE_LINES for span in spans)
            if include_block:
                filtered.append(line)
                filtered.append(divider)
                filtered.extend(block_lines)
                if end_line is not None:
                    filtered.append(end_line)
            else:
                # Skip this block entirely
                pass
            next_index = end_of_block + 1 if end_of_block < len(lines) else len(lines)
            while next_index < len(lines) and not lines[next_index].strip():
                next_index += 1
            i = next_index
            continue
        filtered.append(line)
        i += 1

    output = "\n".join(filtered)
    if report.endswith("\n") and not output.endswith("\n"):
        output += "\n"
    return output


def _analyze_file_job(args: tuple[str, str, str, str]) -> tuple[str, str] | tuple[str, Exception]:
    """Run lizard for a single file; returns (relative_path, output) or (relative_path, error)."""
    dataset, raw_dir_str, relative_str, lizard_bin = args
    raw_dir = Path(raw_dir_str)
    try:
        raw_path = raw_dir / relative_str
        target = raw_path
        output = _run_lizard(lizard_bin, target)
        return relative_str, output
    except Exception as exc:  # pragma: no cover - worker error path
        return relative_str, exc


def analyze_dataset(dataset: str, lizard_bin: str) -> None:
    """
    @brief Run Lizard analysis for all files in a dataset and persist results.
    @param dataset Dataset name to analyze.
    @param lizard_bin Executable path for Lizard.
    @throws RuntimeError On validation errors or Lizard failures.
    """
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
    max_workers = max(1, min(multiprocessing.cpu_count(), 8))
    tasks = [
        (normalized, str(raw_dir), str(path.relative_to(raw_dir)), lizard_bin)
        for path in files
    ]

    with ProcessPoolExecutor(max_workers=max_workers) as executor:
        futures = {executor.submit(_analyze_file_job, task): task for task in tasks}
        for future in as_completed(futures):
            rel = futures[future][2]
            try:
                relative_str, result = future.result()
            except Exception as exc:  # pragma: no cover - aggregator failure
                print(f"  ✗ Failed to analyze {rel}: {exc}")
                continue

            file_path = raw_dir / relative_str
            if isinstance(result, Exception):
                print(f"  ✗ Failed to analyze {relative_str}: {result}")
                continue
            raw_output = result
            try:
                output_path = _write_analysis(normalized, raw_dir, file_path, raw_output)
                print(f"  ✓ {file_path.relative_to(raw_dir)} -> {output_path.relative_to(dataset_path(normalized))}")
                if SHOW_LIZARD_OUTPUT:
                    if raw_output.strip():
                        print("    └─ Lizard output:")
                        print("\n".join(f"       {line}" for line in raw_output.strip().splitlines()))
                    else:
                        print("    └─ Lizard output: <empty>")
            except RuntimeError as exc:
                message = str(exc)
                raw_output = ""
                if isinstance(exc.args, tuple) and len(exc.args) > 1:
                    raw_output = exc.args[1] or ""
                print(f"  ✗ Failed to analyze {file_path}: {message}")
                if SHOW_LIZARD_OUTPUT and raw_output:
                    print("    └─ Lizard output:")
                    print("\n".join(f"       {line}" for line in raw_output.strip().splitlines()))

    # Dataset-wide run
    try:
        print(f"[run] Lizard dataset summary for '{normalized}'")
        dataset_output = _run_lizard(lizard_bin, raw_dir)
        filtered_dataset_output = _filter_duplicate_blocks(dataset_output)
        dataset_root = dataset_path(normalized)
        dataset_output_path = dataset_root / "lizard_dataset.xml"
        dataset_output_path.write_text(filtered_dataset_output, encoding="utf-8")
        print(f"  ✓ Summary saved to {dataset_output_path.relative_to(dataset_root)}")
        if SHOW_LIZARD_OUTPUT:
            if filtered_dataset_output.strip():
                print("    └─ Lizard output:")
                print("\n".join(f"       {line}" for line in filtered_dataset_output.strip().splitlines()))
            else:
                print("    └─ Lizard output: <empty>")
    except RuntimeError as exc:
        message = str(exc)
        summary_output = ""
        if isinstance(exc.args, tuple) and len(exc.args) > 1:
            summary_output = exc.args[1] or ""
        print(f"  ✗ Failed dataset summary for {normalized}: {message}")
        if SHOW_LIZARD_OUTPUT and summary_output:
            print("    └─ Lizard output:")
            print("\n".join(f"       {line}" for line in summary_output.strip().splitlines()))


def parse_args(argv: list[str]) -> argparse.Namespace:
    """
    @brief Configure and parse CLI arguments for the analyzer.
    @param argv Argument vector excluding the executable.
    @return Parsed arguments namespace.
    """
    parser = argparse.ArgumentParser(description="Run Lizard analysis on dataset files.")
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument("--dataset", help="Name of the dataset to analyze.")
    group.add_argument("--all", action="store_true", help="Analyze every available dataset.")
    parser.add_argument("--lizard-bin", default="lizard", help="Path to the Lizard executable (default: lizard)")
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    """
    @brief Entrypoint for running Lizard analysis via CLI.
    @param argv Optional command-line arguments override.
    @return Exit code indicating success (0) or failure (1).
    """
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
