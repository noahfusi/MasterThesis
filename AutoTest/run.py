from __future__ import annotations

import asyncio
from asyncio.subprocess import PIPE
import json
import logging
import os
import re
import shutil
from pathlib import Path
from typing import Any, Dict, List, Optional

import yaml

import config

logger = logging.getLogger("uvicorn.error")

AUTOTEST_FILENAME = "autotest.yaml"
RESULTS_DIRNAME = "tests"

SCALA_DIR = Path(__file__).resolve().parent
OBJECT_DECL_RE = re.compile(r"^\s*object\s+([A-Za-z_][\w\$]*)", re.MULTILINE)
MAIN_METHOD_RE = re.compile(r"\bdef\s+main\s*\(", re.MULTILINE)
EXTENDS_APP_RE = re.compile(r"extends\s+App\b")
VARIANT_SUFFIX_RE = re.compile(r"^(?P<base>.+)_([a-zA-Z])$")


def _dataset_paths(dataset: str) -> tuple[Path, Path, Path]:
    """
    Resolve dataset root, autotest definition, and results directory.
    """
    dataset_root = (config.DATASETS_DIR / dataset).resolve()
    autotest_path = dataset_root / AUTOTEST_FILENAME
    results_dir = dataset_root / RESULTS_DIRNAME
    return dataset_root, autotest_path, results_dir


def _sanitize_result_name(value: Optional[str]) -> str:
    if not value:
        return "dataset"
    return value.replace("\\", "_").replace("/", "_")


def _load_tests(tests_path: Path) -> List[Dict[str, Any]]:
    if not tests_path.exists():
        raise FileNotFoundError(f"Autotest file not found at {tests_path}")
    with tests_path.open("r", encoding="utf-8") as handle:
        payload = yaml.safe_load(handle) or {}
    tests = payload.get("tests") or []
    if not isinstance(tests, list):
        raise ValueError("Invalid autotest format: 'tests' must be a list.")
    return tests


def _ensure_test_ids(tests: list[dict[str, Any]]) -> None:
    """
    Ensure every test has an id so summaries stay stable.
    """
    for index, test in enumerate(tests, start=1):
        if isinstance(test, dict) and not test.get("id"):
            test["id"] = f"test-{index}"


def _scenario_group_id(test_id: str) -> str:
    """
    Treat suffixes like _a/_b as variants of the same scenario.
    """
    match = VARIANT_SUFFIX_RE.match(test_id)
    if match:
        return match.group("base")
    return test_id


def _summarize_results(results: list[dict[str, Any]]) -> dict[str, int]:
    grouped: dict[str, list[dict[str, Any]]] = {}
    for entry in results:
        test_id = entry.get("id") or "test-unknown"
        group_id = _scenario_group_id(test_id)
        grouped.setdefault(group_id, []).append(entry)

    passed = sum(
        1 for entries in grouped.values() if any(item.get("status") == "ok" for item in entries)
    )
    return {"passed": passed, "total": len(grouped)}


def _brace_depth_before(content: str, pos: int) -> int:
    """
    Approximate brace depth (ignoring braces inside strings/comments) before a position.
    Used to prefer top-level object declarations over nested ones.
    """
    depth = 0
    i = 0
    in_line_comment = False
    in_block_comment = False
    in_string = False
    in_char = False
    length = len(content)

    while i < min(pos, length):
        ch = content[i]
        nxt = content[i + 1] if i + 1 < length else ""

        if in_line_comment:
            if ch == "\n":
                in_line_comment = False
        elif in_block_comment:
            if ch == "*" and nxt == "/":
                in_block_comment = False
                i += 1
        elif in_string:
            if ch == "\\":
                i += 1  # skip escaped char
            elif ch == '"':
                in_string = False
        elif in_char:
            if ch == "\\":
                i += 1
            elif ch == "'":
                in_char = False
        else:
            if ch == "/" and nxt == "/":
                in_line_comment = True
                i += 1
            elif ch == "/" and nxt == "*":
                in_block_comment = True
                i += 1
            elif ch == '"':
                in_string = True
            elif ch == "'":
                in_char = True
            elif ch == "{":
                depth += 1
            elif ch == "}":
                depth = max(0, depth - 1)
        i += 1

    return depth


def _detect_main_object(content: str) -> tuple[str, bool]:
    """
    Best-effort detection of the object providing the program entrypoint.

    Preference order:
    - Object containing a def main method.
    - Object extending App.
    - First declared object.
    """
    candidates: list[tuple[int, int, str]] = []
    class_re = re.compile(r"^\s*class\s+[A-Za-z_][\w\$]*", re.MULTILINE)
    found_object = False

    for match in OBJECT_DECL_RE.finditer(content):
        name = match.group(1)
        if _brace_depth_before(content, match.start()) != 0:
            continue  # nested object, skip to avoid invalid fqcn
        found_object = True
        start = match.end()
        next_object = OBJECT_DECL_RE.search(content, pos=start)
        next_class = class_re.search(content, pos=start)
        next_positions = [m.start() for m in (next_object, next_class) if m]
        end = min(next_positions) if next_positions else len(content)
        block = content[start:end]

        score = 0
        if MAIN_METHOD_RE.search(block):
            score = 2
        elif EXTENDS_APP_RE.search(match.group(0) + block):
            score = 1

        candidates.append((score, match.start(), name))

    if candidates:
        candidates.sort(key=lambda entry: (-entry[0], entry[1]))
        return candidates[0][2], found_object
    return "Main", found_object


def _split_package(content: str) -> tuple[Optional[str], str]:
    """
    Extract leading package declaration (if any) and return remaining body.
    """
    package_line: Optional[str] = None
    body_lines: list[str] = []
    found = False
    for line in content.splitlines():
        stripped = line.strip()
        if not found and stripped.startswith("package "):
            package_line = stripped
            found = True
            continue
        body_lines.append(line)
    body = "\n".join(body_lines).lstrip("\n")
    return package_line, body


def _qualify_object_name(obj: str, package_decl: Optional[str]) -> str:
    """
    Build a fully-qualified reference to an object, avoiding clashes with local names.
    """
    if package_decl:
        pkg = package_decl.replace("package", "", 1).strip()
        if pkg:
            return f"{pkg}.{obj}"
    return obj


def _prepare_workspace(dataset_root: Path, target_file: str) -> Path:
    """
    Create an isolated workspace to avoid classfile conflicts across runs.

    On garde un workspace par fichier cible, mais on peut facilement changer
    la stratégie si besoin (ajouter un suffixe unique, etc.).
    """
    base_name = _sanitize_result_name(Path(target_file).with_suffix("").as_posix())
    sanitized = base_name or "dataset"

    workspace = SCALA_DIR / sanitized

    # Nettoyage préalable du workspace de ce fichier
    if workspace.exists():
        shutil.rmtree(workspace, ignore_errors=True)

    workspace.mkdir(parents=True, exist_ok=True)

    source = (dataset_root / config.RAW_FOLDER_NAME / target_file).resolve()
    if not source.exists():
        raise FileNotFoundError(f"Source file not found: {source}")

    content = source.read_text(encoding="utf-8")

    # Suppression des Thread.sleep pour éviter les délais artificiels
    lines = []
    for line in content.splitlines():
        if "Thread.sleep" in line:
            continue
        lines.append(line)

    sanitized_content = "\n".join(lines)

    main_object, has_object = _detect_main_object(sanitized_content)
    package_decl, body_without_package = _split_package(sanitized_content)
    target_filename = f"{main_object}.scala" if has_object else "Main.scala"
    target_path = workspace / target_filename

    if not has_object:
        # Script-like file: wrap content in a runnable object Main.
        indented_body = "\n".join("  " + ln if ln.strip() else "" for ln in body_without_package.splitlines())
        wrapper_lines: list[str] = []
        if package_decl:
            wrapper_lines.append(package_decl)
            wrapper_lines.append("")
        wrapper_lines.append("object Main extends App {")
        if indented_body:
            wrapper_lines.append(indented_body)
        wrapper_lines.append("}")
        wrapper_content = "\n".join(wrapper_lines) + "\n"
        target_path.write_text(wrapper_content, encoding="utf-8")
    else:
        target_path.write_text(sanitized_content, encoding="utf-8")
        if main_object != "Main":
            qualified = _qualify_object_name(main_object, package_decl)
            wrapper_lines: list[str] = []
            if package_decl:
                wrapper_lines.append(package_decl)
                wrapper_lines.append("")
            wrapper_lines.extend(
                [
                    "object Main {",
                    "  def main(args: Array[String]): Unit = {",
                    f"    {qualified}.main(args)",
                    "  }",
                    "}",
                ]
            )
            wrapper_content = "\n".join(wrapper_lines) + "\n"
            (workspace / "Main.scala").write_text(wrapper_content, encoding="utf-8")
    return workspace


async def _compile_workspace(workspace: Path, timeout: float = 20.0) -> tuple[int, str, str]:
    """
    Compile all Scala sources in the workspace and return (exit_code, stdout, stderr).

    Pattern robuste : communicate() + gestion explicite des timeouts/erreurs,
    sans utiliser directement wait() (communicate attend déjà la fin du process
    et ferme les pipes).
    """
    scala_files = [str(path.resolve()) for path in workspace.glob("*.scala")]
    if not scala_files:
        return 1, "", "No Scala sources to compile."
    logger.info("scalac files: %s", scala_files)

    proc = await asyncio.create_subprocess_exec(
        "scalac",
        *scala_files,
        cwd=str(workspace),
        stdout=PIPE,
        stderr=PIPE,
    )

    try:
        stdout_bytes, stderr_bytes = await asyncio.wait_for(proc.communicate(), timeout=timeout)
        exit_code = proc.returncode if proc.returncode is not None else 1
        stdout = stdout_bytes.decode(errors="replace")
        stderr = stderr_bytes.decode(errors="replace")
        return exit_code, stdout, stderr

    except asyncio.TimeoutError:
        # Timeout : on tue le process puis on draine les pipes
        try:
            proc.kill()
        except ProcessLookupError:
            pass
        stdout_bytes, stderr_bytes = await proc.communicate()
        stdout = stdout_bytes.decode(errors="replace")
        stderr = stderr_bytes.decode(errors="replace")
        return 1, stdout, "Compilation timeout."

    except Exception as exc:  # pragma: no cover - defensive
        # Erreur inattendue : on kill + communicate pour fermer proprement
        try:
            proc.kill()
        except Exception:
            pass
        try:
            stdout_bytes, stderr_bytes = await proc.communicate()
        except Exception:
            stdout_bytes, stderr_bytes = b"", b""
        stdout = stdout_bytes.decode(errors="replace")
        stderr = stderr_bytes.decode(errors="replace")
        # On renvoie l'exception dans "stderr" pour debug
        return 1, stdout, f"{stderr}\n{exc}"


_SCALA_RUNTIME_JARS: list[str] | None = None


def _discover_scala_runtime_jars() -> list[str]:
    """
    Locate the Scala runtime jars so we can run compiled classes with java.
    """
    global _SCALA_RUNTIME_JARS
    if _SCALA_RUNTIME_JARS is not None:
        return _SCALA_RUNTIME_JARS

    jars: list[str] = []
    candidate_roots: list[Path] = []

    scala_home = os.environ.get("SCALA_HOME")
    if scala_home:
        candidate_roots.append(Path(scala_home))

    scalac_path = shutil.which("scalac")
    if scalac_path:
        candidate_roots.append(Path(scalac_path).resolve().parent.parent)

    candidate_roots.extend(
        [
            Path("/usr/share/scala"),
            Path("/usr/lib/scala"),
            Path("/usr/share/java"),
        ]
    )

    patterns = ("scala-library*.jar", "scala3-library*.jar", "scala-reflect*.jar")
    for root in candidate_roots:
        for lib_dir in (root, root / "lib"):
            if not lib_dir.exists():
                continue
            for pattern in patterns:
                for jar in lib_dir.glob(pattern):
                    jars.append(str(jar.resolve()))
        if jars:
            break

    _SCALA_RUNTIME_JARS = jars
    return jars


def _build_classpath(workspace: Path) -> str:
    entries = [str(workspace)]
    entries.extend(_discover_scala_runtime_jars())
    return os.pathsep.join(entries)


async def _run_single_test(workspace: Path, test: Dict[str, Any], timeout: float = 10.0):
    test_id = test.get("id") or "test-unknown"
    inputs = "\n".join(test.get("inputs") or []) + "\n"

    proc = await asyncio.create_subprocess_exec(
        "java",
        "-cp",
        _build_classpath(workspace),
        "Main",
        cwd=str(workspace),
        stdin=PIPE,
        stdout=PIPE,
        stderr=PIPE,
    )

    try:
        stdout_bytes, stderr_bytes = await asyncio.wait_for(
            proc.communicate(inputs.encode()),
            timeout=timeout,
        )
        exit_code = proc.returncode if proc.returncode is not None else 1

        return {
            "id": test_id,
            "status": "ok" if exit_code == 0 else "failed",
            "stdout": stdout_bytes.decode(errors="replace"),
            "stderr": stderr_bytes.decode(errors="replace"),
            "exit_code": exit_code,
            "errors": [],
        }

    except asyncio.TimeoutError:
        # Timeout : kill + communicate pour fermer transport et pipes
        try:
            proc.kill()
        except ProcessLookupError:
            pass
        stdout_bytes, stderr_bytes = await proc.communicate()

        return {
            "id": test_id,
            "status": "timeout",
            "stdout": stdout_bytes.decode(errors="replace"),
            "stderr": stderr_bytes.decode(errors="replace"),
            "errors": ["timeout"],
        }

    except Exception as exc:
        # Erreur inattendue : kill + communicate
        try:
            proc.kill()
        except Exception:
            pass
        try:
            stdout_bytes, stderr_bytes = await proc.communicate()
        except Exception:
            stdout_bytes, stderr_bytes = b"", b""

        return {
            "id": test_id,
            "status": "error",
            "stdout": stdout_bytes.decode(errors="replace"),
            "stderr": stderr_bytes.decode(errors="replace"),
            "errors": [str(exc)],
        }


async def _run_tests_once(dataset: str, target_file: str) -> dict[str, Any]:
    """
    Version entièrement async de l'exécution des tests pour un fichier cible.
    """
    dataset_root, autotest_path, results_dir = _dataset_paths(dataset)
    tests = _load_tests(autotest_path)
    _ensure_test_ids(tests)
    if not target_file:
        raise ValueError("target_file is required for running tests.")

    workspace = _prepare_workspace(dataset_root, target_file)

    # Prepare execution artifacts
    results_dir.mkdir(parents=True, exist_ok=True)
    result_filename = f"{_sanitize_result_name(target_file)}.json"
    result_path = results_dir / result_filename

    try:
        results: list[dict[str, Any]] = []
        logger.info(
            "[autotest] start run dataset=%s target=%s tests=%d",
            dataset,
            target_file,
            len(tests),
        )

        compile_exit_code, compile_stdout, compile_stderr = await _compile_workspace(workspace)
        if compile_exit_code != 0:
            logger.warning(
                "[autotest] compilation failed dataset=%s target=%s exit=%s",
                dataset,
                target_file,
                compile_exit_code,
            )
            if compile_stdout or compile_stderr:
                logger.warning(
                    "[autotest] compilation logs dataset=%s target=%s stdout=%s stderr=%s",
                    dataset,
                    target_file,
                    compile_stdout.strip(),
                    compile_stderr.strip(),
                )
            if tests:
                for test in tests:
                    results.append(
                        {
                            "id": test.get("id") or "test-unknown",
                            "status": "error",
                            "stdout": compile_stdout,
                            "stderr": compile_stderr,
                            "exit_code": compile_exit_code,
                            "errors": ["compile_error"],
                        }
                    )
            else:
                results.append(
                    {
                        "id": "compile",
                        "status": "error",
                        "stdout": compile_stdout,
                        "stderr": compile_stderr,
                        "exit_code": compile_exit_code,
                        "errors": ["compile_error"],
                    }
                )
        else:
            # On garde les tests séquentiels pour un même fichier (plus simple et
            # suffisant), mais tout est non-bloquant grâce à asyncio.
            for test in tests:
                result = await _run_single_test(workspace, test)
                results.append(result)

        summary = _summarize_results(results)
        result_payload = {
            "dataset": dataset,
            "target": target_file,
            "results": results,
            "summary": summary,
        }
        result_path.write_text(
            json.dumps(result_payload, ensure_ascii=False, indent=2),
            encoding="utf-8",
        )
        logger.info(
            "[autotest] finished dataset=%s target=%s passed=%s/%s path=%s",
            dataset,
            target_file,
            summary.get("passed"),
            summary.get("total"),
            result_path,
        )
        return {"dataset": dataset, "target": target_file, "path": result_path, "summary": summary}

    except Exception as exc:
        logger.warning("[autotest] run failed dataset=%s target=%s error=%s", dataset, target_file, exc)
        return {
            "dataset": dataset,
            "target": target_file,
            "path": result_path,
            "summary": {"passed": 0, "total": 0},
            "error": str(exc),
        }
    finally:
        # Cleanup workspace after tests to avoid leftover class files.
        try:
            shutil.rmtree(workspace, ignore_errors=True)
        except Exception:
            logger.debug("[autotest] failed to clean workspace %s", workspace)


async def run_autotest(dataset: str, target_file: Optional[str] = None) -> list[dict[str, Any]]:
    """
    Execute autotests for a dataset, optionally scoped to a single file.

    Version entièrement async, sans threads explicites.
    Concurrence limitée par un sémaphore.
    """
    if target_file:
        return [await _run_tests_once(dataset, target_file)]

    # Dataset-wide: run once per raw file.
    dataset_root, _, _ = _dataset_paths(dataset)
    raw_dir = dataset_root / config.RAW_FOLDER_NAME
    if not raw_dir.exists():
        raise FileNotFoundError(f"Raw folder not found for dataset {dataset}")

    sem = asyncio.Semaphore(16)
    tasks: list[asyncio.Task] = []

    async def worker(rel: str):
        async with sem:
            return await _run_tests_once(dataset, rel)

    for path in raw_dir.rglob("*"):
        if path.is_file():
            rel = path.relative_to(raw_dir).as_posix()
            tasks.append(asyncio.create_task(worker(rel)))

    if not tasks:
        raise FileNotFoundError("No files found in raw dataset.")

    results = await asyncio.gather(*tasks)
    return list(results)


def load_results(dataset: str, target_file: Optional[str] = None) -> Optional[Dict[str, Any]]:
    """
    Load existing test results from disk, if present.
    """
    _, _, results_dir = _dataset_paths(dataset)
    path = results_dir / f"{_sanitize_result_name(target_file)}.json"
    if not path.exists():
        return None
    try:
        with path.open("r", encoding="utf-8") as handle:
            return json.load(handle)
    except (OSError, json.JSONDecodeError):
        return None
