from __future__ import annotations

import json
import threading
from contextlib import contextmanager
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict

from Files.dataset_manager import dataset_path

STATUS_FILENAME = "status.json"

_LOCKS: dict[str, threading.Lock] = {}
_LOCKS_MUTEX = threading.Lock()


def _timestamp() -> str:
    return datetime.now(timezone.utc).isoformat()


def status_path(dataset: str) -> Path:
    return dataset_path(dataset) / STATUS_FILENAME


def load_status(dataset: str) -> dict[str, Any]:
    path = status_path(dataset)
    if not path.exists():
        return {"dataset": dataset, "state": "unknown", "phase": "unknown"}
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return {"dataset": dataset, "state": "unknown", "phase": "unknown"}


def _write_status(dataset: str, payload: Dict[str, Any]) -> None:
    path = status_path(dataset)
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp_path = path.with_suffix(".tmp")
    tmp_path.write_text(json.dumps(payload, ensure_ascii=False, indent=2), encoding="utf-8")
    tmp_path.replace(path)


def update_status(
    dataset: str,
    *,
    state: str,
    phase: str,
    message: str | None = None,
    error: str | None = None,
    extra: Dict[str, Any] | None = None,
) -> dict[str, Any]:
    status: dict[str, Any] = load_status(dataset)
    status.update(
        {
            "dataset": dataset,
            "state": state,
            "phase": phase,
            "message": message,
            "error": error,
            "updated_at": _timestamp(),
        }
    )
    if "started_at" not in status and state in {"running", "queued"}:
        status["started_at"] = status["updated_at"]
    if extra:
        status.update(extra)
    _write_status(dataset, status)
    return status


def mark_failed(dataset: str, *, phase: str, error: str) -> dict[str, Any]:
    return update_status(dataset, state="failed", phase=phase, message="Processing failed.", error=error)


def is_ready(status: dict[str, Any]) -> bool:
    return status.get("state") == "ready"


def _get_lock(dataset: str) -> threading.Lock:
    with _LOCKS_MUTEX:
        return _LOCKS.setdefault(dataset, threading.Lock())


@contextmanager
def dataset_lock(dataset: str):
    lock = _get_lock(dataset)
    lock.acquire()
    try:
        yield
    finally:
        lock.release()
