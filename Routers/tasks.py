from __future__ import annotations

import asyncio
import logging
from typing import Any

from fastapi import APIRouter, WebSocket, WebSocketDisconnect

logger = logging.getLogger("uvicorn.error")

router = APIRouter(prefix="/tasks", tags=["tasks"])

# Track active subscribers for task notifications.
_active_sockets: set[WebSocket] = set()
_connections_lock = asyncio.Lock()
_server_loop: asyncio.AbstractEventLoop | None = None


async def _register(websocket: WebSocket) -> None:
    global _server_loop
    try:
        _server_loop = asyncio.get_running_loop()
    except RuntimeError:
        _server_loop = None
    async with _connections_lock:
        _active_sockets.add(websocket)


async def _unregister(websocket: WebSocket) -> None:
    async with _connections_lock:
        _active_sockets.discard(websocket)


async def notify_tasks(payload: dict[str, Any]) -> None:
    """
    Broadcast a payload to every connected task websocket client.

    Other routers can call this helper to push arbitrary task-related events.
    """
    stale: list[WebSocket] = []
    async with _connections_lock:
        targets = list(_active_sockets)

    for websocket in targets:
        try:
            await websocket.send_json(payload)
        except Exception as exc:  # pragma: no cover - websocket send failure
            logger.info("Dropping stale task websocket connection: %s", exc)
            stale.append(websocket)

    if stale:
        async with _connections_lock:
            for websocket in stale:
                _active_sockets.discard(websocket)


def notify_tasks_sync(payload: dict[str, Any]) -> None:
    """
    Fire-and-forget wrapper to emit a websocket notification from sync code paths.
    """
    try:
        loop = asyncio.get_running_loop()
    except RuntimeError:
        target_loop = _server_loop
        if target_loop and target_loop.is_running():
            asyncio.run_coroutine_threadsafe(notify_tasks(payload), target_loop)
        else:
            asyncio.run(notify_tasks(payload))
        return
    loop.create_task(notify_tasks(payload))


@router.websocket("/ws")
async def task_notifications(websocket: WebSocket) -> None:
    """
    Websocket endpoint streaming task notifications to connected clients.
    """
    await websocket.accept()
    await _register(websocket)
    try:
        await websocket.send_json({"type": "connected"})
        while True:
            await websocket.receive_text()
    except WebSocketDisconnect:
        pass
    except Exception as exc:  # pragma: no cover - defensive close handling
        logger.debug("Task websocket closed unexpectedly: %s", exc)
    finally:
        await _unregister(websocket)
