from __future__ import annotations

import config
from fastapi import APIRouter

router = APIRouter(prefix="/meta", tags=["meta"])


@router.get("/messages", name="messages")
async def read_messages() -> dict[str, object]:
    """Expose shared user-facing messages for the frontend."""
    return {"messages": config.MESSAGES}
