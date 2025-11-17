from __future__ import annotations

from typing import Optional

from fastapi import APIRouter, HTTPException, status
from pydantic import BaseModel

from Files.dataset_manager import get_current_dataset, list_datasets, set_current_dataset

router = APIRouter(prefix="/current-dataset", tags=["current_dataset"])


class DatasetSelection(BaseModel):
    dataset_name: Optional[str] = None


@router.get("", name="get-current-dataset")
async def read_current_dataset() -> dict[str, Optional[str]]:
    return {"current_dataset": get_current_dataset(), "datasets": list_datasets()}


@router.post("", name="set-current-dataset")
async def update_current_dataset(selection: DatasetSelection) -> dict[str, Optional[str]]:
    try:
        current = set_current_dataset(selection.dataset_name)
    except ValueError as exc:
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail=str(exc)) from exc

    return {"current_dataset": current}
