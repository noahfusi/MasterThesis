from __future__ import annotations

from typing import Optional

from fastapi import APIRouter, HTTPException, status
from pydantic import BaseModel

from Files.dataset_manager import get_current_dataset, list_datasets, set_current_dataset
from Services import dataset_service

router = APIRouter(prefix="/current-dataset", tags=["current_dataset"])


class DatasetSelection(BaseModel):
    """
    @brief Request payload specifying the dataset to select.
    """
    dataset_name: Optional[str] = None


@router.get("", name="get-current-dataset")
async def read_current_dataset() -> dict[str, object]:
    """
    @brief Retrieve the currently selected dataset.
    @return Mapping with current dataset and list of available datasets.
    """
    current_name = get_current_dataset()
    current = dataset_service.summarize_dataset(current_name) if current_name else None
    return {"current_dataset": current, "datasets": dataset_service.list_dataset_summaries(list_datasets())}


@router.post("", name="set-current-dataset")
async def update_current_dataset(selection: DatasetSelection) -> dict[str, object]:
    """
    @brief Update the currently selected dataset.
    @param selection Body payload containing the dataset name.
    @return Mapping with the updated current dataset.
    @throws HTTPException On validation errors.
    """
    try:
        current = set_current_dataset(selection.dataset_name)
    except ValueError as exc:
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail=str(exc)) from exc

    summary = dataset_service.summarize_dataset(current) if current else None
    return {"current_dataset": summary}
