from fastapi import FastAPI
from fastapi.staticfiles import StaticFiles

from Routers.current_dataset import router as current_dataset_router
from Routers.files import router as files_router
from Routers.metrics import router as metrics_router
from Routers.pages import router as pages_router

app = FastAPI(title="FastAPI Placeholder Dashboard")

app.mount("/static", StaticFiles(directory="Static"), name="static")
app.include_router(files_router)
app.include_router(current_dataset_router)
app.include_router(metrics_router)
app.include_router(pages_router)


@app.get("/health")
async def health():
    """Simple healthcheck endpoint."""
    return {"status": "ok"}
