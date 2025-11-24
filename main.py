from fastapi import FastAPI
from fastapi.staticfiles import StaticFiles

import config
from Routers.clustering import router as clustering_router
from Routers.current_dataset import router as current_dataset_router
from Routers.files import router as files_router
from Routers.metrics import router as metrics_router
from Routers.pages import router as pages_router
from Routers.reports import router as reports_router

app = FastAPI(title=config.APP_TITLE)

app.mount(config.STATIC_MOUNT_PATH, StaticFiles(directory=config.STATIC_DIR), name="static")
app.include_router(files_router)
app.include_router(current_dataset_router)
app.include_router(metrics_router)
app.include_router(clustering_router)
app.include_router(reports_router)
app.include_router(pages_router)


@app.get("/health")
async def health():
    """Simple healthcheck endpoint."""
    return {"status": "ok"}
