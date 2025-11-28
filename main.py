from pathlib import Path

from fastapi import FastAPI, HTTPException
from fastapi.responses import FileResponse
from fastapi.staticfiles import StaticFiles

import config
from Routers.clustering import router as clustering_router
from Routers.current_dataset import router as current_dataset_router
from Routers.feedback import router as feedback_router
from Routers.files import router as files_router
from Routers.metrics import router as metrics_router
from Routers.pages import router as pages_router
from Routers.reports import router as reports_router
from Routers.meta import router as meta_router
from Routers.tasks import router as tasks_router

app = FastAPI(title=config.APP_TITLE)

app.mount(config.STATIC_MOUNT_PATH, StaticFiles(directory=config.STATIC_DIR), name="static")
app.include_router(files_router)
app.include_router(current_dataset_router)
app.include_router(metrics_router)
app.include_router(clustering_router)
app.include_router(reports_router)
app.include_router(meta_router)
app.include_router(pages_router)
app.include_router(feedback_router)
app.include_router(tasks_router)


@app.get("/task-sw.js", include_in_schema=False)
async def task_service_worker():
    """Serve the shared task websocket service worker."""
    sw_path = Path(config.STATIC_DIR) / "task-sw.js"
    if not sw_path.exists():
        raise HTTPException(status_code=404, detail="Service worker not found.")
    return FileResponse(sw_path)


@app.get("/health")
async def health():
    """Simple healthcheck endpoint."""
    return {"status": "ok"}
