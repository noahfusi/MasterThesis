(function () {
  const app = window.App || {};
  const { API_ROUTES, utils = {}, dataset = {}, taskSocket = {}, onReady = (fn) => fn() } = app;
  const { requestJSON, showMessage, getMessage } = utils;
  const { getSelectedDataset } = dataset;
  const subscribeToTaskEvents = taskSocket.subscribe || (() => () => {});

  const fileList = document.getElementById("autotest-file-list");
  const feedback = document.getElementById("autotest-feedback");
  const runDatasetButton = document.getElementById("autotest-run-dataset");

  if (!fileList || !feedback) return;

  let currentDataset = getSelectedDataset ? getSelectedDataset() : null;
  const fileState = new Map(); // filename -> {passed,total,status}
  const pendingTimers = new Map();
  let unsubscribeTasks = null;

  function fileResultsDisplay(entry = {}) {
    const passed = Number(entry.passed) || 0;
    const total = Number(entry.total) || 0;
    return `${passed}/${total || 0}`;
  }

  function getFileResultsUrl(filenames, datasetName) {
    const list = Array.isArray(filenames) ? filenames : filenames ? [filenames] : [];
    if (typeof API_ROUTES.autotestFileResults === "function") {
      return API_ROUTES.autotestFileResults(list, datasetName);
    }
    const params = new URLSearchParams();
    if (datasetName) params.set("dataset", datasetName);
    list.forEach((name) => params.append("filenames", name));
    const suffix = params.toString();
    return `/autotest/files/results${suffix ? `?${suffix}` : ""}`;
  }

  function applyBatchResults(entries = []) {
    entries.forEach((entry) => {
      const name = entry && entry.filename;
      if (!name) return;
      const results = entry.results || { passed: 0, total: 0 };
      const hasCompileError = Boolean(entry.compile_error);
      fileState.set(name, { ...results, status: entry.status, error: entry.error, compileError: hasCompileError });
      const badge =
        fileList.querySelector(`.dataset-row span.badge[data-file="${CSS.escape(name)}"]`) || null;
      if (badge) {
        badge.textContent = fileResultsDisplay(results);
      }
      const compileBadge =
        fileList.querySelector(`.compile-error[data-file="${CSS.escape(name)}"]`) || null;
      if (compileBadge) {
        if (hasCompileError) {
          compileBadge.textContent = "Compilation error";
          compileBadge.style.display = "";
        } else {
          compileBadge.textContent = "";
          compileBadge.style.display = "none";
        }
      }
    });
  }

  async function fetchBatchResults(filenames = []) {
    if (!currentDataset) return;
    const list = Array.isArray(filenames) ? filenames : filenames ? [filenames] : [];
    const unique = Array.from(new Set(list.filter(Boolean)));
    if (!unique.length) return;

    const CHUNK_SIZE = 25;
    for (let offset = 0; offset < unique.length; offset += CHUNK_SIZE) {
      const chunk = unique.slice(offset, offset + CHUNK_SIZE);
      try {
        const url = getFileResultsUrl(chunk, currentDataset);
        const payload = await requestJSON(url);
        applyBatchResults((payload && payload.files) || []);
      } catch {
        /* ignore fetch errors */
      }
    }
  }

  function renderFiles(files = []) {
    fileList.innerHTML = "";
    if (!files.length) {
      const empty = document.createElement("li");
      empty.textContent = getMessage("AUTOTEST_NO_FILES", {}, "No files found for this dataset.");
      fileList.appendChild(empty);
      return;
    }
    files.forEach((name) => {
      const item = document.createElement("li");
      item.className = "dataset-row";

      const label = document.createElement("span");
      label.textContent = name;
      item.appendChild(label);

      const actions = document.createElement("div");
      actions.className = "dataset-row-actions";

      const status = document.createElement("span");
      status.className = "badge";
      status.dataset.file = name;
      const cached = fileState.get(name) || {};
      status.textContent = fileResultsDisplay(cached);

      const compileStatus = document.createElement("span");
      compileStatus.className = "compile-error";
      compileStatus.dataset.file = name;
      compileStatus.style.display = "none";

      const runBtn = document.createElement("button");
      runBtn.type = "button";
      runBtn.textContent = "Run tests";
      runBtn.addEventListener("click", async () => {
        if (!currentDataset) {
          showMessage(feedback, "Select a dataset before running tests.", true);
          return;
        }
        runBtn.disabled = true;
        try {
          await requestJSON(API_ROUTES.autotestRun, {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({ dataset: currentDataset, filename: name }),
          });
          showMessage(feedback, `AutoTest started for "${name}".`);
          scheduleResultRefresh(name);
        } catch (error) {
          showMessage(feedback, error.message || "Unable to fetch results.", true);
        } finally {
          runBtn.disabled = false;
        }
      });

      actions.appendChild(status);
      actions.appendChild(compileStatus);
      actions.appendChild(runBtn);

      item.appendChild(actions);
      fileList.appendChild(item);
    });

    if (currentDataset) {
      void fetchBatchResults(files);
    }
  }

  async function loadFiles(datasetName) {
    currentDataset = datasetName;
    if (!datasetName) {
      renderFiles([]);
      showMessage(feedback, "Select a dataset to load files.", true);
      return;
    }
    showMessage(feedback, "Loading files...");
    try {
      const params = new URLSearchParams({ dataset: datasetName });
      const payload = await requestJSON(`${API_ROUTES.autotestFiles}?${params.toString()}`);
      renderFiles(payload.files || []);
      showMessage(feedback, "");
    } catch (error) {
      renderFiles([]);
      showMessage(feedback, error.message || "Unable to load files.", true);
    }
  }

  function scheduleResultRefresh(filename) {
    if (!currentDataset) return;
    const existing = pendingTimers.get(filename);
    if (existing) {
      clearTimeout(existing);
    }
    const timer = setTimeout(async () => {
      void fetchBatchResults([filename]);
    }, 1500);
    pendingTimers.set(filename, timer);
  }

  function handleDatasetRun() {
    if (!currentDataset) {
      showMessage(feedback, "Select a dataset before running AutoTest.", true);
      return;
    }
    requestJSON(API_ROUTES.autotestRun, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ dataset: currentDataset }),
    })
      .then(() => {
        showMessage(feedback, "Dataset-wide AutoTest started.");
      })
      .catch((error) => {
        showMessage(feedback, error.message || "Unable to start AutoTest.", true);
      });
  }

  document.addEventListener("app:dataset-changed", (event) => {
    const nextDataset = event?.detail?.currentDataset || null;
    fileState.clear();
    pendingTimers.forEach((timer) => clearTimeout(timer));
    pendingTimers.clear();
    void loadFiles(nextDataset);
  });

  function handleTaskEvent(event) {
    if (!event || event.type !== "autotest-completed") return;
    if (!currentDataset || event.dataset !== currentDataset) return;
    const target = event.filename;
    if (target) {
      void fetchBatchResults([target]);
    } else {
      // Dataset-wide completion: refresh all known files.
      const knownFiles = Array.from(fileState.keys());
      if (knownFiles.length) {
        void fetchBatchResults(knownFiles);
      }
      // Also refresh the list in case new files were added.
      void loadFiles(currentDataset);
    }
  }

  onReady(() => {
    renderFiles([]);
    void loadFiles(currentDataset);
    if (runDatasetButton) {
      runDatasetButton.addEventListener("click", handleDatasetRun);
    }
    unsubscribeTasks = subscribeToTaskEvents(handleTaskEvent);
  });
})();
