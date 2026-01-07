(function () {
  const app = window.App || {};
  app.__initialized = app.__initialized || {};
  if (app.__initialized.autotest) return;
  app.__initialized.autotest = true;
  window.App = app;

  const { API_ROUTES, utils = {}, dataset = {}, taskSocket = {}, onReady = (fn) => fn() } = app;
  const { requestJSON, showMessage, getMessage } = utils;
  const { getSelectedDataset } = dataset;
  const subscribeToTaskEvents = taskSocket.subscribe || (() => () => {});

  const fileList = document.getElementById("autotest-file-list");
  const feedback = document.getElementById("autotest-feedback");
  const runDatasetButton = document.getElementById("autotest-run-dataset");
  const filterSelect = document.getElementById("autotest-filter");
  const filterOptionLabels = new Map();
  if (filterSelect) {
    Array.from(filterSelect.options).forEach((option) => {
      filterOptionLabels.set(option.value, option.textContent.trim());
    });
  }

  if (!fileList || !feedback) return;

  let currentDataset = getSelectedDataset ? getSelectedDataset() : null;
  const fileState = new Map(); // filename -> {passed,total,status}
  let allFiles = [];
  const pendingTimers = new Map();
  let unsubscribeTasks = null;

  function fileResultsDisplay(entry = {}) {
    const passed = Number(entry.passed) || 0;
    const total = Number(entry.total) || 0;
    return `${passed}/${total || 0}`;
  }

  function showTestReportModal(content = "", heading = "Test report") {
    const existing = document.querySelector(".autotest-test-modal");
    if (existing) existing.remove();

    const modal = document.createElement("div");
    modal.className = "autotest-test-modal";
    modal.setAttribute("role", "dialog");
    modal.setAttribute("aria-modal", "true");

    const card = document.createElement("div");
    card.className = "autotest-test-card";

    const controls = document.createElement("div");
    controls.className = "autotest-test-controls";

    const title = document.createElement("span");
    title.className = "autotest-test-title";
    title.textContent = heading || "Test report";

    const closeBtn = document.createElement("button");
    closeBtn.type = "button";
    closeBtn.textContent = "Close";
    closeBtn.addEventListener("click", () => modal.remove());

    const pre = document.createElement("pre");
    pre.className = "autotest-test-content";
    pre.textContent = content || "No test report available.";

    controls.appendChild(title);
    controls.appendChild(closeBtn);
    card.appendChild(controls);
    card.appendChild(pre);
    modal.appendChild(card);

    modal.addEventListener("click", (event) => {
      if (event.target === modal) modal.remove();
    });

    document.body.appendChild(modal);
  }

  function normalizeReportText(value) {
    if (value === null || value === undefined) return "";
    const text = typeof value === "string" ? value : String(value);
    return text.replace(/\r\n/g, "\n").replace(/\r/g, "\n").replace(/\\n/g, "\n");
  }

  function indentBlock(text, indent = "  ") {
    if (!text) return "";
    return text
      .split("\n")
      .map((line) => `${indent}${line}`)
      .join("\n");
  }

  function formatTestReport(report = {}, filename = "") {
    if (!report || typeof report !== "object") {
      return normalizeReportText(report);
    }

    const lines = [];
    const targetName = report.target || filename;
    if (targetName) lines.push(`File: ${targetName}`);
    if (report.dataset) lines.push(`Dataset: ${report.dataset}`);
    const summary = report.summary || {};
    if (summary && (summary.passed !== undefined || summary.total !== undefined)) {
      lines.push(`Summary: ${Number(summary.passed) || 0}/${Number(summary.total) || 0}`);
    }

    const results = Array.isArray(report.results) ? report.results : [];
    if (!results.length) {
      if (!lines.length) lines.push("Test report");
      lines.push("");
      lines.push("No test results available.");
      return lines.join("\n");
    }

    results.forEach((entry, index) => {
      if (!entry) return;
      const id = entry.id || `test-${index + 1}`;
      const status = entry.status || "unknown";
      lines.push("");
      lines.push(`[${status}] ${id}`);
      const errors = Array.isArray(entry.errors) ? entry.errors.filter(Boolean) : [];
      if (errors.length) lines.push(`Errors: ${errors.join(", ")}`);
      if (entry.exit_code !== undefined) lines.push(`Exit code: ${entry.exit_code}`);
      if (entry.stdout !== undefined) {
        lines.push("stdout:");
        lines.push(indentBlock(normalizeReportText(entry.stdout)));
      }
      if (entry.stderr !== undefined) {
        lines.push("stderr:");
        lines.push(indentBlock(normalizeReportText(entry.stderr)));
      }
    });

    return lines.join("\n");
  }

  async function handleViewReport(filename, button) {
    if (!currentDataset) {
      showMessage(feedback, "Select a dataset before viewing reports.", true);
      return;
    }
    if (button) button.disabled = true;
    try {
      const params = new URLSearchParams({ dataset: currentDataset, filename });
      const payload = await requestJSON(`${API_ROUTES.autotestTestReport}?${params.toString()}`);
      const report = payload.report || {};
      const formatted = formatTestReport(report, payload.filename || filename);
      showTestReportModal(formatted, `Test report: ${payload.filename || filename}`);
    } catch (error) {
      showMessage(feedback, error.message || "Unable to load test report.", true);
    } finally {
      if (button) button.disabled = false;
    }
  }

  function getFilterValue() {
    return (filterSelect && filterSelect.value) || "all";
  }

  function matchesFilter(filename, filterValue = getFilterValue()) {
    if (filterValue === "all") return true;
    const state = fileState.get(filename) || {};
    const passed = Number(state.passed) || 0;
    const total = Number(state.total) || 0;
    const compileError = Boolean(state.compileError);

    if (filterValue === "compile-error") return compileError;
    if (filterValue === "all-passed") return !compileError && total > 0 && passed === total;
    if (filterValue === "not-all-passed") return !compileError && total > 0 && passed < total;
    return true;
  }

  function getFilteredFiles(files = allFiles) {
    const filterValue = getFilterValue();
    if (filterValue === "all") return files.slice();
    return files.filter((name) => matchesFilter(name, filterValue));
  }

  function updateFilterCounts() {
    if (!filterSelect) return;
    const counts = {
      all: allFiles.length,
      "compile-error": 0,
      "all-passed": 0,
      "not-all-passed": 0,
    };

    allFiles.forEach((name) => {
      if (matchesFilter(name, "compile-error")) counts["compile-error"] += 1;
      if (matchesFilter(name, "all-passed")) counts["all-passed"] += 1;
      if (matchesFilter(name, "not-all-passed")) counts["not-all-passed"] += 1;
    });

    Array.from(filterSelect.options).forEach((option) => {
      const baseLabel = filterOptionLabels.get(option.value) || option.textContent;
      if (Object.prototype.hasOwnProperty.call(counts, option.value)) {
        option.textContent = `${baseLabel} (${counts[option.value]})`;
      } else {
        option.textContent = baseLabel;
      }
    });
  }

  function renderFilteredFiles({ fetchResults = false } = {}) {
    const filtered = getFilteredFiles(allFiles);
    const emptyMessage =
      allFiles.length && !filtered.length
        ? getMessage("AUTOTEST_FILTER_EMPTY", {}, "No files match the selected filter.")
        : null;
    renderFiles(filtered, { emptyMessage, fetchResults });
    updateFilterCounts();
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

  function refreshFilteredView() {
    if (!allFiles.length) return;
    if (getFilterValue() === "all") return;
    renderFilteredFiles({ fetchResults: false });
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
    refreshFilteredView();
    updateFilterCounts();
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

  function renderFiles(files = [], { emptyMessage = null, fetchResults = true } = {}) {
    fileList.innerHTML = "";
    if (!files.length) {
      const empty = document.createElement("li");
      empty.textContent =
        emptyMessage ?? getMessage("AUTOTEST_NO_FILES", {}, "No files found for this dataset.");
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
      if (cached.compileError) {
        compileStatus.textContent = "Compilation error";
        compileStatus.style.display = "";
      } else {
        compileStatus.textContent = "";
        compileStatus.style.display = "none";
      }

      const viewBtn = document.createElement("button");
      viewBtn.type = "button";
      viewBtn.textContent = "View report";
      viewBtn.addEventListener("click", () => {
        void handleViewReport(name, viewBtn);
      });

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
      actions.appendChild(viewBtn);
      actions.appendChild(runBtn);

      item.appendChild(actions);
      fileList.appendChild(item);
    });

    if (currentDataset && fetchResults) {
      void fetchBatchResults(files);
    }
  }

  async function loadFiles(datasetName) {
    currentDataset = datasetName;
    allFiles = [];
    if (!datasetName) {
      renderFiles([]);
      updateFilterCounts();
      showMessage(feedback, "Select a dataset to load files.", true);
      return;
    }
    showMessage(feedback, "Loading files...");
    try {
      const params = new URLSearchParams({ dataset: datasetName });
      const payload = await requestJSON(`${API_ROUTES.autotestFiles}?${params.toString()}`);
      allFiles = payload.files || [];
      renderFilteredFiles({ fetchResults: false });
      if (currentDataset) {
        void fetchBatchResults(allFiles);
      }
      showMessage(feedback, "");
    } catch (error) {
      renderFiles([]);
      updateFilterCounts();
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
    const openModal = document.querySelector(".autotest-test-modal");
    if (openModal) openModal.remove();
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
    if (filterSelect) {
      filterSelect.addEventListener("change", () => {
        renderFilteredFiles({ fetchResults: false });
      });
    }
    if (runDatasetButton) {
      runDatasetButton.addEventListener("click", handleDatasetRun);
    }
    unsubscribeTasks = subscribeToTaskEvents(handleTaskEvent);
  });
})();
