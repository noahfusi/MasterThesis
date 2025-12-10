(function () {
  const app = window.App || {};
  const { API_ROUTES, utils = {}, dataset = {}, duplicates = {}, onReady = (fn) => fn() } = app;
  const { requestJSON, showMessage, mergeLineRanges } = utils;
  const { setCurrentDataset, getSelectedDataset } = dataset;
  const { getDuplicateSummary, getDuplicateRangesForFile, normalizeRelativePath } = duplicates;

  const datasetSelect = document.getElementById("current-dataset-select");
  const fileExplorerWrapper = document.getElementById("file-explorer");
  const fileListElement = document.getElementById("file-list");
  const fileListFeedback = document.getElementById("file-list-feedback");
  const fileDatasetLabel = document.getElementById("file-list-dataset");
  const fileListFilterSelect = document.getElementById("file-list-filter");
  const filePreviewPanel = document.getElementById("file-preview-panel");
  const filePreviewName = document.getElementById("file-preview-name");
  const filePreviewContent = document.getElementById("file-content");
  const refreshFilesButton = document.getElementById("refresh-files-button");

  if (!fileListElement) return;

  const urlParams = new URLSearchParams(window.location.search || "");
  const initialExplorerDatasetParam = urlParams.get("dataset");
  const initialExplorerFileParam = urlParams.get("file") || urlParams.get("filename");
  let pendingFilePreview = initialExplorerFileParam;
  let pendingFilePreviewDataset = initialExplorerDatasetParam;

  const fileListState = {
    files: [],
    dataset: null,
    lastSummary: null,
    lastSummaryDataset: null,
    metricsByFile: new Map(),
    metricsDataset: null,
    excludedFiles: new Set(),
    excludedDataset: null,
  };

  function resetFileExplorerState() {
    if (fileExplorerWrapper) {
      fileExplorerWrapper.classList.remove("show-preview");
    }
    if (filePreviewName) filePreviewName.textContent = "";
    if (filePreviewContent) filePreviewContent.textContent = "";
    if (fileDatasetLabel && !fileDatasetLabel.textContent) {
      fileDatasetLabel.textContent = "";
    }
  }

  function normalizeFilename(filename) {
    const normalized = normalizeRelativePath(filename);
    if (!normalized) return "";
    let cleaned = normalized.startsWith("./") ? normalized.slice(2) : normalized;
    while (cleaned.startsWith("/")) {
      cleaned = cleaned.slice(1);
    }
    return cleaned;
  }

  function setExcludedState(list = [], datasetName = null) {
    const normalizedList = Array.isArray(list)
      ? list
          .map((name) => normalizeFilename(name))
          .filter(Boolean)
      : [];
    fileListState.excludedFiles = new Set(normalizedList);
    fileListState.excludedDataset = datasetName || null;
  }

  function isFileExcluded(filename) {
    const normalized = normalizeFilename(filename);
    if (!normalized) return false;
    return fileListState.excludedFiles.has(normalized);
  }

  function getFileMetadata(filename) {
    if (!filename || !fileListState.lastSummary || fileListState.lastSummaryDataset !== fileListState.dataset) {
      return null;
    }
    const normalizedName = normalizeRelativePath(filename);
    if (!normalizedName) return null;
    const metaByFile = fileListState.lastSummary.metaByFile || new Map();
    return metaByFile.get(normalizedName) || null;
  }

  function applyFileListFilter(files = [], datasetName = null) {
    const filterValue = (fileListFilterSelect && fileListFilterSelect.value) || "all";
    if (filterValue === "all") {
      return files;
    }
    return files.filter((filename) => {
      const meta = getFileMetadata(filename);
      const metrics = getFileMetrics(filename);
      if (filterValue === "no-functions") {
        if (!metrics) return false;
        const fnValue = Number(metrics["Functions"] ?? metrics["functions"]);
        return Number.isFinite(fnValue) && fnValue === 0;
      }
      if (!datasetName || !fileListState.lastSummary || fileListState.lastSummaryDataset !== datasetName) {
        return files;
      }
      if (!meta) return false;
      if (filterValue === "plagiarism") {
        return Boolean(meta.crossFile);
      }
      if (filterValue === "duplication") {
        return Boolean(meta.crossFile || meta.selfOnly);
      }
      if (filterValue === "no-functions") {
        if (!metrics) return false;
        const functionsMetric = metrics["Functions"] ?? metrics["functions"];
        return functionsMetric === 0;
      }
      return true;
    });
  }

  function getFileMetrics(filename) {
    if (!filename || !fileListState.metricsByFile.size) {
      return null;
    }
    const normalizedName = normalizeRelativePath(filename);
    const fallback = typeof filename === "string" ? filename.replace(/\\/g, "/") : null;
    if (normalizedName && fileListState.metricsByFile.has(normalizedName)) {
      return fileListState.metricsByFile.get(normalizedName);
    }
    if (fallback && fileListState.metricsByFile.has(fallback)) {
      return fileListState.metricsByFile.get(fallback);
    }
    return null;
  }

  async function loadMetricsForDataset(datasetName) {
    fileListState.metricsByFile = new Map();
    fileListState.metricsDataset = null;
    if (!datasetName) return;
    try {
      const params = new URLSearchParams({ dataset: datasetName });
      const payload = await requestJSON(`${API_ROUTES.globalMetrics}?${params.toString()}`);
      const files = payload.files || [];
      const map = new Map();
      files.forEach((entry) => {
        const path = entry && (entry.path || entry.raw_path || entry.filename);
        if (!path) return;
        const normalized = normalizeRelativePath(path);
        const cleaned = typeof path === "string" ? path.replace(/\\/g, "/") : null;
        const metrics = entry.metrics || {};
        if (normalized) {
          map.set(normalized, metrics);
        }
        if (cleaned) {
          map.set(cleaned, metrics);
        }
      });
      fileListState.metricsByFile = map;
      fileListState.metricsDataset = payload.dataset || datasetName;
    } catch (error) {
      console.warn("Unable to load metrics for dataset", datasetName, error);
    }
  }

  async function loadExcludedFiles(datasetName) {
    setExcludedState([], datasetName);
    if (!datasetName) return;
    try {
      const params = new URLSearchParams({ dataset: datasetName });
      const payload = await requestJSON(`${API_ROUTES.excludedFiles}?${params.toString()}`);
      const excluded = Array.isArray(payload.excluded_files) ? payload.excluded_files : [];
      const targetDataset = payload.dataset || datasetName;
      setExcludedState(excluded, targetDataset);
    } catch (error) {
      console.warn("Unable to load excluded files for dataset", datasetName, error);
      setExcludedState([], datasetName);
    }
  }

  async function updateFileDuplicateBadges(datasetName) {
    if (!fileListElement) return;
    fileListElement
      .querySelectorAll(".file-duplicate-badge:not(.file-no-functions):not(.file-excluded-badge)")
      .forEach((badge) => badge.remove());
    fileListElement.querySelectorAll(".dataset-row").forEach((row) => row.classList.remove("has-duplicate-code"));
    fileListState.lastSummary = null;
    fileListState.lastSummaryDataset = null;
    if (!datasetName) return;

    let summary = null;
    try {
      summary = await getDuplicateSummary(datasetName);
      fileListState.lastSummary = summary;
      fileListState.lastSummaryDataset = datasetName;
    } catch (error) {
      if (!(error && typeof error.message === "string" && error.message.toLowerCase().includes("not found"))) {
        console.warn(`Unable to load duplicate summary for dataset "${datasetName}":`, error);
      }
      return;
    }
    if (!summary || (fileListElement.dataset.dataset || "") !== datasetName) {
      return;
    }
    const rangesByFile = summary.rangesByFile || new Map();
    const metaByFile = summary.metaByFile || new Map();

    fileListElement.querySelectorAll(".dataset-row").forEach((row) => {
      const filename = (row.dataset && row.dataset.filename) || null;
      const normalizedName = normalizeRelativePath(filename);
      const hasRanges = normalizedName && rangesByFile.has(normalizedName);
      const meta = normalizedName ? metaByFile.get(normalizedName) : null;
      if (!hasRanges || !meta || (!meta.crossFile && !meta.selfOnly)) {
        return;
      }
      const label = row.querySelector(".file-badge-container") || row.querySelector(".file-entry-name") || row.querySelector("span");
      if (!label) return;
      row.classList.add("has-duplicate-code");
      if (meta.crossFile) {
        const badge = document.createElement("span");
        badge.className = "file-duplicate-badge possible-plagiarism";
        badge.textContent = "Possible Plagiarism";
        badge.title = "Duplicate block references multiple files.";
        label.appendChild(badge);
      }
      if (meta.selfOnly) {
        const badge = document.createElement("span");
        badge.className = "file-duplicate-badge code-duplication";
        badge.textContent = "Code duplication";
        badge.title = "Duplicate block references only this file.";
        label.appendChild(badge);
      }
    });
  }

  async function toggleFileExclusion(filename, excluded) {
    const datasetName = fileListState.dataset || getSelectedDataset();
    if (!datasetName) {
      showMessage(fileListFeedback, "No dataset selected.", true);
      return;
    }
    try {
      const payload = await requestJSON(API_ROUTES.excludedFiles, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ filename, excluded, dataset: datasetName }),
      });
      const excludedList = Array.isArray(payload.excluded_files) ? payload.excluded_files : [];
      const targetDataset = payload.dataset || datasetName;
      setExcludedState(excludedList, targetDataset);
      renderFileList(fileListState.files, fileListState.dataset);
      const actionVerb = excluded ? "Excluded" : "Included";
      showMessage(fileListFeedback, `${actionVerb} ${filename} ${excluded ? "from" : "in"} clustering.`);
    } catch (error) {
      showMessage(fileListFeedback, error.message, true);
      throw error;
    }
  }

  function renderFileList(files = [], datasetName = null) {
    if (!fileListElement) return;

    fileListState.files = files.slice();
    fileListState.dataset = datasetName || null;
    fileListElement.dataset.dataset = datasetName || "";

    if (fileDatasetLabel) {
      fileDatasetLabel.textContent = datasetName || "N/A";
    }

    fileListElement.innerHTML = "";
    if (!files.length) {
      const item = document.createElement("li");
      item.textContent = datasetName ? "No files found in raw folder." : "Select a dataset to list files.";
      fileListElement.appendChild(item);
      void updateFileDuplicateBadges(datasetName);
      return;
    }

    const filteredFiles = applyFileListFilter(files, datasetName);

    if (!filteredFiles.length) {
      const item = document.createElement("li");
      item.textContent = "No files match the selected filter.";
      fileListElement.appendChild(item);
      void updateFileDuplicateBadges(datasetName);
      return;
    }

    filteredFiles.forEach((filename) => {
      const item = document.createElement("li");
      item.className = "dataset-row";
      item.dataset.filename = filename;

      const label = document.createElement("span");
      label.className = "file-entry-name";
      label.textContent = filename;
      const badgeContainer = document.createElement("span");
      badgeContainer.className = "file-badge-container";
      label.appendChild(badgeContainer);

      const excluded = isFileExcluded(filename);
      if (excluded) {
        item.classList.add("file-excluded");
        const excludedBadge = document.createElement("span");
        excludedBadge.className = "file-duplicate-badge file-excluded-badge";
        excludedBadge.textContent = "Excluded from clustering";
        excludedBadge.title = "This file will be ignored by clustering runs.";
        badgeContainer.appendChild(excludedBadge);
      }

      const metrics = getFileMetrics(filename);
      if (metrics) {
        const functionsMetric = (() => {
          const keys = Object.keys(metrics || {});
          for (const key of keys) {
            if (typeof key === "string" && key.trim().toLowerCase() === "functions") {
              const value = Number(metrics[key]);
              if (Number.isFinite(value)) {
                return value;
              }
            }
          }
          return null;
        })();
        if (functionsMetric !== null && functionsMetric === 0) {
          const tag = document.createElement("span");
          tag.className = "file-duplicate-badge file-warning-badge file-no-functions";
          tag.textContent = "No functions";
          tag.title = "No functions detected in this file.";
          // Badge is appended here for files with Functions == 0
          badgeContainer.appendChild(tag);
        }
      }

      const actions = document.createElement("div");
      actions.className = "dataset-row-actions";

      const exclusionToggle = document.createElement("label");
      exclusionToggle.className = "file-exclude-toggle";
      const exclusionCheckbox = document.createElement("input");
      exclusionCheckbox.type = "checkbox";
      exclusionCheckbox.checked = excluded;
      const exclusionLabel = document.createElement("span");
      exclusionLabel.textContent = excluded ? "Excluded" : "Use in clustering";
      exclusionCheckbox.addEventListener("change", async () => {
        const target = exclusionCheckbox.checked;
        exclusionCheckbox.disabled = true;
        try {
          await toggleFileExclusion(filename, target);
        } catch (error) {
          exclusionCheckbox.checked = !target;
        } finally {
          exclusionCheckbox.disabled = false;
          exclusionLabel.textContent = exclusionCheckbox.checked ? "Excluded" : "Use in clustering";
        }
      });
      exclusionToggle.appendChild(exclusionCheckbox);
      exclusionToggle.appendChild(exclusionLabel);
      actions.appendChild(exclusionToggle);

      const displayBtn = document.createElement("button");
      displayBtn.type = "button";
      displayBtn.textContent = "Display";
      displayBtn.addEventListener("click", () => loadFileContent(filename));

      actions.appendChild(displayBtn);
      item.appendChild(label);
      item.appendChild(actions);
      fileListElement.appendChild(item);
    });

    void updateFileDuplicateBadges(datasetName);

    if (datasetName && pendingFilePreview) {
      const datasetMatches =
        !pendingFilePreviewDataset || pendingFilePreviewDataset.toLowerCase() === datasetName.toLowerCase();
      if (datasetMatches) {
        const targetFile = pendingFilePreview;
        pendingFilePreview = null;
        pendingFilePreviewDataset = null;
        loadFileContent(targetFile);
      }
    }
  }

  async function refreshFileList() {
    resetFileExplorerState();
    try {
      const data = await requestJSON(API_ROUTES.listFiles);
      const datasetName = data.dataset || null;
      const files = Array.isArray(data.files) ? data.files : [];
      await Promise.all([loadMetricsForDataset(datasetName), loadExcludedFiles(datasetName)]);
      renderFileList(files, datasetName);
      showMessage(fileListFeedback, `Loaded ${files.length} file(s).`);
    } catch (error) {
      showMessage(fileListFeedback, error.message, true);
      if (fileListElement) {
        fileListElement.innerHTML = "";
        const item = document.createElement("li");
        item.textContent = error.message;
        fileListElement.appendChild(item);
      }
    }
  }

  function showFilePreview(filename, content, highlightRanges = []) {
    if (!filePreviewPanel || !filePreviewContent) return;
    if (filePreviewName) {
      filePreviewName.textContent = filename;
    }
    const lines = typeof content === "string" ? content.split(/\r?\n/) : [];
    if (!lines.length) {
      lines.push("");
    }
    const mergedHighlights = mergeLineRanges(highlightRanges);
    const lineNumbersElement = document.getElementById("file-line-numbers");
    const codeElement = document.createElement("code");
    if (lineNumbersElement) {
      lineNumbersElement.innerHTML = "";
    }
    filePreviewContent.innerHTML = "";
    let highlightIndex = 0;
    for (let i = 0; i < lines.length; i += 1) {
      const lineNumber = i + 1;
      while (highlightIndex < mergedHighlights.length && mergedHighlights[highlightIndex].endLine < lineNumber) {
        highlightIndex += 1;
      }
      const currentRange = mergedHighlights[highlightIndex];
      const isHighlighted =
        currentRange && lineNumber >= currentRange.startLine && lineNumber <= currentRange.endLine;
      if (lineNumbersElement) {
        const lineDiv = document.createElement("div");
        if (isHighlighted) {
          lineDiv.classList.add("highlight");
        }
        lineDiv.textContent = lineNumber.toString();
        lineNumbersElement.appendChild(lineDiv);
      }
      const lineSpan = document.createElement("span");
      lineSpan.className = "file-preview-line";
      if (isHighlighted) {
        lineSpan.classList.add("highlight");
      }
      lineSpan.textContent = lines[i] || " ";
      codeElement.appendChild(lineSpan);
      if (i < lines.length - 1) {
        codeElement.appendChild(document.createTextNode("\n"));
      }
    }
    filePreviewContent.appendChild(codeElement);
    if (fileExplorerWrapper) {
      fileExplorerWrapper.classList.add("show-preview");
    }
  }

  async function loadFileContent(filename) {
    if (!filename) return;
    showMessage(fileListFeedback, `Loading ${filename}...`);
    try {
      const params = new URLSearchParams({ filename });
      const selectedDataset = getSelectedDataset();
      if (selectedDataset) {
        params.set("dataset", selectedDataset);
      }
      const data = await requestJSON(`${API_ROUTES.fileContent}?${params.toString()}`);
      const highlightRanges = await getDuplicateRangesForFile(data.dataset, data.filename);
      showFilePreview(data.filename, data.content, highlightRanges);
      showMessage(fileListFeedback, `Displaying ${data.filename}.`);
    } catch (error) {
      showMessage(fileListFeedback, error.message, true);
    }
  }

  async function ensureDatasetFromParam() {
    if (!fileExplorerWrapper || !initialExplorerDatasetParam || !datasetSelect) {
      return;
    }
    if (datasetSelect.value && datasetSelect.value === initialExplorerDatasetParam) {
      return;
    }
    try {
      await setCurrentDataset(initialExplorerDatasetParam);
    } catch (error) {
      console.warn(error);
    }
  }

  onReady(() => {
    if (fileListFilterSelect) {
      fileListFilterSelect.addEventListener("change", () => {
        renderFileList(fileListState.files, fileListState.dataset);
      });
    }
    if (refreshFilesButton) {
      refreshFilesButton.addEventListener("click", refreshFileList);
    }
    void ensureDatasetFromParam().then(refreshFileList);
  });
})();
