const API_ROUTES = {
  datasets: "/datasets",
  currentDataset: "/current-dataset",
  listFiles: "/datasets/files",
  fileContent: "/datasets/file",
  fileAnalysis: "/datasets/file/lizard",
  globalMetrics: "/datasets/metrics/global",
};

const datasetSelect = document.getElementById("current-dataset-select");
const datasetListElement = document.getElementById("dataset-list");
const datasetFeedbackElement = document.getElementById("dataset-feedback");
const currentDatasetDisplay = document.getElementById("current-dataset-display");
const uploadForm = document.getElementById("dataset-upload-form");
const uploadStatusElement = document.getElementById("dataset-upload-status");
const fileExplorerWrapper = document.getElementById("file-explorer");
const fileListElement = document.getElementById("file-list");
const fileListFeedback = document.getElementById("file-list-feedback");
const fileDatasetLabel = document.getElementById("file-list-dataset");
const filePreviewPanel = document.getElementById("file-preview-panel");
const filePreviewName = document.getElementById("file-preview-name");
const filePreviewContent = document.getElementById("file-content");
const refreshFilesButton = document.getElementById("refresh-files-button");
const plagiarismContainer = document.getElementById("plagiarism-viewer");
const plagiarismPrevButton = document.getElementById("plagiarism-prev");
const plagiarismNextButton = document.getElementById("plagiarism-next");
const plagiarismCounter = document.getElementById("plagiarism-counter");
const globalAnalysisPanel = document.getElementById("global-analysis");
const globalAnalysisMetricSelect = document.getElementById("global-analysis-metric");
const globalAnalysisChart = document.getElementById("global-analysis-chart");
const globalAnalysisFeedback = document.getElementById("global-analysis-feedback");
const globalAnalysisDatasetLabel = document.getElementById("global-analysis-dataset");

const DEFAULT_LIZARD_SUMMARY = "lizard_dataset.xml";
const urlParams = new URLSearchParams(window.location.search || "");
const initialExplorerDatasetParam = urlParams.get("dataset");
const initialExplorerFileParam = urlParams.get("file") || urlParams.get("filename");
let pendingFilePreview = initialExplorerFileParam;
let pendingFilePreviewDataset = initialExplorerDatasetParam;

const plagiarismState = {
  blocks: [],
  index: 0,
  dataset: plagiarismContainer ? plagiarismContainer.dataset.dataset : null,
  summaryFilename: plagiarismContainer ? plagiarismContainer.dataset.summaryFilename : DEFAULT_LIZARD_SUMMARY,
  snippetCache: new Map(),
};

const duplicateSummaryCache = new Map();
const globalAnalysisState = {
  dataset: null,
  metrics: [],
  files: [],
  metricKey: null,
};

async function requestJSON(url, options = {}) {
  const response = await fetch(url, options);
  const contentType = response.headers.get("content-type") || "";
  const payload = contentType.includes("application/json") ? await response.json() : {};
  if (!response.ok) {
    const detail = payload.detail || payload.message || response.statusText;
    throw new Error(detail);
  }
  return payload;
}

function showMessage(element, text, isError = false) {
  if (!element) return;
  element.textContent = text || "";
  element.classList.toggle("error", Boolean(isError && text));
  element.classList.toggle("success", Boolean(!isError && text));
}

function updateDatasetSelect(datasets = [], currentDataset = null) {
  if (!datasetSelect) return;
  const valueToSet = currentDataset ?? "";
  datasetSelect.innerHTML = "";

  const placeholder = document.createElement("option");
  placeholder.value = "";
  placeholder.textContent = "No dataset";
  datasetSelect.appendChild(placeholder);

  datasets.forEach((name) => {
    const option = document.createElement("option");
    option.value = name;
    option.textContent = name;
    datasetSelect.appendChild(option);
  });

  datasetSelect.value = valueToSet;
}

function updateCurrentDatasetDisplay(currentDataset) {
  if (currentDatasetDisplay) {
    currentDatasetDisplay.textContent = currentDataset || "None";
  }
}

function renderDatasetList(datasets = []) {
  if (!datasetListElement) return;

  datasetListElement.innerHTML = "";
  if (!datasets.length) {
    const empty = document.createElement("li");
    empty.textContent = "No datasets yet.";
    datasetListElement.appendChild(empty);
    return;
  }

  datasets.forEach((name) => {
    const item = document.createElement("li");
    item.className = "dataset-row";

    const label = document.createElement("span");
    label.textContent = name;
    item.appendChild(label);

    const actions = document.createElement("div");
    actions.className = "dataset-row-actions";

    const selectBtn = document.createElement("button");
    selectBtn.type = "button";
    selectBtn.textContent = "Select";
    selectBtn.addEventListener("click", () => setCurrentDataset(name));
    actions.appendChild(selectBtn);

    const deleteBtn = document.createElement("button");
    deleteBtn.type = "button";
    deleteBtn.classList.add("danger");
    deleteBtn.textContent = "Delete";
    deleteBtn.addEventListener("click", () => deleteDataset(name));
    actions.appendChild(deleteBtn);

    item.appendChild(actions);
    datasetListElement.appendChild(item);
  });
}

function renderDatasetData(data) {
  const datasets = data.datasets || [];
  const current = data.current_dataset || null;
  updateDatasetSelect(datasets, current);
  updateCurrentDatasetDisplay(current);
  renderDatasetList(datasets);
}

async function refreshDatasets() {
  try {
    const data = await requestJSON(API_ROUTES.datasets);
    renderDatasetData(data);
  } catch (error) {
    console.error(error);
    showMessage(datasetFeedbackElement, error.message, true);
  }
}

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

function renderFileList(files = [], dataset = null) {
  if (!fileListElement) return;

  fileListElement.dataset.dataset = dataset || "";

  if (fileDatasetLabel) {
    fileDatasetLabel.textContent = dataset || "N/A";
  }

  fileListElement.innerHTML = "";
  if (!files.length) {
    const item = document.createElement("li");
    item.textContent = dataset ? "No files found in raw folder." : "Select a dataset to list files.";
    fileListElement.appendChild(item);
    void updateFileDuplicateBadges(dataset);
    return;
  }

  files.forEach((filename) => {
    const item = document.createElement("li");
    item.className = "dataset-row";
    item.dataset.filename = filename;

    const label = document.createElement("span");
    label.className = "file-entry-name";
    label.textContent = filename;

    const actions = document.createElement("div");
    actions.className = "dataset-row-actions";

    const displayBtn = document.createElement("button");
    displayBtn.type = "button";
    displayBtn.textContent = "Display";
    displayBtn.addEventListener("click", () => loadFileContent(filename));

    actions.appendChild(displayBtn);
    item.appendChild(label);
    item.appendChild(actions);
    fileListElement.appendChild(item);
  });

  void updateFileDuplicateBadges(dataset);

  if (dataset && pendingFilePreview) {
    const datasetMatches =
      !pendingFilePreviewDataset || pendingFilePreviewDataset.toLowerCase() === dataset.toLowerCase();
    if (datasetMatches) {
      const targetFile = pendingFilePreview;
      pendingFilePreview = null;
      pendingFilePreviewDataset = null;
      loadFileContent(targetFile);
    }
  }
}

async function refreshFileList() {
  if (!fileListElement) return;
  resetFileExplorerState();
  try {
    const data = await requestJSON(API_ROUTES.listFiles);
    renderFileList(data.files || [], data.dataset);
    showMessage(fileListFeedback, `Loaded ${data.files.length} file(s).`);
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
    while (
      highlightIndex < mergedHighlights.length &&
      mergedHighlights[highlightIndex].endLine < lineNumber
    ) {
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
    if (datasetSelect && datasetSelect.value) {
      params.set("dataset", datasetSelect.value);
    }
    const data = await requestJSON(`${API_ROUTES.fileContent}?${params.toString()}`);
    const highlightRanges = await getDuplicateRangesForFile(data.dataset, data.filename);
    showFilePreview(data.filename, data.content, highlightRanges);
    showMessage(fileListFeedback, `Displaying ${data.filename}.`);
  } catch (error) {
    showMessage(fileListFeedback, error.message, true);
  }
}

function getActiveDatasetForPlagiarism() {
  if (!plagiarismContainer) return null;
  const selectValue = datasetSelect && datasetSelect.value ? datasetSelect.value : null;
  if (selectValue) return selectValue;
  if (plagiarismContainer.dataset.dataset) return plagiarismContainer.dataset.dataset;
  return plagiarismState.dataset || null;
}

function parseDuplicateBlocks(report) {
  const lines = (report || "").split(/\r?\n/);
  const blocks = [];
  let i = 0;
  while (i < lines.length) {
    if (lines[i].trim() === "Duplicate block:") {
      i += 1;
      while (i < lines.length && !lines[i].trim()) i += 1;
      if (i < lines.length && lines[i].includes("-")) {
        i += 1;
      }
      const entries = [];
      while (i < lines.length) {
        const line = lines[i].trim();
        if (!line) {
          i += 1;
          continue;
        }
        if (line.startsWith("^")) {
          i += 1;
          break;
        }
        const match = line.match(/^(.*?):\s*(\d+)\s*~\s*(\d+)/);
        if (match) {
          const filePath = match[1].trim();
          const relativePath = extractRelativePath(filePath);
          entries.push({
            filePath,
            relativePath,
            startLine: Number(match[2]),
            endLine: Number(match[3]),
          });
        }
        i += 1;
      }
      if (entries.length > 1) {
        blocks.push({ entries });
      }
    } else {
      i += 1;
    }
  }
  return blocks;
}

function extractRelativePath(fullPath) {
  if (!fullPath) return null;
  const marker = "/raw/";
  const idx = fullPath.lastIndexOf(marker);
  if (idx !== -1) {
    return fullPath.slice(idx + marker.length);
  }
  const winMarker = "\\raw\\";
  const winIdx = fullPath.lastIndexOf(winMarker);
  if (winIdx !== -1) {
    return fullPath.slice(winIdx + winMarker.length).replace(/\\/g, "/");
  }
  return fullPath;
}

function blockHasMultipleFiles(block) {
  if (!block) return false;
  const files = new Set();
  (block.entries || []).forEach((entry) => {
    const relative = normalizeRelativePath(entry && (entry.relativePath || extractRelativePath(entry.filePath)));
    if (relative) {
      files.add(relative);
    }
  });
  return files.size > 1;
}

function filterCrossFileBlocks(blocks = []) {
  return (blocks || []).filter((block) => blockHasMultipleFiles(block));
}

function normalizeRelativePath(path) {
  if (!path) return "";
  return path.replace(/\\/g, "/");
}

function getPreferredSummaryFilename() {
  if (plagiarismState.summaryFilename) {
    return plagiarismState.summaryFilename;
  }
  if (plagiarismContainer && plagiarismContainer.dataset.summaryFilename) {
    return plagiarismContainer.dataset.summaryFilename;
  }
  return DEFAULT_LIZARD_SUMMARY;
}

function duplicateSummaryCacheKey(dataset, summaryFilename) {
  return `${dataset}::${summaryFilename || DEFAULT_LIZARD_SUMMARY}`;
}

function buildDuplicateIndex(blocks = []) {
  const rangesByFile = new Map();
  const metaByFile = new Map();
  (blocks || []).forEach((block) => {
    const fileSet = new Set();
    (block.entries || []).forEach((entry) => {
      if (!entry) return;
      const relativeCandidate = entry.relativePath || extractRelativePath(entry.filePath);
      const relativePath = normalizeRelativePath(relativeCandidate);
      if (relativePath) {
        fileSet.add(relativePath);
      }
    });
    const isCrossFile = fileSet.size > 1;
    const isSingleFileBlock = fileSet.size === 1;
    (block.entries || []).forEach((entry) => {
      if (!entry) return;
      const relativeCandidate = entry.relativePath || extractRelativePath(entry.filePath);
      const relativePath = normalizeRelativePath(relativeCandidate);
      if (!relativePath) {
        return;
      }
      const startLine = Number(entry.startLine);
      const endLine = Number(entry.endLine);
      if (Number.isFinite(startLine) && Number.isFinite(endLine)) {
        if (!rangesByFile.has(relativePath)) {
          rangesByFile.set(relativePath, []);
        }
        rangesByFile.get(relativePath).push({
          startLine,
          endLine,
        });
      }
      if (!metaByFile.has(relativePath)) {
        metaByFile.set(relativePath, { crossFile: false, selfOnly: false });
      }
      const meta = metaByFile.get(relativePath);
      if (isCrossFile) {
        meta.crossFile = true;
      } else if (isSingleFileBlock) {
        meta.selfOnly = true;
      }
    });
  });
  return { rangesByFile, metaByFile };
}

async function requestDuplicateSummary(dataset, summaryFilename) {
  const params = new URLSearchParams({
    dataset,
    summary: "true",
    filename: summaryFilename,
  });
  const data = await requestJSON(`${API_ROUTES.fileAnalysis}?${params.toString()}`);
  const blocks = parseDuplicateBlocks(data.analysis || "");
  const { rangesByFile, metaByFile } = buildDuplicateIndex(blocks);
  return {
    dataset,
    filename: data.filename,
    blocks,
    rangesByFile,
    metaByFile,
  };
}

async function getDuplicateSummary(dataset) {
  if (!dataset) return null;
  const summaryFilename = getPreferredSummaryFilename();
  const cacheKey = duplicateSummaryCacheKey(dataset, summaryFilename);
  if (!duplicateSummaryCache.has(cacheKey)) {
    duplicateSummaryCache.set(cacheKey, requestDuplicateSummary(dataset, summaryFilename));
  }
  try {
    return await duplicateSummaryCache.get(cacheKey);
  } catch (error) {
    duplicateSummaryCache.delete(cacheKey);
    throw error;
  }
}

function getDuplicateRangesFromSummary(summary, filename) {
  if (!summary || !summary.rangesByFile || !filename) return [];
  const normalizedName = normalizeRelativePath(filename);
  if (!normalizedName) return [];
  const entries = summary.rangesByFile.get(normalizedName);
  if (!entries || !entries.length) return [];
  return mergeLineRanges(entries);
}

async function getDuplicateRangesForFile(dataset, filename) {
  if (!dataset || !filename) return [];
  try {
    const summary = await getDuplicateSummary(dataset);
    return getDuplicateRangesFromSummary(summary, filename);
  } catch (error) {
    if (error && typeof error.message === "string" && error.message.toLowerCase().includes("not found")) {
      return [];
    }
    console.warn(`Unable to load duplicate ranges for ${filename}:`, error);
    return [];
  }
}

async function updateFileDuplicateBadges(dataset) {
  if (!fileListElement) return;
  fileListElement.querySelectorAll(".file-duplicate-badge").forEach((badge) => badge.remove());
  fileListElement.querySelectorAll(".dataset-row").forEach((row) => row.classList.remove("has-duplicate-code"));
  if (!dataset) return;

  const expectedDataset = dataset || "";
  let summary = null;
  try {
    summary = await getDuplicateSummary(dataset);
  } catch (error) {
    if (!(error && typeof error.message === "string" && error.message.toLowerCase().includes("not found"))) {
      console.warn(`Unable to load duplicate summary for dataset "${dataset}":`, error);
    }
    return;
  }
  if (!summary || (fileListElement.dataset.dataset || "") !== expectedDataset) {
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
    const label = row.querySelector(".file-entry-name") || row.querySelector("span");
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

function updateGlobalAnalysisMetricOptions(metrics = []) {
  if (!globalAnalysisMetricSelect) return;
  globalAnalysisMetricSelect.innerHTML = "";
  if (!metrics.length) {
    const option = document.createElement("option");
    option.value = "";
    option.textContent = "No metrics available";
    globalAnalysisMetricSelect.appendChild(option);
    globalAnalysisMetricSelect.disabled = true;
    return;
  }
  metrics.forEach((metric) => {
    const option = document.createElement("option");
    option.value = metric.key;
    option.textContent = metric.label || metric.key;
    globalAnalysisMetricSelect.appendChild(option);
  });
  globalAnalysisMetricSelect.disabled = false;
  const activeKey = globalAnalysisState.metricKey || metrics[0].key;
  globalAnalysisMetricSelect.value = activeKey;
  globalAnalysisState.metricKey = activeKey;
}

function getMetricDefinition(metricKey) {
  if (!metricKey) return null;
  return (globalAnalysisState.metrics || []).find((metric) => metric.key === metricKey) || null;
}

function getMetricAverage(metricKey) {
  const definition = getMetricDefinition(metricKey);
  if (!definition) return null;
  const value = definition.average;
  return Number.isFinite(value) ? value : null;
}

function computeStandardDeviation(values = [], mean = null) {
  if (!values.length) return null;
  const actualMean =
    Number.isFinite(mean) && mean !== null
      ? mean
      : values.reduce((sum, value) => sum + value, 0) / values.length;
  const variance =
    values.reduce((sum, value) => sum + (value - actualMean) ** 2, 0) / values.length;
  if (!Number.isFinite(variance)) return null;
  return Math.sqrt(variance);
}

function clearGlobalAnalysisChart() {
  if (!globalAnalysisChart) return;
  if (globalAnalysisChart.__plotlyClickHandler && typeof globalAnalysisChart.removeListener === "function") {
    globalAnalysisChart.removeListener("plotly_click", globalAnalysisChart.__plotlyClickHandler);
    globalAnalysisChart.__plotlyClickHandler = null;
  }
  if (window.Plotly) {
    Plotly.purge(globalAnalysisChart);
  } else {
    globalAnalysisChart.innerHTML = "";
  }
}

function renderGlobalAnalysisChart() {
  if (!globalAnalysisChart || !globalAnalysisState.metricKey) {
    clearGlobalAnalysisChart();
    return;
  }
  if (!window.Plotly) {
    showMessage(globalAnalysisFeedback, "Plotly library failed to load.", true);
    return;
  }
  const metricKey = globalAnalysisState.metricKey;
  const points = (globalAnalysisState.files || [])
    .map((file, index) => {
      const metrics = file.metrics || {};
      const value = Number(metrics[metricKey]);
      if (!Number.isFinite(value)) {
        return null;
      }
      return {
        x: index + 1,
        y: value,
        filename: file.path || file.raw_path || `File ${index + 1}`,
      };
    })
    .filter(Boolean);
  if (!points.length) {
    clearGlobalAnalysisChart();
    showMessage(globalAnalysisFeedback, `No values available for ${metricKey}.`, true);
    return;
  }
  const trace = {
    x: points.map((point) => point.x),
    y: points.map((point) => point.y),
    type: "scatter",
    mode: "markers",
  };
  const values = points.map((point) => point.y);
  const metricAverage = getMetricAverage(metricKey);
  const stddev = computeStandardDeviation(values, metricAverage);
  const hasAverage = Number.isFinite(metricAverage);
  const stddevValid = Number.isFinite(stddev) && stddev > 0;
  const upperThreshold =
    hasAverage && stddevValid ? metricAverage + stddev * 2 : hasAverage ? metricAverage : null;
  const lowerThreshold =
    hasAverage && stddevValid ? metricAverage - stddev * 2 : hasAverage ? metricAverage : null;
  const upperBand =
    hasAverage && stddevValid ? metricAverage + stddev : hasAverage ? metricAverage : null;
  const lowerBand =
    hasAverage && stddevValid ? metricAverage - stddev : hasAverage ? metricAverage : null;
  const pointStatuses = points.map((point) => {
    if (Number.isFinite(upperThreshold) && point.y >= upperThreshold) {
      return "High outlier";
    }
    if (Number.isFinite(lowerThreshold) && point.y <= lowerThreshold) {
      return "Low outlier";
    }
    if (Number.isFinite(upperBand) && point.y >= upperBand) {
      return "High band";
    }
    if (Number.isFinite(lowerBand) && point.y <= lowerBand) {
      return "Low band";
    }
    return "Normal";
  });
  const colors = pointStatuses.map((status) => {
    if (status === "High outlier") return "#dc2626";
    if (status === "Low outlier") return "#1d4ed8";
    if (status === "High band") return "#fb923c";
    if (status === "Low band") return "#60a5fa";
    return "#2563eb";
  });
  trace.marker = {
    size: 10,
    color: colors,
    opacity: 0.9,
    line: {
      width: 1,
      color: pointStatuses.map((status) => {
        if (status === "High outlier") return "#991b1b";
        if (status === "Low outlier") return "#1e40af";
        if (status === "High band") return "#c2410c";
        if (status === "Low band") return "#2563eb";
        return "#1d4ed8";
      }),
    },
  };
  trace.customdata = points.map((point, index) => [point.filename, pointStatuses[index]]);
  trace.hovertemplate = `<b>%{customdata[0]}</b><br>${metricKey}: %{y}<br>Status: %{customdata[1]}<extra></extra>`;

  const shapes = [];
  if (hasAverage) {
    shapes.push({
      type: "line",
      xref: "paper",
      x0: 0,
      x1: 1,
      y0: metricAverage,
      y1: metricAverage,
      line: {
        color: "#94a3b8",
        dash: "dot",
        width: 2,
      },
    });
  }
  if (Number.isFinite(upperThreshold) && (!hasAverage || upperThreshold !== metricAverage)) {
    shapes.push({
      type: "line",
      xref: "paper",
      x0: 0,
      x1: 1,
      y0: upperThreshold,
      y1: upperThreshold,
      line: {
        color: "#dc2626",
        dash: "dash",
        width: 1.5,
      },
    });
  }
  if (
    Number.isFinite(lowerThreshold) &&
    (!hasAverage || lowerThreshold !== metricAverage) &&
    lowerThreshold !== upperThreshold
  ) {
    shapes.push({
      type: "line",
      xref: "paper",
      x0: 0,
      x1: 1,
      y0: lowerThreshold,
      y1: lowerThreshold,
      line: {
        color: "#1d4ed8",
        dash: "dash",
        width: 1.5,
      },
    });
  }
  if (Number.isFinite(upperBand) && (!hasAverage || upperBand !== metricAverage)) {
    shapes.push({
      type: "line",
      xref: "paper",
      x0: 0,
      x1: 1,
      y0: upperBand,
      y1: upperBand,
      line: {
        color: "#fb923c",
        dash: "dot",
        width: 1,
      },
    });
  }
  if (
    Number.isFinite(lowerBand) &&
    (!hasAverage || lowerBand !== metricAverage) &&
    lowerBand !== upperBand
  ) {
    shapes.push({
      type: "line",
      xref: "paper",
      x0: 0,
      x1: 1,
      y0: lowerBand,
      y1: lowerBand,
      line: {
        color: "#60a5fa",
        dash: "dot",
        width: 1,
      },
    });
  }
  const layout = {
    margin: { l: 60, r: 20, t: 30, b: 40 },
    xaxis: {
      title: "File index",
      zeroline: false,
      showgrid: false,
    },
    yaxis: {
      title: metricKey,
      rangemode: "tozero",
    },
    hovermode: "closest",
    showlegend: false,
    shapes,
  };
  Plotly.react(globalAnalysisChart, [trace], layout, { responsive: true, displaylogo: false });
  if (globalAnalysisChart.__plotlyClickHandler && typeof globalAnalysisChart.removeListener === "function") {
    globalAnalysisChart.removeListener("plotly_click", globalAnalysisChart.__plotlyClickHandler);
  }
  if (typeof globalAnalysisChart.on === "function") {
    const clickHandler = (event) => {
      if (!event || !event.points || !event.points.length) {
        return;
      }
      const point = event.points[0];
      const filename = Array.isArray(point.customdata) ? point.customdata[0] : point.customdata;
      openFileInExplorer(filename);
    };
    globalAnalysisChart.on("plotly_click", clickHandler);
    globalAnalysisChart.__plotlyClickHandler = clickHandler;
  }
}

async function refreshGlobalAnalysis() {
  if (!globalAnalysisPanel) return;
  const dataset = datasetSelect && datasetSelect.value ? datasetSelect.value : null;
  if (globalAnalysisDatasetLabel) {
    globalAnalysisDatasetLabel.textContent = dataset || "None";
  }
  if (!dataset) {
    globalAnalysisState.dataset = null;
    globalAnalysisState.metrics = [];
    globalAnalysisState.files = [];
    globalAnalysisState.metricKey = null;
    updateGlobalAnalysisMetricOptions([]);
    clearGlobalAnalysisChart();
    showMessage(globalAnalysisFeedback, "Select a dataset to load the analysis.");
    return;
  }
  showMessage(globalAnalysisFeedback, "Loading metrics...");
  try {
    const params = new URLSearchParams({ dataset });
    const data = await requestJSON(`${API_ROUTES.globalMetrics}?${params.toString()}`);
    globalAnalysisState.dataset = data.dataset || dataset;
    globalAnalysisState.metrics = data.metrics || [];
    globalAnalysisState.files = data.files || [];
    if (!globalAnalysisState.metrics.length || !globalAnalysisState.files.length) {
      globalAnalysisState.metricKey = null;
      updateGlobalAnalysisMetricOptions([]);
      clearGlobalAnalysisChart();
      showMessage(globalAnalysisFeedback, "No file metrics available.", true);
      return;
    }
    if (
      !globalAnalysisState.metricKey ||
      !globalAnalysisState.metrics.some((metric) => metric.key === globalAnalysisState.metricKey)
    ) {
      globalAnalysisState.metricKey = globalAnalysisState.metrics[0].key;
    }
    updateGlobalAnalysisMetricOptions(globalAnalysisState.metrics);
    renderGlobalAnalysisChart();
    showMessage(globalAnalysisFeedback, `Loaded ${globalAnalysisState.files.length} file(s).`);
  } catch (error) {
    globalAnalysisState.metrics = [];
    globalAnalysisState.files = [];
    globalAnalysisState.metricKey = null;
    updateGlobalAnalysisMetricOptions([]);
    clearGlobalAnalysisChart();
    showMessage(globalAnalysisFeedback, error.message, true);
  }
}

function openFileInExplorer(filename) {
  if (!filename) return;
  const params = new URLSearchParams({ file: filename });
  const dataset = datasetSelect && datasetSelect.value ? datasetSelect.value : null;
  if (dataset) {
    params.set("dataset", dataset);
  }
  window.location.href = `/file-explorer?${params.toString()}`;
}

function mergeLineRanges(ranges = []) {
  const normalized = (ranges || [])
    .map((range) => {
      if (!range) return null;
      const startValue = Number(range.startLine ?? range.start ?? range.line);
      const endValue = Number(range.endLine ?? range.end ?? range.line);
      if (!Number.isFinite(startValue) || !Number.isFinite(endValue)) {
        return null;
      }
      const startLine = Math.max(1, Math.min(startValue, endValue));
      const endLine = Math.max(startLine, Math.max(startValue, endValue));
      return { startLine, endLine };
    })
    .filter(Boolean)
    .sort((a, b) => a.startLine - b.startLine);

  const merged = [];
  normalized.forEach((range) => {
    const last = merged[merged.length - 1];
    if (!last || range.startLine > last.endLine + 1) {
      merged.push({ ...range });
    } else {
      last.endLine = Math.max(last.endLine, range.endLine);
    }
  });

  return merged;
}

function showPlagiarismEmptyState(message) {
  if (!plagiarismContainer) return;
  plagiarismContainer.innerHTML = "";
  const wrapper = document.createElement("div");
  wrapper.className = "plagiarism-viewer-empty";
  wrapper.textContent = message;
  plagiarismContainer.appendChild(wrapper);
}

function updatePlagiarismControls() {
  if (!plagiarismContainer) return;
  const total = plagiarismState.blocks.length;
  if (plagiarismCounter) {
    const current = total ? Math.min(plagiarismState.index + 1, total) : 0;
    plagiarismCounter.textContent = `${current} / ${total}`;
  }
  if (plagiarismPrevButton) {
    plagiarismPrevButton.disabled = total <= 1 || plagiarismState.index === 0;
  }
  if (plagiarismNextButton) {
    plagiarismNextButton.disabled = total <= 1 || plagiarismState.index >= total - 1;
  }
}

async function fetchPlagiarismData() {
  if (!plagiarismContainer) return;
  const dataset = getActiveDatasetForPlagiarism();
  if (!dataset) {
    plagiarismState.blocks = [];
    plagiarismState.snippetCache.clear();
    updatePlagiarismControls();
    showPlagiarismEmptyState("Select a dataset to inspect duplicate blocks.");
    return;
  }
  plagiarismState.dataset = dataset;
  plagiarismContainer.dataset.dataset = dataset;
  plagiarismState.snippetCache.clear();
  showPlagiarismEmptyState("Loading duplicate blocks…");
  try {
    const summary = await getDuplicateSummary(dataset);
    const blocks = summary ? filterCrossFileBlocks(summary.blocks) : [];
    plagiarismState.blocks = blocks;
    plagiarismState.index = 0;
    updatePlagiarismControls();
    if (!blocks.length) {
      showPlagiarismEmptyState("No duplicate blocks detected.");
      return;
    }
    await renderPlagiarismBlock();
  } catch (error) {
    plagiarismState.blocks = [];
    updatePlagiarismControls();
    showPlagiarismEmptyState(error.message);
  }
}

async function getFileContentCached(dataset, relativePath) {
  if (!relativePath) throw new Error("Missing file path in duplicate block.");
  const key = `${dataset}:${relativePath}`;
  if (!plagiarismState.snippetCache.has(key)) {
    const params = new URLSearchParams({ filename: relativePath });
    if (dataset) params.set("dataset", dataset);
    const promise = requestJSON(`${API_ROUTES.fileContent}?${params.toString()}`);
    plagiarismState.snippetCache.set(key, promise);
  }
  const payload = await plagiarismState.snippetCache.get(key);
  return payload.content || "";
}

function buildSnippetLines(content, start, end) {
  const lines = content.split(/\r?\n/);
  const before = 10;
  const after = 10;
  const snippetStart = Math.max(start - before, 1);
  const snippetEnd = Math.min(end + after, lines.length);
  const snippetLines = [];
  for (let line = snippetStart; line <= snippetEnd; line += 1) {
    snippetLines.push({
      number: line,
      text: lines[line - 1] ?? "",
      highlight: line >= start && line <= end,
    });
  }
  return snippetLines;
}

function buildSnippetElement(entry, snippetLines) {
  const wrapper = document.createElement("div");
  wrapper.className = "plagiarism-file";

  const title = document.createElement("h3");
  title.textContent = `${entry.relativePath} (${entry.startLine} ~ ${entry.endLine})`;
  wrapper.appendChild(title);

  const snippet = document.createElement("div");
  snippet.className = "plagiarism-snippet";
  if (!snippetLines.length) {
    const empty = document.createElement("div");
    empty.textContent = "Unable to render snippet.";
    snippet.appendChild(empty);
  } else {
    snippetLines.forEach((line) => {
      const lineRow = document.createElement("div");
      lineRow.className = "plagiarism-snippet-line";

      const lineNumber = document.createElement("span");
      lineNumber.className = "plagiarism-line-number";
      lineNumber.textContent = line.number.toString().padStart(4, " ");

      const lineText = document.createElement("span");
      lineText.className = "plagiarism-line-text";
      if (line.highlight) {
        lineText.classList.add("highlight");
      }
      lineText.textContent = line.text || " ";

      lineRow.appendChild(lineNumber);
      lineRow.appendChild(lineText);
      snippet.appendChild(lineRow);
    });
  }

  wrapper.appendChild(snippet);
  return wrapper;
}

async function renderPlagiarismBlock() {
  if (!plagiarismContainer) return;
  const total = plagiarismState.blocks.length;
  if (!total) {
    showPlagiarismEmptyState("No duplicate blocks detected.");
    return;
  }
  const index = Math.min(plagiarismState.index, total - 1);
  const block = plagiarismState.blocks[index];
  plagiarismContainer.innerHTML = "";
  const grid = document.createElement("div");
  grid.className = "plagiarism-grid";
  try {
    const panels = await Promise.all(
      block.entries.map(async (entry) => {
        const content = await getFileContentCached(plagiarismState.dataset, entry.relativePath);
        const snippetLines = buildSnippetLines(content, entry.startLine, entry.endLine);
        return buildSnippetElement(entry, snippetLines);
      }),
    );
    panels.forEach((panel) => grid.appendChild(panel));
    plagiarismContainer.appendChild(grid);
  } catch (error) {
    showPlagiarismEmptyState(error.message);
  }
}

async function refreshPlagiarismView() {
  if (!plagiarismContainer) return;
  await fetchPlagiarismData();
}

function initPlagiarismViewer() {
  if (!plagiarismContainer) return;
  if (plagiarismPrevButton) {
    plagiarismPrevButton.addEventListener("click", async () => {
      if (plagiarismState.index > 0) {
        plagiarismState.index -= 1;
        updatePlagiarismControls();
        await renderPlagiarismBlock();
      }
    });
  }
  if (plagiarismNextButton) {
    plagiarismNextButton.addEventListener("click", async () => {
      if (plagiarismState.index < plagiarismState.blocks.length - 1) {
        plagiarismState.index += 1;
        updatePlagiarismControls();
        await renderPlagiarismBlock();
      }
    });
  }
  refreshPlagiarismView();
}

async function handleUpload(event) {
  event.preventDefault();
  if (!uploadForm) return;

  const formData = new FormData(uploadForm);
  showMessage(uploadStatusElement, "Uploading...");

  try {
    const data = await requestJSON(API_ROUTES.datasets, {
      method: "POST",
      body: formData,
    });
    renderDatasetData(data);
    showMessage(uploadStatusElement, `Dataset "${data.dataset}" created.`);
    uploadForm.reset();
  } catch (error) {
    showMessage(uploadStatusElement, error.message, true);
  }
}

async function deleteDataset(name) {
  if (!window.confirm(`Delete dataset "${name}"?`)) {
    return;
  }

  try {
    const data = await requestJSON(`${API_ROUTES.datasets}/${encodeURIComponent(name)}`, {
      method: "DELETE",
    });
    renderDatasetData(data);
    showMessage(datasetFeedbackElement, `Dataset "${name}" deleted.`);
  } catch (error) {
    showMessage(datasetFeedbackElement, error.message, true);
  }
}

async function setCurrentDataset(name) {
  try {
    const data = await requestJSON(API_ROUTES.currentDataset, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ dataset_name: name || null }),
    });
    updateCurrentDatasetDisplay(data.current_dataset);
    await refreshDatasets();
    if (fileListElement) {
      await refreshFileList();
    }
    if (plagiarismContainer) {
      await refreshPlagiarismView();
    }
    if (globalAnalysisPanel) {
      await refreshGlobalAnalysis();
    }
    showMessage(datasetFeedbackElement, `Current dataset: ${data.current_dataset || "None"}.`);
  } catch (error) {
    showMessage(datasetFeedbackElement, error.message, true);
    refreshDatasets();
    if (fileListElement) {
      refreshFileList();
    }
    if (plagiarismContainer) {
      refreshPlagiarismView();
    }
    if (globalAnalysisPanel) {
      refreshGlobalAnalysis();
    }
  }
}

function initDatasetControls() {
  if (uploadForm) {
    uploadForm.addEventListener("submit", handleUpload);
  }

  if (datasetSelect) {
    datasetSelect.addEventListener("change", (event) => {
      setCurrentDataset(event.target.value || null);
    });
  }

  if (globalAnalysisMetricSelect) {
    globalAnalysisMetricSelect.addEventListener("change", (event) => {
      globalAnalysisState.metricKey = event.target.value || null;
      renderGlobalAnalysisChart();
    });
  }

  if (datasetSelect || datasetListElement) {
    refreshDatasets();
  }

  if (fileListElement) {
    refreshFileList();
  }

  if (refreshFilesButton) {
    refreshFilesButton.addEventListener("click", refreshFileList);
  }

  if (plagiarismContainer) {
    initPlagiarismViewer();
  }

  if (globalAnalysisPanel) {
    refreshGlobalAnalysis();
  }

  if (fileExplorerWrapper && initialExplorerDatasetParam && datasetSelect) {
    const desiredDataset = initialExplorerDatasetParam;
    if (!datasetSelect.value || datasetSelect.value !== desiredDataset) {
      setCurrentDataset(desiredDataset);
    }
  }
}

document.addEventListener("DOMContentLoaded", initDatasetControls);
