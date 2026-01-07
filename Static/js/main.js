const App = window.App || {};
App.__initialized = App.__initialized || {};
const mainAlreadyInitialized = Boolean(App.__initialized.main);

App.API_ROUTES = {
  datasets: "/datasets",
  currentDataset: "/current-dataset",
  listFiles: "/datasets/files",
  excludedFiles: "/datasets/excluded-files",
  fileContent: "/datasets/file",
  fileAnalysis: "/datasets/file/lizard",
  globalMetrics: "/metrics/global",
  studentsOutliers: "/metrics/students",
  thresholdDescriptions: "/metrics/threshold-descriptions",
  messages: "/meta/messages",
  clustering: "/clustering/run",
  clusteringLast: "/clustering/last",
  generateReport: "/reports/generate-report",
  latestReport: (dataset) => `/reports/latest-report${dataset ? `?dataset=${encodeURIComponent(dataset)}` : ""}`,
  downloadReport: (dataset) => `/reports/download-report${dataset ? `?dataset=${encodeURIComponent(dataset)}` : ""}`,
  datasetStatus: (name) => `/datasets/${encodeURIComponent(name)}/status`,
  feedbackFile: "/feedback/file",
  feedbackDataset: "/feedback/dataset",
  feedbackList: "/feedback/files",
  feedbackRead: "/feedback/file",
  autotestFiles: "/autotest/files",
  autotestFileResults: (filenames, dataset) => {
    const list = Array.isArray(filenames) ? filenames : filenames ? [filenames] : [];
    const params = new URLSearchParams();
    if (dataset) params.set("dataset", dataset);
    list.forEach((name) => params.append("filenames", name));
    const suffix = params.toString() ? `?${params.toString()}` : "";
    return `/autotest/files/results${suffix}`;
  },
  autotestTestReport: "/autotest/test-report",
  autotestRun: "/autotest/run",
  autotestUpload: (dataset) => `/datasets/${encodeURIComponent(dataset)}/autotest`,
};

const DEFAULT_LIZARD_SUMMARY = "lizard_dataset.xml";
const datasetSelect = document.getElementById("current-dataset-select");
const duplicateSummaryCache = new Map();

function requestJSON(url, options = {}) {
  return fetch(url, options).then(async (response) => {
    const contentType = response.headers.get("content-type") || "";
    const payload = contentType.includes("application/json") ? await response.json() : {};
    if (!response.ok) {
      const detail = payload.detail || payload.message || response.statusText;
      throw new Error(detail);
    }
    return payload;
  });
}

function showMessage(element, text, isError = false) {
  if (!element) return;
  element.textContent = text || "";
  element.classList.toggle("error", Boolean(isError && text));
  element.classList.toggle("success", Boolean(!isError && text));
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

function formatMetricValue(value) {
  if (typeof value !== "number" || Number.isNaN(value) || !Number.isFinite(value)) {
    return value ?? "-";
  }
  const absValue = Math.abs(value);
  if (absValue >= 1000 || absValue < 0.01) {
    return value.toPrecision(3);
  }
  return value.toFixed(2);
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

function computeQuartiles(values = []) {
  if (!values.length) return { q1: null, median: null, q3: null };
  const sorted = [...values].sort((a, b) => a - b);
  const percentile = (p) => {
    if (!sorted.length) return null;
    const pos = (sorted.length - 1) * p;
    const base = Math.floor(pos);
    const rest = pos - base;
    if (sorted[base + 1] !== undefined) {
      return sorted[base] + rest * (sorted[base + 1] - sorted[base]);
    }
    return sorted[base];
  };
  return {
    q1: percentile(0.25),
    median: percentile(0.5),
    q3: percentile(0.75),
  };
}

let thresholdDescriptionsCache = null;
let thresholdDescriptionsPromise = null;
let messagesPromise = null;
App.messages = App.messages || {};

async function getThresholdDescriptions() {
  if (thresholdDescriptionsCache) return thresholdDescriptionsCache;
  if (!thresholdDescriptionsPromise) {
    thresholdDescriptionsPromise = (async () => {
      try {
        const payload = await requestJSON(App.API_ROUTES.thresholdDescriptions);
        thresholdDescriptionsCache = payload.descriptions || {};
        return thresholdDescriptionsCache;
      } catch (error) {
        thresholdDescriptionsCache = {};
        return thresholdDescriptionsCache;
      }
    })();
  }
  return thresholdDescriptionsPromise;
}

function describeThreshold(metricKey, bucketKey) {
  if (!thresholdDescriptionsCache) {
    void getThresholdDescriptions();
    return "";
  }
  const descriptions = thresholdDescriptionsCache;
  const bucket = bucketKey || "";
  if (!descriptions || !metricKey) return "";
  const key = metricKey.toLowerCase();
  const category =
    (key.includes("duplication") && "duplication") ||
    ((key.includes("ccn") || key.includes("complex")) && "complexity") ||
    ((key.includes("ncss") || key.includes("loc") || key.includes("lines")) && "size") ||
    (key.includes("functions") && "functions") ||
    (key.includes("nesting") && "nesting") ||
    "generic";

  return (descriptions[category] && descriptions[category][bucket]) || (descriptions.generic && descriptions.generic[bucket]) || "";
}

function loadMessages() {
  if (messagesPromise) return messagesPromise;
  messagesPromise = (async () => {
    try {
      const payload = await requestJSON(App.API_ROUTES.messages);
      App.messages = payload.messages || {};
    } catch (err) {
      App.messages = App.messages || {};
    }
    return App.messages;
  })();
  return messagesPromise;
}

function getMessage(key, params = undefined, fallback = "") {
  const catalog = App.messages || {};
  const template = (key && catalog[key]) || fallback || key || "";
  if (!params) return template;
  return Object.keys(params).reduce((text, paramKey) => text.replace(new RegExp(`{${paramKey}}`, "g"), params[paramKey]), template);
}

if (!mainAlreadyInitialized) {
  void getThresholdDescriptions();
  void loadMessages();
}

const TASK_SOCKET_SOURCE = "task-socket";
const TASK_SOCKET_HEARTBEAT_MS = 15000;
const taskSocketSubscribers = new Set();
let taskSocketInitialized = false;
let fallbackTaskSocket = null;
let fallbackHeartbeatTimer = null;

function emitTaskEvent(event) {
  taskSocketSubscribers.forEach((subscriber) => {
    try {
      subscriber(event);
    } catch (err) {
      console.warn("Task event subscriber failed", err);
    }
  });
}

function handleTaskSocketMessage(raw) {
  let payload = raw;
  if (raw && typeof raw.data !== "undefined") {
    payload = raw.data;
  }
  if (typeof payload === "string") {
    try {
      payload = JSON.parse(payload);
    } catch (_) {
      /* keep raw string */
    }
  }
  emitTaskEvent(payload);
}

function ensureFallbackTaskSocket() {
  if (fallbackTaskSocket) return;
  const clearHeartbeat = () => {
    if (fallbackHeartbeatTimer) {
      clearInterval(fallbackHeartbeatTimer);
      fallbackHeartbeatTimer = null;
    }
  };
  try {
    fallbackTaskSocket = new WebSocket(`${window.location.origin.replace(/^http/, "ws")}/tasks/ws`);
  } catch (error) {
    console.warn("Fallback websocket failed to start", error);
    return;
  }
  fallbackTaskSocket.addEventListener("open", () => {
    emitTaskEvent({ type: "connected" });
    clearHeartbeat();
    fallbackHeartbeatTimer = setInterval(() => {
      try {
        if (fallbackTaskSocket && fallbackTaskSocket.readyState === WebSocket.OPEN) {
          fallbackTaskSocket.send("ping");
        }
      } catch (_) {
        /* ignore heartbeat send failures */
      }
    }, TASK_SOCKET_HEARTBEAT_MS);
  });
  fallbackTaskSocket.addEventListener("message", handleTaskSocketMessage);
  fallbackTaskSocket.addEventListener("close", () => {
    clearHeartbeat();
    fallbackTaskSocket = null;
    setTimeout(ensureFallbackTaskSocket, 2000);
  });
  fallbackTaskSocket.addEventListener("error", () => {
    clearHeartbeat();
    if (fallbackTaskSocket) {
      fallbackTaskSocket.close();
    }
  });
}

function requestServiceWorkerSubscription(registration) {
  const controller = navigator.serviceWorker.controller || registration.active || registration.waiting;
  if (controller) {
    controller.postMessage({ type: "task-socket-subscribe" });
  }
}

function handleServiceWorkerMessage(event) {
  const data = event.data || {};
  if (data.source !== TASK_SOCKET_SOURCE) return;
  const payload = data.event || data.payload || data;
  emitTaskEvent(payload);
}

function initTaskSocketBridge() {
  if (taskSocketInitialized) return;
  taskSocketInitialized = true;
  if ("serviceWorker" in navigator) {
    navigator.serviceWorker.addEventListener("message", handleServiceWorkerMessage);
    navigator.serviceWorker
      .register("/task-sw.js", { scope: "/" })
      .then((registration) => {
        requestServiceWorkerSubscription(registration);
        navigator.serviceWorker.addEventListener("controllerchange", () => requestServiceWorkerSubscription(registration));
      })
      .catch(() => {
        ensureFallbackTaskSocket();
      });
    navigator.serviceWorker.ready
      .then((registration) => {
        requestServiceWorkerSubscription(registration);
      })
      .catch(() => {
        ensureFallbackTaskSocket();
      });
  } else {
    ensureFallbackTaskSocket();
  }
}

function subscribeToTaskEvents(handler) {
  if (typeof handler !== "function") return () => {};
  taskSocketSubscribers.add(handler);
  initTaskSocketBridge();
  return () => taskSocketSubscribers.delete(handler);
}

function getClusterColor(cluster) {
  if (!Number.isFinite(cluster) || cluster < 0) {
    return "#94a3b8";
  }
  const palette = [
    "#2563eb",
    "#f97316",
    "#22c55e",
    "#a855f7",
    "#facc15",
    "#0ea5e9",
    "#ef4444",
    "#14b8a6",
    "#d946ef",
    "#fb7185",
  ];
  return palette[cluster % palette.length];
}

function getClusterLabel(cluster) {
  if (!Number.isFinite(cluster) || cluster < 0) return "Noise";
  return `Cluster ${cluster + 1}`;
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

function normalizeRelativePath(path) {
  if (!path) return "";
  return path.replace(/\\/g, "/");
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

async function requestDuplicateSummary(dataset, summaryFilename = DEFAULT_LIZARD_SUMMARY) {
  const params = new URLSearchParams({
    dataset,
    summary: "true",
    filename: summaryFilename,
  });
  const data = await requestJSON(`${App.API_ROUTES.fileAnalysis}?${params.toString()}`);
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

async function getDuplicateSummary(dataset, summaryFilename = DEFAULT_LIZARD_SUMMARY) {
  if (!dataset) return null;
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

async function getDuplicateRangesForFile(dataset, filename, summaryFilename = DEFAULT_LIZARD_SUMMARY) {
  if (!dataset || !filename) return [];
  try {
    const summary = await getDuplicateSummary(dataset, summaryFilename);
    return getDuplicateRangesFromSummary(summary, filename);
  } catch (error) {
    if (error && typeof error.message === "string" && error.message.toLowerCase().includes("not found")) {
      return [];
    }
    console.warn(`Unable to load duplicate ranges for ${filename}:`, error);
    return [];
  }
}

function updateDatasetSelect(datasets = [], currentDataset = null) {
  if (!datasetSelect) return;
  const valueToSet =
    (currentDataset && (currentDataset.name || currentDataset.dataset)) ||
    (typeof currentDataset === "string" ? currentDataset : "");
  datasetSelect.innerHTML = "";

  const placeholder = document.createElement("option");
  placeholder.value = "";
  placeholder.textContent = "No dataset";
  datasetSelect.appendChild(placeholder);

  datasets.forEach((entry) => {
    const name =
      typeof entry === "string" ? entry : entry && (entry.name || entry.dataset) ? entry.name || entry.dataset : null;
    if (!name) return;
    const option = document.createElement("option");
    option.value = String(name);
    option.textContent = String(name);
    datasetSelect.appendChild(option);
  });

  datasetSelect.value = valueToSet;
}

function getSelectedDataset() {
  return datasetSelect ? datasetSelect.value || null : null;
}

async function refreshDatasets() {
  const data = await requestJSON(App.API_ROUTES.datasets);
  updateDatasetSelect(data.datasets || [], data.current_dataset || null);
  document.dispatchEvent(
    new CustomEvent("app:datasets-refreshed", {
      detail: { datasets: data.datasets || [], currentDataset: data.current_dataset || null },
    }),
  );
  return data;
}

async function setCurrentDataset(name) {
  const data = await requestJSON(App.API_ROUTES.currentDataset, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ dataset_name: name || null }),
  });
  if (datasetSelect) {
    datasetSelect.value = (data.current_dataset && data.current_dataset.name) || "";
  }
  document.dispatchEvent(
    new CustomEvent("app:dataset-changed", {
      detail: { currentDataset: data.current_dataset || null },
    }),
  );
  return data;
}

async function getDatasetStatus(datasetName) {
  if (!datasetName) {
    throw new Error("Dataset name is required.");
  }
  const url =
    typeof App.API_ROUTES.datasetStatus === "function"
      ? App.API_ROUTES.datasetStatus(datasetName)
      : `${App.API_ROUTES.datasetStatus}/${encodeURIComponent(datasetName)}`;
  return requestJSON(url);
}

function handleDatasetSelectChange(event) {
  const value = event && event.target ? event.target.value : null;
  setCurrentDataset(value || null).finally(() => {
    window.location.reload();
  });
}

function openFileInExplorer(filename) {
  if (!filename) return;
  const params = new URLSearchParams({ file: filename });
  const dataset = getSelectedDataset();
  if (dataset) {
    params.set("dataset", dataset);
  }
  window.location.href = `/file-explorer?${params.toString()}`;
}

function onReady(fn) {
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", fn);
  } else {
    fn();
  }
}

function initCommonDatasetSelector() {
  if (datasetSelect) {
    datasetSelect.addEventListener("change", handleDatasetSelectChange);
  }
  refreshDatasets().catch(() => {
    /* ignore */
  });
}

if (!mainAlreadyInitialized) {
  App.utils = {
    requestJSON,
    showMessage,
    mergeLineRanges,
    formatMetricValue,
    computeStandardDeviation,
    computeQuartiles,
    describeThreshold,
    getMessage,
    loadMessages,
    getClusterColor,
    getClusterLabel,
    openFileInExplorer,
  };

  App.duplicates = {
    DEFAULT_LIZARD_SUMMARY,
    parseDuplicateBlocks,
    extractRelativePath,
    normalizeRelativePath,
    getDuplicateSummary,
    getDuplicateRangesFromSummary,
    getDuplicateRangesForFile,
  };

  App.dataset = {
    updateDatasetSelect,
    getSelectedDataset,
    refreshDatasets,
    setCurrentDataset,
    getDatasetStatus,
    getThresholdDescriptions,
  };

  App.taskSocket = {
    subscribe: subscribeToTaskEvents,
  };

  App.onReady = onReady;
  window.App = App;

  onReady(initTaskSocketBridge);
  onReady(initCommonDatasetSelector);
  App.__initialized.main = true;
} else {
  window.App = App;
}
