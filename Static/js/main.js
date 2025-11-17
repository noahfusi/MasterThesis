const API_ROUTES = {
  datasets: "/datasets",
  currentDataset: "/current-dataset",
  listFiles: "/datasets/files",
  fileContent: "/datasets/file",
  fileAnalysis: "/datasets/file/lizard",
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

const plagiarismState = {
  blocks: [],
  index: 0,
  dataset: plagiarismContainer ? plagiarismContainer.dataset.dataset : null,
  summaryFilename: plagiarismContainer ? plagiarismContainer.dataset.summaryFilename : "lizard_dataset.xml",
  snippetCache: new Map(),
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

  if (fileDatasetLabel) {
    fileDatasetLabel.textContent = dataset || "N/A";
  }

  fileListElement.innerHTML = "";
  if (!files.length) {
    const item = document.createElement("li");
    item.textContent = dataset ? "No files found in raw folder." : "Select a dataset to list files.";
    fileListElement.appendChild(item);
    return;
  }

  files.forEach((filename) => {
    const item = document.createElement("li");
    item.className = "dataset-row";

    const label = document.createElement("span");
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

function showFilePreview(filename, content) {
  if (!filePreviewPanel || !filePreviewContent) return;
  if (filePreviewName) {
    filePreviewName.textContent = filename;
  }
  filePreviewContent.textContent = content;
  if (fileExplorerWrapper) {
    fileExplorerWrapper.classList.add("show-preview");
  }
}

async function loadFileContent(filename) {
  if (!filename) return;
  showMessage(fileListFeedback, `Loading ${filename}...`);
  try {
    const data = await requestJSON(`${API_ROUTES.fileContent}?filename=${encodeURIComponent(filename)}`);
    showFilePreview(data.filename, data.content);
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
    const params = new URLSearchParams({
      dataset,
      summary: "true",
      filename: plagiarismState.summaryFilename || "lizard_dataset.xml",
    });
    const data = await requestJSON(`${API_ROUTES.fileAnalysis}?${params.toString()}`);
    const blocks = parseDuplicateBlocks(data.analysis || "");
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
}

document.addEventListener("DOMContentLoaded", initDatasetControls);
