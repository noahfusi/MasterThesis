const API_ROUTES = {
  datasets: "/datasets",
  currentDataset: "/current-dataset",
  listFiles: "/datasets/files",
  fileContent: "/datasets/file",
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
    showMessage(datasetFeedbackElement, `Current dataset: ${data.current_dataset || "None"}.`);
  } catch (error) {
    showMessage(datasetFeedbackElement, error.message, true);
    refreshDatasets();
    if (fileListElement) {
      refreshFileList();
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
}

document.addEventListener("DOMContentLoaded", initDatasetControls);
