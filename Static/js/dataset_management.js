(function () {
  const app = window.App || {};
  const { API_ROUTES, utils = {}, dataset = {}, onReady = (fn) => fn() } = app;
  const { requestJSON, showMessage } = utils;
  const { refreshDatasets, setCurrentDataset, getDatasetStatus, pollDatasetStatus } = dataset;

  const datasetListElement = document.getElementById("dataset-list");
  const datasetFeedbackElement = document.getElementById("dataset-feedback");
  const currentDatasetDisplay = document.getElementById("current-dataset-display");
  const uploadForm = document.getElementById("dataset-upload-form");
  const uploadStatusElement = document.getElementById("dataset-upload-status");
  const processingPanel = document.getElementById("dataset-processing-panel");
  const processingName = document.getElementById("dataset-processing-name");
  const processingState = document.getElementById("dataset-processing-state");
  const processingPhase = document.getElementById("dataset-processing-phase");
  const processingMessage = document.getElementById("dataset-processing-message");
  const datasetStatusFeedback = document.getElementById("dataset-status-feedback");

  if (!datasetListElement && !uploadForm) return;

  let stopStatusPolling = null;
  let watchingDataset = null;

  function updateCurrentDatasetDisplay(currentDataset) {
    if (currentDatasetDisplay) {
      currentDatasetDisplay.textContent = currentDataset || "None";
    }
  }

  async function uploadReferenceFile(datasetName, file) {
    if (!file) return;
    const formData = new FormData();
    formData.append("file", file);
    showMessage(datasetFeedbackElement, `Uploading reference for ${datasetName}...`);
    try {
      const response = await fetch(`/datasets/${encodeURIComponent(datasetName)}/reference`, {
        method: "POST",
        body: formData,
      });
      if (!response.ok) {
        const payload = await response.json().catch(() => ({}));
        throw new Error(payload.detail || payload.message || response.statusText);
      }
      const data = await response.json();
      showMessage(datasetFeedbackElement, `Reference file "${data.filename}" saved for ${datasetName}.`);
    } catch (error) {
      showMessage(datasetFeedbackElement, error.message, true);
    }
  }

  async function uploadRequirementsFile(datasetName, file) {
    if (!file) return;
    const formData = new FormData();
    formData.append("file", file);
    showMessage(datasetFeedbackElement, `Uploading requirements for ${datasetName}...`);
    try {
      const response = await fetch(`/datasets/${encodeURIComponent(datasetName)}/requirements`, {
        method: "POST",
        body: formData,
      });
      if (!response.ok) {
        const payload = await response.json().catch(() => ({}));
        throw new Error(payload.detail || payload.message || response.statusText);
      }
      const data = await response.json();
      showMessage(datasetFeedbackElement, `Requirements "${data.filename}" saved for ${datasetName}.`);
    } catch (error) {
      showMessage(datasetFeedbackElement, error.message, true);
    }
  }

  async function deleteDataset(name) {
    if (!window.confirm(`Delete dataset "${name}"?`)) {
      return;
    }
    try {
      await requestJSON(`${API_ROUTES.datasets}/${encodeURIComponent(name)}`, {
        method: "DELETE",
      });
      showMessage(datasetFeedbackElement, `Dataset "${name}" deleted.`);
      await loadDatasets();
    } catch (error) {
      showMessage(datasetFeedbackElement, error.message, true);
    }
  }

  async function handleSelectDataset(name) {
    try {
      await setCurrentDataset(name || null);
      showMessage(datasetFeedbackElement, `Current dataset: ${name || "None"}.`);
      updateCurrentDatasetDisplay(name);
      await loadDatasets();
    } catch (error) {
      showMessage(datasetFeedbackElement, error.message, true);
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
      selectBtn.addEventListener("click", () => handleSelectDataset(name));
      actions.appendChild(selectBtn);

      const referenceBtn = document.createElement("button");
      referenceBtn.type = "button";
      referenceBtn.textContent = "Reference";
      const referenceInput = document.createElement("input");
      referenceInput.type = "file";
      referenceInput.accept = "*/*";
      referenceInput.style.display = "none";
      referenceBtn.addEventListener("click", () => referenceInput.click());
      referenceInput.addEventListener("change", (event) => {
        const file = event.target.files && event.target.files[0];
        if (file) {
          uploadReferenceFile(name, file);
        }
        event.target.value = "";
      });
      const referenceWrapper = document.createElement("div");
      referenceWrapper.className = "reference-upload-wrapper";
      referenceWrapper.appendChild(referenceBtn);
      referenceWrapper.appendChild(referenceInput);
      actions.appendChild(referenceWrapper);

      const requirementsBtn = document.createElement("button");
      requirementsBtn.type = "button";
      requirementsBtn.textContent = "Requirements";
      const requirementsInput = document.createElement("input");
      requirementsInput.type = "file";
      requirementsInput.accept = ".txt,.md,.pdf,.doc,.docx";
      requirementsInput.style.display = "none";
      requirementsBtn.addEventListener("click", () => requirementsInput.click());
      requirementsInput.addEventListener("change", (event) => {
        const file = event.target.files && event.target.files[0];
        if (file) {
          uploadRequirementsFile(name, file);
        }
        event.target.value = "";
      });
      const requirementsWrapper = document.createElement("div");
      requirementsWrapper.className = "reference-upload-wrapper";
      requirementsWrapper.appendChild(requirementsBtn);
      requirementsWrapper.appendChild(requirementsInput);
      actions.appendChild(requirementsWrapper);

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

  async function loadDatasets() {
    try {
      const data = await refreshDatasets();
      renderDatasetList(data.datasets || []);
      updateCurrentDatasetDisplay(data.current_dataset || null);
      showMessage(datasetFeedbackElement, "");
      void trackCurrentDatasetStatus(data.current_dataset || null);
    } catch (error) {
      showMessage(datasetFeedbackElement, error.message, true);
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
      showMessage(uploadStatusElement, `Dataset "${data.dataset}" créé. Traitement en cours...`);
      uploadForm.reset();
      if (data && data.dataset) {
        watchDatasetStatus(data.dataset, data.status);
      }
      await loadDatasets();
    } catch (error) {
      showMessage(uploadStatusElement, error.message, true);
    }
  }

  function renderStatus(status) {
    if (!processingPanel) return;
    if (!status) {
      processingPanel.style.display = "none";
      return;
    }
    processingPanel.style.display = "block";
    processingName.textContent = status.dataset || watchingDataset || "-";
    processingState.textContent = status.state || "";
    processingPhase.textContent = status.phase ? `Phase: ${status.phase}` : "";
    processingMessage.textContent = status.message || status.error || "";
  }

  function stopWatching() {
    if (stopStatusPolling) {
      stopStatusPolling();
    }
    stopStatusPolling = null;
    watchingDataset = null;
  }

  function watchDatasetStatus(name, initialStatus) {
    if (!name) {
      stopWatching();
      renderStatus(null);
      return;
    }
    if (watchingDataset === name && !initialStatus) {
      return;
    }

    stopWatching();
    watchingDataset = name;
    if (initialStatus) {
      renderStatus(initialStatus);
      if (initialStatus.state === "ready" || initialStatus.state === "failed") {
        watchingDataset = null;
        return;
      }
    }

    stopStatusPolling = pollDatasetStatus(name, {
      onUpdate: renderStatus,
      onReady: (status) => {
        renderStatus(status);
        showMessage(datasetStatusFeedback, `Dataset "${name}" prêt.`, false);
        stopWatching();
        void loadDatasets();
      },
      onFailed: (status, error) => {
        const message =
          (status && (status.error || status.message)) || (error && error.message) || "Dataset processing failed.";
        renderStatus(status || null);
        showMessage(datasetStatusFeedback, message, true);
        stopWatching();
      },
    });
  }

  async function trackCurrentDatasetStatus(name) {
    if (!name) {
      stopWatching();
      renderStatus(null);
      return;
    }
    try {
      const status = await getDatasetStatus(name);
      if (status && status.state && status.state !== "ready") {
        watchDatasetStatus(name, status);
      } else {
        renderStatus(status);
      }
    } catch (error) {
      /* ignore bootstrap errors */
    }
  }

  onReady(() => {
    if (uploadForm) {
      uploadForm.addEventListener("submit", handleUpload);
    }
    void loadDatasets();
  });
})();
