(function () {
  const app = window.App || {};
  const { API_ROUTES, utils = {}, dataset = {}, taskSocket = {}, onReady = (fn) => fn() } = app;
  const { requestJSON, showMessage, getMessage } = utils;
  const { refreshDatasets, setCurrentDataset, getDatasetStatus } = dataset;
  const subscribeToTaskEvents = taskSocket.subscribe || (() => () => {});

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

  let unsubscribeStatusEvents = null;
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
    showMessage(
      datasetFeedbackElement,
      getMessage("UPLOAD_REFERENCE_START", { dataset: datasetName }, `Uploading reference for ${datasetName}...`),
    );
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
      showMessage(
        datasetFeedbackElement,
        getMessage(
          "UPLOAD_REFERENCE_SUCCESS",
          { filename: data.filename, dataset: datasetName },
          `Reference file "${data.filename}" saved for ${datasetName}.`,
        ),
      );
    } catch (error) {
      showMessage(datasetFeedbackElement, error.message, true);
    }
  }

  async function uploadRequirementsFile(datasetName, file) {
    if (!file) return;
    const formData = new FormData();
    formData.append("file", file);
    showMessage(
      datasetFeedbackElement,
      getMessage("UPLOAD_REQUIREMENTS_START", { dataset: datasetName }, `Uploading requirements for ${datasetName}...`),
    );
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
      showMessage(
        datasetFeedbackElement,
        getMessage(
          "UPLOAD_REQUIREMENTS_SUCCESS",
          { filename: data.filename, dataset: datasetName },
          `Requirements "${data.filename}" saved for ${datasetName}.`,
        ),
      );
    } catch (error) {
      showMessage(datasetFeedbackElement, error.message, true);
    }
  }

  async function uploadAutotestFile(datasetName, file) {
    if (!file) return;
    const formData = new FormData();
    formData.append("file", file);
    showMessage(datasetFeedbackElement, `Uploading autotest for ${datasetName}...`);
    try {
      const response = await fetch(`/datasets/${encodeURIComponent(datasetName)}/autotest`, {
        method: "POST",
        body: formData,
      });
      if (!response.ok) {
        const payload = await response.json().catch(() => ({}));
        throw new Error(payload.detail || payload.message || response.statusText);
      }
      const data = await response.json();
      showMessage(datasetFeedbackElement, `Autotest file "${data.filename}" saved for ${datasetName}.`);
    } catch (error) {
      showMessage(datasetFeedbackElement, error.message, true);
    }
  }

  async function deleteDataset(name) {
    if (!window.confirm(getMessage("CONFIRM_DELETE_DATASET", { dataset: name }, `Delete dataset "${name}"?`))) {
      return;
    }
    try {
      await requestJSON(`${API_ROUTES.datasets}/${encodeURIComponent(name)}`, {
        method: "DELETE",
      });
      showMessage(datasetFeedbackElement, getMessage("DATASET_DELETED", { dataset: name }, `Dataset "${name}" deleted.`));
      await loadDatasets();
    } catch (error) {
      showMessage(datasetFeedbackElement, error.message, true);
    }
  }

  async function handleSelectDataset(name) {
    try {
      await setCurrentDataset(name || null);
      showMessage(
        datasetFeedbackElement,
        getMessage("DATASET_READY", { dataset: name || "None" }, `Current dataset: ${name || "None"}.`),
      );
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
      empty.textContent = getMessage("NO_DATASETS", {}, "No datasets yet.");
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

      const autotestBtn = document.createElement("button");
      autotestBtn.type = "button";
      autotestBtn.textContent = "Autotest";
      const autotestInput = document.createElement("input");
      autotestInput.type = "file";
      autotestInput.accept = ".yml,.yaml";
      autotestInput.style.display = "none";
      autotestBtn.addEventListener("click", () => autotestInput.click());
      autotestInput.addEventListener("change", (event) => {
        const file = event.target.files && event.target.files[0];
        if (file) {
          uploadAutotestFile(name, file);
        }
        event.target.value = "";
      });
      const autotestWrapper = document.createElement("div");
      autotestWrapper.className = "reference-upload-wrapper";
      autotestWrapper.appendChild(autotestBtn);
      autotestWrapper.appendChild(autotestInput);
      actions.appendChild(autotestWrapper);

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
    showMessage(uploadStatusElement, getMessage("UPLOAD_IN_PROGRESS", {}, "Uploading..."));

    try {
      const data = await requestJSON(API_ROUTES.datasets, {
        method: "POST",
        body: formData,
      });
      showMessage(
        uploadStatusElement,
        getMessage(
          "DATASET_CREATED_PROCESSING",
          { dataset: data.dataset },
          `Dataset "${data.dataset}" created. Processing in progress...`,
        ),
      );
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
    if (unsubscribeStatusEvents) {
      unsubscribeStatusEvents();
    }
    unsubscribeStatusEvents = null;
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

    unsubscribeStatusEvents = subscribeToTaskEvents((event) => {
      if (!event || event.type !== "dataset-status") return;
      const targetDataset = event.dataset || (event.status && event.status.dataset);
      if (targetDataset !== name) return;
      const status = event.status || event;
      renderStatus(status);
      const state = status && status.state;
      if (state === "ready") {
        showMessage(datasetStatusFeedback, `Dataset "${name}" ready.`, false);
        stopWatching();
        void loadDatasets();
      } else if (state === "failed") {
        const message = status.error || status.message || "Dataset processing failed.";
        showMessage(datasetStatusFeedback, message, true);
        stopWatching();
      }
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
