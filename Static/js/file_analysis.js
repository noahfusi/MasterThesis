(function () {
  const app = window.App || {};
  const { API_ROUTES, utils = {}, dataset = {}, taskSocket = {}, onReady = (fn) => fn() } = app;
  const { requestJSON, showMessage, getMessage } = utils;
  const { getSelectedDataset } = dataset;
  const subscribeToTaskEvents = taskSocket.subscribe || (() => () => {});

  const filesContainer = document.getElementById("file-analysis-list");
  const pageFeedback = document.getElementById("file-analysis-feedback");
  const datasetButton = document.getElementById("generate-dataset-feedback");
  const spinner = document.createElement("span");
  spinner.className = "file-analysis-spinner";
  spinner.textContent = "Processing…";
  spinner.hidden = true;
  if (pageFeedback && pageFeedback.parentElement) {
    pageFeedback.parentElement.appendChild(spinner);
  }
  let datasetRequestInFlight = false;
  let initialized = false;
  let filesCache = [];
  let refreshInFlight = false;
  let currentDataset = null;
  let filesInFlight = false;
  const pendingFileFeedback = new Set();
  let datasetFeedbackPending = false;
  let unsubscribeTaskEvents = null;
  function updateSpinnerVisibility() {
    if (!spinner) return;
    const shouldShow = refreshInFlight || datasetFeedbackPending || pendingFileFeedback.size > 0;
    spinner.hidden = !shouldShow;
  }
  const normalizePath = (value) => (value || "").replace(/\\/g, "/").toLowerCase();

  function bindTaskEvents() {
    if (unsubscribeTaskEvents) return;
    unsubscribeTaskEvents = subscribeToTaskEvents((event) => {
      if (!event) return;
      const activeDataset = (getSelectedDataset && getSelectedDataset()) || currentDataset;
      if (!activeDataset || event.dataset !== activeDataset) return;
      if (event.type === "feedback-file-completed") {
        const normalized = normalizePath(event.filename);
        const wasPending = pendingFileFeedback.delete(normalized);
        if (wasPending) {
          showMessage(pageFeedback, getMessage("FEEDBACK_COMPLETED", {}, "Feedback available."));
        }
        updateSpinnerVisibility();
        void refreshFeedbackState(activeDataset);
      } else if (event.type === "feedback-dataset-completed") {
        datasetFeedbackPending = false;
        updateSpinnerVisibility();
        const isError = event.status === "failed";
        const message = isError
          ? event.error || "Dataset-wide feedback failed."
          : getMessage("FEEDBACK_DATASET_COMPLETED", {}, "Dataset feedback completed.");
        showMessage(pageFeedback, message, isError);
        void refreshFeedbackState(activeDataset);
      }
    });
  }

  document.addEventListener("app:dataset-changed", () => {
    pendingFileFeedback.clear();
    datasetFeedbackPending = false;
    updateSpinnerVisibility();
  });

  if (!filesContainer) return;

  function renderFiles(files = [], datasetName = null, feedbackSet = new Set()) {
    filesContainer.innerHTML = "";
    if (!files.length) {
      const empty = document.createElement("p");
      empty.className = "file-analysis-empty";
      empty.textContent = getMessage("STUDENTS_NO_FILES", {}, "No files.");
      filesContainer.appendChild(empty);
      return;
    }
    files.forEach((file) => {
      const row = document.createElement("div");
      row.className = "file-analysis-row";
      const name = document.createElement("span");
      name.className = "file-analysis-name";
      name.textContent = file;
      const actions = document.createElement("div");
      actions.className = "file-analysis-actions-inline";

      const generateBtn = document.createElement("button");
      generateBtn.type = "button";
      generateBtn.className = "file-analysis-action";
      generateBtn.textContent = "Generate feedback";
      generateBtn.addEventListener("click", () => void requestFileFeedback(datasetName, file, row));

      actions.appendChild(generateBtn);

      const feedbackPath = `${file}.feedback.txt`.toLowerCase();
      if (feedbackSet.has(feedbackPath)) {
        const viewBtn = document.createElement("button");
        viewBtn.type = "button";
        viewBtn.className = "file-analysis-secondary";
        viewBtn.textContent = "View feedback";
        viewBtn.addEventListener("click", () => void loadFeedback(datasetName, feedbackPath));
        actions.appendChild(viewBtn);
        const downloadBtn = document.createElement("button");
        downloadBtn.type = "button";
        downloadBtn.className = "file-analysis-secondary";
        downloadBtn.textContent = "Download feedback";
        downloadBtn.addEventListener("click", () => {
          const params = new URLSearchParams({ dataset: datasetName, filename: feedbackPath });
          const url = `${API_ROUTES.feedbackRead}?${params.toString()}`;
          fetch(url)
            .then((res) => res.json())
            .then((payload) => {
              const blob = new Blob([payload.content || ""], { type: "text/plain" });
              const link = document.createElement("a");
              link.href = URL.createObjectURL(blob);
              link.download = feedbackPath.split("/").pop() || "feedback.txt";
              document.body.appendChild(link);
              link.click();
              link.remove();
            })
            .catch((err) => showMessage(pageFeedback, err.message || "Download failed", true));
        });
        actions.appendChild(downloadBtn);
      }

      row.appendChild(name);
      row.appendChild(actions);
      filesContainer.appendChild(row);
    });
  }

  async function loadFiles(datasetName) {
    currentDataset = datasetName;
    if (filesCache.length && datasetName === currentDataset && !filesInFlight) {
      await refreshFeedbackState(datasetName);
      return;
    }
    if (filesInFlight && datasetName === currentDataset) return;
    filesInFlight = true;
    if (!datasetName) {
      renderFiles([]);
      showMessage(pageFeedback, getMessage("STUDENTS_SELECT_DATASET", {}, "Select a dataset to display outliers."), true);
      filesInFlight = false;
      return;
    }
    showMessage(pageFeedback, "Loading files...");
    try {
      const params = new URLSearchParams({ dataset: datasetName });
      const payload = await requestJSON(`${API_ROUTES.listFiles}?${params.toString()}`);
      filesCache = payload.files || [];
      await refreshFeedbackState(datasetName);
      showMessage(pageFeedback, "");
    } catch (error) {
      renderFiles([]);
      showMessage(pageFeedback, error.message, true);
    } finally {
      filesInFlight = false;
    }
  }

  async function requestFileFeedback(datasetName, filename, row) {
    console.log("Requesting file feedback for:", datasetName, filename);
    if (!datasetName) {
      showMessage(pageFeedback, getMessage("STUDENTS_SELECT_DATASET", {}, "Select a dataset to display outliers."), true);
      return;
    }
    const button = row && row.querySelector("button");
    if (button) button.disabled = true;
    spinner.hidden = false;
    showMessage(pageFeedback, "Generating feedback...");
    try {
      await requestJSON(API_ROUTES.feedbackFile, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ dataset: datasetName, filename }),
      });
      showMessage(pageFeedback, getMessage("FEEDBACK_FILE_QUEUED", {}, "Feedback generation queued"));
      const normalized = normalizePath(filename);
      pendingFileFeedback.add(normalized);
      updateSpinnerVisibility();
    } catch (error) {
      showMessage(pageFeedback, error.message, true);
    } finally {
      if (button) button.disabled = false;
      updateSpinnerVisibility();
    }
  }

  async function requestDatasetFeedback(datasetName) {
    if (!datasetName) {
      showMessage(pageFeedback, getMessage("STUDENTS_SELECT_DATASET", {}, "Select a dataset to display outliers."), true);
      return;
    }
    console.log("Requesting dataset feedback for:", datasetName);
    if (datasetRequestInFlight) return;
    datasetRequestInFlight = true;
    if (datasetButton) datasetButton.disabled = true;
    try {
      await requestJSON(API_ROUTES.feedbackDataset, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ dataset: datasetName }),
      });
      showMessage(pageFeedback, getMessage("FEEDBACK_DATASET_QUEUED", {}, "Dataset-wide feedback generation queued."));
      datasetFeedbackPending = true;
      updateSpinnerVisibility();
    } catch (error) {
      showMessage(pageFeedback, error.message, true);
    } finally {
      if (datasetButton) datasetButton.disabled = false;
      datasetRequestInFlight = false;
      updateSpinnerVisibility();
    }
  }

  async function loadFeedback(datasetName, filename) {
    if (!datasetName || !filename) return;
    try {
      const params = new URLSearchParams({ dataset: datasetName, filename });
      const payload = await requestJSON(`${API_ROUTES.feedbackRead}?${params.toString()}`);
      const modal = document.createElement("div");
      modal.className = "file-feedback-modal";
      const content = document.createElement("pre");
      content.textContent = payload.content || "No feedback content available.";
      const controls = document.createElement("div");
      controls.className = "file-feedback-controls";
      const closeBtn = document.createElement("button");
      closeBtn.textContent = "Close";
      closeBtn.addEventListener("click", () => modal.remove());
      controls.appendChild(closeBtn);
      modal.appendChild(controls);
      modal.appendChild(content);
      document.body.appendChild(modal);
    } catch (error) {
      showMessage(pageFeedback, error.message, true);
    }
  }

  async function refreshFeedbackState(datasetName) {
    if (!datasetName) return;
    if (refreshInFlight) return;
    refreshInFlight = true;
    updateSpinnerVisibility();
    try {
      const params = new URLSearchParams({ dataset: datasetName });
      const feedbackList = await requestJSON(`${API_ROUTES.feedbackList}?${params.toString()}`);
      const feedbackFiles = new Set((feedbackList.files || []).map((f) => f.toLowerCase()));
      renderFiles(filesCache, datasetName, feedbackFiles);
    } catch (error) {
      showMessage(pageFeedback, error.message, true);
    }
    refreshInFlight = false;
    updateSpinnerVisibility();
  }

  onReady(() => {
    if (initialized) return;
    initialized = true;
    const datasetName = getSelectedDataset ? getSelectedDataset() : null;
    bindTaskEvents();
    updateSpinnerVisibility();
    if (datasetButton) {
      datasetButton.addEventListener(
        "click",
        () => {
          const current = getSelectedDataset ? getSelectedDataset() : datasetName;
          void requestDatasetFeedback(current);
        },
        { once: true },
      );
    }
    void loadFiles(datasetName);
  });
})();
