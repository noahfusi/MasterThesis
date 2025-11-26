(function () {
  const app = window.App || {};
  const { API_ROUTES, utils = {}, dataset = {}, onReady = (fn) => fn() } = app;
  const { requestJSON, showMessage, getMessage } = utils;
  const { getSelectedDataset } = dataset;

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
      // Poll for availability
      const feedbackPath = `${filename}.feedback.txt`.toLowerCase();
      let attempts = 0;
      const poll = async () => {
        attempts += 1;
        try {
          const params = new URLSearchParams({ dataset: datasetName });
          const feedbackList = await requestJSON(`${API_ROUTES.feedbackList}?${params.toString()}`);
          const feedbackFiles = new Set((feedbackList.files || []).map((f) => f.toLowerCase()));
          if (feedbackFiles.has(feedbackPath)) {
            await refreshFeedbackState(datasetName);
            spinner.hidden = true;
            showMessage(pageFeedback, getMessage("FEEDBACK_COMPLETED", {}, "Feedback available."));
            return;
          }
        } catch (err) {
          // ignore and keep polling
        }
        if (attempts < 20) {
          setTimeout(poll, 2000);
        } else {
          spinner.hidden = true;
          showMessage(
            pageFeedback,
            "Feedback generation is taking longer than expected. Please check again later or refresh the page.",
            true,
          );
        }
      };
      setTimeout(poll, 2000);
    } catch (error) {
      showMessage(pageFeedback, error.message, true);
      spinner.hidden = true;
    } finally {
      if (button) button.disabled = false;
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
      await refreshFeedbackState(datasetName);
    } catch (error) {
      showMessage(pageFeedback, error.message, true);
    } finally {
      if (datasetButton) datasetButton.disabled = false;
      datasetRequestInFlight = false;
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
    if (spinner) spinner.hidden = false;
    try {
      const params = new URLSearchParams({ dataset: datasetName });
      const feedbackList = await requestJSON(`${API_ROUTES.feedbackList}?${params.toString()}`);
      const feedbackFiles = new Set((feedbackList.files || []).map((f) => f.toLowerCase()));
      renderFiles(filesCache, datasetName, feedbackFiles);
      if (spinner) spinner.hidden = true;
    } catch (error) {
      showMessage(pageFeedback, error.message, true);
      if (spinner) spinner.hidden = true;
    }
    refreshInFlight = false;
  }

  onReady(() => {
    if (initialized) return;
    initialized = true;
    const datasetName = getSelectedDataset ? getSelectedDataset() : null;
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
