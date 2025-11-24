(function () {
  const app = window.App || {};
  const { API_ROUTES, utils = {}, dataset = {}, onReady = (fn) => fn() } = app;
  const { requestJSON, showMessage, getMessage } = utils;
  const { getSelectedDataset } = dataset;

  const filesContainer = document.getElementById("file-analysis-list");
  const pageFeedback = document.getElementById("file-analysis-feedback");
  const datasetButton = document.getElementById("generate-dataset-feedback");

  if (!filesContainer) return;

  function renderFiles(files = [], datasetName = null) {
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

      row.appendChild(name);
      row.appendChild(actions);
      filesContainer.appendChild(row);
    });
  }

  async function loadFiles(datasetName) {
    if (!datasetName) {
      renderFiles([]);
      showMessage(pageFeedback, getMessage("STUDENTS_SELECT_DATASET", {}, "Select a dataset to display outliers."), true);
      return;
    }
    showMessage(pageFeedback, "Loading files...");
    try {
      const params = new URLSearchParams({ dataset: datasetName });
      const payload = await requestJSON(`${API_ROUTES.listFiles}?${params.toString()}`);
      const files = payload.files || [];
      renderFiles(files, datasetName);
      showMessage(pageFeedback, "");
    } catch (error) {
      renderFiles([]);
      showMessage(pageFeedback, error.message, true);
    }
  }

  async function requestFileFeedback(datasetName, filename, row) {
    if (!datasetName) {
      showMessage(pageFeedback, getMessage("STUDENTS_SELECT_DATASET", {}, "Select a dataset to display outliers."), true);
      return;
    }
    const button = row && row.querySelector("button");
    if (button) button.disabled = true;
    try {
      await requestJSON(API_ROUTES.feedbackFile, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ dataset: datasetName, filename }),
      });
      showMessage(pageFeedback, getMessage("FEEDBACK_FILE_QUEUED", {}, "Feedback generation queued (not implemented)."));
      await refreshFeedbackState(datasetName); // reload availability after enqueue
    } catch (error) {
      showMessage(pageFeedback, error.message, true);
    } finally {
      if (button) button.disabled = false;
    }
  }

  async function requestDatasetFeedback(datasetName) {
    if (!datasetName) {
      showMessage(pageFeedback, getMessage("STUDENTS_SELECT_DATASET", {}, "Select a dataset to display outliers."), true);
      return;
    }
    if (datasetButton) datasetButton.disabled = true;
    try {
      await requestJSON(API_ROUTES.feedbackDataset, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ dataset: datasetName }),
      });
      showMessage(pageFeedback, getMessage("FEEDBACK_DATASET_QUEUED", {}, "Dataset-wide feedback generation queued (not implemented)."));
      await refreshFeedbackState(datasetName);
    } catch (error) {
      showMessage(pageFeedback, error.message, true);
    } finally {
      if (datasetButton) datasetButton.disabled = false;
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
    try {
      const params = new URLSearchParams({ dataset: datasetName });
      const payload = await requestJSON(`${API_ROUTES.listFiles}?${params.toString()}`);
      const files = payload.files || [];
      const feedbackList = await requestJSON(`${API_ROUTES.feedbackList}?${params.toString()}`);
      const feedbackFiles = new Set((feedbackList.files || []).map((f) => f.toLowerCase()));

      filesContainer.innerHTML = "";
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
        if (feedbackFiles.has(feedbackPath)) {
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
    } catch (error) {
      showMessage(pageFeedback, error.message, true);
    }
  }

  onReady(() => {
    const datasetName = getSelectedDataset ? getSelectedDataset() : null;
    if (datasetButton) {
      datasetButton.addEventListener("click", () => {
        const current = getSelectedDataset ? getSelectedDataset() : datasetName;
        void requestDatasetFeedback(current);
      });
    }
    void loadFiles(datasetName).then(() => refreshFeedbackState(datasetName));
  });
})();
