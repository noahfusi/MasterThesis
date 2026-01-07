(function () {
  const app = window.App || {};
  app.__initialized = app.__initialized || {};
  if (app.__initialized.students) return;
  app.__initialized.students = true;
  window.App = app;

  const { API_ROUTES, utils = {}, onReady = (fn) => fn() } = app;
  const { requestJSON, showMessage, formatMetricValue, getMessage } = utils;

  const datasetSelect = document.getElementById("current-dataset-select");
  const studentsPanel = document.getElementById("students-page");
  const studentsGrid = document.getElementById("students-metrics-grid");
  const studentsFeedback = document.getElementById("students-feedback");
  const studentsDatasetLabel = document.getElementById("students-dataset");
  const studentsMetricSelect = document.getElementById("students-metric-select");
  const reportButton = document.getElementById("students-generate-report");
  const downloadButton = document.getElementById("students-download-report");
  const reportFeedback = document.getElementById("students-report-feedback");
  const bucketTemplate = document.getElementById("students-bucket-template");

  if (!studentsPanel) return;

  const studentsState = {
    dataset: null,
    metrics: [],
    buckets: [],
    metricKey: null,
    cards: [],
    reportAvailable: false,
    students: [],
  };

  function dedupeMetricOptions(metrics = [], buckets = []) {
    const seen = new Set();
    const options = [];

    function pushOption(option) {
      if (!option || !option.key) return;
      if (seen.has(option.key)) return;
      seen.add(option.key);
      options.push({ key: option.key, label: option.label || option.key });
    }

    (metrics || []).forEach(pushOption);
    (buckets || []).forEach((bucket) => pushOption({ key: bucket.key, label: bucket.label }));
    return options;
  }

  function updateStudentsMetricSelect() {
    if (!studentsMetricSelect) return;
    studentsMetricSelect.innerHTML = "";
    const placeholder = document.createElement("option");
    placeholder.value = "";
    placeholder.textContent = getMessage("STUDENTS_NO_METRICS", {}, "Classification view");
    studentsMetricSelect.appendChild(placeholder);
    studentsMetricSelect.disabled = true;
  }

  function classifyStudentMetrics(entry) {
    const metrics = entry.metrics || {};

    const score = (key, level) => {
      const val = metrics[key];
      if (!val || typeof val !== "object") return false;
      return val.signal === level || val[level] === true;
    };

    const countHigh = (keys) => keys.reduce((acc, key) => acc + (score(key, "high") ? 1 : 0), 0);

    const highDifficulty =
      score("CCN", "extra_high") ||
      score("NCSS", "extra_high") ||
      score("Duplication (%)", "extra_high") ||
      score("Max nesting depth", "extra_high") ||
      (score("CCN", "high") &&
        (score("NCSS", "high") || score("Duplication (%)", "high") || score("Max nesting depth", "high"))) ||
      (score("NCSS/Functions", "high") && score("Functions", "low"));

    const moderateDifficulty =
      !highDifficulty &&
      countHigh(["CCN", "NCSS", "Duplication (%)", "If/NCSS", "Vars/NCSS"]) >= 2;

    const notGoodNotBad =
      !highDifficulty &&
      !moderateDifficulty &&
      (score("NCSS", "low") || score("NCSS", "normal")) &&
      (score("CCN", "low") || score("CCN", "normal")) &&
      (score("Duplication (%)", "low") || score("Duplication (%)", "normal"));

    const advanced =
      !highDifficulty &&
      !moderateDifficulty &&
      !score("CCN", "high") &&
      !score("CCN", "extra_high") &&
      !score("NCSS", "high") &&
      !score("NCSS", "extra_high") &&
      !score("Duplication (%)", "high") &&
      !score("Duplication (%)", "extra_high") &&
      score("NCSS", "low") &&
      score("CCN", "low") &&
      score("Max nesting depth", "low") &&
      (score("Functions", "low") || score("Functions", "normal")) &&
      score("Vars/Functions", "normal_low");

    if (highDifficulty) return "high";
    if (moderateDifficulty) return "moderate";
    if (advanced) return "advanced";
    if (notGoodNotBad) return "neutral";
    return "neutral";
  }

  function buildStudentFlags(entry) {
    const flags = [];
    const reportFlags = (studentsState.report && studentsState.report.combined_flags) || {};
    const filename = normalizeRelativePath(entry.filename || entry.path || entry.id || "");
    const mapped = reportFlags[filename] || reportFlags[entry.filename] || [];
    if (Array.isArray(mapped) && mapped.length) {
      mapped.forEach((flag) => {
        if (flag && flag.label) {
          flags.push({ label: flag.label, tone: flag.tone || "neutral" });
        }
      });
    }
    if (!flags.length) {
      flags.push({ label: "Neutral", tone: "neutral" });
    }
    return flags;
  }

  function renderStudentsBuckets(metricBuckets = [], students = []) {
    if (!studentsGrid) return;
    studentsGrid.innerHTML = "";

    if (!students.length) {
      const empty = document.createElement("p");
      empty.className = "students-empty";
      empty.textContent = getMessage("STUDENTS_NO_OUTLIERS", {}, "No students classified.");
      studentsGrid.appendChild(empty);
      studentsState.cards = [];
      return;
    }

    const groups = {
      high: [],
      moderate: [],
      neutral: [],
      advanced: [],
    };

    students.forEach((student) => {
      const bucket = classifyStudentMetrics(student);
      groups[bucket].push(student);
    });

    const order = [
      ["high", "High difficulties"],
      ["moderate", "Moderate difficulties"],
      ["neutral", "In the mean"],
      ["advanced", "Advanced"],
    ];

    order.forEach(([key, title]) => {
      const list = groups[key];
      const card = document.createElement("div");
      card.className = "students-metric-card";
      const heading = document.createElement("h3");
      heading.className = "students-metric-title";
      heading.textContent = title;
      card.appendChild(heading);

      const listEl = document.createElement("ul");
      listEl.className = "students-bucket-list";
      if (!list.length) {
        const li = document.createElement("li");
        li.textContent = getMessage("STUDENTS_NO_FILES", {}, "No students");
        listEl.appendChild(li);
      } else {
        list.forEach((entry) => {
          const li = document.createElement("li");
          const name = document.createElement("span");
          name.textContent = entry.filename || entry.path || entry.id || "Unknown";
          const flags = buildStudentFlags(entry);
          const flagContainer = document.createElement("span");
          flagContainer.className = "student-flags";
          flags.forEach((flag) => {
            const badge = document.createElement("span");
            badge.className = `student-flag student-flag-${flag.tone}`;
            badge.textContent = flag.label;
            flagContainer.appendChild(badge);
          });
          li.appendChild(name);
          if (flags.length) {
            li.appendChild(flagContainer);
          }
          listEl.appendChild(li);
        });
      }
      card.appendChild(listEl);
      studentsGrid.appendChild(card);
    });

    studentsState.cards = [];
  }

  async function downloadReport(datasetName) {
    if (!datasetName) {
      throw new Error(getMessage("STUDENTS_SELECT_DATASET", {}, "Select a dataset to download a report."));
    }
    const downloadUrl =
      typeof API_ROUTES.downloadReport === "function"
        ? API_ROUTES.downloadReport(datasetName)
        : `${API_ROUTES.downloadReport}?dataset=${encodeURIComponent(datasetName)}`;
    const payload = await requestJSON(downloadUrl);
    const reportData = payload.report || payload;
    const filename = `${datasetName || "report"}_students_report.json`;
    const blob = new Blob([JSON.stringify(reportData, null, 2)], { type: "application/json" });
    const url = URL.createObjectURL(blob);
    const anchor = document.createElement("a");
    anchor.href = url;
    anchor.download = filename;
    document.body.appendChild(anchor);
    anchor.click();
    anchor.remove();
    URL.revokeObjectURL(url);
  }

  async function fetchLatestReport(datasetName) {
    if (!datasetName) {
      if (downloadButton) {
        downloadButton.hidden = true;
        downloadButton.disabled = true;
      }
      studentsState.reportAvailable = false;
      return;
    }
    try {
      const payload = await requestJSON(
        typeof API_ROUTES.latestReport === "function"
          ? API_ROUTES.latestReport(datasetName)
          : `${API_ROUTES.latestReport}?dataset=${encodeURIComponent(datasetName)}`,
      );
      const available = Boolean(payload.available);
      studentsState.reportAvailable = available;
      if (downloadButton) {
        downloadButton.hidden = !available;
        downloadButton.disabled = !available;
      }
      if (available && payload.generated_at) {
        showMessage(
          reportFeedback,
          getMessage(
            "REPORT_AVAILABLE",
            { timestamp: payload.generated_at },
            `Latest report generated at ${payload.generated_at}.`,
          ),
        );
      }
    } catch (error) {
      studentsState.reportAvailable = false;
      if (downloadButton) {
        downloadButton.hidden = true;
        downloadButton.disabled = true;
      }
    }
  }

  async function generateReportOnly() {
    const datasetName = datasetSelect && datasetSelect.value ? datasetSelect.value : null;
    if (!datasetName) {
      showMessage(reportFeedback, getMessage("STUDENTS_SELECT_DATASET", {}, "Select a dataset to generate a report."), true);
      return;
    }
    if (reportButton) {
      reportButton.disabled = true;
    }
    showMessage(reportFeedback, getMessage("REPORT_GENERATING", {}, "Generating report..."));
    try {
      await requestJSON(API_ROUTES.generateReport, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ dataset: datasetName }),
      });
      showMessage(reportFeedback, getMessage("REPORT_READY", {}, "Report generated. You can now download it."));
      await fetchLatestReport(datasetName);
    } catch (error) {
      showMessage(reportFeedback, error.message, true);
    } finally {
      if (reportButton) {
        reportButton.disabled = false;
      }
    }
  }

  async function refreshStudentsOutliers() {
    const datasetName = datasetSelect && datasetSelect.value ? datasetSelect.value : null;
    if (studentsDatasetLabel) {
      studentsDatasetLabel.textContent = datasetName || "None";
    }
    if (downloadButton) {
      downloadButton.hidden = true;
      downloadButton.disabled = true;
    }
    if (!datasetName) {
      studentsState.dataset = null;
      studentsState.metrics = [];
      studentsState.buckets = [];
      studentsState.cards = [];
      updateStudentsMetricSelect([], null);
      renderStudentsBuckets([]);
      showMessage(studentsFeedback, getMessage("STUDENTS_SELECT_DATASET", {}, "Select a dataset to display outliers."), true);
      return;
    }
    showMessage(studentsFeedback, getMessage("STUDENTS_LOADING", {}, "Loading precomputed metrics..."));
    try {
      const params = new URLSearchParams({ dataset: datasetName });
      const data = await requestJSON(`${API_ROUTES.studentsOutliers}?${params.toString()}`);
      const buckets = data.buckets || [];
      const metricOptions = dedupeMetricOptions(data.metrics || [], buckets);

      studentsState.dataset = data.dataset || datasetName;
      studentsState.metrics = metricOptions;
      studentsState.buckets = buckets;
      studentsState.students = data.files || [];
      studentsState.report = data.report || null;
      studentsState.cards = [];

      updateStudentsMetricSelect(metricOptions, studentsState.metricKey);
      renderStudentsBuckets(buckets, studentsState.students);
      showMessage(
        studentsFeedback,
        getMessage("STUDENTS_OUTLIERS_COUNT", { count: studentsState.students.length }, `Classified ${studentsState.students.length} students.`),
        studentsState.students.length === 0,
      );
    } catch (error) {
      studentsState.dataset = datasetName;
      studentsState.metrics = [];
      studentsState.buckets = [];
      studentsState.students = [];
      studentsState.cards = [];
      renderStudentsBuckets([], []);
      showMessage(studentsFeedback, error.message, true);
    }
    await fetchLatestReport(datasetName);
  }

  onReady(() => {
    // Metric select is disabled in classification view
    if (reportButton) {
      reportButton.addEventListener("click", generateReportOnly);
    }
    if (downloadButton) {
      downloadButton.addEventListener("click", async () => {
        const datasetName = datasetSelect && datasetSelect.value ? datasetSelect.value : null;
        if (!datasetName) {
          showMessage(reportFeedback, getMessage("STUDENTS_SELECT_DATASET", {}, "Select a dataset to download a report."), true);
          return;
        }
        try {
          await downloadReport(datasetName);
          showMessage(reportFeedback, getMessage("REPORT_DOWNLOADED", {}, "Report download started."));
        } catch (error) {
          showMessage(reportFeedback, error.message, true);
        }
      });
    }
    refreshStudentsOutliers();
  });
})();
