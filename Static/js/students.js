(function () {
  const app = window.App || {};
  const { API_ROUTES, utils = {}, onReady = (fn) => fn() } = app;
  const { requestJSON, showMessage, formatMetricValue } = utils;

  const datasetSelect = document.getElementById("current-dataset-select");
  const studentsPanel = document.getElementById("students-page");
  const studentsGrid = document.getElementById("students-metrics-grid");
  const studentsFeedback = document.getElementById("students-feedback");
  const studentsDatasetLabel = document.getElementById("students-dataset");
  const studentsMetricSelect = document.getElementById("students-metric-select");
  const reportButton = document.getElementById("students-generate-report");
  const reportFeedback = document.getElementById("students-report-feedback");
  const bucketTemplate = document.getElementById("students-bucket-template");

  if (!studentsPanel) return;

  const studentsState = {
    dataset: null,
    metrics: [],
    buckets: [],
    metricKey: null,
    cards: [],
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

  function updateStudentsMetricSelect(metrics = [], activeKey = null) {
    if (!studentsMetricSelect) return;
    studentsMetricSelect.innerHTML = "";
    if (!metrics.length) {
      const placeholder = document.createElement("option");
      placeholder.value = "";
      placeholder.textContent = "Aucune métrique";
      studentsMetricSelect.appendChild(placeholder);
      studentsMetricSelect.disabled = true;
      return;
    }
    metrics.forEach((metric) => {
      const option = document.createElement("option");
      option.value = metric.key;
      option.textContent = metric.label || metric.key;
      studentsMetricSelect.appendChild(option);
    });
    studentsMetricSelect.disabled = false;
    const chosen = activeKey && metrics.some((m) => m.key === activeKey) ? activeKey : metrics[0].key;
    studentsMetricSelect.value = chosen;
    studentsState.metricKey = chosen;
  }

  function buildBucketCard(bucket) {
    if (!bucketTemplate || !studentsGrid) return null;
    const clone = bucketTemplate.content.cloneNode(true);
    const card = clone.querySelector(".students-metric-card");
    if (!card) return null;
    card.dataset.metric = bucket.key;

    const title = card.querySelector(".students-metric-title");
    const meta = card.querySelector(".students-metric-meta");
    if (title) title.textContent = bucket.label || bucket.key;
    if (meta) {
      const medianPart = Number.isFinite(bucket.median) ? ` • Médiane: ${formatMetricValue(bucket.median)}` : "";
      const fencesPart =
        Number.isFinite(bucket.lowerFence) && Number.isFinite(bucket.upperFence)
          ? ` • Bornes: ${formatMetricValue(bucket.lowerFence)} / ${formatMetricValue(bucket.upperFence)}`
          : "";
      meta.textContent = `Q1: ${formatMetricValue(bucket.q1)} • Q3: ${formatMetricValue(bucket.q3)}${medianPart}${fencesPart}`;
    }

    const groupKeys = ["aboveFence", "aboveQ3", "belowQ1", "belowFence"];
    groupKeys.forEach((groupKey) => {
      const column = card.querySelector(`.students-bucket[data-group='${groupKey}']`);
      if (!column) return;
      const comment = column.querySelector(".students-threshold-comment");
      if (comment) {
        const comments = bucket.comments || {};
        comment.textContent = comments[groupKey] || comment.textContent || "";
      }
      const list = column.querySelector(".students-bucket-list");
      if (!list) return;
      list.innerHTML = "";
      const items = (bucket.groups && bucket.groups[groupKey]) || [];
      if (!items.length) {
        const empty = document.createElement("li");
        empty.textContent = "Aucun fichier";
        list.appendChild(empty);
        return;
      }
      items.forEach((item) => {
        const li = document.createElement("li");
        const name = document.createElement("span");
        name.textContent = item.filename;
        const valueEl = document.createElement("span");
        valueEl.className = "metric-value";
        valueEl.textContent = formatMetricValue(item.value);
        li.appendChild(name);
        li.appendChild(valueEl);
        list.appendChild(li);
      });
    });

    return card;
  }

  function renderStudentsBuckets(metricBuckets = []) {
    if (!studentsGrid) return;
    studentsGrid.innerHTML = "";
    studentsGrid.classList.toggle("single-bucket", metricBuckets.length === 1);
    if (!metricBuckets.length) {
      const empty = document.createElement("p");
      empty.className = "students-empty";
      empty.textContent = "Aucun fichier en dehors de la plage Q1-Q3.";
      studentsGrid.appendChild(empty);
      return;
    }

    const cards = [];
    metricBuckets.forEach((bucket) => {
      const card = buildBucketCard(bucket);
      if (card) {
        card.id = `metric-${bucket.key}`;
        studentsGrid.appendChild(card);
        cards.push({ metricKey: bucket.key, element: card });
      }
    });

    studentsState.cards = cards;
  }

  function filterVisibleCards(metricKey) {
    if (!studentsState.cards || !studentsState.cards.length) return;
    const activeKey = metricKey || null;
    studentsState.cards.forEach((card) => {
      const show = !activeKey || card.metricKey === activeKey;
      card.element.classList.toggle("is-hidden", !show);
    });
    const visibleCount = studentsState.cards.filter((card) => !card.element.classList.contains("is-hidden")).length;
    studentsGrid.classList.toggle("single-bucket", visibleCount === 1);
    return visibleCount;
  }

  async function downloadReport(datasetName) {
    const downloadUrl =
      typeof API_ROUTES.downloadReport === "function"
        ? API_ROUTES.downloadReport(datasetName)
        : `${API_ROUTES.downloadReport}?dataset=${encodeURIComponent(datasetName)}`;
    const response = await fetch(downloadUrl);
    if (!response.ok) {
      let detail = response.statusText;
      try {
        const payload = await response.json();
        detail = payload.detail || payload.message || detail;
      } catch (err) {
        // ignore JSON parse errors
      }
      throw new Error(detail);
    }
    const blob = await response.blob();
    const disposition = response.headers.get("content-disposition") || "";
    let filename = `${datasetName || "report"}_students_report.md`;
    const match = disposition.match(/filename=\"?([^\";]+)\"?/i);
    if (match && match[1]) {
      filename = match[1];
    }
    const url = URL.createObjectURL(blob);
    const anchor = document.createElement("a");
    anchor.href = url;
    anchor.download = filename;
    document.body.appendChild(anchor);
    anchor.click();
    anchor.remove();
    URL.revokeObjectURL(url);
  }

  async function generateAndDownloadReport() {
    const datasetName = datasetSelect && datasetSelect.value ? datasetSelect.value : null;
    if (!datasetName) {
      showMessage(reportFeedback, "Sélectionnez un dataset pour générer un rapport.", true);
      return;
    }
    if (reportButton) {
      reportButton.disabled = true;
    }
    showMessage(reportFeedback, "Génération du rapport en cours...");
    try {
      await requestJSON(API_ROUTES.generateReport, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ dataset: datasetName }),
      });
      await downloadReport(datasetName);
      showMessage(reportFeedback, "Rapport généré et téléchargé.");
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
      studentsDatasetLabel.textContent = datasetName || "Aucun";
    }
    if (!datasetName) {
      studentsState.dataset = null;
      studentsState.metrics = [];
      studentsState.buckets = [];
      studentsState.cards = [];
      updateStudentsMetricSelect([], null);
      renderStudentsBuckets([]);
      showMessage(studentsFeedback, "Sélectionnez un dataset pour afficher les écarts.", true);
      return;
    }
    showMessage(studentsFeedback, "Chargement des métriques pré-calculées...");
    try {
      const params = new URLSearchParams({ dataset: datasetName });
      const data = await requestJSON(`${API_ROUTES.studentsOutliers}?${params.toString()}`);
      const buckets = data.buckets || [];
      const metricOptions = dedupeMetricOptions(data.metrics || [], buckets);

      studentsState.dataset = data.dataset || datasetName;
      studentsState.metrics = metricOptions;
      studentsState.buckets = buckets;
      studentsState.cards = [];

      updateStudentsMetricSelect(metricOptions, studentsState.metricKey);
      renderStudentsBuckets(buckets);
      const visibleCount = filterVisibleCards(studentsState.metricKey);
      const targetCount = visibleCount ?? studentsState.cards.length;
      showMessage(
        studentsFeedback,
        targetCount
          ? `Affichage des écarts pour ${targetCount} carte${targetCount > 1 ? "s" : ""}.`
          : "Aucun fichier en dehors de la plage Q1-Q3.",
        targetCount === 0,
      );
    } catch (error) {
      studentsState.dataset = datasetName;
      studentsState.metrics = [];
      studentsState.buckets = [];
      studentsState.cards = [];
      renderStudentsBuckets([]);
      showMessage(studentsFeedback, error.message, true);
    }
  }

  onReady(() => {
    if (studentsMetricSelect) {
      studentsMetricSelect.addEventListener("change", (event) => {
        const value = event && event.target ? event.target.value : null;
        studentsState.metricKey = value || null;
        const visibleCount = filterVisibleCards(studentsState.metricKey);
        const target = studentsState.cards?.find((card) => card.metricKey === studentsState.metricKey);
        if (target && target.element && typeof target.element.scrollIntoView === "function") {
          target.element.scrollIntoView({ behavior: "smooth", block: "start" });
        }
        showMessage(
          studentsFeedback,
          visibleCount
            ? `Affichage des écarts pour ${visibleCount} carte${visibleCount > 1 ? "s" : ""}.`
            : "Aucune carte pour cette métrique.",
          !visibleCount,
        );
      });
    }
    if (reportButton) {
      reportButton.addEventListener("click", generateAndDownloadReport);
    }
    refreshStudentsOutliers();
  });
})();
