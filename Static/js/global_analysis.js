(function () {
  const app = window.App || {};
  app.__initialized = app.__initialized || {};
  if (app.__initialized.globalAnalysis) return;
  app.__initialized.globalAnalysis = true;
  window.App = app;

  const { API_ROUTES, utils = {}, onReady = (fn) => fn() } = app;
  const { requestJSON, showMessage, computeQuartiles, describeThreshold, formatMetricValue, openFileInExplorer } =
    utils;

  const datasetSelect = document.getElementById("current-dataset-select");
  const globalAnalysisPanel = document.getElementById("global-analysis");
  const globalAnalysisMetricSelect = document.getElementById("global-analysis-metric");
  const globalAnalysisChart = document.getElementById("global-analysis-chart");
  const globalAnalysisHistogram = document.getElementById("global-analysis-histogram");
  const globalAnalysisFeedback = document.getElementById("global-analysis-feedback");
  const globalAnalysisDatasetLabel = document.getElementById("global-analysis-dataset");
  const globalAnalysisReferenceName = document.getElementById("global-analysis-reference-name");
  const globalAnalysisReferenceInfo = document.getElementById("global-analysis-reference-info");
  const globalAnalysisThresholds = document.getElementById("global-analysis-thresholds");
  const globalAnalysisThresholdsMetric = document.getElementById("global-analysis-thresholds-metric");
  const globalAnalysisThresholdValues = {
    mean: document.getElementById("global-analysis-threshold-mean"),
    q1: document.getElementById("global-analysis-threshold-q1"),
    median: document.getElementById("global-analysis-threshold-median"),
    q3: document.getElementById("global-analysis-threshold-q3"),
    lowerFence: document.getElementById("global-analysis-threshold-lower-fence"),
    upperFence: document.getElementById("global-analysis-threshold-upper-fence"),
    reference: document.getElementById("global-analysis-threshold-reference"),
  };

  if (!globalAnalysisPanel) return;

  const globalAnalysisState = {
    dataset: null,
    metrics: [],
    files: [],
    metricKey: null,
    reference: null,
  };

  const STATUS_TO_BUCKET = {
    "Above Q3 + 1.5×IQR": "aboveFence",
    "Above Q3": "aboveQ3",
    "Below Q1 - 1.5×IQR": "belowFence",
    "Below Q1": "belowQ1",
  };

  function updateGlobalAnalysisMetricOptions(metrics = []) {
    if (!globalAnalysisMetricSelect) return;
    globalAnalysisMetricSelect.innerHTML = "";
    if (!metrics.length) {
      const option = document.createElement("option");
      option.value = "";
      option.textContent = "No metrics available";
      globalAnalysisMetricSelect.appendChild(option);
      globalAnalysisMetricSelect.disabled = true;
      return;
    }
    metrics.forEach((metric) => {
      const option = document.createElement("option");
      option.value = metric.key;
      option.textContent = metric.label || metric.key;
      globalAnalysisMetricSelect.appendChild(option);
    });
    globalAnalysisMetricSelect.disabled = false;
    const activeKey = globalAnalysisState.metricKey || metrics[0].key;
    globalAnalysisMetricSelect.value = activeKey;
    globalAnalysisState.metricKey = activeKey;
  }

  function getMetricDefinition(metricKey) {
    if (!metricKey) return null;
    return (globalAnalysisState.metrics || []).find((metric) => metric.key === metricKey) || null;
  }

  function getReferenceMetricValue(metricKey) {
    if (!globalAnalysisState.reference || !metricKey) return null;
    const metrics = globalAnalysisState.reference.metrics || {};
    const value = metrics[metricKey];
    return Number.isFinite(value) ? value : null;
  }

  function updateReferenceDisplay(metricKey) {
    if (!globalAnalysisReferenceName || !globalAnalysisReferenceInfo) return;
    if (!globalAnalysisState.reference) {
      globalAnalysisReferenceName.textContent = "None";
      globalAnalysisReferenceInfo.textContent = "";
      return;
    }
    globalAnalysisReferenceName.textContent = globalAnalysisState.reference.filename || "Unknown";
    const value = getReferenceMetricValue(metricKey);
    if (Number.isFinite(value)) {
      globalAnalysisReferenceInfo.textContent = `${metricKey}: ${value.toFixed(2)}`;
    } else {
      globalAnalysisReferenceInfo.textContent = `No value for ${metricKey}.`;
    }
  }

  function updateThresholdDisplay(dataset) {
    if (!globalAnalysisThresholds) return;
    const metricDefinition = dataset?.metricKey ? getMetricDefinition(dataset.metricKey) : null;
    const metricLabel = dataset?.metricKey ? metricDefinition?.label || dataset.metricKey : "No metric selected";
    if (globalAnalysisThresholdsMetric) {
      globalAnalysisThresholdsMetric.textContent = metricLabel;
    }
    const thresholds = dataset?.thresholds || {};
    const valueMap = {
      mean: thresholds.mean,
      q1: thresholds.q1,
      median: thresholds.median,
      q3: thresholds.q3,
      lowerFence: thresholds.lowerFence,
      upperFence: thresholds.upperFence,
      reference: thresholds.referenceValue,
    };
    Object.entries(globalAnalysisThresholdValues).forEach(([key, element]) => {
      if (!element) return;
      const value = valueMap[key];
      element.textContent = Number.isFinite(value) ? formatMetricValue(value) : "-";
    });
  }

  function clearPlot(plotElement) {
    if (!plotElement) return;
    if (plotElement.__plotlyClickHandler && typeof plotElement.removeListener === "function") {
      plotElement.removeListener("plotly_click", plotElement.__plotlyClickHandler);
      plotElement.__plotlyClickHandler = null;
    }
    if (window.Plotly) {
      Plotly.purge(plotElement);
    } else {
      plotElement.innerHTML = "";
    }
  }

  function computeThresholds(values = [], metricKey) {
    if (!values.length) {
      return {
        mean: null,
        q1: null,
        q3: null,
        median: null,
        lowerFence: null,
        upperFence: null,
        referenceValue: getReferenceMetricValue(metricKey),
      };
    }
    const mean = values.reduce((sum, value) => sum + value, 0) / values.length;
    const { q1, q3, median } = computeQuartiles(values);
    const hasQ1 = Number.isFinite(q1);
    const hasQ3 = Number.isFinite(q3);
    const iqr = hasQ1 && hasQ3 ? q3 - q1 : null;
    const hasIqr = Number.isFinite(iqr) && iqr > 0;
    const lowerFence = hasIqr ? q1 - iqr * 1.5 : null;
    const upperFence = hasIqr ? q3 + iqr * 1.5 : null;

    return {
      mean,
      q1,
      q3,
      median,
      lowerFence,
      upperFence,
      referenceValue: getReferenceMetricValue(metricKey),
    };
  }

  function computeKDE(values = [], options = {}) {
    if (!values.length) return { x: [], y: [] };
    const sorted = [...values].sort((a, b) => a - b);
    const n = sorted.length;
    const mean = sorted.reduce((sum, value) => sum + value, 0) / n;
    const variance = sorted.reduce((sum, value) => sum + (value - mean) ** 2, 0) / n;
    const std = Math.sqrt(variance) || 1e-6;
    const min = options.min ?? sorted[0];
    const max = options.max ?? sorted[n - 1];
    const range = max - min || Math.max(std, 1);
    const bandwidth = options.bandwidth || 1.06 * std * n ** -0.2 || range / 20 || 1;
    const steps = options.steps || 60;
    const x = [];
    const y = [];
    for (let i = 0; i < steps; i += 1) {
      const value = min + (range * i) / Math.max(steps - 1, 1);
      let sum = 0;
      for (const v of sorted) {
        const u = (value - v) / bandwidth;
        sum += Math.exp(-0.5 * u * u);
      }
      const density = sum / (n * bandwidth * Math.sqrt(2 * Math.PI));
      x.push(value);
      y.push(density);
    }
    return { x, y };
  }

  function getPointStatus(value, thresholds) {
    const { q1, q3, lowerFence, upperFence } = thresholds || {};
    const hasQ1 = Number.isFinite(q1);
    const hasQ3 = Number.isFinite(q3);
    if (Number.isFinite(upperFence) && value > upperFence) {
      return "Above Q3 + 1.5×IQR";
    }
    if (hasQ3 && value > q3) {
      return "Above Q3";
    }
    if (Number.isFinite(lowerFence) && value < lowerFence) {
      return "Below Q1 - 1.5×IQR";
    }
    if (hasQ1 && value < q1) {
      return "Below Q1";
    }
    return "Within IQR";
  }

  function prepareMetricDataset(metricKey) {
    const points = (globalAnalysisState.files || [])
      .map((file, index) => {
        const metrics = file.metrics || {};
        const value = Number(metrics[metricKey]);
        if (!Number.isFinite(value)) {
          return null;
        }
        return {
          x: index + 1,
          y: value,
          filename: file.path || file.raw_path || `File ${index + 1}`,
        };
      })
      .filter(Boolean);
    const values = points.map((point) => point.y);
    const thresholds = computeThresholds(values, metricKey);
    const pointStatuses = points.map((point) => getPointStatus(point.y, thresholds));
    const statusComments = pointStatuses.map((status) => {
      const bucketKey = STATUS_TO_BUCKET[status] || null;
      if (!bucketKey) return "Within the dataset's interquartile range.";
      return describeThreshold(metricKey, bucketKey);
    });
    return { metricKey, points, values, thresholds, pointStatuses, statusComments };
  }

  function buildThresholdShapes(thresholds, orientation = "horizontal") {
    const shapes = [];
    if (!thresholds) return shapes;

    const isVertical = orientation === "vertical";
    const baseCoords = (value) =>
      isVertical
        ? { xref: "x", yref: "paper", x0: value, x1: value, y0: 0, y1: 1 }
        : { xref: "paper", yref: "y", x0: 0, x1: 1, y0: value, y1: value };

    const addShape = (value, color, dash, width) => {
      if (!Number.isFinite(value)) return;
      shapes.push({
        type: "line",
        ...baseCoords(value),
        line: {
          color,
          dash,
          width,
        },
      });
    };

    addShape(thresholds.lowerFence, "#0f172a", "dot", 1.2);
    addShape(thresholds.upperFence, "#7f1d1d", "dot", 1.2);
    addShape(thresholds.q1, "#1d4ed8", "dash", 1.5);
    if (!Number.isFinite(thresholds.q1) || thresholds.q1 !== thresholds.q3) {
      addShape(thresholds.q3, "#dc2626", "dash", 1.5);
    }
    addShape(thresholds.median, "#94a3b8", "dot", 1.5);
    addShape(thresholds.referenceValue, "#7e22ce", "solid", 2);

    return shapes;
  }

  function renderScatterPlot(dataset) {
    if (!globalAnalysisChart || !dataset) return;

    const { metricKey, points, pointStatuses, statusComments, thresholds } = dataset;
    const trace = {
      x: points.map((point) => point.x),
      y: points.map((point) => point.y),
      type: "scatter",
      mode: "markers",
    };

    const colors = pointStatuses.map((status) => {
      if (status === "Above Q3 + 1.5×IQR") return "#b91c1c";
      if (status === "Above Q3") return "#dc2626";
      if (status === "Below Q1 - 1.5×IQR") return "#1e3a8a";
      if (status === "Below Q1") return "#1d4ed8";
      return "#22c55e";
    });
    trace.marker = {
      size: 10,
      color: colors,
      opacity: 0.9,
      line: {
        width: 1,
        color: pointStatuses.map((status) => {
          if (status === "Above Q3 + 1.5×IQR") return "#7f1d1d";
          if (status === "Above Q3") return "#991b1b";
          if (status === "Below Q1 - 1.5×IQR") return "#111827";
          if (status === "Below Q1") return "#1e40af";
          return "#15803d";
        }),
      },
    };
    trace.customdata = points.map((point, index) => [point.filename, pointStatuses[index], statusComments[index]]);
    trace.hovertemplate = `<b>%{customdata[0]}</b><br>${metricKey}: %{y}<br>Status: %{customdata[1]}<br>%{customdata[2]}<extra></extra>`;

    const layout = {
      margin: { l: 60, r: 20, t: 30, b: 40 },
      xaxis: {
        title: "File index",
        zeroline: false,
        showgrid: false,
      },
      yaxis: {
        title: metricKey,
        rangemode: "tozero",
      },
      hovermode: "closest",
      showlegend: false,
      shapes: buildThresholdShapes(thresholds, "horizontal"),
    };

    Plotly.react(globalAnalysisChart, [trace], layout, { responsive: true, displaylogo: false });
    if (globalAnalysisChart.__plotlyClickHandler && typeof globalAnalysisChart.removeListener === "function") {
      globalAnalysisChart.removeListener("plotly_click", globalAnalysisChart.__plotlyClickHandler);
    }
    if (typeof globalAnalysisChart.on === "function") {
      const clickHandler = (event) => {
        if (!event || !event.points || !event.points.length) {
          return;
        }
        const point = event.points[0];
        const filename = Array.isArray(point.customdata) ? point.customdata[0] : point.customdata;
        openFileInExplorer(filename);
      };
      globalAnalysisChart.on("plotly_click", clickHandler);
      globalAnalysisChart.__plotlyClickHandler = clickHandler;
    }
  }

  function renderHistogramPlot(dataset) {
    if (!globalAnalysisHistogram || !dataset) return;
    const { metricKey, values, thresholds } = dataset;
    const nbinsx = values.length ? Math.min(30, Math.max(5, Math.ceil(Math.sqrt(values.length)))) : null;
    const density = computeKDE(values);
    const histogramTrace = {
      x: values,
      type: "histogram",
      nbinsx,
      histnorm: "probability density",
      marker: {
        color: "#0ea5e9",
        line: { color: "#0f172a", width: 1 },
      },
      opacity: 0.9,
      hovertemplate: `${metricKey}: %{x}<br>Density: %{y:.3f}<extra></extra>`,
    };
    const kdeTrace = {
      x: density.x,
      y: density.y,
      type: "scatter",
      mode: "lines",
      line: { color: "#6b21a8", width: 2 },
      hovertemplate: `${metricKey}: %{x}<br>Density: %{y:.3f}<extra></extra>`,
    };
    const layout = {
      margin: { l: 60, r: 20, t: 30, b: 40 },
      bargap: 0.08,
      xaxis: {
        title: metricKey,
        zeroline: false,
      },
      yaxis: {
        title: "Density",
        rangemode: "tozero",
      },
      showlegend: false,
      shapes: buildThresholdShapes(thresholds, "vertical"),
    };
    Plotly.react(globalAnalysisHistogram, [histogramTrace, kdeTrace], layout, { responsive: true, displaylogo: false });
  }

  function renderGlobalAnalysisCharts() {
    if (!globalAnalysisState.metricKey) {
      clearPlot(globalAnalysisChart);
      clearPlot(globalAnalysisHistogram);
      updateThresholdDisplay(null);
      return;
    }
    const dataset = prepareMetricDataset(globalAnalysisState.metricKey);
    updateThresholdDisplay(dataset);
    updateReferenceDisplay(dataset ? dataset.metricKey : null);
    if (!window.Plotly) {
      showMessage(globalAnalysisFeedback, "Plotly library failed to load.", true);
      return;
    }
    if (!dataset || !dataset.points.length) {
      clearPlot(globalAnalysisChart);
      clearPlot(globalAnalysisHistogram);
      showMessage(globalAnalysisFeedback, `No values available for ${globalAnalysisState.metricKey}.`, true);
      return;
    }
    renderScatterPlot(dataset);
    renderHistogramPlot(dataset);
  }

  async function refreshGlobalAnalysis() {
    const datasetName = datasetSelect && datasetSelect.value ? datasetSelect.value : null;
    if (globalAnalysisDatasetLabel) {
      globalAnalysisDatasetLabel.textContent = datasetName || "None";
    }
    if (!datasetName) {
      globalAnalysisState.dataset = null;
      globalAnalysisState.metrics = [];
      globalAnalysisState.files = [];
      globalAnalysisState.metricKey = null;
      globalAnalysisState.reference = null;
      updateGlobalAnalysisMetricOptions([]);
      clearPlot(globalAnalysisChart);
      clearPlot(globalAnalysisHistogram);
      updateReferenceDisplay(null);
      updateThresholdDisplay(null);
      showMessage(globalAnalysisFeedback, "Select a dataset to load the analysis.");
      return;
    }
    showMessage(globalAnalysisFeedback, "Loading metrics...");
    try {
      const params = new URLSearchParams({ dataset: datasetName });
      const data = await requestJSON(`${API_ROUTES.globalMetrics}?${params.toString()}`);
      globalAnalysisState.dataset = data.dataset || datasetName;
      globalAnalysisState.metrics = data.metrics || [];
      globalAnalysisState.files = data.files || [];
      globalAnalysisState.reference = data.reference || null;
      if (!globalAnalysisState.metrics.length || !globalAnalysisState.files.length) {
        globalAnalysisState.metricKey = null;
        updateGlobalAnalysisMetricOptions([]);
        clearPlot(globalAnalysisChart);
        clearPlot(globalAnalysisHistogram);
        updateReferenceDisplay(null);
        updateThresholdDisplay(null);
        showMessage(globalAnalysisFeedback, "No file metrics available.", true);
        return;
      }
      if (
        !globalAnalysisState.metricKey ||
        !globalAnalysisState.metrics.some((metric) => metric.key === globalAnalysisState.metricKey)
      ) {
        globalAnalysisState.metricKey = globalAnalysisState.metrics[0].key;
      }
      updateGlobalAnalysisMetricOptions(globalAnalysisState.metrics);
      renderGlobalAnalysisCharts();
      showMessage(globalAnalysisFeedback, `Loaded ${globalAnalysisState.files.length} file(s).`);
    } catch (error) {
      globalAnalysisState.metrics = [];
      globalAnalysisState.files = [];
      globalAnalysisState.reference = null;
      globalAnalysisState.metricKey = null;
      updateGlobalAnalysisMetricOptions([]);
      clearPlot(globalAnalysisChart);
      clearPlot(globalAnalysisHistogram);
      updateReferenceDisplay(null);
      updateThresholdDisplay(null);
      showMessage(globalAnalysisFeedback, error.message, true);
    }
  }

  onReady(() => {
    if (globalAnalysisMetricSelect) {
      globalAnalysisMetricSelect.addEventListener("change", (event) => {
        globalAnalysisState.metricKey = event.target.value || null;
        renderGlobalAnalysisCharts();
      });
    }
    refreshGlobalAnalysis();
  });
})();
