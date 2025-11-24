(function () {
  const app = window.App || {};
  const { API_ROUTES, utils = {}, onReady = (fn) => fn() } = app;
  const { requestJSON, showMessage, computeQuartiles, describeThreshold, formatMetricValue, openFileInExplorer } =
    utils;

  const datasetSelect = document.getElementById("current-dataset-select");
  const globalAnalysisPanel = document.getElementById("global-analysis");
  const globalAnalysisMetricSelect = document.getElementById("global-analysis-metric");
  const globalAnalysisChart = document.getElementById("global-analysis-chart");
  const globalAnalysisFeedback = document.getElementById("global-analysis-feedback");
  const globalAnalysisDatasetLabel = document.getElementById("global-analysis-dataset");
  const globalAnalysisReferenceName = document.getElementById("global-analysis-reference-name");
  const globalAnalysisReferenceInfo = document.getElementById("global-analysis-reference-info");

  if (!globalAnalysisPanel) return;

  const globalAnalysisState = {
    dataset: null,
    metrics: [],
    files: [],
    metricKey: null,
    reference: null,
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

  function clearGlobalAnalysisChart() {
    if (!globalAnalysisChart) return;
    if (globalAnalysisChart.__plotlyClickHandler && typeof globalAnalysisChart.removeListener === "function") {
      globalAnalysisChart.removeListener("plotly_click", globalAnalysisChart.__plotlyClickHandler);
      globalAnalysisChart.__plotlyClickHandler = null;
    }
    if (window.Plotly) {
      Plotly.purge(globalAnalysisChart);
    } else {
      globalAnalysisChart.innerHTML = "";
    }
  }

  function renderGlobalAnalysisChart() {
    if (!globalAnalysisChart || !globalAnalysisState.metricKey) {
      clearGlobalAnalysisChart();
      return;
    }
    if (!window.Plotly) {
      showMessage(globalAnalysisFeedback, "Plotly library failed to load.", true);
      return;
    }
    const metricKey = globalAnalysisState.metricKey;
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
    if (!points.length) {
      clearGlobalAnalysisChart();
      showMessage(globalAnalysisFeedback, `No values available for ${metricKey}.`, true);
      return;
    }
    const trace = {
      x: points.map((point) => point.x),
      y: points.map((point) => point.y),
      type: "scatter",
      mode: "markers",
    };
    const values = points.map((point) => point.y);
    const { q1, q3, median } = computeQuartiles(values);
    const hasQ1 = Number.isFinite(q1);
    const hasQ3 = Number.isFinite(q3);
    const iqr = hasQ1 && hasQ3 ? q3 - q1 : null;
    const hasIqr = Number.isFinite(iqr) && iqr > 0;
    const lowerFence = hasIqr ? q1 - iqr * 1.5 : null;
    const upperFence = hasIqr ? q3 + iqr * 1.5 : null;
    const pointStatuses = points.map((point) => {
      if (Number.isFinite(upperFence) && point.y > upperFence) {
        return "Au-dessus de Q3 + 1.5×IQR";
      }
      if (hasQ3 && point.y > q3) {
        return "Au-dessus de Q3";
      }
      if (Number.isFinite(lowerFence) && point.y < lowerFence) {
        return "En dessous de Q1 - 1.5×IQR";
      }
      if (hasQ1 && point.y < q1) {
        return "En dessous de Q1";
      }
      return "Dans l'IQR";
    });
    const statusToBucketKey = {
      "Au-dessus de Q3 + 1.5×IQR": "aboveFence",
      "Au-dessus de Q3": "aboveQ3",
      "En dessous de Q1 - 1.5×IQR": "belowFence",
      "En dessous de Q1": "belowQ1",
    };
    const statusComments = pointStatuses.map((status) => {
      const bucketKey = statusToBucketKey[status] || null;
      if (!bucketKey) return "Dans l'intervalle interquartile du dataset.";
      return describeThreshold(metricKey, bucketKey);
    });
    const colors = pointStatuses.map((status) => {
      if (status === "Au-dessus de Q3 + 1.5×IQR") return "#b91c1c";
      if (status === "Au-dessus de Q3") return "#dc2626";
      if (status === "En dessous de Q1 - 1.5×IQR") return "#1e3a8a";
      if (status === "En dessous de Q1") return "#1d4ed8";
      return "#22c55e";
    });
    trace.marker = {
      size: 10,
      color: colors,
      opacity: 0.9,
      line: {
        width: 1,
        color: pointStatuses.map((status) => {
          if (status === "Au-dessus de Q3 + 1.5×IQR") return "#7f1d1d";
          if (status === "Au-dessus de Q3") return "#991b1b";
          if (status === "En dessous de Q1 - 1.5×IQR") return "#111827";
          if (status === "En dessous de Q1") return "#1e40af";
          return "#15803d";
        }),
      },
    };
    trace.customdata = points.map((point, index) => [point.filename, pointStatuses[index], statusComments[index]]);
    trace.hovertemplate = `<b>%{customdata[0]}</b><br>${metricKey}: %{y}<br>Status: %{customdata[1]}<br>%{customdata[2]}<extra></extra>`;

    const shapes = [];
    if (Number.isFinite(lowerFence)) {
      shapes.push({
        type: "line",
        xref: "paper",
        x0: 0,
        x1: 1,
        y0: lowerFence,
        y1: lowerFence,
        line: {
          color: "#0f172a",
          dash: "dot",
          width: 1.2,
        },
      });
    }
    if (Number.isFinite(upperFence)) {
      shapes.push({
        type: "line",
        xref: "paper",
        x0: 0,
        x1: 1,
        y0: upperFence,
        y1: upperFence,
        line: {
          color: "#7f1d1d",
          dash: "dot",
          width: 1.2,
        },
      });
    }
    if (hasQ1) {
      shapes.push({
        type: "line",
        xref: "paper",
        x0: 0,
        x1: 1,
        y0: q1,
        y1: q1,
        line: {
          color: "#1d4ed8",
          dash: "dash",
          width: 1.5,
        },
      });
    }
    if (hasQ3 && (!hasQ1 || q3 !== q1)) {
      shapes.push({
        type: "line",
        xref: "paper",
        x0: 0,
        x1: 1,
        y0: q3,
        y1: q3,
        line: {
          color: "#dc2626",
          dash: "dash",
          width: 1.5,
        },
      });
    }
    if (Number.isFinite(median)) {
      shapes.push({
        type: "line",
        xref: "paper",
        x0: 0,
        x1: 1,
        y0: median,
        y1: median,
        line: {
          color: "#94a3b8",
          dash: "dot",
          width: 1.5,
        },
      });
    }
    const referenceValue = getReferenceMetricValue(metricKey);
    updateReferenceDisplay(metricKey);
    if (Number.isFinite(referenceValue)) {
      shapes.push({
        type: "line",
        xref: "paper",
        x0: 0,
        x1: 1,
        y0: referenceValue,
        y1: referenceValue,
        line: {
          color: "#7e22ce",
          width: 2,
        },
      });
    } else if (!globalAnalysisState.reference) {
      updateReferenceDisplay(metricKey);
    }
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
      shapes,
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
      clearGlobalAnalysisChart();
      updateReferenceDisplay(null);
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
        clearGlobalAnalysisChart();
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
      renderGlobalAnalysisChart();
      showMessage(globalAnalysisFeedback, `Loaded ${globalAnalysisState.files.length} file(s).`);
    } catch (error) {
      globalAnalysisState.metrics = [];
      globalAnalysisState.files = [];
      globalAnalysisState.reference = null;
      globalAnalysisState.metricKey = null;
      updateGlobalAnalysisMetricOptions([]);
      clearGlobalAnalysisChart();
      updateReferenceDisplay(null);
      showMessage(globalAnalysisFeedback, error.message, true);
    }
  }

  onReady(() => {
    if (globalAnalysisMetricSelect) {
      globalAnalysisMetricSelect.addEventListener("change", (event) => {
        globalAnalysisState.metricKey = event.target.value || null;
        renderGlobalAnalysisChart();
      });
    }
    refreshGlobalAnalysis();
  });
})();
