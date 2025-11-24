(function () {
  const app = window.App || {};
  const { API_ROUTES, utils = {}, onReady = (fn) => fn() } = app;
  const {
    requestJSON,
    showMessage,
    formatMetricValue,
    computeStandardDeviation,
    getClusterColor,
    getClusterLabel,
  } = utils;

  const clusteringForm = document.getElementById("clustering-form");
  const clusteringLaunchButton = document.getElementById("launch-clustering");
  const clusteringAlgorithmSelect = document.getElementById("clustering-algorithm");
  const clusteringClusterSlider = document.getElementById("cluster-count");
  const clusteringClusterSliderInitialDisabled = clusteringClusterSlider ? clusteringClusterSlider.disabled : false;
  const clusteringClusterValue = document.getElementById("cluster-count-value");
  const clusteringKMeansParams = document.getElementById("kmeans-params");
  const clusteringKMeansAutoButton = document.getElementById("kmeans-auto-button");
  const clusteringHdbscanParams = document.getElementById("hdbscan-params");
  const clusteringMinClusterSizeInput = document.getElementById("hdbscan-min-cluster-size");
  const clusteringMinSamplesInput = document.getElementById("hdbscan-min-samples");
  const clusteringAutoButton = document.getElementById("hdbscan-auto-button");
  const featureModeSelect = document.getElementById("feature-mode-select");
  const embeddingDimsContainer = document.getElementById("embedding-dims-container");
  const embeddingDimsSlider = document.getElementById("embedding-dims");
  const embeddingDimsValue = document.getElementById("embedding-dims-value");
  const clusteringFeedback = document.getElementById("clustering-feedback");
  const clusteringChart = document.getElementById("clustering-chart");
  const clusteringSummary = document.getElementById("clustering-summary");
  const clusteringMetricSelect = document.getElementById("clustering-metric-select");
  const clusteringMetricChart = document.getElementById("clustering-metric-chart");
  const clusteringMetricFeedback = document.getElementById("clustering-metric-feedback");
  const clusteringMetricDatasetLabel = document.getElementById("clustering-metric-dataset");

  if (!clusteringChart && !clusteringForm && !clusteringLaunchButton) return;

  const clusteringState = {
    dataset: null,
    algorithm: null,
    points: [],
    metrics: [],
    metricKey: null,
    autoKMeans: false,
    autoHdbscan: false,
    featureMode: "metrics",
    embeddingDims: 16,
  };

  function computeClusterMetricStats(clusters = [], metricKeys = []) {
    const stats = {};
    (metricKeys || []).forEach((key) => {
      const values = [];
      clusters.forEach((cluster) => {
        const value = cluster && cluster.metrics ? cluster.metrics[key] : null;
        if (typeof value === "number" && Number.isFinite(value)) {
          values.push(value);
        }
      });
      if (!values.length) return;
      const mean = values.reduce((sum, v) => sum + v, 0) / values.length;
      const variance = values.reduce((sum, v) => sum + (v - mean) ** 2, 0) / values.length;
      const stddev = Math.sqrt(variance);
      stats[key] = { mean, stddev };
    });
    return stats;
  }

  function deriveClusterLabel(cluster, metricStats) {
    if (!cluster || !cluster.metrics) {
      return { title: cluster?.label || "Cluster", reasons: [] };
    }
    const metrics = cluster.metrics;
    const reason = [];

    function zScore(key) {
      const stats = metricStats[key];
      const value = metrics[key];
      if (!stats || typeof value !== "number") return 0;
      const { mean, stddev } = stats;
      if (!stddev || stddev === 0) return 0;
      return (value - mean) / stddev;
    }

    const duplicationZ = Math.max(
      zScore("duplication"),
      ...Object.keys(metrics)
        .filter((k) => k && k.toLowerCase().includes("dup"))
        .map((k) => zScore(k)),
    );
    const ncssZ = Math.max(
      zScore("ncss"),
      ...Object.keys(metrics)
        .filter((k) => k && /loc|lines/i.test(k))
        .map((k) => zScore(k)),
    );
    const functionsZ = Math.max(
      zScore("functions"),
      ...Object.keys(metrics)
        .filter((k) => k && k.toLowerCase().includes("function"))
        .map((k) => zScore(k)),
    );
    const complexityZ = Math.max(
      zScore("ccn"),
      ...Object.keys(metrics)
        .filter((k) => k && /ccn|complex/i.test(k))
        .map((k) => zScore(k)),
    );
    const nestingZ = Math.max(
      zScore("nesting"),
      ...Object.keys(metrics)
        .filter((k) => k && k.toLowerCase().includes("nest"))
        .map((k) => zScore(k)),
    );

    const duplicationHigh = duplicationZ > 0.8;
    const ncssHigh = ncssZ > 0.8;
    const functionsLow = functionsZ < -0.6; // low count
    const complexityHigh = complexityZ > 0.8;
    const nestingHigh = nestingZ > 0.8;

    if (duplicationHigh && (functionsLow || ncssHigh)) {
      reason.push("Heavy duplication with weak factoring");
    }
    if (complexityHigh && nestingHigh) {
      reason.push("Deeply nested and complex logic");
    }
    if (ncssHigh && !duplicationHigh && !complexityHigh) {
      reason.push("Large/verbose implementations");
    }
    if (functionsLow && !duplicationHigh) {
      reason.push("Monolithic structure (few functions)");
    }
    if (!reason.length && (duplicationHigh || complexityHigh || ncssHigh)) {
      reason.push("Elevated metrics — review recommended");
    }

    const title = reason.length ? reason[0] : cluster.label || `Cluster ${cluster.id + 1}`;
    return { title, reasons: reason };
  }

  function toggleFieldsetVisibility(fieldset, visible) {
    if (!fieldset) return;
    fieldset.hidden = !visible;
    fieldset.style.display = visible ? "" : "none";
  }

  function setKMeansAutoMode(enabled) {
    clusteringState.autoKMeans = Boolean(enabled);
    if (clusteringKMeansAutoButton) {
      clusteringKMeansAutoButton.classList.toggle("active", clusteringState.autoKMeans);
      clusteringKMeansAutoButton.textContent = clusteringState.autoKMeans
        ? "Auto k-means activé"
        : "Auto k-means (silhouette)";
    }
    if (clusteringClusterSlider) {
      clusteringClusterSlider.disabled = clusteringClusterSliderInitialDisabled || clusteringState.autoKMeans;
    }
  }

  function updateClusteringAlgorithmState() {
    if (!clusteringAlgorithmSelect) return;
    const isKMeans = (clusteringAlgorithmSelect.value || "kmeans") === "kmeans";
    toggleFieldsetVisibility(clusteringKMeansParams, isKMeans);
    toggleFieldsetVisibility(clusteringHdbscanParams, !isKMeans);
    if (clusteringKMeansAutoButton) {
      clusteringKMeansAutoButton.disabled = !isKMeans;
    }
    if (clusteringAutoButton) {
      clusteringAutoButton.disabled = isKMeans;
    }
    if (!isKMeans && clusteringState.autoKMeans) {
      setKMeansAutoMode(false);
    }
    if (isKMeans && clusteringState.autoHdbscan) {
      setHdbscanAutoMode(false);
    }
  }

  function updateClusterSliderLabel() {
    if (!clusteringClusterSlider || !clusteringClusterValue) return;
    const value = clusteringClusterSlider.value || "0";
    clusteringClusterValue.textContent = `${value} clusters`;
  }

  function updateEmbeddingDimsLabel() {
    if (!embeddingDimsSlider || !embeddingDimsValue) return;
    const value = embeddingDimsSlider.value || "0";
    clusteringState.embeddingDims = Number(value) || clusteringState.embeddingDims;
    embeddingDimsValue.textContent = `${value} dimensions`;
  }

  function updateFeatureConfigState() {
    const mode = featureModeSelect ? featureModeSelect.value : "metrics";
    clusteringState.featureMode = mode || "metrics";
    const showDims = mode === "embeddings" || mode === "both";
    if (embeddingDimsContainer) {
      embeddingDimsContainer.hidden = !showDims;
      embeddingDimsContainer.style.display = showDims ? "" : "none";
    }
    updateEmbeddingDimsLabel();
  }

  function setHdbscanAutoMode(enabled) {
    clusteringState.autoHdbscan = Boolean(enabled);
    if (clusteringAutoButton) {
      clusteringAutoButton.classList.toggle("active", clusteringState.autoHdbscan);
      clusteringAutoButton.textContent = clusteringState.autoHdbscan
        ? "Auto HDBSCAN activé"
        : "Auto-tune HDBSCAN";
    }
    if (clusteringMinClusterSizeInput) {
      clusteringMinClusterSizeInput.disabled = clusteringState.autoHdbscan;
    }
    if (clusteringMinSamplesInput) {
      clusteringMinSamplesInput.disabled = clusteringState.autoHdbscan;
    }
  }

  function sanitizeHdbscanInputs() {
    if (clusteringMinClusterSizeInput) {
      const min = Number(clusteringMinClusterSizeInput.min) || 2;
      const max = Number(clusteringMinClusterSizeInput.max) || 200;
      let value = Number(clusteringMinClusterSizeInput.value);
      if (!Number.isFinite(value) || value < min) value = min;
      if (value > max) value = max;
      clusteringMinClusterSizeInput.value = String(Math.round(value));
    }
    if (clusteringMinSamplesInput) {
      const min = Number(clusteringMinSamplesInput.min) || 1;
      const max = Number(clusteringMinSamplesInput.max) || 200;
      let value = Number(clusteringMinSamplesInput.value);
      if (!Number.isFinite(value) || value < min) value = min;
      if (value > max) value = max;
      clusteringMinSamplesInput.value = String(Math.round(value));
    }
  }

  function updateClusteringMetricDataset(value) {
    if (clusteringMetricDatasetLabel) {
      clusteringMetricDatasetLabel.textContent = value || "None";
    }
  }

  function updateClusteringMetricOptions(metrics = []) {
    if (!clusteringMetricSelect) return;
    clusteringMetricSelect.innerHTML = "";
    if (!metrics.length) {
      const option = document.createElement("option");
      option.value = "";
      option.textContent = "No metrics";
      clusteringMetricSelect.appendChild(option);
      clusteringMetricSelect.disabled = true;
      clusteringState.metricKey = null;
      return;
    }
    metrics.forEach((metricKey) => {
      const option = document.createElement("option");
      option.value = metricKey;
      option.textContent = metricKey;
      clusteringMetricSelect.appendChild(option);
    });
    clusteringMetricSelect.disabled = false;
    const active =
      clusteringState.metricKey && metrics.includes(clusteringState.metricKey)
        ? clusteringState.metricKey
        : metrics[0];
    clusteringState.metricKey = active;
    clusteringMetricSelect.value = active;
  }

  function clearClusteringMetricChart() {
    if (!clusteringMetricChart) return;
    if (window.Plotly) {
      Plotly.purge(clusteringMetricChart);
    } else {
      clusteringMetricChart.innerHTML = "";
    }
  }

  function formatClusteringParameters(algorithm, parameters = {}) {
    if (!parameters || typeof parameters !== "object") return "";
    const parts = [];
    if (parameters.mode === "auto") {
      parts.push("auto");
    }
    if (algorithm === "kmeans") {
      const clusterCount = Number(parameters.cluster_count);
      if (Number.isFinite(clusterCount)) {
        parts.push(`k=${clusterCount}`);
      }
    } else if (algorithm === "hdbscan") {
      const minSize = Number(parameters.min_cluster_size);
      const minSamples = Number(parameters.min_samples);
      if (Number.isFinite(minSize)) {
        parts.push(`min_size=${minSize}`);
      }
      if (Number.isFinite(minSamples)) {
        parts.push(`min_samples=${minSamples}`);
      }
    }
    const silhouette = Number(parameters.silhouette);
    if (Number.isFinite(silhouette)) {
      parts.push(`silhouette=${formatMetricValue(silhouette)}`);
    }
    return parts.length ? ` [${parts.join(" · ")}]` : "";
  }

  function renderClusteringMetricChart() {
    if (!clusteringMetricChart || !clusteringState.metricKey) {
      clearClusteringMetricChart();
      return;
    }
    if (!window.Plotly) {
      showMessage(clusteringMetricFeedback, "Plotly library failed to load.", true);
      return;
    }
    const metricKey = clusteringState.metricKey;
    const points = (clusteringState.points || [])
      .map((point, index) => {
        const metrics = point.metrics || {};
        const value = Number(metrics[metricKey]);
        if (!Number.isFinite(value)) {
          return null;
        }
        return {
          x: index + 1,
          y: value,
          filename: point.path || point.raw_path || `File ${index + 1}`,
          cluster: typeof point.cluster === "number" ? point.cluster : -1,
        };
      })
      .filter(Boolean);
    if (!points.length) {
      clearClusteringMetricChart();
      showMessage(clusteringMetricFeedback, `No values for ${metricKey}.`, true);
      return;
    }
    const values = points.map((point) => point.y);
    const mean = values.reduce((sum, value) => sum + value, 0) / values.length;
    const stddev = computeStandardDeviation(values, mean);
    const hasAverage = Number.isFinite(mean);
    const stddevValid = Number.isFinite(stddev) && stddev > 0;
    const upperThreshold = hasAverage && stddevValid ? mean + stddev * 2 : hasAverage ? mean : null;
    const lowerThreshold = hasAverage && stddevValid ? mean - stddev * 2 : hasAverage ? mean : null;
    const upperBand = hasAverage && stddevValid ? mean + stddev : hasAverage ? mean : null;
    const lowerBand = hasAverage && stddevValid ? mean - stddev : hasAverage ? mean : null;

    const colors = points.map((point) => getClusterColor(point.cluster));
    const trace = {
      x: points.map((point) => point.x),
      y: values,
      type: "scatter",
      mode: "markers",
      marker: {
        size: 11,
        opacity: 0.9,
        color: colors,
        line: { width: 1, color: colors.map((color) => color) },
      },
      customdata: points.map((point) => [point.filename, getClusterLabel(point.cluster)]),
      hovertemplate: `<b>%{customdata[0]}</b><br>${metricKey}: %{y}<br>%{customdata[1]}<extra></extra>`,
    };

    const shapes = [];
    if (hasAverage) {
      shapes.push({
        type: "line",
        xref: "paper",
        x0: 0,
        x1: 1,
        y0: mean,
        y1: mean,
        line: { color: "#94a3b8", dash: "dot", width: 2 },
      });
    }
    if (Number.isFinite(upperThreshold) && upperThreshold !== mean) {
      shapes.push({
        type: "line",
        xref: "paper",
        x0: 0,
        x1: 1,
        y0: upperThreshold,
        y1: upperThreshold,
        line: { color: "#dc2626", dash: "dash", width: 1.5 },
      });
    }
    if (Number.isFinite(lowerThreshold) && lowerThreshold !== mean && lowerThreshold !== upperThreshold) {
      shapes.push({
        type: "line",
        xref: "paper",
        x0: 0,
        x1: 1,
        y0: lowerThreshold,
        y1: lowerThreshold,
        line: { color: "#1d4ed8", dash: "dash", width: 1.5 },
      });
    }
    if (Number.isFinite(upperBand) && upperBand !== mean) {
      shapes.push({
        type: "line",
        xref: "paper",
        x0: 0,
        x1: 1,
        y0: upperBand,
        y1: upperBand,
        line: { color: "#fb923c", dash: "dot", width: 1 },
      });
    }
    if (Number.isFinite(lowerBand) && lowerBand !== mean && lowerBand !== upperBand) {
      shapes.push({
        type: "line",
        xref: "paper",
        x0: 0,
        x1: 1,
        y0: lowerBand,
        y1: lowerBand,
        line: { color: "#60a5fa", dash: "dot", width: 1 },
      });
    }

    const layout = {
      margin: { l: 60, r: 20, t: 30, b: 40 },
      xaxis: { title: "File index", zeroline: false, showgrid: false },
      yaxis: { title: metricKey, rangemode: "tozero" },
      hovermode: "closest",
      showlegend: false,
      shapes,
    };
    Plotly.react(clusteringMetricChart, [trace], layout, { responsive: true, displaylogo: false });
    showMessage(
      clusteringMetricFeedback,
      `Distribution affichée pour ${metricKey} (${points.length} fichier${points.length > 1 ? "s" : ""}).`,
    );
  }

  function renderClusteringSummary(data) {
    if (!clusteringSummary) return;
    clusteringSummary.innerHTML = "<h2>Clusters</h2>";
    const body = document.createElement("div");
    body.className = "clustering-summary-body";
    const clusters = data && Array.isArray(data.clusters) && data.clusters.length ? data.clusters : null;
    if (!clusters) {
      const empty = document.createElement("p");
      empty.textContent = "Aucun cluster disponible pour le moment.";
      body.appendChild(empty);
    } else {
      const metricStats = computeClusterMetricStats(clusters, data.metrics || []);
      const grid = document.createElement("div");
      grid.className = "clustering-summary-grid";
      clusters.forEach((cluster) => {
        const derived = deriveClusterLabel(cluster, metricStats);
        const card = document.createElement("article");
        card.className = "clustering-summary-card";
        const centroidPreview = (cluster.centroid || [])
          .slice(0, 2)
          .map((value) => formatMetricValue(value))
          .join(", ");
        card.innerHTML = `
        <h3>${derived.title || cluster.label || `Cluster ${cluster.id + 1}`}</h3>
        <p>Taille: ${cluster.size}</p>
        <p>Centroïde (aperçu): ${centroidPreview || "-"}</p>
      `;
        if (derived.reasons && derived.reasons.length) {
          const reasons = document.createElement("ul");
          reasons.className = "cluster-reasons";
          derived.reasons.forEach((text) => {
            const li = document.createElement("li");
            li.textContent = text;
            reasons.appendChild(li);
          });
          card.appendChild(reasons);
        }
        const metrics = cluster && typeof cluster.metrics === "object" ? cluster.metrics : null;
        if (metrics && Object.keys(metrics).length) {
          const metricsContainer = document.createElement("div");
          metricsContainer.className = "cluster-metrics";
          const metricsTitle = document.createElement("p");
          metricsTitle.className = "cluster-metrics-title";
          metricsTitle.textContent = "Métriques moyennes";
          metricsContainer.appendChild(metricsTitle);
          const metricList = document.createElement("dl");
          metricList.className = "cluster-metrics-grid";
          const orderedKeys = Array.isArray(data.metrics) && data.metrics.length ? data.metrics : Object.keys(metrics);
          orderedKeys.forEach((key) => {
            if (!(key in metrics)) return;
            const dt = document.createElement("dt");
            dt.textContent = key;
            const dd = document.createElement("dd");
            dd.textContent = formatMetricValue(metrics[key]);
            metricList.appendChild(dt);
            metricList.appendChild(dd);
          });
          const functionsMean = (() => {
            const fnKey = Object.keys(metrics).find((key) => key && key.toLowerCase() === "functions");
            if (!fnKey) return null;
            const value = Number(metrics[fnKey]);
            return Number.isFinite(value) ? value : null;
          })();
          if (functionsMean === 0) {
            const note = document.createElement("p");
            note.className = "cluster-warning-note";
            note.textContent = "Average functions is 0 — likely files with errors or invalid parsing.";
            metricsContainer.appendChild(note);
          }
          metricsContainer.appendChild(metricList);
          card.appendChild(metricsContainer);
        }
        grid.appendChild(card);
      });
      body.appendChild(grid);
    }
    if (data && typeof data.noise === "number" && data.noise > 0) {
      const noise = document.createElement("p");
      noise.className = "clustering-summary-noise";
      noise.textContent = `${data.noise} fichier(s) marqués comme bruit.`;
      body.appendChild(noise);
    }
    clusteringSummary.appendChild(body);
  }

  function deriveMetricKeysFromPoints(points = []) {
    const keys = new Set();
    (points || []).forEach((point) => {
      const metrics = point && point.metrics;
      if (!metrics || typeof metrics !== "object") return;
      Object.keys(metrics).forEach((key) => {
        keys.add(key);
      });
    });
    return Array.from(keys);
  }

  function renderClusteringResults(data) {
    const datasetName = data && data.dataset ? data.dataset : clusteringState.dataset;
    const parameters = (data && data.parameters) || {};
    clusteringState.dataset = datasetName || null;
    clusteringState.algorithm = data && data.algorithm ? data.algorithm : null;
    clusteringState.points = Array.isArray(data && data.points) ? data.points : [];
    const incomingMetrics = Array.isArray(data && data.metrics) ? data.metrics : [];
    const derivedMetrics = deriveMetricKeysFromPoints(clusteringState.points);
    clusteringState.metrics = incomingMetrics.length ? incomingMetrics : derivedMetrics;

    if (data && data.algorithm === "kmeans") {
      setKMeansAutoMode(parameters.mode === "auto");
      const clusterCount = Number(parameters.cluster_count);
      if (clusteringClusterSlider && Number.isFinite(clusterCount)) {
        clusteringClusterSlider.value = String(clusterCount);
        updateClusterSliderLabel();
      }
    }

    updateClusteringMetricDataset(datasetName);
    updateClusteringMetricOptions(clusteringState.metrics);
    if (!clusteringState.metrics.length) {
      showMessage(clusteringMetricFeedback, "Lancez un clustering pour afficher la distribution des métriques.");
    }
    renderClusteringMetricChart();

    if (!data || !Array.isArray(data.points)) {
      if (clusteringChart) {
        clusteringChart.innerHTML = "";
      }
      renderClusteringSummary(data);
      return;
    }

    if (clusteringChart && window.Plotly) {
      const hoverTexts = data.points.map((point) => {
        const lines = [`${point.path}`];
        const metrics = point.metrics || {};
        Object.keys(metrics).forEach((key) => {
          lines.push(`${key}: ${formatMetricValue(metrics[key])}`);
        });
        return lines.join("<br>");
      });
      const trace = {
        type: "scatter",
        mode: "markers",
        x: data.points.map((point) => point.x || 0),
        y: data.points.map((point) => point.y || 0),
        text: hoverTexts,
        marker: {
          size: 12,
          opacity: 0.85,
          color: data.points.map((point) => getClusterColor(point.cluster)),
          line: { width: 1, color: data.points.map((point) => getClusterColor(point.cluster)) },
        },
      };
      const layout = {
        xaxis: { title: (data.axes && data.axes.x) || "Component 1" },
        yaxis: { title: (data.axes && data.axes.y) || "Component 2" },
        margin: { t: 30, r: 10, b: 50, l: 50 },
        paper_bgcolor: "rgba(0,0,0,0)",
        plot_bgcolor: "rgba(0,0,0,0)",
      };
      Plotly.react(clusteringChart, [trace], layout, { responsive: true });
    }

    renderClusteringSummary(data);
  }

  async function runClusteringJob() {
    if (!clusteringLaunchButton) return;
    updateFeatureConfigState();
    updateEmbeddingDimsLabel();
    const algorithm = clusteringAlgorithmSelect ? clusteringAlgorithmSelect.value : "kmeans";
    const payload = {
      algorithm,
      feature_mode: clusteringState.featureMode || "metrics",
      embedding_dims: Number(clusteringState.embeddingDims) || 16,
    };
    const isAutoKMeans = algorithm === "kmeans" && clusteringState.autoKMeans;
    const isAutoHdbscan = algorithm === "hdbscan" && clusteringState.autoHdbscan;
    if (algorithm === "kmeans") {
      setHdbscanAutoMode(false);
      if (isAutoKMeans) {
        payload.auto_kmeans = true;
      } else {
        if (!clusteringClusterSlider) {
          showMessage(clusteringFeedback, "Le paramètre k-means est introuvable.", true);
          return;
        }
        payload.cluster_count = Number(clusteringClusterSlider.value);
      }
    } else {
      setKMeansAutoMode(false);
      if (isAutoHdbscan) {
        payload.auto_hdbscan = true;
      } else {
        sanitizeHdbscanInputs();
        if (!clusteringMinClusterSizeInput || !clusteringMinSamplesInput) {
          showMessage(clusteringFeedback, "Les paramètres HDBSCAN sont requis.", true);
          return;
        }
        const minClusterSize = Number(clusteringMinClusterSizeInput.value);
        const minSamples = Number(clusteringMinSamplesInput.value);
        if (!Number.isFinite(minClusterSize) || !Number.isFinite(minSamples)) {
          showMessage(clusteringFeedback, "Valeurs HDBSCAN invalides.", true);
          return;
        }
        payload.min_cluster_size = minClusterSize;
        payload.min_samples = minSamples;
      }
    }

    showMessage(
      clusteringFeedback,
      isAutoHdbscan
        ? "Recherche automatique des paramètres HDBSCAN..."
        : isAutoKMeans
          ? "Recherche automatique du meilleur k..."
          : "Clustering en cours...",
    );
    clusteringLaunchButton.disabled = true;
    try {
      const result = await requestJSON(API_ROUTES.clustering, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(payload),
      });
      renderClusteringResults(result);
      const pointCount = Array.isArray(result.points) ? result.points.length : 0;
      const paramsDetail = formatClusteringParameters(result.algorithm, result.parameters);
      showMessage(
        clusteringFeedback,
        `Clustering ${result.algorithm} terminé${paramsDetail} (${pointCount} fichier${
          pointCount > 1 ? "s" : ""
        }).`,
      );
    } catch (error) {
      showMessage(clusteringFeedback, error.message, true);
      if (clusteringChart && window.Plotly) {
        Plotly.purge(clusteringChart);
        clusteringChart.innerHTML = "";
      }
    } finally {
      clusteringLaunchButton.disabled = false;
    }
  }

  async function loadCachedClustering() {
    if (!clusteringChart && !clusteringForm) return;
    try {
      const data = await requestJSON(API_ROUTES.clusteringLast);
      renderClusteringResults(data);
      const pointCount = Array.isArray(data.points) ? data.points.length : 0;
      const paramsDetail = formatClusteringParameters(data.algorithm, data.parameters);
      showMessage(
        clusteringFeedback,
        `Clustering ${data.algorithm || ""} chargé${paramsDetail} (${pointCount} fichier${
          pointCount > 1 ? "s" : ""
        }).`,
      );
    } catch (error) {
      // No cached clustering available; stay silent.
    }
  }

  function initClusteringUI() {
    if (clusteringForm) {
      clusteringForm.addEventListener("submit", (event) => event.preventDefault());
    }
    if (featureModeSelect) {
      featureModeSelect.addEventListener("change", updateFeatureConfigState);
      updateFeatureConfigState();
    }
    if (embeddingDimsSlider) {
      embeddingDimsSlider.addEventListener("input", updateEmbeddingDimsLabel);
      updateEmbeddingDimsLabel();
    }
    if (clusteringAlgorithmSelect) {
      clusteringAlgorithmSelect.addEventListener("change", updateClusteringAlgorithmState);
      updateClusteringAlgorithmState();
    }
    if (clusteringClusterSlider) {
      clusteringClusterSlider.addEventListener("input", updateClusterSliderLabel);
      updateClusterSliderLabel();
    }
    setKMeansAutoMode(clusteringState.autoKMeans);
    if (clusteringKMeansAutoButton) {
      clusteringKMeansAutoButton.addEventListener("click", () => {
        if (clusteringAlgorithmSelect) {
          clusteringAlgorithmSelect.value = "kmeans";
          updateClusteringAlgorithmState();
        }
        setKMeansAutoMode(!clusteringState.autoKMeans);
        const message = clusteringState.autoKMeans
          ? "Mode automatique k-means activé (silhouette score)."
          : "Mode automatique k-means désactivé.";
        showMessage(clusteringFeedback, message);
      });
    }
    if (clusteringMinClusterSizeInput) {
      clusteringMinClusterSizeInput.addEventListener("change", sanitizeHdbscanInputs);
    }
    if (clusteringMinSamplesInput) {
      clusteringMinSamplesInput.addEventListener("change", sanitizeHdbscanInputs);
    }
    if (clusteringAutoButton) {
      clusteringAutoButton.addEventListener("click", () => {
        if (clusteringAlgorithmSelect) {
          clusteringAlgorithmSelect.value = "hdbscan";
          updateClusteringAlgorithmState();
        }
        setHdbscanAutoMode(!clusteringState.autoHdbscan);
        const message = clusteringState.autoHdbscan
          ? "Mode automatique HDBSCAN activé, paramètres verrouillés."
          : "Mode automatique désactivé.";
        showMessage(clusteringFeedback, message);
      });
    }
    sanitizeHdbscanInputs();
    if (clusteringMetricSelect) {
      clusteringMetricSelect.addEventListener("change", (event) => {
        clusteringState.metricKey = event.target.value || null;
        renderClusteringMetricChart();
      });
    }
    if (clusteringLaunchButton) {
      clusteringLaunchButton.addEventListener("click", runClusteringJob);
    }
    void loadCachedClustering();
  }

  onReady(initClusteringUI);
})();
