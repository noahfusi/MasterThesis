(function () {
  const app = window.App || {};
  const { API_ROUTES, utils = {}, onReady = (fn) => fn(), taskSocket = {} } = app;
  const subscribeToTaskEvents = taskSocket.subscribe || (() => () => {});
  const {
    requestJSON,
    showMessage,
    formatMetricValue,
    computeQuartiles,
    describeThreshold,
    getClusterColor,
    getClusterLabel,
    getMessage,
  } = utils;

  const clusteringForm = document.querySelector(".theme-config-form"); // representative form for event prevention
  const clusteringLaunchButton = document.getElementById("launch-clustering");
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
  const clusteringMetadataCard = document.getElementById("clustering-metadata-card");
  const clusteringMetadata = document.getElementById("clustering-metadata");
  const clusteringMembershipFilter = document.getElementById("clustering-membership-filter");
  const themeTabsContainer = document.getElementById("clustering-theme-tabs");
  const themeMetricsContainer = document.getElementById("clustering-theme-metrics");
  const activeThemeLabel = document.getElementById("clustering-active-theme");
  const activeThemeChip = document.getElementById("clustering-active-chip");
  const progressBanner = document.getElementById("clustering-progress-banner");
  const progressText = document.getElementById("clustering-progress-text");
  const themeConfigCards = document.querySelectorAll(".theme-config-card");
  const clusteringDescriptionToggle = document.getElementById("clustering-llm-toggle");

  const CLUSTERING_THEMES = Object.entries(app.clusteringThemes || {}).map(([id, payload]) => ({
    id,
    label: payload && payload.label ? payload.label : id,
    metrics: (payload && payload.metrics) || [],
    description: payload && payload.description ? payload.description : "",
  }));
  if (!CLUSTERING_THEMES.length) {
    CLUSTERING_THEMES.push(
      {
        id: "complexity",
        label: "Complexity and logic structure",
        metrics: ["CCN", "Max nesting depth", "If/NCSS", "Loops/NCSS"],
        description: "Focus on branching depth and control flow.",
      },
      {
        id: "size",
        label: "Size and duplication",
        metrics: ["NCSS", "Duplication (%)"],
        description: "Highlight small vs. oversized files and duplication.",
      },
      {
        id: "functions",
        label: "Split into functions",
        metrics: ["Functions", "NCSS/Functions", "Vars/Functions"],
        description: "Check how code is split across functions.",
      },
      {
        id: "style",
        label: "Style",
        metrics: ["Total variables", "Vars/NCSS"],
        description: "Surface variable density and stylistic footprint.",
      },
    );
  }
  const themeLookup = new Map(CLUSTERING_THEMES.map((theme) => [theme.id, theme]));

  if (!clusteringChart && !clusteringForm && !clusteringLaunchButton) return;

  const clusteringState = {
    dataset: null,
    algorithm: null,
    results: {},
    metricKeyByTheme: {},
    metricKey: null,
    membershipFilterByTheme: {},
    activeTheme: CLUSTERING_THEMES[0] ? CLUSTERING_THEMES[0].id : null,
    themeParams: {},
    featureMode: "metrics",
    embeddingDims: 16,
    generateDescriptions: true,
  };
  const refreshInFlight = new Set();
  let unsubscribeTaskEvents = null;

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

  function normalizeClusters(rawClusters = []) {
    const seen = new Map();
    (rawClusters || []).forEach((cluster) => {
      if (!cluster) return;
      const key = Number.isFinite(cluster.id) ? cluster.id : cluster.label || cluster.centroid?.toString();
      const existing = seen.get(key);
      if (!existing || (!existing.description && cluster.description)) {
        seen.set(key, cluster);
      }
    });
    return Array.from(seen.values());
  }

  function deriveClustersFromPoints(points = []) {
    const counts = new Map();
    (points || []).forEach((p) => {
      const cid = Number(p?.cluster);
      if (!Number.isFinite(cid) || cid < 0) return;
      counts.set(cid, (counts.get(cid) || 0) + 1);
    });
    return Array.from(counts.entries())
      .sort((a, b) => a[0] - b[0])
      .map(([cid, size]) => ({ id: cid, label: getClusterLabel(cid), size }));
  }

  function formatClusterLabel(cluster) {
    if (!cluster) return "Cluster";
    if (cluster.label) return cluster.label;
    if (Number.isFinite(cluster.id)) return `Cluster ${cluster.id + 1}`;
    return "Cluster";
  }

  function getClusterKey(cluster, fallbackIndex = 0) {
    if (!cluster) return String(fallbackIndex);
    if (Number.isFinite(cluster.id)) return String(cluster.id);
    if (cluster.label) return cluster.label;
    if (Array.isArray(cluster.centroid)) return cluster.centroid.join(",");
    return String(fallbackIndex);
  }

  function hasPendingDescriptions(data) {
    const clusters = Array.isArray(data?.clusters) ? data.clusters : [];
    return clusters.some((cluster) => cluster && cluster.llm_pending);
  }

  function normalizeMetadata(data) {
    const meta = data && typeof data === "object" && typeof data.metadata === "object" ? { ...data.metadata } : {};
    const parameters =
      (meta.parameters && typeof meta.parameters === "object" && meta.parameters) ||
      (data && typeof data.parameters === "object" ? data.parameters : null) ||
      {};
    const axes =
      (meta.axes && typeof meta.axes === "object" && meta.axes) ||
      (data && typeof data.axes === "object" && data.axes) ||
      {};
    const metrics = Array.isArray(meta.metrics) && meta.metrics.length
      ? meta.metrics
      : Array.isArray(data?.metrics) && data.metrics.length
        ? data.metrics
        : [];

    if (!meta.dataset && data?.dataset) meta.dataset = data.dataset;
    if (!meta.theme && data?.theme) meta.theme = data.theme;
    if (!meta.theme_label && data?.theme_label) meta.theme_label = data.theme_label;
    if (!meta.algorithm && data?.algorithm) meta.algorithm = data.algorithm;
    if (!meta.feature_mode && parameters.feature_mode) meta.feature_mode = parameters.feature_mode;
    if (typeof meta.embedding_dims === "undefined" && typeof parameters.embedding_dims !== "undefined") {
      meta.embedding_dims = parameters.embedding_dims;
    }
    if (typeof meta.generate_descriptions === "undefined" && typeof parameters.generate_descriptions !== "undefined") {
      meta.generate_descriptions = parameters.generate_descriptions;
    }
    meta.axes = axes;
    meta.metrics = metrics;
    meta.parameters = parameters;
    meta.point_count = meta.point_count ?? (Array.isArray(data?.points) ? data.points.length : undefined);
    meta.cluster_count = meta.cluster_count ?? (Array.isArray(data?.clusters) ? data.clusters.length : undefined);
    return meta;
  }

  function renderMetadata(data) {
    if (!clusteringMetadata) return;
    const meta = data ? normalizeMetadata(data) : null;
    clusteringMetadata.innerHTML = "";
    if (clusteringMetadataCard) {
      clusteringMetadataCard.hidden = false;
    }
    if (!meta || !Object.keys(meta).length) {
      clusteringMetadata.innerHTML = "<p class=\"clustering-hint\">Run clustering to see the configuration details.</p>";
      return;
    }

    const entries = [
      ["Dataset", meta.dataset],
      ["Theme", meta.theme_label || meta.theme],
      ["Algorithm", meta.algorithm],
      ["Parameters", formatClusteringParameters(meta.algorithm, meta.parameters)],
      ["Feature mode", meta.feature_mode],
      ["Embedding dims", meta.embedding_dims],
      ["LLM descriptions", meta.generate_descriptions === false ? "Off" : "On"],
      ["Metrics", Array.isArray(meta.metrics) && meta.metrics.length ? meta.metrics.join(", ") : null],
      [
        "Axes",
        meta.axes && (meta.axes.x || meta.axes.y) ? `${meta.axes.x || "Component 1"} / ${meta.axes.y || "Component 2"}` : null,
      ],
      [
        "Excluded files",
        Number.isFinite(meta.excluded_count)
          ? meta.excluded_count
          : Array.isArray(meta.excluded_files)
            ? meta.excluded_files.length
            : null,
      ],
      ["Points", meta.point_count],
      ["Clusters", meta.cluster_count],
      ["Generated", meta.timestamp ? new Date(meta.timestamp).toLocaleString() : null],
    ].filter(([, value]) => value !== null && value !== undefined && value !== "");

    if (!entries.length) {
      clusteringMetadata.innerHTML = "<p class=\"clustering-hint\">No metadata available for this run.</p>";
      return;
    }

    const list = document.createElement("dl");
    list.className = "clustering-metadata-grid";
    entries.forEach(([label, value]) => {
      const dt = document.createElement("dt");
      dt.textContent = label;
      const dd = document.createElement("dd");
      dd.textContent = Array.isArray(value) ? value.join(", ") : String(value);
      list.appendChild(dt);
      list.appendChild(dd);
    });
    clusteringMetadata.appendChild(list);

    const themeSummaries = Object.entries(clusteringState.results || {}).map(([themeId, payload]) => {
      const normalized = normalizeMetadata(payload);
      const label = normalized.theme_label || themeId;
      const algo = normalized.algorithm || "n/a";
      const paramsText = formatClusteringParameters(algo, normalized.parameters);
      return `${label}: ${algo}${paramsText ? ` ${paramsText}` : ""}`;
    });
    if (themeSummaries.length > 1) {
      const themeSection = document.createElement("div");
      themeSection.className = "clustering-theme-parameters";
      const heading = document.createElement("p");
      heading.className = "clustering-eyebrow";
      heading.textContent = "Per-theme parameters";
      themeSection.appendChild(heading);
      const listEl = document.createElement("ul");
      themeSummaries.forEach((text) => {
        const item = document.createElement("li");
        item.textContent = text;
        listEl.appendChild(item);
      });
      themeSection.appendChild(listEl);
      clusteringMetadata.appendChild(themeSection);
    }
  }

  function parseStructuredDescription(text) {
    if (!text || typeof text !== "string") return null;
    const cleanLine = (line) => (line || "").replace(/^["']+|["']+$/g, "").trim();
    const lines = text
      .split(/\r?\n/)
      .map((line) => cleanLine(line))
      .filter(Boolean);
    const labelLine = lines.find((l) => /^label\s*:/i.test(l));
    const descriptionLine = lines.find((l) => /^description\s*:/i.test(l));
    const comparisonStart = lines.findIndex((l) => /^comparison\s*:/i.test(l));
    const comparisonRows = [];
    const collectBullets = (startIndex) => {
      if (startIndex === -1) return [];
      const bullets = [];
      const inline = lines[startIndex].replace(/^[A-Za-z]+\s*:/, "").trim();
      if (inline) bullets.push(inline);
      for (let i = startIndex + 1; i < lines.length; i += 1) {
        const row = lines[i];
        if (/^[A-Za-z]+\s*:/i.test(row) || /^\|/.test(row)) break;
        const bullet = row.replace(/^[-*]\s*/, "").trim();
        if (bullet) bullets.push(bullet);
      }
      return bullets;
    };
    const goodStart = lines.findIndex((l) => /^good\s*:/i.test(l));
    const badStart = lines.findIndex((l) => /^bad\s*:/i.test(l));
    const goodBullets = collectBullets(goodStart);
    const badBullets = collectBullets(badStart);
    if (comparisonStart !== -1) {
      for (let i = comparisonStart + 1; i < lines.length; i += 1) {
        const row = lines[i];
        if (!/^\|/.test(row)) break;
        const cells = row
          .split("|")
          .map((c) => c.trim())
          .filter((c) => c);
        if (cells.length >= 4 && !/^metric$/i.test(cells[0])) {
          comparisonRows.push({
            metric: cells[0],
            cluster: cells[1],
            dataset: cells[2],
            relation: cells[3],
          });
        }
      }
    }
    return {
      label: labelLine ? labelLine.replace(/^label\s*:/i, "").trim() : null,
      description: descriptionLine ? descriptionLine.replace(/^description\s*:/i, "").trim() : null,
      comparison: comparisonRows,
      good: goodBullets,
      bad: badBullets,
    };
  }

  function buildComparisonTable(rows = []) {
    if (!rows || !rows.length) return "";
    const header = `
      <div class="cluster-comparison">
        <div class="cluster-comparison-row is-header">
          <span>Metric</span><span>Cluster</span><span>Dataset</span><span>Relation</span>
        </div>
        ${rows
          .map(
            (row) => `
          <div class="cluster-comparison-row">
            <span>${row.metric || "-"}</span>
            <span>${row.cluster || "-"}</span>
            <span>${row.dataset || "-"}</span>
            <span>${row.relation || "-"}</span>
          </div>
        `,
          )
          .join("")}
      </div>
    `;
    return header;
  }

  function buildClusterHeader(cluster) {
    const centroidPreview = (cluster.centroid || [])
      .slice(0, 2)
      .map((value) => formatMetricValue(value))
      .join(", ");
    const parsed = parseStructuredDescription(cluster.description) || parseStructuredDescription(cluster.llm_output);
    const displayLabel = (parsed && parsed.label) || cluster.label || `Cluster ${cluster.id + 1}`;
    const isPending = Boolean(cluster.llm_pending);
    const displayDescription =
      (parsed && parsed.description) ||
      (typeof cluster.description === "string" ? cluster.description : "") ||
      (typeof cluster.llm_output === "string" ? cluster.llm_output : "");
    const comparisons =
      (parsed && parsed.comparison && parsed.comparison.length ? parsed.comparison : null) ||
      (Array.isArray(cluster.comparison) && cluster.comparison.length ? cluster.comparison : null);
    const comparisonSection = comparisons ? buildComparisonTable(comparisons) : "";
    const goodPoints =
      (parsed && Array.isArray(parsed.good) && parsed.good.length ? parsed.good : null) ??
      (Array.isArray(cluster.good) ? cluster.good : []);
    const badPoints =
      (parsed && Array.isArray(parsed.bad) && parsed.bad.length ? parsed.bad : null) ??
      (Array.isArray(cluster.bad) ? cluster.bad : []);
    const reps = Array.isArray(cluster.representative_paths) ? cluster.representative_paths : [];
    const goodList =
      goodPoints.length > 0
        ? `<div class="cluster-goodbad-column"><h4>Good</h4><ul class="cluster-good-list">${goodPoints
            .map((item) => `<li>${item}</li>`)
            .join("")}</ul></div>`
        : "";
    const badList =
      badPoints.length > 0
        ? `<div class="cluster-goodbad-column"><h4>Bad</h4><ul class="cluster-bad-list">${badPoints
            .map((item) => `<li>${item}</li>`)
            .join("")}</ul></div>`
        : "";
    const goodBadWrapper =
      goodList || badList
        ? `<div class="cluster-goodbad-grid">${goodList}${badList}</div>`
        : "";
    const repsBlock =
      reps.length > 0
        ? `<div class="cluster-representatives"><h4>Representative files</h4><ul>${reps
            .map((r) => {
              const path = typeof r === "string" ? r : r?.path;
              const conf = typeof r === "object" && r && typeof r.confidence === "number" ? r.confidence : null;
              return `<li>${path || "Unknown"}${conf !== null ? ` (conf ${conf.toFixed(2)})` : ""}</li>`;
            })
            .join("")}</ul></div>`
        : "";
    const descriptionBlock = displayDescription
      ? `<p class="cluster-description">${displayDescription}</p>`
      : isPending
        ? `<p class="cluster-description is-pending">Description is being generated…</p>`
        : "";
    return `
      <h3>${displayLabel}</h3>
      <p>Size: ${cluster.size}</p>
      <p>Centroid (preview): ${centroidPreview || "-"}</p>
      ${descriptionBlock}
      ${repsBlock}
      ${goodBadWrapper}
      ${comparisonSection}
    `;
  }

  function getThemeById(themeId) {
    if (!themeId) return null;
    return themeLookup.get(themeId) || null;
  }

  function getActiveResult() {
    if (!clusteringState.activeTheme) return null;
    return clusteringState.results[clusteringState.activeTheme] || null;
  }

  function updateActiveThemeUI(themeId) {
    const theme = getThemeById(themeId);
    if (activeThemeLabel) {
      activeThemeLabel.textContent = theme ? theme.label : "None";
    }
    if (activeThemeChip) {
      activeThemeChip.textContent = theme ? `Theme: ${theme.label}` : "Theme: None";
    }
    if (themeTabsContainer) {
      themeTabsContainer.querySelectorAll(".clustering-theme-card").forEach((node) => {
        const isActive = node.dataset.themeId === themeId;
        node.classList.toggle("is-active", isActive);
      });
    }
    if (themeMetricsContainer) {
      themeMetricsContainer.innerHTML = "";
      if (theme && Array.isArray(theme.metrics) && theme.metrics.length) {
        const heading = document.createElement("p");
        heading.textContent = "Metrics used for this theme:";
        const list = document.createElement("div");
        list.className = "clustering-theme-badges";
        theme.metrics.forEach((metric) => {
          const badge = document.createElement("span");
          badge.textContent = metric;
          list.appendChild(badge);
        });
        themeMetricsContainer.appendChild(heading);
        themeMetricsContainer.appendChild(list);
      }
    }
  }

  function renderThemeCards() {
    if (!themeTabsContainer) return;
    themeTabsContainer.innerHTML = "";
    CLUSTERING_THEMES.forEach((theme) => {
      const button = document.createElement("button");
      button.type = "button";
      button.className = "clustering-theme-card";
      button.dataset.themeId = theme.id;
      const metrics = Array.isArray(theme.metrics) ? theme.metrics : [];
      button.innerHTML = `
        <div class="clustering-theme-card__header">
          <span class="clustering-theme-name">${theme.label}</span>
          <small>${theme.description || "Focused clustering bundle."}</small>
        </div>
        <div class="clustering-theme-metric-badges">
          ${metrics.map((metric) => `<span>${metric}</span>`).join("")}
        </div>
      `;
      button.addEventListener("click", () => setActiveTheme(theme.id));
      themeTabsContainer.appendChild(button);
    });
    updateActiveThemeUI(clusteringState.activeTheme);
  }

  function setActiveTheme(themeId) {
    if (!themeId || !themeLookup.has(themeId)) return;
    clusteringState.activeTheme = themeId;
    clusteringState.metricKey = clusteringState.metricKeyByTheme[themeId] || null;
    updateActiveThemeUI(themeId);
    const activeResult = getActiveResult();
    const metrics = activeResult
      ? (Array.isArray(activeResult.metrics) && activeResult.metrics.length
          ? activeResult.metrics
          : deriveMetricKeysFromPoints(activeResult.points))
      : (getThemeById(themeId)?.metrics || []);
    updateClusteringMetricOptions(metrics, themeId);
    renderActiveClustering();
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
    if (ncssHigh && duplicationHigh === false && complexityHigh === false) {
      reason.push("Large but clean (low duplication)");
    }
    if (ncssHigh && duplicationHigh === false && functionsLow === false && complexityHigh === false) {
      reason.push("Large file with unique content");
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

  const themeControls = {};
  clusteringState.themeParams = {};

  function setThemeKMeansAutoMode(themeId, enabled) {
    const controls = themeControls[themeId];
    const state = clusteringState.themeParams[themeId] || {};
    state.autoKMeans = Boolean(enabled);
    clusteringState.themeParams[themeId] = state;
    if (controls?.kmeansAuto) {
      controls.kmeansAuto.classList.toggle("active", state.autoKMeans);
      controls.kmeansAuto.textContent = state.autoKMeans ? "Auto k-means enabled" : "Auto k-means (silhouette)";
    }
    if (controls?.clusterSlider) {
      controls.clusterSlider.disabled = state.autoKMeans;
    }
  }

  function setThemeGmmAutoMode(themeId, enabled) {
    const controls = themeControls[themeId];
    const state = clusteringState.themeParams[themeId] || {};
    state.autoGmm = Boolean(enabled);
    clusteringState.themeParams[themeId] = state;
    if (controls?.gmmAuto) {
      controls.gmmAuto.classList.toggle("active", state.autoGmm);
      controls.gmmAuto.textContent = state.autoGmm ? "Auto GMM enabled" : "Auto GMM (BIC)";
    }
    if (controls?.gmmComponents) {
      controls.gmmComponents.disabled = state.autoGmm;
    }
    if (controls?.gmmCovariance) {
      controls.gmmCovariance.disabled = state.autoGmm;
    }
  }

  function setThemeHdbscanAutoMode(themeId, enabled) {
    const controls = themeControls[themeId];
    const state = clusteringState.themeParams[themeId] || {};
    state.autoHdbscan = Boolean(enabled);
    clusteringState.themeParams[themeId] = state;
    if (controls?.hdbscanAuto) {
      controls.hdbscanAuto.classList.toggle("active", state.autoHdbscan);
      controls.hdbscanAuto.textContent = state.autoHdbscan ? "Auto HDBSCAN enabled" : "Auto-tune HDBSCAN";
    }
    if (controls?.minClusterSize) {
      controls.minClusterSize.disabled = state.autoHdbscan;
    }
    if (controls?.minSamples) {
      controls.minSamples.disabled = state.autoHdbscan;
    }
  }

  function setThemeOpticsAutoMode(themeId, enabled) {
    const controls = themeControls[themeId];
    const state = clusteringState.themeParams[themeId] || {};
    state.autoOptics = Boolean(enabled);
    clusteringState.themeParams[themeId] = state;
    if (controls?.opticsAuto) {
      controls.opticsAuto.classList.toggle("active", state.autoOptics);
      controls.opticsAuto.textContent = state.autoOptics ? "Auto OPTICS enabled" : "Auto OPTICS (silhouette)";
    }
    if (controls?.opticsMinSamples) {
      controls.opticsMinSamples.disabled = state.autoOptics;
    }
    if (controls?.opticsXi) {
      controls.opticsXi.disabled = state.autoOptics;
    }
    if (controls?.opticsMaxEps) {
      controls.opticsMaxEps.disabled = state.autoOptics;
    }
  }

  function sanitizeThemeHdbscanInputs(themeId) {
    const controls = themeControls[themeId];
    if (controls?.minClusterSize) {
      const min = Number(controls.minClusterSize.min) || 2;
      const max = Number(controls.minClusterSize.max) || 200;
      let value = Number(controls.minClusterSize.value);
      if (!Number.isFinite(value) || value < min) value = min;
      if (value > max) value = max;
      controls.minClusterSize.value = String(Math.round(value));
    }
    if (controls?.minSamples) {
      const min = Number(controls.minSamples.min) || 1;
      const max = Number(controls.minSamples.max) || 200;
      let value = Number(controls.minSamples.value);
      if (!Number.isFinite(value) || value < min) value = min;
      if (value > max) value = max;
      controls.minSamples.value = String(Math.round(value));
    }
  }

  function sanitizeThemeOpticsInputs(themeId) {
    const controls = themeControls[themeId];
    if (controls?.opticsMinSamples) {
      const min = Number(controls.opticsMinSamples.min) || 2;
      const max = Number(controls.opticsMinSamples.max) || 200;
      let value = Number(controls.opticsMinSamples.value);
      if (!Number.isFinite(value) || value < min) value = min;
      if (value > max) value = max;
      controls.opticsMinSamples.value = String(Math.round(value));
    }
    if (controls?.opticsXi) {
      let value = Number(controls.opticsXi.value);
      if (!Number.isFinite(value) || value < 0.001) value = 0.001;
      if (value > 0.99) value = 0.99;
      controls.opticsXi.value = String(value);
    }
    if (controls?.opticsMaxEps) {
      let value = Number(controls.opticsMaxEps.value);
      if (!Number.isFinite(value) || value < 0) value = 0;
      controls.opticsMaxEps.value = String(value);
    }
  }

  function updateThemeClusterLabel(themeId) {
    const controls = themeControls[themeId];
    if (controls?.clusterSlider && controls?.clusterValue) {
      controls.clusterValue.textContent = `${controls.clusterSlider.value || 0} clusters`;
    }
    if (controls?.gmmComponents && controls?.gmmComponentsValue) {
      controls.gmmComponentsValue.textContent = `${controls.gmmComponents.value || 0} components`;
    }
  }

  function updateThemeAlgorithmState(themeId) {
    const controls = themeControls[themeId];
    if (!controls || !controls.algorithm) return;
    const state = clusteringState.themeParams[themeId] || {};
    const isKMeans = (controls.algorithm.value || "kmeans") === "kmeans";
    const isHdbscan = (controls.algorithm.value || "kmeans") === "hdbscan";
    const isGmm = (controls.algorithm.value || "kmeans") === "gmm";
    const isOptics = (controls.algorithm.value || "kmeans") === "optics";
    toggleFieldsetVisibility(controls.kmeansParams, isKMeans);
    toggleFieldsetVisibility(controls.hdbscanParams, isHdbscan);
    toggleFieldsetVisibility(controls.gmmParams, isGmm);
    toggleFieldsetVisibility(controls.opticsParams, isOptics);
    if (controls.kmeansAuto) {
      controls.kmeansAuto.disabled = !isKMeans;
    }
    if (controls.gmmAuto) {
      controls.gmmAuto.disabled = !isGmm;
    }
    if (controls.hdbscanAuto) {
      controls.hdbscanAuto.disabled = !isHdbscan;
    }
    if (controls.opticsAuto) {
      controls.opticsAuto.disabled = !isOptics;
    }
    if (!isKMeans && state.autoKMeans) {
      setThemeKMeansAutoMode(themeId, false);
    }
    if (!isGmm && state.autoGmm) {
      setThemeGmmAutoMode(themeId, false);
    }
    if (!isHdbscan && state.autoHdbscan) {
      setThemeHdbscanAutoMode(themeId, false);
    }
    if (!isOptics && state.autoOptics) {
      setThemeOpticsAutoMode(themeId, false);
    }
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

  function updateDescriptionPreference() {
    clusteringState.generateDescriptions = clusteringDescriptionToggle ? clusteringDescriptionToggle.checked : true;
  }

  function initThemeControls() {
    themeConfigCards.forEach((card) => {
      const themeId = card.dataset.themeId;
      if (!themeId) return;
      const algorithm = card.querySelector(".theme-algorithm");
      const clusterSlider = card.querySelector(".theme-cluster-count");
      const clusterValue = card.querySelector(".theme-cluster-value");
      const kmeansParams = card.querySelector(".theme-kmeans-params");
      const hdbscanParams = card.querySelector(".theme-hdbscan-params");
      const gmmParams = card.querySelector(".theme-gmm-params");
      const opticsParams = card.querySelector(".theme-optics-params");
      const kmeansAuto = card.querySelector(".theme-kmeans-auto");
      const gmmAuto = card.querySelector(".theme-gmm-auto");
      const hdbscanAuto = card.querySelector(".theme-hdbscan-auto");
      const opticsAuto = card.querySelector(".theme-optics-auto");
      const gmmComponents = card.querySelector(".theme-gmm-components");
      const gmmComponentsValue = card.querySelector(".theme-gmm-components-value");
      const gmmCovariance = card.querySelector(".theme-gmm-covariance");
      const minClusterSize = card.querySelector(".theme-hdbscan-min-size");
      const minSamples = card.querySelector(".theme-hdbscan-min-samples");
      const opticsMinSamples = card.querySelector(".theme-optics-min-samples");
      const opticsXi = card.querySelector(".theme-optics-xi");
      const opticsMaxEps = card.querySelector(".theme-optics-max-eps");
      themeControls[themeId] = {
        algorithm,
        clusterSlider,
        clusterValue,
        kmeansParams,
        gmmParams,
        hdbscanParams,
        opticsParams,
        kmeansAuto,
        gmmAuto,
        hdbscanAuto,
        opticsAuto,
        gmmComponents,
        gmmComponentsValue,
        gmmCovariance,
        minClusterSize,
        minSamples,
        opticsMinSamples,
        opticsXi,
        opticsMaxEps,
      };
      clusteringState.themeParams[themeId] = clusteringState.themeParams[themeId] || {
        autoKMeans: false,
        autoGmm: false,
        autoHdbscan: false,
        autoOptics: false,
      };
      updateThemeClusterLabel(themeId);
      updateThemeAlgorithmState(themeId);
      sanitizeThemeHdbscanInputs(themeId);
      sanitizeThemeOpticsInputs(themeId);
      if (algorithm) {
        algorithm.addEventListener("change", () => updateThemeAlgorithmState(themeId));
      }
      if (clusterSlider) {
        clusterSlider.addEventListener("input", () => updateThemeClusterLabel(themeId));
      }
      if (gmmComponents) {
        gmmComponents.addEventListener("input", () => updateThemeClusterLabel(themeId));
      }
      if (kmeansAuto) {
        kmeansAuto.addEventListener("click", () => {
          setThemeHdbscanAutoMode(themeId, false);
          setThemeGmmAutoMode(themeId, false);
          setThemeKMeansAutoMode(themeId, !clusteringState.themeParams[themeId].autoKMeans);
        });
      }
      if (gmmAuto) {
        gmmAuto.addEventListener("click", () => {
          setThemeKMeansAutoMode(themeId, false);
          setThemeHdbscanAutoMode(themeId, false);
          setThemeOpticsAutoMode(themeId, false);
          setThemeGmmAutoMode(themeId, !clusteringState.themeParams[themeId].autoGmm);
        });
      }
      if (hdbscanAuto) {
        hdbscanAuto.addEventListener("click", () => {
          setThemeKMeansAutoMode(themeId, false);
          setThemeGmmAutoMode(themeId, false);
          setThemeOpticsAutoMode(themeId, false);
          setThemeHdbscanAutoMode(themeId, !clusteringState.themeParams[themeId].autoHdbscan);
        });
      }
      if (opticsAuto) {
        opticsAuto.addEventListener("click", () => {
          setThemeKMeansAutoMode(themeId, false);
          setThemeGmmAutoMode(themeId, false);
          setThemeHdbscanAutoMode(themeId, false);
          setThemeOpticsAutoMode(themeId, !clusteringState.themeParams[themeId].autoOptics);
        });
      }
      if (minClusterSize) {
        minClusterSize.addEventListener("change", () => sanitizeThemeHdbscanInputs(themeId));
      }
      if (minSamples) {
        minSamples.addEventListener("change", () => sanitizeThemeHdbscanInputs(themeId));
      }
      if (opticsMinSamples || opticsXi || opticsMaxEps) {
        [opticsMinSamples, opticsXi, opticsMaxEps].forEach((input) => {
          if (input) {
            input.addEventListener("change", () => sanitizeThemeOpticsInputs(themeId));
          }
        });
      }
    });
  }

  function updateClusteringMetricDataset(value) {
    if (clusteringMetricDatasetLabel) {
      clusteringMetricDatasetLabel.textContent = value || "None";
    }
  }

  function getClusteringLastUrl(themeId) {
    if (typeof API_ROUTES.clusteringLast === "function") {
      return API_ROUTES.clusteringLast(themeId);
    }
    if (themeId) {
      return `${API_ROUTES.clusteringLast}?theme=${encodeURIComponent(themeId)}`;
    }
    return API_ROUTES.clusteringLast;
  }

  function setProgress(isRunning, text = "") {
    if (!progressBanner) return;
    progressBanner.hidden = !isRunning;
    if (progressText) {
      progressText.textContent = text || (isRunning ? "Running clustering…" : "");
    }
  }

  async function refreshThemeFromCache(themeId) {
    if (!themeId || refreshInFlight.has(themeId)) return;
    refreshInFlight.add(themeId);
    try {
      const data = await requestJSON(getClusteringLastUrl(themeId));
      renderClusteringResults(themeId, data);
    } catch (error) {
      console.warn("Failed to refresh clustering results", error);
    }
    refreshInFlight.delete(themeId);
  }

  function bindTaskEvents() {
    if (unsubscribeTaskEvents) return;
    unsubscribeTaskEvents = subscribeToTaskEvents((event) => {
      if (!event || event.type !== "clustering-description-completed") return;
      const targetDataset = event.dataset;
      if (targetDataset && clusteringState.dataset && targetDataset !== clusteringState.dataset) return;
      const themeId = event.theme || clusteringState.activeTheme;
      if (themeId) {
        void refreshThemeFromCache(themeId);
      }
    });
  }

  function updateClusteringMetricOptions(metrics = [], themeId = clusteringState.activeTheme) {
    if (!clusteringMetricSelect) return;
    clusteringMetricSelect.innerHTML = "";
    if (!metrics.length) {
      const option = document.createElement("option");
      option.value = "";
      option.textContent = "No metrics";
      clusteringMetricSelect.appendChild(option);
      clusteringMetricSelect.disabled = true;
      clusteringState.metricKeyByTheme[themeId] = null;
      if (themeId === clusteringState.activeTheme) {
        clusteringState.metricKey = null;
      }
      return;
    }
    metrics.forEach((metricKey) => {
      const option = document.createElement("option");
      option.value = metricKey;
      option.textContent = metricKey;
      clusteringMetricSelect.appendChild(option);
    });
    clusteringMetricSelect.disabled = false;
    const previous = clusteringState.metricKeyByTheme[themeId];
    const active = previous && metrics.includes(previous) ? previous : metrics[0];
    clusteringState.metricKeyByTheme[themeId] = active;
    if (themeId === clusteringState.activeTheme) {
      clusteringState.metricKey = active;
      clusteringMetricSelect.value = active;
    }
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
    } else if (algorithm === "gmm") {
      const components = Number(parameters.cluster_count);
      const covType = parameters.covariance_type || parameters.covariance;
      if (Number.isFinite(components)) {
        parts.push(`components=${components}`);
      }
      if (covType) {
        parts.push(`cov=${covType}`);
      }
    } else if (algorithm === "optics") {
      const minSamples = Number(parameters.min_samples);
      const xi = Number(parameters.xi);
      const maxEps = parameters.max_eps;
      if (Number.isFinite(minSamples)) {
        parts.push(`min_samples=${minSamples}`);
      }
      if (Number.isFinite(xi)) {
        parts.push(`xi=${xi}`);
      }
      if (maxEps !== null && maxEps !== undefined) {
        const value = Number(maxEps);
        if (Number.isFinite(value) && value > 0) {
          parts.push(`max_eps=${value}`);
        }
      }
    }
    const silhouette = Number(parameters.silhouette);
    if (Number.isFinite(silhouette)) {
      parts.push(`silhouette=${formatMetricValue(silhouette)}`);
    }
    return parts.length ? ` [${parts.join(" · ")}]` : "";
  }

  function renderClusteringMetricChart(data = getActiveResult()) {
    const metricKey = clusteringState.metricKeyByTheme[clusteringState.activeTheme] || clusteringState.metricKey;
    if (!clusteringMetricChart || !metricKey) {
      clearClusteringMetricChart();
      return;
    }
    if (!window.Plotly) {
      showMessage(clusteringMetricFeedback, "Plotly library failed to load.", true);
      return;
    }
    if (!data || !Array.isArray(data.points) || !data.points.length) {
      clearClusteringMetricChart();
      showMessage(
        clusteringMetricFeedback,
        getMessage("CLUSTERING_NO_METRICS", {}, "Run a clustering job to display the metric distribution."),
      );
      return;
    }
    const points = (data && data.points ? data.points : [])
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
      showMessage(
        clusteringMetricFeedback,
        getMessage("CLUSTERING_NO_VALUES", { metric: metricKey }, `No values for ${metricKey}.`),
        true,
      );
      return;
    }
    const values = points.map((point) => point.y);
    const { q1, q3, median } = computeQuartiles(values);
    const hasQ1 = Number.isFinite(q1);
    const hasQ3 = Number.isFinite(q3);
    const iqr = hasQ1 && hasQ3 ? q3 - q1 : null;
    const hasIqr = Number.isFinite(iqr) && iqr > 0;
    const lowerFence = hasIqr ? q1 - iqr * 1.5 : null;
    const upperFence = hasIqr ? q3 + iqr * 1.5 : null;
    const pointStatuses = points.map((point) => {
      if (Number.isFinite(upperFence) && point.y > upperFence) return "Above Q3 + 1.5×IQR";
      if (hasQ3 && point.y > q3) return "Above Q3";
      if (Number.isFinite(lowerFence) && point.y < lowerFence) return "Below Q1 - 1.5×IQR";
      if (hasQ1 && point.y < q1) return "Below Q1";
      return "Within IQR";
    });
    const statusToBucketKey = {
      "Above Q3 + 1.5×IQR": "aboveFence",
      "Above Q3": "aboveQ3",
      "Below Q1 - 1.5×IQR": "belowFence",
      "Below Q1": "belowQ1",
    };
    const statusComments = pointStatuses.map((status) => {
      const bucketKey = statusToBucketKey[status] || null;
      if (!bucketKey) return "Within the dataset's interquartile range.";
      return describeThreshold(metricKey, bucketKey) || "Within the dataset's interquartile range.";
    });

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
      customdata: points.map((point, index) => [
        point.filename,
        getClusterLabel(point.cluster),
        pointStatuses[index],
        statusComments[index],
      ]),
      hovertemplate: `<b>%{customdata[0]}</b><br>${metricKey}: %{y}<br>%{customdata[1]}<br>Status: %{customdata[2]}<br>%{customdata[3]}<extra></extra>`,
    };

    const shapes = [];
    if (Number.isFinite(q1)) {
      shapes.push({
        type: "line",
        xref: "paper",
        x0: 0,
        x1: 1,
        y0: q1,
        y1: q1,
        line: { color: "#0ea5e9", dash: "dot", width: 1.5 },
      });
    }
    if (Number.isFinite(q3)) {
      shapes.push({
        type: "line",
        xref: "paper",
        x0: 0,
        x1: 1,
        y0: q3,
        y1: q3,
        line: { color: "#0ea5e9", dash: "dot", width: 1.5 },
      });
    }
    if (Number.isFinite(lowerFence)) {
      shapes.push({
        type: "line",
        xref: "paper",
        x0: 0,
        x1: 1,
        y0: lowerFence,
        y1: lowerFence,
        line: { color: "#1e3a8a", dash: "dash", width: 2 },
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
        line: { color: "#b91c1c", dash: "dash", width: 2 },
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
        line: { color: "#94a3b8", dash: "dot", width: 1 },
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
      getMessage(
        "CLUSTERING_DISTRIBUTION",
        { metric: metricKey, count: points.length, plural: points.length > 1 ? "s" : "" },
        `Showing distribution for ${metricKey} (${points.length} file${points.length > 1 ? "s" : ""}).`,
      ),
    );
  }

  function renderClusteringSummary(data) {
    if (!clusteringSummary) return;
    clusteringSummary.innerHTML = "<h2>Clusters</h2>";
    const body = document.createElement("div");
    body.className = "clustering-summary-body";
    const rawClusters = data && Array.isArray(data.clusters) && data.clusters.length ? data.clusters : null;
    // Deduplicate clusters by id, preferring entries that have a description/LLM label.
    let clusters = null;
    if (rawClusters) {
      const seen = new Map();
      rawClusters.forEach((c) => {
        const key = Number.isFinite(c.id) ? c.id : c.label || c.centroid?.toString();
        const existing = seen.get(key);
        if (!existing || (!existing.description && c.description)) {
          seen.set(key, c);
        }
      });
      clusters = Array.from(seen.values());
    }
    const makeWarning = (text, severity = "danger") => {
      const note = document.createElement("p");
      note.className = "cluster-warning-note";
      if (severity === "warning") {
        note.classList.add("is-warning");
      }
      note.textContent = text;
      return note;
    };
    if (!clusters) {
      const empty = document.createElement("p");
      empty.textContent = "No clusters available yet.";
      body.appendChild(empty);
    } else {
      const metricStats = computeClusterMetricStats(clusters, data.metrics || []);
      const grid = document.createElement("div");
      grid.className = "clustering-summary-grid";
      clusters.forEach((cluster) => {
        const derived = deriveClusterLabel(cluster, metricStats);
        const card = document.createElement("article");
        card.className = "clustering-summary-card";
        const colorSwatch = document.createElement("span");
        colorSwatch.className = "cluster-color-swatch";
        colorSwatch.style.background = getClusterColor(cluster.id);
        card.innerHTML = buildClusterHeader(cluster);
        card.prepend(colorSwatch);
        if (derived.reasons && derived.reasons.length) {
          card.appendChild(makeWarning("Elevated metrics detected", "warning"));
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
          metricsTitle.textContent = "Average metrics";
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
            metricsContainer.appendChild(
              makeWarning("Average functions is 0 — likely files with errors or invalid parsing.", "danger"),
            );
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
      noise.textContent = `${data.noise} file(s) flagged as noise.`;
      body.appendChild(noise);
    }
    clusteringSummary.appendChild(body);
  }

  function updateMembershipFilterOptions(clusters = [], themeId = clusteringState.activeTheme, points = []) {
    if (!clusteringMembershipFilter) return;
    const normalized = normalizeClusters(clusters);
    const derivedFromPoints = deriveClustersFromPoints(points);
    const clustersForSelect = normalized.length ? normalized : derivedFromPoints;
    clusteringMembershipFilter.innerHTML = "";
    const defaultOption = document.createElement("option");
    defaultOption.value = "";
    defaultOption.textContent = "All clusters";
    clusteringMembershipFilter.appendChild(defaultOption);
    clustersForSelect.forEach((cluster, index) => {
      const option = document.createElement("option");
      const optionValue = getClusterKey(cluster, index);
      option.value = optionValue;
      const label = formatClusterLabel(cluster);
      const size = Number.isFinite(cluster.size) ? cluster.size : null;
      option.textContent = size !== null ? `${label} (${size})` : label;
      clusteringMembershipFilter.appendChild(option);
    });
    clusteringMembershipFilter.disabled = clustersForSelect.length === 0;
    const savedValue = themeId ? clusteringState.membershipFilterByTheme[themeId] : "";
    const validValues = Array.from(clusteringMembershipFilter.options).map((opt) => opt.value);
    const nextValue = validValues.includes(savedValue) ? savedValue : "";
    clusteringMembershipFilter.value = nextValue;
    if (themeId) {
      clusteringState.membershipFilterByTheme[themeId] = nextValue;
    }
  }

  function renderMembershipTable(data) {
    const container = document.getElementById("clustering-membership");
    if (!container) return;
    container.innerHTML = "";
    const points = Array.isArray(data?.points) ? data.points : [];
    const rawClusters = Array.isArray(data?.clusters) ? data.clusters : [];
    const clusters = normalizeClusters(rawClusters);
    const clustersForDisplay = clusters.length ? clusters : deriveClustersFromPoints(points);
    updateMembershipFilterOptions(clusters, clusteringState.activeTheme, points);
    if (!points.length || !clustersForDisplay.length) {
      container.textContent = points.length
        ? "Clusters are not available yet for this run."
        : "Run clustering to view soft memberships.";
      return;
    }
    let hasProbs = points.some((p) => Array.isArray(p.probabilities) && p.probabilities.length);
    let enrichedPoints = points;
    if (!hasProbs) {
      // Fallback: derive crisp probabilities from hard labels.
      enrichedPoints = points.map((p) => {
        if (typeof p.cluster !== "number" || p.cluster < 0) return p;
        const probs = clusters.map((_, idx) => (idx === p.cluster ? 1.0 : 0.0));
        return { ...p, probabilities: probs };
      });
      hasProbs = enrichedPoints.some((p) => Array.isArray(p.probabilities) && p.probabilities.length);
      if (!hasProbs) {
        container.textContent = "Soft memberships unavailable for this run.";
        return;
      }
    }
    const clusterKeys = clustersForDisplay.map((cluster, index) => getClusterKey(cluster, index));
    const themeId = clusteringState.activeTheme;
    const selectedFilter = themeId ? clusteringState.membershipFilterByTheme[themeId] : "";
    const hasFilter = selectedFilter !== null && selectedFilter !== undefined && selectedFilter !== "";
    const processed = enrichedPoints
      .map((p) => {
        const probs = Array.isArray(p.probabilities) ? p.probabilities : [];
        const assignedKey = Number.isFinite(p.cluster) ? String(p.cluster) : null;
        const maxProb = probs.length ? Math.max(...probs) : 0;
        const dominantIndex = probs.length ? probs.indexOf(maxProb) : null;
        const dominantKey = dominantIndex !== null && dominantIndex >= 0 ? clusterKeys[dominantIndex] : null;
        return {
          path: p.path,
          probs,
          clusterKey: assignedKey !== null ? assignedKey : dominantKey,
          dominantKey,
          maxProb,
        };
      })
      .filter((entry) => {
        if (!hasFilter || !selectedFilter) return true;
        return entry.clusterKey === selectedFilter || entry.dominantKey === selectedFilter;
      })
      .sort((a, b) => b.maxProb - a.maxProb)
      .slice(0, 50);

    if (!processed.length) {
      container.textContent = "No files match this cluster yet.";
      return;
    }
    const table = document.createElement("table");
    table.className = "membership-table";
    const thead = document.createElement("thead");
    const headerRow = document.createElement("tr");
    ["File", ...clustersForDisplay.map((c) => formatClusterLabel(c))].forEach((label) => {
      const th = document.createElement("th");
      th.textContent = label;
      headerRow.appendChild(th);
    });
    thead.appendChild(headerRow);
    table.appendChild(thead);
    const tbody = document.createElement("tbody");
    processed.forEach((entry) => {
      const tr = document.createElement("tr");
      const name = document.createElement("td");
      name.textContent = entry.path || "-";
      tr.appendChild(name);
      clusters.forEach((_, idx) => {
        const td = document.createElement("td");
        const prob = entry.probs && entry.probs[idx] !== undefined ? entry.probs[idx] : null;
        td.textContent = prob !== null ? `${(prob * 100).toFixed(1)}%` : "-";
        tr.appendChild(td);
      });
      tbody.appendChild(tr);
    });
    table.appendChild(tbody);
    container.appendChild(table);
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

  function renderActiveClustering() {
    const data = getActiveResult();
    const themeId = clusteringState.activeTheme;
    updateActiveThemeUI(themeId);
    const metrics =
      data && Array.isArray(data.metrics) && data.metrics.length
        ? data.metrics
        : deriveMetricKeysFromPoints(data && data.points ? data.points : []);

    updateClusteringMetricDataset(data && data.dataset ? data.dataset : clusteringState.dataset);
    updateClusteringMetricOptions(metrics, themeId);
    if (!metrics.length) {
      showMessage(
        clusteringMetricFeedback,
        getMessage("CLUSTERING_NO_METRICS", {}, "Run a clustering job to display the metric distribution."),
      );
    }
    renderClusteringMetricChart(data);
    renderMetadata(data);

    if (!data || !Array.isArray(data.points)) {
      if (clusteringChart) {
        clusteringChart.innerHTML = "";
      }
      renderClusteringSummary(data);
      renderMembershipTable(data);
      return;
    }

    if (clusteringChart && window.Plotly) {
      const hoverTexts = data.points.map((point) => {
        const lines = [`${point.path}`];
        const pointMetrics = point.metrics || {};
        Object.keys(pointMetrics).forEach((key) => {
          lines.push(`${key}: ${formatMetricValue(pointMetrics[key])}`);
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
        customdata: data.points.map((point) => {
          const probs = Array.isArray(point.probabilities) ? point.probabilities : [];
          return [probs.map((p) => (Number.isFinite(p) ? p : 0))];
        }),
        hovertemplate:
          "<b>%{text}</b><br>" +
          "Probabilities: %{customdata[0]}<extra></extra>",
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
    renderMembershipTable(data);
  }

  function renderClusteringResults(themeId, data) {
    if (!data) return;
    const datasetName = data.dataset || clusteringState.dataset;
    const parameters = data.parameters || {};
    const incomingMetrics = Array.isArray(data.metrics) ? data.metrics : [];
    const derivedMetrics = deriveMetricKeysFromPoints(data.points || []);
    const metrics = incomingMetrics.length ? incomingMetrics : derivedMetrics;
    const metadata = normalizeMetadata(data);
    clusteringState.dataset = datasetName || clusteringState.dataset;
    clusteringState.algorithm = data.algorithm || clusteringState.algorithm;
    const selectedMetric =
      clusteringState.metricKeyByTheme[themeId] && metrics.includes(clusteringState.metricKeyByTheme[themeId])
        ? clusteringState.metricKeyByTheme[themeId]
        : metrics[0] || null;
    clusteringState.metricKeyByTheme[themeId] = selectedMetric;
    clusteringState.results[themeId] = { ...data, metrics, metadata };
    if (hasPendingDescriptions(data)) {
      setProgress(true, "Generating cluster descriptions…");
    } else {
      setProgress(false);
    }
    if (!clusteringState.activeTheme) {
      clusteringState.activeTheme = themeId;
    }

    if (clusteringState.activeTheme === themeId) {
      clusteringState.metricKey = selectedMetric;
      updateClusteringMetricDataset(datasetName);
      updateClusteringMetricOptions(metrics, themeId);
      renderActiveClustering();
    }
  }

  async function runClusteringJob() {
    if (!clusteringLaunchButton) return;
    updateFeatureConfigState();
    updateEmbeddingDimsLabel();
    updateDescriptionPreference();
    const themesPayload = CLUSTERING_THEMES.map((theme) => {
      const controls = themeControls[theme.id] || {};
      const params = clusteringState.themeParams[theme.id] || {};
      const algorithm = controls.algorithm ? controls.algorithm.value : "kmeans";
      const payload = {
        theme: theme.id,
        algorithm,
      };
      if (algorithm === "kmeans") {
        if (params.autoKMeans) {
          payload.auto_kmeans = true;
        } else if (controls.clusterSlider) {
          payload.cluster_count = Number(controls.clusterSlider.value);
        }
      } else if (algorithm === "optics") {
        if (params.autoOptics) {
          payload.auto_optics = true;
        } else {
          sanitizeThemeOpticsInputs(theme.id);
          payload.min_samples = Number(controls.opticsMinSamples ? controls.opticsMinSamples.value : 5);
          payload.optics_xi = Number(controls.opticsXi ? controls.opticsXi.value : 0.05);
          payload.optics_max_eps = Number(controls.opticsMaxEps ? controls.opticsMaxEps.value : 0);
        }
      } else if (algorithm === "hdbscan") {
        if (params.autoHdbscan) {
          payload.auto_hdbscan = true;
        } else {
          sanitizeThemeHdbscanInputs(theme.id);
          payload.min_cluster_size = Number(controls.minClusterSize ? controls.minClusterSize.value : 5);
          payload.min_samples = Number(controls.minSamples ? controls.minSamples.value : 5);
        }
      } else if (algorithm === "gmm") {
        if (params.autoGmm) {
          payload.auto_gmm = true;
        } else {
          payload.cluster_count = Number(controls.gmmComponents ? controls.gmmComponents.value : 2);
        }
        if (controls.gmmCovariance) {
          payload.gmm_covariance_type = controls.gmmCovariance.value || "full";
        }
      }
      return payload;
    });

    const payload = {
      feature_mode: clusteringState.featureMode || "metrics",
      embedding_dims: Number(clusteringState.embeddingDims) || 16,
      themes: themesPayload,
      generate_descriptions: clusteringState.generateDescriptions !== false,
    };

    showMessage(
      clusteringFeedback,
      getMessage("CLUSTERING_RUNNING", {}, "Running clustering..."),
    );
    clusteringLaunchButton.disabled = true;
    setProgress(true, "Running clustering across themes…");
    try {
      const successes = {};
      const failures = [];
      setProgress(true, `Clustering ${themesPayload.length} theme(s)…`);
      const result = await requestJSON(API_ROUTES.clustering, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(payload),
      });
      const resultMap = result && result.results ? result.results : result.theme ? { [result.theme]: result } : {};
      Object.entries(resultMap).forEach(([themeId, value]) => renderClusteringResults(themeId, value));
      if (!clusteringState.activeTheme && Object.keys(resultMap).length) {
        clusteringState.activeTheme = Object.keys(resultMap)[0];
      }
      if (clusteringState.activeTheme) {
        setActiveTheme(clusteringState.activeTheme);
      }
      const active = getActiveResult();
      if (active) {
        const pointCount = Array.isArray(active.points) ? active.points.length : 0;
        const paramsDetail = formatClusteringParameters(active.algorithm, active.parameters);
        showMessage(
          clusteringFeedback,
          getMessage(
            "CLUSTERING_COMPLETED",
            {
              algorithm: active.algorithm,
              details: paramsDetail,
              count: pointCount,
              plural: pointCount > 1 ? "s" : "",
            },
            `Clustering completed${paramsDetail} (${pointCount} file${pointCount > 1 ? "s" : ""}).`,
          ),
        );
      }
    } catch (error) {
      showMessage(clusteringFeedback, error.message, true);
      if (clusteringChart && window.Plotly) {
        Plotly.purge(clusteringChart);
        clusteringChart.innerHTML = "";
      }
    } finally {
      setProgress(false);
      clusteringLaunchButton.disabled = false;
    }
  }

  async function loadCachedClustering() {
    if (!clusteringChart && !clusteringForm) return;
    let loaded = 0;
    for (const theme of CLUSTERING_THEMES) {
      try {
        const data = await requestJSON(getClusteringLastUrl(theme.id));
        renderClusteringResults(theme.id, data);
        loaded += 1;
      } catch (error) {
        // Ignore missing cache for this theme.
      }
    }
    if (!getActiveResult() && Object.keys(clusteringState.results).length) {
      clusteringState.activeTheme = Object.keys(clusteringState.results)[0];
    }
    if (getActiveResult() && loaded) {
      setActiveTheme(clusteringState.activeTheme);
      const data = getActiveResult();
      const pointCount = Array.isArray(data.points) ? data.points.length : 0;
      const paramsDetail = formatClusteringParameters(data.algorithm, data.parameters);
      const themeName = getThemeById(clusteringState.activeTheme)?.label || clusteringState.activeTheme;
      showMessage(
        clusteringFeedback,
        getMessage(
          "CLUSTERING_LOADED",
          { algorithm: data.algorithm || "", details: paramsDetail, count: pointCount, plural: pointCount > 1 ? "s" : "" },
          `Loaded cached clustering for ${themeName}${paramsDetail} (${pointCount} file${pointCount > 1 ? "s" : ""}).`,
        ),
      );
    }
  }

  function initClusteringUI() {
    bindTaskEvents();
    renderThemeCards();
    if (clusteringState.activeTheme) {
      updateActiveThemeUI(clusteringState.activeTheme);
    }
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
    if (clusteringDescriptionToggle) {
      clusteringDescriptionToggle.addEventListener("change", updateDescriptionPreference);
      updateDescriptionPreference();
    }
    initThemeControls();
    if (clusteringMetricSelect) {
      clusteringMetricSelect.addEventListener("change", (event) => {
        const key = event.target.value || null;
        clusteringState.metricKey = key;
        if (clusteringState.activeTheme) {
          clusteringState.metricKeyByTheme[clusteringState.activeTheme] = key;
        }
        renderClusteringMetricChart();
      });
    }
    if (clusteringMembershipFilter) {
      clusteringMembershipFilter.addEventListener("change", (event) => {
        const value = event.target.value || "";
        if (clusteringState.activeTheme) {
          clusteringState.membershipFilterByTheme[clusteringState.activeTheme] = value;
        }
        renderMembershipTable(getActiveResult());
      });
    }
    if (clusteringLaunchButton) {
      clusteringLaunchButton.addEventListener("click", runClusteringJob);
    }
    if (clusteringState.activeTheme) {
      setActiveTheme(clusteringState.activeTheme);
    }
    void loadCachedClustering();
  }

  onReady(initClusteringUI);
})();
