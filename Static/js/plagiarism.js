(function () {
  const app = window.App || {};
  app.__initialized = app.__initialized || {};
  if (app.__initialized.plagiarism) return;
  app.__initialized.plagiarism = true;
  window.App = app;

  const { API_ROUTES, utils = {}, duplicates = {}, onReady = (fn) => fn() } = app;
  const { requestJSON } = utils;
  const {
    getDuplicateSummary,
    extractRelativePath,
    normalizeRelativePath,
    DEFAULT_LIZARD_SUMMARY,
  } = duplicates;

  const datasetSelect = document.getElementById("current-dataset-select");
  const plagiarismContainer = document.getElementById("plagiarism-viewer");
  const plagiarismPrevButton = document.getElementById("plagiarism-prev");
  const plagiarismNextButton = document.getElementById("plagiarism-next");
  const plagiarismCounter = document.getElementById("plagiarism-counter");

  if (!plagiarismContainer) return;

  const summaryFilename = plagiarismContainer.dataset.summaryFilename || DEFAULT_LIZARD_SUMMARY;

  const plagiarismState = {
    blocks: [],
    index: 0,
    dataset: plagiarismContainer.dataset.dataset || null,
    snippetCache: new Map(),
  };

  function getActiveDatasetForPlagiarism() {
    const selectValue = datasetSelect && datasetSelect.value ? datasetSelect.value : null;
    if (selectValue) return selectValue;
    if (plagiarismContainer.dataset.dataset) return plagiarismContainer.dataset.dataset;
    return plagiarismState.dataset || null;
  }

  function normalizeRelativeEntryPath(entry) {
    if (!entry) return "";
    return normalizeRelativePath(entry.relativePath || extractRelativePath(entry.filePath));
  }

  function blockHasMultipleFiles(block) {
    if (!block) return false;
    const files = new Set();
    (block.entries || []).forEach((entry) => {
      const relative = normalizeRelativeEntryPath(entry);
      if (relative) files.add(relative);
    });
    return files.size > 1;
  }

  function filterCrossFileBlocks(blocks = []) {
    return (blocks || []).filter((block) => blockHasMultipleFiles(block));
  }

  function showPlagiarismEmptyState(message) {
    plagiarismContainer.innerHTML = "";
    const wrapper = document.createElement("div");
    wrapper.className = "plagiarism-viewer-empty";
    wrapper.textContent = message;
    plagiarismContainer.appendChild(wrapper);
  }

  function updatePlagiarismControls() {
    const total = plagiarismState.blocks.length;
    if (plagiarismCounter) {
      const current = total ? Math.min(plagiarismState.index + 1, total) : 0;
      plagiarismCounter.textContent = `${current} / ${total}`;
    }
    if (plagiarismPrevButton) {
      plagiarismPrevButton.disabled = total <= 1 || plagiarismState.index === 0;
    }
    if (plagiarismNextButton) {
      plagiarismNextButton.disabled = total <= 1 || plagiarismState.index >= total - 1;
    }
  }

  async function fetchPlagiarismData() {
    const dataset = getActiveDatasetForPlagiarism();
    if (!dataset) {
      plagiarismState.blocks = [];
      plagiarismState.snippetCache.clear();
      updatePlagiarismControls();
      showPlagiarismEmptyState("Select a dataset to inspect duplicate blocks.");
      return;
    }
    plagiarismState.dataset = dataset;
    plagiarismContainer.dataset.dataset = dataset;
    plagiarismState.snippetCache.clear();
    showPlagiarismEmptyState("Loading duplicate blocks…");
    try {
      const summary = await getDuplicateSummary(dataset, summaryFilename);
      const blocks = summary ? filterCrossFileBlocks(summary.blocks) : [];
      plagiarismState.blocks = blocks;
      plagiarismState.index = 0;
      updatePlagiarismControls();
      if (!blocks.length) {
        showPlagiarismEmptyState("No duplicate blocks detected.");
        return;
      }
      await renderPlagiarismBlock();
    } catch (error) {
      plagiarismState.blocks = [];
      updatePlagiarismControls();
      showPlagiarismEmptyState(error.message);
    }
  }

  async function getFileContentCached(dataset, relativePath, fallbackPath) {
    const normalizedPath = normalizeRelativePath(relativePath || fallbackPath || "");
    if (!normalizedPath) throw new Error("Missing file path in duplicate block.");
    const datasetName = dataset || getActiveDatasetForPlagiarism();
    const key = `${datasetName || ""}:${normalizedPath}`;
    if (!plagiarismState.snippetCache.has(key)) {
      const params = new URLSearchParams({ filename: normalizedPath });
      if (datasetName) params.set("dataset", datasetName);
      const promise = requestJSON(`${API_ROUTES.fileContent}?${params.toString()}`);
      plagiarismState.snippetCache.set(key, promise);
    }
    const payload = await plagiarismState.snippetCache.get(key);
    if (!payload) return "";
    if (payload.file && typeof payload.file.content === "string") {
      return payload.file.content;
    }
    if (typeof payload.content === "string") {
      return payload.content;
    }
    return "";
  }

  function buildSnippetLines(content, start, end) {
    const lines = content.split(/\r?\n/);
    const startNum = Number(start);
    const endNum = Number(end);
    const startLine = Number.isFinite(startNum) ? startNum : 1;
    const endLine = Number.isFinite(endNum) ? endNum : startLine;
    const before = 10;
    const after = 10;
    const resolvedStart = startLine || 1;
    const resolvedEnd = endLine || resolvedStart;
    const snippetStart = Math.max(resolvedStart - before, 1);
    const snippetEnd = Math.min(resolvedEnd + after, lines.length || 1);
    const snippetLines = [];
    for (let line = snippetStart; line <= snippetEnd; line += 1) {
      snippetLines.push({
        number: line,
        text: lines[line - 1] ?? "",
        highlight: line >= start && line <= end,
      });
    }
    return snippetLines;
  }

  function buildSnippetElement(entry, snippetLines) {
    const wrapper = document.createElement("div");
    wrapper.className = "plagiarism-file";

    const title = document.createElement("h3");
    const rel = normalizeRelativeEntryPath(entry);
    const startNum = Number(entry.startLine);
    const endNum = Number(entry.endLine);
    const start = Number.isFinite(startNum) ? startNum : "";
    const end = Number.isFinite(endNum) ? endNum : "";
    title.textContent = `${rel || entry.filePath || "Unknown file"} ${start && end ? `(${start} ~ ${end})` : ""}`;
    wrapper.appendChild(title);

    const snippet = document.createElement("div");
    snippet.className = "plagiarism-snippet";
    if (!snippetLines.length) {
      const empty = document.createElement("div");
      empty.textContent = "Unable to render snippet.";
      snippet.appendChild(empty);
    } else {
      snippetLines.forEach((line) => {
        const lineRow = document.createElement("div");
        lineRow.className = "plagiarism-snippet-line";

        const lineNumber = document.createElement("span");
        lineNumber.className = "plagiarism-line-number";
        lineNumber.textContent = line.number.toString().padStart(4, " ");

        const lineText = document.createElement("span");
        lineText.className = "plagiarism-line-text";
        if (line.highlight) {
          lineText.classList.add("highlight");
        }
        lineText.textContent = line.text || " ";

        lineRow.appendChild(lineNumber);
        lineRow.appendChild(lineText);
        snippet.appendChild(lineRow);
      });
    }

    wrapper.appendChild(snippet);
    return wrapper;
  }

  async function renderPlagiarismBlock() {
    const total = plagiarismState.blocks.length;
    if (!total) {
      showPlagiarismEmptyState("No duplicate blocks detected.");
      return;
    }
    const index = Math.min(plagiarismState.index, total - 1);
    const block = plagiarismState.blocks[index];
    plagiarismContainer.innerHTML = "";
    const grid = document.createElement("div");
    grid.className = "plagiarism-grid";
    try {
      const panels = await Promise.all(
        block.entries.map(async (entry) => {
          const content = await getFileContentCached(plagiarismState.dataset, entry.relativePath, entry.filePath);
          const snippetLines = buildSnippetLines(content, entry.startLine, entry.endLine);
          return buildSnippetElement(entry, snippetLines);
        }),
      );
      panels.forEach((panel) => grid.appendChild(panel));
      plagiarismContainer.appendChild(grid);
    } catch (error) {
      showPlagiarismEmptyState(error.message);
    }
  }

  function initPlagiarismViewer() {
    if (plagiarismPrevButton) {
      plagiarismPrevButton.addEventListener("click", async () => {
        if (plagiarismState.index > 0) {
          plagiarismState.index -= 1;
          updatePlagiarismControls();
          await renderPlagiarismBlock();
        }
      });
    }
    if (plagiarismNextButton) {
      plagiarismNextButton.addEventListener("click", async () => {
        if (plagiarismState.index < plagiarismState.blocks.length - 1) {
          plagiarismState.index += 1;
          updatePlagiarismControls();
          await renderPlagiarismBlock();
        }
      });
    }
    fetchPlagiarismData();
  }

  onReady(initPlagiarismViewer);
})();
