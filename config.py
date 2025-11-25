"""Centralized configuration defaults for the project."""

from pathlib import Path
import os

# Application wiring
APP_TITLE = "FastAPI Placeholder Dashboard"
STATIC_DIR = Path("Static")
STATIC_MOUNT_PATH = "/static"
TEMPLATES_DIR = STATIC_DIR / "templates"

# Dataset storage and filenames
DATASETS_DIR = Path("Datasets")
RAW_FOLDER_NAME = "raw"
LIZARD_FOLDER_NAME = "lizard"
STRUCTURAL_FOLDER_NAME = "structural"
STRUCTURAL_EMBEDDINGS_FOLDER_NAME = "structural_embeddings"
STRUCTURAL_SOURCE_EXTENSIONS = {".scala"}
STRUCTURAL_EMBEDDING_SUFFIX = ".embedding.json"
STATUS_FILENAME = "status.json"
SUMMARY_FILENAME = "lizard_dataset.xml"
METRICS_FILENAME = "metrics.csv"
REFERENCE_METRICS_FILENAME = "reference_metrics.json"
STUDENTS_OUTLIERS_FILENAME = "students_outliers.json"
CLUSTERING_CACHE_FILENAME = "clustering.csv"
CLUSTERING_META_FILENAME = "clustering_meta.json"
REPORTS_SUBDIR = "reports"
STUDENTS_REPORT_FILENAME = "students_outliers_report.md"

# Processing pipeline labels
PROCESSING_PHASES = {
    "extracting": "Extracting dataset.",
    "analyzing_lizard": "Running Lizard analysis.",
    "computing_metrics": "Computing derived metrics.",
    "computing_outliers": "Computing student thresholds.",
    "building_structural": "Building structural views.",
    "building_embeddings": "Generating structural embeddings.",
    "ready": "Dataset ready.",
}

# Threshold descriptions exposed to the UI
THRESHOLD_DESCRIPTIONS: dict[str, dict[str, str]] = {
    "generic": {
        "aboveFence": "Value far above the dataset—investigate first; prioritize reviewing logic and intent.",
        "aboveQ3": "Value above most files—confirm the difference is warranted and consistent with expectations.",
        "belowFence": "Extremely low value—could indicate an incomplete or atypical file; verify coverage and inputs.",
        "belowQ1": "Below most files—confirm this behavior is intended and not missing required work.",
    },
    "duplication": {
        "aboveFence": "Very high duplication—likely heavy copy/paste or redundant fragments; refactor into shared helpers.",
        "aboveQ3": "Notable duplication—consolidate repeated sections or extract shared helpers to reduce redundancy.",
        "belowFence": "Low duplication—positive, but confirm the file is not overly minimal or missing logic.",
        "belowQ1": "Low duplication—positive, but confirm the file is not overly minimal or missing logic.",
    },
    "complexity": {
        "aboveFence": "Extreme cyclomatic complexity—prioritize a refactor; break apart branches and nestings.",
        "aboveQ3": "High complexity—consider splitting or simplifying conditional branches and loops.",
        "belowFence": "Very low complexity—could indicate a stub or incomplete code; confirm expected behavior exists.",
        "belowQ1": "Low complexity—could indicate a stub or incomplete code; confirm expected behavior exists.",
    },
    "size": {
        "aboveFence": "Large file—risk of a God object; split into smaller modules and separate responsibilities.",
        "aboveQ3": "Larger than most—see if responsibilities can be extracted or separated for clarity.",
        "belowFence": "Very short file—ensure expected logic is not missing or offloaded incorrectly.",
        "belowQ1": "Short file—ensure expected logic is not missing or overly stubbed.",
    },
    "functions": {
        "aboveFence": "Many functions—possible mix of responsibilities; consider grouping by domain and reducing churn.",
        "aboveQ3": "Function count above the norm—review for grouping opportunities and cohesion.",
        "belowFence": "Few functions—the file may be too limited or still a stub; validate completeness.",
        "belowQ1": "Few functions—the file may be too limited or still a stub; validate completeness.",
        "zero": "No functions—file likely incomplete or incorrect.",
    },
    "nesting": {
        "aboveFence": "Very deep nesting—refactor by extracting functions to reduce depth and improve readability.",
        "aboveQ3": "High nesting—simplify branches or use early returns to flatten logic.",
    },
}

# Clustering defaults
CLUSTERING_THEMES: dict[str, dict[str, object]] = {
    "complexity": {
        "label": "Complexity and logic structure",
        "metrics": ["CCN", "Max nesting depth", "If/NCSS", "Loops/NCSS"],
        "description": "Focus on branching, depth, and control-flow shape.",
    },
    "size": {
        "label": "Size and duplication",
        "metrics": ["NCSS", "Duplication (%)"],
        "description": "Highlight very small or oversized files and duplicated code.",
    },
    "functions": {
        "label": "Split into functions",
        "metrics": ["Functions", "NCSS/Functions", "Vars/Functions"],
        "description": "Check whether logic is broken down into smaller pieces.",
    },
    "style": {
        "label": "Style",
        "metrics": ["Total variables", "Vars/NCSS"],
        "description": "Look at naming density and variable usage.",
    },
}
CLUSTERING_METRIC_KEYS: list[str] = sorted(
    {metric for theme in CLUSTERING_THEMES.values() for metric in theme.get("metrics", [])} | {"CCN/Functions"}
)

# Ollama defaults
DEFAULT_OLLAMA_BASE_URL = os.environ.get("OLLAMA_BASE_URL", "http://localhost:11434")
DEFAULT_OLLAMA_EMBED_MODEL = os.environ.get("OLLAMA_EMBED_MODEL", "bge-m3")
DEFAULT_OLLAMA_TIMEOUT = 60.0
FEEDBACK_MODEL = "danielsheep/Qwen3-Coder-30B-A3B-Instruct-1M-Unsloth:UD-Q4_K_XL"
# "danielsheep/Qwen3-Coder-30B-A3B-Instruct-1M-Unsloth:UD-Q4_K_XL"
FEEDBACK_PROMPT_WITH_REQUIREMENTS = (
    "You are a code reviewer for an introductory programming course.  "
    "Your goal is to evaluate a student's code based *only* on the given requirements."
    "For each requirement :"
    " 1. Restate the requirement in 1 sentence."
    " 2. Evaluate whether the student's code satisfies it."
    " 3. Provide concise, beginner-friendly comments explaining why (with references to the code)."
    "After the requirement-by-requirement analysis, write:"
    "- **Strengths:** What the student did well, focusing on clarity, structure, and correctness observable in the code."
    "- **Weaknesses:** Concrete issues found in the code (without speculating about intent)."
    "- **Improvements:** Actionable suggestions tailored for beginners (simpler logic, clearer structure, reducing duplication, using functions, reducing nesting, etc.)."
    "Keep the tone constructive, short and educational."
    "Do not mention grading or score"
    "Format the feedback in markdown with clear sections : "
    "Requirement Analysis :"
    " <one section per requirement>"
    "Strengths :"
    " <bullet points>"
    "Weaknesses :"
    " <bullet points>"
    "Improvements :"
    " <bullet points with beginner friendly suggestions>"
    "Requirements:\\n{requirements}\\n\\nCode for {filename}:\\n{code}"
)
FEEDBACK_PROMPT_NO_REQUIREMENTS = (
    "You are a code reviewer. Provide concise, actionable feedback (strengths, issues, and suggested fixes) "
    "for the following code file: {filename}.\\n\\nCode:\\n{code}"
)
FEEDBACK_OUTPUT_DIRNAME = "feedbacks"
CLUSTER_LABEL_PROMPT = (
    "You are summarizing clusters of student code files."
    "Your task is to compare : the cluster’s average metrics, the dataset-wide average metrics, the structure of the representative file."
    "Use only this information, produce : "
    "- a short label (max 10 words) that captures what distinguishes this cluster"
    "- 2-3 sentences explaining what characterizes this cluster."
    "Instructions :"
    "Base your analysis strictly on the metrics provided (cluster vs dataset) and the representative file content/structure."
    "Do not use any external standards or typical good/bad coding practices. Only compare values relatively (higher, lower, similar)."
    "When analyzing the representative file, comment on observable structural aspects such as:"
    "number and size of functions, depth of nesting, presence or absence of duplication, overall organization and readabilit"
    "Do not speculate about unobservable aspects like code functionality, correctness, or intent."
    "Be concise, factual and grounded only in the data provided."
    "Format as: \\nLabel: <short label>\\nDescription: <sentences>.\\n"
    "Cluster metrics: {cluster_metrics}\\nDataset averages: {dataset_metrics}\\nRepresentative file: {representative}"

)
CLUSTER_LABEL_PROMPT_OLD = (
    "You are summarizing clusters of code files. Given the cluster's average metrics, "
    "the dataset-wide average metrics, and the most representative file path, propose a short label "
    "(max 10 words) and 2-3 sentences explaining what characterizes this cluster. "
    "Only base your summary on the metrics and the representative file; do not speculate about other aspects. "
    "When comparing the metrics only compare them to the provided metrics and not usual good/bad values. "
    "Format as: \\nLabel: <short label>\\nDescription: <sentences>.\\n"
    "Cluster metrics: {cluster_metrics}\\nDataset averages: {dataset_metrics}\\nRepresentative file: {representative}"
)

# Lizard analysis defaults
DEFAULT_LIZARD_BIN = "lizard"
LIZARD_SHOW_OUTPUT = False
LIZARD_MIN_DUPLICATE_LINES = 30

# Autotest settings
AUTOTEST_BASE_DIR = Path(__file__).resolve().parent / "AutoTest"
AUTOTEST_STUDENT_FILE = AUTOTEST_BASE_DIR / "Main.scala"
AUTOTEST_TEST_FILE = AUTOTEST_BASE_DIR / "tests.json"
AUTOTEST_OUTPUT_DIR = AUTOTEST_BASE_DIR / "work"
AUTOTEST_SCALA_BIN = "scala"
AUTOTEST_SCALAC_BIN = "scalac"
AUTOTEST_DEFAULT_COMMAND_DELAY = 0.5
AUTOTEST_DEFAULT_SCENARIO_TIMEOUT = 15

# User-facing messages (shared between backend and frontend)
MESSAGES: dict[str, str] = {
    # Datasets
    "DATASET_CREATED_PROCESSING": 'Dataset "{dataset}" created. Processing in progress...',
    "DATASET_ALREADY_EXISTS": "Dataset already exists.",
    "DATASET_NOT_FOUND": "Dataset not found.",
    "DATASET_DELETED": 'Dataset "{dataset}" deleted.',
    "DATASET_INVALID_NAME": "Dataset name contains invalid characters.",
    "DATASET_NO_SELECTION": "No dataset selected.",
    "DATASET_STATUS_FAILED": "Dataset processing failed.",
    "DATASET_READY": 'Dataset "{dataset}" ready.',
    "INVALID_ZIP": "File must be a .zip archive.",
    "INVALID_FILE_PATH": "Invalid file path.",
    # Files
    "FILE_NOT_FOUND": "File not found.",
    "RAW_FOLDER_MISSING": "Raw folder not found.",
    # Metrics / Lizard
    "LIZARD_NOT_FOUND": "Lizard analysis not found.",
    # Clustering
    "CLUSTERING_NO_DATA": "No usable data for this feature mode. Check metrics and embeddings.",
    "CLUSTERING_KMEANS_COUNT_REQUIRED": "Cluster count is required to run k-means.",
    "CLUSTERING_HDBSCAN_PARAMS_REQUIRED": "HDBSCAN parameters are required.",
    "CLUSTERING_INVALID_HDBSCAN_VALUES": "Invalid HDBSCAN values.",
    "CLUSTERING_AUTO_HDBSCAN": "Auto-searching HDBSCAN parameters...",
    "CLUSTERING_AUTO_KMEANS": "Auto-searching best k...",
    "CLUSTERING_RUNNING": "Running clustering...",
    "CLUSTERING_AUTO_KMEANS_ON": "Auto k-means enabled (silhouette score).",
    "CLUSTERING_AUTO_KMEANS_OFF": "Auto k-means disabled.",
    "CLUSTERING_AUTO_HDBSCAN_ON": "Auto HDBSCAN enabled; parameters locked.",
    "CLUSTERING_AUTO_HDBSCAN_OFF": "Auto mode disabled.",
    "CLUSTERING_COMPLETED": 'Clustering {algorithm} completed{details} ({count} file{plural}).',
    "CLUSTERING_LOADED": 'Clustering {algorithm} loaded{details} ({count} file{plural}).',
    "CLUSTERING_NO_METRICS": "Run a clustering job to display the metric distribution.",
    "CLUSTERING_NO_VALUES": "No values available for this metric.",
    "CLUSTERING_DISTRIBUTION": "Showing distribution for {metric} ({count} file{plural}).",
    # Students/reporting
    "STUDENTS_SELECT_DATASET": "Select a dataset to display outliers.",
    "STUDENTS_LOADING": "Loading precomputed metrics...",
    "STUDENTS_NO_OUTLIERS": "No files outside the Q1-Q3 range.",
    "STUDENTS_NO_CARDS": "No cards for this metric.",
    "STUDENTS_OUTLIERS_COUNT": "Showing outliers for {count} card{plural}.",
    "STUDENTS_NO_FILES": "No files.",
    "STUDENTS_NO_METRICS": "No metrics",
    "LABEL_MEDIAN": "Median",
    "LABEL_FENCES": "Fences",
    "REPORT_GENERATING": "Generating report...",
    "REPORT_READY": "Report generated and downloaded.",
    # Uploads
    "UPLOAD_REFERENCE_START": 'Uploading reference for {dataset}...',
    "UPLOAD_REFERENCE_SUCCESS": 'Reference file "{filename}" saved for {dataset}.',
    "UPLOAD_REQUIREMENTS_START": 'Uploading requirements for {dataset}...',
    "UPLOAD_REQUIREMENTS_SUCCESS": 'Requirements "{filename}" saved for {dataset}.',
    "UPLOAD_DATASET_START": "Uploading dataset...",
    "UPLOAD_IN_PROGRESS": "Uploading...",
    "NO_DATASETS": "No datasets yet.",
    # Confirmation
    "CONFIRM_DELETE_DATASET": 'Delete dataset "{dataset}"?',
    # Generic
    "ACTION_FAILED": "Operation failed.",
    "FEEDBACK_DATASET_QUEUED": "Dataset-wide feedback generation queued (not implemented).",
    "FEEDBACK_FILE_QUEUED": "Feedback generation queued (not implemented).",
}
