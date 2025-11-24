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
        "aboveFence": "Value far above the dataset—investigate first.",
        "aboveQ3": "Value above most files—confirm the difference is warranted.",
        "belowFence": "Extremely low value—could indicate an incomplete or atypical file.",
        "belowQ1": "Below most files—confirm this behavior is intended.",
    },
    "duplication": {
        "aboveFence": "Very high duplication—likely heavy copy/paste or generated fragments.",
        "aboveQ3": "Notable duplication—consolidate repeated sections or shared helpers.",
        "belowFence": "Low duplication—positive, but confirm the file is not overly minimal.",
        "belowQ1": "Low duplication—positive, but confirm the file is not overly minimal.",
    },
    "complexity": {
        "aboveFence": "Extreme cyclomatic complexity—prioritize a refactor.",
        "aboveQ3": "High complexity—consider splitting or simplifying conditional branches.",
        "belowFence": "Very low complexity—could indicate a stub or incomplete code.",
        "belowQ1": "Low complexity—could indicate a stub or incomplete code.",
    },
    "size": {
        "aboveFence": "Large file—risk of a God object; split into smaller modules.",
        "aboveQ3": "Larger than most—see if responsibilities can be extracted.",
        "belowFence": "Very short file—ensure expected logic is not missing.",
        "belowQ1": "Short file—ensure expected logic is not missing.",
    },
    "functions": {
        "aboveFence": "Many functions—possible mix of responsibilities; consider grouping by domain.",
        "aboveQ3": "Function count above the norm—quickly review for grouping opportunities.",
        "belowFence": "Few functions—the file may be too limited or still a stub.",
        "belowQ1": "Few functions—the file may be too limited or still a stub.",
        "zero": "No functions—file likely incomplete or incorrect.",
    },
    "nesting": {
        "aboveFence": "Very deep nesting—refactor by extracting functions to reduce depth.",
        "aboveQ3": "High nesting—simplify branches or use early returns.",
    },
}

# Clustering defaults
CLUSTERING_METRIC_KEYS: list[str] = [
    "NCSS",
    "CCN",
    "Functions",
    "Duplication (%)",
    "Max nesting depth",
    "NCSS/Functions",
    "CCN/Functions",
]

# Ollama defaults
DEFAULT_OLLAMA_BASE_URL = os.environ.get("OLLAMA_BASE_URL", "http://localhost:11434")
DEFAULT_OLLAMA_EMBED_MODEL = os.environ.get("OLLAMA_EMBED_MODEL", "bge-m3")
DEFAULT_OLLAMA_TIMEOUT = 60.0

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
