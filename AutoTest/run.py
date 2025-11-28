import subprocess
import time
import yaml
from pathlib import Path
import json

# -----------------------
# CONFIGURATION
# -----------------------

SCALA_DIR = Path("AutoTest/")
# Command to run your Scala program
SCALA_COMPILE_CMD = ["scalac", "Main.scala"]
SCALA_CMD = ["scala", "Main"]
# Alternative:
# SCALA_CMD = ["java", "-cp", "student.jar", "Main"]

TEST_FILE = "Autotest/tests.yaml"
RESULT_FILE = "results_ex1.json"


compile = subprocess.run(
        SCALA_COMPILE_CMD,
        cwd=SCALA_DIR,
        text=True,
        capture_output=True
)
# -----------------------
# LOAD YAML TESTS
# -----------------------

with open(TEST_FILE, "r", encoding="utf-8") as f:
    tests_yaml = yaml.safe_load(f)

tests = tests_yaml["tests"]
print("INPUTS LUS PAR YAML:", tests)
results = []

# -----------------------
# RUN TESTS
# -----------------------

for test in tests:
    print(f"→ Running test {test['id']}...")

    input_str = "\n".join(test["inputs"]) + "\n"

    start_time = time.time()
    try:
        proc = subprocess.run(
            SCALA_CMD,
            cwd=SCALA_DIR,
            input=input_str,
            text=True,
            capture_output=True,
            timeout=10  # seconds
        )
        duration = (time.time() - start_time) * 1000
    except subprocess.TimeoutExpired:
        results.append({
            "id": test["id"],
            "status": "timeout",
            "duration_ms": None,
            "stdout": "",
            "stderr": "",
            "errors": ["Execution timed out"]
        })
        continue

    stdout = proc.stdout
    stderr = proc.stderr

    # Evaluate expected outputs
    errors = []
    for expected in test.get("expected_contains", []):
        if expected.lower() not in stdout.lower():
            errors.append(f"Missing expected text: {expected}")

    results.append({
        "id": test["id"],
        "description": test["description"],
        "status": "ok" if len(errors) == 0 else "failed",
        "duration_ms": duration,
        "stdout": stdout,
        "stderr": stderr,
        "exit_code": proc.returncode,
        "errors": errors
    })

# -----------------------
# SAVE REPORT
# -----------------------

with open(RESULT_FILE, "w", encoding="utf-8") as f:
    json.dump(results, f, indent=4, ensure_ascii=False)

print(f"\n✔ Tests finished. Results saved to {RESULT_FILE}")
