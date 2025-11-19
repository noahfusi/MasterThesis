import os
import subprocess
import json
import re
import shutil
import time

# -------------------------------------------------
# CONFIG
# -------------------------------------------------

# On derive les chemins par rapport au dossier de ce script pour éviter
# les erreurs lorsque le script est lancé depuis un autre répertoire.
BASE_DIR     = os.path.dirname(os.path.abspath(__file__))
STUDENT_FILE = os.path.join(BASE_DIR, "Main.scala")   # fichier scala du student
TEST_FILE    = os.path.join(BASE_DIR, "tests.json")     # fichier JSON avec scénarios
OUTPUT_DIR   = os.path.join(BASE_DIR, "work")                 # dossier de compilation
SCALA_BIN    = "scala"
SCALAC_BIN   = "scalac"

DEFAULT_COMMAND_DELAY = 0.5   # délai par défaut entre les inputs
DEFAULT_SCENARIO_TIMEOUT = 15

# -------------------------------------------------
# UTILS
# -------------------------------------------------

def compile_scala(source_file, output_dir):
    """
    Compile un fichier Scala avec scalac
    """
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)

    compile_cmd = [SCALAC_BIN, source_file, "-d", output_dir]

    proc = subprocess.run(
        compile_cmd,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True
    )

    success = proc.returncode == 0
    return success, proc.stdout, proc.stderr


def _sanitize_float(value, default):
    try:
        parsed = float(value)
        if parsed < 0:
            return 0.0
        return parsed
    except (TypeError, ValueError):
        return default


def run_scenario(main_class, scenario, classpath):
    """Exécute une classe Scala en envoyant les commandes d'un scénario."""
    cmd = [SCALA_BIN, "-classpath", classpath, main_class]
    commands = scenario.get("commands", [])
    default_delay = _sanitize_float(
        scenario.get("command_delay_seconds", DEFAULT_COMMAND_DELAY),
        DEFAULT_COMMAND_DELAY,
    )
    timeout_seconds = _sanitize_float(
        scenario.get("timeout_seconds", DEFAULT_SCENARIO_TIMEOUT),
        DEFAULT_SCENARIO_TIMEOUT,
    )

    stdin_stream = None
    try:
        proc = subprocess.Popen(
            cmd,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True
        )
        stdin_stream = proc.stdin
    except FileNotFoundError:
        return -1, "", f"Impossible de lancer {SCALA_BIN}."

    try:
        for command in commands:
            if proc.poll() is not None:
                break

            if stdin_stream is None or stdin_stream.closed:
                break

            raw_value = str(command.get("input", ""))
            if not raw_value.endswith("\n"):
                raw_value += "\n"

            try:
                stdin_stream.write(raw_value)
                stdin_stream.flush()
            except (BrokenPipeError, ValueError):
                break

            delay_value = _sanitize_float(
                command.get("delay_seconds", default_delay),
                default_delay,
            )
            if delay_value > 0:
                time.sleep(delay_value)

        if stdin_stream and not stdin_stream.closed:
            try:
                stdin_stream.close()
            except ValueError:
                pass
        proc.stdin = None

        stdout, stderr = proc.communicate(timeout=timeout_seconds)
        return proc.returncode, stdout, stderr

    except subprocess.TimeoutExpired:
        proc.kill()
        return -1, "", "Timeout"


def evaluate_test(test, output):
    """
    Compare la sortie réelle avec les règles du test JSON
    """
    # Vérifications obligatoires
    for req in test.get("require", []):
        if req["type"] == "regex":
            if not re.search(req["value"], output, re.DOTALL):
                return False, f"Missing required pattern: {req['value']}"
        elif req["type"] == "plain":
            if req["value"] not in output:
                return False, f"Missing required text: {req['value']}"

    # Vérifications interdites
    for forb in test.get("forbid", []):
        if re.search(forb["value"], output, re.DOTALL):
            return False, f"Forbidden pattern matched: {forb['value']}"

    return True, "OK"


# -------------------------------------------------
# MAIN
# -------------------------------------------------

def main():
    print("=== AUTOGRADER SCALA I/O ===")

    # Charger les scénarios
    with open(TEST_FILE, encoding="utf-8") as fh:
        data = json.load(fh)
    scenarios = data.get("scenarios", [])

    # Préparer compilation
    shutil.rmtree(OUTPUT_DIR, ignore_errors=True)
    os.makedirs(OUTPUT_DIR, exist_ok=True)

    # Compiler le code étudiant
    print("Compiling student code...")
    success, out, err = compile_scala(STUDENT_FILE, OUTPUT_DIR)

    if not success:
        print("Compilation failed!\n")
        print(err)
        return

    print("Compilation OK.\n")

    # Lancer les scénarios un par un
    for scenario in scenarios:
        scenario_name = scenario.get("name", "Unnamed scenario")
        print(f"--- Scénario: {scenario_name} ---")

        ret, stdout, stderr = run_scenario("Main", scenario, OUTPUT_DIR)

        print("PROGRAM OUTPUT:")
        print(stdout)

        if stderr:
            print("PROGRAM STDERR:")
            print(stderr)

        ok, msg = evaluate_test(scenario, stdout)
        score = scenario.get("score", 0)

        if ok:
            print(f"RESULT: OK  (+{score} points)")
        else:
            print(f"RESULT: FAIL ({msg})")

        print()

    print("=== END ===")


if __name__ == "__main__":
    main()
