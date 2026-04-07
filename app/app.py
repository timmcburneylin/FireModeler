from __future__ import annotations

import json
import os
import shutil
import subprocess
import time
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, List

import pandas as pd
import streamlit as st
from PIL import Image


ROOT = Path(__file__).resolve().parents[1]
CONFIG_PATH = ROOT / "config" / "config.json"
CONFIG_EXAMPLE_PATH = ROOT / "config" / "config.example.json"
PROJECTS_DIR = ROOT / "projects"

REQUIRED_RAW_DIRS = [
    "SNAP",
    "Weather",
    "FuelCalc",
    "FuelCalcBC",
    "Outputs",
    "Stand_StockTables",
]

REQUIRED_PROJECT_FILES = [
    ("SNAP overstory", "raw/SNAP/{project_name}_OS.csv"),
    ("SNAP understory", "raw/SNAP/{project_name}_US.csv"),
    ("SNAP extra", "raw/SNAP/{project_name}_EXTRA.csv"),
    ("SNAP fuels", "raw/SNAP/{project_name}_FUELS.csv"),
    ("Daily weather FWI", "raw/Weather/{project_name}_Daily_FWI_AllYear.csv"),
]

STEP0_REQUIRED_FILES = [
    ("SNAP overstory", "raw/SNAP/{project_name}_OS.csv"),
    ("SNAP understory", "raw/SNAP/{project_name}_US.csv"),
    ("SNAP extra", "raw/SNAP/{project_name}_EXTRA.csv"),
    ("SNAP fuels", "raw/SNAP/{project_name}_FUELS.csv"),
]


def load_json(path: Path) -> Dict[str, Any]:
    if not path.exists():
        return {}
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except Exception as exc:
        return {"_error": str(exc)}


def load_config() -> Dict[str, Any]:
    if CONFIG_PATH.exists():
        return load_json(CONFIG_PATH)
    return load_json(CONFIG_EXAMPLE_PATH)


def save_config(cfg: Dict[str, Any]) -> None:
    CONFIG_PATH.write_text(json.dumps(cfg, indent=2), encoding="utf-8")

def trigger_rerun() -> None:
    if hasattr(st, "rerun"):
        st.rerun()
    else:
        st.experimental_rerun()


def list_projects() -> List[str]:
    if not PROJECTS_DIR.exists():
        return []
    return sorted([p.name for p in PROJECTS_DIR.iterdir() if p.is_dir()])


def project_dir(project_name: str) -> Path:
    return PROJECTS_DIR / project_name


def project_data_dir(project_name: str) -> Path:
    return project_dir(project_name) / "data"


def project_manifest_path(project_name: str) -> Path:
    return project_data_dir(project_name) / "outputs" / "manifest" / "pipeline_manifest.json"


def project_status_path(project_name: str) -> Path:
    return project_data_dir(project_name) / "outputs" / "run_status" / "pipeline_status.json"


def project_step3_progress_path(project_name: str) -> Path:
    return project_data_dir(project_name) / "outputs" / "step3_fire_model" / "step3_progress.json"


def resolve_rscript() -> str | None:
    cfg = st.session_state.get("config_state", load_config())
    configured = str(cfg.get("rscript_path", "")).strip()
    if configured and Path(configured).exists():
        return configured

    for candidate in ("Rscript", "Rscript.exe"):
        found = shutil.which(candidate)
        if found:
            return found

    r_home = os.environ.get("R_HOME", "").strip()
    if r_home:
        for rel in ("bin/Rscript.exe", "bin/x64/Rscript.exe"):
            candidate = Path(r_home) / rel
            if candidate.exists():
                return str(candidate)

    program_files = [os.environ.get("ProgramFiles", r"C:\Program Files")]
    for base in program_files:
        r_root = Path(base) / "R"
        if not r_root.exists():
            continue
        version_dirs = sorted([p for p in r_root.iterdir() if p.is_dir() and p.name.startswith("R-")], reverse=True)
        for version_dir in version_dirs:
            for rel in ("bin/Rscript.exe", "bin/x64/Rscript.exe"):
                candidate = version_dir / rel
                if candidate.exists():
                    return str(candidate)
    return None


def initialize_step3_progress(project_name: str) -> None:
    progress_path = project_step3_progress_path(project_name)
    progress_path.parent.mkdir(parents=True, exist_ok=True)
    progress_payload = {
        "project_name": project_name,
        "status": "starting",
        "timestamp": datetime.now().isoformat(timespec="seconds"),
        "period": None,
        "weather_index": 0,
        "weather_total": 0,
        "stratum": None,
        "iter": 0,
        "total_iters": 0,
        "message": "Step 3 queued and waiting for R to initialize",
    }
    progress_path.write_text(json.dumps(progress_payload, indent=2), encoding="utf-8")


def environment_status(cfg: Dict[str, Any]) -> List[Dict[str, str]]:
    venv_python = ROOT / ".venv" / "Scripts" / "python.exe"
    rscript = resolve_rscript()
    configured_rscript = str(cfg.get("rscript_path", "")).strip()
    fuelcalc_configured = str(cfg.get("fuelcalc_path", "")).strip()
    fuelcalc_default = Path(r"C:\Program Files (x86)\FuelCalc1.7")
    fuelcalc_found = Path(fuelcalc_configured).exists() if fuelcalc_configured else fuelcalc_default.exists()
    fuelcalc_path = fuelcalc_configured or str(fuelcalc_default)

    rows = [
        {
            "component": "Python venv",
            "status": "Ready" if venv_python.exists() else "Missing",
            "details": str(venv_python) if venv_python.exists() else "Run setup_windows.bat or scripts/bootstrap_windows.ps1",
        },
        {
            "component": "Rscript",
            "status": "Ready" if rscript else "Missing",
            "details": rscript or (configured_rscript if configured_rscript else "Set rscript_path in config/config.json or install R"),
        },
        {
            "component": "FuelCalc",
            "status": "Ready" if fuelcalc_found else "Missing",
            "details": fuelcalc_path,
        },
        {
            "component": "Config file",
            "status": "Ready" if CONFIG_PATH.exists() else "Using example",
            "details": str(CONFIG_PATH if CONFIG_PATH.exists() else CONFIG_EXAMPLE_PATH),
        },
    ]
    return rows


def ensure_project_dirs(project_name: str) -> None:
    base = project_data_dir(project_name)
    for rel in ("raw", "intermediate", "outputs", "external"):
        (base / rel).mkdir(parents=True, exist_ok=True)
    for rel in REQUIRED_RAW_DIRS:
        (base / "raw" / rel).mkdir(parents=True, exist_ok=True)


def create_project(project_name: str) -> None:
    ensure_project_dirs(project_name)
    raw_readme = project_data_dir(project_name) / "raw" / "README.md"
    if not raw_readme.exists():
        raw_readme.write_text(
            "# Raw Inputs\n\n"
            "Put project-specific raw inputs in this folder tree.\n",
            encoding="utf-8",
        )


def project_validation_rows(project_name: str) -> List[Dict[str, Any]]:
    rows: List[Dict[str, Any]] = []
    if not project_name:
        return rows

    base = project_data_dir(project_name)
    for label, rel_template in REQUIRED_PROJECT_FILES:
        rel_path = rel_template.format(project_name=project_name)
        full_path = base / rel_path
        rows.append(
            {
                "type": "file",
                "label": label,
                "relative_path": rel_path,
                "exists": full_path.exists(),
            }
        )

    for rel in REQUIRED_RAW_DIRS:
        full_path = base / "raw" / rel
        rows.append(
            {
                "type": "directory",
                "label": rel,
                "relative_path": f"raw/{rel}",
                "exists": full_path.exists(),
            }
        )
    return rows


def step0_validation_rows(project_name: str) -> List[Dict[str, Any]]:
    rows: List[Dict[str, Any]] = []
    if not project_name:
        return rows

    base = project_data_dir(project_name)
    for label, rel_template in STEP0_REQUIRED_FILES:
        rel_path = rel_template.format(project_name=project_name)
        full_path = base / rel_path
        rows.append(
            {
                "label": label,
                "expected_name": Path(rel_path).name,
                "relative_path": rel_path,
                "exists": full_path.exists(),
            }
        )
    return rows


def step1_input_ready(project_name: str, step0_status: Any) -> bool:
    if not project_name:
        return False
    step0_summary = project_data_dir(project_name) / "intermediate" / "step0_snap_to_process" / "snap_to_process_summary.json"
    step0_complete = (step0_status is True) or step0_summary.exists()
    if not step0_complete:
        return False
    stand_stocktables_dir = project_data_dir(project_name) / "raw" / "Stand_StockTables"
    if not stand_stocktables_dir.exists():
        return False
    # Require at least one uploaded file under Stand_StockTables before enabling Step 1.
    return any(path.is_file() for path in stand_stocktables_dir.rglob("*"))


def step2_weather_input(project_name: str, weather_code: int) -> Dict[str, Any]:
    if not project_name:
        return {"exists": False, "path": "", "name": ""}
    weather_name = f"{int(weather_code)}.csv"
    full_path = project_data_dir(project_name) / "raw" / "Weather" / "raw" / weather_name
    return {
        "exists": full_path.exists(),
        "path": str(full_path),
        "name": weather_name,
    }


def run_pipeline(steps: List[str] | None = None) -> Dict[str, Any]:
    rscript = resolve_rscript()
    if not rscript:
        return {
            "returncode": 1,
            "stdout": "",
            "stderr": (
                "Could not find Rscript.exe. Install R or set `rscript_path` in config/config.json "
                "to the full path of Rscript.exe."
            ),
            "ran_at": datetime.now().isoformat(timespec="seconds"),
        }
    cmd = [rscript, "R/run_pipeline.R"]
    if steps:
        cmd.append(f"--steps={','.join(steps)}")
    try:
        proc = subprocess.run(cmd, cwd=ROOT, capture_output=True, text=True)
    except FileNotFoundError:
        return {
            "returncode": 1,
            "stdout": "",
            "stderr": (
                f"Could not launch Rscript at `{rscript}`. Install R or update `rscript_path` "
                "in config/config.json."
            ),
            "ran_at": datetime.now().isoformat(timespec="seconds"),
        }
    return {
        "returncode": proc.returncode,
        "stdout": proc.stdout,
        "stderr": proc.stderr,
        "ran_at": datetime.now().isoformat(timespec="seconds"),
    }


def start_pipeline_async(steps: List[str], run_key: str, log_dir: Path) -> None:
    rscript = resolve_rscript()
    if not rscript:
        st.session_state.last_run = {
            "returncode": 1,
            "stdout": "",
            "stderr": (
                "Could not find Rscript.exe. Install R or set `rscript_path` in config/config.json "
                "to the full path of Rscript.exe."
            ),
            "ran_at": datetime.now().isoformat(timespec="seconds"),
        }
        return
    log_dir.mkdir(parents=True, exist_ok=True)
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    stdout_path = log_dir / f"{run_key}_{timestamp}_stdout.log"
    stderr_path = log_dir / f"{run_key}_{timestamp}_stderr.log"
    cmd = [rscript, "R/run_pipeline.R", f"--steps={','.join(steps)}"]
    stdout_handle = open(stdout_path, "w", encoding="utf-8")
    stderr_handle = open(stderr_path, "w", encoding="utf-8")
    proc = subprocess.Popen(cmd, cwd=ROOT, stdout=stdout_handle, stderr=stderr_handle, text=True)
    stdout_handle.close()
    stderr_handle.close()
    st.session_state[f"{run_key}_job"] = {
        "proc": proc,
        "stdout_path": str(stdout_path),
        "stderr_path": str(stderr_path),
        "ran_at": datetime.now().isoformat(timespec="seconds"),
        "steps": steps,
    }


def sync_async_pipeline_run(run_key: str) -> Dict[str, Any] | None:
    job = st.session_state.get(f"{run_key}_job")
    if not job:
        return None
    proc = job["proc"]
    returncode = proc.poll()
    if returncode is None:
        return {"status": "running", **job}

    stdout = Path(job["stdout_path"]).read_text(encoding="utf-8", errors="ignore") if Path(job["stdout_path"]).exists() else ""
    stderr = Path(job["stderr_path"]).read_text(encoding="utf-8", errors="ignore") if Path(job["stderr_path"]).exists() else ""
    result = {
        "status": "completed",
        "returncode": returncode,
        "stdout": stdout,
        "stderr": stderr,
        "ran_at": job["ran_at"],
    }
    st.session_state.last_run = result
    del st.session_state[f"{run_key}_job"]
    return result


def open_folder(path: Path) -> None:
    path.mkdir(parents=True, exist_ok=True)
    if hasattr(os, "startfile"):
        os.startfile(str(path))
    else:
        raise RuntimeError("Folder opening is only supported on Windows in this UI.")


def fuelcalc_batch_files(project_name: str) -> List[Path]:
    if not project_name:
        return []
    batch_root = project_data_dir(project_name) / "raw" / "FuelCalc"
    if not batch_root.exists():
        return []
    return sorted(batch_root.rglob("Run_FuelCalc_*.bat"))


def fuelcalc_error_reports(project_name: str) -> List[Dict[str, Any]]:
    if not project_name:
        return []
    outputs_root = project_data_dir(project_name) / "raw" / "FuelCalc" / "Outputs"
    if not outputs_root.exists():
        return []

    reports: List[Dict[str, Any]] = []
    for error_file in sorted(outputs_root.rglob("*_Errors.txt")):
        treatment = error_file.parent.name
        try:
            content = error_file.read_text(encoding="utf-8", errors="ignore").strip()
        except Exception as exc:
            content = f"Failed to read file: {exc}"
        reports.append(
            {
                "treatment": treatment,
                "error_file": str(error_file),
                "has_errors": bool(content),
                "content": content,
            }
        )
    return reports


def run_fuelcalc_batches(batch_files: List[Path]) -> Dict[str, Any]:
    results: List[Dict[str, Any]] = []
    overall_success = True
    for batch_file in batch_files:
        proc = subprocess.run(
            ["cmd", "/c", str(batch_file)],
            cwd=batch_file.parent,
            capture_output=True,
            text=True,
        )
        item = {
            "batch_file": str(batch_file),
            "returncode": proc.returncode,
            "stdout": proc.stdout,
            "stderr": proc.stderr,
        }
        results.append(item)
        if proc.returncode != 0:
            overall_success = False

    return {
        "ran_at": datetime.now().isoformat(timespec="seconds"),
        "success": overall_success,
        "results": results,
    }


def ensure_list_length(values: List[Any], n: int, default: Any) -> List[Any]:
    values = list(values or [])
    if len(values) >= n:
        return values[:n]
    return values + [default] * (n - len(values))


def parse_treatment_names(value: str) -> List[str]:
    return [item.strip() for item in value.split(",") if item.strip()]


def parse_csv_text(value: str) -> List[str]:
    return [item.strip() for item in value.split(",") if item.strip()]


def parse_float_csv(value: str) -> List[float]:
    return [float(item.strip()) for item in value.split(",") if item.strip()]


def parse_int_csv(value: str) -> List[int]:
    return [int(float(item.strip())) for item in value.split(",") if item.strip()]


def parse_bool_csv(value: str) -> List[bool]:
    return [item.strip().lower() in {"true", "t", "1", "yes", "y"} for item in value.split(",") if item.strip()]


def flatten_manifest(manifest: Dict[str, Any]) -> pd.DataFrame:
    rows: List[Dict[str, Any]] = []
    for section in ("step0", "step1", "step2", "step3"):
        block = manifest.get(section, {})
        if not isinstance(block, dict):
            continue
        for name, item in block.items():
            if not isinstance(item, dict):
                continue
            rows.append(
                {
                    "section": section,
                    "artifact": name,
                    "exists": item.get("exists"),
                    "size_bytes": item.get("size_bytes"),
                    "path": item.get("path"),
                }
            )
    return pd.DataFrame(rows)


def show_json_file(path_str: str, title: str) -> None:
    if not path_str:
        return
    path = Path(path_str)
    st.write(f"**{title}**")
    if not path.exists():
        st.warning(f"Missing: {path}")
        return
    data = load_json(path)
    st.json(data)


def show_csv_preview(path_str: str, title: str, n: int = 25) -> None:
    if not path_str:
        return
    path = Path(path_str)
    st.write(f"**{title}**")
    if not path.exists():
        st.warning(f"Missing: {path}")
        return
    try:
        df = pd.read_csv(path)
        st.dataframe(df.head(n), use_container_width=True)
    except Exception as exc:
        st.error(f"Failed to read CSV: {exc}")


def show_image_file(path_str: str, title: str) -> None:
    if not path_str:
        return
    path = Path(path_str)
    st.write(f"**{title}**")
    if not path.exists():
        st.warning(f"Missing: {path}")
        return
    try:
        with Image.open(path) as img:
            st.image(img.copy(), use_column_width=True)
    except Exception as exc:
        st.error(f"Failed to display image {path.name}: {exc}")


def show_image_grid(items: List[tuple[str, str]], columns: int = 3) -> None:
    valid_items = [(path_str, title) for path_str, title in items if path_str]
    if not valid_items:
        return
    for start in range(0, len(valid_items), columns):
        row_items = valid_items[start:start + columns]
        cols = st.columns(columns)
        for idx, (path_str, title) in enumerate(row_items):
            with cols[idx]:
                show_image_file(path_str, title)


def show_step_result(step_key: str, title: str) -> None:
    last_step = st.session_state.get("last_step_action")
    last_run = st.session_state.get("last_run", {})
    if last_step != step_key or not last_run:
        return

    ran_at = last_run.get("ran_at", "Unknown")
    if last_run.get("returncode", 1) == 0:
        st.success(f"{title} finished successfully at {ran_at}.")
    else:
        st.error(f"{title} failed at {ran_at}.")
        stderr = (last_run.get("stderr") or "").strip()
        stdout = (last_run.get("stdout") or "").strip()
        details = stderr or stdout or "No log output captured."
        st.code(details, language="text")


def show_step3_progress(project_name: str) -> None:
    if not project_name:
        return
    progress_data = load_json(project_step3_progress_path(project_name))
    if not progress_data or progress_data.get("_error"):
        return

    total_iters = progress_data.get("total_iters")
    iter_value = progress_data.get("iter")
    status = str(progress_data.get("status", "unknown"))
    message = progress_data.get("message")
    period = progress_data.get("period")
    weather_index = progress_data.get("weather_index")
    weather_total = progress_data.get("weather_total")
    stratum = progress_data.get("stratum")
    timestamp = progress_data.get("timestamp")

    st.markdown("**Step 3 Progress**")

    progress_fraction = 0.0
    if isinstance(total_iters, (int, float)) and total_iters not in (None, 0) and isinstance(iter_value, (int, float)):
        progress_fraction = max(0.0, min(float(iter_value) / float(total_iters), 1.0))
    st.progress(progress_fraction)

    pct_label = f"{progress_fraction * 100:.1f}%"
    st.caption(
        f"Status: `{status}` | Iteration: `{iter_value or 0}` / `{total_iters or 0}` | Completion: `{pct_label}`"
    )

    detail_parts = []
    if period is not None:
        detail_parts.append(f"Period: `{period}`")
    if weather_index is not None and weather_total is not None:
        detail_parts.append(f"Weather: `{weather_index}` / `{weather_total}`")
    if stratum is not None:
        detail_parts.append(f"Stratum: `{stratum}`")
    if timestamp:
        detail_parts.append(f"Updated: `{timestamp}`")
    if detail_parts:
        st.write(" | ".join(detail_parts))
    if message:
        st.caption(str(message))

    if status == "completed":
        st.success("Step 3 completed.")


def style_step0_validation(df: pd.DataFrame) -> Any:
    def highlight_missing(row: pd.Series) -> List[str]:
        color = "#f8d7da" if not bool(row.get("exists")) else ""
        return [f"background-color: {color}"] * len(row)

    return df.style.apply(highlight_missing, axis=1)


st.set_page_config(page_title="FireModel UI", layout="wide")
st.title("FireModel Pipeline UI")
st.caption("Manage projects, validate inputs, run pipeline, and inspect outputs.")

if "last_run" not in st.session_state:
    st.session_state.last_run = {}
if "last_fuelcalc_run" not in st.session_state:
    st.session_state.last_fuelcalc_run = {}
if "last_step_action" not in st.session_state:
    st.session_state.last_step_action = ""
if "config_state" not in st.session_state:
    st.session_state.config_state = load_config()

cfg = st.session_state.config_state
env_rows = environment_status(cfg)
saved_project = str(cfg.get("project_name", "")).strip()
known_projects = list_projects()
default_project = saved_project if saved_project in known_projects else (known_projects[0] if known_projects else "")

with st.expander("Environment Check", expanded=True):
    env_df = pd.DataFrame(env_rows)
    st.dataframe(env_df, use_container_width=True, hide_index=True)
    missing_env = [row for row in env_rows if row["status"] == "Missing"]
    if missing_env:
        st.warning("Some setup items are missing. Run `setup_windows.bat` or `scripts/bootstrap_windows.ps1` before using the pipeline on a new machine.")
    else:
        st.success("Core environment checks look good.")

left, right = st.columns([1, 3])
with left:
    st.subheader("Project")
    if known_projects:
        selected_project = st.selectbox(
            "Select Project",
            options=known_projects,
            index=known_projects.index(default_project) if default_project in known_projects else 0,
            help="Choose an existing project folder under projects/.",
        )
    else:
        selected_project = ""
        st.info("No project folders found under projects/.")
    current_project = selected_project or saved_project
    step0_rows = step0_validation_rows(current_project)
    step0_df = pd.DataFrame(step0_rows)
    step0_ready = bool(current_project) and (not step0_df.empty) and bool(step0_df["exists"].all())

    if st.button("Use Selected Project", use_container_width=True):
        if not current_project:
            st.error("Select a project first.")
        else:
            ensure_project_dirs(current_project)
            cfg["project_name"] = current_project
            save_config(cfg)
            st.session_state.config_state = cfg
            st.success(f"Using project {current_project}")
            trigger_rerun()

    step0_help = "Step 0 requires exactly these files in raw/SNAP: <project>_OS.csv, <project>_US.csv, <project>_EXTRA.csv, <project>_FUELS.csv"
    st.markdown("**Step 0 Required Files**")
    if current_project:
        st.markdown(
            "\n".join(
                [
                    f"- `{current_project}_OS.csv`",
                    f"- `{current_project}_US.csv`",
                    f"- `{current_project}_EXTRA.csv`",
                    f"- `{current_project}_FUELS.csv`",
                ]
            )
        )
    else:
        st.markdown(
            "\n".join(
                [
                    "- `<project>_OS.csv`",
                    "- `<project>_US.csv`",
                    "- `<project>_EXTRA.csv`",
                    "- `<project>_FUELS.csv`",
                ]
            )
        )
    if st.button("Run Step 0", use_container_width=True, disabled=not step0_ready, help=step0_help):
        ensure_project_dirs(current_project)
        cfg["project_name"] = current_project
        save_config(cfg)
        st.session_state.config_state = cfg
        with st.spinner("Running Step 0..."):
            st.session_state.last_step_action = "step0_snap_to_process"
            st.session_state.last_run = run_pipeline(["step0_snap_to_process"])

    if st.button("Refresh", use_container_width=True):
        trigger_rerun()

current_project = str(st.session_state.config_state.get("project_name", "")).strip()
manifest = load_json(project_manifest_path(current_project)) if current_project else {}
status = load_json(project_status_path(current_project)) if current_project else {}
step3_async_state = sync_async_pipeline_run("step3_fire_model")
validation_rows = project_validation_rows(current_project)
validation_df = pd.DataFrame(validation_rows)
step0_rows = step0_validation_rows(current_project)
step0_df = pd.DataFrame(step0_rows)
step0_ready = bool(current_project) and (not step0_df.empty) and bool(step0_df["exists"].all())
batch_files = fuelcalc_batch_files(current_project)
error_reports = fuelcalc_error_reports(current_project)
step0_status = status.get("steps", {}).get("step0_snap_to_process", {}).get("success", "Unknown") if status else "Unknown"
step1_ready = step1_input_ready(current_project, step0_status)

with right:
    c1, c2, c3, c4 = st.columns(4)
    c1.metric("Project", current_project or "Unset")
    c2.metric("Step 0 Ready", "Yes" if step0_ready else "No")
    c3.metric("Artifacts", int(flatten_manifest(manifest).shape[0]) if manifest else 0)
    c4.metric("Step 0 Last Run", step0_status)

if st.session_state.last_run:
    with st.expander("Last Pipeline Run Logs", expanded=False):
        st.write(f"Ran at: {st.session_state.last_run.get('ran_at')}")
        st.code(st.session_state.last_run.get("stdout", ""), language="text")
        if st.session_state.last_run.get("stderr"):
            st.code(st.session_state.last_run.get("stderr", ""), language="text")

if st.session_state.last_fuelcalc_run:
    with st.expander("Last FuelCalc Batch Run Logs", expanded=False):
        st.write(f"Ran at: {st.session_state.last_fuelcalc_run.get('ran_at')}")
        st.write(f"Success: {st.session_state.last_fuelcalc_run.get('success')}")
        for result in st.session_state.last_fuelcalc_run.get("results", []):
            st.write(f"**{Path(result['batch_file']).name}**")
            st.code(result.get("stdout", ""), language="text")
            if result.get("stderr"):
                st.code(result.get("stderr", ""), language="text")

tabs = st.tabs(["Inputs", "Status", "Artifacts", "Data Preview"])

with tabs[0]:
    st.subheader("Step 0")
    if not current_project:
        st.info("Select or create a project first.")
    else:
        st.write(f"Project folder: `{project_dir(current_project)}`")
        st.write("Expected Step 0 files for the selected project:")
        if step0_df.empty:
            st.info("No Step 0 validation checks available.")
        else:
            st.dataframe(style_step0_validation(step0_df), use_container_width=True)
            missing_step0 = step0_df[~step0_df["exists"]]
            if missing_step0.empty:
                st.success("Step 0 is ready to run.")
            else:
                st.error("Step 0 is blocked. Required files are missing or misnamed.")
                st.write("Missing or incorrectly named files:")
                st.dataframe(missing_step0[["label", "expected_name", "relative_path"]], use_container_width=True)
        show_step_result("step0_snap_to_process", "Step 0")

        with st.expander("Full Project Validation", expanded=False):
            if validation_df.empty:
                st.info("No broader validation checks available.")
            else:
                st.dataframe(validation_df, use_container_width=True)
                missing = validation_df[~validation_df["exists"]]
                if missing.empty:
                    st.success("All broader project folders/files currently checked are present.")
                else:
                    st.warning("Some non-Step-0 project inputs are still missing.")

        st.divider()
        st.subheader("Step Guidance")
        stand_stocktables_dir = project_data_dir(current_project) / "raw" / "Stand_StockTables"
        st.write(
            "After Step 0, upload the required Stand_StockTables files into:"
        )
        st.code(str(stand_stocktables_dir), language="text")
        if st.button("Open Stand_StockTables Folder", use_container_width=False):
            try:
                open_folder(stand_stocktables_dir)
                st.success("Opened Stand_StockTables folder in Explorer.")
            except Exception as exc:
                st.error(f"Failed to open folder: {exc}")

        st.divider()
        st.subheader("Step 1")
        st.caption("Process To FuelCalc")
        process_cfg = cfg.get("process_to_fuelcalc", {})
        treatment_names_default = process_cfg.get("tr_names", ["A", "B", "C"])
        treatment_names_text = st.text_input(
            "Treatment names",
            value=", ".join(treatment_names_default),
            help="Comma-separated treatment names. All per-treatment settings below follow this order.",
        )
        treatment_names = parse_treatment_names(treatment_names_text) or ["A", "B", "C"]
        n_treatments = len(treatment_names)

        cutting_value = st.checkbox("Are you cutting?", value=bool(process_cfg.get("cutting", True)))
        moist_value = st.selectbox(
            "10hr fuel moisture",
            options=["VeryDry", "Dry", "Moderate", "Wet"],
            index=["VeryDry", "Dry", "Moderate", "Wet"].index(str(process_cfg.get("moist", "VeryDry"))),
        )

        thinning_values = ensure_list_length(process_cfg.get("thinning_target_order", [593, 647, 683]), n_treatments, 593)
        prune_values = ensure_list_length(process_cfg.get("prune", [2, 2, 2]), n_treatments, 2)
        burning_values = ensure_list_length(process_cfg.get("burning_flags", [False, False, False]), n_treatments, False)
        thin_values = ensure_list_length(process_cfg.get("thin_flags", [True, True, True]), n_treatments, True)
        prune_flag_values = ensure_list_length(process_cfg.get("prune_flags", [True, True, True]), n_treatments, True)
        emission_values = ensure_list_length(process_cfg.get("emission_factor", [4, 4, 4]), n_treatments, 4)
        surf_fuel_values = ensure_list_length(process_cfg.get("surf_fuel", ["TimberLitter", "TimberLitter", "TimberLitter"]), n_treatments, "TimberLitter")
        bole_char_values = ensure_list_length(process_cfg.get("bole_char_height", [2, 2, 2]), n_treatments, 2)
        climate_values = ensure_list_length(process_cfg.get("climate", ["Arid", "Arid", "Arid"]), n_treatments, "Arid")
        moist_regime_values = ensure_list_length(process_cfg.get("moist_regimes", ["VeryDry", "VeryDry", "VeryDry"]), n_treatments, "VeryDry")
        season_values = ensure_list_length(process_cfg.get("season", ["Summer", "Summer", "Summer"]), n_treatments, "Summer")

        ui_thinning_values: List[int] = []
        ui_prune_values: List[float] = []
        ui_burning_values: List[bool] = []
        ui_thin_values: List[bool] = []
        ui_prune_flag_values: List[bool] = []
        ui_emission_values: List[int] = []
        ui_surf_fuel_values: List[str] = []
        ui_bole_char_values: List[float] = []
        ui_climate_values: List[str] = []
        ui_moist_regime_values: List[str] = []
        ui_season_values: List[str] = []

        for i, treatment_name in enumerate(treatment_names):
            with st.expander(f"Treatment {treatment_name}", expanded=(i == 0)):
                col1, col2 = st.columns(2)
                with col1:
                    ui_thinning_values.append(
                        int(
                            st.number_input(
                                f"Thinning target TPH - {treatment_name}",
                                min_value=0,
                                value=int(thinning_values[i]),
                                step=1,
                                key=f"thinning_{i}",
                            )
                        )
                    )
                    ui_prune_values.append(
                        float(
                            st.number_input(
                                f"Prune height (m) - {treatment_name}",
                                min_value=0.0,
                                value=float(prune_values[i]),
                                step=0.5,
                                key=f"prune_{i}",
                            )
                        )
                    )
                    ui_burning_values.append(
                        st.checkbox(
                            f"Burning - {treatment_name}",
                            value=bool(burning_values[i]),
                            key=f"burning_{i}",
                        )
                    )
                with col2:
                    ui_thin_values.append(
                        st.checkbox(
                            f"Thinning - {treatment_name}",
                            value=bool(thin_values[i]),
                            key=f"thin_{i}",
                        )
                    )
                    ui_prune_flag_values.append(
                        st.checkbox(
                            f"Pruning - {treatment_name}",
                            value=bool(prune_flag_values[i]),
                            key=f"prune_flag_{i}",
                        )
                    )
                    ui_bole_char_values.append(
                        float(
                            st.number_input(
                                f"Bole char height (m) - {treatment_name}",
                                min_value=0.0,
                                value=float(bole_char_values[i]),
                                step=0.5,
                                key=f"bole_char_{i}",
                            )
                        )
                    )

                ui_emission_values.append(
                    int(
                        st.selectbox(
                            f"Emission factor - {treatment_name}",
                            options=[2, 3, 4, 5, 6],
                            index=[2, 3, 4, 5, 6].index(int(emission_values[i])) if int(emission_values[i]) in [2, 3, 4, 5, 6] else 2,
                            key=f"emission_{i}",
                        )
                    )
                )
                ui_surf_fuel_values.append(
                    st.selectbox(
                        f"Surface fuel type - {treatment_name}",
                        options=["Grass", "Chaparral", "TimberLitter", "Slash"],
                        index=["Grass", "Chaparral", "TimberLitter", "Slash"].index(str(surf_fuel_values[i])) if str(surf_fuel_values[i]) in ["Grass", "Chaparral", "TimberLitter", "Slash"] else 2,
                        key=f"surf_fuel_{i}",
                    )
                )
                ui_climate_values.append(
                    st.selectbox(
                        f"Climate - {treatment_name}",
                        options=["Humid", "Arid"],
                        index=["Humid", "Arid"].index(str(climate_values[i])) if str(climate_values[i]) in ["Humid", "Arid"] else 1,
                        key=f"climate_{i}",
                    )
                )

                if ui_burning_values[i]:
                    burn_col1, burn_col2 = st.columns(2)
                    with burn_col1:
                        ui_moist_regime_values.append(
                            st.selectbox(
                                f"Burn moisture regime - {treatment_name}",
                                options=["VeryDry", "Dry", "Moderate", "Wet"],
                                index=["VeryDry", "Dry", "Moderate", "Wet"].index(str(moist_regime_values[i])) if str(moist_regime_values[i]) in ["VeryDry", "Dry", "Moderate", "Wet"] else 0,
                                key=f"moist_regime_{i}",
                            )
                        )
                    with burn_col2:
                        ui_season_values.append(
                            st.selectbox(
                                f"Burn season - {treatment_name}",
                                options=["Spring", "Summer", "Fall", "Winter"],
                                index=["Spring", "Summer", "Fall", "Winter"].index(str(season_values[i])) if str(season_values[i]) in ["Spring", "Summer", "Fall", "Winter"] else 1,
                                key=f"season_{i}",
                            )
                        )
                else:
                    ui_moist_regime_values.append(str(moist_regime_values[i]))
                    ui_season_values.append(str(season_values[i]))

        save_col, run_col = st.columns(2)
        if save_col.button("Save Step 1 Settings", use_container_width=True):
            cfg["process_to_fuelcalc"] = {
                "cutting": cutting_value,
                "tr_names": treatment_names,
                "thinning_target_order": ui_thinning_values,
                "prune": ui_prune_values,
                "burning_flags": ui_burning_values,
                "thin_flags": ui_thin_values,
                "prune_flags": ui_prune_flag_values,
                "emission_factor": ui_emission_values,
                "surf_fuel": ui_surf_fuel_values,
                "bole_char_height": ui_bole_char_values,
                "moist": moist_value,
                "climate": ui_climate_values,
                "moist_regimes": ui_moist_regime_values,
                "season": ui_season_values,
            }
            save_config(cfg)
            st.session_state.config_state = cfg
            st.success("Saved Step 1 settings.")

        step1_help = "Run Step 0 first, then upload the required Stand_StockTables files before running Step 1."
        if run_col.button("Run Step 1", use_container_width=True, disabled=not step1_ready, help=step1_help):
            cfg["process_to_fuelcalc"] = {
                "cutting": cutting_value,
                "tr_names": treatment_names,
                "thinning_target_order": ui_thinning_values,
                "prune": ui_prune_values,
                "burning_flags": ui_burning_values,
                "thin_flags": ui_thin_values,
                "prune_flags": ui_prune_flag_values,
                "emission_factor": ui_emission_values,
                "surf_fuel": ui_surf_fuel_values,
                "bole_char_height": ui_bole_char_values,
                "moist": moist_value,
                "climate": ui_climate_values,
                "moist_regimes": ui_moist_regime_values,
                "season": ui_season_values,
            }
            cfg["project_name"] = current_project
            save_config(cfg)
            st.session_state.config_state = cfg
            with st.spinner("Running Step 1..."):
                st.session_state.last_step_action = "step1_process_to_fuelcalc"
                st.session_state.last_run = run_pipeline(["step1_process_to_fuelcalc"])
            trigger_rerun()
        show_step_result("step1_process_to_fuelcalc", "Step 1")

        st.divider()
        st.subheader("FuelCalc")
        st.caption("Run generated FuelCalc batch files and review FuelCalc error files")
        if batch_files:
            st.write("Generated FuelCalc batch files:")
            st.dataframe(
                pd.DataFrame({"batch_file": [str(path) for path in batch_files]}),
                use_container_width=True,
            )
            if st.button("Run FuelCalc Batch Files", use_container_width=False):
                with st.spinner("Running FuelCalc batch files..."):
                    st.session_state.last_fuelcalc_run = run_fuelcalc_batches(batch_files)
                if st.session_state.last_fuelcalc_run.get("success"):
                    st.success("FuelCalc batch files completed.")
                else:
                    st.error("One or more FuelCalc batch files failed. Check the logs below.")
        else:
            st.info("No FuelCalc batch files found yet. Run Step 1 first to generate them.")

        st.subheader("FuelCalc Error Review")
        if error_reports:
            error_df = pd.DataFrame(
                [
                    {
                        "treatment": report["treatment"],
                        "error_file": report["error_file"],
                        "has_errors": report["has_errors"],
                    }
                    for report in error_reports
                ]
            )
            st.dataframe(error_df, use_container_width=True)
            for report in error_reports:
                label = f"{report['treatment']} Errors"
                with st.expander(label, expanded=bool(report["has_errors"])):
                    st.write(report["error_file"])
                    if report["content"]:
                        st.code(report["content"], language="text")
                    else:
                        st.success("No errors reported in this file.")
        else:
            st.info("No FuelCalc error files found yet. Run FuelCalc first to generate them.")

        st.divider()
        st.subheader("Step 2")
        st.caption("FuelCalc To FireModel")
        step2_cfg = cfg.get("fuelcalc_to_firemodel", {})
        weather_raw_dir = project_data_dir(current_project) / "raw" / "Weather" / "raw"
        st.markdown(
            "Upload raw weather station data before running Step 2. "
            "Supported sources are `EC` (Environment Canada) and `MOF` (Ministry of Forests)."
        )
        st.markdown(
            "Find the source data from the "
            "[Pacific Climate Data Services portal](https://services.pacificclimate.org/met-data-portal-pcds/app/)."
        )
        st.write("Put the downloaded raw station CSV into:")
        st.code(str(weather_raw_dir), language="text")
        if st.button("Open Weather Raw Folder", use_container_width=False):
            try:
                open_folder(weather_raw_dir)
                st.success("Opened Weather raw folder in Explorer.")
            except Exception as exc:
                st.error(f"Failed to open folder: {exc}")
        step2_col1, step2_col2 = st.columns(2)
        with step2_col1:
            step2_weather_type = st.selectbox(
                "Weather station type",
                options=["MOF", "EC"],
                index=["MOF", "EC"].index(str(step2_cfg.get("weather_type", "MOF"))) if str(step2_cfg.get("weather_type", "MOF")) in ["MOF", "EC"] else 0,
                key="step2_weather_type",
            )
            step2_weather_name = st.text_input("Weather station name", value=str(step2_cfg.get("weather_name", "MERRITT 2 HUB")), key="step2_weather_name")
            step2_weather_code = int(st.number_input("Weather station code", min_value=0, value=int(step2_cfg.get("weather_code", 1399)), step=1, key="step2_weather_code"))
        with step2_col2:
            step2_weather_lat = float(st.number_input("Weather latitude", value=float(step2_cfg.get("weather_lat", 50.121389)), format="%.6f", key="step2_weather_lat"))
            step2_weather_long = float(st.number_input("Weather longitude", value=float(step2_cfg.get("weather_long", -120.744167)), format="%.6f", key="step2_weather_long"))
            step2_danger_region = int(st.selectbox("Danger region", options=[1, 2, 3], index=[1, 2, 3].index(int(step2_cfg.get("danger_region", 3))), key="step2_danger_region"))

        fuelcalc_outputs_exist = any(
            path.is_file()
            for path in (project_data_dir(current_project) / "raw" / "FuelCalc" / "Outputs").rglob("*_FuelCalc_FFI_Outputs.csv")
        ) if current_project else False
        step2_weather_file = step2_weather_input(current_project, step2_weather_code)
        step2_ready = fuelcalc_outputs_exist and step2_weather_file["exists"]

        st.write("Expected raw weather input file:")
        st.code(step2_weather_file["path"], language="text")
        if step2_weather_file["exists"]:
            st.success(f"Found weather input file: {step2_weather_file['name']}")
        else:
            st.error(f"Missing weather input file: {step2_weather_file['name']}")
            st.caption("Step 2 expects a user-provided raw station CSV in this exact location. The script does not download weather data itself.")

        step2_save_col, step2_run_col = st.columns(2)
        if step2_save_col.button("Save Step 2 Settings", use_container_width=True):
            cfg["fuelcalc_to_firemodel"] = {
                "weather_type": step2_weather_type,
                "weather_name": step2_weather_name,
                "weather_code": step2_weather_code,
                "weather_lat": step2_weather_lat,
                "weather_long": step2_weather_long,
                "danger_region": step2_danger_region,
            }
            save_config(cfg)
            st.session_state.config_state = cfg
            st.success("Saved Step 2 settings.")

        step2_help = "Run Step 1 and FuelCalc first, and place the raw weather CSV at raw/Weather/raw/<station_code>.csv before Step 2."
        if step2_run_col.button("Run Step 2", use_container_width=True, disabled=not step2_ready, help=step2_help):
            cfg["fuelcalc_to_firemodel"] = {
                "weather_type": step2_weather_type,
                "weather_name": step2_weather_name,
                "weather_code": step2_weather_code,
                "weather_lat": step2_weather_lat,
                "weather_long": step2_weather_long,
                "danger_region": step2_danger_region,
            }
            save_config(cfg)
            st.session_state.config_state = cfg
            with st.spinner("Running Step 2..."):
                st.session_state.last_step_action = "step2_fuelcalc"
                st.session_state.last_run = run_pipeline(["step2_fuelcalc"])
            trigger_rerun()
        show_step_result("step2_fuelcalc", "Step 2")

        if manifest:
            st.markdown("**Step 2 Plots**")
            show_image_grid(
                [
                    (manifest.get("step2", {}).get("wind_rose", {}).get("path", ""), "Wind Rose"),
                    (manifest.get("step2", {}).get("danger_days", {}).get("path", ""), "Danger Days"),
                    (manifest.get("step2", {}).get("weather_conditions", {}).get("path", ""), "Weather Conditions"),
                ],
                columns=3,
            )

        st.divider()
        st.subheader("Step 3")
        st.caption("FireModel Results")
        step3_cfg = cfg.get("firemodel_results", {})
        step3_summary_exists = bool(manifest.get("step2", {}).get("summary", {}).get("exists")) if manifest else False
        step3_col1, step3_col2 = st.columns(2)
        with step3_col1:
            step3_elevation = float(st.number_input("Elevation (m)", value=float(step3_cfg.get("elevation", 483)), step=1.0, key="step3_elevation"))
            step3_intensity_flag = st.selectbox("Intensity flag", options=["Byram", "Nelson", "Rothermel"], index=["Byram", "Nelson", "Rothermel"].index(str(step3_cfg.get("intensity_flag", "Byram"))) if str(step3_cfg.get("intensity_flag", "Byram")) in ["Byram", "Nelson", "Rothermel"] else 0, key="step3_intensity_flag")
            step3_fuel_moisture_type = st.selectbox("Fuel moisture type", options=["Model", "Wotton"], index=["Model", "Wotton"].index(str(step3_cfg.get("fuel_moisture_type", "Model"))) if str(step3_cfg.get("fuel_moisture_type", "Model")) in ["Model", "Wotton"] else 0, key="step3_fuel_moisture_type")
            step3_heat_flag = st.selectbox("Heat flag", options=["Manual", "Nelson"], index=["Manual", "Nelson"].index(str(step3_cfg.get("heat_flag", "Manual"))) if str(step3_cfg.get("heat_flag", "Manual")) in ["Manual", "Nelson"] else 0, key="step3_heat_flag")
            step3_wind_gust_mod = st.checkbox("Modify for wind gusts", value=bool(step3_cfg.get("wind_gust_mod", False)), key="step3_wind_gust_mod")
            step3_grass_curing = float(st.number_input("Grass curing (%)", value=float(step3_cfg.get("grass_curing", 75)), step=1.0, key="step3_grass_curing"))
            step3_num_weathers = int(st.number_input("Number of weather days", min_value=1, value=int(step3_cfg.get("num_weathers", 250)), step=1, key="step3_num_weathers"))
            step3_weather_name = st.text_input("Weather name for Step 3", value=str(step3_cfg.get("weather_name", step2_weather_name)), key="step3_weather_name")
            step3_season = st.selectbox("Season", options=["Spring", "Summer", "Winter"], index=["Spring", "Summer", "Winter"].index(str(step3_cfg.get("season", "Summer"))) if str(step3_cfg.get("season", "Summer")) in ["Spring", "Summer", "Winter"] else 1, key="step3_season")
            step3_advanced_models = st.checkbox("Plot advanced models", value=bool(step3_cfg.get("advanced_models", True)), key="step3_advanced_models")
        with step3_col2:
            st.caption("Comma-separated lists for per-treatment inputs. Keep the order aligned to the treatment names above.")
            step3_custom_fuels = st.text_input("Custom fuels", value=", ".join(map(str, step3_cfg.get("custom_fuels", [True, True, True]))), key="step3_custom_fuels")
            step3_prune_vector = st.text_input("Prune vector", value=", ".join(map(str, step3_cfg.get("prune_vector", [2, 2, 2]))), key="step3_prune_vector")
            step3_fuels = st.text_input("Fine fuels", value=", ".join(map(str, step3_cfg.get("fuels", [0.75, 0.75, 0.75]))), key="step3_fuels")
            step3_hr1000s = st.text_input("1000hr fuels", value=", ".join(map(str, step3_cfg.get("hr1000s", [1, 1, 1]))), key="step3_hr1000s")
            step3_ftcad_vector = st.text_input("Post-treatment fuel types", value=", ".join(map(str, step3_cfg.get("ftcad_vector", ["C-7", "C-7", "C-7"]))), key="step3_ftcad_vector")
            step3_forest_type = st.text_input("Forest type list", value=", ".join(map(str, step3_cfg.get("forest_type", ["Pine"] * 6))), key="step3_forest_type")
            step3_surf_fuel = st.text_input("Surface fuel list", value=", ".join(map(str, step3_cfg.get("surf_fuel", ["grass"] * 6))), key="step3_surf_fuel")
            step3_fsg_mod = st.text_input("FSG modify flags", value=", ".join(map(str, step3_cfg.get("fsg_mod_flag", [False, False, False]))), key="step3_fsg_mod")
            step3_fsg_field = st.text_input("FSG field flags", value=", ".join(map(str, step3_cfg.get("fsg_field_flag", [True, True, True]))), key="step3_fsg_field")
            step3_crown_fire_type = st.text_input("Crown fire type", value=", ".join(map(str, step3_cfg.get("crown_fire_type", ["Wagner", "Wagner", "Wagner"]))), key="step3_crown_fire_type")
            step3_crown_fire_model = st.text_input("Crown fire model", value=", ".join(map(str, step3_cfg.get("crown_fire_model", ["Perrakis", "Perrakis", "Perrakis"]))), key="step3_crown_fire_model")

        step3_save_col, step3_run_col = st.columns(2)
        if step3_save_col.button("Save Step 3 Settings", use_container_width=True):
            cfg["firemodel_results"] = {
                "elevation": step3_elevation,
                "custom_fuels": parse_bool_csv(step3_custom_fuels),
                "prune_vector": parse_float_csv(step3_prune_vector),
                "fuels": parse_float_csv(step3_fuels),
                "hr1000s": parse_float_csv(step3_hr1000s),
                "ftcad_vector": parse_csv_text(step3_ftcad_vector),
                "forest_type": parse_csv_text(step3_forest_type),
                "surf_fuel": parse_csv_text(step3_surf_fuel),
                "fsg_mod_flag": parse_bool_csv(step3_fsg_mod),
                "fsg_field_flag": parse_bool_csv(step3_fsg_field),
                "intensity_flag": step3_intensity_flag,
                "fuel_moisture_type": step3_fuel_moisture_type,
                "heat_flag": step3_heat_flag,
                "wind_gust_mod": step3_wind_gust_mod,
                "crown_fire_type": parse_csv_text(step3_crown_fire_type),
                "crown_fire_model": parse_csv_text(step3_crown_fire_model),
                "grass_curing": step3_grass_curing,
                "num_weathers": step3_num_weathers,
                "weather_name": step3_weather_name,
                "season": step3_season,
                "advanced_models": step3_advanced_models,
            }
            save_config(cfg)
            st.session_state.config_state = cfg
            st.success("Saved Step 3 settings.")

        step3_help = "Run Step 2 first so weather and fire behavior inputs are ready before Step 3."
        if step3_run_col.button("Run Step 3", use_container_width=True, disabled=not step3_summary_exists, help=step3_help):
            cfg["firemodel_results"] = {
                "elevation": step3_elevation,
                "custom_fuels": parse_bool_csv(step3_custom_fuels),
                "prune_vector": parse_float_csv(step3_prune_vector),
                "fuels": parse_float_csv(step3_fuels),
                "hr1000s": parse_float_csv(step3_hr1000s),
                "ftcad_vector": parse_csv_text(step3_ftcad_vector),
                "forest_type": parse_csv_text(step3_forest_type),
                "surf_fuel": parse_csv_text(step3_surf_fuel),
                "fsg_mod_flag": parse_bool_csv(step3_fsg_mod),
                "fsg_field_flag": parse_bool_csv(step3_fsg_field),
                "intensity_flag": step3_intensity_flag,
                "fuel_moisture_type": step3_fuel_moisture_type,
                "heat_flag": step3_heat_flag,
                "wind_gust_mod": step3_wind_gust_mod,
                "crown_fire_type": parse_csv_text(step3_crown_fire_type),
                "crown_fire_model": parse_csv_text(step3_crown_fire_model),
                "grass_curing": step3_grass_curing,
                "num_weathers": step3_num_weathers,
                "weather_name": step3_weather_name,
                "season": step3_season,
                "advanced_models": step3_advanced_models,
            }
            save_config(cfg)
            st.session_state.config_state = cfg
            st.session_state.last_step_action = "step3_fire_model"
            st.session_state.last_run = {}
            initialize_step3_progress(current_project)
            start_pipeline_async(
                ["step3_fire_model"],
                "step3_fire_model",
                project_data_dir(current_project) / "outputs" / "run_status",
            )
            trigger_rerun()
        show_step_result("step3_fire_model", "Step 3")
        show_step3_progress(current_project)
        if step3_async_state and step3_async_state.get("status") == "running":
            time.sleep(2)
            trigger_rerun()

        if manifest:
            st.markdown("**Step 3 Plots**")
            show_image_grid(
                [
                    (manifest.get("step3", {}).get("treatment_summary", {}).get("path", ""), "Treatment Summary"),
                    (manifest.get("step3", {}).get("crown_fire_probability_boxplots", {}).get("path", ""), "Crown Fire Probability BoxPlots"),
                    (manifest.get("step3", {}).get("crowning_index_windspeed", {}).get("path", ""), "Crowning Index at Windspeed"),
                    (manifest.get("step3", {}).get("crowning_index_fuelmoist", {}).get("path", ""), "Crowning Index at Fuel Moisture"),
                    (manifest.get("step3", {}).get("head_fire_intensity", {}).get("path", ""), "Head Fire Intensity"),
                    (manifest.get("step3", {}).get("rate_of_spread", {}).get("path", ""), "Rate of Spread"),
                    (manifest.get("step3", {}).get("fbp_90th_csi_stand", {}).get("path", ""), "FBP 90th CSI Stand"),
                ],
                columns=3,
            )

with tabs[1]:
    st.subheader("Run Status")
    if status:
        st.json(status)
    else:
        st.info("No status file yet. Run the pipeline first.")

with tabs[2]:
    st.subheader("Manifest Artifacts")
    if manifest:
        df = flatten_manifest(manifest)
        if not df.empty:
            st.dataframe(df, use_container_width=True)
        else:
            st.info("Manifest loaded but no artifacts found.")
    else:
        st.info("No manifest file yet. Run the pipeline first.")

with tabs[3]:
    st.subheader("Data Preview")
    if not manifest:
        st.info("No manifest found.")
    else:
        choices = {
            "Step 0 Summary": manifest.get("step0", {}).get("summary", {}).get("path", ""),
            "Process To FuelCalc Summary": manifest.get("step1", {}).get("summary", {}).get("path", ""),
            "FuelCalc To FireModel Summary": manifest.get("step2", {}).get("summary", {}).get("path", ""),
            "FireModel Results Summary": manifest.get("step3", {}).get("summary", {}).get("path", ""),
        }
        selected = st.selectbox("Choose dataset", list(choices.keys()))
        show_json_file(choices[selected], selected)
