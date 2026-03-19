from __future__ import annotations

import json
import os
import subprocess
from datetime import datetime
from pathlib import Path
from typing import Dict, List

import pandas as pd
import streamlit as st


ROOT = Path(__file__).resolve().parents[1]
CONFIG_PATH = ROOT / "config" / "config.json"
CONFIG_EXAMPLE_PATH = ROOT / "config" / "config.example.json"
PROJECTS_DIR = ROOT / "projects"

REQUIRED_RAW_DIRS = [
    "SNAP",
    "Weather",
    "templates",
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


def run_pipeline(steps: List[str] | None = None) -> Dict[str, Any]:
    cmd = ["Rscript", "R/run_pipeline.R"]
    if steps:
        cmd.append(f"--steps={','.join(steps)}")
    proc = subprocess.run(cmd, cwd=ROOT, capture_output=True, text=True)
    return {
        "returncode": proc.returncode,
        "stdout": proc.stdout,
        "stderr": proc.stderr,
        "ran_at": datetime.now().isoformat(timespec="seconds"),
    }


def open_folder(path: Path) -> None:
    path.mkdir(parents=True, exist_ok=True)
    if hasattr(os, "startfile"):
        os.startfile(str(path))
    else:
        raise RuntimeError("Folder opening is only supported on Windows in this UI.")


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


st.set_page_config(page_title="FireModel UI", layout="wide")
st.title("FireModel Pipeline UI")
st.caption("Manage projects, validate inputs, run pipeline, and inspect outputs.")

if "last_run" not in st.session_state:
    st.session_state.last_run = {}
if "config_state" not in st.session_state:
    st.session_state.config_state = load_config()

cfg = st.session_state.config_state
saved_project = str(cfg.get("project_name", "")).strip()
known_projects = list_projects()
default_project = saved_project if saved_project in known_projects else (known_projects[0] if known_projects else "")

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
            st.session_state.last_run = run_pipeline(["step0_snap_to_process"])

    if st.button("Refresh", use_container_width=True):
        trigger_rerun()

current_project = str(st.session_state.config_state.get("project_name", "")).strip()
manifest = load_json(project_manifest_path(current_project)) if current_project else {}
status = load_json(project_status_path(current_project)) if current_project else {}
validation_rows = project_validation_rows(current_project)
validation_df = pd.DataFrame(validation_rows)
step0_rows = step0_validation_rows(current_project)
step0_df = pd.DataFrame(step0_rows)
step0_ready = bool(current_project) and (not step0_df.empty) and bool(step0_df["exists"].all())

with right:
    c1, c2, c3, c4 = st.columns(4)
    c1.metric("Project", current_project or "Unset")
    step0_status = status.get("steps", {}).get("step0_snap_to_process", {}).get("success", "Unknown") if status else "Unknown"
    c2.metric("Step 0 Ready", "Yes" if step0_ready else "No")
    c3.metric("Artifacts", int(flatten_manifest(manifest).shape[0]) if manifest else 0)
    c4.metric("Step 0 Last Run", step0_status)

if st.session_state.last_run:
    with st.expander("Last Pipeline Run Logs", expanded=False):
        st.write(f"Ran at: {st.session_state.last_run.get('ran_at')}")
        st.code(st.session_state.last_run.get("stdout", ""), language="text")
        if st.session_state.last_run.get("stderr"):
            st.code(st.session_state.last_run.get("stderr", ""), language="text")

tabs = st.tabs(["Inputs", "Status", "Artifacts", "Data Preview"])

with tabs[0]:
    st.subheader("Step 0 Validation")
    if not current_project:
        st.info("Select or create a project first.")
    else:
        st.write(f"Project folder: `{project_dir(current_project)}`")
        st.write("Expected Step 0 files for the selected project:")
        if step0_df.empty:
            st.info("No Step 0 validation checks available.")
        else:
            st.dataframe(step0_df, use_container_width=True)
            missing_step0 = step0_df[~step0_df["exists"]]
            if missing_step0.empty:
                st.success("Step 0 is ready to run.")
            else:
                st.error("Step 0 is blocked. Required files are missing or misnamed.")
                st.write("Missing or incorrectly named files:")
                st.dataframe(missing_step0[["label", "expected_name", "relative_path"]], use_container_width=True)

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
        }
        selected = st.selectbox("Choose dataset", list(choices.keys()))
        show_json_file(choices[selected], selected)
