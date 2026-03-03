from __future__ import annotations

import json
import subprocess
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, List

import pandas as pd
import streamlit as st


ROOT = Path(__file__).resolve().parents[1]
MANIFEST_PATH = ROOT / "data" / "outputs" / "manifest" / "pipeline_manifest.json"
STATUS_PATH = ROOT / "data" / "outputs" / "run_status" / "pipeline_status.json"


def load_json(path: Path) -> Dict[str, Any]:
    if not path.exists():
        return {}
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except Exception as exc:
        return {"_error": str(exc)}


def run_pipeline() -> Dict[str, Any]:
    cmd = ["Rscript", "R/run_pipeline.R"]
    proc = subprocess.run(cmd, cwd=ROOT, capture_output=True, text=True)
    return {
        "returncode": proc.returncode,
        "stdout": proc.stdout,
        "stderr": proc.stderr,
        "ran_at": datetime.now().isoformat(timespec="seconds"),
    }


def flatten_manifest(manifest: Dict[str, Any]) -> pd.DataFrame:
    rows: List[Dict[str, Any]] = []
    for section in ("step1", "step2", "step3"):
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
st.caption("Run pipeline, inspect status, and browse outputs from one place.")

if "last_run" not in st.session_state:
    st.session_state.last_run = {}

left, right = st.columns([1, 3])
with left:
    if st.button("Run Pipeline", use_container_width=True):
        with st.spinner("Running R pipeline..."):
            st.session_state.last_run = run_pipeline()
    if st.button("Refresh", use_container_width=True):
        st.rerun()

manifest = load_json(MANIFEST_PATH)
status = load_json(STATUS_PATH)

with right:
    c1, c2, c3 = st.columns(3)
    c1.metric("Project", manifest.get("project", "Unknown"))
    c2.metric("Pipeline Success", status.get("success", "Unknown"))
    c3.metric("Artifacts", int(flatten_manifest(manifest).shape[0]) if manifest else 0)

if st.session_state.last_run:
    with st.expander("Last Pipeline Run Logs", expanded=False):
        st.write(f"Ran at: {st.session_state.last_run.get('ran_at')}")
        st.code(st.session_state.last_run.get("stdout", ""), language="text")
        if st.session_state.last_run.get("stderr"):
            st.code(st.session_state.last_run.get("stderr", ""), language="text")

tabs = st.tabs(["Status", "Artifacts", "Data Preview", "Stage Summaries"])

with tabs[0]:
    st.subheader("Run Status")
    if status:
        st.json(status)
    else:
        st.info("No status file yet. Run the pipeline first.")

with tabs[1]:
    st.subheader("Manifest Artifacts")
    if manifest:
        df = flatten_manifest(manifest)
        if not df.empty:
            st.dataframe(df, use_container_width=True)
        else:
            st.info("Manifest loaded but no artifacts found.")
    else:
        st.info("No manifest file yet. Run the pipeline first.")

with tabs[2]:
    st.subheader("Data Preview")
    if not manifest:
        st.info("No manifest found.")
    else:
        choices = {
            "Step2 Fuel Averages": manifest.get("step2", {}).get("fuel_averages", {}).get("path", ""),
            "Step3 Stand Structure": manifest.get("step3", {}).get("stand_structure", {}).get("path", ""),
            "Step3 Slash Residuals": manifest.get("step3", {}).get("slash_residuals", {}).get("path", ""),
        }
        selected = st.selectbox("Choose dataset", list(choices.keys()))
        show_csv_preview(choices[selected], selected)

with tabs[3]:
    st.subheader("Step 3 Stage Summaries")
    if not manifest:
        st.info("No manifest found.")
    else:
        show_json_file(manifest.get("step3", {}).get("run_summary", {}).get("path", ""), "Step 3 Run Summary")
        show_json_file(manifest.get("step3", {}).get("weather_stage", {}).get("path", ""), "Weather Stage")
        show_json_file(manifest.get("step3", {}).get("fire_behavior_stage", {}).get("path", ""), "Fire Behavior Stage")
        show_json_file(manifest.get("step3", {}).get("fire_behavior_run", {}).get("path", ""), "Fire Behavior Run")

