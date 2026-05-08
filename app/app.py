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

try:
    from st_aggrid import AgGrid, GridOptionsBuilder, GridUpdateMode
    HAS_AGGRID = True
    AGGRID_IMPORT_ERROR = ""
except Exception as exc:
    AgGrid = None
    GridOptionsBuilder = None
    GridUpdateMode = None
    HAS_AGGRID = False
    AGGRID_IMPORT_ERROR = str(exc)


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


def step0_treatments(project_name: str, fallback: List[str] | None = None) -> List[str]:
    fallback = fallback or ["A", "B", "C"]
    if not project_name:
        return fallback
    summary_path = project_data_dir(project_name) / "intermediate" / "step0_snap_to_process" / "snap_to_process_summary.json"
    summary = load_json(summary_path)
    treatments = summary.get("treatments")
    if isinstance(treatments, list):
        parsed = [str(x).strip() for x in treatments if str(x).strip()]
        if parsed:
            return parsed
    return fallback


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


def step05_input_ready(project_name: str, treatment_names: List[str]) -> bool:
    if not project_name:
        return False
    step025_summary = project_data_dir(project_name) / "intermediate" / "step025_cuttingspecs" / "cuttingspecs_summary.json"
    if not step025_summary.exists():
        return False

    stand_stocktables_dir = project_data_dir(project_name) / "raw" / "Stand_StockTables"
    required_files = [
        "OS_SPH.csv",
        "US_SPH.csv",
        "OS_BA.csv",
        "US_Ht_CBH.csv",
        "OS_Vol.csv",
        "OS_Ht_cbh.csv",
    ]
    for treatment_name in treatment_names:
        treatment_dir = stand_stocktables_dir / f"{treatment_name}_tables"
        if not treatment_dir.exists():
            return False
        for file_name in required_files:
            if not (treatment_dir / file_name).exists():
                return False
        if not (treatment_dir / f"cuttingSpecs_{treatment_name}.csv").exists():
            return False
    return True


def step025_input_ready(project_name: str, treatment_names: List[str]) -> bool:
    if not project_name:
        return False
    step0_summary = project_data_dir(project_name) / "intermediate" / "step0_snap_to_process" / "snap_to_process_summary.json"
    if not step0_summary.exists():
        return False

    stand_stocktables_dir = project_data_dir(project_name) / "raw" / "Stand_StockTables"
    required_files = [
        "OS_SPH.csv",
        "US_SPH.csv",
    ]
    for treatment_name in treatment_names:
        treatment_dir = stand_stocktables_dir / f"{treatment_name}_tables"
        if not treatment_dir.exists():
            return False
        for file_name in required_files:
            if not (treatment_dir / file_name).exists():
                return False
    return True


def treatment_description_outputs(project_name: str, treatment_names: List[str]) -> Dict[str, List[tuple[str, str]]]:
    if not project_name:
        return {}
    stand_stocktables_dir = project_data_dir(project_name) / "raw" / "Stand_StockTables"
    output_names = [
        ("SPHTable.png", "Species per Hectare Table"),
        ("SPHPlot.png", "Species per Hectare Plot"),
        ("SPH_Cut_Plot.png", "Species per Hectare Cut vs Leave"),
        ("VOLTable.png", "Volume Table"),
        ("VOL_Cut_Plot.png", "Volume Cut vs Leave"),
        ("BATable.png", "Basal Area Table"),
        ("BA_Cut_Plot.png", "Basal Area Cut vs Leave"),
    ]
    outputs: Dict[str, List[tuple[str, str]]] = {}
    for treatment_name in treatment_names:
        treatment_dir = stand_stocktables_dir / f"{treatment_name}_tables"
        items: List[tuple[str, str]] = []
        for file_name, label in output_names:
            image_path = treatment_dir / file_name
            items.append((str(image_path), label))
        outputs[treatment_name] = items
    return outputs


def treatment_template_csvs(project_name: str, treatment_names: List[str]) -> Dict[str, List[tuple[str, Path]]]:
    if not project_name:
        return {}
    stand_stocktables_dir = project_data_dir(project_name) / "raw" / "Stand_StockTables"
    template_names = [
        ("Overstory Stems Per Hectare", "OS_SPH.csv"),
        ("Understory Stems Per Hectare", "US_SPH.csv"),
        ("Overstory Basal Area", "OS_BA.csv"),
        ("Understory Height and CBH", "US_Ht_CBH.csv"),
        ("Overstory Volume", "OS_Vol.csv"),
        ("Overstory Height and CBH", "OS_Ht_cbh.csv"),
    ]
    outputs: Dict[str, List[tuple[str, Path]]] = {}
    for treatment_name in treatment_names:
        treatment_dir = stand_stocktables_dir / f"{treatment_name}_tables"
        items: List[tuple[str, Path]] = []
        for label, file_name in template_names:
            items.append((label, treatment_dir / file_name))
        outputs[treatment_name] = items
    return outputs


def cutting_specs_csvs(project_name: str, treatment_names: List[str]) -> Dict[str, List[tuple[str, Path]]]:
    if not project_name:
        return {}
    stand_stocktables_dir = project_data_dir(project_name) / "raw" / "Stand_StockTables"
    outputs: Dict[str, List[tuple[str, Path]]] = {}
    for treatment_name in treatment_names:
        treatment_dir = stand_stocktables_dir / f"{treatment_name}_tables"
        outputs[treatment_name] = [("Cutting Specs", treatment_dir / f"cuttingSpecs_{treatment_name}.csv")]
    return outputs


def load_editable_csv(path: Path) -> pd.DataFrame:
    if not path.exists():
        return pd.DataFrame()
    try:
        return pd.read_csv(path)
    except Exception:
        return pd.DataFrame()


def save_editable_csv(path: Path, df: pd.DataFrame) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    df.to_csv(path, index=False)


def editable_template_state_key(csv_path: Path) -> str:
    return f"editable_template_df::{csv_path}"


def get_editable_template_df(csv_path: Path) -> pd.DataFrame:
    state_key = editable_template_state_key(csv_path)
    if state_key not in st.session_state:
        st.session_state[state_key] = load_editable_csv(csv_path)
    return st.session_state[state_key].copy()


def set_editable_template_df(csv_path: Path, df: pd.DataFrame) -> None:
    st.session_state[editable_template_state_key(csv_path)] = df.copy()


def clear_editable_template_state(csv_path: Path) -> None:
    keys_to_clear = [
        editable_template_state_key(csv_path),
    ]
    stem = csv_path.stem
    path_str = str(csv_path)
    for key in list(st.session_state.keys()):
        if path_str in key or stem in key:
            if (
                key.startswith("template_editor_")
                or key.startswith("cutting_specs_editor_")
                or key.startswith("editable_template_df::")
            ):
                keys_to_clear.append(key)
    for key in set(keys_to_clear):
        st.session_state.pop(key, None)


def compute_cutting_specs_summary(df: pd.DataFrame) -> pd.DataFrame:
    if df.empty:
        return pd.DataFrame(columns=["Stand.Layer", "DBH.Class", "Overall Cutting Specs", "Cut", "Leave"])

    required_cols = {"Stand.Layer", "DBH.Class"}
    if not required_cols.issubset(df.columns):
        return pd.DataFrame(columns=["Stand.Layer", "DBH.Class", "Overall Cutting Specs", "Cut", "Leave"])

    species_cols = [
        col for col in df.columns
        if col not in ("Stand.Layer", "DBH.Class") and not str(col).endswith(".%")
    ]
    if not species_cols:
        summary = df[["Stand.Layer", "DBH.Class"]].copy()
        summary["Overall Cutting Specs"] = 0.0
        summary["Cut"] = 0.0
        summary["Leave"] = 0.0
        return summary

    working = df.copy()
    total_series = pd.Series(0.0, index=working.index, dtype="float64")
    cut_series = pd.Series(0.0, index=working.index, dtype="float64")

    for species in species_cols:
        value_series = pd.to_numeric(working.get(species, 0), errors="coerce").fillna(0.0)
        percent_series = pd.to_numeric(working.get(f"{species}.%", 0), errors="coerce").fillna(0.0)
        total_series = total_series + value_series
        cut_series = cut_series + (value_series * percent_series / 100.0)

    leave_series = total_series - cut_series
    overall_series = pd.Series(0.0, index=working.index, dtype="float64")
    nonzero_mask = total_series != 0
    overall_series.loc[nonzero_mask] = (cut_series.loc[nonzero_mask] / total_series.loc[nonzero_mask]) * 100.0

    summary = working[["Stand.Layer", "DBH.Class"]].copy()
    summary["Overall Cutting Specs"] = overall_series.round(2)
    summary["Cut"] = cut_series.round(2)
    summary["Leave"] = leave_series.round(2)

    total_cut = float(cut_series.sum())
    total_leave = float(leave_series.sum())
    total_overall = (total_cut / (total_cut + total_leave) * 100.0) if (total_cut + total_leave) else 0.0
    totals_row = pd.DataFrame([
        {
            "Stand.Layer": "Totals",
            "DBH.Class": "Totals",
            "Overall Cutting Specs": round(total_overall, 2),
            "Cut": round(total_cut, 2),
            "Leave": round(total_leave, 2),
        }
    ])
    return pd.concat([summary, totals_row], ignore_index=True)


def render_editable_template_table(df: pd.DataFrame, editor_key: str) -> pd.DataFrame:
    if HAS_AGGRID:
        gb = GridOptionsBuilder.from_dataframe(df)
        gb.configure_default_column(editable=True, resizable=True, sortable=False, filter=False)
        gb.configure_grid_options(
            rowSelection="single",
            suppressRowClickSelection=False,
            domLayout="normal",
        )
        gb.configure_selection(selection_mode="single", use_checkbox=True)
        grid_response = AgGrid(
            df,
            key=f"{editor_key}_aggrid",
            gridOptions=gb.build(),
            update_mode=GridUpdateMode.VALUE_CHANGED | GridUpdateMode.SELECTION_CHANGED,
            fit_columns_on_grid_load=False,
            allow_unsafe_jscode=False,
            height=min(max(220, 42 * (len(df) + 2)), 520),
            theme="streamlit",
            reload_data=False,
        )
        updated_df = pd.DataFrame(grid_response.get("data", df))
        st.session_state[f"{editor_key}_selected_rows"] = grid_response.get("selected_rows", [])
        return updated_df

    return st.data_editor(
        df,
        key=editor_key,
        use_container_width=False,
        num_rows="fixed",
    )


def render_csv_editor_card(label: str, csv_path: Path, editor_key: str) -> None:
    start_panel("fm-template-card")
    st.markdown(f"**{label}**")
    df = get_editable_template_df(csv_path)
    is_cutting_specs = csv_path.stem.lower().startswith("cuttingspecs_")
    if df.empty and not csv_path.exists():
        st.warning(f"Missing template file: {csv_path.name}")
        end_panel()
        return

    if not is_cutting_specs:
        add_col_name_key = f"{editor_key}_new_col_name"
        add_row_name_key = f"{editor_key}_new_row_name"
        drop_col_key = f"{editor_key}_drop_col_select"
        first_col = df.columns[0] if len(df.columns) else "Name"

        st.caption("Table tools")
        add_col_input_col, add_col_button_col = st.columns([2.2, 0.6])
        with add_col_input_col:
            st.text_input(
                "Column name",
                key=add_col_name_key,
                placeholder="Column name",
                label_visibility="collapsed",
            )
        with add_col_button_col:
            if st.button("Add column", key=f"{editor_key}_add_col", use_container_width=False):
                new_col = st.session_state.get(add_col_name_key, "").strip()
                if not new_col:
                    st.warning("Enter a column name before adding it.")
                elif new_col in df.columns:
                    st.warning(f"`{new_col}` already exists in {csv_path.name}.")
                else:
                    df[new_col] = ""
                    set_editable_template_df(csv_path, df)
                    trigger_rerun()

        add_row_input_col, add_row_button_col = st.columns([2.2, 0.6])
        with add_row_input_col:
            st.text_input(
                f"{first_col} value",
                key=add_row_name_key,
                placeholder=f"{first_col} value",
                label_visibility="collapsed",
            )
        with add_row_button_col:
            if st.button("Add row", key=f"{editor_key}_add_row", use_container_width=False):
                row_name = st.session_state.get(add_row_name_key, "").strip()
                blank_row = {col: "" for col in df.columns}
                if first_col in blank_row:
                    blank_row[first_col] = row_name
                df = pd.concat([df, pd.DataFrame([blank_row])], ignore_index=True)
                set_editable_template_df(csv_path, df)
                trigger_rerun()

        removable_columns = [c for c in df.columns if c not in ("DBH.Class", "Stand.Layer")]
        row_options = []
        if not df.empty:
            for idx in range(len(df)):
                raw_label = df.iloc[idx][first_col] if first_col in df.columns else ""
                label_value = str(raw_label).strip() if pd.notna(raw_label) else ""
                row_options.append((f"{idx}: {label_value}" if label_value else f"{idx}: Row {idx + 1}", idx))
        remove_col_input_col, remove_col_button_col, remove_row_input_col, remove_row_button_col = st.columns([1.25, 0.75, 1.25, 0.9])
        with remove_col_input_col:
            st.selectbox(
                "Remove column",
                options=[""] + removable_columns,
                key=drop_col_key,
                label_visibility="collapsed",
            )
        with remove_col_button_col:
            if st.button("Remove column", key=f"{editor_key}_drop_col", use_container_width=False):
                drop_col = st.session_state.get(drop_col_key, "").strip()
                if not drop_col:
                    st.warning("Choose a column to remove.")
                elif drop_col not in df.columns:
                    st.warning(f"`{drop_col}` is not present in {csv_path.name}.")
                else:
                    df = df.drop(columns=[drop_col])
                    set_editable_template_df(csv_path, df)
                    trigger_rerun()
        with remove_row_input_col:
            if HAS_AGGRID:
                st.caption("Select a grid row below, then click remove.")
            else:
                st.selectbox(
                    "Remove row",
                    options=[""] + [label_text for label_text, _ in row_options],
                    key=f"{editor_key}_drop_row_select",
                    label_visibility="collapsed",
                )
        with remove_row_button_col:
            if HAS_AGGRID:
                if st.button("Remove row", key=f"{editor_key}_drop_selected_row", use_container_width=False):
                    selected_rows = st.session_state.get(f"{editor_key}_selected_rows", []) or []
                    if not selected_rows:
                        st.warning("Select a row in the grid first.")
                    else:
                        selected_df = pd.DataFrame(selected_rows)
                        match_mask = pd.Series([True] * len(df))
                        target = selected_df.iloc[0].to_dict()
                        for col, val in target.items():
                            if col in df.columns:
                                if pd.isna(val):
                                    match_mask &= df[col].isna()
                                else:
                                    match_mask &= df[col].astype(str) == str(val)
                        matches = df.index[match_mask]
                        if len(matches) == 0:
                            st.warning("Could not match the selected row in the table.")
                        else:
                            df = df.drop(matches[0]).reset_index(drop=True)
                            set_editable_template_df(csv_path, df)
                            trigger_rerun()
            else:
                if st.button("Remove row", key=f"{editor_key}_drop_row_manual", use_container_width=False):
                    selected_label = st.session_state.get(f"{editor_key}_drop_row_select", "")
                    match = next((idx for label_text, idx in row_options if label_text == selected_label), None)
                    if match is None:
                        st.warning("Choose a row to remove.")
                    else:
                        df = df.drop(df.index[match]).reset_index(drop=True)
                        set_editable_template_df(csv_path, df)
                        trigger_rerun()

    if is_cutting_specs:
        st.markdown("<div class='fm-template-wrap'>", unsafe_allow_html=True)
        edited_df = render_editable_template_table(df, editor_key)
        st.markdown("</div>", unsafe_allow_html=True)
        st.markdown("**Overall Cutting Specs**")
        st.caption("Live summary from the current cutting specs inputs. This updates before save.")
        st.dataframe(
            compute_cutting_specs_summary(edited_df),
            use_container_width=True,
            height=min(max(220, 42 * (len(edited_df) + 3)), 520),
        )
    else:
        st.markdown("<div class='fm-template-wrap'>", unsafe_allow_html=True)
        edited_df = render_editable_template_table(df, editor_key)
        st.markdown("</div>", unsafe_allow_html=True)
    set_editable_template_df(csv_path, edited_df)
    save_table_col1, save_table_col2 = st.columns([0.9, 2.1])
    with save_table_col1:
        if st.button("Save CSV", key=f"{editor_key}_save_csv", use_container_width=False):
            if edited_df is not None and not edited_df.empty:
                save_editable_csv(csv_path, edited_df)
                st.success(f"Saved {csv_path.name}.")
            else:
                st.info(f"No editable data available to save for {csv_path.name}.")
    with save_table_col2:
        st.caption("Save any changes here for them to update the backend CSV used by later steps.")
    end_panel()


def step3_input_ready(project_name: str, weather_name: str, treatment_names: List[str]) -> bool:
    if not project_name:
        return False

    project_base = project_data_dir(project_name)
    weather_list = project_base / "raw" / "Weather" / "WeatherLists" / "allstations_90th_FWList_dates_summer.csv"
    hourly_weather = project_base / "raw" / "Weather" / "raw" / f"{weather_name}_Hourly_Weather.csv"
    if not weather_list.exists() or not hourly_weather.exists():
        return False

    fuelcalc_outputs_root = project_base / "raw" / "FuelCalc" / "Outputs"
    if not fuelcalc_outputs_root.exists():
        return False

    for treatment_name in treatment_names:
        treatment_file = (
            fuelcalc_outputs_root
            / f"TU_{treatment_name}"
            / f"TU_{treatment_name}_FuelCalc_FFI_Outputs.csv"
        )
        if not treatment_file.exists():
            return False

    return True


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


def show_image_file(path_str: str, title: str) -> bool:
    if not path_str:
        return False
    path = Path(path_str)
    if not path.exists():
        return False
    st.write(f"**{title}**")
    try:
        with Image.open(path) as img:
            st.image(img.copy(), use_column_width=True)
        return True
    except Exception as exc:
        st.error(f"Failed to display image {path.name}: {exc}")
        return False


def show_image_grid(items: List[tuple[str, str]], columns: int = 3) -> int:
    valid_items = [(path_str, title) for path_str, title in items if path_str]
    if not valid_items:
        return 0
    existing_items = [(path_str, title) for path_str, title in valid_items if Path(path_str).exists()]
    if not existing_items:
        return 0
    displayed_count = 0
    for start in range(0, len(existing_items), columns):
        row_items = existing_items[start:start + columns]
        cols = st.columns(columns)
        for idx, (path_str, title) in enumerate(row_items):
            with cols[idx]:
                if show_image_file(path_str, title):
                    displayed_count += 1
    return displayed_count


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

def step3_run_completed_in_session() -> bool:
    return (
        st.session_state.get("last_step_action") == "step3_fire_model"
        and st.session_state.get("last_run", {}).get("returncode") == 0
    )


def render_step3_runtime(project_name: str, title: str) -> None:
    placeholder = st.empty()
    completion_announced = False

    while True:
        step3_async_state = sync_async_pipeline_run("step3_fire_model")
        with placeholder.container():
            show_step_result("step3_fire_model", title)
            show_step3_progress(project_name)
            if step3_async_state and step3_async_state.get("status") == "running":
                st.caption("Updating Step 3 progress automatically.")
            elif step3_run_completed_in_session():
                st.success("Fire modeling results completed.")
                completion_announced = True

        if not step3_async_state or step3_async_state.get("status") != "running":
            break
        time.sleep(2)

    if completion_announced:
        st.session_state["step3_completion_announced"] = True


def style_step0_validation(df: pd.DataFrame) -> Any:
    def highlight_missing(row: pd.Series) -> List[str]:
        color = "#f8d7da" if not bool(row.get("exists")) else ""
        return [f"background-color: {color}"] * len(row)

    return df.style.apply(highlight_missing, axis=1)


def status_icon(status: str) -> str:
    return "✅" if status == "Ready" else ("❌" if status == "Missing" else "⚠️")


def start_panel(css_class: str = "fm-card") -> None:
    st.markdown(f'<div class="{css_class}">', unsafe_allow_html=True)


def end_panel() -> None:
    st.markdown("</div>", unsafe_allow_html=True)


st.set_page_config(page_title="FireModel UI", layout="wide")
st.title("FireModel Pipeline UI")
st.caption("Manage projects, validate inputs, run pipeline, and inspect outputs.")
st.markdown(
    """
    <style>
    .fm-card {
        border: 1px solid rgba(120, 132, 158, 0.35);
        border-radius: 16px;
        padding: 1rem 1rem 1.15rem 1rem;
        margin: 0 0 1rem 0;
        background: rgba(19, 23, 34, 0.38);
        box-shadow: 0 0 0 1px rgba(255,255,255,0.02) inset;
    }
    .fm-sidecard {
        border: 1px solid rgba(120, 132, 158, 0.45);
        border-radius: 18px;
        padding: 0.9rem 0.9rem 1rem 0.9rem;
        margin-top: 0.25rem;
        background: rgba(22, 27, 40, 0.62);
        box-shadow: 0 12px 28px rgba(0,0,0,0.18);
    }
    .fm-mini {
        border: 1px solid rgba(120, 132, 158, 0.35);
        border-radius: 14px;
        padding: 0.55rem 0.7rem;
        margin-bottom: 0.55rem;
        background: rgba(19, 23, 34, 0.32);
    }
    .fm-template-card {
        border: 1px solid rgba(76, 175, 80, 0.45);
        border-radius: 14px;
        padding: 0.8rem 0.9rem 0.95rem 0.9rem;
        margin: 0.2rem 0 0.9rem 0;
        background: rgba(24, 48, 28, 0.22);
        box-shadow: 0 0 0 1px rgba(124, 179, 66, 0.08) inset;
    }
    .fm-template-wrap {
        overflow-x: auto;
        padding-bottom: 0.35rem;
        margin-bottom: 0.35rem;
    }
    .fm-template-note {
        font-size: 0.85rem;
        color: rgba(220, 226, 237, 0.72);
        margin: 0.2rem 0 0.45rem 0;
    }
    </style>
    """,
    unsafe_allow_html=True,
)

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

top_left, top_spacer = st.columns([1.35, 3.65])
with top_left:
    with st.expander("Environment Check", expanded=False):
        for row in env_rows:
            st.markdown(f"{status_icon(row['status'])} **{row['component']}**")
            st.caption(row["details"])
        missing_env = [row for row in env_rows if row["status"] == "Missing"]
        if missing_env:
            st.warning("Some setup items are missing. Run `setup_windows.bat` or `scripts/bootstrap_windows.ps1` before using the pipeline on a new machine.")
        else:
            st.success("Core environment checks look good.")

project_left, project_right = st.columns([1.25, 3.75])
with project_left:
    st.subheader("Project")
    current_project = default_project or saved_project
    step0_rows = step0_validation_rows(current_project)
    step0_df = pd.DataFrame(step0_rows)
    step0_ready = bool(current_project) and (not step0_df.empty) and bool(step0_df["exists"].all())
    with st.container():
        st.caption("Project Name")
        st.write(current_project or "Unset")
    with st.container():
        st.caption("Project Setup Ready")
        st.write("✅ Ready" if step0_ready else "❌ Missing inputs")

    if known_projects:
        select_col, use_col = st.columns([4, 1.5])
        with select_col:
            selected_project = st.selectbox(
                "Select Project",
                options=known_projects,
                index=known_projects.index(default_project) if default_project in known_projects else 0,
                help="Choose an existing project folder under projects/.",
                label_visibility="collapsed",
            )
        with use_col:
            use_selected_project = st.button("Use", use_container_width=True)
    else:
        selected_project = ""
        use_selected_project = False
        st.info("No project folders found under projects/.")

    current_project = selected_project or saved_project
    step0_rows = step0_validation_rows(current_project)
    step0_df = pd.DataFrame(step0_rows)
    step0_ready = bool(current_project) and (not step0_df.empty) and bool(step0_df["exists"].all())

    if use_selected_project:
        if not current_project:
            st.error("Select a project first.")
        else:
            ensure_project_dirs(current_project)
            cfg["project_name"] = current_project
            save_config(cfg)
            st.session_state.config_state = cfg
            st.success(f"Using project {current_project}")
            trigger_rerun()

    step0_help = "Project Setup requires exactly these files in raw/SNAP: <project>_OS.csv, <project>_US.csv, <project>_EXTRA.csv, <project>_FUELS.csv"
    run_col, refresh_col = st.columns([5, 1])
    with run_col:
        run_project_setup = st.button("Run Project Setup", use_container_width=True, disabled=not step0_ready, help=step0_help)
    with refresh_col:
        refresh_page = st.button("↻", help="Refresh page")

    if run_project_setup:
        ensure_project_dirs(current_project)
        cfg["project_name"] = current_project
        save_config(cfg)
        st.session_state.config_state = cfg
        with st.spinner("Running Project Setup..."):
            st.session_state.last_step_action = "step0_snap_to_process"
            st.session_state.last_run = run_pipeline(["step0_snap_to_process"])

    if refresh_page:
        trigger_rerun()

current_project = str(st.session_state.config_state.get("project_name", "")).strip()
manifest = load_json(project_manifest_path(current_project)) if current_project else {}
status = load_json(project_status_path(current_project)) if current_project else {}
step3_async_state = sync_async_pipeline_run("step3_fire_model")
step3_running = bool(step3_async_state and step3_async_state.get("status") == "running")
validation_rows = project_validation_rows(current_project)
validation_df = pd.DataFrame(validation_rows)
step0_rows = step0_validation_rows(current_project)
step0_df = pd.DataFrame(step0_rows)
step0_ready = bool(current_project) and (not step0_df.empty) and bool(step0_df["exists"].all())
batch_files = fuelcalc_batch_files(current_project)
error_reports = fuelcalc_error_reports(current_project)
step0_status = status.get("steps", {}).get("step0_snap_to_process", {}).get("success", "Unknown") if status else "Unknown"
step1_ready = step1_input_ready(current_project, step0_status)


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

main_col, diag_col = st.columns([3.4, 1.2])

with main_col:
    process_cfg = cfg.get("process_to_fuelcalc", {})
    st.subheader("Project Setup")
    if not current_project:
        st.info("Select or create a project first.")
    else:
        st.write(f"Project folder: `{project_dir(current_project)}`")
        st.write("Required SNAP files for the selected project:")
        if step0_df.empty:
            st.info("No project setup validation checks available.")
        else:
            st.dataframe(style_step0_validation(step0_df), use_container_width=True)
            missing_step0 = step0_df[~step0_df["exists"]]
            if missing_step0.empty:
                st.success("Project Setup is ready to run.")
            else:
                st.error("Project Setup is blocked. Required files are missing or misnamed.")
                st.write("Missing or incorrectly named files:")
                st.dataframe(missing_step0[["label", "expected_name", "relative_path"]], use_container_width=True)
        show_step_result("step0_snap_to_process", "Project Setup")

    step05_treatment_names = step0_treatments(current_project, process_cfg.get("tr_names", ["A", "B", "C"]))
    template_csvs = treatment_template_csvs(current_project, step05_treatment_names)
    if step05_treatment_names:
        st.markdown("**Editable Treatment Templates**")
        st.caption("Edit the generated CSV templates here, then save them before running Treatment Description or Step 1.")
        for treatment_name in step05_treatment_names:
            with st.expander(f"Treatment {treatment_name} Templates", expanded=(treatment_name == step05_treatment_names[0])):
                files_for_treatment = template_csvs.get(treatment_name, [])
                for label, csv_path in files_for_treatment:
                    render_csv_editor_card(label, csv_path, f"template_editor_{treatment_name}_{csv_path.stem}")

    st.subheader("Cutting Specs")
    st.caption("Generate and edit cutting specs separately from the stand tables. These richer tables will be trimmed automatically before backend use.")
    step025_ready = step025_input_ready(current_project, step05_treatment_names)
    step025_help = (
        "Run Project Setup first so the treatment folders and template CSVs exist for each treatment."
        if not step025_ready
        else "Generate expanded cutting specs tables from the current treatment templates."
    )
    cutting_spec_files = cutting_specs_csvs(current_project, step05_treatment_names)
    step025_run_col1, step025_run_col2 = st.columns([1, 1])
    with step025_run_col1:
        st.caption("Run this after saving any template-table edits so cutting specs reflect the latest backend CSVs.")
    with step025_run_col2:
        if st.button("Generate Cutting Specs", use_container_width=True, disabled=not step025_ready, help=step025_help):
            st.session_state.last_step_action = "step025_cuttingspecs"
            st.session_state.last_run = run_pipeline(["step025_cuttingspecs"])
            refreshed_cutting_specs = cutting_specs_csvs(current_project, step05_treatment_names)
            for files_for_treatment in refreshed_cutting_specs.values():
                for _, csv_path in files_for_treatment:
                    clear_editable_template_state(csv_path)
            trigger_rerun()
    show_step_result("step025_cuttingspecs", "Cutting Specs")

    if step05_treatment_names:
        for treatment_name in step05_treatment_names:
            with st.expander(f"Treatment {treatment_name} Cutting Specs", expanded=False):
                files_for_treatment = cutting_spec_files.get(treatment_name, [])
                for label, csv_path in files_for_treatment:
                    render_csv_editor_card(label, csv_path, f"cutting_specs_editor_{treatment_name}_{csv_path.stem}")

    st.subheader("Treatment Description")
    st.caption("Generate treatment-specific stand tables and summary plots from the saved templates and cutting specs.")
    step05_ready = step05_input_ready(current_project, step05_treatment_names)
    step05_help = (
        "Run Cutting Specs first so treatment description has the required cutting specs tables."
        if not step05_ready
        else "Generate treatment description tables and plots."
    )
    step05_save_col, step05_run_col = st.columns([1, 1])
    with step05_save_col:
        st.caption("This step uses the treatment folders and editable CSV templates created during Project Setup.")
    with step05_run_col:
        if st.button("Run Treatment Description", use_container_width=True, disabled=not step05_ready, help=step05_help):
            st.session_state.last_step_action = "step05_treatment_description"
            st.session_state.last_run = run_pipeline(["step05_treatment_description"])
            trigger_rerun()
    show_step_result("step05_treatment_description", "Treatment Description")

    step05_outputs = treatment_description_outputs(current_project, step05_treatment_names)
    any_step05_images = any(Path(path_str).exists() for items in step05_outputs.values() for path_str, _ in items if path_str)
    if any_step05_images:
        st.markdown("**Treatment Description Outputs**")
        for treatment_name in step05_treatment_names:
            items = step05_outputs.get(treatment_name, [])
            with st.expander(f"Treatment {treatment_name} Description", expanded=(treatment_name == step05_treatment_names[0])):
                displayed_count = show_image_grid(items, columns=2)
                if displayed_count == 0:
                    st.info(f"No treatment description images are available yet for treatment {treatment_name}.")
    else:
        st.info("No treatment description outputs are available yet. Run Treatment Description after Project Setup.")

    st.subheader("FuelCalc Setup and Processing")
    st.caption("Prepare treatment settings, generate FuelCalc inputs, then run FuelCalc.")
    stand_stocktables_dir = project_data_dir(current_project) / "raw" / "Stand_StockTables"
    st.markdown("**Stand_StockTables Review and Edit**")
    st.write("The same treatment template files are available above in the app. This folder view is optional if you want to inspect the CSVs directly:")
    st.code(str(stand_stocktables_dir), language="text")
    if st.button("Open Stand_StockTables Folder", use_container_width=False):
        try:
            open_folder(stand_stocktables_dir)
            st.success("Opened Stand_StockTables folder in Explorer.")
        except Exception as exc:
            st.error(f"Failed to open folder: {exc}")

    treatment_names_default = process_cfg.get("tr_names", ["A", "B", "C"])
    treatment_names_text = st.text_input(
        "List treatment names with comma separation",
        value=", ".join(treatment_names_default),
        help="Comma-separated treatment names. All per-treatment settings below follow this order.",
    )
    treatment_names = parse_treatment_names(treatment_names_text) or ["A", "B", "C"]
    n_treatments = len(treatment_names)

    cutting_value = st.checkbox("Are you cutting?", value=bool(process_cfg.get("cutting", True)))
    moist_value = st.selectbox(
        "What 10 hour fuel moistures are you planning for?",
        options=["VeryDry", "Dry", "Moderate", "Wet"],
        index=["VeryDry", "Dry", "Moderate", "Wet"].index(str(process_cfg.get("moist", "VeryDry"))),
        help=(
            "VeryDry = 6% extreme drought and high ignition likelihood\n"
            "Dry = 10% dry, typical of peak summer fuel dryness in closed stands\n"
            "Moderate = 16%, moderate fuel moisture, typical summer coastal fuel moisture levels\n"
            "Wet = 22%, wet fuels, recent rain or very closed stands, ignitions unlikely"
        ),
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
            toggle_cols = st.columns(3)
            with toggle_cols[0]:
                ui_thin_values.append(
                    st.checkbox(
                        f"Thinning - {treatment_name}",
                        value=bool(thin_values[i]),
                        key=f"thin_{i}",
                    )
                )
            with toggle_cols[1]:
                ui_prune_flag_values.append(
                    st.checkbox(
                        f"Pruning - {treatment_name}",
                        value=bool(prune_flag_values[i]),
                        key=f"prune_flag_{i}",
                    )
                )
            with toggle_cols[2]:
                ui_burning_values.append(
                    st.checkbox(
                        f"Burning - {treatment_name}",
                        value=bool(burning_values[i]),
                        key=f"burning_{i}",
                    )
                )

            col1, col2 = st.columns(2)
            with col1:
                ui_thinning_values.append(
                    int(
                        st.number_input(
                            f"What density do you want to thin to? (SPH) - {treatment_name}",
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
                            f"What pruning height do you want? (m) - {treatment_name}",
                            min_value=0.0,
                            value=float(prune_values[i]),
                            step=0.5,
                            key=f"prune_{i}",
                        )
                    )
                )
                ui_bole_char_values.append(
                    float(
                        st.number_input(
                            f"What bole char height can you accept? (m) - {treatment_name}",
                            min_value=0.0,
                            value=float(bole_char_values[i]),
                            step=0.5,
                            key=f"bole_char_{i}",
                            help="Set bole char height to 1 m below CBH if unknown.",
                        )
                    )
                )
            with col2:
                ui_emission_values.append(
                    int(
                        st.selectbox(
                            f"What emission factor are you using? - {treatment_name}",
                            options=[2, 3, 4, 5, 6],
                            format_func=lambda x: {
                                2: "2 = BorealForest",
                                3: "3 = WesternForestRX",
                                4: "4 = WesternForestWF",
                                5: "5 = Shrubland",
                                6: "6 = Grassland",
                            }[x],
                            index=[2, 3, 4, 5, 6].index(int(emission_values[i])) if int(emission_values[i]) in [2, 3, 4, 5, 6] else 2,
                            key=f"emission_{i}",
                            help="When planning for fire risk use 4. When planning for a burn use 3. When working in Boreal use 2. Grass and Shrub where appropriate.",
                        )
                    )
                )
                ui_surf_fuel_values.append(
                    st.selectbox(
                        f"What surface fuel drives fire behaviour? - {treatment_name}",
                        options=["Grass", "Chaparral", "TimberLitter", "Slash"],
                        index=["Grass", "Chaparral", "TimberLitter", "Slash"].index(str(surf_fuel_values[i])) if str(surf_fuel_values[i]) in ["Grass", "Chaparral", "TimberLitter", "Slash"] else 2,
                        key=f"surf_fuel_{i}",
                        help="Grass = grass dominated fuels, Chaparral = chaparral, scrubfield, and shrub fuels, TimberLitter = timber fuels with downed woody debris and litter (recommended), and Slash = logging slash fuels.",
                    )
                )
                ui_climate_values.append(
                    st.selectbox(
                        f"What climate type are you in? - {treatment_name}",
                        options=["Humid", "Arid"],
                        index=["Humid", "Arid"].index(str(climate_values[i])) if str(climate_values[i]) in ["Humid", "Arid"] else 1,
                        key=f"climate_{i}",
                        help="For areas outside of Caribou, Kamloops, and Southeast use Humid. For microclimate conditions of your site consider the moisture regime and weather fires are likely under more humid conditions or if fuel dryness is a necessity for fire behaviour.",
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
        st.success("Saved FuelCalc setup settings.")

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
        with st.spinner("Running FuelCalc setup and processing..."):
            st.session_state.last_step_action = "step1_process_to_fuelcalc"
            st.session_state.last_run = run_pipeline(["step1_process_to_fuelcalc"])
        trigger_rerun()
    show_step_result("step1_process_to_fuelcalc", "FuelCalc Setup and Processing")

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

    st.subheader("Local Weather Analysis")
    st.caption("Prepare local weather products and weather-based plots for the project.")
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
        step2_weather_code = int(
            st.number_input(
                "Weather station code",
                min_value=0,
                value=int(step2_cfg.get("weather_code", 1399)),
                step=1,
                key="step2_weather_code",
                help="Station code can be found next to the station point on PCIC as Native ID.",
            )
        )
    with step2_col2:
        step2_weather_lat = float(st.number_input("Weather latitude", value=float(step2_cfg.get("weather_lat", 50.121389)), format="%.6f", key="step2_weather_lat"))
        step2_weather_long = float(st.number_input("Weather longitude", value=float(step2_cfg.get("weather_long", -120.744167)), format="%.6f", key="step2_weather_long"))
        step2_danger_region = int(
            st.selectbox(
                "Danger region",
                options=[1, 2, 3],
                index=[1, 2, 3].index(int(step2_cfg.get("danger_region", 3))),
                key="step2_danger_region",
                help="Danger Region for your AOI can be found in the Wildfire Regulation (2005), Division 2, Section 6 image: https://www.bclaws.gov.bc.ca/civix/document/id/complete/statreg/11_38_2005",
            )
        )

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
        st.success("Saved local weather settings.")

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
        with st.spinner("Running local weather analysis..."):
            st.session_state.last_step_action = "step2_fuelcalc"
            st.session_state.last_run = run_pipeline(["step2_fuelcalc"])
        trigger_rerun()
    show_step_result("step2_fuelcalc", "Local Weather Analysis")

    if manifest:
        st.markdown("**Local Weather Analysis Plots**")
        displayed_step2_plots = show_image_grid(
            [
                (manifest.get("step2", {}).get("wind_rose", {}).get("path", ""), "Wind Rose"),
                (manifest.get("step2", {}).get("danger_days", {}).get("path", ""), "Danger Days"),
                (manifest.get("step2", {}).get("weather_conditions", {}).get("path", ""), "Weather Conditions"),
            ],
            columns=3,
        )
        if displayed_step2_plots == 0:
            st.warning("No local weather analysis plots are available yet.")
    
    st.subheader("Fire Modeling Setup")
    st.caption("Configure treatment-specific fire modeling inputs and run final fire behavior results.")
    step3_cfg = cfg.get("firemodel_results", {})
    treatment_names_step3 = process_cfg.get("tr_names", ["A", "B", "C"])
    n_step3_treatments = len(treatment_names_step3)

    step3_col1, step3_col2 = st.columns(2)
    with step3_col1:
        st.markdown("**General Environment**")
        step3_elevation = float(st.number_input("Elevation of your site in meters.", value=float(step3_cfg.get("elevation", 483)), step=1.0, key="step3_elevation"))
        step3_intensity_flag = st.selectbox(
            "What type of intensity calculation do you want?",
            options=["Byram", "Nelson", "Rothermel"],
            index=["Byram", "Nelson", "Rothermel"].index(str(step3_cfg.get("intensity_flag", "Byram"))) if str(step3_cfg.get("intensity_flag", "Byram")) in ["Byram", "Nelson", "Rothermel"] else 0,
            key="step3_intensity_flag",
            help="Byram (recommended): ROS x Surface Fuel Consumed x 300, Rothermel: fireline intensity calculation, Nelson: fire intensity calculation modified by residence time.",
        )
        step3_fuel_moisture_type = st.selectbox(
            "What model do you want to use for fine fuel moisture?",
            options=["Model", "Wotton"],
            index=["Model", "Wotton"].index(str(step3_cfg.get("fuel_moisture_type", "Model"))) if str(step3_cfg.get("fuel_moisture_type", "Model")) in ["Model", "Wotton"] else 0,
            key="step3_fuel_moisture_type",
            help="Model (recommended): models fine fuel moisture based on site, aspect, slope, temp, and rh. Wotton: models fine fuel moisture based on a modified FFMC calculation.",
        )
        step3_heat_flag = st.selectbox(
            "How do you want to calculate low heat of combustion?",
            options=["Manual", "Nelson"],
            index=["Manual", "Nelson"].index(str(step3_cfg.get("heat_flag", "Manual"))) if str(step3_cfg.get("heat_flag", "Manual")) in ["Manual", "Nelson"] else 0,
            key="step3_heat_flag",
            help="Nelson: calculates a weighted average heat of combustion based on fuel load distribution. Manual (recommended): uses the standard 18,600 kj.",
        )
        step3_wind_gust_mod = st.checkbox(
            "Modify for wind gusts",
            value=bool(step3_cfg.get("wind_gust_mod", False)),
            key="step3_wind_gust_mod",
            help="If clicked the models will automatically increase wind speed to account for wind gusting. Use if weather data predicts wind speeds far too low for expected extreme behaviour.",
        )
        step3_grass_curing = float(
            st.number_input(
                "What is your cured grass percentage?",
                value=float(step3_cfg.get("grass_curing", 75)),
                step=1.0,
                key="step3_grass_curing",
                help="0-10: end winter/spring fully live\n10-30: late spring, early summer\n30-70: mid summer\n70+: late summer, drought conditions, or fall",
            )
        )
        step3_num_weathers = int(st.number_input("Number of weather days", min_value=1, value=int(step3_cfg.get("num_weathers", 250)), step=1, key="step3_num_weathers"))
        step3_weather_name = st.text_input("Weather name for Step 3", value=str(step3_cfg.get("weather_name", step2_weather_name)), key="step3_weather_name")
        step3_season = st.selectbox("Season", options=["Spring", "Summer", "Winter"], index=["Spring", "Summer", "Winter"].index(str(step3_cfg.get("season", "Summer"))) if str(step3_cfg.get("season", "Summer")) in ["Spring", "Summer", "Winter"] else 1, key="step3_season")
        step3_advanced_models = st.checkbox(
            "Plot advanced models",
            value=bool(step3_cfg.get("advanced_models", True)),
            key="step3_advanced_models",
            help="Checked box = generate advanced fire modeling plots in addition to the basic results.",
        )
    with step3_col2:
        st.markdown("**Treatment Specific Setup**")

    step3_custom_fuels_vals = ensure_list_length(step3_cfg.get("custom_fuels", [True, True, True]), n_step3_treatments, True)
    step3_prune_vals = ensure_list_length(step3_cfg.get("prune_vector", [2, 2, 2]), n_step3_treatments, 2)
    step3_fuels_vals = ensure_list_length(step3_cfg.get("fuels", [0.75, 0.75, 0.75]), n_step3_treatments, 0.75)
    step3_hr1000_vals = ensure_list_length(step3_cfg.get("hr1000s", [1, 1, 1]), n_step3_treatments, 1)
    step3_ftcad_vals = ensure_list_length(step3_cfg.get("ftcad_vector", ["C-7", "C-7", "C-7"]), n_step3_treatments, "C-7")
    step3_fsg_mod_vals = ensure_list_length(step3_cfg.get("fsg_mod_flag", [False, False, False]), n_step3_treatments, False)
    step3_fsg_field_vals = ensure_list_length(step3_cfg.get("fsg_field_flag", [True, True, True]), n_step3_treatments, True)
    step3_crown_type_vals = ensure_list_length(step3_cfg.get("crown_fire_type", ["Wagner", "Wagner", "Wagner"]), n_step3_treatments, "Wagner")
    step3_crown_model_vals = ensure_list_length(step3_cfg.get("crown_fire_model", ["Perrakis", "Perrakis", "Perrakis"]), n_step3_treatments, "Perrakis")
    step3_forest_vals = ensure_list_length(step3_cfg.get("forest_type", ["Pine"] * (n_step3_treatments * 2)), n_step3_treatments * 2, "Pine")
    step3_surf_vals = ensure_list_length(step3_cfg.get("surf_fuel", ["grass"] * (n_step3_treatments * 2)), n_step3_treatments * 2, "grass")

    ui_step3_custom_fuels: List[bool] = []
    ui_step3_prune: List[float] = []
    ui_step3_fuels: List[float] = []
    ui_step3_hr1000s: List[float] = []
    ui_step3_ftcad: List[str] = []
    ui_step3_fsg_mod: List[bool] = []
    ui_step3_fsg_field: List[bool] = []
    ui_step3_crown_type: List[str] = []
    ui_step3_crown_model: List[str] = []
    ui_step3_forest: List[str] = []
    ui_step3_surf: List[str] = []

    forest_type_options = ["Deciduous", "Douglas-fir", "Mixed", "Pine", "Spruce", "Grass", "Shrub", "Slash"]
    surface_fuel_options = ["litter", "b_litter", "shrub", "grass", "slash", "moss"]
    ftcad_options = ["C-1", "C-2", "C-3", "C-4", "C-5", "C-6", "C-7", "D-1/2", "M-1/2", "M-3/4", "O-1a"]

    for i, treatment_name in enumerate(treatment_names_step3):
        with st.expander(f"Fire Modeling Treatment {treatment_name}", expanded=(i == 0)):
            top_left, top_right = st.columns(2)
            with top_left:
                ui_step3_custom_fuels.append(
                    st.checkbox(
                        f"Do you want to use measured fuels or predicted fuel model preset fuels? - {treatment_name}",
                        value=bool(step3_custom_fuels_vals[i]),
                        key=f"step3_custom_fuels_{i}",
                        help="Checked box = measured fuels (custom field-measured fuel loads). Recommended, except where fuels may not be representative of the site and fire behaviour underpredicts.",
                    )
                )
                ui_step3_prune.append(
                    float(
                        st.number_input(
                            f"What prune height do you want? (m) - {treatment_name}",
                            min_value=0.0,
                            value=float(step3_prune_vals[i]),
                            step=0.5,
                            key=f"step3_prune_{i}",
                        )
                    )
                )
                ui_step3_fuels.append(
                    float(
                        st.number_input(
                            f"What fine fuels do you want to treat to? (kg/m^2) - {treatment_name}",
                            min_value=0.0,
                            value=float(step3_fuels_vals[i]),
                            step=0.05,
                            format="%.2f",
                            key=f"step3_fuels_{i}",
                        )
                    )
                )
                ui_step3_hr1000s.append(
                    float(
                        st.number_input(
                            f"What 1000-hr (LWD+CWD) fuels do you want to retain? (kg/m^2) - {treatment_name}",
                            min_value=0.0,
                            value=float(step3_hr1000_vals[i]),
                            step=0.1,
                            format="%.2f",
                            key=f"step3_hr1000s_{i}",
                            help="Does not affect fire behaviour modeling outputs.",
                        )
                    )
                )
                ui_step3_ftcad.append(
                    st.selectbox(
                        f"What is the CAN FBP post treatment fuel type? - {treatment_name}",
                        options=ftcad_options,
                        index=ftcad_options.index(str(step3_ftcad_vals[i])) if str(step3_ftcad_vals[i]) in ftcad_options else ftcad_options.index("C-7"),
                        key=f"step3_ftcad_{i}",
                        help="Many fuel treated stands with intertree spacing and high CBH values are most closely related to C-7. Consider C-6 for uniform stands with uniform CBH.",
                    )
                )
            with top_right:
                ui_step3_fsg_mod.append(
                    st.checkbox(
                        f"Do you want to modify FSG to account for thinning? - {treatment_name}",
                        value=bool(step3_fsg_mod_vals[i]),
                        key=f"step3_fsg_mod_{i}",
                        help="Checked box = yes, modify FSG to account for thinning. If fire behaviour overpredicts, consider using this.",
                    )
                )
                ui_step3_fsg_field.append(
                    st.checkbox(
                        f"Do you want to use a field measured FSG or a modeled FSG? - {treatment_name}",
                        value=bool(step3_fsg_field_vals[i]),
                        key=f"step3_fsg_field_{i}",
                        help="Checked box = field-measured FSG. Unchecked box = modeled FSG. If the stand is homogeneous with obvious differentiation between strata, consider modeled FSG; otherwise, field FSG may be a better estimator.",
                    )
                )
                ui_step3_crown_type.append(
                    st.selectbox(
                        f"What model do you want to link to crown fire? - {treatment_name}",
                        options=["Wagner", "Finney", "ScottReinhardt"],
                        index=["Wagner", "Finney", "ScottReinhardt"].index(str(step3_crown_type_vals[i])) if str(step3_crown_type_vals[i]) in ["Wagner", "Finney", "ScottReinhardt"] else 0,
                        key=f"step3_crown_type_{i}",
                        help="Wagner 1993 (recommended), Finney 1998, Scott and Reinhardt 2001.",
                    )
                )
                ui_step3_crown_model.append(
                    st.selectbox(
                        f"What type of model should predict crown fire? - {treatment_name}",
                        options=["Perrakis", "CFIS"],
                        index=["Perrakis", "CFIS"].index(str(step3_crown_model_vals[i])) if str(step3_crown_model_vals[i]) in ["Perrakis", "CFIS"] else 0,
                        key=f"step3_crown_model_{i}",
                        help="Perrakis (recommended) unless you think it overpredicts, then use CFIS.",
                    )
                )

            pre_col, post_col = st.columns(2)
            with pre_col:
                ui_step3_forest.append(
                    st.selectbox(
                        f"What forest type is your unit? (Pre) - {treatment_name}",
                        options=forest_type_options,
                        index=forest_type_options.index(str(step3_forest_vals[i])) if str(step3_forest_vals[i]) in forest_type_options else forest_type_options.index("Pine"),
                        key=f"step3_forest_pre_{i}",
                        help="Deciduous, Douglas-fir, Mixed, Pine, Spruce, Grass, Shrub, Slash.",
                    )
                )
                ui_step3_surf.append(
                    st.selectbox(
                        f"What surface fuel will carry fire? (Pre) - {treatment_name}",
                        options=surface_fuel_options,
                        index=surface_fuel_options.index(str(step3_surf_vals[i])) if str(step3_surf_vals[i]) in surface_fuel_options else surface_fuel_options.index("grass"),
                        key=f"step3_surf_pre_{i}",
                        help="litter = fine needle litter and twigs (usually recommended), b_litter = broadleaf litter stands, shrub = shrub fuels, grass = grass dominated stands, slash = logging slash or activity fuels, moss = moss and peat fuels.",
                    )
                )
            with post_col:
                ui_step3_forest.append(
                    st.selectbox(
                        f"What forest type is your unit? (Post) - {treatment_name}",
                        options=forest_type_options,
                        index=forest_type_options.index(str(step3_forest_vals[i + n_step3_treatments])) if str(step3_forest_vals[i + n_step3_treatments]) in forest_type_options else forest_type_options.index("Pine"),
                        key=f"step3_forest_post_{i}",
                        help="Deciduous, Douglas-fir, Mixed, Pine, Spruce, Grass, Shrub, Slash.",
                    )
                )
                ui_step3_surf.append(
                    st.selectbox(
                        f"What surface fuel will carry fire? (Post) - {treatment_name}",
                        options=surface_fuel_options,
                        index=surface_fuel_options.index(str(step3_surf_vals[i + n_step3_treatments])) if str(step3_surf_vals[i + n_step3_treatments]) in surface_fuel_options else surface_fuel_options.index("grass"),
                        key=f"step3_surf_post_{i}",
                        help="litter = fine needle litter and twigs (usually recommended), b_litter = broadleaf litter stands, shrub = shrub fuels, grass = grass dominated stands, slash = logging slash or activity fuels, moss = moss and peat fuels.",
                    )
                )

    step3_save_col, step3_run_col = st.columns(2)
    if step3_save_col.button("Save Step 3 Settings", use_container_width=True):
        cfg["firemodel_results"] = {
            "elevation": step3_elevation,
            "custom_fuels": ui_step3_custom_fuels,
            "prune_vector": ui_step3_prune,
            "fuels": ui_step3_fuels,
            "hr1000s": ui_step3_hr1000s,
            "ftcad_vector": ui_step3_ftcad,
            "forest_type": ui_step3_forest,
            "surf_fuel": ui_step3_surf,
            "fsg_mod_flag": ui_step3_fsg_mod,
            "fsg_field_flag": ui_step3_fsg_field,
            "intensity_flag": step3_intensity_flag,
            "fuel_moisture_type": step3_fuel_moisture_type,
            "heat_flag": step3_heat_flag,
            "wind_gust_mod": step3_wind_gust_mod,
            "crown_fire_type": ui_step3_crown_type,
            "crown_fire_model": ui_step3_crown_model,
            "grass_curing": step3_grass_curing,
            "num_weathers": step3_num_weathers,
            "weather_name": step3_weather_name,
            "season": step3_season,
            "advanced_models": step3_advanced_models,
        }
        save_config(cfg)
        st.session_state.config_state = cfg
        st.success("Saved fire modeling settings.")

    step3_ready = step3_input_ready(current_project, step3_weather_name, treatment_names_step3)
    step3_help = (
        "Step 3 needs Step 2 weather outputs plus FuelCalc output files for each treatment."
        if not step3_ready
        else "Run Step 3 with the current fire modeling settings."
    )
    if step3_run_col.button("Run Step 3", use_container_width=True, disabled=(not step3_ready) or step3_running, help=step3_help):
        cfg["firemodel_results"] = {
            "elevation": step3_elevation,
            "custom_fuels": ui_step3_custom_fuels,
            "prune_vector": ui_step3_prune,
            "fuels": ui_step3_fuels,
            "hr1000s": ui_step3_hr1000s,
            "ftcad_vector": ui_step3_ftcad,
            "forest_type": ui_step3_forest,
            "surf_fuel": ui_step3_surf,
            "fsg_mod_flag": ui_step3_fsg_mod,
            "fsg_field_flag": ui_step3_fsg_field,
            "intensity_flag": step3_intensity_flag,
            "fuel_moisture_type": step3_fuel_moisture_type,
            "heat_flag": step3_heat_flag,
            "wind_gust_mod": step3_wind_gust_mod,
            "crown_fire_type": ui_step3_crown_type,
            "crown_fire_model": ui_step3_crown_model,
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
    render_step3_runtime(current_project, "Fire Modeling Setup")

    if step3_running:
        st.info("Step 3 is running. Results will refresh after the run completes.")
    elif manifest:
        step3_results = manifest.get("step3", {})
        basic_result_items = [
            (step3_results.get("treatment_summary", {}).get("path", ""), "Treatment Summary"),
            (step3_results.get("fbp_90th_csi_stand", {}).get("path", ""), "FBP 90th CSI Stand"),
        ]
        advanced_result_items = [
            (step3_results.get("crown_fire_probability_boxplots", {}).get("path", ""), "Crown Fire Probability BoxPlots"),
            (step3_results.get("crowning_index_windspeed", {}).get("path", ""), "Crowning Index at Windspeed"),
            (step3_results.get("crowning_index_fuelmoist", {}).get("path", ""), "Crowning Index at Fuel Moisture"),
            (step3_results.get("head_fire_intensity", {}).get("path", ""), "Head Fire Intensity"),
            (step3_results.get("rate_of_spread", {}).get("path", ""), "Rate of Spread"),
        ]
        has_step3_results = any(Path(path_str).exists() for path_str, _ in (basic_result_items + advanced_result_items) if path_str)
        st.markdown("**Results: Fire Modeling Prediction**")
        st.markdown("**Section 1: Basic Results**")
        displayed_basic_results = show_image_grid(basic_result_items, columns=2)
        if displayed_basic_results == 0:
            st.warning("No basic fire modeling result plots are available yet.")

        st.markdown("**Section 2: Advanced Results**")
        displayed_advanced_results = show_image_grid(advanced_result_items, columns=3)
        if displayed_advanced_results == 0:
            st.warning("No advanced fire modeling result plots are available yet.")

with diag_col:
    start_panel("fm-sidecard")
    with st.container():
        st.markdown("**Diagnostics**")
        if step3_running:
            st.caption("Diagnostics are paused during Step 3 to keep the live view calmer.")
        else:
            diag_tabs = st.tabs(["Status", "Artifacts", "Preview"])
            with diag_tabs[0]:
                if status:
                    st.json(status)
                else:
                    st.info("No status file yet.")
            with diag_tabs[1]:
                if manifest:
                    df = flatten_manifest(manifest)
                    if not df.empty:
                        st.dataframe(df, use_container_width=True)
                    else:
                        st.info("No artifacts found.")
                else:
                    st.info("No manifest file yet.")
            with diag_tabs[2]:
                if not manifest:
                    st.info("No manifest found.")
                else:
                    choices = {
                        "Step 0 Summary": manifest.get("step0", {}).get("summary", {}).get("path", ""),
                        "Step 1 Summary": manifest.get("step1", {}).get("summary", {}).get("path", ""),
                        "Treatment Description Summary": manifest.get("step05", {}).get("summary", {}).get("path", ""),
                        "Step 2 Summary": manifest.get("step2", {}).get("summary", {}).get("path", ""),
                        "Step 3 Summary": manifest.get("step3", {}).get("summary", {}).get("path", ""),
                    }
                    selected = st.selectbox("Choose dataset", list(choices.keys()), label_visibility="collapsed")
                    show_json_file(choices[selected], selected)
