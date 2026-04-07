# FireModeler

A Python wrapper to run R scripts for fire modeling behavior.

Generic wildfire processing pipeline.

How to run:

1. Install R
2. Open terminal
3. Navigate to this folder:
   cd FireModel
4. Run:
   Rscript R/run_pipeline.R

All paths are relative, so this folder can be moved anywhere.

Outputs to check after each run:

1. Step 1 artifacts: `projects/<project_name>/data/intermediate/step1_clean_snap/`
2. Step 2 artifacts: `projects/<project_name>/data/intermediate/step2_fuelcalc/`
3. Step 3 artifacts: `projects/<project_name>/data/outputs/step3_fire_model/`
4. Run status (step-by-step success/errors): `projects/<project_name>/data/outputs/run_status/pipeline_status.json`
5. UI manifest (single source for app): `projects/<project_name>/data/outputs/manifest/pipeline_manifest.json`

Windows setup:

1. Install:
   - Python 3.10+
   - R 4.5.x recommended
   - FuelCalc
2. From the repo root, run:
   `powershell -ExecutionPolicy Bypass -File scripts/bootstrap_windows.ps1`
   or double-click `setup_windows.bat`
3. The bootstrap script will:
   - create `.venv`
   - install Python dependencies
   - locate `Rscript.exe`
   - write `rscript_path` into `config/config.json`
   - run `renv::restore()`

Python UI:

1. Activate the venv if desired:
   `.\.venv\Scripts\Activate.ps1`
2. Run:
   `python -m streamlit run app/app.py`
   or double-click `run_ui_windows.bat`
3. In the UI, set:
   - `Project Name`: used as both the display label and the raw file prefix such as `TR_LionsBurn`

Git/GitHub workflow:

1. This repo tracks code/config/templates, not generated outputs or large raw data.
2. Shared templates live under `templates/` at the repo root and are used by all projects.
3. The large `BEC_Zones_OLD.*` shapefile set is intentionally not stored in Git because it exceeds GitHub's file size limit.
4. Copy `BEC_Zones_OLD.*` into `templates/` manually on any machine that needs Step 2 spatial weather-zone lookup.
5. After clone, copy your project-specific inputs into `projects/<project_name>/data/raw/`.
6. Optional local config override:
   - Create `config/config.json` for local changes.
   - If missing, pipeline falls back to `config/config.example.json`.
