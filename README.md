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

1. Step 1 artifacts: `data/intermediate/step1_clean_snap/`
2. Step 2 artifacts: `data/intermediate/step2_fuelcalc/`
3. Step 3 artifacts: `data/outputs/step3_fire_model/`
4. Run status (step-by-step success/errors): `data/outputs/run_status/pipeline_status.json`
5. UI manifest (single source for app): `data/outputs/manifest/pipeline_manifest.json`

Python UI:

1. Install dependencies:
   `pip install -r requirements.txt`
2. Run:
   `streamlit run app/app.py`
   or double-click `run_ui_windows.bat`

Git/GitHub workflow:

1. This repo tracks code/config/templates, not generated outputs or large raw data.
2. After clone, copy your `data/raw/` inputs from your source machine.
3. Optional local config override:
   - Create `config/config.json` for local changes.
   - If missing, pipeline falls back to `config/config.example.json`.
