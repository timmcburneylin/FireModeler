# FireModel Office Deployment Checklist

This checklist is for installing FireModel on multiple office Windows computers with the least amount of setup drift.

## Current Baseline

Use these versions unless there is a deliberate migration:

- Python: `3.7.6` is currently in use on the main dev machine, but this is old and has already caused dependency friction.
- Streamlit: `1.23.1`
- Pandas: `>=1.3,<3`
- Pillow: `9.5.0`
- streamlit-aggrid: `0.3.0`
- R: `4.5.2` is what `renv.lock` currently records
- renv: `1.1.7`

## Important Risk Before Rollout

The current `renv.lock` does **not** list some R packages that the pipeline has needed during development and testing.

Notably missing from `renv.lock`:

- `sf`
- `cffdrs`
- `terra`
- `flextable`
- `openair`
- `patchwork`

If those packages are required on a machine but not captured in `renv.lock`, `renv::restore()` may complete and the app can still fail later when a step actually runs.

## Recommended Pre-Deployment Fixes

Before installing on office machines:

1. On the known-good development machine, activate the working R project.
2. Make sure the project can run the intended steps end-to-end.
3. Run:

```r
renv::snapshot()
```

4. Commit the updated `renv.lock`.

This is the single best way to reduce “works on one machine, fails on another” R-package errors.

## Supported Install Strategy

For each office machine:

1. Install R `4.5.x`
2. Install Python
3. Install FuelCalc
4. Clone or copy the repo
5. Copy required non-Git assets
6. Run the Windows bootstrap
7. Validate the app

## Required Non-Git Assets

These are not stored in GitHub and must be copied manually if the machine needs the corresponding functionality:

- `templates/BEC_Zones_OLD.*`
  - needed for Step 2 spatial weather-zone lookup

If other large local-only assets are still being used outside Git, list and copy those too before validation.

## Per-Machine Install Steps

### 1. Install R

Install:

- `R 4.5.x`

Recommended:

- use the same minor line as the development lockfile, ideally `4.5.2` or as close as possible

### 2. Install Python

Recommended:

- install one consistent Python version across all office machines

Important:

- avoid relying on random system Python or old Anaconda base environments
- prefer a clean install that will be used only for this app’s `.venv`

### 3. Install FuelCalc

FuelCalc is expected to already be installed on the office machines.

### 4. Clone or Copy the Repo

Place the repo somewhere local, for example:

- `C:\Work\FireModel`

Avoid:

- network drives
- highly locked folders
- unusual OneDrive-only locations if possible

### 5. Copy Manual Assets

Copy the missing large shapefile set into:

- `templates/`

At minimum, this includes:

- `BEC_Zones_OLD.*`

### 6. Run Bootstrap

From repo root:

```powershell
powershell -ExecutionPolicy Bypass -File scripts/bootstrap_windows.ps1
```

or double-click:

- `setup_windows.bat`

What this should do:

- create `.venv`
- install Python dependencies
- detect `Rscript.exe`
- write `rscript_path` into `config/config.json`
- run `renv::restore()`

## First-Run Validation

After setup, run:

- `run_ui_windows.bat`

Then validate in the app:

1. `Environment Check`
   - `Python venv` ready
   - `Rscript` ready
   - `FuelCalc` ready
   - `Config file` ready

2. `Project Setup`
   - select a known test project
   - confirm required SNAP files are recognized
   - run `Project Setup`

3. Template editing
   - confirm treatment template CSVs appear
   - edit one value
   - click `Save CSV`

4. `Generate Cutting Specs`
   - confirm species columns appear
   - confirm live `Overall Cutting Specs / Cut / Leave` summary updates

5. `Run Treatment Description`
   - confirm outputs appear

6. `FuelCalc Setup and Processing`
   - confirm the step opens and loads its settings cleanly

7. `Local Weather Analysis`
   - confirm Wind Rose, Danger Days, and Weather Conditions plots can generate

8. `Fire Modeling Setup`
   - confirm Step 3 can launch and show progress

## Known Local Environment Hazards

### Python drift

We already hit:

- old Python `3.7.6`
- old Anaconda base environment
- `streamlit-aggrid` incompatibility with that interpreter

Current app behavior:

- the app falls back safely if `st_aggrid` import fails

But for office rollout, consistency is much better than fallback.

### R package drift

We already hit issues with:

- `jsonlite`
- `plyr`
- `sf`
- `cffdrs`

If `renv.lock` is stale, these failures will reappear on new machines.

### Path drift

We already hit machines running:

- the wrong `app.py`
- the wrong project folder
- the wrong `Rscript.exe`

Always confirm:

- the correct repo folder
- the correct project selected in the app
- the correct `rscript_path` in `config/config.json`

## Best Deployment Sequence

Use this rollout order:

1. Update and commit `renv.lock`
2. Test on one clean office machine
3. Fix anything that fails there
4. Repeat the same install on the remaining office machines

Do **not** start with multiple machines in parallel until one clean machine has passed the full validation list.

## What To Hand Other Staff

For each staff machine, give them:

- the repo
- the manual assets for `templates/`
- the exact install instruction:

```powershell
powershell -ExecutionPolicy Bypass -File scripts/bootstrap_windows.ps1
```

- the exact launch instruction:

```powershell
.\run_ui_windows.bat
```

## Recommended Next Improvement

Before broad office deployment, do this:

1. refresh `renv.lock` from the known-good R environment
2. document any remaining manual assets beyond `BEC_Zones_OLD.*`
3. test a clean install on one fresh machine

If those three are done, the office rollout becomes much more predictable.
