# FireModel Office Install Guide

This is the short version for installing FireModel on an office Windows computer.

For deeper troubleshooting and deployment notes, see:

- [DEPLOYMENT_CHECKLIST.md](C:\Users\tgmcb\Desktop\FireModel\DEPLOYMENT_CHECKLIST.md)

## Before You Start

You will need:

- R `4.5.x`
- Python
- FuelCalc
- a local copy of this repo
- the manual template files not stored in Git:
  - `BEC_Zones_OLD.*`

## Recommended Install Location

Install the repo somewhere local, for example:

- `C:\Work\FireModel`

Avoid network drives if possible.

## Step 1. Install Software

Install:

1. R `4.5.x`
2. Python
3. FuelCalc

## Step 2. Copy Manual Template Files

Copy:

- `BEC_Zones_OLD.*`

into:

- `templates/`

These files are needed for Step 2 spatial weather-zone lookup.

## Step 3. Run Setup

From the repo root, run:

```powershell
powershell -ExecutionPolicy Bypass -File scripts/bootstrap_windows.ps1
```

Or double-click:

- `setup_windows.bat`

What setup does:

- creates `.venv`
- installs Python dependencies
- finds `Rscript.exe`
- writes `config/config.json`
- runs `renv::restore()`

## Step 4. Launch the App

Run:

```powershell
.\run_ui_windows.bat
```

## Step 5. First Validation

When the app opens, check:

1. `Environment Check`
   - Python venv = ready
   - Rscript = ready
   - FuelCalc = ready
   - Config file = ready

2. Select a known test project

3. Run `Project Setup`

4. Confirm treatment tables appear

5. Run `Generate Cutting Specs`

6. Confirm the cutting specs table and live summary appear

7. Run `Treatment Description`

If all of that works, the machine is in good shape to continue.

## If Something Fails

Check these first:

1. Is `templates/BEC_Zones_OLD.*` present?
2. Did setup finish fully?
3. Does `config/config.json` point to the correct `Rscript.exe`?
4. Is the machine using the intended Python install?

## Daily Use

Normal launch command:

```powershell
.\run_ui_windows.bat
```

## Installing Updates

Updates do not require a GitHub account because the repository is public.

1. Close FireModeler.
2. Double-click `update_firemodeler.bat` in the FireModeler folder.
3. Wait for the successful update message.
4. Reopen FireModeler with `run_ui_windows.bat`.

The updater changes only Git-tracked application files. Local projects, `config/config.json`, generated outputs, the virtual environment, and manually supplied template files remain in place. If an application file was edited manually on that computer, the updater stops and reports the change instead of overwriting it.

## Notes

- This project is sensitive to Python and R version drift.
- Try to keep office machines on the same Python/R versions.
- If one machine works, copy that install process as closely as possible for the others.
