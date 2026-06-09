# FireModeler SOP Source Brief

This brief is a compact source document for drafting a full Standard Operating Procedure for the FireModeler app. It is intentionally practical and focused on what an operator needs to know when setting up and running a project.

## Purpose

FireModeler is a Streamlit app that wraps an R-based workflow for preparing SNAP inventory data, creating treatment and FuelCalc inputs, processing local weather, and running final fire behavior modeling outputs.

The SOP should help a user:

- Create or select a project.
- Prepare required raw input files.
- Generate and edit treatment tables.
- Generate cutting specifications and treatment summaries.
- Run Step 1 to prepare FuelCalc inputs.
- Run FuelCalc externally and confirm outputs.
- Run Step 2 local weather analysis.
- Run Step 3 fire behavior modeling.
- Diagnose common setup errors.

## App Launch

Preferred Windows launch path:

1. Open the repository root.
2. Double-click `run_ui_windows.bat`, or run:

```powershell
.\.venv\Scripts\python.exe -m streamlit run app\app.py
```

Initial setup on a new machine should use `setup_windows.bat` or the office install guide.

## Project Folder Structure

Each project lives under:

```text
projects/<project_name>/
```

Expected structure:

```text
projects/<project_name>/data/raw/
projects/<project_name>/data/intermediate/
projects/<project_name>/data/outputs/
projects/<project_name>/data/external/
```

Shared app templates live in the repository-level `templates/` folder, not inside individual projects.

## Required SNAP Inputs

For a project named `Eastgate`, the expected SNAP files are:

```text
projects/Eastgate/data/raw/SNAP/Eastgate_OS.csv
projects/Eastgate/data/raw/SNAP/Eastgate_US.csv
projects/Eastgate/data/raw/SNAP/Eastgate_EXTRA.csv
projects/Eastgate/data/raw/SNAP/Eastgate_FUELS.csv
```

The project name must match the file prefix.

Important SNAP notes:

- SNAP exports must be true CSV files, not Excel files renamed with a `.csv` extension.
- If R reports embedded nulls or invalid multibyte strings such as `<d0><cf>`, the file is probably an Excel binary file with the wrong extension.
- The app expects these files to be readable and writable. If a permission denied error occurs, check whether the CSV is open in Excel.

## Project Setup

Run Project Setup after placing SNAP files in the project folder.

Project Setup:

- Reads the four SNAP files.
- Generates treatment-specific folders under `data/raw/Stand_StockTables/`.
- Generates editable stock table CSVs for each treatment or stratum.
- Autopopulates species columns by treatment/stratum, based on the species found in that treatment.

Example generated folder:

```text
projects/Eastgate/data/raw/Stand_StockTables/FTU-A_tables/
```

Example generated files:

```text
OS_SPH.csv
OS_BA.csv
OS_Ht_cbh.csv
OS_Vol.csv
US_SPH.csv
US_Ht_CBH.csv
cuttingSpecs_FTU-A.csv
```

## Treatments to Process

The app includes a `Treatments to process` field near the treatment table section.

Use this to scope downstream processing. For example, if testing only FTU-A:

```text
FTU-A
```

This field controls `process_to_fuelcalc.tr_names` in `config/config.json`.

This is important because Step 1 and Step 3 should only look for FuelCalc inputs and outputs for the selected treatments.

## Editing Treatment Tables

The generated CSV tables can be edited in the app.

Recommended workflow:

1. Open the treatment expander.
2. Edit values directly in the table.
3. Paste columns from Excel directly into the Streamlit table when needed.
4. Click the table save button before running later steps.

Notes:

- The app currently uses Streamlit's built-in `st.data_editor` for these tables.
- No extra table package is required for this editable table behavior.
- If copied values appear briefly and disappear, refresh the app and verify the current code is using the saved editor state callback.
- If a CSV cannot be saved, make sure the file is not open in Excel.

## Cutting Specs

Generate Cutting Specs after reviewing or editing the treatment stock tables.

Expected output for FTU-A:

```text
projects/Eastgate/data/raw/Stand_StockTables/FTU-A_tables/cuttingSpecs_FTU-A.csv
```

If the app reports missing files for `A_tables` but generated folders are named `FTU-A_tables`, the treatment names in the config are stale. Set `Treatments to process` to the exact generated treatment name, such as `FTU-A`.

## Treatment Description

Run Treatment Description after Cutting Specs.

This step creates tables and plots summarizing pre- and post-treatment structure.

Example output folder:

```text
projects/<project_name>/data/outputs/TreatmentDescription/
```

Treatment Description can partially generate plots before failing if one of the generated species columns is missing from an overstory or understory table. Current behavior should fill missing species columns with zeros where needed.

## Step 1: Process to FuelCalc

Run Step 1 after treatment tables and cutting specs are ready.

Step 1 prepares FuelCalc inputs for each treatment listed in `Treatments to process`.

Expected FuelCalc working/output area:

```text
projects/<project_name>/data/raw/FuelCalc/
```

Expected FTU-A output folder:

```text
projects/Eastgate/data/raw/FuelCalc/Outputs/TU_FTU-A/
```

The app may create files such as:

```text
TU_FTU-A_FuelCalc_FFI.fcp
TU_FTU-A_FuelCalc_FFI.ffi
TU_FTU-A_FuelCalc_FFI.tre
Run_FuelCalc_TU_FTU-A.bat
```

After Step 1, the operator may need to run FuelCalc externally, depending on the workflow.

## FuelCalc External Run

Confirm FuelCalc has produced final output CSVs before Step 3.

For FTU-A, Step 3 expects:

```text
projects/Eastgate/data/raw/FuelCalc/Outputs/TU_FTU-A/TU_FTU-A_FuelCalc_FFI_Outputs.csv
```

If Step 3 reports a missing FuelCalc file for another treatment, such as `TU_FTU-B`, check `Treatments to process`. If testing only FTU-A, it should be set to `FTU-A`.

## Step 2: Local Weather Analysis

Step 2 processes a raw station weather CSV and creates daily FWI/weather products.

The raw station file must be placed here:

```text
projects/<project_name>/data/raw/Weather/raw/<station_code>.csv
```

Example:

```text
projects/Eastgate/data/raw/Weather/raw/317.csv
```

Step 2 settings include:

- Weather station type: `MOF` or `EC`.
- Weather station name.
- Weather station code.
- Latitude.
- Longitude.
- Danger region.

MOF files are currently the proven path. The raw MOF file should include hourly weather fields similar to:

```text
station_observations
wind_speed, temperature, wind_direction, relative_humidity, time, precipitation
```

Step 2 outputs include:

```text
data/raw/Weather/raw/<station_name>_Hourly_Weather.csv
data/raw/Weather/WeatherLists/allstations_90th_FWList_dates_summer.csv
data/raw/Weather/WindRoses/<station_name>_WindRoses.jpg
data/raw/Weather/DangerDays/<station_name>_DangerDays.jpg
data/raw/Weather/WeatherConditions/<station_name>_WeatherDistributions.jpg
```

For Eastgate using Allison Pass, confirmed outputs included:

```text
ALLISON PASS_Hourly_Weather.csv
allstations_90th_FWList_dates_summer.csv
ALLISON PASS_WindRoses.jpg
ALLISON PASS_DangerDays.jpg
ALLISON PASS_WeatherDistributions.jpg
```

## EC Weather Caveat

Some EC downloads may contain daily station observations only, such as:

```text
ONE_DAY_PRECIPITATION
ONE_DAY_RAIN
ONE_DAY_SNOW
time
MIN_TEMP
MAX_TEMP
```

That file is not sufficient for the current Step 2 weather pipeline because Step 2 needs hourly fields including wind direction, wind speed, relative humidity, temperature, precipitation, and time.

For EC mode, the script expects columns similar to:

```text
wind_direction
total_precipitation
wind_speed
air_temperature
relative_humidity
time
```

If the only available source is EC daily data, additional app logic or a different preprocessing path will likely be needed.

## Step 3: Fire Modeling

Step 3 needs both weather outputs and FuelCalc output files.

Step 3 readiness requires:

```text
data/raw/Weather/WeatherLists/allstations_90th_FWList_dates_summer.csv
data/raw/Weather/raw/<step3_weather_name>_Hourly_Weather.csv
data/raw/FuelCalc/Outputs/TU_<treatment>/TU_<treatment>_FuelCalc_FFI_Outputs.csv
```

The Step 3 weather name should match the Step 2 station name unless a separate matching hourly weather file exists.

Example:

```text
Step 2 station: ALLISON PASS
Step 3 weather name: ALLISON PASS
```

If Step 3 is greyed out:

- Confirm the Step 2 weather list exists.
- Confirm the hourly weather file exists with the same station name Step 3 is using.
- Confirm every treatment listed in `Treatments to process` has a FuelCalc output CSV.

## Common Errors and Likely Causes

### Embedded nulls or invalid multibyte string in SNAP CSV

Likely cause:

- Excel file was renamed to `.csv` instead of exported as CSV.

Fix:

- Open the file in Excel and export/save as true CSV.

### Permission denied writing SNAP or treatment CSV

Likely cause:

- The file is open in Excel or another program.

Fix:

- Close the file and rerun the step.

### `replacement has length zero` during Project Setup

Likely cause:

- Missing SNAP tree data needed for imputation, such as missing height or CBH values.

Fix:

- Review the affected plots and fill or correct missing data before rerunning.

### Missing `A_tables` or `NA_tables`

Likely cause:

- Treatment names in config do not match generated treatment folder names.
- The app is trying to process all strata when only one treatment is configured.

Fix:

- Set `Treatments to process` to the exact generated name, such as `FTU-A`.

### Missing `TU_FTU-B_FuelCalc_FFI_Outputs.csv` while testing FTU-A

Likely cause:

- Downstream step is trying to run more treatments than intended.

Fix:

- Confirm `Treatments to process` is `FTU-A`.
- Confirm `config/config.json` has `process_to_fuelcalc.tr_names` set to `["FTU-A"]`.

### Step 3 greyed out after successful Step 2

Likely cause:

- Step 3 weather name is stale and does not match the Step 2 station output.

Fix:

- Set Step 3 weather name to the Step 2 station name.
- Example: use `ALLISON PASS`, not an older value like `MERRITT 2 HUB`.

### `wind_direction` column does not exist

Likely cause:

- The weather file does not contain hourly fire weather fields.
- The file may be a daily EC station observation dataset.

Fix:

- Download hourly weather data with wind direction, wind speed, humidity, temperature, precipitation, and time.
- Use MOF data where available, because that path is currently verified.

## Current Eastgate Test Notes

Eastgate was used as a fresh project test.

Known working setup during testing:

```text
Project: Eastgate
Treatment scope: FTU-A
Weather type: MOF
Weather station: ALLISON PASS
Weather station code: 317
```

Confirmed FTU-A overstory species in raw SNAP did not include `At` or `Py`; generated FTU-A overstory tables should only include FTU-A species.

## SOP Drafting Recommendation

Use this brief as source material in a fresh chat or writing pass.

Suggested SOP structure:

1. Purpose and scope.
2. Required software and setup.
3. Project folder and file naming rules.
4. Preparing SNAP inputs.
5. Running Project Setup.
6. Editing treatment tables.
7. Generating Cutting Specs and Treatment Description.
8. Running Step 1 and FuelCalc.
9. Preparing weather data.
10. Running Step 2.
11. Running Step 3.
12. Output locations.
13. Troubleshooting appendix.
14. Quality control checklist.

