# Fire Modeler Standard Operating Procedure (SOP)

## 1. Purpose

### 1.1 Purpose of Fire Modeler

Fire Modeler is a Streamlit application that provides a user interface for an R-based workflow used to prepare SNAP inventory data, generate treatment and FuelCalc inputs, process weather data, and produce final fire behavior modeling outputs.

The application is intended to guide users through a standardized workflow from project setup through fire behavior modeling while organizing project data and intermediate outputs in a consistent directory structure.

### 1.2 Intended Audience

This SOP is intended for office staff responsible for preparing forestry inventory data and running fire behavior modeling workflows.

It assumes users are familiar with forestry inventory concepts and SNAP data but are not expected to understand the internal implementation of the Fire Modeler application.

### 1.3 Scope of the Workflow

This SOP covers the standard office workflow for:

- Creating or selecting a Fire Modeler project.
- Preparing required SNAP input files.
- Running Project Setup.
- Generating and editing treatment/stratum tables.
- Creating cutting specifications and treatment summaries.
- Preparing FuelCalc inputs.
- Processing weather data.
- Running fire behavior modeling.
- Diagnosing common setup and configuration issues.

Operation of external software such as FuelCalc is covered only to the extent necessary for successful completion of the Fire Modeler workflow.

### 1.4 Overview of the Modeling Pipeline

The standard Fire Modeler workflow consists of the following stages:

1. Create or select a project.
2. Place required SNAP input files into the project directory.
3. Run Project Setup to generate treatment/stratum-specific tables.
4. Review and edit treatment/stratum tables as required.
5. Generate Cutting Specifications and Treatment Description outputs.
6. Run Step 1 to prepare FuelCalc inputs.
7. Run FuelCalc externally and confirm output files have been created.
8. Run Step 2 to process local weather data.
9. Run Step 3 to complete fire behavior modeling.

Most stages depend on successful completion of earlier setup steps. Step 3 requires both completed weather outputs from Step 2 and completed FuelCalc output files.

## 2. System Requirements and Installation

### 2.1 Required Software

Fire Modeler is a Streamlit application that wraps an R-based workflow.

Required software and local dependencies include:

- Windows workstation.
- Python 3.10 or newer.
- R. The README recommends R 4.5.x; the current development workstation is using R 4.6.0.
- FuelCalc.
- Fire Modeler repository with Python virtual environment dependencies installed.
- R package dependencies restored through the repository's `renv` setup.

The application repository includes setup and launch scripts for Windows users.

### 2.2 Initial Office Installation

For installation on a new workstation, use either:

- `setup_windows.bat`, or
- the office installation guide.

The setup process should:

- Create or prepare the Python virtual environment.
- Install Python dependencies.
- Locate `Rscript.exe`.
- Restore R package dependencies.
- Confirm required shared templates are present in the repository-level `templates/` folder.

Additional site-specific installation procedures should be documented in the office installation guide.

### 2.3 Launching the Application

The preferred Windows launch method is to open the repository root directory and either:

- Double-click `run_ui_windows.bat`, or
- launch the application from the command line using:

```powershell
.\.venv\Scripts\python.exe -m streamlit run app\app.py
```

### 2.4 Verifying Successful Startup

After launching, verify that the Fire Modeler user interface opens successfully.

At startup, confirm:

- The app opens in the browser.
- A project can be selected or created.
- The configured project name appears correctly in the interface.
- No startup error is shown in the terminal or browser.

## 3. Project Organization

### 3.1 Project Directory Structure

Each Fire Modeler project is stored beneath the `projects` directory using the following structure:

```text
projects/<project_name>/
```

Each project is expected to contain the following subdirectories:

```text
projects/<project_name>/data/raw/
projects/<project_name>/data/intermediate/
projects/<project_name>/data/outputs/
projects/<project_name>/data/external/
```

Maintaining this directory structure is required for proper operation of the application.

### 3.2 Required Folders

At a minimum, each project should contain the required data folders generated or used by the workflow:

- `data/raw`
- `data/intermediate`
- `data/outputs`
- `data/external`

Additional folders may be generated during processing.

### 3.3 Naming Conventions

Project names are used as the prefix for required SNAP input files.

The project folder name and SNAP file prefix must match exactly.

Failure to maintain consistent naming may prevent the application from locating required files.

### 3.4 Template Locations

Shared application templates are stored at the repository level in:

```text
templates/
```

Templates are not stored inside individual project directories.

## 4. Preparing SNAP Input Data

### 4.1 Required SNAP Export Files

Before Project Setup can be run, four SNAP export files must be copied into the project's SNAP directory.

For a project named `Eastgate`, the expected files are:

```text
projects/Eastgate/data/raw/SNAP/Eastgate_OS.csv
projects/Eastgate/data/raw/SNAP/Eastgate_US.csv
projects/Eastgate/data/raw/SNAP/Eastgate_EXTRA.csv
projects/Eastgate/data/raw/SNAP/Eastgate_FUELS.csv
```

All four files are required.

To create these four SNAP files, it is recommended that the user save the four required preset reports to the analysis tool within SNAP.

General SNAP export workflow:

1. Open SNAP.
2. Click the **Analysis** button.
3. Select the appropriate card for the project.
4. Click **Next**.
5. In the pop-up window, locate the **Items to View** list.
6. Click **Clear All** before building each preset so each saved report starts from a blank selection.
7. Select the required checkboxes for the report being created.
8. Click **Add**.
9. Enter the preset name.
10. Save the preset.

Create presets for:

- `EXTRA.csv`
- `FUELS.csv`
- `OVERSTORY.csv`
- `UNDERSTORY.csv`

The exact checkbox selections for each preset should be documented with screenshots or office-approved checkbox lists. Insert those screenshots or lists into this SOP when available.

### 4.2 File Naming Requirements

Each SNAP file must use the project name as its filename prefix and follow the required report suffixes:

- `EXTRA`
- `FUELS`
- `OS`
- `US`

For example, if the project name is `Eastgate`, the SNAP folder should contain:

```text
Eastgate_EXTRA.csv
Eastgate_FUELS.csv
Eastgate_OS.csv
Eastgate_US.csv
```

Changing the project name without updating the file prefixes may result in missing file errors.

### 4.3 Correct CSV Formatting

SNAP exports must be genuine CSV files.

Excel workbooks that have simply been renamed with a `.csv` extension are not valid inputs and may cause import failures.

CSV files must be readable by the application and available for writing during processing.

Important notes:

- Ensure the files are correctly saved as CSV files. If needed, open the saved file in Excel and use **Save As** to save it as `.csv`.
- Ensure all CSV files are complete, saved, and closed before attempting to run Fire Modeler.

### 4.4 Common Formatting Mistakes

Common input problems include:

- Renaming an Excel workbook with a `.csv` extension instead of exporting it as CSV.
- Leaving a CSV file open in Excel while Fire Modeler attempts to access it.

The first issue may generate errors related to embedded null values or invalid multibyte strings.

The second may generate permission denied errors during processing.

### 4.5 Verifying Data Integrity

Before running Project Setup:

- Confirm all four required SNAP files are present.
- Confirm the project name matches the file prefix.
- Confirm the files are true CSV exports.
- Confirm none of the files are currently open in Excel or another application.

Performing these checks before processing can prevent common setup failures.

## 5. Creating a New Project

### 5.1 Creating or Selecting a Project

Create or select the desired project within the Fire Modeler interface.

The project should follow the standard directory structure described in Section 3.

If creating the project folder manually, use the exact project name that will be used as the SNAP file prefix.

### 5.2 Copying SNAP Files into the Project

Copy the four required SNAP export files into:

```text
projects/<project_name>/data/raw/SNAP/
```

Verify that each filename begins with the exact project name.

### 5.3 Running Project Setup

After the SNAP files have been copied into the project, run **Project Setup**.

Project Setup performs the following functions:

- Reads the four SNAP input files.
- Generates treatment/stratum-specific folders within `data/raw/Stand_StockTables/`.
- Creates editable stock table CSV files for each treatment/stratum.
- Automatically populates species columns based on the species present within each treatment/stratum.

### 5.4 Generated Folders and Outputs

Project Setup creates treatment/stratum-specific folders beneath:

```text
projects/<project_name>/data/raw/Stand_StockTables/
```

For example:

```text
projects/Eastgate/data/raw/Stand_StockTables/FTU-A_tables/
```

Example generated Project Setup files include:

```text
OS_SPH.csv
OS_BA.csv
OS_Ht_cbh.csv
OS_Vol.csv
US_SPH.csv
US_Ht_CBH.csv
cuttingSpecs_<treatment>.csv
```

Project Setup may create an initial cutting specification template, such as `cuttingSpecs_FTU-A.csv`. The Generate Cutting Specs step should still be run after table review to update or finalize cutting specifications for downstream processing.

The generated stock tables are intended for review and editing before subsequent workflow steps.

### 5.5 Verifying Successful Project Initialization

Project Setup is considered successful when:

- Treatment/stratum-specific folders have been created.
- Editable stock table CSV files have been generated.
- Species columns have been automatically populated based on the species found within each treatment/stratum.

Users should review the generated folders and files before proceeding to treatment editing and downstream processing.

## 6. Treatment Configuration

### 6.1 Understanding Treatment Folders

After Project Setup is completed, Fire Modeler generates treatment/stratum-specific folders within:

```text
projects/<project_name>/data/raw/Stand_StockTables/
```

Each generated folder contains editable stock tables associated with a specific treatment or stratum.

For example:

```text
projects/Eastgate/data/raw/Stand_StockTables/FTU-A_tables/
```

These folders serve as the working location for treatment-specific inventory data used by downstream processing steps.

### 6.2 Treatments to Process Setting

Fire Modeler includes a **Treatments to process** field located near the treatment table section of the application.

This field defines which treatments will be included in downstream processing and controls the `process_to_fuelcalc.tr_names` setting within `config/config.json`.

For example, to process only the `FTU-A` treatment:

```text
FTU-A
```

Limiting the treatment scope ensures that subsequent processing steps only expect inputs and outputs for the selected treatments.

### 6.3 Treatment Naming Conventions

Treatment names entered in **Treatments to process** must exactly match the names of the generated treatment folders, excluding the `_tables` suffix.

For example, if Project Setup generates:

```text
FTU-A_tables
```

the treatment entered should be:

```text
FTU-A
```

Using outdated or incorrect treatment names may cause downstream processing steps to search for folders or files that do not exist.

### 6.4 Limiting Processing Scope for Testing

When testing a single treatment, configure **Treatments to process** to include only that treatment.

Restricting the processing scope ensures that Step 1 and Step 3 only expect FuelCalc inputs and outputs for the selected treatment and reduces the likelihood of missing file errors associated with unrelated treatments.

## 7. Editing Treatment Tables

### 7.1 Opening Editable Tables

Following Project Setup, editable treatment tables become available within the application.

Users should expand the appropriate treatment/stratum section to access the generated stock tables for review and modification.

### 7.2 Editing Within Fire Modeler

The generated CSV tables can be edited directly within the Fire Modeler interface.

The application uses Streamlit's built-in editable table functionality to allow modifications without requiring external spreadsheet software.

Users can add columns or rows for missing data if necessary. Before doing so, confirm that the added species, layer, or diameter class is actually required for the treatment/stratum being edited.

When entering treatment table values, users should open the project's Cathro report, or other approved source report, and copy values into the corresponding Fire Modeler tables. Ensure that all columns and rows match the report exactly before saving.

### 7.3 Copying Data from Excel

Where appropriate, users may copy columns of data from Excel and paste them directly into the editable tables within Fire Modeler.

This can speed up data entry from the Cathro report, but users should verify that the pasted values align with the correct species columns, stand layers, and diameter classes before saving.

### 7.4 Saving Changes

After making edits, users should use the table save function before proceeding to later workflow steps.

The edited table values are written back to the corresponding CSV file in the treatment/stratum folder.

If edited values appear briefly and then disappear, refresh the application and confirm the table was saved before continuing.

### 7.5 Best Practices for Editing

When editing treatment tables:

- Review generated species columns before making changes.
- Save each table after completing edits.
- Confirm that changes have been written successfully before continuing.
- Ensure that the underlying CSV files are not open in Excel during editing or saving operations.

Following these practices helps prevent data loss and file access conflicts.

## 8. Generating Cutting Specifications

### 8.1 Purpose of Cutting Specifications

The Cutting Specs step generates or updates treatment-specific cutting specification files after the treatment stock tables have been reviewed or edited.

These outputs are used by Treatment Description, Step 1, and later fire modeling steps.

### 8.2 Running the Tool

Run **Generate Cutting Specs** after completing review and editing of the treatment tables.

Treatment tables should be finalized before generating cutting specifications.

### 8.3 Expected Outputs

For a treatment named `FTU-A`, the expected output is:

```text
projects/Eastgate/data/raw/Stand_StockTables/FTU-A_tables/cuttingSpecs_FTU-A.csv
```

The file may already exist as an initial template after Project Setup, but the Cutting Specs step should be used to ensure it reflects the current treatment table data.

### 8.4 Verifying Generated Files

After generation, confirm that the expected cutting specification file exists within the corresponding treatment/stratum folder.

If the application reports missing files for folders such as `A_tables` while generated folders are named `FTU-A_tables`, verify that **Treatments to process** contains the exact generated treatment name.

## 9. Generating Treatment Descriptions

### 9.1 Purpose of Treatment Description

The Treatment Description step generates tables and figures summarizing pre-treatment and post-treatment stand structure.

These outputs provide a summary of treatment effects based on the configured treatment data and cutting specifications.

### 9.2 Running the Analysis

Run **Treatment Description** after Cutting Specifications have been generated.

Treatment tables and cutting specifications should be reviewed before executing this step.

### 9.3 Generated Summaries and Figures

Treatment Description creates summary tables and plots inside each treatment/stratum folder under:

```text
projects/<project_name>/data/raw/Stand_StockTables/<treatment>_tables/
```

For `FTU-A`, example outputs include:

```text
projects/Eastgate/data/raw/Stand_StockTables/FTU-A_tables/SPHTable.png
projects/Eastgate/data/raw/Stand_StockTables/FTU-A_tables/SPHPlot.png
projects/Eastgate/data/raw/Stand_StockTables/FTU-A_tables/SPH_Cut_Plot.png
projects/Eastgate/data/raw/Stand_StockTables/FTU-A_tables/VOLTable.png
projects/Eastgate/data/raw/Stand_StockTables/FTU-A_tables/VOL_Cut_Plot.png
projects/Eastgate/data/raw/Stand_StockTables/FTU-A_tables/BATable.png
projects/Eastgate/data/raw/Stand_StockTables/FTU-A_tables/BA_Cut_Plot.png
```

The app also writes a Step 0.5 treatment description summary under the project intermediate outputs.

### 9.4 Reviewing Outputs

Following successful completion, review the generated tables and plots to confirm that treatment summaries have been produced.

The process may partially generate outputs before failing if expected species columns are missing from overstory or understory tables.

Current application behavior is intended to populate missing species columns with zeros where required.

## 10. Step 1 - Process to FuelCalc

### 10.1 Purpose

Step 1 prepares FuelCalc input files for each treatment specified in **Treatments to process**.

This step serves as the interface between Fire Modeler and the external FuelCalc workflow.

### 10.2 Required Inputs

Before running Step 1, ensure that:

- Project Setup has completed successfully.
- Treatment tables have been reviewed and edited.
- Cutting Specifications have been generated.
- The desired treatments have been specified in **Treatments to process**.

### 10.3 Running Step 1

Run **Step 1 - Process to FuelCalc** after treatment preparation has been completed.

The application prepares FuelCalc input files for each selected treatment.

### 10.4 Expected FuelCalc Files

FuelCalc working files are generated beneath:

```text
projects/<project_name>/data/raw/FuelCalc/
```

For treatment `FTU-A`, expected outputs include:

```text
projects/Eastgate/data/raw/FuelCalc/Outputs/TU_FTU-A/
```

Example generated files include:

```text
TU_FTU-A_FuelCalc_FFI.fcp
TU_FTU-A_FuelCalc_FFI.ffi
TU_FTU-A_FuelCalc_FFI.tre
Run_FuelCalc_TU_FTU-A.bat
```

### 10.5 Output Verification

After Step 1 completes, verify that the expected FuelCalc working files have been created for each selected treatment.

Successful completion of Step 1 prepares the project for execution within FuelCalc.

## 11. Step 2 - Local Weather Analysis

### 11.1 Required Weather Inputs

Step 2 processes a raw weather station file and generates weather products used by Step 3.

The raw weather station CSV must be placed in:

```text
projects/<project_name>/data/raw/Weather/raw/<station_code>.csv
```

For example:

```text
projects/Eastgate/data/raw/Weather/raw/317.csv
```

The weather file should be available before running Step 2.

### 11.2 MOF vs EC Weather Sources

Step 2 supports both **MOF** and **EC** weather station types.

The MOF workflow is the verified processing path.

MOF weather files should contain hourly fire weather observations including fields such as:

- wind speed
- temperature
- wind direction
- relative humidity
- time
- precipitation

Some Environment Canada (EC) downloads may contain daily observations only and do not provide the hourly fields required by the current Step 2 workflow.

If only daily EC observations are available, additional preprocessing or application logic will likely be required.

For EC mode, the current script expects hourly fields similar to:

```text
wind_direction
total_precipitation
wind_speed
air_temperature
relative_humidity
time
```

### 11.3 Configuring Weather Settings

Step 2 requires the following weather settings:

- Weather station type.
- Weather station name.
- Weather station code.
- Latitude.
- Longitude.
- Danger region.

Users should verify that these settings match the selected weather station before processing.

### 11.4 Running Step 2

Run **Step 2 - Local Weather Analysis** after the required weather file has been placed in the project directory and the Step 2 weather settings have been configured.

Step 2 can be run before or after FuelCalc processing, but Step 3 requires both completed Step 2 weather outputs and completed FuelCalc output files.

The application processes the raw weather data and generates hourly weather products and supporting weather summaries for use in Step 3.

### 11.5 Generated Weather Products

Step 2 produces weather outputs including:

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

### 11.6 Output Verification

After Step 2 completes, verify that:

- The hourly weather file has been created.
- The weather list file has been generated.
- The expected weather summary figures have been produced.

These outputs are required for successful execution of Step 3.

## 12. Step 3 - Fire Behavior Modeling

### 12.1 Required Prerequisites

Before running Step 3, confirm that:

- Step 2 has completed successfully.
- FuelCalc has completed successfully.
- FuelCalc output files exist for every treatment listed in **Treatments to process**.

### 12.2 Required Weather Outputs

Step 3 requires the following weather products:

```text
data/raw/Weather/WeatherLists/allstations_90th_FWList_dates_summer.csv
data/raw/Weather/raw/<step3_weather_name>_Hourly_Weather.csv
```

The Step 3 weather name should match the weather station name used during Step 2 unless an alternate hourly weather file exists.

For example:

```text
Step 2 station: ALLISON PASS
Step 3 weather name: ALLISON PASS
```

### 12.3 Required FuelCalc Outputs

For each treatment listed in **Treatments to process**, Step 3 requires:

```text
data/raw/FuelCalc/Outputs/TU_<treatment>/TU_<treatment>_FuelCalc_FFI_Outputs.csv
```

All required FuelCalc output files should be verified before continuing.

### 12.4 Running Step 3

Run **Step 3 - Fire Behavior Modeling** after confirming that both weather processing and FuelCalc processing have completed successfully.

The application combines weather, FuelCalc, SNAP, treatment, and fuel configuration inputs to generate final fire behavior modeling outputs.

### 12.5 Reviewing Model Outputs

Upon completion, review the generated fire behavior outputs and confirm that processing completed without errors.

Primary Step 3 outputs are written under:

```text
projects/<project_name>/data/raw/FireBehavior/Outputs/
```

Expected outputs include:

```text
FireModelingResults.csv
FireModelingResults.rds
TreatmentSummaryTable.png
ProbabilityCrownFireBoxPlot.png
CrownProbWindSpeed.png
CrownProbFuelMoist.png
MedianHFIBarPlot.png
MedianROSBarPlot.png
FBP_CSISummaryTable.png
```

Step 3 also writes run summary artifacts under:

```text
projects/<project_name>/data/outputs/step3_fire_model/
```

## 13. Output Directory Structure

### 13.1 FuelCalc Outputs

FuelCalc working files and outputs are stored beneath:

```text
projects/<project_name>/data/raw/FuelCalc/
```

Treatment-specific outputs are stored within:

```text
projects/<project_name>/data/raw/FuelCalc/Outputs/TU_<treatment>/
```

### 13.2 Weather Outputs

Weather processing outputs are generated beneath:

```text
projects/<project_name>/data/raw/Weather/
```

This directory contains:

- Hourly weather files.
- Weather lists.
- Wind rose figures.
- Danger day figures.
- Weather distribution figures.

### 13.3 Treatment Description Outputs

Treatment Description outputs are generated within the treatment-specific folders beneath:

```text
projects/<project_name>/data/raw/Stand_StockTables/<treatment>_tables/
```

These include summary tables and figures describing treatment conditions.

### 13.4 Fire Behavior Outputs

Fire behavior modeling outputs are generated during Step 3 beneath:

```text
projects/<project_name>/data/raw/FireBehavior/Outputs/
```

The app also writes Step 3 summary artifacts beneath:

```text
projects/<project_name>/data/outputs/step3_fire_model/
```

### 13.5 Recommended File Review Locations

Before completing a project, users should verify:

- Treatment tables and cutting specifications within `data/raw/Stand_StockTables/`.
- FuelCalc output CSV files within `data/raw/FuelCalc/Outputs/`.
- Generated weather products within `data/raw/Weather/`.
- Final fire behavior outputs within `data/raw/FireBehavior/Outputs/`.
- Step status and manifest files within `data/outputs/`.

## 14. Quality Assurance Checklist

### 14.1 Verify SNAP Inputs

Before Project Setup:

- Confirm all required SNAP files are present.
- Confirm filenames match the project name.
- Confirm files are valid CSV exports.
- Confirm files are not open in Excel.

### 14.2 Verify Project Setup

Confirm that:

- Treatment/stratum folders have been created.
- Stock tables have been generated.
- Species columns have been populated correctly.

### 14.3 Verify Treatment Tables

Confirm that:

- Treatment tables have been reviewed.
- Required edits have been saved.
- Species columns appear appropriate for the treatment/stratum.

### 14.4 Verify Cutting Specifications

Confirm that:

- Cutting specification files exist.
- They reflect the current treatment table configuration.

### 14.5 Verify FuelCalc Completion

Confirm that:

- Step 1 completed successfully.
- FuelCalc was executed for each selected treatment.
- Final FuelCalc output CSV files exist.

### 14.6 Verify Weather Processing

Confirm that:

- Hourly weather files have been generated.
- Weather list files exist.
- Weather summary figures have been created.

### 14.7 Verify Step 3 Readiness

Before running Step 3, confirm that:

- Weather outputs are present.
- FuelCalc outputs are present.
- Step 3 weather name matches the generated hourly weather file.
- **Treatments to process** matches the available FuelCalc outputs.

### 14.8 Final Project Validation Checklist

Before considering the workflow complete, verify that:

- All required processing steps completed successfully.
- Expected output files have been generated.
- No application errors remain unresolved.

## 15. Troubleshooting Guide

### 15.1 Invalid SNAP CSV Files

#### Embedded Null Errors or Invalid Multibyte Strings

**Likely cause:**

An Excel workbook has been renamed with a `.csv` extension instead of being exported as a true CSV file.

**Resolution:**

Open the workbook in Excel and export or save it as a genuine CSV file before rerunning the workflow.

### 15.2 File Permission Errors

#### Permission Denied While Reading or Writing Files

**Likely cause:**

The CSV file is currently open in Excel or another application.

**Resolution:**

Close the file and rerun the processing step.

### 15.3 Project Setup Errors

#### `replacement has length zero`

**Likely cause:**

Required SNAP tree measurements such as height or crown base height are missing.

**Resolution:**

Review the affected inventory records, correct missing values, and rerun Project Setup.

### 15.4 Treatment Configuration Errors

#### Missing `A_tables` or `NA_tables`

**Likely cause:**

Treatment names do not match the generated treatment folders or downstream processing is attempting to process unintended treatments.

**Resolution:**

Confirm that **Treatments to process** contains the exact generated treatment names.

### 15.5 FuelCalc Errors

#### Missing FuelCalc Output Files

**Likely cause:**

FuelCalc has not been run successfully or downstream processing expects additional treatments.

**Resolution:**

Verify that FuelCalc completed successfully for each selected treatment and confirm that **Treatments to process** matches the intended treatment list.

### 15.6 Weather Processing Errors

#### Missing `wind_direction` Column

**Likely cause:**

The weather file contains daily observations rather than the required hourly fire weather fields.

**Resolution:**

Use an hourly weather dataset containing wind direction, wind speed, temperature, relative humidity, precipitation, and time.

Where available, use the verified MOF weather data path.

### 15.7 Step 3 Availability Issues

#### Step 3 Disabled or Greyed Out

**Likely cause:**

One or more required prerequisite files are missing.

**Resolution:**

Confirm that:

- `allstations_90th_FWList_dates_summer.csv` exists.
- The expected hourly weather file exists.
- FuelCalc output CSV files exist for every treatment listed in **Treatments to process**.
- The Step 3 weather name matches the Step 2 station name.

## Appendix A - Example Working Configuration

The following configuration was confirmed during testing:

| Setting | Value |
| --- | --- |
| Project | Eastgate |
| Treatment scope | FTU-A |
| Weather type | MOF |
| Weather station | ALLISON PASS |
| Weather station code | 317 |

For this configuration, the generated overstory species tables contained only species present in the source SNAP data for the treatment.

## Appendix B - Project Folder Reference

The standard project structure is:

```text
projects/<project_name>/
  data/
    raw/
      SNAP/
      Stand_StockTables/
      FuelCalc/
      Weather/
      FireBehavior/
    intermediate/
    outputs/
    external/
```

Key processing locations include:

| Processing Stage | Primary Location |
| --- | --- |
| SNAP inputs | `data/raw/SNAP/` |
| Treatment tables | `data/raw/Stand_StockTables/` |
| Cutting specifications | `data/raw/Stand_StockTables/<treatment>_tables/` |
| Treatment Description outputs | `data/raw/Stand_StockTables/<treatment>_tables/` |
| FuelCalc inputs and outputs | `data/raw/FuelCalc/` |
| Weather processing | `data/raw/Weather/` |
| Fire behavior outputs | `data/raw/FireBehavior/Outputs/` |
| Step summaries and run status | `data/outputs/` and `data/intermediate/` |
