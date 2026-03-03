library(jsonlite)

cat("FireModel pipeline starting...\n")

root <- normalizePath(getwd())
config_path <- file.path(root, "config", "config.json")
config_example_path <- file.path(root, "config", "config.example.json")

if (!file.exists(config_path) && file.exists(config_example_path)) {
  cfg <- fromJSON(config_example_path)
  cat("Using config/config.example.json (config.json not found)\n")
} else if (file.exists(config_path)) {
  cfg <- fromJSON(config_path)
} else {
  stop("Missing config/config.json and config/config.example.json")
}

cat("Project:", cfg$project_name, "\n")

source(file.path(root, "R", "step1_clean_snap.R"))
source(file.path(root, "R", "step2_fuelcalc.R"))
source(file.path(root, "R", "step3_fire_model.R"))

run_started <- Sys.time()
status <- list(
  project = cfg$project_name,
  started_at = as.character(run_started),
  completed_at = NA_character_,
  success = FALSE,
  steps = list()
)

run_step <- function(step_name, fn) {
  step_start <- Sys.time()
  status$steps[[step_name]] <<- list(
    started_at = as.character(step_start),
    completed_at = NA_character_,
    success = FALSE,
    error = NA_character_
  )

  tryCatch(
    {
      fn(cfg, root)
      status$steps[[step_name]]$completed_at <<- as.character(Sys.time())
      status$steps[[step_name]]$success <<- TRUE
    },
    error = function(e) {
      status$steps[[step_name]]$completed_at <<- as.character(Sys.time())
      status$steps[[step_name]]$error <<- conditionMessage(e)
      stop(e)
    }
  )
}

err <- NULL
tryCatch(
  {
    run_step("step1_clean_snap", step1)
    run_step("step2_fuelcalc", step2)
    run_step("step3_fire_model", step3)
    status$success <- TRUE
  },
  error = function(e) {
    err <<- e
  }
)

status$completed_at <- as.character(Sys.time())

status_dir <- file.path(root, "data", "outputs", "run_status")
dir.create(status_dir, recursive = TRUE, showWarnings = FALSE)
write_json(
  status,
  path = file.path(status_dir, "pipeline_status.json"),
  pretty = TRUE,
  auto_unbox = TRUE,
  null = "null"
)

# Build a UI-friendly manifest of the latest pipeline artifacts.
manifest_dir <- file.path(root, "data", "outputs", "manifest")
dir.create(manifest_dir, recursive = TRUE, showWarnings = FALSE)

artifact <- function(path) {
  list(
    path = path,
    exists = file.exists(path),
    size_bytes = if (file.exists(path)) as.integer(file.info(path)$size) else NA_integer_
  )
}

manifest <- list(
  project = cfg$project_name,
  generated_at = as.character(Sys.time()),
  run_status = artifact(file.path(status_dir, "pipeline_status.json")),
  step1 = list(
    summary = artifact(file.path(root, "data", "intermediate", "step1_clean_snap", "snap_summary.json")),
    os_csv = artifact(file.path(root, "data", "intermediate", "step1_clean_snap", "clean_snap_os.csv")),
    us_csv = artifact(file.path(root, "data", "intermediate", "step1_clean_snap", "clean_snap_us.csv")),
    fuels_csv = artifact(file.path(root, "data", "intermediate", "step1_clean_snap", "clean_snap_fuels.csv"))
  ),
  step2 = list(
    summary = artifact(file.path(root, "data", "intermediate", "step2_fuelcalc", "fuelcalc_summary.json")),
    fuel_averages = artifact(file.path(root, "data", "intermediate", "step2_fuelcalc", "fuel_class_averages_by_stratum.csv")),
    inputs_rds = artifact(file.path(root, "data", "intermediate", "step2_fuelcalc", "fuelcalc_inputs.rds"))
  ),
  step3 = list(
    run_summary = artifact(file.path(root, "data", "outputs", "step3_fire_model", "fire_model_run.json")),
    stand_structure = artifact(file.path(root, "data", "outputs", "step3_fire_model", "All_Treatments_StandStructure.csv")),
    slash_residuals = artifact(file.path(root, "data", "outputs", "step3_fire_model", "SlashResiduals", "All_Treatments_SlashResiduals.csv")),
    weather_stage = artifact(file.path(root, "data", "outputs", "step3_fire_model", "WeatherStage", "weather_stage_summary.json")),
    fire_behavior_stage = artifact(file.path(root, "data", "outputs", "step3_fire_model", "FireBehaviorStage", "fire_behavior_stage_summary.json")),
    fire_behavior_run = artifact(file.path(root, "data", "outputs", "step3_fire_model", "FireBehaviorRun", "fire_behavior_run_stage_summary.json"))
  )
)

write_json(
  manifest,
  path = file.path(manifest_dir, "pipeline_manifest.json"),
  pretty = TRUE,
  auto_unbox = TRUE,
  null = "null"
)

if (!is.null(err)) {
  cat("Pipeline failed. See data/outputs/run_status/pipeline_status.json\n")
  stop(err)
}

cat("Pipeline complete.\n")
cat("Status written to data/outputs/run_status/pipeline_status.json\n")
cat("Manifest written to data/outputs/manifest/pipeline_manifest.json\n")
