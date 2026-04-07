library(jsonlite)

cat("FireModel pipeline starting...\n")
args <- commandArgs(trailingOnly = TRUE)

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

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) return(b)
  if (is.atomic(a) && length(a) == 1 && is.na(a)) return(b)
  a
}
sanitize_data_rel <- function(x) {
  x <- x %||% ""
  x <- gsub("\\\\", "/", x)
  sub("^data/", "", x)
}

cfg$project_name <- cfg$project_name %||% ""
if (!nzchar(cfg$project_name)) {
  stop("config project_name is required")
}

cfg$outputs <- cfg$outputs %||% list()
cfg$outputs$step1 <- sanitize_data_rel(cfg$outputs$step1 %||% "intermediate/step1_process_to_fuelcalc")
cfg$outputs$step2 <- sanitize_data_rel(cfg$outputs$step2 %||% "intermediate/step2_fuelcalc")
cfg$outputs$step3 <- sanitize_data_rel(cfg$outputs$step3 %||% "outputs/step3_fire_model")
cfg$outputs$reports <- sanitize_data_rel(cfg$outputs$reports %||% "outputs/reports")

cfg$paths <- cfg$paths %||% list()
for (nm in names(cfg$paths)) {
  cfg$paths[[nm]] <- sanitize_data_rel(cfg$paths[[nm]])
}

project_root <- file.path(root, "projects", cfg$project_name)
data_root <- file.path(project_root, "data")
cfg$runtime <- list(
  project_root = project_root,
  data_root = data_root,
  raw_dir = file.path(data_root, "raw"),
  intermediate_dir = file.path(data_root, "intermediate"),
  outputs_dir = file.path(data_root, "outputs"),
  external_dir = file.path(data_root, "external"),
  templates_dir = file.path(root, "templates"),
  status_dir = file.path(data_root, "outputs", "run_status"),
  manifest_dir = file.path(data_root, "outputs", "manifest")
)

dir.create(cfg$runtime$raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cfg$runtime$intermediate_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cfg$runtime$outputs_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cfg$runtime$external_dir, recursive = TRUE, showWarnings = FALSE)

cat("Project:", cfg$project_name, "\n")

source(file.path(root, "R", "step0_snap_to_process.R"))
source(file.path(root, "R", "step1_process_to_fuelcalc.R"))
source(file.path(root, "R", "step2_fuelcalc.R"))
source(file.path(root, "R", "step3_fire_model.R"))

requested_steps <- NULL
step_arg <- grep("^--steps=", args, value = TRUE)
if (length(step_arg) > 0) {
  requested_steps <- strsplit(sub("^--steps=", "", step_arg[[1]]), ",")[[1]]
  requested_steps <- requested_steps[nzchar(requested_steps)]
}

available_steps <- list(
  step0_snap_to_process = step0,
  step1_process_to_fuelcalc = step1,
  step2_fuelcalc = step2,
  step3_fire_model = step3
)

steps_to_run <- if (is.null(requested_steps) || length(requested_steps) == 0) {
  names(available_steps)
} else {
  unknown <- setdiff(requested_steps, names(available_steps))
  if (length(unknown) > 0) {
    stop("Unknown step name(s): ", paste(unknown, collapse = ", "))
  }
  requested_steps
}

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
    for (step_name in steps_to_run) {
      run_step(step_name, available_steps[[step_name]])
    }
    status$success <- TRUE
  },
  error = function(e) {
    err <<- e
  }
)

status$completed_at <- as.character(Sys.time())

status_dir <- cfg$runtime$status_dir
dir.create(status_dir, recursive = TRUE, showWarnings = FALSE)
write_json(
  status,
  path = file.path(status_dir, "pipeline_status.json"),
  pretty = TRUE,
  auto_unbox = TRUE,
  null = "null"
)

# Build a UI-friendly manifest of the latest pipeline artifacts.
manifest_dir <- cfg$runtime$manifest_dir
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
  project_root = cfg$runtime$project_root,
  generated_at = as.character(Sys.time()),
  run_status = artifact(file.path(status_dir, "pipeline_status.json")),
  step0 = list(
    summary = artifact(file.path(cfg$runtime$intermediate_dir, "step0_snap_to_process", "snap_to_process_summary.json"))
  ),
  step1 = list(
    summary = artifact(file.path(cfg$runtime$intermediate_dir, "step1_process_to_fuelcalc", "process_to_fuelcalc_summary.json")),
    outputs_rds = artifact(file.path(cfg$runtime$intermediate_dir, "step1_process_to_fuelcalc", "process_to_fuelcalc_outputs.rds")),
    fuelcalc_dir = artifact(file.path(cfg$runtime$raw_dir, "FuelCalc")),
    fuelcalcbc_dir = artifact(file.path(cfg$runtime$raw_dir, "FuelCalcBC"))
  ),
  step2 = list(
    summary = artifact(file.path(cfg$runtime$intermediate_dir, "step2_fuelcalc", "fuelcalc_to_firemodel_summary.json")),
    outputs_rds = artifact(file.path(cfg$runtime$intermediate_dir, "step2_fuelcalc", "fuelcalc_to_firemodel_outputs.rds")),
    wind_rose = artifact(file.path(cfg$runtime$raw_dir, "Weather", paste0("WindRoses/", (cfg$fuelcalc_to_firemodel$weather_name %||% ""), "_WindRoses.jpg"))),
    danger_days = artifact(file.path(cfg$runtime$raw_dir, "Weather", paste0("DangerDays/", (cfg$fuelcalc_to_firemodel$weather_name %||% ""), "_DangerDays.jpg"))),
    weather_conditions = artifact(file.path(cfg$runtime$raw_dir, "Weather", paste0("WeatherConditions/", (cfg$fuelcalc_to_firemodel$weather_name %||% ""), "_WeatherDistributions.jpg")))
  ),
  step3 = list(
    summary = artifact(file.path(cfg$runtime$outputs_dir, "step3_fire_model", "firemodel_results_summary.json")),
    outputs_rds = artifact(file.path(cfg$runtime$outputs_dir, "step3_fire_model", "firemodel_results_outputs.rds")),
    treatment_summary = artifact(file.path(cfg$runtime$raw_dir, "FireBehavior", "Outputs", "TreatmentSummaryTable.png")),
    crown_fire_probability_boxplots = artifact(file.path(cfg$runtime$raw_dir, "FireBehavior", "Outputs", "ProbabilityCrownFireBoxPlot.png")),
    crowning_index_windspeed = artifact(file.path(cfg$runtime$raw_dir, "FireBehavior", "Outputs", "CrownProbWindSpeed.png")),
    crowning_index_fuelmoist = artifact(file.path(cfg$runtime$raw_dir, "FireBehavior", "Outputs", "CrownProbFuelMoist.png")),
    head_fire_intensity = artifact(file.path(cfg$runtime$raw_dir, "FireBehavior", "Outputs", "MedianHFIBarPlot.png")),
    rate_of_spread = artifact(file.path(cfg$runtime$raw_dir, "FireBehavior", "Outputs", "MedianROSBarPlot.png")),
    fbp_90th_csi_stand = artifact(file.path(cfg$runtime$raw_dir, "FireBehavior", "Outputs", "FBP_CSISummaryTable.png"))
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
  cat("Pipeline failed. See ", file.path("projects", cfg$project_name, "data", "outputs", "run_status", "pipeline_status.json"), "\n", sep = "")
  stop(err)
}

cat("Pipeline complete.\n")
cat("Status written to ", file.path("projects", cfg$project_name, "data", "outputs", "run_status", "pipeline_status.json"), "\n", sep = "")
cat("Manifest written to ", file.path("projects", cfg$project_name, "data", "outputs", "manifest", "pipeline_manifest.json"), "\n", sep = "")
