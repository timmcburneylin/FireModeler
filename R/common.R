# Common setup for FireModel scripts
root <- normalizePath(getwd())

`%||%` <- function(a,b) if (is.null(a)) b else a

cfg_path <- file.path(root, "config", "config.json")
cfg <- if (file.exists(cfg_path)) jsonlite::fromJSON(cfg_path) else list()

# convenience dirs
dir_data_raw   <- file.path(root, "data", "raw")
dir_data_out   <- file.path(root, "data", "outputs")
dir_templates  <- file.path(root, "templates")

dir.create(dir_data_out, recursive = TRUE, showWarnings = FALSE)

# --- portable function loader ---
source_fun <- function(fname) {
  f <- file.path(root, "R_functions", fname)
  if (!file.exists(f)) stop("Missing R_functions file: ", f)
  source(f, local = TRUE)
}

