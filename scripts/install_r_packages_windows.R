options(repos = c(
  firebehavioR = "https://cran.r-universe.dev",
  CRAN = "https://cloud.r-project.org"
))

project <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
r_minor <- paste(R.version$major, strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1], sep = ".")
platform_prefix <- if (getRversion() >= "4.4.0") "windows" else NULL
project_lib <- file.path(project, "renv", "library", platform_prefix, paste0("R-", r_minor), R.version$platform)

dir.create(project_lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(project_lib, .libPaths()))

renv_version <- tryCatch(as.character(utils::packageVersion("renv")), error = function(...) NA_character_)
if (!identical(renv_version, "1.1.7")) {
  message("Installing renv 1.1.7 into ", project_lib)
  utils::install.packages(
    "https://cran.r-project.org/src/contrib/Archive/renv/renv_1.1.7.tar.gz",
    lib = project_lib,
    repos = NULL,
    type = "source"
  )
}

critical_packages <- c(
  "jsonlite",
  "dplyr",
  "tidyr",
  "purrr",
  "tibble",
  "ggplot2",
  "flextable",
  "sf",
  "terra",
  "cffdrs",
  "reshape2",
  "glue",
  "ggdist",
  "firebehavioR",
  "progress",
  "stringr",
  "readxl",
  "openxlsx"
)

support_packages <- c(
  "plyr",
  "raster",
  "sp",
  "geosphere",
  "data.table",
  "zoo",
  "cubature",
  "lubridate",
  "matrixStats",
  "truncnorm",
  "gganimate",
  "gifski",
  "foreach",
  "doParallel",
  "gtools",
  "gdistance",
  "viridis",
  "ggtext",
  "patchwork",
  "openair",
  "GA",
  "ftsa",
  "Rothermel",
  "devtools",
  "stars",
  "geostats",
  "plantecophys",
  "ggridges",
  "MetBrewer",
  "DBI",
  "RSQLite",
  "rootSolve",
  "spatialEco",
  "solrad",
  "ncdf4",
  "lwgeom",
  "abind",
  "ecmwfr"
)

packages <- unique(c(critical_packages, support_packages))
installed <- rownames(utils::installed.packages(lib.loc = project_lib))
missing <- setdiff(packages, installed)

if (length(missing)) {
  available <- rownames(utils::available.packages())
  installable <- intersect(missing, available)
  unavailable <- setdiff(missing, available)

  if (length(installable)) {
    message("Installing R packages into ", project_lib)
    message(paste(installable, collapse = ", "))
    utils::install.packages(installable, lib = project_lib, type = "binary")
  }

  if (length(unavailable)) {
    warning("These packages were not available from CRAN binaries and were skipped: ", paste(unavailable, collapse = ", "))
  }
} else {
  message("R packages already installed.")
}

if (!requireNamespace("firebehavioR", quietly = TRUE)) {
  message("Installing firebehavioR from R-universe")
  utils::install.packages("firebehavioR", lib = project_lib, type = "source")
}

if (!requireNamespace("Rothermel", quietly = TRUE)) {
  message("Installing archived Rothermel package")
  try(
    utils::install.packages(
      "https://cran.r-project.org/src/contrib/Archive/Rothermel/Rothermel_1.2.tar.gz",
      lib = project_lib,
      repos = NULL,
      type = "source"
    ),
    silent = TRUE
  )
}

still_missing_critical <- critical_packages[!vapply(critical_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(still_missing_critical)) {
  stop("Missing required R packages after install: ", paste(still_missing_critical, collapse = ", "), call. = FALSE)
}

message("Required R packages are installed.")
