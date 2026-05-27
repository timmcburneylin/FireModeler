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

project_library_package_version <- function(package) {
  desc <- file.path(project_lib, package, "DESCRIPTION")
  if (!file.exists(desc)) {
    return(NA_character_)
  }
  as.character(utils::packageDescription(package, lib.loc = project_lib)$Version)
}

renv_version <- project_library_package_version("renv")
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
  "cli",
  "dplyr",
  "generics",
  "lifecycle",
  "magrittr",
  "pillar",
  "pkgconfig",
  "rlang",
  "tidyr",
  "tidyselect",
  "purrr",
  "tibble",
  "utf8",
  "vctrs",
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

dependency_packages <- c(
  "askpass",
  "base64enc",
  "bslib",
  "cachem",
  "cpp11",
  "curl",
  "data.table",
  "digest",
  "evaluate",
  "fansi",
  "farver",
  "fastmap",
  "fontawesome",
  "fs",
  "gdtools",
  "htmltools",
  "isoband",
  "jquerylib",
  "knitr",
  "memoise",
  "mime",
  "openssl",
  "R6",
  "ragg",
  "Rcpp",
  "rmarkdown",
  "sass",
  "scales",
  "stringi",
  "systemfonts",
  "textshaping",
  "tinytex",
  "viridisLite",
  "withr",
  "xfun",
  "xml2",
  "yaml",
  "zip"
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

package_loadable <- function(package) {
  system.file(package = package, lib.loc = project_lib) != "" &&
    suppressWarnings(require(package, character.only = TRUE, lib.loc = project_lib, quietly = TRUE))
}

available_packages <- utils::available.packages()
seed_packages <- unique(c(critical_packages, dependency_packages, support_packages))
recursive_dependencies <- unlist(
  utils::package_dependencies(
    seed_packages,
    db = available_packages,
    which = c("Depends", "Imports", "LinkingTo"),
    recursive = TRUE
  ),
  use.names = FALSE
)
base_packages <- rownames(utils::installed.packages(priority = "base"))
packages <- unique(c(seed_packages, recursive_dependencies))
packages <- setdiff(packages, base_packages)
missing <- packages[!vapply(packages, package_loadable, logical(1))]

if (length(missing)) {
  available <- rownames(available_packages)
  installable <- intersect(missing, available)
  unavailable <- setdiff(missing, available)

  if (length(installable)) {
    message("Installing R packages into ", project_lib)
    message(paste(installable, collapse = ", "))
    utils::install.packages(
      installable,
      lib = project_lib,
      type = "binary",
      dependencies = c("Depends", "Imports", "LinkingTo")
    )
  }

  if (length(unavailable)) {
    warning("These packages were not available from CRAN binaries and were skipped: ", paste(unavailable, collapse = ", "))
  }
} else {
  message("R packages already installed in ", project_lib)
}

still_missing <- packages[!vapply(packages, package_loadable, logical(1))]
retry_packages <- setdiff(still_missing, c("firebehavioR", "Rothermel"))
if (length(retry_packages)) {
  message("Retrying packages that are installed incompletely or missing dependencies")
  message(paste(retry_packages, collapse = ", "))
  utils::install.packages(
    retry_packages,
    lib = project_lib,
    type = "binary",
    dependencies = c("Depends", "Imports", "LinkingTo")
  )
}

if (!package_loadable("firebehavioR")) {
  message("Installing firebehavioR from R-universe")
  utils::install.packages("firebehavioR", lib = project_lib, type = "source")
}

if (!package_loadable("Rothermel")) {
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

still_missing_critical <- critical_packages[!vapply(critical_packages, package_loadable, logical(1))]
if (length(still_missing_critical)) {
  stop("Missing required R packages after install: ", paste(still_missing_critical, collapse = ", "), call. = FALSE)
}

message("Required R packages are installed.")
