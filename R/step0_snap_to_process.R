step0 <- function(cfg, root) {
  cat("Running Step 0: SNAP To Process\n")

  `%||%` <- function(a, b) {
    if (is.null(a) || length(a) == 0) return(b)
    if (is.atomic(a) && length(a) == 1 && is.na(a)) return(b)
    a
  }

  name <- cfg$project_name %||% ""
  if (!nzchar(name)) {
    stop("config project_name is required for Step 0")
  }

  path <- cfg$runtime$raw_dir
  snap_dir <- file.path(path, "SNAP")
  if (!dir.exists(snap_dir)) {
    stop("Missing SNAP directory: ", snap_dir)
  }

  snap_prefix <- "/SNAP/"
  s_s_prefix <- "/Stand_StockTables/"

  read_required <- function(suffixes) {
    candidates <- file.path(path, paste0("SNAP/", name, suffixes))
    hit <- candidates[file.exists(candidates)][1]
    if (is.na(hit)) {
      stop("Missing required SNAP file for project_name=", name, ": tried ", paste(candidates, collapse = ", "))
    }
    read.csv(hit, stringsAsFactors = FALSE)
  }

  Snap_OS <- read_required(c("_OS.csv"))
  Snap_US <- read_required(c("_US.csv"))
  Snap_EX <- read_required(c("_EX.csv", "_EXTRA.csv"))
  Snap_fuels <- read_required(c("_FUELS.csv"))

  folder_paths <- file.path(path, "Stand_StockTables")
  dir.create(folder_paths, recursive = TRUE, showWarnings = FALSE)

  treatments <- unique(Snap_EX$Stratum)

  # Preset template files per Cathro CSV from legacy script.
  # OSTEMP <- data.frame(
  #   DBH.Class = c("12.5 - 17.5", "17.5 - 22.5", "22.5 - 27.5", "27.5 - 35", "35 - 45", "45+"),
  #   Total = c(rep(0, 6))
  # )
  # USTEMP <- data.frame(
  #   Layer = c("Layer 4 (<1.3m)", "Layer 3 (2.5-7.49)", "Layer 2 (7.5-12.49)", "Layer 1 (12.5-17.5)"),
  #   Total = c(rep(0, 4))
  # )
  # cut_spec <- data.frame(
  #   Stand.Layer = c("L4", "L3", "L2", "L1", "L1", "L1", "L1", "L1", "L1"),
  #   DBH.Class = c("0 - 1.5", "1.5 - 7.5", "7.5 - 12.5", "12.5 - 17.5", "17.5 - 22.5", "22.5 - 27.5", "27.5 - 35", "35 - 45", "45+")
  # )

  for (treatment in treatments) {
    treatment_folder <- paste0(folder_paths, "/", treatment, "_tables")
    if (!dir.exists(treatment_folder)) {
      dir.create(treatment_folder, recursive = TRUE)
    }

    # Legacy optional exports kept here for future reuse.
    # index <- which(treatments == treatment)
    #
    # if (!L1_OS[index]) {
    #   OSOUT <- OSTEMP[-1, ]
    #   USOUT <- USTEMP
    # } else {
    #   OSOUT <- OSTEMP
    #   USOUT <- USTEMP
    # }
    #
    # write.csv(OSOUT, paste0(treatment_folder, "/OS_SPH.csv"), row.names = FALSE)
    # write.csv(USOUT, paste0(treatment_folder, "/US_SPH.csv"), row.names = FALSE)
    # write.csv(OSOUT, paste0(treatment_folder, "/OS_BA.csv"), row.names = FALSE)
    # write.csv(USOUT, paste0(treatment_folder, "/US_Ht_CBH.csv"), row.names = FALSE)
    # write.csv(OSOUT, paste0(treatment_folder, "/OS_Vol.csv"), row.names = FALSE)
    # write.csv(OSOUT, paste0(treatment_folder, "/OS_Ht_cbh.csv"), row.names = FALSE)
    # write.csv(cut_spec, paste0(treatment_folder, "/cuttingSpecs_", treatment, ".csv"), row.names = FALSE)
  }

  summary <- list(
    project_name = name,
    project_root = cfg$runtime$project_root,
    snap_dir = snap_dir,
    stand_stock_tables_dir = file.path(path, "Stand_StockTables"),
    treatment_count = length(treatments),
    treatments = as.list(treatments),
    os_rows = nrow(Snap_OS),
    us_rows = nrow(Snap_US),
    ex_rows = nrow(Snap_EX),
    fuels_rows = nrow(Snap_fuels)
  )

  out_dir <- file.path(cfg$runtime$intermediate_dir, "step0_snap_to_process")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    summary,
    path = file.path(out_dir, "snap_to_process_summary.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )
}
