step2 <- function(cfg, root){
  cat("Running Step 2: FuelCalc\n")

  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

  in_dir <- file.path(root, cfg$outputs$step1)
  out_dir <- file.path(root, cfg$outputs$step2)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  snap_rds <- file.path(in_dir, "clean_snap.rds")
  if (!file.exists(snap_rds)) stop("Missing Step 1 output: ", snap_rds)

  dat <- readRDS(snap_rds)
  Snap_OS <- dat$Snap_OS
  Snap_US <- dat$Snap_US
  Snap_fuels <- dat$Snap_fuels

  # Filter known non-modeled strata.
  if ("Stratum" %in% names(Snap_OS) && "Stratum" %in% names(Snap_US)) {
    excluded <- c("Excluded", "all plots", "RESERVE", "unused")
    Snap_OS <- Snap_OS[!(Snap_OS$Stratum %in% excluded), , drop = FALSE]
    Snap_US <- Snap_US[!(Snap_US$Stratum %in% excluded), , drop = FALSE]
    Snap_OS$Stratum <- gsub("[/\\\\]", "-", Snap_OS$Stratum)
    Snap_US$Stratum <- gsub("[/\\\\]", "-", Snap_US$Stratum)
  }

  # Fill BAF gaps by stratum.
  fill_baf <- function(df, all_missing_default){
    if (!all(c("Stratum", "BAF") %in% names(df))) return(df)
    dplyr::bind_rows(lapply(split(df, df$Stratum), function(tmp){
      if (all(is.na(tmp$BAF))) {
        tmp$BAF <- all_missing_default
      } else {
        donor <- tmp$BAF[which(!is.na(tmp$BAF))[1]]
        tmp$BAF[is.na(tmp$BAF)] <- donor
      }
      tmp
    }))
  }
  Snap_OS <- fill_baf(Snap_OS, "NO BAF")
  Snap_US <- fill_baf(Snap_US, 0)

  # Replace missing fuel loads with sentinel used by legacy script.
  cols_99 <- c(
    "Avg.1.hr.fuels...0.6.cm...kg.m2.",
    "Avg.10.hr.fuels..0.6.2.5cm...kg.m2.",
    "Avg.100.hr.fuels..2.6.7.5.cm...kg.m2.",
    "X1000.hr.fuels...7.6.cm.kg.m2."
  )
  for (col in cols_99) {
    if (col %in% names(Snap_fuels)) Snap_fuels[[col]][is.na(Snap_fuels[[col]])] <- 99
  }

  # Per-stratum fuel averages used by downstream FuelCalc prep.
  avg <- dplyr::summarise(
    dplyr::group_by(Snap_fuels, Stratum),
    DuffDepth.cm = mean(.data[["Duff.Depth..cm."]], na.rm = TRUE),
    Grass.kg.m2 = mean(.data[["Avg.Grass.Loading..kg.m2."]], na.rm = TRUE),
    Shrub.kg.m2 = mean(.data[["Avg.Shrub.Loading..kg.m2."]], na.rm = TRUE),
    Hr1.kg.m2 = mean(.data[["Avg.1.hr.fuels...0.6.cm...kg.m2."]], na.rm = TRUE),
    Hr10.kg.m2 = mean(.data[["Avg.10.hr.fuels..0.6.2.5cm...kg.m2."]], na.rm = TRUE),
    Hr100.kg.m2 = mean(.data[["Avg.100.hr.fuels..2.6.7.5.cm...kg.m2."]], na.rm = TRUE),
    Hr1000.kg.m2 = mean(.data[["X1000.hr.fuels...7.6.cm.kg.m2."]], na.rm = TRUE),
    LWD.kg.m2 = mean(.data[["LWD.fuels..7.0.20.0cm...kg.m2."]], na.rm = TRUE),
    CWD.kg.m2 = mean(.data[["CWD.fuels...20.0cm...kg.m2."]], na.rm = TRUE),
    LWDPieces.ha = mean(.data[["LWD.pieces"]], na.rm = TRUE),
    .groups = "drop"
  )

  write.csv(Snap_OS, file.path(out_dir, "step2_snap_os.csv"), row.names = FALSE)
  write.csv(Snap_US, file.path(out_dir, "step2_snap_us.csv"), row.names = FALSE)
  write.csv(Snap_fuels, file.path(out_dir, "step2_snap_fuels.csv"), row.names = FALSE)
  write.csv(avg, file.path(out_dir, "fuel_class_averages_by_stratum.csv"), row.names = FALSE)

  saveRDS(list(Snap_OS = Snap_OS, Snap_US = Snap_US, Snap_fuels = Snap_fuels, fuel_averages = avg),
          file.path(out_dir, "fuelcalc_inputs.rds"))

  jsonlite::write_json(
    list(
      strata_count = if ("Stratum" %in% names(Snap_fuels)) length(unique(Snap_fuels$Stratum)) else NA_integer_,
      fuel_rows = nrow(Snap_fuels),
      avg_rows = nrow(avg)
    ),
    path = file.path(out_dir, "fuelcalc_summary.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )
}

