step025 <- function(cfg, root) {
  cat("Running Step 0.25: Cutting Specs\n")

  suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
  })

  `%||%` <- function(a, b) {
    if (is.null(a) || length(a) == 0) return(b)
    if (is.atomic(a) && length(a) == 1 && is.na(a)) return(b)
    a
  }

  coerce_numeric_columns <- function(df, id_cols) {
    value_cols <- setdiff(names(df), id_cols)
    df[value_cols] <- lapply(df[value_cols], function(x) {
      x <- as.character(x)
      x <- gsub("\u00A0", "", x, fixed = TRUE)
      x <- trimws(x)
      x[x %in% c("", "NA", "N/A", "na", "n/a", "NaN")] <- NA_character_
      values <- suppressWarnings(as.numeric(x))
      values[is.na(values)] <- 0
      values
    })
    df
  }

  restore_existing_cut_percentages <- function(cut_spec, out_path) {
    if (!file.exists(out_path)) {
      return(cut_spec)
    }

    existing <- read.csv(out_path, check.names = FALSE, stringsAsFactors = FALSE)
    names(existing) <- trimws(names(existing))
    if (!all(c("Stand.Layer", "DBH.Class") %in% names(existing))) {
      return(cut_spec)
    }

    pct_cols <- names(cut_spec)[endsWith(names(cut_spec), ".%")]
    pct_cols <- intersect(pct_cols, names(existing))
    if (!length(pct_cols)) {
      return(cut_spec)
    }

    existing_pct <- existing[, c("Stand.Layer", "DBH.Class", pct_cols), drop = FALSE]
    existing_pct[pct_cols] <- lapply(existing_pct[pct_cols], function(x) {
      values <- suppressWarnings(as.numeric(trimws(gsub("\u00A0", "", as.character(x), fixed = TRUE))))
      values[is.na(values)] <- 0
      values
    })

    merged <- dplyr::left_join(
      cut_spec[, c("Stand.Layer", "DBH.Class"), drop = FALSE],
      existing_pct,
      by = c("Stand.Layer", "DBH.Class")
    )

    for (col in pct_cols) {
      cut_spec[[col]] <- dplyr::coalesce(merged[[col]], cut_spec[[col]])
    }

    cut_spec
  }

  project_name <- cfg$project_name %||% ""
  if (!nzchar(project_name)) {
    stop("config project_name is required for Cutting Specs")
  }

  path <- cfg$runtime$raw_dir
  snap_fuels_path <- file.path(path, "SNAP", paste0(project_name, "_FUELS.csv"))
  if (!file.exists(snap_fuels_path)) {
    stop("Missing SNAP fuels file: ", snap_fuels_path)
  }
  Snap_fuels <- read.csv(snap_fuels_path, stringsAsFactors = FALSE)

  folder_paths <- file.path(path, "Stand_StockTables")
  treatments <- cfg$process_to_fuelcalc$tr_names %||% unique(Snap_fuels$Stratum)
  if (!length(treatments)) treatments <- unique(Snap_fuels$Stratum)
  if (!length(treatments)) stop("No treatments found for Cutting Specs")

  generated <- list()

  for (treatment in treatments) {
    treatment_folder <- file.path(folder_paths, paste0(treatment, "_tables"))
    required_paths <- c(
      OS.SPH = file.path(treatment_folder, "OS_SPH.csv"),
      US.SPH = file.path(treatment_folder, "US_SPH.csv")
    )

    missing <- required_paths[!file.exists(required_paths)]
    if (length(missing) > 0) {
      stop("Cutting Specs is missing required files for treatment ", treatment, ": ", paste(missing, collapse = ", "))
    }

    OS.SPH <- read.csv(required_paths[["OS.SPH"]], stringsAsFactors = FALSE)
    US.SPH <- read.csv(required_paths[["US.SPH"]], stringsAsFactors = FALSE)

    if (!("Layer" %in% names(US.SPH)) && "DBH.Class" %in% names(US.SPH)) {
      US.SPH <- US.SPH |> dplyr::rename(Layer = DBH.Class)
    }
    if ("Dead" %in% colnames(US.SPH)) {
      US.SPH <- US.SPH |> dplyr::rename(DP = Dead)
    }
    OS.SPH <- coerce_numeric_columns(OS.SPH, "DBH.Class")
    US.SPH <- coerce_numeric_columns(US.SPH, "Layer")

    SPHtemp <- data.frame(
      "Stand Layer" = c("L4", "L3", "L2", "L1", "L1", "L1", "L1", "L1", "L1"),
      "DBH Class" = c("0 - 1.5", "1.5 - 7.5", "7.5 - 12.5", "12.5 - 17.5", "17.5 - 22.5", "22.5 - 27.5", "27.5 - 35", "35 - 45", "45+"),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    US.SPH.mod <- US.SPH |>
      mutate(
        "Stand Layer" = case_when(
          Layer == "Layer 4 (<1.3m)" ~ "L4",
          Layer == "Layer 3 (2.5-7.49)" ~ "L3",
          Layer == "Layer 2 (7.5-12.49)" ~ "L2",
          Layer == "Layer 1 (12.5-17.5)" ~ "L1",
          TRUE ~ NA_character_
        ),
        "DBH Class" = case_when(
          Layer == "Layer 4 (<1.3m)" ~ "0 - 1.5",
          Layer == "Layer 3 (2.5-7.49)" ~ "1.5 - 7.5",
          Layer == "Layer 2 (7.5-12.49)" ~ "7.5 - 12.5",
          Layer == "Layer 1 (12.5-17.5)" ~ "12.5 - 17.5",
          TRUE ~ NA_character_
        )
      ) |>
      dplyr::select(-Layer) |>
      dplyr::select(`Stand Layer`, `DBH Class`, everything())

    OS.SPH.mod <- OS.SPH |>
      mutate(`Stand Layer` = "L1", `DBH Class` = DBH.Class) |>
      dplyr::select(-DBH.Class) |>
      dplyr::select(`Stand Layer`, `DBH Class`, everything())

    SPH.combined <- bind_rows(
      US.SPH.mod |> mutate(.source = 1),
      OS.SPH.mod |> mutate(.source = 2)
    ) |>
      mutate(across(where(is.numeric), ~ tidyr::replace_na(., 0))) |>
      mutate(.row_sum = rowSums(across(where(is.numeric)))) |>
      dplyr::group_by(`Stand Layer`, `DBH Class`) |>
      dplyr::arrange(desc(.row_sum), .by_group = TRUE) |>
      dplyr::slice(1) |>
      dplyr::ungroup() |>
      dplyr::arrange(factor(`Stand Layer`, levels = c("L4", "L3", "L2", "L1")), .source) |>
      dplyr::select(-.row_sum, -.source)

    SPH <- left_join(SPHtemp, SPH.combined, by = c("Stand Layer", "DBH Class"))
    species_cols <- setdiff(names(SPH), c("Stand Layer", "DBH Class"))
    if (!length(species_cols)) {
      stop("No species columns found in cutting specs inputs for treatment ", treatment)
    }
    SPH <- coerce_numeric_columns(SPH, c("Stand Layer", "DBH Class"))
    desc_cols <- c("Stand Layer", "DBH Class")
    spp_cols <- names(SPH)[!names(SPH) %in% desc_cols]
    interleaved <- as.vector(rbind(spp_cols, paste0(spp_cols, ".%")))
    SPH[paste0(spp_cols, ".%")] <- 0
    cutspec_out_mod <- SPH[, c(desc_cols, interleaved)]
    names(cutspec_out_mod) <- gsub("^Stand Layer$", "Stand.Layer", names(cutspec_out_mod))
    names(cutspec_out_mod) <- gsub("^DBH Class$", "DBH.Class", names(cutspec_out_mod))

    out_path <- file.path(treatment_folder, paste0("cuttingSpecs_", treatment, ".csv"))
    cutspec_out_mod <- restore_existing_cut_percentages(cutspec_out_mod, out_path)
    write.csv(cutspec_out_mod, out_path, row.names = FALSE)
    generated[[treatment]] <- out_path
  }

  out_dir <- file.path(cfg$runtime$intermediate_dir, "step025_cuttingspecs")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  summary <- list(
    project_name = project_name,
    stand_stocktables_dir = folder_paths,
    treatment_count = length(treatments),
    treatments = as.list(treatments),
    generated_outputs = generated
  )
  jsonlite::write_json(
    summary,
    path = file.path(out_dir, "cuttingspecs_summary.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )
  saveRDS(summary, file.path(out_dir, "cuttingspecs_outputs.rds"))
}
