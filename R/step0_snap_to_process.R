step0 <- function(cfg, root) {
  cat("Running Step 0: SNAP To Process\n")

  suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
    library(purrr)
    library(tibble)
  })

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

  # Clean strata names and remove clearly excluded strata before downstream steps.
  removable_strata <- c("Excluded", "all plots", "RESERVE", "unused")
  Snap_OS <- Snap_OS[!(Snap_OS$Stratum %in% removable_strata), , drop = FALSE]
  Snap_US <- Snap_US[!(Snap_US$Stratum %in% removable_strata), , drop = FALSE]
  if ("Stratum" %in% names(Snap_fuels)) {
    Snap_fuels <- Snap_fuels[!(Snap_fuels$Stratum %in% removable_strata), , drop = FALSE]
  }
  if ("Stratum" %in% names(Snap_EX)) {
    Snap_EX <- Snap_EX[!(Snap_EX$Stratum %in% removable_strata), , drop = FALSE]
  }

  clean_stratum <- function(x) gsub("/", "-", gsub("\\\\", "-", x))
  if ("Stratum" %in% names(Snap_OS)) Snap_OS$Stratum <- clean_stratum(Snap_OS$Stratum)
  if ("Stratum" %in% names(Snap_US)) Snap_US$Stratum <- clean_stratum(Snap_US$Stratum)
  if ("Stratum" %in% names(Snap_fuels)) Snap_fuels$Stratum <- clean_stratum(Snap_fuels$Stratum)
  if ("Stratum" %in% names(Snap_EX)) Snap_EX$Stratum <- clean_stratum(Snap_EX$Stratum)

  # Fill BAF values within each stratum.
  if (all(c("Stratum", "BAF") %in% names(Snap_OS))) {
    for (stratum in unique(Snap_OS$Stratum)) {
      idx <- which(Snap_OS$Stratum == stratum)
      tmp <- Snap_OS[idx, , drop = FALSE]
      if (all(is.na(tmp$BAF))) {
        tmp$BAF <- "NO BAF"
      } else {
        fill_val <- tmp$BAF[which(!is.na(tmp$BAF))[1]]
        tmp$BAF[is.na(tmp$BAF)] <- fill_val
      }
      Snap_OS[idx, ] <- tmp
    }
  }
  if (all(c("Stratum", "BAF") %in% names(Snap_US))) {
    for (stratum in unique(Snap_US$Stratum)) {
      idx <- which(Snap_US$Stratum == stratum)
      tmp <- Snap_US[idx, , drop = FALSE]
      if (all(is.na(tmp$BAF))) {
        tmp$BAF <- 0
      } else {
        fill_val <- tmp$BAF[which(!is.na(tmp$BAF))[1]]
        tmp$BAF[is.na(tmp$BAF)] <- fill_val
      }
      Snap_US[idx, ] <- tmp
    }
  }

  # Fill missing fuels with per-stratum averages.
  fuel_cols <- c(
    "Avg.1.hr.fuels...0.6.cm...kg.m2.",
    "Avg.10.hr.fuels..0.6.2.5cm...kg.m2.",
    "Avg.100.hr.fuels..2.6.7.5.cm...kg.m2.",
    "X1000.hr.fuels...7.6.cm.kg.m2."
  )
  if (all(c("Stratum", fuel_cols) %in% names(Snap_fuels))) {
    for (stratum in unique(Snap_fuels$Stratum)) {
      idx <- which(Snap_fuels$Stratum == stratum)
      tmp <- Snap_fuels[idx, , drop = FALSE]
      for (fuel_col in fuel_cols) {
        mean_val <- mean(tmp[[fuel_col]], na.rm = TRUE)
        tmp[[fuel_col]][is.na(tmp[[fuel_col]])] <- mean_val
      }
      Snap_fuels[idx, ] <- tmp
    }
  }

  # Fill understory DBH / CBH defaults by layer.
  if (all(c("Layer", "DBH", "CBH") %in% names(Snap_US))) {
    for (i in seq_len(nrow(Snap_US))) {
      if (Snap_US$Layer[i] == "Layer 1 (12.5-17.5)") {
        Snap_US$DBH[i] <- 15
        if (is.na(Snap_US$CBH[i])) Snap_US$CBH[i] <- 4
      } else if (Snap_US$Layer[i] == "Layer 2 (7.5-12.49)") {
        Snap_US$DBH[i] <- 10
        if (is.na(Snap_US$CBH[i])) Snap_US$CBH[i] <- 3
      } else if (Snap_US$Layer[i] == "Layer 3 (2.5-7.49)") {
        Snap_US$DBH[i] <- 5
        if (is.na(Snap_US$CBH[i])) Snap_US$CBH[i] <- 1
      } else if (Snap_US$Layer[i] == "Layer 4 (<1.3m)") {
        Snap_US$DBH[i] <- 1.5
        if (is.na(Snap_US$CBH[i])) Snap_US$CBH[i] <- 0
      }
    }
  }

  repair_understory_heights <- function(df) {
    height_col <- "Height..0.1m."
    required <- c("Stratum", "Layer", "SPP", height_col)
    if (!all(required %in% names(df))) return(df)

    df[[height_col]] <- suppressWarnings(as.numeric(df[[height_col]]))
    df[[height_col]][!is.finite(df[[height_col]]) | df[[height_col]] <= 0] <- NA_real_
    layer_defaults <- c(
      "Layer 4 (<1.3m)" = 1,
      "Layer 3 (2.5-7.49)" = 5,
      "Layer 2 (7.5-12.49)" = 10,
      "Layer 1 (12.5-17.5)" = 15
    )

    missing_rows <- which(is.na(df[[height_col]]))
    for (row in missing_rows) {
      same_species <- !is.na(df$Stratum) & !is.na(df$Layer) & !is.na(df$SPP) &
        !is.na(df$Stratum[row]) & !is.na(df$Layer[row]) & !is.na(df$SPP[row]) &
        df$Stratum == df$Stratum[row] &
        df$Layer == df$Layer[row] &
        df$SPP == df$SPP[row] &
        !is.na(df[[height_col]])
      donors <- df[[height_col]][same_species]

      if (!length(donors)) {
        same_layer <- !is.na(df$Stratum) & !is.na(df$Layer) &
          !is.na(df$Stratum[row]) & !is.na(df$Layer[row]) &
          df$Stratum == df$Stratum[row] &
          df$Layer == df$Layer[row] &
          !is.na(df[[height_col]])
        donors <- df[[height_col]][same_layer]
      }

      if (length(donors)) {
        df[[height_col]][row] <- stats::median(donors, na.rm = TRUE)
      } else {
        df[[height_col]][row] <- unname(layer_defaults[df$Layer[row]])
      }
    }

    unresolved <- which(is.na(df[[height_col]]) | df[[height_col]] <= 0)
    if (length(unresolved)) {
      stop(
        "Unable to repair non-positive understory heights at rows: ",
        paste(unresolved, collapse = ", ")
      )
    }
    df
  }

  repair_overstory_heights <- function(df) {
    height_col <- "Total.Height..m."
    required <- c("Stratum", "Spp", height_col)
    if (!all(required %in% names(df))) return(df)

    df[[height_col]] <- suppressWarnings(as.numeric(df[[height_col]]))
    df[[height_col]][!is.finite(df[[height_col]]) | df[[height_col]] <= 0] <- NA_real_
    missing_rows <- which(is.na(df[[height_col]]))

    for (row in missing_rows) {
      same_species <- !is.na(df$Stratum) & !is.na(df$Spp) &
        !is.na(df$Stratum[row]) & !is.na(df$Spp[row]) &
        df$Stratum == df$Stratum[row] &
        df$Spp == df$Spp[row] &
        !is.na(df[[height_col]])
      donors <- df[[height_col]][same_species]

      if (!length(donors)) {
        same_stratum <- !is.na(df$Stratum) & !is.na(df$Stratum[row]) &
          df$Stratum == df$Stratum[row] & !is.na(df[[height_col]])
        donors <- df[[height_col]][same_stratum]
      }

      if (length(donors)) {
        df[[height_col]][row] <- stats::median(donors, na.rm = TRUE)
      }
    }

    unresolved <- which(is.na(df[[height_col]]) | df[[height_col]] <= 0)
    if (length(unresolved)) {
      details <- paste0(
        "plot ", df$Plot..[unresolved], " / species ", df$Spp[unresolved]
      )
      stop(
        "Unable to repair non-positive overstory heights: ",
        paste(details, collapse = "; ")
      )
    }
    df
  }

  Snap_US <- repair_understory_heights(Snap_US)
  Snap_OS <- repair_overstory_heights(Snap_OS)

  # Normalize understory species codes.
  if ("SPP" %in% names(Snap_US)) {
    valid_species <- c("Ba","Bl","Bg","Bb","Cw","Fd","Hw","T","Pl","Sx","Sb","Sw","Lw","Lt","Pw","Yc","Fdi","Fdc","Py","Pli","Act","Acb","Ac","At","Ep","DP","DU","Dead","Dr","D","Mb","Hm","Ax","Bp","Dg","La","Pa","Pf","Pj","Plc","Pli","Pw","Sn","Ss")
    for (i in seq_len(nrow(Snap_US))) {
      current_species <- Snap_US$SPP[i]
      if (is.na(current_species) || current_species == "") {
        Snap_US$SPP[i] <- "D"
        next
      }
      current_species <- trimws(current_species)
      if (!current_species %in% valid_species) {
        match_idx <- which(tolower(valid_species) == tolower(current_species))
        if (length(match_idx) > 0) {
          Snap_US$SPP[i] <- valid_species[match_idx[1]]
        } else {
          Snap_US$SPP[i] <- "D"
          warning(paste("Invalid species code at row", i, ":", current_species))
        }
      }
    }
  }

  # Fill missing understory plots by borrowing the nearest available plot.
  if ("Plot.." %in% names(Snap_OS) && "Plot.." %in% names(Snap_US)) {
    os_plots <- unique(Snap_OS$Plot..)
    us_plots <- unique(Snap_US$Plot..)
    missing_plots <- setdiff(os_plots, us_plots)
    if (length(missing_plots) > 0 && length(us_plots) > 0) {
      fake_us <- vector("list", length(missing_plots))
      for (i in seq_along(missing_plots)) {
        plot_id <- missing_plots[[i]]
        closest <- us_plots[which.min(abs(us_plots - plot_id))]
        template <- Snap_US[Snap_US$Plot.. == closest, , drop = FALSE]
        template$Plot.. <- plot_id
        fake_us[[i]] <- template
      }
      Snap_US <- dplyr::bind_rows(Snap_US, dplyr::bind_rows(fake_us))
    }
  }

  # Fill missing overstory CBH from nearest same-species donor when possible.
  cbh_col <- "CBH..0.1m."
  spp_col <- "Spp"
  ht_col <- "Total.Height..m."
  dbh_col <- "DBH"
  if (all(c(cbh_col, spp_col, ht_col, dbh_col) %in% names(Snap_OS))) {
    missing_idx <- which(is.na(Snap_OS[[cbh_col]]))
    non_conifer <- c("Act", "Acb", "Ac", "At", "Ep", "DP", "DU", "Dead", "Dr", "Lt", "Lw")
    for (row in missing_idx) {
      d <- Snap_OS[row, , drop = FALSE]
      spp <- d[[spp_col]][1]
      ht <- suppressWarnings(as.numeric(d[[ht_col]][1]))
      dbh <- suppressWarnings(as.numeric(d[[dbh_col]][1]))

      if (!is.na(spp) && spp %in% non_conifer && !is.na(ht)) {
        Snap_OS[[cbh_col]][row] <- max(ht - 1, 0)
        next
      }

      donors <- Snap_OS[-row, , drop = FALSE]
      donors <- donors[donors[[spp_col]] == spp & !is.na(donors[[cbh_col]]), , drop = FALSE]
      if (nrow(donors) == 0) {
        Snap_OS[[cbh_col]][row] <- if (!is.na(ht)) max(ht - 1, 0) else NA_real_
        next
      }

      dist <- sqrt((as.numeric(donors[[dbh_col]]) - dbh)^2 + (as.numeric(donors[[ht_col]]) - ht)^2)
      Snap_OS[[cbh_col]][row] <- donors[[cbh_col]][which.min(dist)]
    }
  }

  # Ensure CBH stays below height.
  if (all(c("CBH..0.1m.", "Total.Height..m.") %in% names(Snap_OS))) {
    Snap_OS$CBH..0.1m. <- ifelse(
      !is.na(Snap_OS$CBH..0.1m.) &
        !is.na(Snap_OS$Total.Height..m.) &
        Snap_OS$CBH..0.1m. >= Snap_OS$Total.Height..m.,
      Snap_OS$Total.Height..m. - 1,
      Snap_OS$CBH..0.1m.
    )
  }
  if (all(c("CBH..0.1m.", "Height..0.1m.") %in% names(Snap_US))) {
    Snap_US$CBH..0.1m. <- ifelse(
      !is.na(Snap_US$CBH..0.1m.) &
        !is.na(Snap_US$Height..0.1m.) &
        Snap_US$CBH..0.1m. >= Snap_US$Height..0.1m.,
      Snap_US$Height..0.1m. - 1,
      Snap_US$CBH..0.1m.
    )
  }

  # Persist cleaned SNAP files for downstream steps.
  write.csv(Snap_OS, paste0(path, snap_prefix, name, "_OS.csv"), row.names = FALSE)
  write.csv(Snap_US, paste0(path, snap_prefix, name, "_US.csv"), row.names = FALSE)
  write.csv(Snap_EX, paste0(path, snap_prefix, name, "_EXTRA.csv"), row.names = FALSE)
  write.csv(Snap_fuels, paste0(path, snap_prefix, name, "_FUELS.csv"), row.names = FALSE)

  treatments <- unique(Snap_fuels$Stratum)

  # Build editable treatment templates needed by the Treatment Description step.
  ostemp <- data.frame(
    DBH.Class = c("12.5 - 17.5", "17.5 - 22.5", "22.5 - 27.5", "27.5 - 35", "35 - 45", "45+"),
    Total = rep(0, 6),
    stringsAsFactors = FALSE
  )
  ustemp <- data.frame(
    Layer = c("Layer 4 (<1.3m)", "Layer 3 (2.5-7.49)", "Layer 2 (7.5-12.49)", "Layer 1 (12.5-17.5)"),
    Total = rep(0, 4),
    stringsAsFactors = FALSE
  )
  cut_spec <- data.frame(
    Stand.Layer = c("L4", "L3", "L2", "L1", "L1", "L1", "L1", "L1", "L1"),
    DBH.Class = c("0 - 1.5", "1.5 - 7.5", "7.5 - 12.5", "12.5 - 17.5", "17.5 - 22.5", "22.5 - 27.5", "27.5 - 35", "35 - 45", "45+"),
    stringsAsFactors = FALSE
  )

  template_outputs <- list()
  for (treatment in treatments) {
    treatment_folder <- file.path(folder_paths, paste0(treatment, "_tables"))
    dir.create(treatment_folder, recursive = TRUE, showWarnings = FALSE)

    OSTR <- Snap_OS |> filter(Stratum == treatment)
    USTR <- Snap_US |> filter(Stratum == treatment)
    os_species <- unique(OSTR$Spp)
    us_species <- unique(USTR$SPP)

    os_species <- os_species[!is.na(os_species) & nzchar(os_species)]
    us_species <- us_species[!is.na(us_species) & nzchar(us_species)]
    if (!length(os_species)) os_species <- "Unknown"
    if (!length(us_species)) us_species <- "Unknown"

    os_spp <- data.frame(Spp = os_species, Value = 0, stringsAsFactors = FALSE) |>
      pivot_wider(names_from = Spp, values_from = Value, values_fill = 0)
    ba_out <- data.frame(DBH.Class = ostemp$DBH.Class, stringsAsFactors = FALSE) |> cbind(os_spp)
    vol_out <- data.frame(DBH.Class = ostemp$DBH.Class, stringsAsFactors = FALSE) |> cbind(os_spp)
    sph_out_os <- data.frame(DBH.Class = ostemp$DBH.Class, stringsAsFactors = FALSE) |> cbind(os_spp)
    os_spp_expanded <- map_dfc(names(os_spp), function(spp) {
      tibble(!!paste(spp, "Ht", sep = ".") := 0, !!paste(spp, "CBH", sep = ".") := 0)
    })
    oscbhht_out <- data.frame(DBH.Class = ostemp$DBH.Class, stringsAsFactors = FALSE) |> cbind(os_spp_expanded)

    us_spp <- data.frame(Spp = us_species, Value = 0, stringsAsFactors = FALSE) |>
      pivot_wider(names_from = Spp, values_from = Value, values_fill = 0)
    sph_out_us <- data.frame(DBH.Class = ustemp$Layer, stringsAsFactors = FALSE) |> cbind(us_spp)
    us_spp_expanded <- map_dfc(names(us_spp), function(spp) {
      tibble(!!paste(spp, "Ht", sep = ".") := 0, !!paste(spp, "CBH", sep = ".") := 0)
    })
    uscbhout_out <- data.frame(DBH.Class = ustemp$Layer, stringsAsFactors = FALSE) |> cbind(us_spp_expanded)

    write.csv(sph_out_os, file.path(treatment_folder, "OS_SPH.csv"), row.names = FALSE)
    write.csv(sph_out_us, file.path(treatment_folder, "US_SPH.csv"), row.names = FALSE)
    write.csv(ba_out, file.path(treatment_folder, "OS_BA.csv"), row.names = FALSE)
    write.csv(uscbhout_out, file.path(treatment_folder, "US_Ht_CBH.csv"), row.names = FALSE)
    write.csv(vol_out, file.path(treatment_folder, "OS_Vol.csv"), row.names = FALSE)
    write.csv(oscbhht_out, file.path(treatment_folder, "OS_Ht_cbh.csv"), row.names = FALSE)
    write.csv(cut_spec, file.path(treatment_folder, paste0("cuttingSpecs_", treatment, ".csv")), row.names = FALSE)

    template_outputs[[treatment]] <- as.list(file.path(
      treatment_folder,
      c("OS_SPH.csv", "US_SPH.csv", "OS_BA.csv", "US_Ht_CBH.csv", "OS_Vol.csv", "OS_Ht_cbh.csv", paste0("cuttingSpecs_", treatment, ".csv"))
    ))
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
    fuels_rows = nrow(Snap_fuels),
    generated_templates = template_outputs
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
