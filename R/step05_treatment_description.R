step05 <- function(cfg, root) {
  cat("Running Step 0.5: Treatment Description\n")

  suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
    library(ggplot2)
    library(flextable)
  })

  `%||%` <- function(a, b) {
    if (is.null(a) || length(a) == 0) return(b)
    if (is.atomic(a) && length(a) == 1 && is.na(a)) return(b)
    a
  }

  project_name <- cfg$project_name %||% ""
  if (!nzchar(project_name)) {
    stop("config project_name is required for Treatment Description")
  }

  path <- cfg$runtime$raw_dir
  snap_fuels_path <- file.path(path, "SNAP", paste0(project_name, "_FUELS.csv"))
  if (!file.exists(snap_fuels_path)) {
    stop("Missing SNAP fuels file: ", snap_fuels_path)
  }
  Snap_fuels <- read.csv(snap_fuels_path, stringsAsFactors = FALSE)

  folder_paths <- file.path(path, "Stand_StockTables")
  treatments <- cfg$process_to_fuelcalc$tr_names %||% unique(Snap_fuels$Stratum)
  if (!length(treatments)) {
    treatments <- unique(Snap_fuels$Stratum)
  }
  if (!length(treatments)) {
    stop("No treatments found for Treatment Description")
  }

  academic_theme <- theme_bw(base_size = 12, base_family = "serif") +
    theme(
      panel.grid.minor = element_line(color = "grey94", linetype = "dotted", linewidth = 0.3),
      panel.grid.major = element_line(color = "grey85", linetype = "dashed", linewidth = 0.4),
      panel.border = element_rect(color = "black", linewidth = 0.8, fill = NA),
      strip.background = element_rect(fill = "grey92", color = "black", linewidth = 0.8),
      strip.text = element_text(face = "bold", size = 9, family = "serif"),
      axis.text.x = element_text(size = 9, family = "serif"),
      axis.text.y = element_text(size = 9, family = "serif"),
      axis.title = element_text(face = "bold", size = 11, family = "serif"),
      axis.ticks = element_line(color = "black", linewidth = 0.4),
      plot.title = element_text(face = "bold", size = 15, hjust = 0, family = "serif"),
      plot.subtitle = element_text(size = 10, hjust = 0, color = "grey30", family = "serif"),
      plot.caption = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", family = "serif"),
      legend.text = element_text(family = "serif"),
      legend.background = element_rect(color = "grey70", linewidth = 0.4),
      legend.key.size = grid::unit(0.6, "cm"),
      plot.margin = ggplot2::margin(10, 15, 10, 10)
    )

  class_midpoints <- c(
    "0 - 1.5" = 0,
    "1.5 - 7.5" = 5,
    "7.5 - 12.5" = 10,
    "12.5 - 17.5" = 17.5,
    "17.5 - 22.5" = 22.5,
    "22.5 - 27.5" = 27.5,
    "27.5 - 35" = 35,
    "35 - 45" = 45,
    "45+" = 50
  )

  save_plot <- function(plot_obj, filename, width = 8, height = 5) {
    ggsave(filename = filename, plot = plot_obj, width = width, height = height, dpi = 300, units = "in")
  }

  render_flextable <- function(df, title, header_color, body_color, filename) {
    species_cols <- setdiff(names(df), c("Stand Layer", "DBH Class"))
    table_obj <- df |>
      flextable() |>
      merge_v(j = "Stand Layer") |>
      add_header_row(values = c(title), colwidths = c(2 + length(species_cols))) |>
      bg(bg = header_color, part = "header") |>
      bg(j = c("Stand Layer", "DBH Class"), bg = body_color, part = "body") |>
      bg(j = species_cols, bg = "white", part = "body") |>
      color(color = "white", part = "header") |>
      align(i = 1, align = "center", part = "header") |>
      align(j = c("Stand Layer", "DBH Class"), align = "left", part = "all") |>
      align(j = species_cols, align = "center", part = "all") |>
      bold(part = "header") |>
      border_outer(part = "all", border = fp_border_default(color = header_color, width = 1.5)) |>
      border_inner_h(part = "body", border = fp_border_default(color = "#CCCCCC", width = 0.5)) |>
      border_inner_v(part = "all", border = fp_border_default(color = "#CCCCCC", width = 0.5)) |>
      width(j = "Stand Layer", width = 1.1) |>
      width(j = "DBH Class", width = 1.4) |>
      width(j = species_cols, width = 0.9) |>
      font(fontname = "Arial", part = "all") |>
      fontsize(size = 10, part = "all") |>
      colformat_num(na_str = "-") |>
      set_table_properties(layout = "autofit")
    save_as_image(table_obj, filename)
  }

  generated <- list()

  for (treatment in treatments) {
    treatment_folder <- file.path(folder_paths, paste0(treatment, "_tables"))
    required_paths <- c(
      OS.SPH = file.path(treatment_folder, "OS_SPH.csv"),
      OS.BA = file.path(treatment_folder, "OS_BA.csv"),
      OS.VOL = file.path(treatment_folder, "OS_Vol.csv"),
      OS.CBH = file.path(treatment_folder, "OS_Ht_cbh.csv"),
      US.SPH = file.path(treatment_folder, "US_SPH.csv"),
      US.CBH = file.path(treatment_folder, "US_Ht_CBH.csv"),
      CutSpec = file.path(treatment_folder, paste0("cuttingSpecs_", treatment, ".csv"))
    )
    missing <- required_paths[!file.exists(required_paths)]
    if (length(missing) > 0) {
      stop("Treatment Description is missing required files for treatment ", treatment, ": ", paste(missing, collapse = ", "))
    }

    OS.SPH <- read.csv(required_paths[["OS.SPH"]], stringsAsFactors = FALSE)
    OS.BA <- read.csv(required_paths[["OS.BA"]], stringsAsFactors = FALSE)
    OS.VOL <- read.csv(required_paths[["OS.VOL"]], stringsAsFactors = FALSE)
    US.SPH <- read.csv(required_paths[["US.SPH"]], stringsAsFactors = FALSE)
    CutSpec <- read.csv(required_paths[["CutSpec"]], stringsAsFactors = FALSE)

    if (!("Layer" %in% names(US.SPH)) && "DBH.Class" %in% names(US.SPH)) {
      US.SPH <- US.SPH |> dplyr::rename(Layer = DBH.Class)
    }
    if ("Stand.Layer" %in% names(CutSpec) && !("Stand Layer" %in% names(CutSpec))) {
      CutSpec <- CutSpec |> dplyr::rename(`Stand Layer` = Stand.Layer)
    }
    if ("DBH.Class" %in% names(CutSpec) && !("DBH Class" %in% names(CutSpec))) {
      CutSpec <- CutSpec |> dplyr::rename(`DBH Class` = DBH.Class)
    }

    if ("Dead" %in% colnames(US.SPH)) {
      US.SPH <- US.SPH |> dplyr::rename(DP = Dead)
    }

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
      stop("No species columns found in treatment description inputs for treatment ", treatment)
    }

    render_flextable(SPH, paste("Species (stems/ha):", treatment), "#4A7C3F", "#D6E8D0", file.path(treatment_folder, "SPHTable.png"))

    species_colours <- stats::setNames(grDevices::hcl.colors(length(species_cols), "Set 2"), species_cols)
    SPH_plot <- SPH |>
      mutate(Midpoint = unname(class_midpoints[`DBH Class`])) |>
      tidyr::pivot_longer(cols = all_of(species_cols), names_to = "Species", values_to = "SPH") |>
      mutate(Species = factor(Species, levels = species_cols))

    p_sph <- ggplot(SPH_plot, aes(x = factor(Midpoint), y = SPH, fill = Species)) +
      geom_col(position = "stack", width = 0.7) +
      scale_fill_manual(values = species_colours) +
      scale_x_discrete(labels = c("0", "5", "10", "17.5", "22.5", "27.5", "35", "45", "45+")) +
      labs(
        title = paste("Stems per hectare distribution FTU:", treatment),
        x = "Diameter Class Midpoint (cm)",
        y = "Stems per Hectare (sph)",
        fill = NULL
      ) +
      academic_theme
    save_plot(p_sph, file.path(treatment_folder, "SPHPlot.png"))

    CutSpec_long <- CutSpec |>
      tidyr::pivot_longer(cols = all_of(species_cols), names_to = "Species", values_to = "PctCut")

    SPH_long <- SPH |>
      tidyr::pivot_longer(cols = all_of(species_cols), names_to = "Species", values_to = "SPH")
    CutLeave <- SPH_long |>
      left_join(CutSpec_long, by = c("Stand Layer", "DBH Class", "Species")) |>
      mutate(
        Cut = round(SPH * (PctCut / 100)),
        Leave = SPH - Cut
      )
    CutLeave_summary <- CutLeave |>
      dplyr::group_by(`DBH Class`) |>
      dplyr::summarise(Cut = sum(Cut, na.rm = TRUE), Leave = sum(Leave, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(Midpoint = unname(class_midpoints[`DBH Class`])) |>
      dplyr::arrange(Midpoint) |>
      tidyr::pivot_longer(cols = c(Cut, Leave), names_to = "Type", values_to = "SPH") |>
      dplyr::mutate(Type = factor(Type, levels = c("Cut", "Leave")))
    plot_CutLeave <- ggplot(CutLeave_summary, aes(x = factor(Midpoint), y = SPH, fill = Type)) +
      geom_col(position = "stack", width = 0.7) +
      scale_fill_manual(values = c("Cut" = "#C0C0C0", "Leave" = "#4A5E23")) +
      scale_x_discrete(labels = c("0", "5", "10", "17.5", "22.5", "27.5", "35", "45", "45+")) +
      labs(
        title = paste("Stems per hectare cutting distribution FTU:", treatment),
        x = "Diameter Class Midpoint (cm)",
        y = "Stems per Hectare (sph)",
        fill = NULL
      ) +
      academic_theme
    save_plot(plot_CutLeave, file.path(treatment_folder, "SPH_Cut_Plot.png"))

    OS.VOL.mod <- OS.VOL |>
      mutate(`Stand Layer` = "L1", `DBH Class` = DBH.Class) |>
      dplyr::select(-DBH.Class) |>
      dplyr::select(`Stand Layer`, `DBH Class`, everything())
    OS.VOL.mod[is.na(OS.VOL.mod)] <- 0
    render_flextable(OS.VOL.mod, paste("Volume (m^3/ha):", treatment), "#8B1F1F", "#F5D6D6", file.path(treatment_folder, "VOLTable.png"))

    VOL_long <- OS.VOL.mod |>
      tidyr::pivot_longer(cols = all_of(species_cols), names_to = "Species", values_to = "VOL")
    CutLeaveVOL <- VOL_long |>
      left_join(CutSpec_long, by = c("Stand Layer", "DBH Class", "Species")) |>
      mutate(Cut = round(VOL * (PctCut / 100)), Leave = VOL - Cut)
    CutLeave_summaryVOL <- CutLeaveVOL |>
      dplyr::group_by(`DBH Class`) |>
      dplyr::summarise(Cut = sum(Cut, na.rm = TRUE), Leave = sum(Leave, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(Midpoint = unname(class_midpoints[`DBH Class`])) |>
      dplyr::arrange(Midpoint) |>
      tidyr::pivot_longer(cols = c(Cut, Leave), names_to = "Type", values_to = "VOL") |>
      dplyr::mutate(Type = factor(Type, levels = c("Cut", "Leave")))
    plot_CutLeaveVOL <- ggplot(CutLeave_summaryVOL, aes(x = factor(Midpoint), y = VOL, fill = Type)) +
      geom_col(position = "stack", width = 0.7) +
      scale_fill_manual(values = c("Cut" = "#C0C0C0", "Leave" = "#8B1F1F")) +
      scale_x_discrete(labels = c("0", "5", "10", "17.5", "22.5", "27.5", "35", "45", "45+")) +
      labs(
        title = paste("Volume cutting distribution FTU:", treatment),
        x = "Diameter Class Midpoint (cm)",
        y = "Volume (m^3/ha)",
        fill = NULL
      ) +
      academic_theme
    save_plot(plot_CutLeaveVOL, file.path(treatment_folder, "VOL_Cut_Plot.png"))

    OS.BA.mod <- OS.BA |>
      mutate(`Stand Layer` = "L1", `DBH Class` = DBH.Class) |>
      dplyr::select(-DBH.Class) |>
      dplyr::select(`Stand Layer`, `DBH Class`, everything())
    OS.BA.mod[is.na(OS.BA.mod)] <- 0
    render_flextable(OS.BA.mod, paste("Basal Area (m^2/ha):", treatment), "#1F5F8B", "#D6E8F5", file.path(treatment_folder, "BATable.png"))

    BA_long <- OS.BA.mod |>
      tidyr::pivot_longer(cols = all_of(species_cols), names_to = "Species", values_to = "BA")
    CutLeaveBA <- BA_long |>
      left_join(CutSpec_long, by = c("Stand Layer", "DBH Class", "Species")) |>
      mutate(Cut = round(BA * (PctCut / 100)), Leave = BA - Cut)
    CutLeave_summaryBA <- CutLeaveBA |>
      dplyr::group_by(`DBH Class`) |>
      dplyr::summarise(Cut = sum(Cut, na.rm = TRUE), Leave = sum(Leave, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(Midpoint = unname(class_midpoints[`DBH Class`])) |>
      dplyr::arrange(Midpoint) |>
      tidyr::pivot_longer(cols = c(Cut, Leave), names_to = "Type", values_to = "BA") |>
      dplyr::mutate(Type = factor(Type, levels = c("Cut", "Leave")))
    plot_CutLeaveBA <- ggplot(CutLeave_summaryBA, aes(x = factor(Midpoint), y = BA, fill = Type)) +
      geom_col(position = "stack", width = 0.7) +
      scale_fill_manual(values = c("Cut" = "#C0C0C0", "Leave" = "#1F5F8B")) +
      scale_x_discrete(labels = c("0", "5", "10", "17.5", "22.5", "27.5", "35", "45", "45+")) +
      labs(
        title = paste("Basal area cutting distribution FTU:", treatment),
        x = "Diameter Class Midpoint (cm)",
        y = "Basal Area (m^2/ha)",
        fill = NULL
      ) +
      academic_theme
    save_plot(plot_CutLeaveBA, file.path(treatment_folder, "BA_Cut_Plot.png"))

    generated[[treatment]] <- as.list(file.path(
      treatment_folder,
      c("SPHTable.png", "SPHPlot.png", "SPH_Cut_Plot.png", "VOLTable.png", "VOL_Cut_Plot.png", "BATable.png", "BA_Cut_Plot.png")
    ))
  }

  out_dir <- file.path(cfg$runtime$intermediate_dir, "step05_treatment_description")
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
    path = file.path(out_dir, "treatment_description_summary.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )
  saveRDS(summary, file.path(out_dir, "treatment_description_outputs.rds"))
}
