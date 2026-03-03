step3 <- function(cfg, root){
  cat("Running Step 3: Fire Modeling\n")

  in_dir <- file.path(root, cfg$outputs$step2)
  out_dir <- file.path(root, cfg$outputs$step3)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  in_rds <- file.path(in_dir, "fuelcalc_inputs.rds")
  if (!file.exists(in_rds)) stop("Missing Step 2 output: ", in_rds)

  dat <- readRDS(in_rds)
  avg <- dat$fuel_averages

  strata <- character(0)
  if (!is.null(dat$Snap_OS) && "Stratum" %in% names(dat$Snap_OS)) {
    strata <- unique(as.character(dat$Snap_OS$Stratum))
  } else if (!is.null(dat$Snap_fuels) && "Stratum" %in% names(dat$Snap_fuels)) {
    strata <- unique(as.character(dat$Snap_fuels$Stratum))
  }
  strata <- strata[!is.na(strata) & nzchar(strata)]

  fuelcalc_root <- file.path(root, "data", "raw", "FuelCalcBC")
  plot_root <- file.path(fuelcalc_root, "Outputs", "Plot Files")

  expected_dirs <- lapply(strata, function(st) {
    d <- file.path(plot_root, paste0(st, "_Plots"))
    files <- if (dir.exists(d)) list.files(d, pattern = "\\.csv$", full.names = TRUE) else character(0)
    list(
      stratum = st,
      directory = d,
      exists = dir.exists(d),
      csv_count = length(files)
    )
  })

  can_run_bridge <- length(expected_dirs) > 0 &&
    all(vapply(expected_dirs, function(x) isTRUE(x$exists) && x$csv_count > 0, logical(1)))

  extract_plot_structure <- function(csv_path, stratum) {
    raw <- read.csv(csv_path, header = FALSE, stringsAsFactors = FALSE)
    if (nrow(raw) < 2 || ncol(raw) < 28) {
      stop("Unexpected FuelCalc plot format in: ", csv_path)
    }
    to_num <- function(x) suppressWarnings(as.numeric(x))
    abs_or_na <- function(x) ifelse(is.na(x), NA_real_, abs(x))

    labels <- trimws(as.character(raw$V1))
    pre_i <- which(labels == "Pre-Treatment")[1]
    post_i <- which(labels == "Post-Treatment")[1]
    if (is.na(pre_i) || is.na(post_i)) {
      stop("Could not find Pre/Post rows in: ", csv_path)
    }

    data.frame(
      Treatment = stratum,
      Plot = gsub("\\D+", "", basename(csv_path)),
      Pre_CBH = abs_or_na(to_num(raw$V24[pre_i])),
      Post_CBH = abs_or_na(to_num(raw$V24[post_i])),
      Pre_CFL = abs_or_na(to_num(raw$V22[pre_i])) / 10,
      Post_CFL = abs_or_na(to_num(raw$V22[post_i])) / 10,
      Pre_CBD = abs_or_na(to_num(raw$V23[pre_i])),
      Post_CBD = abs_or_na(to_num(raw$V23[post_i])),
      Pre_CC = abs_or_na(to_num(raw$V28[pre_i])),
      Post_CC = abs_or_na(to_num(raw$V28[post_i])),
      Pre_TPH = abs_or_na(to_num(raw$V26[pre_i])),
      Post_TPH = abs_or_na(to_num(raw$V26[post_i])),
      stringsAsFactors = FALSE
    )
  }

  extract_plot_slash <- function(csv_path, stratum) {
    raw <- read.csv(csv_path, header = FALSE, stringsAsFactors = FALSE)
    if (nrow(raw) < 2 || ncol(raw) < 11) {
      stop("Unexpected FuelCalc plot format in: ", csv_path)
    }

    labels <- trimws(as.character(raw$V1))
    idx <- function(lbl) which(labels == lbl)[1]
    pre_i <- idx("Pre-Treatment")
    thn_i <- idx("Thinned")
    prn_i <- idx("Pruned")
    post_i <- idx("Post-Treatment")
    if (is.na(pre_i) || is.na(post_i)) {
      stop("Could not find Pre/Post rows in: ", csv_path)
    }

    # Legacy FuelCalc exports often shift Thinned/Pruned values a few columns right.
    # Normalize those rows to match the Pre/Post column layout used below.
    if (!is.na(thn_i) && ncol(raw) >= 17) raw[thn_i, 2:14] <- raw[thn_i, 5:17]
    if (!is.na(prn_i) && ncol(raw) >= 17) raw[prn_i, 2:14] <- raw[prn_i, 5:17]

    num <- function(col, i) suppressWarnings(as.numeric(col[i]))
    metrics <- function(i) {
      if (is.na(i)) {
        return(c(ShrHer = NA_real_, LitLic = NA_real_, HR1 = NA_real_, HR10 = NA_real_, HR100 = NA_real_, HR1000 = NA_real_))
      }
      pair_sum <- function(a, b) sum(c(a, b), na.rm = TRUE)
      c(
        ShrHer = num(raw$V3, i), # legacy behavior: no scaling for shrub/herb
        LitLic = num(raw$V4, i) * 0.1,
        HR1 = num(raw$V5, i) * 0.1,
        HR10 = pair_sum(num(raw$V6, i), num(raw$V7, i)) * 0.1,
        HR100 = pair_sum(num(raw$V8, i), num(raw$V9, i)) * 0.1,
        HR1000 = pair_sum(num(raw$V10, i), num(raw$V11, i)) * 0.1
      )
    }

    pre <- metrics(pre_i)
    thn <- metrics(thn_i)
    prn <- metrics(prn_i)
    post <- metrics(post_i)

    slash <- c(
      ShrHer = 0,
      LitLic = sum(thn["LitLic"], prn["LitLic"], na.rm = TRUE),
      HR1 = sum(thn["HR1"], prn["HR1"], na.rm = TRUE),
      HR10 = sum(thn["HR10"], prn["HR10"], na.rm = TRUE),
      HR100 = sum(thn["HR100"], prn["HR100"], na.rm = TRUE),
      HR1000 = sum(thn["HR1000"], prn["HR1000"], na.rm = TRUE)
    )

    out <- rbind(
      c(Treatment = "Pre-Treatment", pre),
      c(Treatment = "Thinned", thn),
      c(Treatment = "Pruned", prn),
      c(Treatment = "Total Slash", slash),
      c(Treatment = "Total Fuel", post)
    )
    out <- as.data.frame(out, stringsAsFactors = FALSE)
    for (nm in c("ShrHer", "LitLic", "HR1", "HR10", "HR100", "HR1000")) {
      out[[nm]] <- suppressWarnings(as.numeric(out[[nm]]))
    }
    out$Stratum <- stratum
    out$Plot <- gsub("\\D+", "", basename(csv_path))
    out <- out[, c("Stratum", "Plot", "Treatment", "ShrHer", "LitLic", "HR1", "HR10", "HR100", "HR1000")]
    out
  }

  write.csv(avg, file.path(out_dir, "step3_input_fuel_averages.csv"), row.names = FALSE)

  if (!can_run_bridge) {
    run_info <- list(
      timestamp = as.character(Sys.time()),
      status = "blocked_missing_fuelcalc_exports",
      note = "Step 3 bridge needs FuelCalc plot CSV exports before legacy calculations can run.",
      expected_plot_root = plot_root,
      expected_plot_dirs = expected_dirs
    )
    jsonlite::write_json(run_info, path = file.path(out_dir, "fire_model_run.json"), auto_unbox = TRUE, pretty = TRUE)
    return(invisible(run_info))
  }

  stratum_frames <- list()
  slash_stratum_summary <- list()
  slash_plot_rows <- list()
  slash_out_dir <- file.path(out_dir, "SlashResiduals")
  dir.create(slash_out_dir, recursive = TRUE, showWarnings = FALSE)
  for (st in strata) {
    d <- file.path(plot_root, paste0(st, "_Plots"))
    files <- list.files(d, pattern = "\\.csv$", full.names = TRUE)
    rows <- lapply(files, function(f) extract_plot_structure(f, st))
    df_st <- dplyr::bind_rows(rows)
    stratum_frames[[st]] <- df_st
    write.csv(df_st, file.path(out_dir, paste0(st, "_StandStructure.csv")), row.names = FALSE)

    slash_rows <- dplyr::bind_rows(lapply(files, function(f) extract_plot_slash(f, st)))
    slash_plot_rows[[st]] <- slash_rows
    slash_mean <- dplyr::summarise(
      dplyr::group_by(slash_rows, Treatment),
      ShrHer = mean(ShrHer, na.rm = TRUE),
      LitLic = mean(LitLic, na.rm = TRUE),
      HR1 = mean(HR1, na.rm = TRUE),
      HR10 = mean(HR10, na.rm = TRUE),
      HR100 = mean(HR100, na.rm = TRUE),
      HR1000 = mean(HR1000, na.rm = TRUE),
      .groups = "drop"
    )
    order_levels <- c("Pre-Treatment", "Thinned", "Pruned", "Total Slash", "Total Fuel")
    slash_mean$Treatment <- factor(slash_mean$Treatment, levels = order_levels)
    slash_mean <- slash_mean[order(slash_mean$Treatment), ]
    slash_mean$Treatment <- as.character(slash_mean$Treatment)
    slash_stratum_summary[[st]] <- slash_mean
    write.csv(slash_mean, file.path(slash_out_dir, paste0(st, ".csv")), row.names = FALSE)
  }

  all_struct <- dplyr::bind_rows(stratum_frames)
  summary_struct <- dplyr::summarise(
    dplyr::group_by(all_struct, Treatment),
    Pre_CBH = mean(Pre_CBH, na.rm = TRUE),
    Post_CBH = mean(Post_CBH, na.rm = TRUE),
    Pre_CFL = mean(Pre_CFL, na.rm = TRUE),
    Post_CFL = mean(Post_CFL, na.rm = TRUE),
    Pre_CBD = mean(Pre_CBD, na.rm = TRUE),
    Post_CBD = mean(Post_CBD, na.rm = TRUE),
    Pre_CC = mean(Pre_CC, na.rm = TRUE),
    Post_CC = mean(Post_CC, na.rm = TRUE),
    Pre_TPH = mean(Pre_TPH, na.rm = TRUE),
    Post_TPH = mean(Post_TPH, na.rm = TRUE),
    .groups = "drop"
  )

  write.csv(all_struct, file.path(out_dir, "All_Plots_StandStructure.csv"), row.names = FALSE)
  write.csv(summary_struct, file.path(out_dir, "All_Treatments_StandStructure.csv"), row.names = FALSE)

  all_slash_rows <- dplyr::bind_rows(slash_plot_rows)
  all_slash_summary <- dplyr::summarise(
    dplyr::group_by(all_slash_rows, Treatment),
    ShrHer = mean(ShrHer, na.rm = TRUE),
    LitLic = mean(LitLic, na.rm = TRUE),
    HR1 = mean(HR1, na.rm = TRUE),
    HR10 = mean(HR10, na.rm = TRUE),
    HR100 = mean(HR100, na.rm = TRUE),
    HR1000 = mean(HR1000, na.rm = TRUE),
    .groups = "drop"
  )
  order_levels <- c("Pre-Treatment", "Thinned", "Pruned", "Total Slash", "Total Fuel")
  all_slash_summary$Treatment <- factor(all_slash_summary$Treatment, levels = order_levels)
  all_slash_summary <- all_slash_summary[order(all_slash_summary$Treatment), ]
  all_slash_summary$Treatment <- as.character(all_slash_summary$Treatment)
  write.csv(all_slash_rows, file.path(slash_out_dir, "All_Plots_SlashResiduals.csv"), row.names = FALSE)
  write.csv(all_slash_summary, file.path(slash_out_dir, "All_Treatments_SlashResiduals.csv"), row.names = FALSE)

  # Weather staging bridge (legacy inputs -> canonical files for downstream blocks).
  weather_root <- file.path(root, "data", "raw", "Weather")
  weather_out_dir <- file.path(out_dir, "WeatherStage")
  dir.create(weather_out_dir, recursive = TRUE, showWarnings = FALSE)

  snap_dir <- file.path(root, "data", "raw", "SNAP")
  snap_os <- list.files(snap_dir, pattern = "_OS\\.csv$", full.names = FALSE)
  daily_fwi_candidates <- list.files(weather_root, pattern = "_Daily_FWI_AllYear\\.csv$", full.names = FALSE)
  project_id <- if (length(daily_fwi_candidates) > 0) {
    sub("_Daily_FWI_AllYear\\.csv$", "", daily_fwi_candidates[[1]])
  } else if (length(snap_os) > 0) {
    # Prefer legacy TR_ project prefix when multiple SNAP prefixes are present.
    snap_prefixes <- sub("_OS\\.csv$", "", snap_os)
    tr_hit <- snap_prefixes[grepl("^TR_", snap_prefixes)]
    if (length(tr_hit) > 0) tr_hit[[1]] else snap_prefixes[[1]]
  } else {
    "TR_LionsBurn"
  }

  to_num <- function(x) suppressWarnings(as.numeric(trimws(as.character(x))))
  as_date_chr <- function(x) format(as.Date(x), "%Y-%m-%d")

  daily_source_candidates <- c(
    file.path(weather_root, paste0(project_id, "_Daily_FWI_AllYear.csv")),
    file.path(weather_root, "TR_LionsBurn_Daily_FWI_AllYear.csv"),
    file.path(weather_root, "Daily_Weather_AllYear.csv")
  )
  daily_source <- daily_source_candidates[file.exists(daily_source_candidates)][1]

  hourly_source_candidates <- c(
    file.path(weather_root, "Hourly_Weather_TUMBLER(DENISON).csv"),
    file.path(weather_root, "Hourly_Weather_TUMBLER(Denison).csv")
  )
  hourly_source <- hourly_source_candidates[file.exists(hourly_source_candidates)][1]

  # Fallback parser for raw FLNRO station file if preprocessed hourly CSV is unavailable.
  if (length(hourly_source) == 0 || is.na(hourly_source)) {
    flnro_raw <- file.path(weather_root, "FLNRO-WMB", "127.csv")
    if (file.exists(flnro_raw)) {
      lines <- readLines(flnro_raw, warn = FALSE)
      if (length(lines) >= 3) {
        hourly_raw <- read.csv(text = paste(lines[-1], collapse = "\n"), stringsAsFactors = FALSE)
        hourly_raw <- hourly_raw[, c("wind_direction", "precipitation", "wind_speed", "temperature", "relative_humidity", "time")]
        names(hourly_raw) <- c("wind_direction", "precipitation", "wind_speed", "temperature", "rel_hum", "time")
        hourly_raw$time <- trimws(hourly_raw$time)
        hourly_raw$Month <- as.integer(substr(hourly_raw$time, 6, 7))
        hourly_raw$Day <- as.integer(substr(hourly_raw$time, 9, 10))
        hourly_raw$Year <- as.integer(substr(hourly_raw$time, 1, 4))
        hourly_raw$Hour <- as.integer(substr(hourly_raw$time, 12, 13))
        hourly_raw$date <- as_date_chr(hourly_raw$time)
        hourly_raw$wind_direction <- to_num(hourly_raw$wind_direction)
        hourly_raw$precipitation <- to_num(hourly_raw$precipitation)
        hourly_raw$wind_speed <- to_num(hourly_raw$wind_speed)
        hourly_raw$temperature <- to_num(hourly_raw$temperature)
        hourly_raw$rel_hum <- to_num(hourly_raw$rel_hum)
        hourly_source <- file.path(weather_out_dir, "canonical_hourly_weather.csv")
        write.csv(hourly_raw, hourly_source, row.names = FALSE)
      }
    }
  }

  weather_info <- list(
    project_id = project_id,
    daily_source = if (length(daily_source) > 0) daily_source else NA_character_,
    hourly_source = if (length(hourly_source) > 0) hourly_source else NA_character_,
    canonical_daily = NA_character_,
    canonical_hourly = NA_character_,
    summer_daily = NA_character_,
    issues = character(0)
  )

  if (length(daily_source) > 0 && !is.na(daily_source) && file.exists(daily_source)) {
    daily <- read.csv(daily_source, stringsAsFactors = FALSE)
    if (!("MON" %in% names(daily)) && ("DATE" %in% names(daily) || "Date" %in% names(daily))) {
      dcol <- if ("DATE" %in% names(daily)) "DATE" else "Date"
      ddate <- as.Date(daily[[dcol]])
      daily$MON <- as.integer(format(ddate, "%m"))
      daily$DAY <- as.integer(format(ddate, "%d"))
    }
    keep <- c("WD", "PREC", "WS", "TEMP", "RH", "TIME", "MON", "DAY", "YR", "HR", "LAT", "LONG", "DATE", "FFMC", "DMC", "DC", "ISI", "BUI", "FWI", "DSR")
    cols <- keep[keep %in% names(daily)]
    daily_out <- daily[, cols, drop = FALSE]
    weather_info$canonical_daily <- file.path(weather_out_dir, "canonical_daily_fwi_all_year.csv")
    write.csv(daily_out, weather_info$canonical_daily, row.names = FALSE)

    if (all(c("MON", "DAY") %in% names(daily_out))) {
      # Boreal legacy season window: May 15 through Aug 31.
      m <- as.integer(daily_out$MON)
      d <- as.integer(daily_out$DAY)
      summer_idx <- (m > 5 & m < 9) | (m == 5 & d >= 15) | (m == 8 & d <= 31)
      summer <- daily_out[which(summer_idx), , drop = FALSE]
      weather_info$summer_daily <- file.path(weather_out_dir, "canonical_daily_fwi_summer.csv")
      write.csv(summer, weather_info$summer_daily, row.names = FALSE)
    }
  } else {
    weather_info$issues <- c(weather_info$issues, "Missing daily weather FWI source.")
  }

  if (length(hourly_source) > 0 && !is.na(hourly_source) && file.exists(hourly_source)) {
    hourly <- read.csv(hourly_source, stringsAsFactors = FALSE)
    # Standardize column names found in preprocessed hourly export.
    rename_map <- c(
      wind_direction = "wind_direction",
      precipitation = "precipitation",
      wind_speed = "wind_speed",
      temperature = "temperature",
      rel_hum = "rel_hum",
      time = "time",
      Month = "Month",
      Day = "Day",
      Year = "Year",
      Hour = "Hour",
      date = "date"
    )
    common <- intersect(names(rename_map), names(hourly))
    hourly_out <- hourly[, common, drop = FALSE]
    names(hourly_out) <- rename_map[common]
    if (!("date" %in% names(hourly_out)) && "time" %in% names(hourly_out)) hourly_out$date <- as_date_chr(trimws(hourly_out$time))
    weather_info$canonical_hourly <- file.path(weather_out_dir, "canonical_hourly_weather.csv")
    write.csv(hourly_out, weather_info$canonical_hourly, row.names = FALSE)
  } else {
    weather_info$issues <- c(weather_info$issues, "Missing hourly weather source (and FLNRO fallback unavailable).")
  }

  jsonlite::write_json(weather_info, path = file.path(weather_out_dir, "weather_stage_summary.json"), auto_unbox = TRUE, pretty = TRUE)

  # Fire behavior input staging: prefer legacy pre/post files, fallback to generated summary.
  fire_raw_inputs <- file.path(root, "data", "raw", "FireBehavior", "Inputs")
  fire_stage_dir <- file.path(out_dir, "FireBehaviorStage")
  dir.create(fire_stage_dir, recursive = TRUE, showWarnings = FALSE)

  pre_src <- file.path(fire_raw_inputs, "Pre_Treatment_Structure_Data.csv")
  post_src <- file.path(fire_raw_inputs, "Post_Treatment_Structure_Data.csv")
  pre_out <- file.path(fire_stage_dir, "canonical_pre_treatment_structure.csv")
  post_out <- file.path(fire_stage_dir, "canonical_post_treatment_structure.csv")

  daily_ref <- if (!is.null(weather_info$summer_daily) && !is.na(weather_info$summer_daily) && nzchar(weather_info$summer_daily)) {
    weather_info$summer_daily
  } else {
    weather_info$canonical_daily
  }

  fire_stage <- list(
    mode = NA_character_,
    pre_source = NA_character_,
    post_source = NA_character_,
    pre_output = pre_out,
    post_output = post_out,
    strata = strata,
    weather_daily = daily_ref,
    weather_hourly = weather_info$canonical_hourly,
    issues = character(0)
  )

  if (file.exists(pre_src) && file.exists(post_src)) {
    pre_df <- read.csv(pre_src, stringsAsFactors = FALSE)
    post_df <- read.csv(post_src, stringsAsFactors = FALSE)
    if ("Stratum" %in% names(pre_df)) pre_df <- pre_df[order(pre_df$Stratum), , drop = FALSE]
    if ("Stratum" %in% names(post_df)) post_df <- post_df[order(post_df$Stratum), , drop = FALSE]
    write.csv(pre_df, pre_out, row.names = FALSE)
    write.csv(post_df, post_out, row.names = FALSE)
    fire_stage$mode <- "copied_existing_firebehavior_inputs"
    fire_stage$pre_source <- pre_src
    fire_stage$post_source <- post_src
  } else {
    # Fallback builder from staged stand/slash summaries.
    summarize_row <- function(df, treatment) {
      if (is.null(df) || nrow(df) == 0) return(rep(NA_real_, 6))
      r <- df[df$Treatment == treatment, , drop = FALSE]
      if (nrow(r) == 0) return(rep(NA_real_, 6))
      c(
        LitLic = as.numeric(r$LitLic[1]),
        HR1 = as.numeric(r$HR1[1]),
        HR10 = as.numeric(r$HR10[1]),
        HR100 = as.numeric(r$HR100[1]),
        HR1000 = as.numeric(r$HR1000[1]),
        ShrHer = as.numeric(r$ShrHer[1])
      )
    }

    pre_rows <- list()
    post_rows <- list()
    for (st in strata) {
      s <- summary_struct[summary_struct$Treatment == st, , drop = FALSE]
      if (nrow(s) == 0) next
      slash_df <- slash_stratum_summary[[st]]
      pre_sl <- summarize_row(slash_df, "Pre-Treatment")
      post_sl <- summarize_row(slash_df, "Total Fuel")
      a <- avg[avg$Stratum == st, , drop = FALSE]

      pre_rows[[st]] <- data.frame(
        Stratum = st,
        Fuelcalc_CBH = as.numeric(s$Pre_CBH[1]),
        TPH = as.numeric(s$Pre_TPH[1]),
        CC = as.numeric(s$Pre_CC[1]),
        CFL = as.numeric(s$Pre_CFL[1]),
        CBD = as.numeric(s$Pre_CBD[1]),
        Duff_depth = if (nrow(a) > 0) as.numeric(a$DuffDepth.cm[1]) else NA_real_,
        Lit_kg = pre_sl["LitLic"],
        hr_1_kg = pre_sl["HR1"],
        hr_10_kg = pre_sl["HR10"],
        hr_100_kg = pre_sl["HR100"],
        hr_1000_kg = pre_sl["HR1000"],
        Shrub_Loading = if (nrow(a) > 0) as.numeric(a$Shrub.kg.m2[1]) else NA_real_,
        Grass_Loading = if (nrow(a) > 0) as.numeric(a$Grass.kg.m2[1]) else NA_real_,
        stringsAsFactors = FALSE
      )

      post_rows[[st]] <- data.frame(
        Stratum = st,
        Fuelcalc_CBH = as.numeric(s$Post_CBH[1]),
        TPH = as.numeric(s$Post_TPH[1]),
        CC = as.numeric(s$Post_CC[1]),
        CFL = as.numeric(s$Post_CFL[1]),
        CBD = as.numeric(s$Post_CBD[1]),
        Duff_depth = if (nrow(a) > 0) as.numeric(a$DuffDepth.cm[1]) else NA_real_,
        Lit_kg = post_sl["LitLic"],
        hr_1_kg = post_sl["HR1"],
        hr_10_kg = post_sl["HR10"],
        hr_100_kg = post_sl["HR100"],
        hr_1000_kg = post_sl["HR1000"],
        Shrub_Loading = if (nrow(a) > 0) as.numeric(a$Shrub.kg.m2[1]) else NA_real_,
        Grass_Loading = if (nrow(a) > 0) as.numeric(a$Grass.kg.m2[1]) else NA_real_,
        stringsAsFactors = FALSE
      )
    }

    pre_df <- dplyr::bind_rows(pre_rows)
    post_df <- dplyr::bind_rows(post_rows)
    write.csv(pre_df, pre_out, row.names = FALSE)
    write.csv(post_df, post_out, row.names = FALSE)
    fire_stage$mode <- "generated_fallback_from_step3_summaries"
    fire_stage$pre_source <- "generated"
    fire_stage$post_source <- "generated"
    fire_stage$issues <- c(fire_stage$issues, "Raw FireBehavior pre/post structure files were missing; generated fallback tables.")
  }

  jsonlite::write_json(
    fire_stage,
    path = file.path(fire_stage_dir, "fire_behavior_stage_summary.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )

  # Fire behavior run bridge: stage legacy model outputs when present.
  fire_run_dir <- file.path(out_dir, "FireBehaviorRun")
  dir.create(fire_run_dir, recursive = TRUE, showWarnings = FALSE)

  fb_outputs_root <- file.path(root, "data", "raw", "FireBehavior", "Outputs")
  pre_rds <- file.path(fb_outputs_root, "Results_PreTreatment.rds")
  post_rds <- file.path(fb_outputs_root, "Results_PostTreatment.rds")
  fullburn_csv <- file.path(fb_outputs_root, "FullBurnConditions_Data.csv")

  fire_run <- list(
    mode = NA_character_,
    pre_rds_source = if (file.exists(pre_rds)) pre_rds else NA_character_,
    post_rds_source = if (file.exists(post_rds)) post_rds else NA_character_,
    fullburn_source = if (file.exists(fullburn_csv)) fullburn_csv else NA_character_,
    summary_csv = file.path(fire_run_dir, "fire_behavior_run_summary.csv"),
    index_csv = file.path(fire_run_dir, "fire_behavior_result_index.csv"),
    issues = character(0)
  )

  if (file.exists(pre_rds) && file.exists(post_rds) && file.exists(fullburn_csv)) {
    pre_obj <- readRDS(pre_rds)
    post_obj <- readRDS(post_rds)
    fb_df <- read.csv(fullburn_csv, stringsAsFactors = FALSE)

    # Build a compact index of result objects (names only; keeps stage lightweight).
    idx <- data.frame(
      scenario = c(rep("pre", length(pre_obj)), rep("post", length(post_obj))),
      object_name = c(names(pre_obj), names(post_obj)),
      stringsAsFactors = FALSE
    )
    write.csv(idx, fire_run$index_csv, row.names = FALSE)

    summary_tbl <- data.frame(
      metric = c(
        "pre_result_objects",
        "post_result_objects",
        "fullburn_rows",
        "fullburn_strata",
        "fullburn_weather_ids"
      ),
      value = c(
        length(pre_obj),
        length(post_obj),
        nrow(fb_df),
        if ("Strata" %in% names(fb_df)) length(unique(fb_df$Strata)) else NA_integer_,
        if ("Weather" %in% names(fb_df)) length(unique(fb_df$Weather)) else NA_integer_
      ),
      stringsAsFactors = FALSE
    )
    write.csv(summary_tbl, fire_run$summary_csv, row.names = FALSE)

    # Keep a canonical copy for downstream UI consumption.
    write.csv(fb_df, file.path(fire_run_dir, "canonical_fullburnconditions_data.csv"), row.names = FALSE)
    fire_run$mode <- "staged_from_legacy_outputs"
  } else {
    # Fallback: build reproducible scenario matrix from staged structure + weather.
    pre_df <- read.csv(pre_out, stringsAsFactors = FALSE)
    post_df <- read.csv(post_out, stringsAsFactors = FALSE)
    daily_path <- daily_ref

    if (is.null(daily_path) || is.na(daily_path) || !file.exists(daily_path)) {
      fire_run$mode <- "blocked_missing_inputs"
      fire_run$issues <- c(fire_run$issues, "Missing canonical daily weather for fallback fire behavior matrix.")
    } else {
      daily_df <- read.csv(daily_path, stringsAsFactors = FALSE)
      if ("DATE" %in% names(daily_df)) {
        date_col <- "DATE"
      } else if ("Date" %in% names(daily_df)) {
        date_col <- "Date"
      } else {
        date_col <- NA_character_
      }

      # Trim to reasonable size while preserving variability.
      if ("FWI" %in% names(daily_df)) {
        ord <- order(suppressWarnings(as.numeric(daily_df$FWI)), decreasing = TRUE)
        keep_n <- min(120, nrow(daily_df))
        daily_df <- daily_df[ord[seq_len(keep_n)], , drop = FALSE]
      } else {
        keep_n <- min(120, nrow(daily_df))
        daily_df <- daily_df[seq_len(keep_n), , drop = FALSE]
      }

      pre_df$Scenario <- "Pre"
      post_df$Scenario <- "Post"
      structure_df <- dplyr::bind_rows(pre_df, post_df)
      structure_df$join_key <- 1
      daily_df$join_key <- 1
      run_matrix <- merge(structure_df, daily_df, by = "join_key")
      run_matrix$join_key <- NULL
      if (!is.na(date_col)) run_matrix$WeatherDate <- as.character(run_matrix[[date_col]])

      write.csv(run_matrix, file.path(fire_run_dir, "fallback_run_matrix.csv"), row.names = FALSE)
      summary_tbl <- data.frame(
        metric = c("rows", "strata", "scenarios", "weather_rows_used"),
        value = c(
          nrow(run_matrix),
          if ("Stratum" %in% names(run_matrix)) length(unique(run_matrix$Stratum)) else NA_integer_,
          if ("Scenario" %in% names(run_matrix)) length(unique(run_matrix$Scenario)) else NA_integer_,
          nrow(daily_df)
        ),
        stringsAsFactors = FALSE
      )
      write.csv(summary_tbl, fire_run$summary_csv, row.names = FALSE)
      write.csv(data.frame(scenario = unique(run_matrix$Scenario), object_name = "fallback_run_matrix", stringsAsFactors = FALSE),
                fire_run$index_csv, row.names = FALSE)
      fire_run$mode <- "generated_fallback_run_matrix"
      fire_run$issues <- c(fire_run$issues, "Legacy FireBehavior outputs missing; generated fallback matrix.")
    }
  }

  jsonlite::write_json(
    fire_run,
    path = file.path(fire_run_dir, "fire_behavior_run_stage_summary.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )

  run_info <- list(
    timestamp = as.character(Sys.time()),
    status = "legacy_bridge_completed",
    strata = length(strata),
    plots_processed = nrow(all_struct),
    outputs = list(
      all_plots = file.path(out_dir, "All_Plots_StandStructure.csv"),
      all_treatments = file.path(out_dir, "All_Treatments_StandStructure.csv"),
      slash_residuals = file.path(slash_out_dir, "All_Treatments_SlashResiduals.csv"),
      weather_stage = file.path(weather_out_dir, "weather_stage_summary.json"),
      fire_behavior_stage = file.path(fire_stage_dir, "fire_behavior_stage_summary.json"),
      fire_behavior_run = file.path(fire_run_dir, "fire_behavior_run_stage_summary.json")
    )
  )
  jsonlite::write_json(run_info, path = file.path(out_dir, "fire_model_run.json"), auto_unbox = TRUE, pretty = TRUE)
}

