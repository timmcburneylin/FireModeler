step1 <- function(cfg, root){
  cat("Running Step 1: Clean SNAP\n")

  `%||%` <- function(a, b) {
    if (is.null(a) || length(a) == 0) return(b)
    if (is.atomic(a) && length(a) == 1 && is.na(a)) return(b)
    a
  }
  project_name <- cfg$project_name %||% ""
  resolve_data_path <- function(path_cfg) {
    path_cfg <- path_cfg %||% ""
    if (!nzchar(path_cfg)) return("")
    file.path(cfg$runtime$data_root, path_cfg)
  }

  out_dir <- file.path(cfg$runtime$data_root, cfg$outputs$step1)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  snap_dir <- file.path(cfg$runtime$raw_dir, "SNAP")
  if (!dir.exists(snap_dir)) stop("Missing SNAP directory: ", snap_dir)

  project_file <- function(suffix) file.path(snap_dir, paste0(project_name, suffix, ".csv"))

  validate_required_project_files <- function() {
    required <- c("_OS", "_US", "_EXTRA", "_FUELS")
    missing <- required[!vapply(required, function(suffix) file.exists(project_file(suffix)), logical(1))]
    if (length(missing) > 0) {
      stop(
        "Missing project-specific SNAP files for project_name=", project_name,
        ". Expected: ",
        paste(vapply(missing, function(suffix) project_file(suffix), character(1)), collapse = ", ")
      )
    }
  }

  validate_required_project_files()

  read_snap <- function(path_cfg, suffix){
    from_cfg <- resolve_data_path(path_cfg)
    if (nzchar(from_cfg) && file.exists(from_cfg)) return(read.csv(from_cfg, stringsAsFactors = FALSE))

    auto_path <- project_file(suffix)
    if (!file.exists(auto_path)) stop("Missing SNAP file: ", auto_path)
    read.csv(auto_path, stringsAsFactors = FALSE)
  }

  Snap_OS <- read_snap(cfg$paths$snap_overstory %||% "", "_OS")
  Snap_US <- read_snap(cfg$paths$snap_understory %||% "", "_US")
  Snap_EX <- read_snap(cfg$paths$snap_extra %||% "", "_EXTRA")
  Snap_fuels <- read_snap(cfg$paths$snap_fuels %||% "", "_FUELS")

  # Fill missing understory plots by borrowing nearest plot template.
  if ("Plot.." %in% names(Snap_OS) && "Plot.." %in% names(Snap_US)) {
    os_plots <- unique(Snap_OS$Plot..)
    us_plots <- unique(Snap_US$Plot..)
    missing_plots <- setdiff(os_plots, us_plots)

    if (length(missing_plots) > 0 && length(us_plots) > 0) {
      fake_us <- vector("list", length(missing_plots))
      for (i in seq_along(missing_plots)) {
        pl <- missing_plots[[i]]
        closest <- us_plots[which.min(abs(us_plots - pl))]
        template <- Snap_US[Snap_US$Plot.. == closest, , drop = FALSE]
        template$Plot.. <- pl
        fake_us[[i]] <- template
      }
      Snap_US <- dplyr::bind_rows(Snap_US, dplyr::bind_rows(fake_us))
    }
  }

  # Fill missing CBH values from nearest same-species donor, else fallback to height-1.
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
      closest <- donors[[cbh_col]][which.min(dist)]
      Snap_OS[[cbh_col]][row] <- closest
    }
  }

  write.csv(Snap_OS, file.path(out_dir, "clean_snap_os.csv"), row.names = FALSE)
  write.csv(Snap_US, file.path(out_dir, "clean_snap_us.csv"), row.names = FALSE)
  write.csv(Snap_EX, file.path(out_dir, "clean_snap_extra.csv"), row.names = FALSE)
  write.csv(Snap_fuels, file.path(out_dir, "clean_snap_fuels.csv"), row.names = FALSE)

  summary <- list(
    project_name = project_name,
    project_root = cfg$runtime$project_root,
    os_rows = nrow(Snap_OS),
    us_rows = nrow(Snap_US),
    extra_rows = nrow(Snap_EX),
    fuels_rows = nrow(Snap_fuels),
    missing_cbh_after = if (cbh_col %in% names(Snap_OS)) sum(is.na(Snap_OS[[cbh_col]])) else NA_integer_
  )

  saveRDS(list(Snap_OS = Snap_OS, Snap_US = Snap_US, Snap_EX = Snap_EX, Snap_fuels = Snap_fuels, summary = summary),
          file.path(out_dir, "clean_snap.rds"))
  jsonlite::write_json(summary, path = file.path(out_dir, "snap_summary.json"), auto_unbox = TRUE, pretty = TRUE)
}

