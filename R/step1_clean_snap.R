step1 <- function(cfg, root){
  cat("Running Step 1: Clean SNAP\n")

  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

  out_dir <- file.path(root, cfg$outputs$step1)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  snap_dir <- file.path(root, "data", "raw", "SNAP")
  if (!dir.exists(snap_dir)) stop("Missing SNAP directory: ", snap_dir)

  find_project_prefix <- function(){
    os_files <- list.files(snap_dir, pattern = "_OS\\.csv$", full.names = FALSE)
    if (length(os_files) == 0) stop("No *_OS.csv files found in ", snap_dir)
    sub("_OS\\.csv$", "", os_files[[1]])
  }

  read_snap <- function(path_cfg, suffix){
    from_cfg <- if (!is.null(path_cfg) && nzchar(path_cfg)) file.path(root, path_cfg) else ""
    if (nzchar(from_cfg) && file.exists(from_cfg)) return(read.csv(from_cfg, stringsAsFactors = FALSE))

    prefix <- find_project_prefix()
    auto_path <- file.path(snap_dir, paste0(prefix, suffix, ".csv"))
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

