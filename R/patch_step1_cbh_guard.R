f <- "R/steps/step1_clean_snap_portable.R"
x <- readLines(f, warn = FALSE)

# Find the Species_Only line
i <- grep("Species_Only<-Snap_OS\\[-row,\\]", x)

if (length(i) == 0) {
  stop("Couldn't find the Species_Only<-Snap_OS[-row,] line to patch.")
}

# Don't double-insert if we already patched
already <- any(grepl("nrow\\(Species_Only\\) == 0", x))
if (already) {
  cat("Patch already present; not modifying.\n")
} else {
  insert <- c(
    "  if (nrow(Species_Only) == 0) {",
    "    # Fallback: no same-species donor CBH available",
    "    if (!is.na(d$Total.Height..m.)) {",
    "      Snap_OS[row,]$CBH..0.1m. <- max(d$Total.Height..m. - 1, 0)",
    "    } else {",
    "      Snap_OS[row,]$CBH..0.1m. <- NA_real_",
    "    }",
    "    next",
    "  }"
  )

  # Insert right after the first match
  k <- i[1]
  x <- c(x[1:k], insert, x[(k+1):length(x)])
  writeLines(x, f, useBytes = TRUE)
  cat("Inserted Species_Only empty-guard after line", k, "in", f, "\n")
}
