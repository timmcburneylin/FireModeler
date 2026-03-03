root <- normalizePath(getwd())
rfun_dir <- file.path(root, "R_functions")
files <- list.files(rfun_dir, pattern = "\\.R$", full.names = TRUE)

# Add to this list whenever you hit another "there is no package called X"
soft <- c(
  "raster","terra","stars","sf","geosphere","sp","rgdal","rgeos",
  "openair","XLConnect","xlsx","magick","webshot2", "firebehavioR", "Rothermel", "plantecophys", "rootSolve"
)

patched_files <- 0L
patched_lines <- 0L

for (f in files) {
  x <- readLines(f, warn = FALSE)
  y <- x

  for (pkg in soft) {
    # Match: library(pkg) or library("pkg") or library('pkg')
    pat <- paste0("^\\s*library\\(\\s*['\\\"]?", pkg, "['\\\"]?\\s*\\)\\s*$")
    repl <- paste0('if (requireNamespace("', pkg, '", quietly = TRUE)) library(', pkg, ')')

    hit <- grepl(pat, y)
    if (any(hit)) {
      y[hit] <- repl
      patched_lines <- patched_lines + sum(hit)
    }
  }

  if (!identical(x, y)) {
    writeLines(y, f, useBytes = TRUE)
    patched_files <- patched_files + 1L
    cat("Patched:", basename(f), "\n")
  }
}

cat("\nDone. Patched files:", patched_files, "  Patched library() lines:", patched_lines, "\n")
