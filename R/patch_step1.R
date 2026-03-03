f <- file.path("R","steps","step1_clean_snap.R")
if (!file.exists(f)) stop("Missing: ", f)

x <- readLines(f, warn=FALSE)

# Drop lines that load heavy fire-model packages (keep Step 1 light)
drop <- grepl("^\\s*library\\((cffdrs|Rothermel|terra|raster|stars|spatialEco|firebehavioR)\\)\\s*$", x)
x2 <- x[!drop]

# Also drop explicit namespace loads if present
drop2 <- grepl("require\\((cffdrs|Rothermel|terra|raster|stars|spatialEco|firebehavioR)\\)", x2)
x2 <- x2[!drop2]

out <- file.path("R","steps","step1_clean_snap_portable.R")
writeLines(x2, out)
cat("Wrote:", normalizePath(out, winslash="/"), "\n")
