infile  <- file.path("R","steps","step1_clean_snap_portable.R")
outfile <- file.path("R","steps","step1_clean_snap_portable.R")  # in-place

stopifnot(file.exists(infile))
x <- readLines(infile, warn=FALSE)

# Drop unused/heavy libs from Step 1
x <- x[!grepl("^\\s*library\\(XLConnect\\)\\s*$", x)]
x <- x[!grepl("^\\s*library\\(openair\\)\\s*$", x)]

writeLines(x, outfile)
cat("Patched:", normalizePath(outfile, winslash="/"), "\n")
