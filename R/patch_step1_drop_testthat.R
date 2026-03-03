infile <- file.path("R","steps","step1_clean_snap_portable.R")
x <- readLines(infile, warn=FALSE)

before <- sum(grepl("^\\s*library\\(testthat\\)\\s*$", x))
x <- x[!grepl("^\\s*library\\(testthat\\)\\s*$", x)]
after <- sum(grepl("^\\s*library\\(testthat\\)\\s*$", x))

writeLines(x, infile)
cat("Removed library(testthat):", before, "->", after, "\n")
