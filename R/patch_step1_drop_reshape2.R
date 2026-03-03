infile <- file.path("R","steps","step1_clean_snap_portable.R")
x <- readLines(infile, warn=FALSE)

before <- sum(grepl("^\\s*library\\(reshape2\\)\\s*$", x))
x <- x[!grepl("^\\s*library\\(reshape2\\)\\s*$", x)]
after <- sum(grepl("^\\s*library\\(reshape2\\)\\s*$", x))

writeLines(x, infile)
cat("Removed library(reshape2):", before, "->", after, "\n")
