cat("WD: ", getwd(), "\n", sep="")

dir_rf <- file.path(getwd(), "R_functions")
if (!dir.exists(dir_rf)) stop("No R_functions folder at: ", dir_rf)

fs <- list.files(dir_rf, full.names = TRUE)
cat("Found ", length(fs), " files in R_functions\n", sep="")

is_stub <- vapply(fs, function(f) {
  any(grepl('^stop\\("Missing dependency:', readLines(f, warn = FALSE)))
}, logical(1))

cat("Stub files remaining: ", sum(is_stub), "\n", sep="")
if (any(is_stub)) cat(paste(basename(fs[is_stub]), collapse="\n"), "\n")
