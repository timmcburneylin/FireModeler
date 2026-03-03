rmd <- file.path(getwd(), "Rmd", "FireModeling_TR_LionsBurn.Rmd")
if (!file.exists(rmd)) stop(paste("Cannot find:", rmd))

x <- readLines(rmd, warn = FALSE)
s <- grep("source\\(", x, value = TRUE)

paths <- sub(".*source\\(\"", "", s)
paths <- sub("\"\\).*", "", paths)

files <- unique(basename(paths))

dir.create("R_functions", showWarnings = FALSE)

sanitize <- function(f) gsub('[<>:"/\\\\|?*]', "_", f)

map <- data.frame(
  original = files,
  sanitized = vapply(files, sanitize, character(1)),
  stringsAsFactors = FALSE
)

write.csv(map, file.path("R_functions", "_filename_map.csv"), row.names = FALSE)

for (i in seq_along(files)) {
  orig <- files[i]
  san  <- sanitize(orig)
  out  <- file.path("R_functions", san)

  if (!file.exists(out)) {
    writeLines(
      sprintf('stop("Missing dependency: %s. Put original file in R_functions/%s")', orig, san),
      out
    )
  }
}

cat("Created stubs for", length(files), "files in R_functions/ (see R_functions/_filename_map.csv)\n")
