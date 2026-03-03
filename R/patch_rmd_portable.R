infile  <- file.path(getwd(), "Rmd", "FireModeling_TR_LionsBurn.Rmd")
outfile <- file.path(getwd(), "Rmd", "FireModeling_Portable.Rmd")
mapfile <- file.path(getwd(), "R_functions", "_filename_map.csv")

if (!file.exists(infile)) stop(paste("Cannot find:", infile))
if (!file.exists(mapfile)) stop(paste("Cannot find:", mapfile))

map <- read.csv(mapfile, stringsAsFactors = FALSE)
x <- readLines(infile, warn = FALSE)

# Add a root definition chunk at the top (safe even if the Rmd already has one)
prefix <- c(
  "```{r firemodel_setup, include=FALSE}",
  "root <- normalizePath(getwd())",
  "```",
  ""
)

x2 <- c(prefix, x)

# Replace all Z: source() calls with local R_functions calls
for (i in seq_len(nrow(map))) {
  orig <- map$original[i]
  san  <- map$sanitized[i]  # same here, but keep general

  # Use fixed=TRUE so we don't have to worry about regex escaping or spaces
  old <- paste0('source("Z:/Scripts/FronteraCodez/Functions/', orig, '")')
  new <- paste0('source(file.path(root, "R_functions", "', san, '"))')

  x2 <- gsub(old, new, x2, fixed = TRUE)
}

writeLines(x2, outfile)
cat("Wrote:", outfile, "\n")
