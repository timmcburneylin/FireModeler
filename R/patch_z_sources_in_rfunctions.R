root <- normalizePath(getwd())

rfun_dir <- file.path(root, "R_functions")
files <- list.files(rfun_dir, pattern = "\\.R$", full.names = TRUE)

for (f in files) {
  x <- readLines(f, warn = FALSE)

  # Replace: source("Z:/Scripts/FronteraCodez/Functions/FILE.R")
  # With:    source_fun("FILE.R")
  x2 <- gsub(
    'source\\("Z:/Scripts/FronteraCodez/Functions/([^"]+)"\\)',
    'source_fun("\\1")',
    x
  )

  # Also replace any single quotes variant just in case:
  x2 <- gsub(
    "source\\('Z:/Scripts/FronteraCodez/Functions/([^']+)'\\)",
    "source_fun('\\1')",
    x2
  )

  if (!identical(x2, x)) {
    writeLines(x2, f, useBytes = TRUE)
    cat("Patched:", basename(f), "\n")
  }
}
