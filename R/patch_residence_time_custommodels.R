f <- file.path("R_functions","Residence_Time_Function_Nelson2003b.R")
x <- readLines(f, warn=FALSE)

# Ensure 'root' exists in this function file; if not, define it near the top.
has_root <- any(grepl("^root\\s*<-\\s*normalizePath\\(getwd\\(\\)\\)", x))
if (!has_root) {
  # insert after initial comments / at top
  x <- c('root <- normalizePath(getwd())', x)
}

# Replace the Z:/ CustomFuelModels.csv read with portable path
x2 <- gsub(
  'read\\.csv\\("\\s*Z:/Scripts/FronteraCodez/Modeling Templates/Modeling/CustomFuelModels\\.csv\\s*"\\)',
  'read.csv(file.path(root, "templates", "Modeling", "CustomFuelModels.csv"))',
  x
)

# Also catch escaped-backslash variant if it exists
x2 <- gsub(
  'read\\.csv\\("\\s*Z:\\\\Scripts\\\\FronteraCodez\\\\Modeling Templates\\\\Modeling\\\\CustomFuelModels\\.csv\\s*"\\)',
  'read.csv(file.path(root, "templates", "Modeling", "CustomFuelModels.csv"))',
  x2
)

if (identical(x2, x)) {
  cat("No change made (pattern not found). Show the first 60 lines so we can patch exactly.\n")
} else {
  writeLines(x2, f, useBytes=TRUE)
  cat("Patched:", f, "\n")
}
