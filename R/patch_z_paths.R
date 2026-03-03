infile  <- file.path(getwd(), "Rmd", "FireModeling_Portable.Rmd")
outfile <- file.path(getwd(), "Rmd", "FireModeling_Portable_NoZ.Rmd")

x <- readLines(infile, warn = FALSE)

# Helper
`%||%` <- function(a,b) if (is.null(a)) b else a

# 1) Project data root
x <- gsub(
  'path = "Z:/Projects/MOF/NE WRR Prescriptions 2025/Modelling/TR_LionsBurn/Export"',
  'path = file.path(root, "data", "raw")',
  x,
  fixed = TRUE
)

# 2) Modeling templates
x <- gsub(
  'readLines("Z:/Scripts/FronteraCodez/Modeling Templates/Modeling/',
  'readLines(file.path(root, "templates", "Modeling", "',
  x,
  fixed = TRUE
)

x <- gsub(
  'read.csv("Z:/Scripts/FronteraCodez/Modeling Templates/Modeling/',
  'read.csv(file.path(root, "templates", "Modeling", "',
  x,
  fixed = TRUE
)

# Close the file.path calls
x <- gsub('Template_FCP.fcp"', 'Template_FCP.fcp"))', x, fixed = TRUE)
x <- gsub('.ffi"', '.ffi"))', x, fixed = TRUE)
x <- gsub('.tre"', '.tre"))', x, fixed = TRUE)
x <- gsub('FuelCalc.csv"', 'FuelCalc.csv"))', x, fixed = TRUE)
x <- gsub('CustomFuelModels.csv"', 'CustomFuelModels.csv"))', x, fixed = TRUE)

# 3) Grass curing path — replace ANY occurrence of that Z folder
x <- gsub(
  "Z:/General-GIS/Holden's Data/Grass Curing/BC Rasters/",
  'cfg$external$grass_curing_raster_dir %||% ""',
  x,
  fixed = TRUE
)

writeLines(x, outfile)

cat("Wrote:", outfile, "\n")
cat("Remaining Z refs:", sum(grepl("Z:/", x, fixed=TRUE)), "\n")
