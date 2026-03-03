infile  <- file.path(getwd(), "Rmd", "FireModeling_Portable_NoZ.Rmd")
outfile <- file.path(getwd(), "Rmd", "FireModeling_Portable_NoZ_FIXED.Rmd")

x <- readLines(infile, warn = FALSE)

old <- 'CustomModels<-read.csv(paste0("Z:/Scripts/FronteraCodez/Modeling Templates/Modeling/CustomFuelModels.csv"))))'
new <- 'CustomModels <- read.csv(file.path(root, "templates", "Modeling", "CustomFuelModels.csv"))'

x <- gsub(old, new, x, fixed = TRUE)

writeLines(x, outfile)
cat("Wrote:", outfile, "\n")
cat("Remaining Z refs:", sum(grepl("Z:/", x, fixed=TRUE)), "\n")
