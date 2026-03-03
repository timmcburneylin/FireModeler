target <- "R_functions/Residence_Time_Function_Nelson2003b.R"
x <- readLines(target, warn = FALSE)

# Find the line (ignoring whitespace differences) that matches:
# fuelModels<-rbind(fuelModels[,1:16],CustomModels[,2:17])
squash <- gsub("[[:space:]]+", "", x)
hit <- grepl("fuelModels<-rbind\\(fuelModels\\[,1:16\\],CustomModels\\[,2:17\\]\\)", squash)

cat("Matches found:", sum(hit), "\n")
if (sum(hit) != 1) stop("Could not uniquely locate fuelModels rbind line. Found: ", sum(hit))

i <- which(hit)

repl <- c(
  'if (exists("fuelModels")) {',
  '  fuelModels <- rbind(fuelModels[,1:16], CustomModels[,2:17])',
  '} else {',
  '  # start from custom models (drop the first column to match expected 16 cols)',
  '  fuelModels <- CustomModels[,2:17]',
  '}'
)

x <- c(x[1:(i-1)], repl, x[(i+1):length(x)])
writeLines(x, target, useBytes = TRUE)
cat("Patched:", target, "\n")
