f <- "R_functions/Montilla_FireDynamics_Function.R"
x <- readLines(f, warn = FALSE)

# Replace any assignment like: path <- "Z:/Projects/...."
x2 <- gsub('path\\s*<-\\s*"Z:/[^"]+"', 'path <- root', x)

if (!identical(x2, x)) {
  writeLines(x2, f, useBytes = TRUE)
  cat("Patched path in:", f, "\n")
} else {
  cat("No Z:/ path assignment found to patch in:", f, "\n")
}
