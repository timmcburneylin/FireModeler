f <- "R/steps/step1_clean_snap_portable.R"
x <- readLines(f, warn = FALSE)

keep <- c("dplyr","tidyr","stringr","readr","tibble","purrr","magrittr","glue")

is_library <- grepl("^[[:space:]]*library\\(", x)

x[is_library] <- vapply(x[is_library], function(line) {
  pkg <- sub("^[[:space:]]*library\\(([^)]+)\\).*", "\\1", line)
  pkg <- gsub("[\"'[:space:]]", "", pkg)
  if (pkg %in% keep) line else paste0("# ", line, "  # dropped for portability")
}, character(1))

writeLines(x, f)
cat("Patched libraries in:", f, "\n")