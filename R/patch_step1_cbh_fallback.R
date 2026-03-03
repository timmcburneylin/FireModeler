f <- "R/steps/step1_clean_snap_portable.R"
x <- readLines(f, warn = FALSE)

# Replace the "sample CBH from Species_Only" block with a safe fallback.
pat <- "newCBH\\s*<-\\s*sample\\(Species_Only\\$CBH\\.\\.0\\.1m\\.\\.,\\s*1\\)\\s*\\n\\s*Snap_OS\\[row,\\]\\$CBH\\.\\.0\\.1m\\.\\.\\s*<-\\s*newCBH"

repl <- paste(
  "donors <- Species_Only$CBH..0.1m.[!is.na(Species_Only$CBH..0.1m.)]",
  "if (length(donors) == 0) {",
  "  # Fallback: if we can't borrow CBH from same-species rows, set CBH to (Total Height - 1m), bounded at >= 0",
  "  if (!is.na(d$Total.Height..m.)) {",
  "    Snap_OS[row,]$CBH..0.1m. <- max(d$Total.Height..m. - 1, 0)",
  "  } else {",
  "    Snap_OS[row,]$CBH..0.1m. <- NA_real_",
  "  }",
  "} else {",
  "  Snap_OS[row,]$CBH..0.1m. <- sample(donors, 1)",
  "}",
  sep = "\n"
)

x2 <- gsub(pat, repl, x, perl = TRUE)

if (identical(x2, x)) {
  cat("Patch did NOT apply (pattern not found). We'll patch manually by locating the block.\n")
} else {
  writeLines(x2, f, useBytes = TRUE)
  cat("Patched Step1 CBH fill logic in:", f, "\n")
}
