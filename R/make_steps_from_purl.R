purl <- file.path(getwd(), "R", "generated", "firemodel_purl.R")
if (!file.exists(purl)) stop("Missing: ", purl)

x <- readLines(purl, warn = FALSE)

# ---- Step boundaries (first-pass, adjust if needed) ----
L1_end <- 378
L2_end <- 2000

if (length(x) < L2_end) stop("purl file shorter than expected; adjust boundaries")

dir.create(file.path(getwd(), "R", "steps"), recursive=TRUE, showWarnings=FALSE)

write_step <- function(fname, lines, label){
  out <- file.path(getwd(), "R", "steps", fname)
  header <- c(
    "# AUTO-GENERATED from R/generated/firemodel_purl.R",
    paste0("# ", label),
    "source('R/common.R')",
    "cat('", label, " starting...\\n')",
    ""
  )
  footer <- c("", paste0("cat('", label, " complete.\\n')"))
  writeLines(c(header, lines, footer), out)
  cat("Wrote:", out, "\n")
}

write_step("step1_clean_snap.R", x[1:L1_end], "Step 1: Clean SNAP")
write_step("step2_fuelcalc.R",   x[(L1_end+1):L2_end], "Step 2: FuelCalc")
write_step("step3_fire_model.R", x[(L2_end+1):length(x)], "Step 3: Fire modeling")
