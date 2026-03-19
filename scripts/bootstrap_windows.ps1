Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

Write-Host "Bootstrapping FireModel on Windows..."

if (Get-Command pip -ErrorAction SilentlyContinue) {
  Write-Host "Installing Python dependencies..."
  pip install -r requirements.txt
} else {
  Write-Warning "pip not found; skipping Python dependency install."
}

if (Get-Command Rscript -ErrorAction SilentlyContinue) {
  Write-Host "Restoring R dependencies via renv..."
  Rscript -e "if (!requireNamespace('renv', quietly=TRUE)) install.packages('renv', repos='https://cloud.r-project.org'); renv::restore(prompt = FALSE)"
} else {
  Write-Warning "Rscript not found; skipping renv restore."
}

Write-Host "Done. Copy your data into projects/<project_name>/data/raw/ before running pipeline."
