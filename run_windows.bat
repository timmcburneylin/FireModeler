@echo off
cd /d "%~dp0"
Rscript -e "renv::activate(); source('R/run_pipeline.R')"
pause
