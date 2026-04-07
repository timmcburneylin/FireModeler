Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$RepoRoot = Split-Path -Parent $ScriptDir
Set-Location $RepoRoot

function Write-Step([string]$Message) {
  Write-Host ""
  Write-Host "==> $Message" -ForegroundColor Cyan
}

function Find-Python {
  $candidates = @(
    @{ Name = "py -3.13"; Command = "py"; Args = @("-3.13") },
    @{ Name = "py -3.12"; Command = "py"; Args = @("-3.12") },
    @{ Name = "py -3.11"; Command = "py"; Args = @("-3.11") },
    @{ Name = "py -3.10"; Command = "py"; Args = @("-3.10") },
    @{ Name = "python"; Command = "python"; Args = @() }
  )

  foreach ($candidate in $candidates) {
    try {
      $null = & $candidate.Command @($candidate.Args + @("-c", "import sys; print(sys.executable)")) 2>$null
      if ($LASTEXITCODE -eq 0) {
        return $candidate
      }
    } catch {
    }
  }

  return $null
}

function Find-Rscript {
  $configPaths = @(
    Join-Path $RepoRoot "config\config.json",
    Join-Path $RepoRoot "config\config.example.json"
  )

  foreach ($configPath in $configPaths) {
    if (-not (Test-Path $configPath)) { continue }
    try {
      $config = Get-Content $configPath -Raw | ConvertFrom-Json
      $configured = [string]$config.rscript_path
      if ($configured -and (Test-Path $configured)) {
        return $configured
      }
    } catch {
    }
  }

  foreach ($cmd in @("Rscript.exe", "Rscript")) {
    try {
      $found = (Get-Command $cmd -ErrorAction Stop).Source
      if ($found) { return $found }
    } catch {
    }
  }

  $programFiles = @($env:ProgramFiles, ${env:ProgramFiles(x86)}) | Where-Object { $_ }
  foreach ($base in $programFiles) {
    $rRoot = Join-Path $base "R"
    if (-not (Test-Path $rRoot)) { continue }
    $versions = Get-ChildItem $rRoot -Directory | Where-Object { $_.Name -like "R-*" } | Sort-Object Name -Descending
    foreach ($version in $versions) {
      foreach ($rel in @("bin\Rscript.exe", "bin\x64\Rscript.exe")) {
        $candidate = Join-Path $version.FullName $rel
        if (Test-Path $candidate) {
          return $candidate
        }
      }
    }
  }

  return $null
}

function Save-RscriptPath([string]$RscriptPath) {
  $configPath = Join-Path $RepoRoot "config\config.json"
  if (Test-Path $configPath) {
    $config = Get-Content $configPath -Raw | ConvertFrom-Json
  } else {
    $examplePath = Join-Path $RepoRoot "config\config.example.json"
    $config = Get-Content $examplePath -Raw | ConvertFrom-Json
  }

  $config | Add-Member -NotePropertyName "rscript_path" -NotePropertyValue $RscriptPath -Force
  $config | ConvertTo-Json -Depth 20 | Set-Content -Path $configPath -Encoding UTF8
}

Write-Step "Bootstrapping FireModel on Windows"
Write-Host "Repo root: $RepoRoot"

$pythonCandidate = Find-Python
if (-not $pythonCandidate) {
  throw "Could not find a usable Python installation. Install Python 3.10+ and rerun this script."
}

Write-Step "Using Python via $($pythonCandidate.Name)"
$venvPath = Join-Path $RepoRoot ".venv"
if (-not (Test-Path $venvPath)) {
  & $pythonCandidate.Command @($pythonCandidate.Args + @("-m", "venv", ".venv"))
}

$venvPython = Join-Path $venvPath "Scripts\python.exe"
if (-not (Test-Path $venvPython)) {
  throw "Virtual environment creation failed. Missing $venvPython"
}

Write-Step "Installing Python dependencies into .venv"
& $venvPython -m pip install --upgrade pip setuptools wheel
& $venvPython -m pip install -r requirements.txt

$rscript = Find-Rscript
if (-not $rscript) {
  throw "Could not find Rscript.exe. Install R 4.5.x or set rscript_path in config\config.json, then rerun this script."
}

Write-Step "Using Rscript at $rscript"
Save-RscriptPath $rscript

Write-Step "Restoring R packages with renv"
& $rscript -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); if (!requireNamespace('renv', quietly = TRUE)) install.packages('renv'); renv::restore(project = getwd(), prompt = FALSE)"

$fuelCalcRoot = "C:\Program Files (x86)\FuelCalc1.7"
Write-Step "Checking FuelCalc"
if (Test-Path $fuelCalcRoot) {
  Write-Host "FuelCalc found at $fuelCalcRoot" -ForegroundColor Green
} else {
  Write-Warning "FuelCalc was not found at '$fuelCalcRoot'. The app can still open, but FuelCalc batch runs will fail until FuelCalc is installed."
}

Write-Step "Done"
Write-Host "Next steps:"
Write-Host "1. Activate the venv: .\.venv\Scripts\Activate.ps1"
Write-Host "2. Launch the app: python -m streamlit run app/app.py"
