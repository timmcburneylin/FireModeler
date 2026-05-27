Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$RepoRoot = Split-Path -Parent $ScriptDir
Set-Location $RepoRoot

$SetupTemp = Join-Path $RepoRoot ".tmp"
if (-not (Test-Path $SetupTemp)) {
  New-Item -Path $SetupTemp -ItemType Directory | Out-Null
}
$env:TEMP = $SetupTemp
$env:TMP = $SetupTemp

function Write-Step([string]$Message) {
  Write-Host ""
  Write-Host "==> $Message" -ForegroundColor Cyan
}

function Invoke-Checked([scriptblock]$Command, [string]$Description) {
  & $Command
  if ($LASTEXITCODE -ne 0) {
    throw "$Description failed with exit code $LASTEXITCODE."
  }
}

function Test-WingetAvailable {
  try {
    $null = Get-Command winget -ErrorAction Stop
    return $true
  } catch {
    return $false
  }
}

function Install-WithWinget([string]$PackageId, [string]$Description) {
  if (-not (Test-WingetAvailable)) {
    throw "winget is not available, so $Description could not be installed automatically. Install it manually and rerun setup."
  }

  Write-Step "Installing $Description with winget"
  Write-Host "Package: $PackageId"
  Invoke-Checked {
    & winget install --id $PackageId --source winget --accept-package-agreements --accept-source-agreements
  } "$Description install"
}

function Find-Python {
  $candidates = @(
    @{ Name = "py -3.12"; Command = "py"; Args = @("-3.12") },
    @{ Name = "py -3.11"; Command = "py"; Args = @("-3.11") },
    @{ Name = "py -3.10"; Command = "py"; Args = @("-3.10") },
    @{ Name = "python"; Command = "python"; Args = @() }
  )

  foreach ($candidate in $candidates) {
    try {
      $version = & $candidate.Command @($candidate.Args + @("-c", "import sys; print(f'{sys.version_info.major}.{sys.version_info.minor}')")) 2>$null
      if ($LASTEXITCODE -eq 0) {
        $parts = [string]$version -split "\."
        $major = [int]$parts[0]
        $minor = [int]$parts[1]
        if ($major -eq 3 -and $minor -ge 10 -and $minor -le 12) {
          $candidate.Version = [string]$version
          return $candidate
        }
      }
    } catch {
    }
  }

  return $null
}

function Find-Rscript([switch]$IgnoreConfig) {
  if (-not $IgnoreConfig) {
    $configPaths = @(
      (Join-Path $RepoRoot "config\config.json"),
      (Join-Path $RepoRoot "config\config.example.json")
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

function Get-RVersion([string]$RscriptPath) {
  try {
    $version = & $RscriptPath --vanilla -e 'cat(as.character(getRversion()))' 2>$null
    if ($LASTEXITCODE -eq 0) { return [string]$version }
  } catch {
  }

  return $null
}

function Test-SupportedRVersion([string]$Version) {
  if (-not $Version) { return $false }
  $parts = $Version -split "\."
  if ($parts.Count -lt 2) { return $false }
  $major = [int]$parts[0]
  $minor = [int]$parts[1]
  return ($major -eq 4 -and $minor -ge 5)
}

function Get-PythonVersion([string]$PythonPath) {
  try {
    $version = & $PythonPath -c "import sys; print(f'{sys.version_info.major}.{sys.version_info.minor}')" 2>$null
    if ($LASTEXITCODE -eq 0) { return [string]$version }
  } catch {
  }

  return $null
}

function Test-SupportedPythonVersion([string]$Version) {
  if (-not $Version) { return $false }
  $parts = $Version -split "\."
  if ($parts.Count -lt 2) { return $false }
  $major = [int]$parts[0]
  $minor = [int]$parts[1]
  return ($major -eq 3 -and $minor -ge 10 -and $minor -le 12)
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
  Write-Warning "Could not find Python 3.10, 3.11, or 3.12. Python 3.13 is not supported by this project's pinned dependencies."
  Install-WithWinget "Python.Python.3.12" "Python 3.12"
  $pythonCandidate = Find-Python
  if (-not $pythonCandidate) {
    throw "Python 3.12 was installed or requested, but setup still cannot find a usable Python. Close this terminal, open a new one, and rerun setup_windows.bat. If it still fails, install Python 3.12 manually."
  }
}

Write-Step "Using Python $($pythonCandidate.Version) via $($pythonCandidate.Name)"
$venvPath = Join-Path $RepoRoot ".venv"
$venvPython = Join-Path $venvPath "Scripts\python.exe"
$venvPip = Join-Path $venvPath "Scripts\pip.exe"
if ((Test-Path $venvPython) -and -not (Test-SupportedPythonVersion (Get-PythonVersion $venvPython))) {
  $oldVenvPath = Join-Path $RepoRoot (".venv.unsupported-python-{0:yyyyMMddHHmmss}" -f (Get-Date))
  Write-Warning "Existing .venv uses an unsupported Python version. Moving it to $oldVenvPath."
  Move-Item -Path $venvPath -Destination $oldVenvPath
}

if ((Test-Path $venvPython) -and -not (Test-Path $venvPip)) {
  $oldVenvPath = Join-Path $RepoRoot (".venv.missing-pip-{0:yyyyMMddHHmmss}" -f (Get-Date))
  Write-Warning "Existing .venv is missing pip. Moving it to $oldVenvPath."
  Move-Item -Path $venvPath -Destination $oldVenvPath
}

if (-not (Test-Path $venvPath)) {
  Invoke-Checked { & $pythonCandidate.Command @($pythonCandidate.Args + @("-m", "venv", ".venv")) } "Virtual environment creation"
}

if (-not (Test-Path $venvPython)) {
  throw "Virtual environment creation failed. Missing $venvPython"
}

if (-not (Test-Path $venvPip)) {
  throw "Virtual environment creation failed. Missing $venvPip"
}

Write-Step "Installing Python dependencies into .venv"
Invoke-Checked { & $venvPython -m pip install --upgrade pip setuptools } "Python packaging tools install"
Invoke-Checked { & $venvPython -m pip install -r requirements.txt } "Python dependency install"

$rscript = Find-Rscript
if (-not $rscript) {
  Write-Warning "Could not find Rscript.exe. Installing R with winget."
  Install-WithWinget "RProject.R" "R"
  $rscript = Find-Rscript -IgnoreConfig
  if (-not $rscript) {
    throw "R was installed or requested, but setup still cannot find Rscript.exe. Close this terminal, open a new one, and rerun setup_windows.bat. If it still fails, install R manually and set rscript_path in config\config.json."
  }
}

$rVersion = Get-RVersion $rscript
if (-not (Test-SupportedRVersion $rVersion)) {
  Write-Warning "Found R $rVersion at $rscript, but this project requires R 4.5 or newer. Installing/updating R with winget."
  Install-WithWinget "RProject.R" "R"
  $rscript = Find-Rscript -IgnoreConfig
  $rVersion = Get-RVersion $rscript
  if (-not (Test-SupportedRVersion $rVersion)) {
    throw "Setup still found R $rVersion at $rscript after installing/updating R. Install R 4.5 or newer manually, update config\config.json if needed, then rerun setup."
  }
}

Write-Step "Using R $rVersion at $rscript"
Save-RscriptPath $rscript

Write-Step "Installing R packages"
Invoke-Checked { & $rscript --vanilla "scripts/install_r_packages_windows.R" } "R package install"

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
