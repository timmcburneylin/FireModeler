param(
  [switch]$CheckOnly,
  [switch]$SkipDependencySetup
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$RepoRoot = Split-Path -Parent $ScriptDir
Set-Location $RepoRoot

function Write-Step([string]$Message) {
  Write-Host ""
  Write-Host "==> $Message" -ForegroundColor Cyan
}

function Invoke-Git([string[]]$Arguments) {
  $output = & git @Arguments 2>&1
  if ($LASTEXITCODE -ne 0) {
    throw "Git command failed: git $($Arguments -join ' ')`n$($output -join [Environment]::NewLine)"
  }
  return @($output)
}

Write-Step "Checking this FireModeler installation"
Write-Host "Make sure FireModeler is closed before continuing."

if (-not (Get-Command git -ErrorAction SilentlyContinue)) {
  throw "Git is not installed or is not available on PATH. Install Git for Windows once, then rerun this updater."
}

if (-not (Test-Path (Join-Path $RepoRoot ".git"))) {
  throw "This folder is not a Git checkout. Install FireModeler by cloning the public GitHub repository before using automatic updates."
}

$branch = (Invoke-Git @("branch", "--show-current") | Select-Object -First 1).Trim()
if ($branch -ne "main") {
  throw "Automatic updates require the main branch. This installation is currently on '$branch'."
}

$trackedChanges = @(
  @(Invoke-Git @("status", "--porcelain", "--untracked-files=no")) |
    Where-Object { -not [string]::IsNullOrWhiteSpace($_) }
)
if ($trackedChanges.Count -gt 0) {
  throw "Tracked FireModeler files have local changes. Automatic update stopped to avoid overwriting them:`n$($trackedChanges -join [Environment]::NewLine)"
}

Write-Step "Checking GitHub for updates"
Invoke-Git @("fetch", "--quiet", "origin", "main") | Out-Null

$currentCommit = (Invoke-Git @("rev-parse", "HEAD") | Select-Object -First 1).Trim()
$remoteCommit = (Invoke-Git @("rev-parse", "origin/main") | Select-Object -First 1).Trim()

if ($currentCommit -eq $remoteCommit) {
  Write-Host "FireModeler is already up to date." -ForegroundColor Green
  exit 0
}

$behindCount = [int]((Invoke-Git @("rev-list", "--count", "HEAD..origin/main") | Select-Object -First 1).Trim())
$aheadCount = [int]((Invoke-Git @("rev-list", "--count", "origin/main..HEAD") | Select-Object -First 1).Trim())
if ($aheadCount -gt 0) {
  throw "This installation contains local commits that are not on GitHub main. Automatic update stopped."
}

Write-Host "$behindCount update commit(s) are available."
if ($CheckOnly) {
  Write-Host "Check complete; no files were changed." -ForegroundColor Green
  exit 0
}

$changedFiles = @(Invoke-Git @("diff", "--name-only", $currentCommit, $remoteCommit)) |
  Where-Object { -not [string]::IsNullOrWhiteSpace($_) }

Write-Step "Updating FireModeler application files"
Invoke-Git @("pull", "--ff-only", "origin", "main") | ForEach-Object { Write-Host $_ }

$dependencyFiles = @(
  "requirements.txt",
  "renv.lock",
  "scripts/bootstrap_windows.ps1",
  "scripts/install_r_packages_windows.R"
)
$dependencyUpdate = @($changedFiles | Where-Object { $_ -in $dependencyFiles }).Count -gt 0

if ($dependencyUpdate -and -not $SkipDependencySetup) {
  Write-Step "Refreshing changed Python or R dependencies"
  & powershell -NoProfile -ExecutionPolicy Bypass -File (Join-Path $RepoRoot "scripts\bootstrap_windows.ps1")
  if ($LASTEXITCODE -ne 0) {
    throw "Application files were updated, but dependency setup failed with exit code $LASTEXITCODE. Run setup_windows.bat before reopening FireModeler."
  }
} elseif ($dependencyUpdate) {
  Write-Warning "Dependency files changed, but dependency setup was skipped. Run setup_windows.bat before reopening FireModeler."
}

Write-Host ""
Write-Host "FireModeler updated successfully." -ForegroundColor Green
Write-Host "Local projects, machine configuration, and untracked template files were left in place."
