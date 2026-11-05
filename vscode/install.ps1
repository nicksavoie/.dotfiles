# Installs the VSCode settings and extensions on Windows.
# Run from the repo root in PowerShell:  .\vscode\install.ps1
#
# settings.json is symlinked when Windows allows it (Developer Mode or an admin
# shell), otherwise copied; re-run after pulling if it was copied.
# Not yet run on a real Win11 machine; check the first run.

$ErrorActionPreference = "Stop"

$here = Split-Path -Parent $MyInvocation.MyCommand.Path
$userDir = Join-Path $env:APPDATA "Code\User"
New-Item -ItemType Directory -Force -Path $userDir | Out-Null

$target = Join-Path $userDir "settings.json"
$source = Join-Path $here "settings.json"

if (Test-Path $target) {
    $backup = "$target.bak"
    Move-Item -Force $target $backup
    Write-Host "Existing settings.json moved to $backup"
}

try {
    New-Item -ItemType SymbolicLink -Path $target -Target $source | Out-Null
    Write-Host "Linked settings.json -> $source"
} catch {
    Copy-Item $source $target
    Write-Host "Symlinks are not allowed here; copied settings.json instead."
}

if (-not (Get-Command code -ErrorAction SilentlyContinue)) {
    Write-Warning "'code' is not on PATH; skipping extensions."
    exit 0
}

Get-Content (Join-Path $here "extensions.txt") |
    Where-Object { $_.Trim() -ne "" } |
    ForEach-Object { code --install-extension $_.Trim() --force }
