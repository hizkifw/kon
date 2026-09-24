# Install kon from a GitHub release archive.
#
#   iwr -useb https://raw.githubusercontent.com/hizkifw/kon/main/scripts/install.ps1 | iex
#
# Environment overrides:
#   KON_VERSION      release tag to install (default: latest, e.g. v0.1.0)
#   KON_INSTALL_DIR  where to put the binary (default: $env:LOCALAPPDATA\Programs\kon)
#   KON_BASE_URL     release base, for mirrors and testing
#   NO_COLOR         disable colored output
#
# This runs through `iex`, so it never calls `exit`: that would close the
# caller's console. Failures surface as errors instead.
$ErrorActionPreference = 'Stop'
# The progress bar is noise here and makes Invoke-WebRequest much slower on
# Windows PowerShell 5.1.
$ProgressPreference = 'SilentlyContinue'

$repo = 'hizkifw/kon'
$baseUrl = if ($env:KON_BASE_URL) { $env:KON_BASE_URL } else { "https://github.com/$repo/releases" }
$installDir = if ($env:KON_INSTALL_DIR) { $env:KON_INSTALL_DIR } else { Join-Path $env:LOCALAPPDATA 'Programs\kon' }
$version = $env:KON_VERSION

# Color only when the user has not opted out. Write-Host to the console keeps
# this to the screen; piped logs then stay plain text. IsOutputRedirected can
# throw when there is no console at all, so treat that as "not a terminal".
$redirected = $true
try { $redirected = [System.Console]::IsOutputRedirected } catch { $redirected = $true }
$useColor = -not $env:NO_COLOR -and -not $redirected
# [char]27 rather than `e: Windows PowerShell 5.1, which `iex` may run under,
# does not support the `e escape. Without a terminal every color stays empty,
# so the plain path emits no escape or bracket noise.
if ($useColor) {
  $esc = [char]27
  $accent = "$esc[38;2;201;138;138m" # kon brand accent, #C98A8A
  $faint  = "$esc[38;2;117;117;117m" # #757575
  $good   = "$esc[38;2;121;201;139m" # #79C98B
  $bad    = "$esc[38;2;224;108;108m" # #E06C6C
  $reset  = "$esc[0m"
} else {
  $accent = ''
  $faint  = ''
  $good   = ''
  $bad    = ''
  $reset  = ''
}

# The kon wordmark from internal/ui/banner.go. The figure carries the brand
# accent and the caption is faint, mirroring the TUI welcome header. The mark
# cannot wrap, so it is printed as-is.
function Write-Banner {
  Write-Host ''
  @(
    '┌──┐              ┌──┐',
    '│  ├──┬─────┬─────┤  │',
    '│  ┌─<│  _  │     ├──┤',
    '└──┴──┴─────┴──┴──┴──┘'
  ) | ForEach-Object { Write-Host "  $accent$_$reset" }
  Write-Host "  ${faint}harness for foxes =˄▾˄=$reset"
  Write-Host ''
}

# Write-Step reports work in progress; Write-Done the one successful outcome.
# Every line is indented two cells to align with the mark above.
function Write-Step { param([string]$Message) Write-Host "  $accent•$reset $Message" }
function Write-Done { param([string]$Message) Write-Host "  $good✓$reset $Message" }
function Write-Bad  { param([string]$Message) Write-Host "  $bad!$reset $Message" }

function Get-Arch {
  $osArch = $null
  if ('System.Runtime.InteropServices.RuntimeInformation' -as [type]) {
    $osArch = [System.Runtime.InteropServices.RuntimeInformation]::OSArchitecture
  }
  # A 32-bit PowerShell process may report x86, so check the host architecture first.
  foreach ($arch in @($osArch, $env:PROCESSOR_ARCHITEW6432, $env:PROCESSOR_ARCHITECTURE)) {
    switch ("$arch") {
      'X64' { return 'amd64' }
      'AMD64' { return 'amd64' }
      'Arm64' { return 'arm64' }
    }
  }
  throw "unsupported architecture: OSArchitecture=$osArch, PROCESSOR_ARCHITEW6432=$env:PROCESSOR_ARCHITEW6432, PROCESSOR_ARCHITECTURE=$env:PROCESSOR_ARCHITECTURE"
}

Write-Banner

if (-not $version) {
  $latest = Invoke-RestMethod -Uri "https://api.github.com/repos/$repo/releases/latest" `
    -Headers @{ 'User-Agent' = 'kon-installer' }
  $version = $latest.tag_name
}
if (-not $version.StartsWith('v')) { $version = "v$version" }

$arch = Get-Arch
$name = "kon_$($version.TrimStart('v'))_windows_$arch"
$assetUrl = "$baseUrl/download/$version"

$tmp = Join-Path ([System.IO.Path]::GetTempPath()) ("kon-" + [guid]::NewGuid().ToString('n'))
New-Item -ItemType Directory -Path $tmp | Out-Null

try {
  Write-Step "downloading kon $version (windows/$arch)"
  $zipPath = Join-Path $tmp "$name.zip"
  Invoke-WebRequest -Uri "$assetUrl/$name.zip" -OutFile $zipPath -UseBasicParsing
  # GitHub serves this as octet-stream, which comes back as a byte array from
  # Invoke-WebRequest. Read it as a file so the text survives.
  $checksumPath = Join-Path $tmp 'checksums.txt'
  Invoke-WebRequest -Uri "$assetUrl/checksums.txt" -OutFile $checksumPath -UseBasicParsing
  $checksums = Get-Content -Path $checksumPath

  Write-Step 'verifying checksum'
  $expected = (
    $checksums |
      ForEach-Object { $fields = $_ -split '\s+'; if ($fields.Count -ge 2) { [pscustomobject]@{
        Hash = $fields[0]
        File = $fields[-1] -replace '^\./', ''
      } } } |
      Where-Object { $_ -and $_.File -eq "$name.zip" } |
      Select-Object -ExpandProperty Hash -First 1
  )
  if (-not $expected) { throw "no checksum for $name.zip" }

  $actual = (Get-FileHash -Algorithm SHA256 -Path $zipPath).Hash.ToLower()
  if ($actual -ne $expected.ToLower()) { throw "checksum mismatch for $name.zip" }

  Write-Step "installing kon $version"
  Expand-Archive -Path $zipPath -DestinationPath $tmp
  New-Item -ItemType Directory -Path $installDir -Force | Out-Null
  $binary = Join-Path $installDir 'kon.exe'
  Copy-Item -Path (Join-Path $tmp "$name\kon.exe") -Destination $binary -Force

  # The new binary applies pending storage migrations and refreshes the model
  # catalog. Its own lines name internal steps ("check unmarked storage") that
  # read as noise on a fresh install, so keep them back unless something fails.
  Write-Step 'preparing kon'
  $logPath = Join-Path $tmp 'finalize.log'
  $finalizeOk = $true
  # kon writes migration progress to stderr. Under $ErrorActionPreference
  # 'Stop', Windows PowerShell turns redirected native stderr into a
  # terminating error, so relax the preference for this call and restore it.
  $prevEap = $ErrorActionPreference
  $ErrorActionPreference = 'Continue'
  try {
    & $binary upgrade --finalize *> $logPath
    if ($LASTEXITCODE -ne 0) { $finalizeOk = $false }
  } catch {
    $finalizeOk = $false
  } finally {
    $ErrorActionPreference = $prevEap
  }

  Write-Done "kon $version installed to $binary"

  if (-not $finalizeOk) {
    Write-Bad "finishing install failed; re-run `"$binary`" upgrade --finalize to retry"
    if (Test-Path $logPath) { Get-Content -Path $logPath | ForEach-Object { "    $_" } }
  }
} finally {
  Remove-Item -Path $tmp -Recurse -Force -ErrorAction SilentlyContinue
}

# Add the install directory to the user PATH once, so new shells find kon.
$userPath = [Environment]::GetEnvironmentVariable('Path', 'User')
$entries = @($userPath -split ';' | Where-Object { $_ })
if ($entries -notcontains $installDir) {
  $newPath = (@($entries) + $installDir) -join ';'
  [Environment]::SetEnvironmentVariable('Path', $newPath, 'User')
  Write-Host "added $installDir to your PATH; open a new terminal to use kon"
}
$env:Path = "$env:Path;$installDir"
