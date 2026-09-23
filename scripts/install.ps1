# Install kon from a GitHub release archive.
#
#   iwr -useb https://raw.githubusercontent.com/hizkifw/kon/main/scripts/install.ps1 | iex
#
# Environment overrides:
#   KON_VERSION      release tag to install (default: latest, e.g. v0.1.0)
#   KON_INSTALL_DIR  where to put the binary (default: $env:LOCALAPPDATA\Programs\kon)
#   KON_BASE_URL     release base, for mirrors and testing
#
# This runs through `iex`, so it never calls `exit`: that would close the
# caller's console. Failures surface as errors instead.
$ErrorActionPreference = 'Stop'

$repo = 'hizkifw/kon'
$baseUrl = if ($env:KON_BASE_URL) { $env:KON_BASE_URL } else { "https://github.com/$repo/releases" }
$installDir = if ($env:KON_INSTALL_DIR) { $env:KON_INSTALL_DIR } else { Join-Path $env:LOCALAPPDATA 'Programs\kon' }
$version = $env:KON_VERSION

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
  Write-Host "downloading kon $version (windows/$arch)"
  $zipPath = Join-Path $tmp "$name.zip"
  Invoke-WebRequest -Uri "$assetUrl/$name.zip" -OutFile $zipPath -UseBasicParsing
  # GitHub serves this as octet-stream, which comes back as a byte array from
  # Invoke-WebRequest. Read it as a file so the text survives.
  $checksumPath = Join-Path $tmp 'checksums.txt'
  Invoke-WebRequest -Uri "$assetUrl/checksums.txt" -OutFile $checksumPath -UseBasicParsing
  $checksums = Get-Content -Path $checksumPath

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

  Expand-Archive -Path $zipPath -DestinationPath $tmp
  New-Item -ItemType Directory -Path $installDir -Force | Out-Null
  $binary = Join-Path $installDir 'kon.exe'
  Copy-Item -Path (Join-Path $tmp "$name\kon.exe") -Destination $binary -Force

  Write-Host "installed kon $version to $binary"
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
