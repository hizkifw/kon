#!/bin/sh
# Install kon from a GitHub release archive.
#
#   curl -fsSL https://raw.githubusercontent.com/hizkifw/kon/main/scripts/install.sh | sh
#
# Environment overrides:
#   KON_VERSION      release tag to install (default: latest, e.g. v0.1.0)
#   KON_INSTALL_DIR  where to put the binary (default: $HOME/.local/bin)
#   KON_BASE_URL     release base, for mirrors and testing
set -eu

repo=hizkifw/kon
base_url=${KON_BASE_URL:-https://github.com/$repo/releases}
install_dir=${KON_INSTALL_DIR:-"$HOME/.local/bin"}
version=${KON_VERSION:-}

die() {
  printf 'kon: %s\n' "$*" >&2
  exit 1
}

need() {
  command -v "$1" >/dev/null 2>&1 || die "missing required command: $1"
}

need curl
need tar
need mktemp

case "$(uname -s)" in
  Linux) os=linux ;;
  Darwin) os=darwin ;;
  MINGW* | MSYS* | CYGWIN*)
    die "this is Windows; run the PowerShell installer instead:
  iwr -useb https://raw.githubusercontent.com/$repo/main/scripts/install.ps1 | iex"
    ;;
  *) die "unsupported operating system: $(uname -s)" ;;
esac

case "$(uname -m)" in
  x86_64 | amd64) arch=amd64 ;;
  arm64 | aarch64) arch=arm64 ;;
  *) die "unsupported architecture: $(uname -m)" ;;
esac

# The /releases/latest redirect names the tag without API quota or a token.
# For a few minutes after a release is published GitHub serves the generic
# releases page instead, so fall back to the API before giving up.
if [ -z "$version" ]; then
  version=$(curl -fsSL -o /dev/null -w '%{url_effective}' "$base_url/latest" | sed 's#/*$##; s#.*/##')
  case "$version" in
    '' | latest)
      json=$(curl -fsSL -H 'Accept: application/vnd.github+json' \
        -H 'User-Agent: kon-installer' \
        "https://api.github.com/repos/$repo/releases/latest") ||
        die "could not determine the latest release; set KON_VERSION to install a specific release"
      version=$(printf '%s\n' "$json" | sed -n 's/.*"tag_name"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p' | head -1)
      ;;
  esac
fi
[ -n "$version" ] || die "could not determine the latest release; set KON_VERSION"
case "$version" in
  v*) ;;
  *) version="v$version" ;;
esac

name="kon_${version#v}_${os}_${arch}"
url="$base_url/download/$version"

tmp=$(mktemp -d "${TMPDIR:-/tmp}/kon.XXXXXX")
trap 'rm -rf "$tmp"' EXIT HUP INT TERM

printf 'downloading kon %s (%s/%s)\n' "$version" "$os" "$arch" >&2
curl -fsSL -o "$tmp/$name.tar.gz" "$url/$name.tar.gz" ||
  die "download failed: $url/$name.tar.gz"
curl -fsSL -o "$tmp/checksums.txt" "$url/checksums.txt" ||
  die "download failed: $url/checksums.txt"

expected=$(awk -v n="$name.tar.gz" '{ sub(/^\.\//, "", $2) } $2 == n { print $1; exit }' "$tmp/checksums.txt")
[ -n "$expected" ] || die "no checksum for $name.tar.gz"

if command -v sha256sum >/dev/null 2>&1; then
  actual=$(sha256sum "$tmp/$name.tar.gz" | awk '{ print $1 }')
elif command -v shasum >/dev/null 2>&1; then
  actual=$(shasum -a 256 "$tmp/$name.tar.gz" | awk '{ print $1 }')
else
  die "need sha256sum or shasum to verify the download"
fi
[ "$actual" = "$expected" ] || die "checksum mismatch for $name.tar.gz"

tar -xzf "$tmp/$name.tar.gz" -C "$tmp" || die "could not extract $name.tar.gz"
mkdir -p "$install_dir"
cp "$tmp/$name/kon" "$install_dir/kon"
chmod 0755 "$install_dir/kon"

printf 'installed kon %s to %s\n' "$version" "$install_dir/kon" >&2
case ":${PATH:-}:" in
  *":$install_dir:"*) ;;
  *)
    printf '\n%s is not on your PATH. Add it:\n  export PATH="%s:$PATH"\n' \
      "$install_dir" "$install_dir" >&2
    ;;
esac
