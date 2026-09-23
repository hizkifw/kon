#!/bin/sh
set -eu

# kon upgrade in every published release builds these asset names and parses
# checksums.txt. Changing either breaks upgrades from those releases.

if [ "$#" -ne 1 ]; then
  echo "usage: $0 vX.Y.Z" >&2
  exit 2
fi

version=$1
case "$version" in
  v[0-9]*.[0-9]*.[0-9]*) ;;
  *) echo "version must look like v0.1.0" >&2; exit 2 ;;
esac

root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd -P)
dist="$root/dist"
stage="$dist/.stage"
cd "$root"
rm -rf "$dist"
mkdir -p "$stage"

for target in linux/amd64 linux/arm64 darwin/amd64 darwin/arm64 windows/amd64 windows/arm64; do
  os=${target%/*}
  arch=${target#*/}
  name="kon_${version#v}_${os}_${arch}"
  dir="$stage/$name"
  mkdir -p "$dir"
  binary=kon
  [ "$os" = windows ] && binary=kon.exe
  echo "building $target"
  CGO_ENABLED=0 GOOS="$os" GOARCH="$arch" go build -trimpath -buildvcs=false \
    -ldflags="-s -w -X github.com/hizkifw/kon/internal/buildinfo.version=$version" -o "$dir/$binary" ./cmd/kon
  cp "$root/README.md" "$root/LICENSE" "$root/THIRD_PARTY_NOTICES" "$dir/"
  if [ "$os" = windows ]; then
    archive="$dist/$name.zip"
    (cd "$stage" && zip -qr "$archive" "$name")
  else
    archive="$dist/$name.tar.gz"
    (cd "$stage" && tar -czf "$archive" "$name")
  fi
  size=$(wc -c < "$archive" | tr -d ' ')
  if [ "$size" -gt 10485760 ]; then
    echo "$target archive is larger than 10 MiB ($size bytes)" >&2
    exit 1
  fi
done

rm -rf "$stage"
(cd "$dist" && sha256sum ./*.tar.gz ./*.zip > checksums.txt)
echo "release archives written to $dist"
