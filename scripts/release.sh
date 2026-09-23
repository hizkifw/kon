#!/bin/sh
set -eu

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
    -ldflags="-s -w -X main.version=$version" -o "$dir/$binary" ./cmd/kon
  size=$(wc -c < "$dir/$binary" | tr -d ' ')
  if [ "$size" -gt 10485760 ]; then
    echo "$target binary is larger than 10 MiB ($size bytes)" >&2
    exit 1
  fi
  cp "$root/README.md" "$root/LICENSE" "$root/THIRD_PARTY_NOTICES" "$dir/"
  if [ "$os" = windows ]; then
    (cd "$stage" && zip -qr "$dist/$name.zip" "$name")
  else
    (cd "$stage" && tar -czf "$dist/$name.tar.gz" "$name")
  fi
done

rm -rf "$stage"
(cd "$dist" && sha256sum ./*.tar.gz ./*.zip > checksums.txt)
echo "release archives written to $dist"
