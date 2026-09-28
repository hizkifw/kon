#!/bin/sh
set -eu

# Writes checksums.txt for the release archives in a directory. kon upgrade in
# every published release parses it as sha256sum output with each path written
# "./name", tar.gz archives before zip ones, so the format must not change.

if [ "$#" -ne 1 ]; then
  echo "usage: $0 DIR" >&2
  exit 2
fi

cd "$1"
set --
for archive in ./*.tar.gz ./*.zip; do
  # A pattern that matches nothing stays as it is written.
  [ -e "$archive" ] && set -- "$@" "$archive"
done
if [ "$#" -eq 0 ]; then
  echo "no release archives in $PWD" >&2
  exit 1
fi
sha256sum "$@" > checksums.txt
