# Contributing

kon values small, readable changes. Keep the public product surface narrow and
prefer the Go standard library unless a dependency removes substantial,
well-tested platform work.

Before submitting a change:

```sh
make fmt
make check
make test-race
```

Changes to sessions, context projection, SSE parsing, compaction boundaries, or
tool execution need focused tests. Do not use live provider credentials in the
test suite.

Commit `go.mod` and `go.sum` changes together. Review new transitive dependencies
as code. Release artifacts must remain pure Go and below the documented size
limit.

`scripts/install.sh` and `scripts/install.ps1` are published to users exactly as
they sit in the repository, so keep them dependency-free and test them against a
local mirror before changing them:

```sh
(cd dist && python3 -m http.server 8731) &
KON_BASE_URL=http://127.0.0.1:8731/download KON_VERSION=v0.1.0 \
  KON_INSTALL_DIR=/tmp/konbin sh scripts/install.sh
```

`make release VERSION=vX.Y.Z` writes the archives the scripts expect. The
installers read the download name and checksum layout described in
[Development](docs/development.md); update both together.

Read [Development](docs/development.md) before changing package boundaries,
identifiers, session records, or provider wire types.
