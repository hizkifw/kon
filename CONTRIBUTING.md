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
test suite. Update `CHANGELOG.md` for user-visible behavior.

Commit `go.mod` and `go.sum` changes together. Review new transitive dependencies
as code. Release artifacts must remain pure Go and below the documented size
limit.

Read [Development](docs/development.md) before changing package boundaries,
identifiers, session records, or provider wire types.
