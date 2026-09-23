# Development

This guide is the shortest path from checkout to a safe change.

## Commands

```sh
make help
make deps
make fmt
make check
make test-race
make smoke
```

`make check` is the normal pre-commit gate. Tests shuffle package test order.
The race target is separate because it requires CGO and is slower. `make smoke`
builds the normal development binary and verifies its metadata entry point.

To exercise the full first-run TUI without touching real state, set temporary
roots before launching:

```sh
XDG_CONFIG_HOME="$(mktemp -d)" XDG_DATA_HOME="$(mktemp -d)" go run ./cmd/kon
```

## Package map

| Package | Owns | Must not own |
| --- | --- | --- |
| `cmd/kon` | startup wiring and CLI metadata | business logic |
| `docs/product` | bundled user guide and on-demand extraction | terminal state or config mutation |
| `internal/ui` | terminal state and presentation | HTTP or JSONL encoding |
| `internal/app` | live runner/store lifecycle and model switching | terminal presentation |
| `internal/agent` | model/tool loop and compaction policy | terminal rendering |
| `internal/provider` | provider `Model` backends and durable-message conversion | session policy |
| `internal/catalog` | bundled model metadata and local refresh cache | provider requests or configuration writes |
| `internal/session` | domain messages and append-only context tree | provider requests |
| `internal/tools` | tool registry, bounded tool schemas and execution | agent orchestration |
| `internal/typedid` | identifier construction and parsing | storage or provider policy |
| `internal/config` | paths, defaults, validation, credentials | runtime mutation |
| `internal/contextfiles` | AGENTS.md/CLAUDE.md discovery up the directory tree | prompt assembly |
| `internal/history` | bounded prompt recall | conversation context |

Known structural debt and the planned restructuring order are tracked in
[architecture-review.md](architecture-review.md).

## Identifier rules

kon-owned IDs are immutable value objects with unexported storage. Add a prefix
only when introducing a new durable entity with its own identity. Register it in
`internal/typedid`, use 20 unbiased base62 characters, add JSON rejection tests,
and document it in the session format.

Current prefixes:

| Type | Prefix | Example |
| --- | --- | --- |
| Session | `ses_` | `ses_7Yk2mP9Qa4Zx8Vc1Nd6R` |
| Entry | `ent_` | `ent_B3mN8qL2xR7vK5cT9Za1` |

Never validate the shape of an ID owned by a provider. Wrap it in a distinct
named type such as `ToolCallID` or `ModelID`, validate only whether a field is
semantically required, and preserve its bytes exactly at the wire boundary.

## Durable changes

Session files are an API even before resume is exposed. Changes must retain
these invariants:

- one complete JSON object per synced line;
- stable, unique typed IDs and earlier parents;
- completed assistant tool calls persisted before their tool results;
- no silent repair except truncating an incomplete final line;
- context assembled by parent traversal rather than append order.

Because no format has shipped yet, schema-breaking changes should update the
current version and fixtures directly. Once a release is published, add an
explicit reader migration or bump the schema version; never guess.

## Release artifacts

The bundled models.dev snapshot is committed at `internal/catalog/snapshot.json.gz`.
Run `make catalog-update` to fetch and validate a new snapshot, then review and
commit the generated file. Ordinary builds, tests, and releases use that file
and do not need network access or a JavaScript toolchain.

At runtime, `kon models` reads the bundled snapshot or newer local cache;
`kon models --refresh` is the only user-facing command that requests the latest
catalog from models.dev. Starting kon never refreshes it automatically.

`/login <provider>` explicitly checks a configured provider endpoint and
caches its returned model IDs in the data directory. It does not refresh the
models.dev catalog. Explicit profiles and derived provider/model names are
merged by the app runtime without rewriting the user's `models` array.

Product pages live in `docs/product` and are embedded in the binary. `kon docs`
extracts only those Markdown files to a content-versioned directory under the
data path. Development pages stay in `docs/development` and are not embedded.

`scripts/release.sh VERSION` cross-builds every target and writes `dist/`:

```text
kon_<version>_<os>_<arch>.tar.gz   linux and darwin, containing kon/, README, LICENSE, THIRD_PARTY_NOTICES
kon_<version>_<os>_<arch>.zip      windows, same contents with kon.exe
checksums.txt                      sha256 of every archive
```

Each complete release archive must be at most 10 MiB. The installer extracts
it once, leaving the executable uncompressed for subsequent launches.

`scripts/install.sh` (POSIX) and `scripts/install.ps1` (PowerShell) consume
exactly that layout: they resolve the tag from the `/releases/latest` redirect
or the `releases/latest` API, download the matching archive plus `checksums.txt`
from `/releases/download/<tag>/`, verify the hash, extract, and place the binary
on `PATH`. Both are self-contained, so they can be piped from `raw.githubusercontent.com`
straight into `sh` or `iex`.

## Testing seams

The provider, agent, tools, session store, and Bubble Tea model can all be tested
without a real terminal or paid API. Prefer `httptest.Server`, temporary
directories, and fake `agent.Provider` implementations. Tests that exercise SSE
must include fragmented events and `[DONE]` behavior. Tests that change context
must cover repeated compaction and tool-call/result adjacency.
