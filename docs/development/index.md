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
make bench
make loadtest
```

`make check` is the normal pre-commit gate. Tests shuffle package test order.
The race target is separate because it requires CGO and is slower. `make smoke`
builds the normal development binary and verifies its metadata entry point.
`make bench` and `make loadtest` measure performance; see
[benchmarking.md](benchmarking.md).

To exercise the full first-run TUI without touching real state, set temporary
roots before launching:

```sh
XDG_CONFIG_HOME="$(mktemp -d)" XDG_DATA_HOME="$(mktemp -d)" go run ./cmd/kon
```

## Package map

Packages under `core/` are the reusable half of kon and its public Go API: the
loop, providers, sessions, and the tool contract. They never import `internal/`
or `cmd/` (`core/boundary_test.go` enforces it), so another program can drive
them with its own prompt and tools. Everything kon-specific lives in `internal/`.

| Package | Owns | Must not own |
| --- | --- | --- |
| `cmd/kon` | startup wiring and CLI metadata | business logic |
| `docs/product` | bundled user guide and on-demand extraction | terminal state or config mutation |
| `internal/ui` | terminal state and presentation | HTTP or JSONL encoding |
| `internal/headless` | `kon run` output: streamed text or JSON events, one run per process | terminal state or session policy |
| `internal/app` | live runner/store lifecycle and model switching | terminal presentation |
| `core/agent` | the model/tool loop, retry events, and compaction policy | prompts, concrete tools, or terminal rendering |
| `core/tool` | the tool contract: `Tool`, `Registry`, and `Executor` | concrete tools or their presentation |
| `core/provider` | provider `Model` backends and durable-message conversion | session policy |
| `core/provider/wire` | the wire-format table: names, default endpoints, and dialect facts | HTTP, backends, or service quirks |
| `internal/login` | `/login` choices per service and connection verification | wire backends or config writes |
| `internal/catalog` | bundled model metadata and local refresh cache | provider requests or configuration writes |
| `core/session` | domain messages and append-only context tree | provider requests |
| `internal/migrate` | storage version tracking and process locks | session format conversion |
| `internal/migrations` | ordered storage conversion steps | live runtime state |
| `internal/codetools` | kon's coding tools: bounded schemas, execution, and transcript displays | agent orchestration |
| `internal/prompt` | kon's system prompt | context-file discovery |
| `internal/web` | `kon tool` web access: fetching pages as Markdown | tool schemas or agent state |
| `core/typedid` | identifier construction and parsing | storage or provider policy |
| `core/tokens` | the token count type and its compact display | usage policy or estimation |
| `internal/buildinfo` | build version and the outgoing User-Agent | configuration or network clients |
| `internal/config` | paths, defaults, validation, credentials | runtime mutation |
| `internal/contextfiles` | AGENTS.md/CLAUDE.md discovery up the directory tree | prompt assembly |
| `internal/history` | bounded prompt recall | conversation context |
| `internal/selfupdate` | release lookup, verified download, and executable replacement | storage migrations or CLI parsing |

Known structural debt and the planned restructuring order are tracked in
[architecture-review.md](architecture-review.md).

## Identifier rules

kon-owned IDs are immutable value objects with unexported storage. Add a prefix
only when introducing a new durable entity with its own identity. Register it in
`core/typedid`, use 20 unbiased base62 characters, add JSON rejection tests,
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

Schema-breaking changes need a named migration in the ordered registry. See
[migrations.md](migrations.md) for the authoring and retry contract.

## Release artifacts

The bundled models.dev snapshot is committed at `internal/catalog/snapshot.json.gz`,
so `go install` and ordinary builds work offline. Run `make catalog-update` to
fetch and validate a new snapshot, then review and commit the generated file.

Release builds do not use the committed file. On a version tag, the `catalog`
job in `.github/workflows/ci.yml` runs `make catalog-update` once and every
target embeds that result, so a release ships the catalog as of its release
day.

Tests must not depend on what the snapshot lists, since models.dev adds and
drops models freely. `internal/app` tests read `internal/app/testdata/catalog.json`
instead; add any catalog entry a new test needs there.

At runtime, `kon models` reads the bundled snapshot or newer local cache.
`kon models --refresh` and `kon upgrade` are the only commands that request
the latest catalog from models.dev. Starting kon never refreshes it.

`/login <provider>` explicitly checks a configured provider endpoint and
caches its returned model IDs in the data directory. It does not refresh the
models.dev catalog. Explicit profiles and derived provider/model names are
merged by the app runtime without rewriting the user's `models` array.

Product pages live in `docs/product` and are embedded in the binary. `kon docs`
extracts only those Markdown files to a content-versioned directory under the
data path. Development pages stay in `docs/development` and are not embedded.

`scripts/release.sh VERSION [OS/ARCH...]` cross-builds every target, or the
ones named, and writes `dist/`:

```text
kon_<version>_<os>_<arch>.tar.gz   linux and darwin, containing kon/, README, LICENSE, THIRD_PARTY_NOTICES
kon_<version>_<os>_<arch>.zip      windows, same contents with kon.exe
checksums.txt                      sha256 of every archive, written by scripts/checksums.sh
```

CI builds each target in a job of its own with the same script, so every push
checks that each target builds and fits the size limit. On a version tag the
archives carry the tag's version, and once the tests pass the release job
publishes them with a `checksums.txt` written over all of them, without
building again.

Each complete release archive must be at most 10 MiB. The installer extracts
it once, leaving the executable uncompressed for subsequent launches.

`scripts/install.sh` (POSIX) and `scripts/install.ps1` (PowerShell) consume
exactly that layout: they resolve the tag from the `/releases/latest` redirect
or the `releases/latest` API, download the matching archive plus `checksums.txt`
from `/releases/download/<tag>/`, verify the hash, extract, and place the binary
on `PATH`. Both then run the installed binary as `kon upgrade --finalize`, which
applies any pending storage migrations and refreshes the model catalog, so the
first launch starts clean. A finalize failure only warns: migrations are
retry-safe and run again at next startup. That subcommand's own progress lines
name internal migration steps, so both installers capture its output and surface
it only when it fails; the interactive run prints the wordmark from
`internal/ui/banner.go`, a bullet per step, and one confirmation line once
everything is done. Color follows the terminal and `NO_COLOR`; a piped run stays
plain. Both are self-contained, so they can be piped from
`raw.githubusercontent.com` straight into `sh` or `iex`.
The PowerShell installer selects the Windows archive from `RuntimeInformation`
when available, then falls back to the host and process architecture environment
variables for older Windows PowerShell runtimes.

## Testing seams

The provider, agent, tools, session store, and Bubble Tea model can all be tested
without a real terminal or paid API. Prefer `httptest.Server`, temporary
directories, and fake `agent.Provider` implementations. Tests that exercise SSE
must include fragmented events and `[DONE]` behavior. Tests that change context
must cover repeated compaction and tool-call/result adjacency.
