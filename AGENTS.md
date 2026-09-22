# AGENTS.md

Guidance for coding agents working in this repository. See
[CONTRIBUTING.md](CONTRIBUTING.md) and [docs/development.md](docs/development.md)
for the human-facing versions.

## What kon is

kon is a small, fast terminal coding agent in Go. It starts instantly, streams
promptly, and ships as one static binary with no daemon, no account, and no
plugins. It gives the model exactly four tools (`read`, `write`, `edit`,
`shell`) after [pi.dev](https://pi.dev/). Sessions are plain JSONL files a person
can inspect and keep.

The north star is readability end to end. Prefer small, obvious changes over
clever ones, and keep the public product surface narrow.

## Commands

```sh
make fmt          # gofmt cmd internal
make check        # formatting, vet, and shuffled unit tests — the pre-commit gate
make test-race    # race detector; separate because it needs CGO and is slower
make build        # build bin/kon
make smoke        # build and verify the CLI entry point
```

Run `make check` before considering any change done. Tests shuffle package test
order, so do not depend on ordering across packages.

To exercise the first-run TUI without touching real state:

```sh
XDG_CONFIG_HOME="$(mktemp -d)" XDG_DATA_HOME="$(mktemp -d)" go run ./cmd/kon
```

## Dependencies

The provider layer uses only the Go standard library; chat-completions request
bodies and SSE streams are parsed in-tree so token accounting and streaming stay
exact and inspected. JSON, typed-ID generation, files, subprocesses, and release
cross-compilation also use the standard library.

Prefer the standard library. Add a dependency only when it removes substantial,
well-tested platform work. The only current direct dependencies are Bubble Tea,
Bubbles, and Lip Gloss (with goldmark for markdown rendering). Commit `go.mod`
and `go.sum` together and review new transitive dependencies as code. Release
artifacts must remain pure Go.

## Layout

```text
Bubble Tea UI
    │ commands / prompt / events
    ▼
app runtime ────── agent runner ───── provider layer
    │                    │
    │                    ├── provider.Model backends (wire formats)
    │                    └── tool executor
    └── session store ───── append-only JSONL
```

Each package owns one boundary. Do not reach across them.

| Package | Owns | Must not own |
| --- | --- | --- |
| `cmd/kon` | startup wiring and CLI metadata | business logic |
| `internal/ui` | terminal state and presentation | HTTP or JSONL encoding |
| `internal/app` | live runner/store lifecycle and model switching | terminal presentation |
| `internal/agent` | model/tool loop and compaction policy | terminal rendering |
| `internal/provider` | provider `Model` backends and durable-message conversion | session policy |
| `internal/session` | domain messages and append-only context tree | provider requests |
| `internal/tools` | tool registry, bounded tool schemas and execution | agent orchestration |
| `internal/typedid` | identifier construction and parsing | storage or provider policy |
| `internal/config` | paths, defaults, validation, credentials | runtime mutation |
| `internal/contextfiles` | AGENTS.md/CLAUDE.md discovery up the directory tree | prompt assembly |
| `internal/history` | bounded prompt recall | conversation context |

The app runtime is the sole owner of the live store and runner. The runner owns
no terminal state; the UI owns no provider or session serialization.

## Code conventions

- Comments explain **why**, not what. Match the surrounding voice: complete
  sentences, no filler.
- A prompt is assembled once and persisted as the session's root system message.
  It stays byte-identical across compactions — provider prompt caches key on a
  stable leading prefix, and folding anything into it forces a full cache miss.
  Do not rebuild the system prompt on a resumed session.
- Compaction keeps the summary out of the system prompt, projecting it as its
  own user message instead. Preserve that property.
- kon-owned IDs are immutable value objects with unexported storage. Add a
  prefix only for a new durable entity with its own identity: register it in
  `internal/typedid`, use 20 unbiased base62 characters, add JSON rejection
  tests, and document it in `docs/session-format.md`. Current prefixes are
  `ses_` and `ent_`.
- Never validate the shape of an ID owned by a provider. Wrap it in a distinct
  named type (`ToolCallID`, `ModelID`), validate only whether it is semantically
  required, and preserve its bytes exactly at the wire boundary.

## Durability

Session files are an API even before resume is exposed. Changes must retain:

- one complete JSON object per synced line;
- stable, unique typed IDs and earlier parents;
- completed assistant tool calls persisted before their tool results;
- no silent repair except truncating an incomplete final line;
- context assembled by parent traversal, not append order.

No format has shipped yet, so schema-breaking changes update the current version
and fixtures directly. Once a release is published, add an explicit reader
migration or bump the schema version; never guess.

## Testing

Prefer `httptest.Server`, temporary directories, and fake `agent.Provider`
implementations. Never use live provider credentials.

- Tests that exercise SSE must include fragmented events and `[DONE]` behavior.
- Tests that change context must cover repeated compaction and tool-call/result
  adjacency.
- Changes to sessions, context projection, SSE parsing, compaction boundaries,
  or tool execution need focused tests.

## When you finish

- Run `make check`; run `make test-race` for concurrency-sensitive changes.
- Update `CHANGELOG.md` under `## [Unreleased]` for user-visible behavior.
- Update the relevant doc when a boundary, identifier, session record, or
  provider wire type changes: `docs/architecture.md`, `docs/session-format.md`,
  `docs/development.md`, or `README.md`.
