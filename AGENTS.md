kon is a terminal coding agent in Go. It follows these core principles:

- It must start up instantly
- Feels snappy to use
- Single self-contained binary
- Avoids vendor lock-in by using plain files to store configuration and sessions
- Maintains high prompt prefix cache hit rate by avoiding mutation of early messages unless absolutely necessary

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

Prefer the standard library. Add a dependency only when it removes substantial,
well-tested platform work. The only current direct dependencies are Bubble Tea,
Bubbles, and Lip Gloss (with goldmark for markdown rendering).

Each package owns one boundary. Do not reach across them.

| Package | Owns | Must not own |
| --- | --- | --- |
| `cmd/kon` | startup wiring and CLI metadata | business logic |
| `internal/ui` | terminal state and presentation | HTTP or JSONL encoding |
| `internal/app` | live runner/store lifecycle and model switching | terminal presentation |
| `internal/agent` | model/tool loop and compaction policy | terminal rendering |
| `internal/provider` | provider `Model` backends and durable-message conversion | session policy |
| `internal/provider/wire` | the wire-format table: names, default endpoints, and dialect facts | HTTP, backends, or service quirks |
| `internal/login` | `/login` choices per service and connection verification | wire backends or config writes |
| `internal/session` | domain messages and append-only context tree | provider requests |
| `internal/tools` | tool registry, bounded tool schemas and execution | agent orchestration |
| `internal/typedid` | identifier construction and parsing | storage or provider policy |
| `internal/tokens` | the token count type and its compact display | usage policy or estimation |
| `internal/buildinfo` | build version and the outgoing User-Agent | configuration or network clients |
| `internal/config` | paths, defaults, validation, credentials | runtime mutation |
| `internal/contextfiles` | AGENTS.md/CLAUDE.md discovery up the directory tree | prompt assembly |
| `internal/history` | bounded prompt recall | conversation context |
| `internal/selfupdate` | release lookup, verified download, and executable replacement | storage migrations or CLI parsing |

The app runtime is the sole owner of the live store and runner. The runner owns
no terminal state; the UI owns no provider or session serialization.

## Code conventions

- Comments explain **why**, not what. Match the surrounding voice: complete
  sentences, no filler.
- A prompt is assembled once and persisted as the session's root system message.
  It stays byte-identical across compactions — provider prompt caches key on a
  stable leading prefix, and folding anything into it forces a full cache miss.
  Do not rebuild the system prompt on a resumed session.
- kon-owned IDs are immutable value objects with unexported storage. Add a
  prefix only for a new durable entity with its own identity: register it in
  `internal/typedid`, use 20 unbiased base62 characters, add JSON rejection
  tests, and document it in `docs/development/session-format.md`.
- Never validate the shape of an ID owned by a provider. Wrap it in a distinct
  named type (`ToolCallID`, `ModelID`), validate only whether it is semantically
  required, and preserve its bytes exactly at the wire boundary.

## When you finish

- Run `make check`; run `make test-race` for concurrency-sensitive changes.
- Update the relevant docs: `docs/*`, `README.md`.

## Releases

A release tag's message becomes the GitHub release notes, so tag it properly:

1. Check both `git tag` and `git ls-remote --tags origin` and pick the next
   version that does not collide with either.
2. Format the tag as semver with a `v` prefix, e.g. `v0.1.4`.
3. Write the tag message as a summary of the changes between the previous
   tag and this one; `gh release create` in `.github/workflows/release.yml`
   uses it as the release description.
