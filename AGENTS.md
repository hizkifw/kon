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
make fmt          # gofmt cmd core internal
make check        # formatting, vet, and shuffled unit tests — the pre-commit gate
make test-race    # race detector; separate because it needs CGO and is slower
make build        # build bin/kon
make smoke        # build and verify the CLI entry point
make bench        # Go benchmarks for the hot paths
make loadtest     # CPU and memory of bin/kon under load (TUI=1 adds TUI runs)
make tag          # have kon tag the next release (BUMP=patch|minor|major)
make commit       # have kon review, check, and commit the changes
```

Run `make check` before considering any change done. Tests shuffle package test
order, so do not depend on ordering across packages.

To exercise the first-run TUI without touching real state:

```sh
XDG_CONFIG_HOME="$(mktemp -d)" XDG_DATA_HOME="$(mktemp -d)" go run ./cmd/kon
```

Prefer the standard library. Add a dependency only when it removes substantial,
well-tested platform work. The only current direct dependencies are Bubble Tea,
Bubbles, and Lip Gloss (with goldmark for markdown rendering and
golang.org/x/net/html for parsing fetched pages).

Each package owns one boundary. Do not reach across them.

Packages under `core/` are the reusable half of kon and its public Go API: the
loop, providers, sessions, the tool contract, and the ACP client. They never import `internal/`
or `cmd/` (`core/boundary_test.go` enforces it), so another program can drive
them with its own prompt and tools. Everything kon-specific lives in `internal/`.

| Package | Owns | Must not own |
| --- | --- | --- |
| `cmd/kon` | startup wiring and CLI metadata | business logic |
| `internal/ui` | terminal state and presentation | HTTP or JSONL encoding |
| `internal/tui` | kon-agnostic terminal widgets: scroll container, word wrap, line fitting, and the drawer stack with its lists | anything kon-specific: colors, transcripts, sessions, or actions |
| `internal/markdown` | Markdown to styled, wrapped lines, streaming, and selections cut back out as Markdown | colors, terminal output, or transcript state |
| `internal/headless` | `kon run` output: streamed text or JSON events, one run per process | terminal state or session policy |
| `internal/acp` | `kon acp`: serving the Agent Client Protocol over stdio, its turn queue per session, and kon's extensions | terminal state, session storage, or the wire types |
| `internal/app` | live runner/store lifecycle and model switching | terminal presentation |
| `core/agent` | the model/tool loop, retry events, and compaction policy | prompts, concrete tools, or terminal rendering |
| `core/tool` | the tool contract: `Tool`, `Registry`, and `Executor` | concrete tools or their presentation |
| `core/provider` | provider `Model` backends and durable-message conversion | session policy |
| `core/provider/wire` | the wire-format table: names, default endpoints, and dialect facts | HTTP, backends, or service quirks |
| `internal/login` | `/login` choices per service and connection verification | wire backends or config writes |
| `internal/catalog` | models.dev metadata: the bundled snapshot and its cached refresh | which provider APIs kon supports |
| `internal/catalog/generate` | refreshing the bundled snapshot, run only by `go generate` | anything a normal build runs |
| `core/session` | domain messages, the append-only context tree, and its file and memory stores | where a program keeps its sessions, or provider requests |
| `internal/sessions` | where kon keeps sessions: per-workspace directories, discovery, subagent usage, and previews | the session format |
| `internal/migrate` | storage upgrade locking, version tracking, and the `Step` interface | the concrete steps |
| `internal/migrations` | the concrete, ordered storage upgrade steps | locking or version tracking |
| `internal/codetools` | kon's coding tools: bounded schemas, execution, and transcript displays | agent orchestration |
| `internal/prompt` | kon's system prompt | context-file discovery |
| `internal/web` | `kon tool` web access: fetching pages as Markdown | tool schemas or agent state |
| `internal/websearch` | the web search contract: the `Engine` type, the provider table, shared HTTP calls, and result formatting | any one provider's API |
| `internal/websearch/<provider>` | one search provider's requests and response parsing | config, output, or another provider |
| `internal/websearch/engines` | the map from provider names to their engines | request or response details |
| `core/acp` | the Agent Client Protocol as kon speaks it: wire types, extension names, and a client that drives `kon acp` | serving the protocol, or anything about sessions |
| `core/typedid` | identifier construction and parsing | storage or provider policy |
| `core/tokens` | the token count type and its compact display | usage policy or estimation |
| `internal/buildinfo` | build version and the outgoing User-Agent | configuration or network clients |
| `internal/config` | paths, defaults, validation, credentials | runtime mutation |
| `internal/contextfiles` | AGENTS.md/CLAUDE.md discovery up the directory tree | prompt assembly |
| `internal/projectfiles` | bounded project-file discovery and Git ignore rules | terminal presentation or file contents |
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
  `core/typedid`, use 20 unbiased base62 characters, add JSON rejection
  tests, and document it in `docs/development/session-format.md`.
- Never validate the shape of an ID owned by a provider. Wrap it in a distinct
  named type (`ToolCallID`, `ModelID`), validate only whether it is semantically
  required, and preserve its bytes exactly at the wire boundary.

## When you finish

- Run `make check`; run `make test-race` for concurrency-sensitive changes.
- Update the relevant docs: `docs/*`, `README.md`.

## Releases

A release tag's message becomes the GitHub release notes, so tag it properly.
`make tag [BUMP=patch|minor|major]` has `kon run` follow these steps:

1. Check both `git tag` and `git ls-remote --tags origin` and pick the next
   version that does not collide with either.
2. Format the tag as semver with a `v` prefix, e.g. `v0.1.4`.
3. Write the tag message as a summary of the changes between the previous
   tag and this one; `gh release create` in the release job of
   `.github/workflows/ci.yml` uses it as the release description. Write it for kon's users, using this
   format:

   ```
   vX.Y.Z

   ## Section

   - **Headline**: short description
   - **Headline**: short description
   ```

   The first line is the version, each `##` section groups related commits,
   and each bullet pairs a bold headline with a short description.
4. Create the tag with `git tag -a --cleanup=whitespace -F <file>`: the
   default cleanup strips every line starting with `#`, which drops Markdown
   headings.
5. Do not push the tag. Pushing publishes the release, so leave that to a
   person.
