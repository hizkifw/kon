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
| `internal/ui` | terminal state and presentation | HTTP or JSONL encoding |
| `internal/app` | live runner/store lifecycle and model switching | terminal presentation |
| `internal/agent` | model/tool loop and compaction policy | terminal rendering |
| `internal/provider` | provider `Model` backends and durable-message conversion | session policy |
| `internal/session` | domain messages and append-only context tree | provider requests |
| `internal/tools` | tool registry, bounded tool schemas and execution | agent orchestration |
| `internal/typedid` | identifier construction and parsing | storage or provider policy |
| `internal/config` | paths, defaults, validation, credentials | runtime mutation |
| `internal/history` | bounded prompt recall | conversation context |

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

## Testing seams

The provider, agent, tools, session store, and Bubble Tea model can all be tested
without a real terminal or paid API. Prefer `httptest.Server`, temporary
directories, and fake `agent.Provider` implementations. Tests that exercise SSE
must include fragmented events and `[DONE]` behavior. Tests that change context
must cover repeated compaction and tool-call/result adjacency.
