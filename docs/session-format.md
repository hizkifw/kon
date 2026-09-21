# Session format

Sessions are UTF-8 JSON Lines files. Each physical line is one complete JSON
object. The first line is a header; later lines form an append-only tree.

## Header

```json
{"type":"session","version":1,"id":"ses_7Yk2mP9Qa4Zx8Vc1Nd6R","app_version":"v0.1.0","timestamp":"2026-09-21T08:00:00Z","cwd":"/work/project"}
```

The schema version governs the file representation. Readers must reject newer
versions they do not understand rather than guessing.

## Entry envelope

Every entry has these fields:

```json
{"type":"message","id":"ent_B3mN8qL2xR7vK5cT9Za1","parent_id":"ent_6Hd2Wp9Js4Uf8Qx7Mk3C","timestamp":"2026-09-21T08:00:01Z"}
```

`parent_id` is `null` for the root system message. Session IDs use the `ses_`
prefix and entry IDs use `ent_`, followed by 20 cryptographically random base62
characters. A child may point to any earlier entry, so future rewind can append
a new branch without modifying old lines. The active leaf in v1 is the final
valid entry.

Prefix and alphabet validation happens during JSON decoding. A session ID cannot
be used where an entry ID is required. Tool-call and model IDs originate outside
kon, so they are separately typed but have no local format restrictions.

Readers validate unique IDs and existing parents. Unknown entry types retain
their raw JSON envelope but do not enter model context.

## Message entries

The `message` object uses provider-neutral roles while keeping provider metadata:

```json
{"type":"message","id":"ent_1rT8zN4mQ6xK9Bc3Vp7D","parent_id":"ent_B3mN8qL2xR7vK5cT9Za1","timestamp":"...","message":{"role":"user","content":"Inspect this project"}}
{"type":"message","id":"ent_H7kP2dR9wA5nM3xQ8Lc4","parent_id":"ent_1rT8zN4mQ6xK9Bc3Vp7D","timestamp":"...","message":{"role":"assistant","content":"I will inspect it.","model":"provider-model-id","finish_reason":"stop","usage":{"prompt_tokens":100,"completion_tokens":10,"total_tokens":110}}}
{"type":"message","id":"ent_9qW4mK7zT2bN8Vc5Rx1A","parent_id":"ent_H7kP2dR9wA5nM3xQ8Lc4","timestamp":"...","message":{"role":"tool","content":"output","tool_call_id":"provider-call-id","name":"read"}}
```

Assistant tool calls are stored as structured `tool_calls`, with their arguments
as JSON rather than an escaped provider string. The optional ordered `parts`
array preserves reasoning blocks, tool-call metadata, and other opaque values
needed to replay provider-native conversations. Partial assistant messages are
never written.

## Model change entries

```json
{"type":"model_change","id":"ent_2xM8vQ5kR9cT3Np7Za4L","parent_id":"ent_9qW4mK7zT2bN8Vc5Rx1A","timestamp":"...","model":{"name":"review","provider":"anthropic","external_id":"claude-sonnet-4-5"}}
```

The name is kon's configured alias. The external ID remains opaque. Model
change entries record which profile applies to subsequent turns and do not
enter model context.

## Compaction entries

```json
{"type":"compaction","id":"ent_2xM8vQ5kR9cT3Np7Za4L","parent_id":"ent_9qW4mK7zT2bN8Vc5Rx1A","timestamp":"...","summary":"...","first_kept_entry_id":"ent_1rT8zN4mQ6xK9Bc3Vp7D","tokens_before":90000,"tokens_before_estimated":false,"usage":{"prompt_tokens":70000,"completion_tokens":1200,"total_tokens":71200}}
```

The entry is a child of the context it summarizes. `first_kept_entry_id` points
to an ancestor whose message and following ancestors remain verbatim. The
summary call's usage is separate from normal assistant usage.

## Durability

kon marshals, appends, and syncs each complete line before continuing. On open,
only a malformed final line may be removed as crash residue. Existing complete
entries are never rewritten by normal operation or compaction.
