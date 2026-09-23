# Session format

Sessions are UTF-8 JSON Lines files. Each physical line is one complete JSON
object. The first line is a header; later lines form an append-only tree.

## Header

```json
{"type":"session","version":4,"id":"ses_7Yk2mP9Qa4Zx8Vc1Nd6R","app_version":"v0.1.0","timestamp":"2026-09-21T08:00:00Z","cwd":"/work/project"}
```

The schema version governs the file representation. Readers accept only version
4. The startup migration converts version 1 sessions written by kon v0.1.1 to
version 4 before a reader opens them. Versions 2 and 3 are not migrated.

## Entry envelope

Every entry has these fields:

```json
{"type":"message","id":"ent_B3mN8qL2xR7vK5cT9Za1","parent_id":"ent_6Hd2Wp9Js4Uf8Qx7Mk3C","timestamp":"2026-09-21T08:00:01Z"}
```

`parent_id` is `null` for the root system message. Session IDs use the `ses_`
prefix and entry IDs use `ent_`, followed by 20 cryptographically random base62
characters. A child may point to any earlier entry, so future rewind can append
a new branch without modifying old lines. The active leaf in v4 is the final
valid entry.

Prefix and alphabet validation happens during JSON decoding. A session ID cannot
be used where an entry ID is required. Tool-call and model IDs originate outside
kon, so they are separately typed but have no local format restrictions.

Readers validate unique IDs and existing parents. Unknown entry types retain
their raw JSON envelope but do not enter model context.

## Message entries

The `message` object uses provider-neutral roles while keeping provider metadata:

```json
{"type":"message","id":"ent_1rT8zN4mQ6xK9Bc3Vp7D","parent_id":"ent_B3mN8qL2xR7vK5cT9Za1","timestamp":"...","message":{"role":"user","parts":[{"type":"text","text":"Inspect this project"}]}}
{"type":"message","id":"ent_H7kP2dR9wA5nM3xQ8Lc4","parent_id":"ent_1rT8zN4mQ6xK9Bc3Vp7D","timestamp":"...","message":{"role":"assistant","parts":[{"type":"reasoning","text":"I should inspect the files.","provider_options":{"signature":"opaque"}},{"type":"text","text":"I will inspect it."}],"model":"provider-model-id","finish_reason":"stop","usage":{"prompt_tokens":100,"completion_tokens":10,"total_tokens":110}}}
{"type":"message","id":"ent_9qW4mK7zT2bN8Vc5Rx1A","parent_id":"ent_H7kP2dR9wA5nM3xQ8Lc4","timestamp":"...","message":{"role":"tool","parts":[{"type":"tool_result","tool_call_id":"provider-call-id","tool_name":"read","tool_output":"output"}]}}
```

Assistant tool calls are `tool_call` parts with opaque IDs and JSON arguments.
The `parts` array is the sole content source and preserves reasoning blocks,
tool calls, text, and provider-owned metadata in order. An `image` part holds
`image_hash` (a lowercase SHA-256 digest) and `image_mime`. Its bytes live in a
file named by the hash inside `<session.jsonl>.blobs/`; the JSONL contains no
base64 image data. Equal bytes in one session share a blob. The provider
verifies and loads blobs when it builds a vision request; text-only results
carry no image part.

Tool results may include `is_error: true` and a tool-owned `details` object.
Shell details record `exit_code`, `duration`, and `output_bytes` (the byte
length of output before the model-facing status marker). Text reads record
`line_count` and, for an empty file, `empty_file`. The transcript uses these
details to replay the tool's display without parsing its model-facing text.

A turn that is interrupted before completion (user cancellation or a dropped
connection) is written with `interrupted: true`, carrying whatever answer text
and reasoning arrived. Its `finish_reason` and `usage` are omitted because the
turn never completed. Interrupted turns are retained so the partial trace is
visible after a resume and the next request can continue from it; the provider
wire mapping skips an interrupted turn that has no answer text.

A cancelled turn may leave an assistant tool-call batch without a result for
every call. The agent appends `not executed: interrupted` for the calls it did
not reach before returning; if the process died first, context projection
recreates the same synthetic result for any call still unanswered. Projection
places every result directly after its assistant message in call order, so a
batch is always complete and ordered on the wire, and it never rewrites the
durable log — the repair is rebuilt on each projection.

## Model change entries

```json
{"type":"model_change","id":"ent_2xM8vQ5kR9cT3Np7Za4L","parent_id":"ent_9qW4mK7zT2bN8Vc5Rx1A","timestamp":"...","model":{"name":"fireworks-2/gpt-4o","wire_format":"openai-compatible","connection_id":"fireworks-2","external_id":"gpt-4o"}}
```

The name is kon's selected profile. `wire_format` names the request protocol;
`connection_id` names the configured connection used for a derived model and
is omitted for a standalone profile. The external ID remains opaque. Model
change entries record which profile applies to subsequent turns and do not
enter model context.

## Compaction entries

```json
{"type":"compaction","id":"ent_2xM8vQ5kR9cT3Np7Za4L","parent_id":"ent_9qW4mK7zT2bN8Vc5Rx1A","timestamp":"...","summary":"...","first_kept_entry_id":"ent_1rT8zN4mQ6xK9Bc3Vp7D","tokens_before":90000,"tokens_before_estimated":false,"usage":{"prompt_tokens":70000,"completion_tokens":1200,"total_tokens":71200}}
```

The entry is a child of the context it summarizes. `first_kept_entry_id` points
to an ancestor whose message and following ancestors remain verbatim. The
summary call's usage is separate from normal assistant usage.

At request time the newest summary is projected as a standalone user message
immediately after the byte-identical system prompt, followed by the retained
tail and later messages. It is never folded into the system message: keeping the
system prompt stable preserves the leading prefix that provider prompt caches
key on, so compaction does not invalidate the cache for the retained context.

## Durability

kon marshals, appends, and syncs each complete line before continuing. On open,
only a malformed final line may be removed as crash residue. Existing complete
entries are never rewritten by normal operation or compaction.
