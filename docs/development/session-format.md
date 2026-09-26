# Session format

Sessions are UTF-8 JSON Lines files. Each physical line is one complete JSON
object. The first line is a header; later lines form an append-only tree.

## Header

```json
{"type":"session","version":4,"id":"ses_7Yk2mP9Qa4Zx8Vc1Nd6R","app_version":"v0.1.0","timestamp":"2026-09-21T08:00:00Z","cwd":"/work/project"}
```

A session started by `kon run` from an agent's shell, a subagent, also records
`parent_session_id`, the session that started it. The field is optional and
additive, so it does not change the schema version.

The schema version governs the file representation. Readers accept only version
4. The startup migration converts version 1 sessions written by kon v0.1.1 to
version 4 before a reader opens them. Versions 2 and 3 are not migrated.

Session files are named `<UTC timestamp>_<session ID>.jsonl`, where the timestamp
has millisecond precision. Listing orders sessions by the header `timestamp`
rather than the name, because two sessions created in the same millisecond would
otherwise tie on their random session ID.

## Writer lock

A session has one writer at a time. The writer holds an exclusive advisory
lock on `<session.jsonl>.lock`, taken before the session file is created or
parsed, and released when the session is closed. Each writer appends children
of its own in-memory leaf. A second writer would silently fork the
conversation, and trimming a torn tail could cut a record the first writer is
still appending. Opening a session another process holds for writing fails
with `ErrInUse`.

A follower reads a held session without the lock through `session.View`. It
never repairs the file, and reads only complete lines, starting after the last
one it read. A torn tail is read once its newline lands. A follower becomes the
writer by opening the session normally once the lock is free. Everything
written before the old writer released the lock is on disk by then.

The lock lives in its own file because Windows locks are mandatory, and they
would block the read-only previews and listing that run against live sessions.
The operating system drops the lock when its holder exits, so a crash leaves no
stale lock. Lock files are never removed, except together with a discarded
empty session. Unlinking one while another process has it open would let two
processes lock different files. Advisory locks on network filesystems are
best-effort.

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

Background shell jobs keep their files in `<session.jsonl>.jobs/<id>/`, with
IDs counting up from 1 per session: `cmd` (the command), `pid` (the process,
which leads its own process group on Unix), `output` (combined output, capped
at 16 MiB), `exit`, written when the job ends, and `session`, which a `kon run`
subagent writes with its own session ID through the `KON_JOB` variable. `exit` holds the exit code,
or a reason such as `killed: kon exited` or `lost: …` for a job whose kon
exited without recording it. None of these are referenced from the JSONL; the
shell tool result that started a job records `details.job`.

An assistant message's `provider_options` holds metadata the backend that
wrote it needs to send the reply back as it arrived. For chat completions that is
`reasoning_field`, the field its reasoning streamed in (`reasoning_content`,
`reasoning`, or `reasoning_text`), and `reasoning_details`, OpenRouter's
structured reasoning, kept verbatim because it can hold encrypted entries.
Reasoning is replayed only to the model that wrote it.

Parts carry their own `provider_options` too. The Messages backend stores a
thinking block's `signature`, or a redacted block's opaque `redacted` payload,
on its reasoning part. The Responses backend stores each output item verbatim
as `item` on the part it produced, reasoning items with their encrypted
content included.

Tool results may include `is_error: true` and a tool-owned `details` object.
Shell details record `exit_code`, `duration`, and `output_bytes` (the byte
length of output before the model-facing status marker), or `job` alone for a
command started in the background. Text reads record
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

The name is kon's selected profile. `wire_format` is the profile's `type`, the
wire format its requests used, such as `openrouter`; it says how kon talked to
the server, not which service it was. `connection_id` names the configured
connection used for a derived model, which is what identifies the service, and
is omitted for a standalone profile. The external ID remains opaque. Model
change entries record which profile applies to subsequent turns and do not
enter model context. Resuming restores the last one on the active path; kon
appends a new entry only when a different model will answer, such as when the
recorded profile no longer resolves, so the log always names the model behind
each reply.

## Turn entries

```json
{"type":"turn_start","id":"ent_4Tn7Wq2Lc9Xm5Rb8Kd3F","parent_id":"ent_9qW4mK7zT2bN8Vc5Rx1A","timestamp":"..."}
{"type":"turn_end","id":"ent_8Pz3Hs6Vn1Qa7Mf4Jy2E","parent_id":"ent_H7kP2dR9wA5nM3xQ8Lc4","timestamp":"...","duration_ms":320450}
```

A turn is one prompt's run through every model call and tool step until kon
stops. `turn_start` is written before the turn's first user message, and
`turn_end` after its last entry, carrying the duration kon measured in
milliseconds (omitted when zero). The end is written however the turn stops,
including user cancellation, so a start with no matching end means the process
died mid-turn. The transcript shows the recorded duration on replay rather than
inferring one from timestamps, because entries such as a later manual
compaction can land long after the turn they follow. Turn entries do not enter
model context.

## Compaction entries

```json
{"type":"compaction","id":"ent_2xM8vQ5kR9cT3Np7Za4L","parent_id":"ent_9qW4mK7zT2bN8Vc5Rx1A","timestamp":"...","summary":"...","first_kept_entry_id":"ent_1rT8zN4mQ6xK9Bc3Vp7D","tokens_before":90000,"tokens_before_estimated":false,"usage":{"prompt_tokens":70000,"completion_tokens":1200,"total_tokens":71200}}
```

The entry is a child of the context it summarizes. `first_kept_entry_id` points
to an ancestor whose message and following ancestors remain verbatim. The
summary call's usage is separate from normal assistant usage.
`tokens_before` is the size of the compacted context: measured from the
summary call's prompt usage when that call carried the live context, and
otherwise an estimate marked by `tokens_before_estimated`.

At request time the newest summary is projected as a standalone user message
immediately after the byte-identical system prompt, followed by the retained
tail and later messages. It is never folded into the system message: keeping the
system prompt stable preserves the leading prefix that provider prompt caches
key on, so compaction does not invalidate the cache for the retained context.

## Durability

kon marshals, appends, and syncs each complete line before continuing. The one
exception is a session that is still empty (header, root system message, and
model changes or turn starts only): those lines are not synced, because an empty session is
discarded on close and skipped by discovery if a crash leaves it behind. The
first conversation or compaction entry is synced, which makes every earlier line
durable along with it. On open,
only a malformed final line may be removed as crash residue. An append whose
write or sync fails (a full disk) is truncated away at once, so the next line
never follows a partial one; if that truncation also fails, the store refuses
further appends. Existing complete entries are never rewritten by normal
operation or compaction.
