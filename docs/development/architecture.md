# Architecture

kon is split into small packages around durable boundaries rather than UI
screens. The command initializes paths and configuration, creates a session,
and connects the following pieces:

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

## Runtime flow

1. Resolve configuration and data roots. Create only missing files.
2. Create a cwd-scoped session and persist the exact system prompt. With
   `--resume`, open the newest existing session for the directory (or a named
   one) instead of creating a session, and replay its active path for display.
3. Render the alternate-screen TUI. No provider request occurs during startup.
4. Persist a submitted user message before starting network work.
5. Stream one assistant message. A completed message is persisted atomically as
   one JSON line; an interrupted partial response remains display-only.
6. Persist the assistant's complete tool-call set, execute calls serially, and
   persist one result for every call before requesting another assistant turn.
7. Stop at a final assistant message. Tool iterations are unbounded.

kon deliberately invokes the provider one generation step at a time. This
keeps every assistant tool request and every tool result durable before the
next network request. The provider layer owns wire formats and stream parsing;
kon owns orchestration, persistence, and compaction.

The app runtime is the sole owner of the live store and runner. A new session is
fully prepared before it replaces the current one, so creation failures leave
the current session usable. Its explicit phase distinguishes ready, running,
configuration-required, and closed states. Resume follows the same rule: the
target session is opened and validated before the current store is closed, and
only sessions in the working directory's session folder are eligible.

The UI and runtime communicate through typed events and operations. Typed
transcript blocks own their rendering, prompt history owns recall state,
and slash commands are declared in a central registry that owns parsing,
argument validation, and autocomplete dispatch while the command bodies run as
model methods. Suggestions render in a generic popup widget driven by a
`menuSource`, so command completion and future pickers share one renderer and
key handler. A popup row may also carry a lazy `Preview` builder: while it is
highlighted the model draws that scratch transcript in place of the live one, so
`/resume` can be browsed without committing, and closing the popup restores the
live transcript at the reader's previous scroll position. Because sessions grow
without bound, a resume preview reads only the tail of the file — the active
path's last couple of user turns — rather than parsing the whole transcript, so
highlighting stays fast regardless of session size. Each block renders
as a role-colored slab: user and error messages carry distinct backgrounds,
agent messages render on the default terminal background, thinking traces
render in muted gray, and consecutive tool calls pair with their results and
collapse into a single grouped slab so bursts of tool activity stay compact.
Each tool owns its transcript presentation through the `tools.Displayer`
interface: it renders the request-line summary and the trimmed result body
from the persisted arguments and content, so the transcript never parses tool
output, and a resumed session replays the identical display. A long-running
tool publishes live display snapshots while it runs, and the transcript
replaces the running call's body with the latest one, so shell output streams
into the view under the same tail policy the finished result uses. A running
shell call's status line also ticks elapsed time against the command's timeout,
so the view shows the command is alive and how much budget remains; the finished
result replaces it with the exit-code status. A
presentation-only welcome banner leads every transcript as a stable prefix above
the conversation; the banner is not a block and never reaches session records or
model context, so it stays at the top across messages and resumed sessions.
Streaming deltas are accumulated immediately but
viewport rebuilds are capped at 20 frames per second. The runner owns no
terminal state, and the UI owns no provider or session serialization.

## Model catalog

`internal/catalog` embeds a timestamped models.dev snapshot in the binary. Its
read methods return provider and model metadata immediately. `kon models`
loads the bundled snapshot or a newer valid cache from the data directory and
lists model IDs without network access. Only `kon models --refresh` contacts
models.dev. Refreshes use the upstream ETag, validate the whole response, and
atomically replace the cache before publishing a new in-memory snapshot. An
invalid cache or failed network request leaves the previous snapshot
available. The cache is metadata only; configured models and session records
are never rewritten by a catalog refresh. Normal startup does not load the
catalog or contact models.dev.

Catalog provider IDs and AI SDK package names describe upstream metadata.
`internal/provider` remains responsible for deciding which wire formats kon
can call. Login maps recognized `npm` values to those formats and uses the
catalog `api` URL when it is a concrete base endpoint. A small override table
covers providers with missing URLs or special flows. A catalog model may
therefore be visible even when its provider is not yet supported by kon.

The `providers` config array stores named connections, while `models` holds
standalone explicit profiles whose `type` names a wire format and never refers
to a connection. `/login <provider>`
checks the chosen provider on user request and caches returned model IDs in
`provider-models.json` under the data directory. The runtime joins explicit
profiles, cached discovery, and bundled catalog metadata into `/model` choices.
Derived names are `<provider-id>/<model-id>` and are never materialized as
config profiles. The catalog is never loaded before the first frame: the UI
starts a background load after it renders, then refreshes the header's display
name and the status bar's context window. The load happens at most once; the
picker, `/login`, or a first request that arrives earlier waits for that same
load, and a derived model's capabilities are always resolved before its first
request. Neither startup nor model switching contacts a provider.

## Context

The session store resolves context by following `parent_id` from the active leaf
to the root. File order is append order, not conversation order. This distinction
is already enforced in v2 even though the UI cannot move the leaf yet.

Compaction selects a legal boundary while preserving complete user turns when
possible and never separates an assistant tool call from its results. A
compaction entry points to the first retained entry. Context projection combines:

1. the original system prompt, byte-identical across compactions;
2. the newest compaction summary, projected as its own user message;
3. retained ancestors preceding that compaction entry; and
4. messages appended after it.

The system prompt is built once when the session is created. It carries the
built-in rules, the absolute path to the current kon executable and guidance to
consult `kon docs` for self-questions, then any `AGENTS.md`-style project
instructions discovered by walking up from the working directory (outermost
first, tagged with their paths), the working directory, and finally the user's
configured instructions. Because the prompt is persisted verbatim and never
rebuilt, edits
to an instruction file take effect on a new session rather than a resumed one;
this is the same byte-stability the compaction design depends on.

The summary is kept out of the system prompt on purpose. Provider prompt caches
key on a stable leading prefix, so folding the summary into the system message
would invalidate the cache for the entire retained context on every compaction.

Repeated compaction summarizes the previous summary together with newly aged
messages. Original entries remain available for future tree navigation.
Automatic compaction runs at the context threshold; the `/compact` command
forces the same routine on demand, so both paths share one boundary policy and
one durable summary entry format.

Summary generation reuses the live prefix cache. The preferred request sends the
live system prompt and tool roster plus every message up to the compaction
boundary verbatim, appending only one trailing user message that asks for the
summary. The provider then reads the prompt cache the last streaming turn wrote
and bills only the trailing message as new input. Instructions live in that
trailing message rather than a system message so the prefix stays byte-identical.
When the context plus the reserve no longer fits the window, the request is
instead built in isolation from only the history being dropped, so emergency
overflow recovery still works.

Provider prompt usage is preferred when it covers the current context. Otherwise
kon estimates serialized text and tool-schema bytes at four bytes per token and
marks the result approximate. Exact tokenization is model-specific and is not a
sensible dependency for a provider-neutral harness.

## Failure rules

- Configuration and session initialization errors are fatal before terminal
  takeover, with the affected path in the error.
- A malformed final JSONL record is treated as an interrupted append and
  truncated. Malformed interior records, duplicate IDs, missing parents, and
  unsupported schema versions are rejected.
- Network failures and provider errors stop the current run. A 400 that
  rejects `stream_options` or `max_tokens` is retried once without the field
  or with the modern replacement, so older and newer OpenAI-compatible servers
  both work.
- A recognized context-overflow rejection is safe to compact and retry once.
- Tool failures become tool-result messages so the model can respond to them.
- Shell commands must carry a model-specified timeout (at most 600 seconds)
  and report their wall-clock time with the exit status. A timed-out or
  cancelled command's captured output is preserved in the tool result.
- Ctrl+C cancels the active request or command through a shared context. A
  second press force-kills the command's process group.

## Dependency policy

Direct dependencies are Bubble Tea, Bubbles, and Lip Gloss. The provider layer
uses only the Go standard library: chat completions request bodies and SSE
streams are parsed in-tree so token accounting and streaming stay exact and
inspected. JSON, typed-ID generation, files, subprocesses, and release
cross-compilation also use the Go standard library. Dependencies are pinned in
`go.mod` and authenticated by `go.sum`.

Owned identifiers are value objects from `internal/typedid`. Their unexported
representation prevents arbitrary construction outside that package. External
provider IDs use distinct named-string wrappers: kon prevents category mistakes
without applying rules to identifiers it does not own.
