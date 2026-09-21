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
2. Create a cwd-scoped session and persist the exact system prompt.
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
configuration-required, and closed states.

The UI and runtime communicate through typed events and operations. Typed
transcript blocks own labels and rendering, prompt history owns recall state,
and slash commands are declared in a central registry that owns parsing,
argument validation, and autocomplete dispatch while the command bodies run as
model methods. Suggestions render in a generic popup widget driven by a
`menuSource`, so command completion and future pickers share one renderer and
key handler. Each block renders
as a role-colored slab: user, agent, tool, and error messages carry distinct
backgrounds, thinking traces render in muted gray, and consecutive tool calls
pair with their results and collapse into a single grouped slab so bursts of
tool activity stay compact. Streaming deltas are accumulated immediately but
viewport rebuilds are capped at 20 frames per second. The runner owns no
terminal state, and the UI owns no provider or session serialization.

## Context

The session store resolves context by following `parent_id` from the active leaf
to the root. File order is append order, not conversation order. This distinction
is already enforced in v1 even though the UI cannot move the leaf yet.

Compaction selects a legal boundary while preserving complete user turns when
possible and never separates an assistant tool call from its results. A
compaction entry points to the first retained entry. Context projection combines:

1. the original system prompt;
2. the newest compaction summary;
3. retained ancestors preceding that compaction entry; and
4. messages appended after it.

Repeated compaction summarizes the previous summary together with newly aged
messages. Original entries remain available for future tree navigation.

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
