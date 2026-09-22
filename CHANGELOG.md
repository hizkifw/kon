# Changelog

## [Unreleased]

### Added

- Slash commands are registered in a central registry inside `internal/ui`.
  Each command declares its positional arguments and, per argument, an
  autocomplete handler.
  Typing `/` at the start of the prompt opens a suggestion popup with the first
  entry selected; Tab (or Enter) fills the selection and arrow keys cycle it.
  Commands are only recognized at the start of the prompt.
- The suggestion popup is a reusable `ui.menu` widget driven by a `menuSource`,
  so future pickers can reuse the same rendering and key handling.
- Sessions can be resumed. `kon --resume` reopens the newest session for the
  current directory after replaying its conversation into the transcript;
  `--resume <id>` opens a specific session. The `/resume` command lists sessions
  for the current directory and `/resume <id>` switches to one in place, and
  `/resume <id>` autocompletes from stored session IDs.
- On exit kon prints a `kon --resume <id>` hint so the conversation can be
  picked up again later.
- The `/compact` command summarizes older context on demand, using the same
  boundary rules and durable summary entry as automatic compaction.
- `Esc` interrupts a streaming turn. Unlike `Ctrl+C` it never escalates to
  killing a running shell command; it only cancels the generation.
- Partial turns are kept. When a stream is interrupted (Esc, `Ctrl+C`, or a
  dropped connection) the assistant text and reasoning produced so far are
  persisted as an `interrupted` message, so a resume shows the partial trace and
  a follow-up message continues from it. A partial turn with no answer text is
  skipped on the request wire (this format has no reasoning field) but stays in
  the local transcript.

### Changed

- Compaction no longer rewrites the system prompt. The newest summary is
  projected as its own user message right after a byte-identical system prompt,
  so each compaction preserves the stable leading prefix that provider prompt
  caches key on instead of forcing a full cache miss.
- Summary generation reuses the live prompt-cache prefix: the request carries the
  live system prompt, tool roster, and message history up to the compaction
  boundary, with the summary instructions appended as one trailing user message.
  Only emergency overflow compaction, which no longer fits the window, falls back
  to an isolated request.
- Resuming a session restores the last provider-reported context size, so the
  status line shows the real token count instead of `ctx ?` until the next turn.
  Usage was already persisted on each assistant message; it is now re-seeded when
  the session is reopened, for both the indicator and the compaction threshold.
- kon now owns the OpenAI Chat Completions wire format directly and the goai
  dependency is gone. The provider layer builds request messages, parses SSE
  deltas, and assembles tool calls in-tree, and reports usage exactly as the
  server counted it (`cached_tokens` is now persisted alongside token totals).
  Supported providers are `openai`, `openrouter`, `ollama`, and
  `openai-compatible`; the `provider.Model` interface keeps other wire formats
  easy to add later.
- Shell commands must specify a `timeout` in whole seconds (at most 600); the
  tool rejects calls without one, and a timed-out command's captured output
  survives into the tool result instead of being discarded.
- Shell results end with the command's wall-clock time (`exit code: 0 (took
  1.2s)`), which the transcript echoes next to the exit code, so the model
  knows how long commands take.
- Transcript rendering is styled end to end: user, agent, tool, and error
  messages render as distinct background-colored slabs, thinking traces render
  in muted gray, and context compaction appears as a centered separator rule.
- Consecutive tool calls group into a single slab instead of littering the
  history, with one line per call: a status icon (running, ok, failed), the
  tool name, and a concise summary of the arguments in place of raw JSON.
- `read` results collapse to a line count, `edit`/`write` show the path with
  changed-line or byte counts, and `shell` results trim to the last few output
  lines plus a color-coded exit code.

### Added

- Initial fullscreen coding agent with named, switchable model profiles.
- Streaming against the OpenAI Chat Completions format for OpenAI, OpenRouter,
  Ollama, and OpenAI-compatible endpoints.
- Append-only tree sessions, prompt history, and automatic context compaction.
- Read, write, edit, and shell tools.
- Switching models with `/model <name>` persists the selection to the config
  file, so the next kon launch starts on the same model.
- Linux, macOS, and Windows release targets for amd64 and arm64.
- Stripe-style typed session and entry IDs, with opaque types for provider IDs.
- Streaming display of model thinking: reasoning deltas render as interleaved
  `thinking` transcript blocks.

### Fixed

- A command that backgrounds a child inheriting its output (e.g. `server &`)
  no longer stalls the shell tool until the interrupt grace expires: output is
  captured through an executor-owned pipe whose reader is detached once the
  command exits, so `Wait` reports immediately and the exit status is kept.
- A cancelled command's tool result reports that it was cancelled (with the
  elapsed time and captured output) instead of a bare `context canceled`.
