# Changelog

## [Unreleased]

### Added

- Assistant messages render as markdown in the transcript. Headings (bright
  and bold, with inline emphasis preserved inside), lists, fenced and indented
  code, blockquotes, tables, rules, and task checkboxes (`[ ]` / `[✓]`) now
  display as formatted text instead of raw markdown syntax, with inline
  emphasis, strong, inline code, strikethrough, and links styled through the
  transcript palette (bold, italic, underline, and dedicated colors). Text
  entities and backslash escapes render as their literal characters, and raw
  HTML shows as written rather than being interpreted or dropped. Adjacent
  blocks are separated by a blank line, so headings, paragraphs, lists,
  tables, quotes, code, and rules have breathing room (multi-paragraph quotes
  get a bar-only separator). Every line wraps to fit the transcript width --
  including list markers, blockquote gutters, wide tables, and code -- so no
  text is cut off with an ellipsis at the right edge. The formatting appears
  incrementally as a message streams and does not shift when the message
  finalizes into history.
- A streaming markdown core (`internal/markdown`) backs the transcript
  rendering above, built on goldmark with GFM tables, strikethrough, task
  lists, and autolinks. It renders CommonMark blocks into wrapped display
  lines carrying presentation-free style tokens, and freezes provably closed
  blocks so per-frame render cost is proportional to the unfrozen tail, not
  the document. The streaming view is byte-identical to a from-scratch render
  of the same bytes at every frame (guarded by mid-stream convergence tests
  and a randomized corpus covering blank-spanning containers, partial-list
  markers, and inline styling), and `Finish` folds the open tail in one pass
  when a stream ends. Link destinations are not surfaced yet (link text
  renders, the URL does not).

### Changed

- Tools are refactored around a central registry inside `internal/tools`,
  mirroring the slash-command registry in `internal/ui`. Every tool implements
  a common `Tool` interface (`Definition`, `Run`, and `Interrupt`) in its own
  file (`read.go`, `write.go`, `edit.go`, `shell.go`) and registers itself once
  in `defaultRegistry`; `executor.go` resolves model tool calls against the
  registry instead of a hardcoded switch.
- Cancellation escalation is tool-agnostic. The harness counts consecutive
  `Ctrl+C` presses during a run and forwards the count through
  `Runtime.Interrupt` → `agent.Runner.Interrupt` → `tools.Registry.InterruptAll`
  to every registered tool. Synchronous tools report nothing to interrupt; the
  shell tool interrupts its command on the first press and force-kills it on
  the second.

### Added

- The `read` tool loads image files (png, jpeg, gif, webp, up to 5 MB) as
  image content for models configured with `"vision": true`, so screenshots
  and diagrams in the workspace can be asked about directly. Images are
  detected from the file's bytes rather than its extension, so any image
  loads whatever it is named, and known-but-unsendable formats (BMP, TIFF,
  ICO, HEIF, AVIF) get a convert-it hint instead of a confusing failure.
  Without the flag, reading an image returns a text notice instead of bytes.
  Images persist as `image` parts on the tool result and map to chat
  completions `image_url` content; text-only configurations keep plain text.
- Context estimation no longer guesses image token costs from the base64
  payload, which overcounted by orders of magnitude and compacted after every
  image read. The provider's own `prompt_tokens` — which includes the true
  vision cost from the first response on — drives compaction, with the
  context-overflow retry as the backstop for oversized turns.
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
