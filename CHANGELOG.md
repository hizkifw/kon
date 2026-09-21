# Changelog

## [Unreleased]

### Changed

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
- Provider-native streaming through goai for OpenAI, Anthropic, Google,
  OpenRouter, Ollama, and OpenAI-compatible endpoints.
- Append-only tree sessions, prompt history, and automatic context compaction.
- Read, write, edit, and shell tools.
- Switching models with `/model <name>` persists the selection to the config
  file, so the next kon launch starts on the same model.
- Linux, macOS, and Windows release targets for amd64 and arm64.
- Stripe-style typed session and entry IDs, with opaque types for provider IDs.
- Streaming display of model thinking: reasoning deltas from goai render as
  interleaved `thinking` transcript blocks.

### Fixed

- A command that backgrounds a child inheriting its output (e.g. `server &`)
  no longer stalls the shell tool until the interrupt grace expires: output is
  captured through an executor-owned pipe whose reader is detached once the
  command exits, so `Wait` reports immediately and the exit status is kept.
- A cancelled command's tool result reports that it was cancelled (with the
  elapsed time and captured output) instead of a bare `context canceled`.
