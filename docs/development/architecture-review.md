# Architecture review (2026-09-23)

A whole-codebase review taken at commit `0bbfa07`, before the session format
ships. The goal is to find structural decisions that would be cheap to change
now and expensive later. Line references point at that commit.

The foundation is sound: package boundaries are mostly real, durability is
taken seriously, and the cache-stable system prompt is a deliberate design. The
problems cluster in two places — the **durable session schema** and the
**provider seam** — which is exactly where a mistake now becomes a migration
later.

Each item carries a checkbox so this page can track the work. Remove or update
an item when it lands.

## Tier 1 — fix first (correctness and durable format)

### 1. A cancelled or crashed turn can permanently break a session

- [x] Persist a result for every tool call before returning
- [x] Repair unanswered calls during context projection

`agent.Runner.Run` returns after the current tool when the context is cancelled
(`internal/agent/agent.go:268`). Remaining tool calls in the same assistant
message never get results. On the next prompt, OpenAI-compatible servers reject
the request ("an assistant message with tool_calls must be followed by tool
messages responding to each tool_call_id"), and every request after that fails
too. Compaction cannot recover because the cache-preserving request sends the
same invalid prefix and the failure is not a context overflow. A process killed
during a long shell command produces the same state.

Fix: before returning, the agent appends an explicit "not executed:
interrupted" result for each unanswered call. As a backstop for crashes, context
projection (or the wire mapping) synthesizes the same result for any call that
still has none.

### 2. Quitting mid-run can hang kon on exit

- [ ] Make `emit` non-blocking once the run is abandoned

Traced from code, not yet reproduced. `internal/ui/model.go:537` sends agent
events on an unbuffered channel. `ctrl+d` quits even while a run is busy
(`model.go:281`), after which nothing reads that channel. The shell tool's
reporter goroutine then blocks on its send, `shell.go:252` waits for that
goroutine forever, and `app.Runtime.Close` waits forever for the run. A
streaming text delta can block the same way, because `decodeChatStream` calls
`emit` synchronously and never gets back to observe cancellation.
`tools.Env.report` documents "must not block", but nothing guarantees it.

Fix: have the event sink select on the run's context or a done channel so an
abandoned run drops events instead of blocking.

### 3. Tool outcome is not persisted; display parses model-facing text

- [ ] Persist `is_error` on tool results
- [ ] Persist structured tool `details` alongside model-facing content

A tool-result message has no error flag, so replay always passes
`failed=false` (`internal/ui/events.go:129`): failed calls look successful after
a resume. Anthropic's wire format also needs `is_error` on tool results.

Separately, `shellTool.Describe` recovers the exit code by parsing the
`"exit code: N (took D)"` marker out of the model-facing content, and
`readTool.Describe` parses its own `%6d` line-number rows. The text the model
sees has become a display API. Persist structured details (error flag, exit
code, duration, line count) next to the content, and let `Displayer` read them.

### 4. Make ordered parts the single source of message content

- [ ] Redesign `session.Message` as `{Role, Parts}`
- [ ] Let backends return ordered parts with their own metadata

`session.Message` stores content twice: `Content` and `text` parts, `ToolCalls`
and `tool_call` parts, `ToolCallID`/`Name` and an unused `tool_result` part
type. The wire mapping reads one copy, transcript replay reads the other, and
images exist only as parts.

`provider.Response` compounds this. Reasoning is a single string, and
`Client.buildAssistant` (`internal/provider/provider.go:115`) rebuilds parts in
a fixed reasoning → text → tool-calls order. That loses:

- interleaved thinking between tool calls;
- reasoning signatures and encrypted reasoning (Anthropic thinking signatures,
  OpenAI Responses encrypted reasoning, Gemini thought signatures), which must
  round-trip verbatim. `Part.ProviderOptions` exists, but no backend can fill it.

Recommendation: a message is a role plus ordered parts (`text`, `reasoning`,
`tool_call`, `tool_result`, `image`). Backends return parts in stream order,
each with opaque provider-owned metadata. Plain-text views are derived. This is
the change most worth making before any session is written by a release.

### 5. Move images out of session lines

- [ ] Content-addressed blob storage beside session files

Images are stored as base64 `data:` URIs inside JSONL lines. Every screenshot
permanently inflates the file, stays resident in `Store.entries`, and makes
`session.Open` parse megabytes. Store blobs by hash in a sibling directory and
keep only the hash (and MIME type) in the part.

### 6. Rename the wire-format field in model-change entries

- [ ] Rename `ModelSelection.Provider`

`ModelSelection.Provider` is set to `profile.WireType()`, so it records
`"openai-compatible"`, while `config.Providers` means named connections. Fix the
name, and decide whether to record the connection ID too, before the format
ships. See item 15 for the broader vocabulary problem.

## Tier 2 — restructure before building more

### 7. Replace scattered wire-type strings with one provider-owned table

- [ ] Single wire-format table in `internal/provider`

`"openai" | "openai-compatible" | "openrouter" | "ollama"` is re-spelled in
`config.supportedProviders`, the `provider.newModel` switch, the
`provider.Discover` switch, the `newChatModel` switch (duplicating `Discover`'s
base-URL defaults), `app.catalogKey`, and `ui/login.go`, which hard-codes the
Ollama URL and knows which types need a URL or key. One table keyed by wire
type — default base URL, needs URL, needs key, discovery, constructor — would
let config validate against it and let login ask it instead of re-deriving
rules.

### 8. Separate user-written profiles from resolved runtime specs

- [ ] Introduce a resolved spec type; drop `agent`'s `config` import

`config.Model` is both what the user wrote and the resolved runtime profile:
`app.resolveModel` injects `Vision` and `ContextWindowTokens` from the catalog.
`agent.New` takes `config.Model` and `config.Compaction`; `provider.New` takes
`config.Model`. Introduce a resolved spec (for example `provider.Spec`) and
agent-owned options so config stays pure input.

### 9. Stop rewriting the user's config at runtime

- [ ] Move machine-written state (last model, credentials) to the data directory
- [ ] Support `api_key_env`

`Runtime.SwitchModel` saves the in-memory config loaded at startup
(`internal/app/app.go:303`). It silently discards edits made to `config.json`
while kon runs, materializes defaults such as the placeholder `default` model,
and races other kon instances. `Runtime.Login` re-reads before saving, so the
two paths already disagree, and the package map says config must not own
runtime mutation.

Keep `config.json` user-authored and read-only to kon. Write the last-used model
to a state file and credentials to an `auth.json` (0600) in the data directory.
That also keeps API keys out of dotfile repositories. The catalog already
carries each provider's `Env` names, so `api_key_env` is a natural addition.

### 10. Consolidate runner construction and session swaps in `app`

- [ ] One `buildRunner`, one operation guard, one session swap

`agent.New(profile, cfg.Compaction, client, store, tools.New(cwd, vision))`
appears at `app.go:117`, `app.go:502`, `models.go:109`, and `models.go:263`,
with subtly different behavior (only `createRunner` appends a model change).
`Run` and `Compact` duplicate the busy-phase begin/end logic; `Resume` and
`NewSession` duplicate the swap logic. `New`/`NewResumed`/`NewResumedID` could
be one constructor with options.

### 11. Decide what model a resumed session uses

- [ ] Restore the recorded model, or record the switch

`Runtime.openStore` uses the current default profile and appends nothing, so
the durable log can say model X while new turns run on model Y. Either restore
the session's last model-change entry or append one when they differ.

### 12. Keep prompt wording out of `session`

- [ ] Return a structured summary item from `Store.Context`

`Store.Context` wraps the compaction summary in `CompactionSummaryPrefix`, and
`agent.projectedSummary` strips it back off. Return the summary as structured
data and let the agent own the wording.

### 13. Use reported usage for compaction decisions

- [ ] Reported prompt tokens plus an estimate of messages since
- [ ] Fix `/compact` messages for short sessions and missing context windows

`Runner.usageFor` trusts provider usage only when the projected message count
equals the count at the time it was reported (`agent.go:113`). Every tool-loop
iteration has appended tool results by then, so the pre-request threshold check
runs on a pure bytes/4 estimate that ignores images. Use the reported prompt
tokens plus an estimate of only the newer messages.

Related: forcing `/compact` on a short session reports "active turn is too large
to compact safely" (`agent.go:322`), and with no context window it reports
"nothing to compact". The `!compacted` branch of the overflow retry is
unreachable.

### 14. Move login and connection mapping out of the wire-backend package

- [ ] Relocate `provider/registry.go` and `provider/discovery.go`
- [ ] Give tool specs a neutral home

Login mapping makes `provider` import `catalog` and `config`. `tools` imports
`provider` only for the `Tool` struct, so the tools package transitively pulls
in the embedded catalog. Move connection mapping to `app` or a dedicated
package, and define tool specs where both sides can use them without the
dependency.

### 15. Fix the vocabulary

- [ ] Write a glossary and rename to match

There are five `Model` types (`ui.Model`, `app.Model`, `config.Model`,
`provider.Model`, `catalog.Model`) and four meanings of "provider": a config
connection, the session's wire-type field, the Go package, and
`agent.Provider` (implemented by `provider.Client`). Pick terms — wire format,
connection, profile, model — document them here, and rename. This is cheap now
and compounds quickly.

## Tier 3 — seams for what comes next

- [ ] **Tool call IDs on agent events.** The UI attaches live snapshots and
  results to "the trailing tool block"; parallel tool execution breaks that.
- [ ] **Replace `Tool.Interrupt`.** Every tool implements it (three no-ops),
  `Registry.InterruptAll` broadcasts, and `shellTool` has a single `running`
  slot. A per-call context plus a kill signal in `Env` is simpler and allows
  concurrent calls.
- [ ] **Approval hook between agent and executor.** "Tools execute without a
  sandbox or confirmation" is hard-coded in the system prompt. A `Policy`
  interface with an allow-all default costs little now.
- [ ] **Streaming tool-call events.** A large `write` shows nothing while its
  arguments stream.
- [ ] **Headless frontend** (`kon -p`, JSON event stream). `app.Runtime` is
  nearly UI-agnostic; keep `agent.Event` stable enough to serialize.
- [ ] **Keep blocking work off the Bubble Tea update loop.** `Resume` parses the
  whole file, `NewSession` fsyncs and walks directories, `SwitchModel` fsyncs
  config, and `Sessions()` runs on every `/resume` completion keystroke — all
  synchronously in `Update`. `Runtime.Models` holds `r.mu` while the catalog
  decompresses, which blocks `State()`.
- [ ] **Stream idle timeout and retry with backoff** for 429/5xx and dropped
  connections. A stalled server currently hangs until Ctrl+C.
- [ ] **Freeze the tool roster per session.** The system prompt is frozen, but
  the tool list is also part of the cache prefix. Once MCP or dynamic tools
  exist, consider persisting a roster hash.

## Smaller items

- [ ] `atomicWrite` renames over the target, replacing a symlink with a regular
  file (stow-managed dotfiles) and dropping hardlinks and ownership. Resolve
  symlinks before writing.
- [ ] The `Env.Resolve` comment says "under the workspace root", but any
  absolute path is accepted.
- [ ] `defaultDisplays` and `defaultRegistry` register the same four tools. The
  UI calls `tools.Describe` directly (`events.go`) and also through
  `Runtime.DescribeTool`.
- [ ] Replay reads `session.Entry`, `provider.PartReasoning`, and `typedid`
  directly. After item 4, a runtime-provided transcript view model would
  decouple `ui` from the schema. The `ui.Runtime` interface has 17 methods.
- [ ] `read` refuses files over 1 MiB even when `offset`/`limit` asks for a
  small window, so large logs cannot be inspected.
- [ ] `config.Load` decodes over `Default()`, so a providers-only config
  inherits the placeholder `default` model.
- [ ] Documentation drift: the AGENTS.md package table omits `catalog`,
  `markdown`, and `docs/product`; the architecture dependency policy omits
  goldmark.

## Suggested order

1. Items 1 and 2 — small and self-contained.
2. One session-format change covering items 3, 4, 5, 6, and the structured
   summary from item 12.
3. The provider restructure: items 7, 8, 14, plus event call IDs from Tier 3.
4. Config versus state (item 9), then `app` consolidation (items 10 and 11).
5. Context accounting (item 13).
