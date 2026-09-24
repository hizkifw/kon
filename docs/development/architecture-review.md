# Architecture review (2026-09-23, refreshed 2026-09-24)

A whole-codebase review first taken at commit `0bbfa07`, before the session
format shipped, and re-verified against `dca30c2`. The goal is to find
structural decisions that would be cheap to change now and expensive later.
Line references point at `dca30c2`.

The foundation is sound: package boundaries are mostly real, durability is
taken seriously, and the cache-stable system prompt is a deliberate design. The
problems cluster in two places — the **durable session schema** and the
**provider seam** — which is exactly where a mistake now becomes a migration
later. The session-schema work (Tier 1) has landed; the provider seam (Tier 2)
has not, and has grown since the first pass.

Each item carries a checkbox so this page can track the work. Remove or update
an item when it lands.

## Tier 1 — fix first (correctness and durable format)

### 1. A cancelled or crashed turn can permanently break a session

- [x] Persist a result for every tool call before returning
- [x] Repair unanswered calls during context projection

A cancelled run used to return after the current tool, leaving the remaining
calls in the same assistant message without results. OpenAI-compatible servers
reject that history, so every later request failed, and compaction could not
recover because it sends the same prefix.

Fixed: on cancellation the runner appends an interrupted result for each
remaining call (`appendInterruptedToolResults`, `internal/agent/agent.go:308`).
As a crash backstop, `Store.Context` runs `repairUnansweredToolCalls`
(`internal/session/session.go:965`), which synthesizes the same result for any
call that still has none. The assistant message is persisted before tools
execute, so a process killed during a long shell command is covered too.

### 2. Quitting mid-run can hang kon on exit

- [x] Make `emit` non-blocking once the run is abandoned
- [x] Cancel the run on every exit path, not only Ctrl+D

Ctrl+D cancels the active run before quitting. Agent events and the final
completion message select on the run's context (`runAndForward`,
`internal/ui/model.go:646`), so abandoned sends cannot hold the runner open
after cancellation. A closed event channel after cancellation is treated as an
interrupted run (`waitRunEvent`). `TestRunForwardingStopsWhenEventsAreNoLongerRead`
covers abandoned event and completion sends.

Fixed: a SIGTERM or SIGINT quits Bubble Tea without passing through the key
handler, so the UI never cancelled the run's context. Once no one read the
events, a shell live-display tick could block in `emit`, and the shell tool
waits for its reporter before returning, so `Runtime.Close` waited forever.
Runs and logins now derive from a program-lifetime context passed to `ui.New`,
which `cmd/kon/session.go` cancels as soon as `program.Run` returns and before
`Runtime.Close`. `TestCancellingProgramContextReleasesAbandonedRun` covers it.

### 3. Tool outcome is not persisted; display parses model-facing text

- [x] Persist `is_error` on tool results
- [x] Persist structured tool `details` alongside model-facing content

Tool-result messages persist `is_error` and tool-owned `details`. Shell results
record exit code, duration, and output length; text reads record line count.
Live and replayed displays use those fields. Legacy results without details
still use the model-facing text as a fallback.

### 4. Make ordered parts the single source of message content

- [x] Redesign `session.Message` as `{Role, Parts}`
- [x] Let backends return ordered parts with their own metadata

Messages persist content only as ordered parts (`text`, `reasoning`,
`tool_call`, `tool_result`, `image`). Plain text and tool-call views are
derived. The chat backend assembles streaming parts in arrival order, and a
complete response carries those parts unchanged into the session; a partial or
interrupted stream keeps its text and reasoning but drops unfinished tool calls
(`internal/provider/provider.go:76`). Each part can retain opaque
provider-owned JSON metadata. This introduced the v2 session format.

### 5. Move images out of session lines

- [x] Content-addressed blob storage beside session files

Images live in content-addressed files beside each session. Parts persist only
the SHA-256 hash and MIME type; duplicate bytes share one blob, and the
provider reads and verifies the blob only when building a vision request. This
advanced the session format to v3.

### 6. Rename the wire-format field in model-change entries

- [x] Rename `ModelSelection.Provider`

Model changes record `wire_format` and, for models resolved through a named
connection, `connection_id`. Standalone profiles omit `connection_id` because
they do not use one. The current session format is v4; the later `turn_start`
and `turn_end` entries were added without a version bump. Storage upgrades now run through `internal/migrate` and
`internal/migrations` (see item 19). See item 15 for the broader vocabulary
problem.

## Tier 2 — restructure before building more

### 7. Replace scattered wire-type strings with one provider-owned table

- [x] Single wire-format table in `internal/provider`

`internal/provider/wire` now holds the only list of wire formats and what
each implies: default base URL (and so whether one is required), API path,
nested versus top-level reasoning effort, default reasoning field, and
whether model listing is optional. It is a leaf package, so `config`
validates against it without importing the backends. `newModel`, the chat
backend, and `Discover` read it instead of switching on type strings.

Service quirks stay keyed on service identity, not format. Login entries
(`provider.LoginEntry`) say whether to ask for a URL or key, so `ui/login.go`
no longer knows about Ollama. `app.catalogKey` decides which models.dev
entry describes a connection, folding in the Azure and local-login
exceptions. OpenRouter's key check still lives in `Discover`, and DeepSeek is
still detected by base URL, since explicit profiles carry no service identity.

### 8. Separate user-written profiles from resolved runtime specs

- [ ] Introduce a resolved spec type; drop `agent`'s `config` import

`config.Model` is both what the user wrote and the resolved runtime profile:
`app.resolveModel` (`internal/app/models.go:224`) injects `Vision`,
`ContextWindowTokens`, and the runtime-only `ReasoningEffort` from the catalog
and config. `agent.New` (`internal/agent/agent.go:69`) takes `config.Model` and
`config.Compaction`; `provider.New` (`provider.go:40`) takes `config.Model`.
Introduce a resolved spec (for example `provider.Spec`) and agent-owned options
so config stays pure input.

### 9. Stop rewriting the user's config at runtime

- [ ] Move machine-written state (last model, reasoning effort, credentials) to
  the data directory
- [ ] Support `api_key_env`

`Runtime.SwitchModel` (`internal/app/app.go:324`) and `Runtime.CycleEffort`
(`internal/app/models.go:148`, bound to Shift+Tab) save the in-memory config
loaded at startup. They silently discard edits made to `config.json` while kon
runs, including another instance's `/login`, and materialize defaults the user
never wrote. `CycleEffort` also writes the machine-owned `reasoning_effort`
field on every keypress. `Runtime.Login` re-reads before saving
(`models.go:283`), so the paths already disagree, and the package map says
config must not own runtime mutation.

`SwitchModel` also appends the model-change entry before the save, so a failed
save leaves the session log naming a model the runner never switched to.

Keep `config.json` user-authored and read-only to kon. Write the last-used
model and effort to a state file and credentials to an `auth.json` (0600) in
the data directory. That also keeps API keys out of dotfile repositories. The
catalog already carries each provider's `Env` names, so `api_key_env` is a
natural addition.

### 10. Consolidate runner construction and session swaps in `app`

- [ ] One `buildRunner`, one operation guard, one session swap

`agent.New(profile, compaction, client, store, tools.New(cwd, vision))` appears
five times: `app.go:133` (`createRunner`), `app.go:523` (`openStore`),
`models.go:111` (`resolveActive`), `models.go:153` (`CycleEffort`), and
`models.go:325` (`Login`), with subtly different behavior (only `createRunner`
appends a model change). `Run` and `Compact` (`app.go:203`, `:247`) duplicate
the busy-phase begin/end logic; `Resume` and `NewSession` (`app.go:391`,
`:419`) duplicate the swap logic. The three constructors already share
`start`.

`Close` has a related window: after waiting for the active run it re-checks
only for `PhaseClosed`, so an operation that starts in the gap can have its
store closed underneath it. One operation guard would close that too.

### 11. Decide what model a resumed session uses

- [x] Restore the recorded model, or record the switch
- [x] Stop naming the model in the frozen system prompt, or accept that it goes stale

`Runtime.openStore` now restores the session's last model-change entry, and
appends one only when a different model will answer (the recorded profile no
longer resolves, or it now names another model ID). Resuming does not touch
`default_model`. Replay renders each later model change the way a live switch
does, re-titled once the catalog loads.

The system prompt no longer names the model, the approach pi takes. The prompt
is never rebuilt, so a named model went stale after `/model`; announcing a
switch in a later message was considered and rejected, because some models
distrust a user message that speaks as the system, and not every server
accepts a system message after the first. Sessions created before this change
keep the `Current model:` line in their stored prompt.

### 12. Keep prompt wording out of `session`

- [ ] Return a structured summary item from `Store.Context`

`Store.Context` wraps the compaction summary in `CompactionSummaryPrefix`
(`internal/session/session.go:34`, applied at `:950`), and
`agent.projectedSummary` (`internal/agent/agent.go:526`) strips it back off.
`ContextMessage.Summary` already flags the item, so only the wording has to
move to the agent.

### 13. Use reported usage for compaction decisions

- [ ] Reported prompt tokens plus an estimate of messages since
- [ ] Fix `/compact` messages for short sessions and missing context windows
- [ ] Check the summary's finish reason

`Runner.usageFor` (`internal/agent/agent.go:114`) trusts provider usage only
when the projected message count equals the count at the time it was reported
(`:117`). Every tool-loop iteration has appended tool results by then, so the
pre-request threshold check runs on a pure bytes/4 estimate that ignores
images. Conversely, the count stamped after an assistant tool-call message
already includes the repaired placeholder results, so the real results can be
mistaken for measured usage. Use the reported prompt tokens plus an estimate
of only the newer messages.

Related:

- Forcing `/compact` on a short session reports "active turn is too large to
  compact safely" (`agent.go:366`); with no context window it reports "nothing
  to compact" (`:343`). `Runtime.Compact` never resolves the active model, so a
  catalog-derived profile has no window until its first `Run`.
- The `!compacted` branch of the overflow retry (`agent.go:232`) is
  unreachable.
- The summary request ignores `finish_reason`, so a summary cut off at its
  token limit is persisted and older turns are dropped for good. With a
  reasoning effort set, reasoning can consume the whole summary budget and fail
  the turn with "empty assistant message".

### 14. Move login and connection mapping out of the wire-backend package

- [ ] Relocate `provider/registry.go` and `provider/discovery.go`
- [ ] Give tool specs a neutral home

Login mapping makes `provider` import `catalog` and `config`. `tools` imports
`provider` only for the `Tool` struct, so the tools package transitively pulls
in the catalog, config, and session packages. Move connection mapping to `app`
or a dedicated package, and define tool specs where both sides can use them
without the dependency.

### 15. Fix the vocabulary

- [ ] Write a glossary and rename to match

There are five `Model` types (`ui.Model`, `app.Model`, `config.Model`,
`provider.Model`, `catalog.Model`) and four meanings of "provider": a config
connection (`config.Provider`), a catalog vendor (`catalog.Provider`), the Go
package, and `agent.Provider` (implemented by `provider.Client`).
`config.SupportedProviders` and `Model.Type` still say "provider" and "type"
for what the session format now calls the wire format. Pick terms — wire
format, connection, profile, model — document them here, and rename. This is
cheap now and compounds quickly.

## Tier 2b — found in the refresh

### 16. Reasoning replay matches on model ID alone

- [ ] Key replayed reasoning on connection as well as model

Reasoning parts are replayed to "the model that wrote them" by comparing model
IDs (`chatReplay`, `internal/provider/chat.go`). The same model ID on two
connections — OpenRouter and the vendor directly — receives the other
connection's opaque `reasoning_details`, and messages with no recorded model
match every model. The model-change entry already records `connection_id`;
compare that too.

### 17. Reasoning effort is global and unrecorded

- [ ] Record effort per session, not in `config.json`

`config.ReasoningEffort` is one top-level field (`internal/config/config.go:25`)
shared by every profile, and the model-change entry does not record it, so a
session cannot say what effort a turn ran at. Folding effort into the model
selection that item 9 moves to state would fix both.

### 18. The tool roster already varies by machine

- [ ] Freeze the tool roster per session (promoted from Tier 3)

The shell tool's description names the platform interpreter
(`internal/tools/shell.go:86`, via `shellName`). Tool definitions are part of
the cached prefix, so resuming a session on another OS or shell changes the
prefix without any MCP or dynamic tools. Persist the roster (or its hash) with
the session and reuse it on resume.

### 19. Frozen migrations depend on the live session schema

- [ ] Give each migration step frozen copies of the types it writes

`internal/migrations/v002_v011_sessions.go` builds its output from
`session.Part`, `session.ToolCall`, and `session.Role`, validates with
`session.ValidateFile`, and compares against `session.SchemaVersion`;
`v001_baseline.go` checks `session.SchemaVersion` too. The next schema bump will
change what these historical steps produce. Each step should own the shapes it
reads and writes and validate against its own target version. A session file
with no parseable header (the normal result of a crash right after creation)
also aborts the whole migration, and with it startup; skip or quarantine it
instead.

### 20. Record the self-update trust model

- [ ] Document that updates are integrity-checked, not signed

`kon upgrade` verifies the archive against `checksums.txt` from the same
release (`internal/selfupdate/selfupdate.go:165`) and then executes the
candidate to verify it. Authenticity rests entirely on TLS and the GitHub
account. That is a reasonable choice for now; write it down so it is a
decision rather than an accident, and revisit before adding a second download
source.

## Tier 3 — seams for what comes next

- [ ] **Tool call IDs on agent events.** `agent.Event` carries no call ID
  (`internal/agent/agent.go:44`), and the UI attaches live snapshots and results
  to "the trailing tool block" (`internal/ui/transcript.go:176`); parallel tool
  execution breaks that. Replay already keys on IDs (`callArgs`,
  `internal/ui/events.go:104`).
- [ ] **Replace `Tool.Interrupt`.** Every tool implements it (three no-ops),
  `Registry.InterruptAll` broadcasts, and `shellTool` has a single `running`
  slot. A per-call context plus a kill signal in `Env` is simpler and allows
  concurrent calls. `read` also ignores its context, so reading a device file
  or FIFO cannot be interrupted.
- [ ] **Approval hook between agent and executor.** "Tools execute without a
  sandbox or confirmation" is hard-coded in the system prompt
  (`agent.go:147`). A `Policy` interface with an allow-all default costs little
  now.
- [ ] **Streaming tool-call events.** A large `write` shows nothing while its
  arguments stream.
- [ ] **Headless frontend** (`kon -p`, JSON event stream). `app.Runtime` is
  nearly UI-agnostic; keep `agent.Event` (which now also carries
  `tools.Display`, `Details`, and `tokens.Count`) stable enough to serialize.
- [ ] **Keep blocking work off the Bubble Tea update loop.** All of these run
  synchronously in `Update`:
  - `Resume` scans session headers and parses the whole file;
  - `NewSession` fsyncs and walks directories;
  - `SwitchModel` and `CycleEffort` fsync the config;
  - `Sessions()` runs on every `/resume` completion keystroke, and highlighting
    a row reads that session's tail;
  - `Models()` runs on every `/model` completion keystroke;
  - each prompt submit fsyncs the history file.

  `Runtime.Models` holds `r.mu` while the catalog decompresses
  (`internal/app/models.go:24`), which blocks `State()`.
- [ ] **Stream idle timeout and retry with backoff** for 429/5xx and dropped
  connections. The streaming client has no timeout (`internal/provider/chat.go:82`),
  so a stalled server hangs until Esc.

## Smaller items

- [ ] `atomicWrite` (`internal/tools/fsutil.go:14`) renames over the target,
  replacing a symlink with a regular file (stow-managed dotfiles, or
  `CLAUDE.md -> AGENTS.md`) and dropping hardlinks and ownership. The link
  target is left unchanged while the tool reports success. Resolve symlinks
  before writing.
- [ ] The `Env.Resolve` comment says "under the workspace root", but any
  absolute or `..` path is accepted.
- [ ] `defaultDisplays` and `defaultRegistry` register the same four tools. The
  UI calls `tools.Describe` directly (`events.go:69`) and also through
  `Runtime.DescribeTool`.
- [ ] Replay reads `session.Entry` (including the turn and compaction entry
  types), `provider.PartReasoning`, and `typedid` directly. A runtime-provided
  transcript view model would decouple `ui` from the schema. The `ui.Runtime`
  interface has 19 methods.
- [ ] `read` refuses files over 1 MiB even when `offset`/`limit` asks for a
  small window, so large logs cannot be inspected.
- [x] `config.Load` decodes over `Default()`, so a providers-only config
  inherited the placeholder `default` model — and, because `encoding/json`
  reuses slice elements, the user's first `models[]` entry inherited its
  OpenAI `base_url`. The default config no longer ships a model.
- [ ] `ui` owns history file I/O through `internal/history`
  (`internal/ui/prompt_history.go`); the package map says `ui` owns
  presentation.
- [ ] Documentation drift: the AGENTS.md package table omits `catalog`,
  `markdown`, `migrate`, `migrations`, and `docs/product`. AGENTS.md's
  dependency list omits `charmbracelet/x/ansi` and `gofrs/flock`, and the
  dependency policy in `architecture.md` omits those and goldmark.

## Correctness backlog

Bugs found in the refresh that are not structural. Most are small; the first
group can make kon unusable until the user intervenes by hand.

Blocks the user:

- [x] Releases publish with empty notes: `gh release create` needs
  `--notes-from-tag` (`.github/workflows/release.yml:22`), and the checkout
  should fetch the annotated tag object. Both are in the workflow now.
- [x] A history line over 2 MiB (one large paste) makes `history.Load` fail,
  and kon refuses to start (`internal/history/history.go`). The bounded tail
  is now read whole, with no per-line limit.
- [x] A missing or corrupt image blob fails every later request, including
  compaction (`internal/provider/chat.go:186`). Send a text placeholder.
  `messageContent` now substitutes one.
- [x] Image parts are sent after `/model` switches to a model without vision;
  `Runner.vision` is set but never read (`internal/agent/agent.go:61`). The
  chat backend now sends a placeholder instead, and the unused field is gone.
- [x] A write that fails partway (disk full) leaves a torn line that the next
  append joins, putting a corrupt line mid-file that `Open` refuses
  (`internal/session/session.go:886`). Truncate to the last good offset on
  write failure. `writeLine` now does, and a failed truncation stops appends.

Live view and tools:

- [ ] After Esc, `runAndForward` drops about half of later events at random:
  the send and `ctx.Done()` are both ready (`internal/ui/model.go:648`). A
  cancelled shell block can stay "running".
- [ ] Tool-call deltas without `index` merge into one call
  (`internal/provider/chat.go:889`); start a new call when a different ID
  arrives on the same index.
- [ ] Malformed tool arguments after a clean stream discard the text and
  reasoning that already streamed.
- [ ] Shell display slices output by a byte count measured before `sanitize`
  strips `\r` and escapes, garbling progress bars and colored output.
- [ ] Tabs count as zero or one cell but render as four, so code blocks and
  shell output overflow the width.
- [ ] A multi-line status (an HTML error body) breaks the frame and reaches the
  terminal unsanitized; `sanitize` also leaves OSC 8 remnants.
- [ ] Markdown span styling locates spans with `strings.Index`, so a code span
  or link styles the first matching word instead of itself.
- [ ] `fitLine` counts escape bytes as cells and can drop the SGR reset.
- [ ] `read` counts a trailing newline as an extra line.
- [ ] `edit` counts matches with `bytes.Count`, so overlapping occurrences pass
  the exactly-once check; CRLF files cannot be edited across lines.
- [ ] A prompt that begins with `/` (a path) is always parsed as a command.

Installers and display:

- [ ] `install.sh` overwrites the binary with `cp` (fails with "Text file busy"
  while kon runs, and can invalidate the macOS signature); write a temp file
  and `mv`. Its `/releases/latest` fallback never triggers because the parsed
  version is `releases`.
- [ ] Token counts from 999,950 to 999,999 display as "1000.0k"
  (`internal/tokens/tokens.go`).
- [ ] The catalog generator validates less than the runtime decodes, so an
  upstream type change silently removes a provider's models.

## Suggested order

1. The "blocks the user" group of the correctness backlog, and the item 2
   exit-path gap — each is small and self-contained.
2. Item 11 and item 18 together: decide what a resumed session runs on and what
   it is told, and freeze the tool roster, before more sessions accumulate.
3. The structured summary (item 12) and the compaction fixes (item 13).
4. The provider restructure: items 7, 8, 14, and 16, plus event call IDs from
   Tier 3.
5. Config versus state (items 9 and 17), then `app` consolidation (item 10).
6. Migration hygiene (item 19) before the next session-format bump.
