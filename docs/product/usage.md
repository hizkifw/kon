# Using kon

## Sessions

kon prints a resume hint when it exits:

```text
resume with: kon --resume ses_7Yk2mP9Qa4Zx8Vc1Nd6R
```

Use that command, or `kon --resume` alone to reopen the most recent session
for the directory. A resumed session continues on the model it last used, and
the transcript shows where it switched models. If that model is no longer
configured, kon continues on your current default instead. Resuming never
changes `default_model`. `/resume` lists sessions inside the app, and `/resume <id>`
switches to one. Typing `/resume ` opens a picker. Highlighting a row previews
its recent turns without switching; `Esc` cancels the preview.

A session has one writer at a time. Resuming a session that another kon has
open, with `--resume` or `/resume`, shows it read-only and follows along: each
message, tool call, and result appears once the other kon writes it, and the
status line reads `read-only: open in another session` (with `· working` while
it is mid-turn). Streamed text and live command output are not shown, because
only finished messages are written to the session file. The picker marks such
sessions `open elsewhere`.

Once the other kon quits or leaves the session, the status line says the
session is free. Sending a prompt, or `/compact`, then continues the session
here. While the other kon still has it open, the prompt stays in the input and
nothing is sent. `/model` and effort changes wait until you have taken over;
`/new` and `/resume` leave the followed session as usual.

Sessions are plain JSONL files under `~/.local/share/kon/sessions/` by default.
On Windows, kon uses `%LOCALAPPDATA%\kon\sessions\`.

## Commands

Type `/` at the start of the prompt to see the available commands.

| Command | Action |
| --- | --- |
| `/new` | Start a new session with the active model |
| `/model [name]` | List model profiles, or switch to one |
| `/login <provider>` | Connect and check a supported provider |
| `/resume [id]` | List sessions for this directory, or switch to one |
| `/compact` | Summarize older context now |
| `/jobs [id]` | Pick a background job or subagent to preview, or show one's recent output |
| `/kill <id>` | Stop a running background job; the agent is told you stopped it |
| `/queue [item\|clear]` | Pick a pending steer or queued message to pull back for editing, or drop them all |

`/clear` is a hidden alias for `/new`: typing it works, and completing it fills
in `/new`.

## Steering and queueing

While kon is working you can keep typing:

- **Enter steers.** The message waits above the status line as `↳ steer`
  and reaches the agent the next time kon calls the model: after the running
  tool finishes, or, if the agent was writing its final answer, right after
  it, and the turn continues. Several steers sent before then arrive together
  as one message.
- **Tab queues.** The message waits as `⏵ queue` and is sent as its own
  prompt once the current turn finishes. Queued messages go one per turn, in
  order.
- **Esc with a steer pending** interrupts the turn and sends the steer at
  once.

An interrupted or failed turn holds the queue instead of sending the next
message, so you can decide first; Enter on an empty prompt sends the next one.
A steer a failed turn never delivered moves to the front of the queue.

`/queue` lists everything pending. Choosing a message takes it back into the
prompt, which cancels it; edit it and send it again with Enter to steer or Tab
to queue. `/queue clear` drops everything pending.

These are in-app commands. For the command-line surface, run `kon --help` to see
the available subcommands; `kon <command> --help` prints the flags for one.

## Scripting

`kon run` sends one prompt in the current directory without the full-screen
UI, and exits when the turn is done:

```sh
kon run "summarize what changed in this branch"
git diff | kon run --stdin review this
kon run --resume "now fix the first issue"
```

The message is the words after the flags. Flags must come first, and `--`
ends them early, so a message may contain words that look like flags. With no
words, piped stdin is the message (`echo hi | kon run`). To send both, pass
`--stdin`: stdin is appended to the words after a blank line
(`git diff | kon run --stdin review this`). kon reads stdin to the end before
sending anything, and only in those two cases, so a message given as words
never waits on a stdin that stays open.

Every assistant message streams to stdout as it is written, separated by blank
lines. When stderr is a terminal, kon also prints one line per tool call and
the resume hint there. A pipe or log file gets only the conversation.

| Flag | Effect |
| --- | --- |
| `--model <name>` | Use this model for this run. `default_model` is not changed. |
| `--effort <level>` | Use this reasoning effort for this run. It must be one of the model's levels. |
| `--resume`, `-r` | Continue the most recent session in this directory. |
| `--resume=<id>` | Continue a specific session. |
| `--format text\|json` | Stream text (the default), or write one JSON event per line. |

Each run is an ordinary session that you can resume later, in `kon run` or in
the full-screen UI. It never writes `config.json`, and its prompts are not added
to the Up-arrow history. A session that another kon has open cannot be
continued: `kon run --resume` exits with an error instead of following it.

Tools run without confirmation, exactly as they do in the full-screen UI. With
nobody watching, give kon only work you would let it do unattended.

Exit status is `0` when the turn completes, `1` on an error (including a model
that is not configured or a provider failure), `2` on a usage error, and `130`
when interrupted. A tool that fails does not fail the run; the model sees the
failure and carries on. The first Ctrl+C (or SIGTERM) stops the turn and keeps
what was written so far. A second Ctrl+C kills a command that ignored the
first.

### JSON events

With `--format json`, stdout carries one JSON object per line. Each object has
a `type`:

| Type | Fields |
| --- | --- |
| `session` | `session_id`, `model`, `cwd`. Written first. |
| `assistant` | `text`, `reasoning` when the model reasoned, and `partial: true` for a message cut off by an interrupt. |
| `tool_start` | `call_id`, `tool`, `arguments` (the model's JSON arguments). |
| `tool_done` | `call_id`, `tool`, `is_error`, `output` (what the model sees), `details` (tool-specific). |
| `compacted` | `tokens_before`, `estimated`. |
| `usage` | `context_tokens`, the context size the provider reported. |
| `result` | `session_id`, `text` (the last assistant message), `error` when the run failed, `duration_ms`. Written last. |

Events are whole messages, not streamed fragments. New fields and event types
may be added, so ignore ones you do not recognize.

## Upgrading

`kon upgrade` installs the latest GitHub release over the running executable.
It verifies the archive against the release checksums and runs the new binary
once before replacing anything, so a failed download leaves the current kon in
place. The new binary then applies any pending storage migrations, waiting for
other kon instances to exit first, and refreshes the model catalog as
`kon models --refresh` would. An offline refresh only warns. `kon upgrade --check` only reports whether a
newer release exists.

The release source is fixed and cannot be overridden. kon never checks for
updates on its own. Builds from source (`go run`, `go install`) report version
`dev` and cannot self-upgrade; reinstall them the same way instead. If the
executable's directory is not writable, rerun the installer or fix the
directory's permissions rather than running kon as root.

## Keys

| Key | Action |
| --- | --- |
| Enter | Submit the prompt, or accept the selected suggestion; while kon works, steer it; on an empty prompt, send the next held queued message |
| Shift+Enter / Ctrl+Enter | Insert a newline |
| Enter after a trailing `\` | Continue on a new line instead of submitting |
| Tab / Shift+Tab | Fill in the selected suggestion |
| Tab | Queue the prompt while kon works, when there is no suggestion to fill in |
| Shift+Tab | Cycle the model's reasoning effort when no suggestion popup is open |
| Up/Down | Recall prompts, or move the suggestion selection |
| Esc | Dismiss the popup, or interrupt the running turn and send any pending steer (press again to kill the command) |
| Page Up/Page Down | Scroll the transcript |
| Mouse wheel | Scroll the transcript |
| Ctrl+C | Clear the input, or hint at Ctrl+D when it is empty |
| Ctrl+D | Quit when the input is empty, cancelling active work first |
| Ctrl+U | Kill from the cursor to the start of the line |
| Ctrl+K | Kill from the cursor to the end of the line |
| Ctrl+W | Kill the word before the cursor |
| Ctrl+Y | Yank the last kill back at the cursor |
| Ctrl+R | Reverse-search prompt history; type to match, Ctrl+R or Up for older matches, Down for newer, Enter accepts, Esc cancels |

The status bar shows the working directory, context usage, and a status
message; the header shows the active model and, for a model with reasoning
effort levels, the selected level (`default` when kon sends none). `ctx ~12.4k/128.0k` means usage is
estimated; `?` means the provider has not supplied enough information yet.

## Tools and long conversations

Tool calls run serially in the directory where kon was started:

- `read` returns numbered file contents, at most 2,000 lines and 1 MiB.
- `write` atomically creates or replaces a file.
- `edit` replaces exactly one occurrence and fails on zero or multiple matches.
- `shell` runs a command through your configured `$SHELL`, falling back to
  `/bin/sh` when it is unset or missing. On Windows it uses Git Bash when
  available, then PowerShell, then `cmd.exe`. Every command carries a
  model-specified timeout, capped at 600 seconds; a timeout of 0 starts it as
  a background job instead. The tool description tells the model which
  interpreter it is.

### Background jobs

For servers, watchers, and long builds, the model can start a shell command as
a background job, by giving it a timeout of 0. The call returns at once and the job keeps running. The
status bar shows `⚙ N` while jobs run, and when a job exits kon tells the
agent, quoting the job's last lines of output: at its next step if it is
working, or by starting a turn if it is idle.

Each job is a directory of plain files beside the session, at
`<session>.jsonl.jobs/<id>/`: `cmd`, `pid`, `output` (combined stdout and
stderr, capped at 16 MiB), and `exit` once it has finished. Every shell
command, foreground or background, gets `KON_JOBS` (that directory) and
`KON_SESSION` (the session ID) in its environment, so the agent can list jobs
with `ls $KON_JOBS` and read or stop them with ordinary commands.

Jobs belong to the kon that started them. Quitting kon, `/new`, and `/resume`
kill every job still running, and its `exit` file records why. A job whose kon
crashed is marked lost the next time the session opens. `kon run` supports
background jobs too, but they end when it exits.

### Subagents

Ask the agent to use a subagent and it hands the task off: it runs `kon run
"<task>"` as a background job. The system prompt tells it subagents exist but
to use them only when you ask. The subagent works in the same directory with
the same tools and model, in a session of its own that records the parent
session. When it finishes, the exit notice quotes its whole final answer,
while the job's output keeps its full turn log. In
`/jobs`, a subagent's row previews its own conversation. Subagent sessions stay
out of `--resume` and the `/resume` list, which belong to the sessions you
started.

A subagent may delegate once more and no further: `kon run` refuses to start
more than two levels below the kon you are using, which it tracks with
`KON_DEPTH`.

There is no sandbox or confirmation prompt, and shell commands inherit your
environment. Run kon with the same care you would give any coding agent.

When the context nears the model's window, kon summarizes older turns and
continues. Original session entries remain in the JSONL file.
