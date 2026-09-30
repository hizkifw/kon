# Working with kon

This page covers the full-screen session: prompting, sessions, long-running
work, and getting text back out. Every key and command is also listed in the
[Reference](reference.md).

## Prompting

Type a prompt and press Enter. Shift+Enter or Ctrl+Enter inserts a newline, or
end a line with `\` before pressing Enter. Up and Down recall earlier prompts,
and Ctrl+R searches them.

### Mention files

Type `@` to pick a file from the current directory. Type part of a name to
narrow the list, or include a `/` for fuzzy paths: `@i/u/mod` finds
`internal/ui/model.go`. Tab or Enter inserts the reference, and Esc dismisses
the list.

A mention is plain text, such as `@internal/ui/model.go` or
`@"docs/design notes.md"`. kon does not attach the file; the agent reads it
when it needs to. The list follows your Git ignore rules and says when it is
incomplete, as it can be in a very large tree.

### Steer or queue while kon works

You can keep typing while kon is working:

- **Enter steers.** The message shows as `↳ steer` and reaches the agent at
  its next step, after the running tool finishes, and the turn continues with
  it. Several steers sent in a row arrive together.
- **Tab queues.** The message shows as `⏵ queue` and is sent as its own
  prompt when the turn finishes. Queued messages go one per turn, in order.

Press Esc twice to interrupt the turn; a pending steer is sent at once. After
an interrupted or failed turn the queue waits, so you can decide what to do
first. Press Enter on an empty prompt to send the next queued message.

`/queue` lists everything pending. Pick a message to take it back into the
prompt for editing, or run `/queue clear` to drop them all.

### Ask a side question

`/btw <question>` asks the active model about the conversation so far without
interrupting the task. The answer opens in a drawer while the main task keeps
running behind it. Press Esc to close it.

Side questions have no tools: the model can explain what it has already seen,
but cannot read files or run commands. Ask in the main conversation for that.
The question and answer never enter the conversation or the saved session,
though they still go to your provider and count toward the session's cost.

## Sessions

Each conversation is a session, saved as a plain JSONL file under
`~/.local/share/kon/sessions/` (`%LOCALAPPDATA%\kon\sessions\` on Windows). On
exit kon prints how to come back:

```text
resume with: kon --resume ses_7Yk2mP9Qa4Zx8Vc1Nd6R
```

`kon --resume` alone reopens the most recent session in the current
directory. Inside kon, type `/resume ` for a picker of this directory's
sessions with a preview of each, and `/new` to start a fresh one.

A resumed session continues on the model it last used, or on your default if
that model is no longer configured. Resuming never changes your default.

### A session open in another kon

Only one kon writes to a session at a time. If you resume a session another
kon has open, you see it read-only, and it follows along as the other kon
saves each message and tool result. The status bar reads
`read-only: open in another session`, and the picker marks the session
`open elsewhere`.

Once the other kon quits or moves on, the status bar says the session is free,
and your next prompt continues it here.

### Incognito

`kon --incognito` starts a session that is never saved. The conversation and
your prompts live in memory and are gone when kon exits, so there is nothing
to resume. The banner is drawn as a dashed outline while it is on. `/new`
starts another incognito session, and subagents it starts are incognito too.

Incognito covers the session and prompt history only. Prompts still go to your
provider, and `/login`, `/model`, and effort changes still save to the config.

## Background jobs

For servers, watchers, and long builds, the agent can start a shell command as
a background job. The status bar shows `⚙ N` while jobs run. When a job exits,
kon tells the agent and quotes the end of its output: at the agent's next step
if it is working, or by starting a turn if it is idle.

`/jobs` opens a list of the session's jobs. Select one to follow its output
live, and press ⇧K twice to stop a running job; the agent is told that you
stopped it. Esc goes back.

Jobs belong to the kon that started them. Quitting, `/new`, and `/resume` stop
every job still running.

Each job is a directory of plain files next to the session file, so you and
the agent can inspect it with ordinary commands. See
[Background job files](reference.md#background-job-files).

## Subagents

Ask the agent to use a subagent and it hands a task to a second kon, running
`kon run "<task>"` as a background job. The subagent works in the same
directory with the same tools, on your default model, in its own session. Its final answer
reaches the agent when it finishes, and its spending is added to the status
bar's cost as it goes. In `/jobs`, a subagent's entry shows its conversation.

The agent uses subagents only when you ask. A subagent may start one more
level of subagents, and no further.

## Web pages

The agent reads web pages by running `kon tool webfetch <url>`, which prints a
page as Markdown. You can use it yourself:

```sh
kon tool webfetch go.dev/doc/effective_go > effective_go.md
```

It keeps a page's main content when the page marks it, and prints JSON and
plain text as they are. Pages that build their content with JavaScript, and
sites behind bot protection, may come back empty or refused.

## Long conversations

When the conversation nears the model's context window, kon summarizes older
turns and carries on with the summary and the most recent turns. The summary
streams into the transcript. `/compact` does the same on demand. The full
history stays in the session file.

This relies on kon knowing the model's context window; see
[Context window and compaction](configuration.md#context-window-and-compaction).

## Context and cost

The header shows the active model and its reasoning effort; Shift+Tab cycles
the effort between turns. The status bar shows context usage, such as
`ctx 12.4k/128.0k`, and what the session has cost, such as `$0.42`. A `~`
marks an estimate, and `?` means the provider has not reported enough yet.

The cost is estimated from listed prices, and includes compaction and
subagents, plus side questions until you leave the session. It appears once a model with known prices has
answered; see [Cost](configuration.md#cost).

## Copying text

`/copy` copies the last reply as Markdown, and `/copy all` copies the whole
conversation.

In the transcript, drag to select and release to copy. Double-click copies a
word, and triple-click a paragraph. A reply copies as the Markdown behind it,
so code copies without the wrapping on screen. To use your terminal's own
selection instead, hold Shift while dragging (Option in iTerm2).

kon copies to the local clipboard with `pbcopy`, `wl-copy`, `xclip`, or
`xsel`, and to your terminal's clipboard with the OSC 52 escape sequence, which
also works over SSH. If copying does nothing:

- In iTerm2, allow clipboard access under Settings → General → Selection.
- Inside tmux, use tmux 3.2 or newer.
- Over SSH into a machine where kon runs inside tmux, the tmux on your own
  machine needs `set -g set-clipboard on`.

## Upgrading

`kon upgrade` installs the latest release over the running executable, and
`kon upgrade --check` only reports whether there is one. kon never checks for
updates on its own.

The download is verified against the release checksums and test-run before
anything is replaced. The new version then upgrades kon's stored data if its
format changed, which waits for other running kon instances to exit, and
refreshes the
[model catalog](configuration.md#model-catalog).

Builds from source (`go install`, `go run`) report version `dev` and cannot
upgrade themselves; reinstall them the same way. If the executable's
directory is not writable, rerun the installer rather than running kon as
root.
