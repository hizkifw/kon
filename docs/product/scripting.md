# Scripting

`kon run` sends one prompt without the full-screen UI and exits when the turn
is done, so kon fits into shell pipelines, scripts, and CI. `kon md` renders
Markdown the way kon shows a reply.

## Send one prompt

```sh
kon run "summarize what changed in this branch"
git diff | kon run --stdin review this
kon run --resume "now fix the first issue"
```

The message is the words after the flags. With no words, piped stdin is the
message. With `--stdin`, stdin is appended to the words after a blank line.
Flags must come first, and `--` ends them, so a message can contain words that
look like flags.

The reply streams to stdout. On a terminal it is rendered as Markdown; piped or
redirected, it is the Markdown exactly as the model wrote it. When stderr is a
terminal, kon also prints progress there: one line per tool call, retries,
compactions, and the resume hint. A pipe or log file gets only the reply.

Tools run without confirmation, as they do in the full-screen UI. Give
`kon run` only work you would let it do unattended.

## Flags

| Flag | Effect |
| --- | --- |
| `--model <name>` | Use this model for this run. Your default is not changed. |
| `--effort <level>` | Use this reasoning effort for this run. It must be one of the model's levels. |
| `--resume`, `-r` | Continue the most recent session in this directory. |
| `--resume=<id>` | Continue a specific session. |
| `--incognito` | Keep this run's session in memory only; it cannot be resumed. |
| `--format text\|json` | Stream text (the default), or [JSON events](#json-events). |
| `--stdin` | Append stdin to the message. |
| `--instructions <text>` | Add instructions as if from an `AGENTS.md`; repeat for more. |
| `--instructions-file <file>` | Add a file as if it were an `AGENTS.md`; repeat for more. See [Project instructions](configuration.md#project-instructions). |
| `--system-prompt-override <file>` | Replace kon's built-in instructions; see [System prompt](configuration.md#system-prompt). |

## Sessions

Each run is an ordinary session: resume it later with `kon run --resume` or in
the full-screen UI, unless it was incognito. A run never changes an existing
config or adds to the Up-arrow prompt history. It cannot continue a session that
another kon has open, and exits with an error instead.

Background jobs a run starts are stopped when it exits.

## Exit status

| Status | Meaning |
| --- | --- |
| `0` | The turn completed. |
| `1` | An error, such as a model that is not configured or a provider failure. |
| `2` | A usage error, such as an unknown flag. |
| `130` | Interrupted. |

A tool that fails does not fail the run; the model sees the failure and
carries on. The first Ctrl+C or SIGTERM stops the turn and keeps what was
written so far. A second Ctrl+C also kills a command that ignored the first.

## JSON events

`--format json` writes one JSON object per line to stdout, each with a `type`:

| Type | Fields |
| --- | --- |
| `session` | `session_id`, `model`, `cwd`. Written before the first message. |
| `assistant` | `text`; `reasoning` when the model reasoned; `partial: true` for a message cut off by an interrupt. |
| `tool_start` | `call_id`, `tool`, `arguments` (the model's JSON arguments). |
| `tool_done` | `call_id`, `tool`, `is_error`, `output` (what the model sees), and `details` (tool-specific). |
| `compacted` | `tokens_before`, `estimated`. |
| `retry` | `reason` (such as `rate limited (429)`), `attempt`, `max_attempts`, and `delay_ms` before the request is sent again. |
| `usage` | `context_tokens`, the context size the provider reported. |
| `result` | `session_id`, `text` (the last assistant message), `error` when the run failed, `duration_ms`. Always written last. |

Events are whole messages, not streamed fragments.

Rely on `result`, not `session`: a run that fails before the model answers
writes only `result`, with `error` set; `session_id` is there if the session
had started. A run that cannot
start at all, such as one given an unknown `--model`, writes nothing to stdout,
reports the error on stderr, and exits with status 1. An incognito run
has no session ID, so `session_id` is empty in `session` and absent from
`result`.

New fields and event types may be added; ignore those you do not recognize.

```sh
kon run --format json "list the TODOs" | jq -r 'select(.type == "result") | .text'
```

## Render Markdown

`kon md` prints Markdown as kon shows a reply: headings, emphasis, lists,
quotes, code, tables, and clickable links. It reads a file, or piped stdin
when no file is named:

```sh
kon md README.md
curl -s https://example.com/notes.md | kon md
```

Each block prints as soon as it closes, so streamed input renders as it
arrives. Lines wrap at 80 columns; `--width <n>` changes that, and `--width 0`
leaves wrapping to the terminal.

Color and links are written only to a terminal, so `kon md README.md > out.txt`
writes plain wrapped text. `NO_COLOR` turns color off, and `CLICOLOR_FORCE=1`
keeps it in a pipe, as for `less -R`. The colors are chosen for a dark
background.
