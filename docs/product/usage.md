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

A session can be open in only one kon at a time. The picker marks sessions
that are open elsewhere, and resuming one reports that it is open in another
kon.

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

`/clear` is a hidden alias for `/new`: typing it works, and completing it fills
in `/new`.

These are in-app commands. For the command-line surface, run `kon --help` to see
the available subcommands; `kon <command> --help` prints the flags for one.

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
| Enter | Submit the prompt, or accept the selected suggestion |
| Shift+Enter / Ctrl+Enter | Insert a newline |
| Enter after a trailing `\` | Continue on a new line instead of submitting |
| Tab / Shift+Tab | Fill in the selected suggestion |
| Shift+Tab | Cycle the model's reasoning effort when no suggestion popup is open |
| Up/Down | Recall prompts, or move the suggestion selection |
| Esc | Dismiss the popup, or interrupt the running turn (press again to kill the command) |
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
  model-specified timeout, capped at 600 seconds, and the tool description tells
  the model which interpreter it is.

There is no sandbox or confirmation prompt, and shell commands inherit your
environment. Run kon with the same care you would give any coding agent.

When the context nears the model's window, kon summarizes older turns and
continues. Original session entries remain in the JSONL file.
