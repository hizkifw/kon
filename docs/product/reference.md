# Reference

Every command, key, tool, and setting in one place. For how to use them, see
[Working with kon](usage.md), [Scripting](scripting.md), and
[Configuration](configuration.md).

## Command line

| Command | Does |
| --- | --- |
| `kon` | Start a session in the current directory. |
| `kon --resume`, `kon -r` | Reopen the most recent session in this directory, or start one if there is none. |
| `kon --resume <id>` | Reopen a specific session. |
| `kon --incognito` | Start a session that is never saved. It cannot be combined with `--resume`. |
| `kon --system-prompt-override <file>` | Replace kon's built-in instructions in new sessions; see [System prompt](configuration.md#system-prompt). `kon run` and `kon acp` take it too. |
| `kon --instructions <text>`, `kon --instructions-file <file>` | Add text or a file to new sessions as if it were an `AGENTS.md`; both repeat. See [Project instructions](configuration.md#project-instructions). `kon run` and `kon acp` take it too. |
| `kon --version` | Print the version. |
| `kon run [flags] <message>` | Send one prompt without the full-screen UI; see [Scripting](scripting.md#flags). |
| `kon acp` | Serve the Agent Client Protocol on stdin and stdout for an editor; see [Editor integration](acp.md). |
| `kon md [--width <n>] [file]` | Render Markdown for the terminal, wrapped at 80 columns unless `--width` (or `-w`) says otherwise; see [Render Markdown](scripting.md#render-markdown). |
| `kon models [--refresh]` | List the catalog's model IDs; `--refresh` downloads the latest catalog first. |
| `kon upgrade [--check]` | Install the latest release; `--check` only reports whether there is one. |
| `kon docs` | Write this guide to a local directory as Markdown, and print only that directory's path, as in `ls "$(kon docs)"`. |
| `kon tool webfetch <url>` | Print a web page as Markdown; see [Tools](#tools). |
| `kon tool websearch [-n <count>] <query>` | Search the web with the configured provider; see [Tools](#tools). |

`kon --help` lists the commands, and `kon <command> --help` the flags of one.

## Slash commands

Type `/` at the start of the prompt to see these.

| Command | Does |
| --- | --- |
| `/new` | Start a new session with the active model. `/clear` does the same. |
| `/model [name]` | List models, or switch to one and save it as the default. |
| `/login <provider>` | Connect to a provider and fetch its model list for `/model`. |
| `/resume [id]` | Pick a session from this directory, or switch to one. |
| `/compact` | Summarize older context now. |
| `/btw <question>` | Ask a side question about the conversation, without tools. |
| `/jobs` | Watch and stop background jobs and subagents. |
| `/queue [n\|clear]` | Pick a pending steer or queued message to take back for editing, by its position, or drop them all. |
| `/copy [all]` | Copy the last reply, or the whole conversation, as Markdown. |
| `/help` | Read this guide in a drawer. |

## Keys

### Prompt

| Key | Does |
| --- | --- |
| Enter | Send the prompt. |
| Shift+Enter, Ctrl+Enter | Insert a newline. Some terminals do not report these; use `\` then Enter. |
| `\` then Enter | Continue on a new line. |
| Up, Down | Recall earlier prompts, from every directory. In a prompt of several lines, move the cursor. |
| Ctrl+R | Search earlier prompts; see [History search](#history-search). |
| Shift+Tab | Cycle the model's reasoning effort, between turns. |
| Ctrl+U, Ctrl+K | Delete to the start, or the end, of the line. |
| Ctrl+W | Delete the word before the cursor. |
| Ctrl+Y | Paste back the last deleted text. |
| Ctrl+C | Clear the prompt. |
| Ctrl+D | Quit, on an empty prompt. Any running turn is stopped first. |

### While kon works

| Key | Does |
| --- | --- |
| Enter | Steer: send the message to the agent at its next step. |
| Tab | Queue: send the message as its own prompt after this turn. |
| Esc, Esc | Interrupt the turn, and send any pending steer. The first press only warns. |
| Esc, a third time | Kill a command that is still running. |
| Enter on an empty prompt | After an interrupted or failed turn, send the next queued message. |

### Suggestions

These apply while a `/` command or `@` file list is open.

| Key | Does |
| --- | --- |
| Up, Down | Move through the suggestions. |
| Tab | Fill in the selected suggestion. |
| Shift+Tab | Move to the previous suggestion. |
| Enter | Accept the suggestion. |
| Esc | Close the list. |

### History search

| Key | Does |
| --- | --- |
| Type | Find the newest earlier prompt that matches. |
| Ctrl+R, Ctrl+S | Move to an older, or newer, match. |
| Backspace | Remove a character from the search. |
| Esc | Keep the match in the prompt. |
| Ctrl+G | Cancel, and restore what you were typing. |
| Any other key | Keep the match and act on it, so Enter sends it. |

### Transcript

| Key or action | Does |
| --- | --- |
| Page Up, Page Down, mouse wheel | Scroll. |
| Drag | Select text, and copy it on release. Dragging past the edge scrolls. |
| Double-click, triple-click | Copy a word, or a paragraph. Keep dragging to extend by words or paragraphs. |
| Shift+drag (Option+drag in iTerm2) | Use the terminal's own selection. |

### Drawers

`/btw`, `/jobs`, a job's output, and `/help` open in a drawer. The bottom row
of a drawer lists its keys, and clicking one works like pressing it.

| Key | Does |
| --- | --- |
| Up, Down, `k`, `j` | Scroll, or move through a list. |
| Page Up, Page Down, Home, End | Scroll by a page, or to either end. |
| Enter | Open the selected job, or follow the link in focus in `/help`. |
| Tab, Shift+Tab | In `/help`, move the focus to the next or previous link. |
| Backspace | In `/help`, go back to where the last link was followed from. |
| ⇧K, ⇧K | Stop the selected job. Any other key in between cancels. |
| Esc, Ctrl+C, or a click outside | Close the drawer. |
| Ctrl+D | Quit kon. |

`/help` follows a link to another page or section of the guide in the same
drawer, by Enter or by a click on it; a web link is left to the terminal.
Dragging selects and copies there as it does in the conversation. Its
top row names the page and the section scrolled to. Click the page there to
go to its top, or the guide to go to its first page.

## Status bar

| Shows | Means |
| --- | --- |
| The working directory | Where tools run. |
| `ctx 12.4k/128.0k` | Context used, of the model's window. `~` marks an estimate, and `?` an unknown. |
| `$0.42` | Estimated cost of the session so far, including subagents. `<$0.01` below a cent. |
| `⚙ N` | Background jobs running. |
| `read-only: …` | A session open in another kon; see [Sessions](usage.md#a-session-open-in-another-kon). |
| Other messages | The result of your last command. |

The header at the top of the screen shows the active model and, for a model
with effort levels, its reasoning effort (`default` when kon sends none).

## Tools

The model has four tools. They run one at a time, in the directory where kon
started, without asking first.

| Tool | Does |
| --- | --- |
| `read` | Return a file's lines, numbered, up to 2,000 per call; the model asks for more by offset. Text files over 1 MiB are refused. Images, audio, video, and PDFs are passed whole to models that [accept them](configuration.md#media). |
| `write` | Create or replace a whole file, atomically. The directory must already exist. |
| `edit` | Replace one exact piece of text in a file. It fails if the text occurs zero times or more than once. |
| `shell` | Run a command, with a timeout the model chooses, of up to 600 seconds. A timeout of `0` starts a [background job](#background-job-files). Output past 64 KiB is cut. |

`shell` runs your `$SHELL`, or `/bin/sh` if that is unset or missing. On
Windows it uses Git Bash when installed, then PowerShell, then `cmd.exe`.
Commands inherit your environment.

The agent reaches more tools through its shell, as `kon tool <name>`:

| Tool | Does |
| --- | --- |
| `kon tool webfetch <url>` | Print a web page as Markdown, or other text as it arrived. A URL without a scheme uses https. It reads at most 5 MiB and gives up after 30 seconds. Links within the site print as paths from its root; after a redirect, the final URL is noted on stderr. Images, PDFs, and other non-text responses are errors. |
| `kon tool websearch [-n <count>] <query>` | Print search results from the provider set as [`web_search.provider`](configuration.md#web-search): a numbered title, the URL, and a date and snippet when the provider gives them. The query is every argument joined by spaces. `-n` asks for 1 to 20 results, 5 by default. It gives up after 30 seconds, and is an error while no provider is set. |

## Background job files

Each background job is a directory next to the session file, at
`<session>.jsonl.jobs/<id>/`. An incognito session uses a temporary directory
that is removed when the session ends.

| File | Holds |
| --- | --- |
| `cmd` | The command. |
| `pid` | Its process ID. |
| `output` | Combined stdout and stderr, up to 16 MiB. |
| `exit` | How it ended, written once it has. |
| `session` | A subagent's session ID. |
| `answer` | A subagent's final answer. |

When a job ends, the agent is shown its last 20 lines of output. A subagent's
final answer is quoted whole, up to 64 KiB. A job whose kon crashed is marked
lost the next time its session opens.

`/jobs` marks each job `● running`, `✓ done`, `■ stopped by you`,
`■ stopped when kon exited`, `■ lost when kon exited`, or `✗` with how it
failed.

## Environment variables

Read by kon:

| Variable | Effect |
| --- | --- |
| `XDG_CONFIG_HOME` | Where the config directory goes, on every OS. |
| `XDG_DATA_HOME` | Where the data directory goes, on every OS. |
| `SHELL` | The shell the `shell` tool runs. |
| `NO_COLOR` | Turn off color. |
| `CLICOLOR_FORCE` | Keep color when output is not a terminal, as in `kon md \| less -R`. |

Set by kon for every shell command the agent runs:

| Variable | Holds |
| --- | --- |
| `KON_SESSION` | The session ID. |
| `KON_JOBS` | The session's job directory, so `ls $KON_JOBS` lists its jobs. |
| `KON_JOB` | In a background job only, that job's own directory. |
| `KON_DEPTH` | How many kon agents the command runs beneath: `1` under your own session. `kon run` refuses to start above `2`. |
| `KON_INCOGNITO` | Set in an incognito session, so subagents are incognito too. |
| `PATH` | Your `PATH`, with a directory holding only `kon` put first, so `kon` runs the kon you are using. |

## Files

| Path | Holds |
| --- | --- |
| `~/.config/kon/config.json` | [Configuration](configuration.md). `%APPDATA%\kon\` on Windows. |
| `~/.config/kon/AGENTS.md` | Your own [instructions](configuration.md#project-instructions) for every project. |
| `~/.local/share/kon/sessions/` | Sessions, as JSONL. `%LOCALAPPDATA%\kon\` holds the data directory on Windows. |
| `~/.local/share/kon/history.jsonl` | Prompt history for Up and Ctrl+R. |
| `~/.local/share/kon/models.json.gz` | The downloaded model catalog, when newer than the bundled one. |
| `~/.local/share/kon/provider-models.json` | Model lists fetched at login. |
| `~/.local/share/kon/docs/` | This guide, written by `kon docs`. |
| `~/.local/share/kon/bin/` | A link to each kon executable you have run, which the agent's shell finds as `kon`. |

## Config fields

### Top level

| Field | Type | Default | Meaning |
| --- | --- | --- | --- |
| `default_model` | string | `""` | The model for new sessions: a profile `name`, or `<connection-id>/<model-id>`. `/model` sets it. |
| `reasoning_effort` | string | none | The effort last chosen with Shift+Tab. Ignored if the model does not list it. |
| `providers` | array | none | [Connections](#providers) shared by all their models. |
| `models` | array | `[]` | [Model profiles](#models) defined by hand. |
| `compaction.reserve_tokens` | integer | derived | Room kept free below the context window. |
| `compaction.keep_recent_tokens` | integer | derived | Recent context kept verbatim when compacting. |
| `context_files` | boolean | `true` | Load the project's `AGENTS.md` files. |
| `web_search.provider` | string | `""` | The [web search](configuration.md#web-search) provider: `brave`, `duckduckgo`, `exa`, `perplexity`, `searxng`, or `tavily`. Empty turns search off. |
| `web_search.providers` | object | none | [Search connections](#web-search-providers), keyed by provider name. |

### Providers

| Field | Type | Required | Meaning |
| --- | --- | --- | --- |
| `id` | string | yes | Unique name, and the prefix of its models' names. |
| `type` | string | yes | [Wire format](configuration.md#wire-formats). |
| `catalog_provider` | string | no | The models.dev provider, when it differs from `id`. |
| `base_url` | string | for `openai-compatible` | API root; other formats have a default. |
| `api_key` | string | no | Sent as a bearer token, or as `x-api-key` for `anthropic`. |
| `headers` | object | no | Extra HTTP headers, which may override kon's own. |

### Web search providers

Each key of `web_search.providers` is a provider name, and its value has:

| Field | Type | Required | Meaning |
| --- | --- | --- | --- |
| `api_key` | string | for `brave`, `exa`, `perplexity`, `tavily` | The provider's API key. |
| `base_url` | string | for `searxng` | The server's root; the hosted providers have a default. |

### Models

| Field | Type | Required | Meaning |
| --- | --- | --- | --- |
| `name` | string | yes | Unique name, used by `/model` and `default_model`. No `/`. |
| `model` | string | yes | The provider's model ID, sent as written. |
| `type` | string | no | [Wire format](configuration.md#wire-formats); `openai-compatible` if omitted. |
| `base_url` | string | for `openai-compatible` | API root; other formats have a default. |
| `api_key` | string | no | Sent as a bearer token, or as `x-api-key` for `anthropic`. |
| `headers` | object | no | Extra HTTP headers, which may override kon's own. |
| `context_window_tokens` | integer | no | Context limit; `0` means unknown. It must exceed the `compaction` sizes you set, combined. |
| `max_output_tokens` | integer | no | The most the model writes in one reply; `0` means unknown. |
| `inputs` | array of strings | no | [Media](configuration.md#media) the model accepts: `image`, `audio`, `video`, `pdf`. |
| `reasoning` | boolean | no | Reasons; see [Reasoning](configuration.md#reasoning). |
| `reasoning_efforts` | array of strings | no | Effort levels, in Shift+Tab order, without duplicates. |
| `cost` | object | no | US dollars per million tokens: `input`, `output`, and optionally `cache_read` and `cache_write`. |

Names and IDs you choose (`name`, `id`, `catalog_provider`, and effort levels)
may use only letters, digits, `.`, `_`, and `-`.
