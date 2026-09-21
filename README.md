# kon

`kon` is a small, full-screen coding-agent harness for foxes. It starts directly
in a prompt, streams a configured model, and gives that model four tools:
`read`, `write`, `edit`, and `shell`.

The project favors a small product surface and readable Go over frameworks and
hidden state. Sessions and prompt history are ordinary JSONL files. There is no
database, daemon, account system, or plugin loader.

Provider wire formats are owned in-tree: kon speaks the OpenAI Chat
Completions format directly and keeps full control of message construction,
streaming, and token accounting. A small `provider.Model` interface isolates
each wire format so others can be added later. kon also owns its durable agent
loop, context policy, tools, and terminal UX.

Owned identifiers are Stripe-style typed IDs: sessions use `ses_…` and entries
use `ent_…`. Provider-owned model and tool-call IDs are kept in separate opaque
types and are preserved exactly.

## Install

Download a release archive for Linux, macOS, or Windows, or build from source:

```sh
go install github.com/hizkifw/kon/cmd/kon@latest
```

Development requires Go 1.25 or newer:

```sh
go build ./cmd/kon
go test ./...
```

## First run

Run `kon` from the directory where the agent should work. The first launch
creates a config file and storage directories, opens a new session, and renders
the TUI without contacting a provider.

On Linux, macOS, and other POSIX systems, the default paths are:

```text
~/.config/kon/config.json
~/.local/share/kon/history.jsonl
~/.local/share/kon/sessions/
```

`XDG_CONFIG_HOME` and `XDG_DATA_HOME` override those roots. On Windows, `%APPDATA%`
and `%LOCALAPPDATA%` are used when the XDG variables are unset.

Edit `config.json` before sending the first prompt:

```json
{
  "default_model": "fast",
  "models": [
    {
      "name": "fast",
      "provider": "openai",
      "model": "gpt-5-mini",
      "api_key": "sk-...",
      "context_window_tokens": 128000
    },
    {
      "name": "local",
      "provider": "ollama",
      "model": "qwen3-coder",
      "base_url": "http://localhost:11434",
      "context_window_tokens": 32768
    }
  ],
  "compaction": {
    "reserve_tokens": 16384,
    "keep_recent_tokens": 20000
  },
  "instructions": ""
}
```

Each `name` is a unique alias used by `/model`. Supported providers are
`openai`, `openrouter`, `ollama`, and `openai-compatible`. All of them speak
the OpenAI Chat Completions format; the last option covers other services that
expose it and requires `base_url`. Profiles use their standard endpoint unless
`base_url` overrides it — for `ollama`, a base URL without a path gains `/v1`,
its OpenAI-compatible endpoint. `api_key` may be empty when the provider uses
its conventional environment variable or needs no credential. Optional
`headers` are sent on every provider request. Set `context_window_tokens` to
`0` to disable automatic compaction for that profile.

Configuration is read once at startup. Invalid files are reported and never
rewritten. Since a literal API key may be stored in the file, kon creates it
with owner-only permissions where the platform supports them.

## Controls

| Key | Action |
| --- | --- |
| Enter | Submit the prompt |
| Alt+Enter | Insert a newline |
| Up/Down | Recall prompts while editing a single line |
| Page Up/Page Down | Scroll the transcript |
| Mouse wheel | Scroll the transcript |
| Ctrl+C | Cancel active work, or exit while idle |
| Ctrl+D | Quit when the input is empty |

`/new` closes the current session and starts a new one with the active model.
`/model` lists configured profiles and `/model <name>` switches the active
profile without starting a new session. Model changes are written to the
session log. Unknown slash commands are never sent to a provider.

The status bar shows the working directory and context usage. `~12.4k` means
usage is estimated; `?` means the provider has not supplied enough information.

## Agent behavior and safety

Tool calls run serially in the directory where kon was started:

- `read` returns numbered UTF-8 lines, at most 2,000 lines and 1 MiB.
- `write` atomically creates or replaces a file. It does not create parents.
- `edit` replaces exactly one occurrence and fails on zero or multiple matches.
- `shell` uses `/bin/sh -c` on POSIX and `%COMSPEC% /d /s /c` on Windows. It
  times out after 120 seconds and limits returned output to 64 KiB.

There is no sandbox or confirmation prompt. Relative and absolute paths are
accepted, and shell commands inherit the user's environment. Run kon with the
same care as any other coding agent.

When context exceeds `context_window_tokens - reserve_tokens`, kon summarizes
older complete turns and keeps approximately `keep_recent_tokens`. Summaries
are stored as new session entries; original entries are never deleted. A failed
compaction stops the run instead of discarding context. Provider-reported
context-overflow errors cause one compaction and one retry.

See [Architecture](docs/architecture.md), [Development](docs/development.md),
and [Session format](docs/session-format.md) for implementation contracts.

## Command line

```text
kon
kon --help
kon --version
```

Every invocation starts a new session. Resume, branch navigation, rewind,
manual compaction, Markdown rendering, and plugins are outside v0.1.0.

## Development

```sh
make check       # format check, vet, and tests
make build       # bin/kon
make release VERSION=v0.1.0
```

Windows developers can run the underlying Go commands directly; `make` is not
required. Release builds are pure Go (`CGO_ENABLED=0`) and target Linux, macOS,
and Windows on amd64 and arm64.

## License

MIT
