# kon

A coding agent for your terminal. Starts in 27 ms. One binary. Every OS.

kon reads, edits, and runs commands in your project until the work is done. One
12 MB executable: no runtime, no daemon, no warm-up.

**[kon.kitsu.red](https://kon.kitsu.red)**

![kon in a terminal](.github/screenshots/kon.png)

## Install

Linux and macOS:

```sh
curl -fsSL https://kon.kitsu.red/install.sh | sh
```

Windows, in PowerShell:

```powershell
iwr -useb https://kon.kitsu.red/install.ps1 | iex
```

With Go:

```sh
go install kon.kitsu.red/cmd/kon@latest
```

## Get started

Run `kon` in any project. Connect a provider with `/login`, pick a model with
`/model`, and start typing. Type `@` to mention a file, and `/` for commands.

kon has no sandbox and does not ask before running commands, so use it on work
you keep in version control. The [user guide](docs/product/index.md) covers
the rest.

## Why kon

- **Four sharp tools.** `read`, `write`, `edit`, and `shell`, after
  [pi](https://pi.dev/). Context goes to your code, not a tool menu.
- **Instant.** 27 ms from launch to first frame. No update check, no catalog
  refresh, and no session scan on launch.
- **Native everywhere.** Linux, macOS, and Windows on x64 and ARM64. No WSL, no
  account, no plugins.
- **No lock-in.** Sessions are plain JSONL you can read, grep, and keep.
  OpenAI, Anthropic, OpenRouter, Ollama, or any OpenAI-compatible endpoint.

## Scripting

`kon run` sends one prompt and streams the reply, so kon fits in pipelines:

```sh
git diff | kon run --stdin review this
```

Add `--format json` for one event per line. See [scripting](docs/product/scripting.md).

## Documentation

- [User guide](docs/product/index.md): install and first session
- [Working with kon](docs/product/usage.md): prompting, sessions, jobs, and subagents
- [Scripting](docs/product/scripting.md): `kon run` and `kon md`
- [Configuration](docs/product/configuration.md): providers, models, and project instructions
- [Reference](docs/product/reference.md): every command, key, tool, and setting
- [Development](docs/development/index.md): building and contributing
- [Architecture](docs/development/architecture.md): how the pieces fit together
- [Session format](docs/development/session-format.md): the on-disk JSONL contract
- [Storage migrations](docs/development/migrations.md): version upgrades and recovery
- [Rendering performance](docs/development/rendering-performance.md): how the TUI stays fast

## License

MIT
