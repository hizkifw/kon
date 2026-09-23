# kon

`kon` is a terminal coding agent that lives in your project directory. Point it
at a task and it reads, edits, and runs commands until the work is done.

It is built on a few beliefs:

- **Few tools, not many.** kon follows [pi](https://pi.dev/)'s four-tool
  philosophy: the model gets exactly `read`, `write`, `edit`, and `shell`.
  Sharper tools mean it spends its context on your code, not on a tool menu.
- **Instant and snappy.** One static binary that starts immediately and streams
  its answer as it arrives — no daemon, no warm-up.
- **Reliable anywhere.** Linux, macOS, and Windows, with no account and no
  plugins. Run it wherever you are.
- **No vendor lock-in.** Sessions are plain JSONL files you can read, grep, and
  keep; any OpenAI-compatible endpoint works. Your prompts never disappear
  behind a database.

![kon in a terminal](.github/screenshots/kon.png)

## Install

One line, Linux and macOS:

```sh
curl -fsSL https://raw.githubusercontent.com/hizkifw/kon/main/scripts/install.sh | sh
```

Windows, in PowerShell:

```powershell
iwr -useb https://raw.githubusercontent.com/hizkifw/kon/main/scripts/install.ps1 | iex
```

Install with Go:

```sh
go install github.com/hizkifw/kon/cmd/kon@latest
```

## Getting started

Run `kon` from the directory where you want it to work. On first launch it
creates a config file and opens the prompt. Use `/login openai` to connect a
provider, then `/model` to choose a model. Explicit profiles in `config.json`
remain supported. See the [user guide](docs/product/index.md) for setup and
[configuration](docs/product/configuration.md).

When a release changes the on-disk format, kon runs its registered storage
migrations before opening the prompt. It waits for other kon instances to exit
before changing shared files. Normal launches check a small version marker and
do not scan sessions.

Run `kon docs` to extract the bundled product guide on demand and print its
local directory. The files are ordinary Markdown, available offline. Each
version gets its own directory, so renamed or removed pages from an older
release do not appear in the path returned by the new release.

`kon models` lists bundled or cached model IDs offline. Run
`kon models --refresh` to explicitly fetch the latest catalog from models.dev;
kon never refreshes it on startup.

## Documentation

- [Using kon](docs/product/usage.md) — commands, keys, sessions, and tools
- [Development](docs/development/index.md) — building and contributing
- [Architecture](docs/development/architecture.md) — how the pieces fit together
- [Session format](docs/development/session-format.md) — the on-disk JSONL contract
- [Storage migrations](docs/development/migrations.md) — version upgrades and recovery
- [Rendering performance](docs/development/rendering-performance.md) — how the TUI stays fast

## License

MIT
