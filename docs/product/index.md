# kon user guide

kon is a terminal coding agent. Run it from the project directory where you
want it to work. It reads files, edits them, and runs commands in that directory.

## Install

Linux and macOS:

```sh
curl -fsSL https://raw.githubusercontent.com/hizkifw/kon/main/scripts/install.sh | sh
```

Windows PowerShell:

```powershell
iwr -useb https://raw.githubusercontent.com/hizkifw/kon/main/scripts/install.ps1 | iex
```

Or install with Go:

```sh
go install github.com/hizkifw/kon/cmd/kon@latest
```

## First session

Run `kon` from your project directory. On first launch it creates a config file
and opens the prompt. Nothing is sent to a model until you submit a prompt.
Run `/login openai` to enter an API key, then `/model` to choose a model. You
can also add an explicit model to the config file and restart kon. For example:

```json
{
  "default_model": "fast",
  "models": [
    {
      "name": "fast",
      "type": "openai",
      "model": "gpt-5-mini",
      "api_key": "sk-...",
      "context_window_tokens": 128000
    }
  ]
}
```

The config is at `~/.config/kon/config.json` on Linux and macOS, or
`%APPDATA%\kon\config.json` on Windows. See [Configuration](configuration.md)
for providers, model limits, vision, and project instructions.

Type `/` to see in-app commands. See [Using kon](usage.md) for keys, sessions,
tools, and compaction. Press `Ctrl+D` to exit when the input is empty.

`kon models` lists bundled or cached model IDs offline. `kon models --refresh`
fetches the latest catalog from models.dev only when you request it. See
[Configuration](configuration.md#model-catalog) for details.

## Local copy

Run `kon docs` to print the directory containing this guide and its companion
pages. kon writes them under its data directory on demand, so they are available
as ordinary Markdown files for offline reading or searching. The path includes
a content version. An upgrade that adds, renames, or removes pages gets a new
directory; the path printed by the new binary contains only its bundled pages.
Older extracted versions remain available separately.

`kon docs` checks the current directory against the bundled files each time;
if it was edited, it restores the bundled copy. Keep personal notes elsewhere.
