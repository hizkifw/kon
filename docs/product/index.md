# kon user guide

kon is a coding agent for your terminal. Run it in a project directory, and it
reads files, edits them, and runs commands there until the work is done.

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

`kon upgrade` installs later releases; see [Upgrading](usage.md#upgrading).

## First session

1. Run `kon` in your project directory. The first launch creates an empty
   config file and opens the prompt. Nothing is sent anywhere until you log
   in or submit a prompt.
2. Connect a provider with `/login <provider>`, for example
   `/login openrouter` or `/login deepseek`. kon asks for an API key, checks
   it, and fetches the provider's model list. `/login ollama` connects a local Ollama instead.
3. Pick a model with `/model`. Your choice is saved as the default.
4. Type what you want done and press Enter.

Type `/` to see the other commands, `@` to mention a project file, and press
`Ctrl+D` on an empty prompt to quit. kon prints a `kon --resume` command on exit
so you can pick the session up later.

To set up models by hand instead of with `/login`, see
[Configuration](configuration.md).

## Before you rely on it

kon has no sandbox and no confirmation prompt. Its shell commands run as you,
in your environment, the moment the model asks. Run it only in directories and
on work you would hand to someone else with the same access, and keep your
work in version control.

## The rest of this guide

| Page | Covers |
| --- | --- |
| [Working with kon](usage.md) | Prompting, steering, sessions, background jobs, subagents, and copying |
| [Scripting](scripting.md) | `kon run` in pipelines, JSON events, and `kon md` |
| [Editor integration](acp.md) | `kon acp` for editors that speak the Agent Client Protocol |
| [Configuration](configuration.md) | Providers, models, context limits, reasoning, cost, and project instructions |
| [Reference](reference.md) | Every command, flag, key, tool, environment variable, and config field |

Run `kon docs` to get these pages as local Markdown files. It prints the
directory they were written to.
