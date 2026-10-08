# Getting started

This page takes you from installing kon to your first prompt, and covers
keeping it up to date. Once you are in
a session, [Working with kon](usage.md) covers what you can do there.

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

### Upgrade

`kon upgrade` installs the latest release over the running executable, and
`kon upgrade --check` only reports whether there is one. kon never checks for
updates on its own. Upgrading also refreshes the
[model catalog](configuration.md#model-catalog), where kon's model list and
prices come from.

Builds from source (`go install`, `go run`) report version `dev` and cannot
upgrade themselves; reinstall them the same way. If the executable's
directory is not writable, rerun the installer rather than running kon as
root.

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
`Ctrl+D` on an empty prompt to quit. kon prints a `kon --resume` command on
exit so you can pick the session up later.

`/help` opens this guide inside kon, and you can ask kon about itself: it
reads the guide before answering, so "how do I queue a message?" works as a
prompt.

To set up models by hand instead of with `/login`, see
[Configuration](configuration.md).

## Before you rely on it

kon has no sandbox and no confirmation prompt. Its shell commands run as you,
in your environment, the moment the model asks. Run it only in directories and
on work you would hand to someone else with the same access, and keep your
work in version control.
