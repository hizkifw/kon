# kon user guide

kon is a coding agent for your terminal. Run it in a project directory, and it
reads files, edits them, and runs commands there until the work is done.

| Page | Covers |
| --- | --- |
| [Getting started](getting-started.md) | Installing kon, connecting a provider, and what to know before relying on it |
| [Working with kon](usage.md) | Prompting, steering, sessions, background jobs, subagents, and copying |
| [Configuration](configuration.md) | Providers, models, context limits, reasoning, cost, and project instructions |
| [Scripting](scripting.md) | `kon run` in pipelines, JSON events, and `kon md` |
| [Editor integration](acp.md) | `kon acp` for editors that speak the Agent Client Protocol |
| [Reference](reference.md) | Every command, flag, key, tool, environment variable, and config field |

You can also just ask: kon reads this guide before it answers a question about
itself, so "how do I add a second OpenRouter account?" works as a prompt.

`/help` opens this guide inside kon. Run `kon docs` to get these pages as local
Markdown files. It prints only the directory they were written to, so
`ls "$(kon docs)"` lists them.
