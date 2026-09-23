# Configuration

kon creates `config.json` on first launch. The default location is
`~/.config/kon/config.json` on Linux and macOS, or
`%APPDATA%\kon\config.json` on Windows. The file is created with owner-only
permissions because it contains literal API keys.

Each model profile needs a distinct `name`, a `provider`, and the provider's
model ID. `default_model` names the profile used for new sessions. The current
providers are `openai`, `openrouter`, `ollama`, and `openai-compatible`.
The last accepts any endpoint speaking OpenAI Chat Completions and requires
`base_url`. For example:

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
  ]
}
```

Set `context_window_tokens` to the model's actual context limit. kon uses it
to show context usage and to compact before the window fills. A value of `0`
means unknown and disables proactive compaction; in that case kon can still
summarize after a provider overflow error. The numbers above are examples.

Set `"vision": true` for a model that accepts image input. The `read` tool
then attaches PNG, JPEG, GIF, and WebP files up to 5 MB as image content.
Without this flag, reading an image returns a text notice.

## Model catalog

Run `kon models` to list model IDs from the bundled models.dev catalog or a
newer local cache. It works offline and does not change your configuration.
Run `kon models --refresh` when you choose to fetch the latest catalog from
models.dev; the validated response is cached for later offline use. kon never
refreshes the catalog automatically.

The catalog is reference data, not a list of providers kon can necessarily
call. Choose a supported provider and add the model ID to `config.json` yourself.

## Project instructions

kon walks from the working directory to the filesystem root and loads the
first `AGENTS.md` it finds in each directory. Inherited instructions come
first and more specific ones last. `CLAUDE.md` is accepted as a compatibility
alias, and `AGENTS.override.md` replaces the plain file in its directory.
Empty files and directories whose name begins with `.` are ignored.

Set `"context_files": false` in the config to turn off discovery. The prompt
is recorded when a session is created. Changes to instruction files therefore
apply to new sessions (`/new` or a fresh launch); resumed sessions keep their
original prompt.
