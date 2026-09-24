# Configuration

kon creates `config.json` on first launch. The default location is
`~/.config/kon/config.json` on Linux and macOS, or
`%APPDATA%\kon\config.json` on Windows. The file is created with owner-only
permissions because it contains literal API keys.

The generated file configures no model:

```json
{
  "default_model": "",
  "models": [],
  "compaction": {"reserve_tokens": 16384, "keep_recent_tokens": 20000},
  "instructions": ""
}
```

kon still opens the prompt, but asks you to run `/login <provider>` and pick
a model with `/model`, or to add a profile under `models` as described below.

Each model profile is standalone: it needs a distinct `name`, the provider's
model ID in `model`, and its own connection fields. `default_model` names the
profile used for new sessions. `type` is the wire format kon speaks: `openai`,
`openrouter`, `ollama`, or `openai-compatible`, which is the default when
`type` is omitted. `openai-compatible` accepts any endpoint speaking OpenAI
Chat Completions and requires `base_url`. A wire format says how kon talks to
a server, not which service it is: many services share `openai-compatible`,
and all four formats are dialects of Chat Completions that differ in their
default endpoint and how they encode reasoning. For example:

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
    },
    {
      "name": "local",
      "type": "ollama",
      "model": "qwen3-coder",
      "base_url": "http://localhost:11434",
      "context_window_tokens": 32768
    }
  ]
}
```

To reuse one connection across many models, add it to `providers` instead.
Explicit profiles never read from `providers`; models from a connection are
selected by derived names (below). `/login <provider>` accepts a supported models.dev provider ID
(for example `openai`, `openrouter`, `fireworks-ai`, or `deepinfra`), creates
or replaces a connection with that ID, and asks only for its API key when
the provider has a fixed endpoint. The `type` field is kon's wire format,
not the provider's identity:

```json
{
  "default_model": "openai/gpt-5-mini",
  "providers": [
    {"id": "openai", "type": "openai", "api_key": "sk-..."}
  ],
  "models": []
}
```

For example, `/login fireworks-ai` saves a `fireworks-ai` connection with
`type: "openai-compatible"` and the API URL from models.dev. `/login`
autocomplete includes catalog providers whose `npm` package maps to a wire
format kon implements and whose API URL can be used directly. A few providers
with missing endpoints or special login behavior use kon-maintained
overrides. kon does not install or execute AI SDK packages. Catalog entries
with templated URLs or non-HTTPS remote endpoints are not offered for key-only
login. Azure is an exception to the key-only flow:
`/login azure` also asks for your resource-specific OpenAI v1 endpoint.
Azure deployments have user-defined names, so its catalog models are not
automatically offered as deployable models; add an explicit model or use a
derived `azure/<deployment-name>` ID.

For a second connection to the same provider, edit the config manually and
give it a distinct `id`, plus `catalog_provider` to retain the original
models.dev identity. For example:

```json
{"id":"fireworks-2","catalog_provider":"fireworks-ai","type":"openai-compatible","base_url":"https://api.fireworks.ai/inference/v1","api_key":"..."}
```

Its derived model names start with `fireworks-2/`.

`/model` combines explicit profiles with model IDs discovered during login
and metadata from the bundled models.dev catalog. Derived names have the form
`<provider-id>/<model-id>`; provider model IDs may themselves contain `/`.
Selecting one saves its name as `default_model` without copying it into
`models`. Explicit profiles take precedence over a duplicate derived entry.
The picker shows `<connection id> · <model display name>` when catalog metadata
is available, with the provider's model ID in the muted detail. Selecting a
row still saves the stable qualified name. The no-argument `/model` list also
marks configured, provider-listed, and catalog-only entries so reference data
is not mistaken for confirmed access.

You can still configure a model manually when a server has no listing API or
the catalog lacks its ID. A derived model absent from the catalog has an
unknown context window; add an explicit profile when you need a known limit
for proactive compaction.

During setup, login is the only time kon contacts the chosen provider to check
a connection and discover models; it does not poll on startup. OpenAI and
Ollama use their model-list endpoints. OpenRouter checks the key before
fetching its model list.
An OpenAI-compatible server without `/models` can be saved, but kon labels it
unverified. A successful list request does not guarantee that a model supports
kon's chat and tool calls. Discovery results are cached under kon's data
directory for later offline use.

Set `context_window_tokens` to the model's actual context limit. kon uses it
to show context usage and to compact before the window fills. A value of `0`
means unknown and disables proactive compaction; in that case kon can still
summarize after a provider overflow error. The numbers above are examples.

Set `"vision": true` for a model that accepts image input. The `read` tool
then attaches PNG, JPEG, GIF, and WebP files up to 5 MB as image content.
Without this flag, reading an image returns a text notice. Derived models use
catalog image-input metadata when available. After `/model` switches to a
model without vision, images already in the session are sent as a short text
placeholder, so the conversation continues instead of failing.

Set `reasoning_efforts` to the effort levels a reasoning model accepts, in the
order Shift+Tab should cycle through them:

```json
{"name": "deep", "model": "o4-mini", "base_url": "https://api.openai.com/v1", "api_key": "...", "reasoning_efforts": ["low", "medium", "high"]}
```

The cycle ends on `default`, which sends no effort and leaves the choice to
the provider. Without `reasoning_efforts`, kon never sends the parameter. For
a model that can only switch reasoning off, use `["none"]`; the header shows
that level as `no thinking`. Derived models use the catalog's effort levels,
and a catalog model that only has an on/off toggle gets the same `none` level.
The selected level is sent as `reasoning_effort`, or as `reasoning.effort` for
OpenRouter.

A model's reasoning is sent back with its earlier replies, in the field the
server streamed it in, so a model that thinks across tool calls keeps its
chain of thought. Reasoning written by a different model is left out after a
model switch. Set `"reasoning": true` for a model that produces reasoning;
DeepSeek's API then receives the empty `reasoning_content` it requires on
replies without reasoning. Derived models use the catalog's reasoning metadata.

Like `default_model`, the selected level is saved as the top-level
`reasoning_effort` and restored on the next launch. Switching models resets it
to `default`. A saved level the model does not list is ignored, so the model
starts on `default` instead of failing.

## Schema reference

Unknown fields are rejected, so a typo fails loudly instead of being ignored.
Names and IDs that kon owns (`name`, `id`, `catalog_provider`, and each
reasoning effort) use only letters, digits, `.`, `_`, and `-`.

Top level:

| Field | Type | Default | Meaning |
| --- | --- | --- | --- |
| `default_model` | string | `""` | Model used for new sessions: an explicit profile `name`, or a derived `<provider-id>/<model-id>`. Empty means no model is configured yet. `/model` rewrites it. |
| `reasoning_effort` | string | omitted | Effort last selected with Shift+Tab. Ignored when the model does not list it. |
| `providers` | array | omitted | Reusable connections; see below. |
| `models` | array | `[]` | Explicit model profiles; see below. |
| `compaction.reserve_tokens` | integer | `16384` | Tokens kept free for the next reply. Must be positive. |
| `compaction.keep_recent_tokens` | integer | `20000` | Recent context kept verbatim when older history is summarized. Must be positive. |
| `instructions` | string | `""` | Text appended to the system prompt of new sessions. |
| `context_files` | boolean | `true` | Load `AGENTS.md` and `CLAUDE.md` files; see [Project instructions](#project-instructions). |

Each entry in `providers`:

| Field | Type | Required | Meaning |
| --- | --- | --- | --- |
| `id` | string | yes | Unique connection ID and the prefix of its derived model names. |
| `type` | string | yes | Wire format: `openai`, `openrouter`, `ollama`, or `openai-compatible`. |
| `catalog_provider` | string | no | models.dev provider key, when `id` differs from it. |
| `base_url` | string | for `openai-compatible` | API root. Other types default to their public endpoint. |
| `api_key` | string | no | Sent as a bearer token. |
| `headers` | object | no | Extra HTTP headers, which may override kon's own. |

Each entry in `models`:

| Field | Type | Required | Meaning |
| --- | --- | --- | --- |
| `name` | string | yes | Unique profile name used by `default_model` and `/model`. |
| `type` | string | no | Wire format, as for providers. Defaults to `openai-compatible`. |
| `model` | string | to run | The provider's model ID, sent as written. |
| `base_url` | string | for `openai-compatible` | API root. `openai` defaults to `https://api.openai.com/v1`, `openrouter` to `https://openrouter.ai/api/v1`, and `ollama` to `http://localhost:11434/v1`. |
| `api_key` | string | no | Sent as a bearer token. |
| `headers` | object | no | Extra HTTP headers, which may override kon's own. |
| `context_window_tokens` | integer | no | Context limit. `0` means unknown; otherwise it must exceed both compaction budgets combined. |
| `vision` | boolean | no | Accepts image input. |
| `reasoning` | boolean | no | Produces reasoning. |
| `reasoning_efforts` | array of strings | no | Effort levels in Shift+Tab order, without duplicates. |

## Model catalog

Run `kon models` to list model IDs from the bundled models.dev catalog or a
newer local cache. It works offline and does not change your configuration.
Run `kon models --refresh` when you choose to fetch the latest catalog from
models.dev; the validated response is cached for later offline use. kon never
refreshes the catalog automatically.

The catalog is reference data, not a list of providers kon can necessarily
call. `/model` only derives names for supported, configured connections.

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
