# Configuration

kon keeps its settings in one JSON file. `/login`, `/model`, and Shift+Tab
write it for you, so most people never edit it; this page covers what they
write and what you can set by hand. Every field is listed under
[Config fields](reference.md#config-fields).

The file is `~/.config/kon/config.json` on Linux and macOS and
`%APPDATA%\kon\config.json` on Windows, or `$XDG_CONFIG_HOME/kon/config.json`
when that variable is set. kon creates it on first launch, readable only by
you, because it holds API keys. Unknown fields are rejected, so a typo fails
loudly instead of being ignored. Changes by hand take effect the next time kon
starts.

## Connect a provider

`/login <provider>` saves a connection to a provider. It asks for an API key,
checks it, and fetches the provider's model list for `/model`. Autocomplete offers the providers
from the [model catalog](#model-catalog) that kon can talk to. A few need more
than a key:

| Login | Asks for |
| --- | --- |
| `/login ollama` | The server URL, `http://localhost:11434` by default. No key. |
| `/login openai-compatible` | The server URL, and an optional key. |
| `/login azure` | Your resource's OpenAI v1 endpoint, and a key. |

Login is the only time kon contacts a provider before you send a prompt. The
model list is cached for offline use. A compatible server that has no model
list can still be saved, marked unverified. A working model list does not
prove a model can handle kon's tool calls.

A connection is saved under `providers`. `/login zai`, then choosing
`zai/glm-5.3-flash` with `/model`, writes:

```json
{
  "default_model": "zai/glm-5.3-flash",
  "providers": [
    {"id": "zai", "type": "openai-compatible", "catalog_provider": "zai", "base_url": "https://api.z.ai/api/paas/v4", "api_key": "..."}
  ],
  "models": []
}
```

The catalog tells kon which [wire format](#wire-formats) a provider speaks
and where its API is. `/login openai` saves `"type": "openai-responses"`
instead, and `/login anthropic` saves `"type": "anthropic"`.

Logging in again to the same provider replaces its connection. For a second
connection to the same provider, such as another account, add one by hand
with its own `id`, and set `catalog_provider` so kon still finds its models in
the catalog:

```json
{"id": "fireworks-2", "catalog_provider": "fireworks-ai", "type": "openai-compatible", "base_url": "https://api.fireworks.ai/inference/v1", "api_key": "..."}
```

## Choose a model

`/model` lists the models of every connection, named
`<connection-id>/<model-id>`, such as `zai/glm-5.3-flash` or
`openrouter/qwen/qwen3.8-27b`. The list marks which models
the provider reported and which are only known from the catalog. Choosing one
saves its name as `default_model`, which new sessions use.

A model chosen this way takes its context window, output limit, media
inputs, reasoning levels, and prices from the catalog. For a model the
catalog does not know, those are unknown; define it by hand to set them.

Azure deployments have names you choose, so use `azure/<deployment-name>`.

## Define a model by hand

Add a profile under `models` when a server has no model list, when the
catalog lacks the model, or when you want your own limits or prices. A profile
is standalone: it carries its own connection and never reads from
`providers`.

```json
{
  "default_model": "flash",
  "models": [
    {
      "name": "flash",
      "type": "openai-compatible",
      "base_url": "https://api.deepseek.com",
      "model": "deepseek-v4.1-flash",
      "api_key": "...",
      "context_window_tokens": 1000000,
      "max_output_tokens": 393216,
      "reasoning": true
    },
    {
      "name": "local",
      "type": "ollama",
      "model": "qwen3.8:27b",
      "context_window_tokens": 262144
    }
  ]
}
```

`name` is what `/model` and `default_model` use, and `model` is the provider's
own model ID, sent exactly as written. Names you choose, such as `name` and
`id`, may use only letters, digits, `.`, `_`, and `-`.

### Wire formats

`type` says which API kon speaks to the server. It describes the protocol,
not the company: many services speak `openai-compatible`.

| `type` | API | Default `base_url` |
| --- | --- | --- |
| `openai-compatible` | OpenAI Chat Completions. The default when `type` is omitted. | none; `base_url` is required |
| `openai` | Chat Completions | `https://api.openai.com/v1` |
| `openrouter` | Chat Completions | `https://openrouter.ai/api/v1` |
| `ollama` | Chat Completions | `http://localhost:11434` (kon adds `/v1`) |
| `openai-responses` | OpenAI Responses | `https://api.openai.com/v1` |
| `anthropic` | Anthropic Messages, also spoken by services such as MiniMax | `https://api.anthropic.com/v1` |

The Chat Completions formats differ only in their default server and in how
they send reasoning. `openai-responses` is what `/login openai` uses, because
it carries a reasoning model's reasoning from one turn to the next. kon asks
the server not to store the conversation, so the session file stays the only
copy.

The API key is sent as a bearer token, or as `x-api-key` for `anthropic`.
`headers` adds or overrides HTTP headers; an `anthropic-beta` header is
combined with the betas kon sends itself.

## Context window and compaction

Set `context_window_tokens` to the model's context limit and
`max_output_tokens` to its output limit. kon uses them to show context usage
and to [compact](usage.md#long-conversations) before the window fills.

kon compacts once the context reaches about 80% of the window, sooner on
smaller windows so there is room left for a reply and the summary, and keeps
roughly the most recent 15% of the window verbatim. On a 1M-token window, it
compacts at 800K tokens and keeps about 155K.

- With `context_window_tokens` unset or `0`, kon cannot compact ahead of time.
  It still compacts when the provider reports that the context overflowed, and
  `/compact` still works.
- With `max_output_tokens` unset, the summary is held to 8K tokens, a size
  every model accepts.

`compaction.reserve_tokens` (how much room to keep free below the window) and
`compaction.keep_recent_tokens` (how much recent context to keep verbatim)
override the sizes kon works out, for every model.

## Reasoning

For a model that reasons, set `"reasoning": true`, and list the effort levels
it accepts in `reasoning_efforts`, in the order Shift+Tab should cycle through
them:

```json
{"name": "glm", "type": "openai-compatible", "base_url": "https://api.z.ai/api/paas/v4", "model": "glm-5.3-flash", "api_key": "...", "reasoning": true, "reasoning_efforts": ["low", "high", "max"]}
```

The cycle ends on `default`, which sends no effort and leaves the choice to the
provider. Without `reasoning_efforts`, kon never sends an effort. For a model
that can only turn reasoning off, use `["none"]`; the header shows it as
`no thinking`. Models from `/model` take both settings from the catalog.

The chosen level is saved as `reasoning_effort` and restored at the next
launch. Switching models resets it to `default`.

What the two fields do depends on the wire format:

| Wire format | Effort is sent as | `"reasoning": true` |
| --- | --- | --- |
| Chat Completions formats | `reasoning_effort`, or `reasoning.effort` for `openrouter` | For DeepSeek's own API (a `base_url` on `deepseek.com`), sends the empty `reasoning_content` it requires |
| `openai-responses` | `reasoning.effort`, only with `"reasoning": true` | Required for any reasoning: requests it, and carries it between turns |
| `anthropic` | `output_config.effort` | Turns on extended thinking |

For example, OpenAI's and Anthropic's own APIs both need `"reasoning": true`
for their models to reason at all:

```json
{"name": "sol", "type": "openai-responses", "model": "gpt-6.1-sol", "api_key": "...", "reasoning": true, "reasoning_efforts": ["low", "medium", "high", "xhigh", "max"]}
{"name": "sonnet", "type": "anthropic", "model": "claude-sonnet-5-5", "api_key": "...", "reasoning": true, "reasoning_efforts": ["low", "medium", "high", "xhigh", "max"]}
```

kon sends a model's reasoning back with its earlier replies, so a model that
thinks across tool calls keeps its chain of thought. After a model switch,
reasoning from the previous model is left out where the provider cannot use it.

## Media

Set `inputs` to the media a model accepts beyond text: any of `"image"`,
`"audio"`, `"video"`, and `"pdf"`. The `read` tool then passes such files to
the model whole:

| Input | Formats | Largest |
| --- | --- | --- |
| `image` | PNG, JPEG, GIF, WebP | 5 MiB |
| `audio` | WAV, MP3 | 20 MiB |
| `video` | MP4, MOV, WebM | 20 MiB |
| `pdf` | PDF | 20 MiB |

For a modality the model does not accept, reading such a file returns a short
text notice. Models from `/model` take their inputs from the catalog.

```json
{"name": "gemini", "type": "openrouter", "model": "google/gemini-3.1-flash-lite", "api_key": "...", "inputs": ["image", "audio", "video", "pdf"]}
```

Not every wire format can carry every modality. `anthropic` and
`openai-responses` carry images and PDFs only. The chat formats carry all four;
video goes as `video_url`, which OpenRouter, vLLM, and DashScope accept but
OpenAI's own API does not.

After switching to a model that does not accept some media, that media earlier
in the session is sent as a text placeholder, so the conversation can continue.

## Cost

The status bar's [cost](usage.md#context-and-cost) needs each model's prices.
Models from `/model` use the catalog's. For a profile, set `cost` in US
dollars per million tokens:

```json
{"name": "qwen", "type": "openrouter", "model": "qwen/qwen3.8-27b", "api_key": "...", "cost": {"input": 0.42, "output": 3, "cache_read": 0.085}}
```

Input read from or written to a prompt cache is priced at `cache_read` and
`cache_write`, or at `input` when those are left out. A profile without `cost`
is not priced, even if the catalog knows the model.

## Project instructions

kon adds instructions from `AGENTS.md` files to every new session:

- **Your own**, from `AGENTS.md` next to `config.json`
  (`~/.config/kon/AGENTS.md` on Linux and macOS), for every project.
- **The project's**, from the `AGENTS.md` in the working directory and in
  each directory above it, up to the filesystem root. Outer directories
  come first, so the most specific instructions come last.
- **Given on the command line** to `kon`, `kon run`, or `kon acp`:
  `--instructions <text>` adds text, and `--instructions-file <file>` adds a
  file. Repeat either to add several; they come last, in the order given:

  ```sh
  kon run --instructions-file review.md --instructions "Be terse." review the diff
  ```

In each directory, `AGENTS.override.md` takes the place of `AGENTS.md`, and
`CLAUDE.md` is used when there is no `AGENTS.md`. Empty files, and directories
whose names start with `.`, are skipped; a file given with
`--instructions-file` must not be empty.

Set `"context_files": false` to skip the project's files; your own
`AGENTS.md` and instructions given on the command line still load.

Instructions are read when a session starts and kept with it, so edits apply
to new sessions (`/new` or a fresh launch). A resumed session keeps the
instructions it started with.

Subagents do not inherit instructions given on the command line, nor
`--system-prompt-override`: each starts from your `AGENTS.md` files and kon's
built-in prompt.

## System prompt

`--system-prompt-override <file>` replaces kon's built-in instructions with the
contents of a file, for `kon`, `kon run`, and `kon acp`:

```sh
kon run --system-prompt-override reviewer.md review the staged diff
```

Only the built-in part is replaced. Your `AGENTS.md`, the project's, and the
working directory are still added after it. The file is read once, at launch.
The prompt is fixed when a session starts: the override applies to new
sessions, including `/new`, and a resumed session keeps the prompt it started
with.

## Web search

Web search is off until you choose a provider. Set `web_search.provider`, and
give that provider what it needs under `web_search.providers`:

```json
{
  "web_search": {
    "provider": "brave",
    "providers": {
      "brave": {"api_key": "..."},
      "searxng": {"base_url": "http://localhost:8888"}
    }
  }
}
```

The agent then searches by running `kon tool websearch <query>`; new sessions
are told about it, and a session started earlier is not. `providers` may hold
more than the one in use, so switching is a one-word edit. To turn search off
again, set `provider` to `""` or remove it.

| Provider | Needs | Notes |
| --- | --- | --- |
| `brave` | `api_key` | The [Brave Search API](https://brave.com/search/api/). |
| `exa` | `api_key` | [Exa](https://exa.ai). Snippets are the passages that match the query. |
| `perplexity` | `api_key` | The [Perplexity Search API](https://docs.perplexity.ai): ranked results, not a written answer. |
| `tavily` | `api_key` | [Tavily](https://tavily.com), at its basic search depth. |
| `searxng` | `base_url` | Your own [SearXNG](https://docs.searxng.org) instance, which must list `json` under `search.formats` in its `settings.yml`. An `api_key`, if set, is sent as a bearer token for an instance behind a proxy. |
| `duckduckgo` | nothing | Reads DuckDuckGo's HTML results page, since it has no search API. It needs no account, but DuckDuckGo may refuse automated requests. |

`base_url` is optional for the hosted providers, and points kon at a proxy
instead of the provider's own address. A selected provider that lacks its key
or URL, or a provider name kon does not know, is an error when kon starts.

## Model catalog

kon ships a copy of the [models.dev](https://models.dev) catalog, which it uses
for `/login` autocomplete and for the limits, media inputs, reasoning levels,
and prices of models chosen with `/model`. The catalog is reference data: a
model listed there is not necessarily one your account can use.

`kon models` lists the catalog's model IDs, offline.
`kon models --refresh` downloads the latest catalog and caches it; so does
`kon upgrade`. kon never downloads it otherwise.
