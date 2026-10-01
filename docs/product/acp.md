# Editor integration (ACP)

`kon acp` runs kon as an agent for editors and other programs that speak the
[Agent Client Protocol](https://agentclientprotocol.com), version 1. The
client starts `kon acp` as a subprocess and exchanges newline-delimited
JSON-RPC 2.0 messages with it on stdin and stdout. kon writes nothing else to
stdout; diagnostics go to stderr.

Configure the client to run `kon acp` with no arguments. Log in and pick a
default model in the full-screen UI first: kon offers no ACP authentication
methods, and a session with no configured model fails each prompt with an
error that names the config file.

Tools run without confirmation, as they do everywhere else in kon, so kon
never sends `session/request_permission`. It also never calls the client's
`fs/*` or `terminal/*` methods: its tools read, write, and run commands
themselves, in the session's working directory.

## Supported methods

| Method | Behavior |
| --- | --- |
| `initialize` | Answers protocol version `1` whatever the client asks for. See [Capabilities](#capabilities). |
| `authenticate` | Accepted and does nothing; no methods are offered. |
| `session/new` | Starts a kon session in `cwd`. The session ID is kon's own `ses_…` ID. |
| `session/load` | Opens a saved session of `cwd` and [replays](#replay) it before answering. |
| `session/resume` | Opens a saved session of `cwd` without replaying it. |
| `session/list` | Lists saved sessions of `cwd`, newest first. Subagent sessions are left out. |
| `session/close` | Cancels the session's turn, stops its background jobs, and closes it. |
| `session/prompt` | Runs one turn. See [Prompts](#prompts). |
| `session/cancel` | Cancels the running turn and every prompt waiting behind it. |
| `session/set_config_option` | Sets the model or reasoning effort. See [Config options](#config-options). |

Every other method gets "Method not found", including `session/set_mode`,
`session/delete`, and `logout`, which kon does not advertise.

`cwd` must be an absolute path. A `session/list` without `cwd` lists the
directory `kon acp` was started in. Sessions belong to the directory they were
started in, so loading or resuming one needs that same `cwd`.

kon has no MCP client, so `mcpServers` is accepted and ignored, and
`mcpCapabilities` advertises no transports.

A session that another kon process has open cannot be loaded or resumed, and
the request fails. Loading a session that is already open on this connection
replays it again; resuming one returns at once.

A session that never receives a prompt is discarded when it closes, just as an
empty session is in the full-screen UI. Its ID is not kept.

## Capabilities

`initialize` answers with:

```json
{
  "protocolVersion": 1,
  "agentInfo": {"name": "kon", "title": "kon", "version": "v0.4.0"},
  "agentCapabilities": {
    "loadSession": true,
    "promptCapabilities": {"image": true, "audio": true, "embeddedContext": true},
    "mcpCapabilities": {"http": false, "sse": false},
    "sessionCapabilities": {"list": {}, "resume": {}, "close": {}},
    "_meta": {"kon.kitsu.red": {"steer": true, "jobs": true, "agentTurns": true}}
  },
  "authMethods": []
}
```

The `kon.kitsu.red` entry lists the [extensions](#extensions) kon supports.

## Prompts

A prompt's content blocks become one user message:

| Block | Becomes |
| --- | --- |
| `text` | Its text. |
| `resource_link` | A mention of the file, `@path`, which the model reads with its tools. A path inside `cwd` is written relative to it. |
| `resource` with text | A mention in place, with the text attached after the prompt in a fenced block headed by the path. |
| `resource` with a blob, `image`, `audio` | An attachment, sent with the prompt as the read tool sends a file. |

Text and mentions are joined in order with nothing between them, so a client
that splits a sentence around a mention gets the sentence back. A model that
does not accept an attachment's kind is sent a short placeholder instead, as
for any attachment.

A prompt whose text is exactly `/compact` compacts the session instead of
being sent to the model, as in the full-screen UI. kon lists it in an
`available_commands_update` once the session is set up.

### Turns and the queue

A session runs one turn at a time. A `session/prompt` that arrives while a
turn is running waits for it to finish and then runs, so a client may send a
follow-up without waiting. `session/cancel` stops the running turn and answers
every waiting prompt with `cancelled`. Cancelling a turn that is already
cancelling also kills a shell command that ignored the first cancel.

A turn ends with `end_turn`, or `cancelled` when cancelled. A turn that fails,
such as on a provider error, answers with a JSON-RPC error whose message says
why. What the turn wrote before failing is kept in the session.

### Session updates

| kon event | `session/update` |
| --- | --- |
| Assistant text | `agent_message_chunk` |
| Reasoning | `agent_thought_chunk` |
| Steering or a notice delivered mid-turn | `user_message_chunk` |
| Tool call started | `tool_call`, `in_progress` |
| Shell command output so far | `tool_call_update` with the output's tail as text |
| Tool call finished | `tool_call_update`, `completed` or `failed` |
| Provider usage reported | `usage_update` |

A tool call's `title` is the tool and its summary as the transcript shows it,
such as `read internal/ui/view.go from 100`. Its `kind` is `read` for read,
`edit` for write and edit, and `execute` for shell. `locations` names the file
a read, write, or edit touches. An edit's content is a `diff` of the replaced
text, and a write's a `diff` with no old text. Other finished calls carry the
output the model sees as text. `rawInput` is the model's arguments and
`rawOutput` the tool's details, when it has any.

`usage_update` reports the context size the provider last reported, the
model's context window, and the session's cost so far in USD, its subagents
included. It is sent only when the model's context window is known.

### Replay

`session/load` replays the conversation as the context tree's active path
holds it: user messages as `user_message_chunk`, reasoning as
`agent_thought_chunk`, assistant text as `agent_message_chunk`, and each tool
call as a `tool_call` followed by a `tool_call_update` with its result.
Attachments and compaction summaries are not replayed.

## Config options

Each session offers two options, sent in the `session/new`, `session/load`,
and `session/resume` responses and after every change:

| ID | Category | Values |
| --- | --- | --- |
| `model` | `model` | The models `/model` lists, by name. Omitted when there are none. |
| `effort` | `thought_level` | `default` and the active model's reasoning efforts. Omitted when the model has none. |

Setting either is saved as your default, as `/model` and Shift+Tab are in the
full-screen UI. Neither can change while a turn is running.

## Extensions

kon's extensions are named under `kon.kitsu.red`. Their methods start with
`_kon.kitsu.red/`, as ACP reserves names starting with `_` for extensions. A
client that knows none of them still gets standard ACP behavior.

### Agent-started turns

kon starts a turn of its own when something arrives for an idle session:

- **A background job exits.** The agent hears about it, as it does in the
  full-screen UI, so it can act on a job that outlived its turn.
- **Steering is left over.** [Steering](#steering) that arrived too late for
  the turn it was sent to, or a turn cancelled with steering pending, runs as
  the next turn.

A plain ACP client cannot accept a turn it did not request, so kon starts
these only for a client that opts in during `initialize`:

```json
{"clientCapabilities": {"_meta": {"kon.kitsu.red": {"agentTurns": true}}}}
```

Without the opt-in, nothing is lost: what arrived waits for the next prompt
and is delivered during its turn, reported as a `user_message_chunk`.

An agent-started turn is bracketed by two notifications. Between them, kon
sends ordinary `session/update` notifications, beginning with the message
that started the turn as a `user_message_chunk`.

```json
{"jsonrpc": "2.0", "method": "_kon.kitsu.red/turn_start", "params": {"sessionId": "ses_…"}}
{"jsonrpc": "2.0", "method": "_kon.kitsu.red/turn_end", "params": {"sessionId": "ses_…", "stopReason": "end_turn"}}
```

`turn_end` carries `stopReason` as a `session/prompt` response would, and
`error`, a message, instead when the turn failed. A `session/prompt` sent
during an agent-started turn waits for it like any other, and `session/cancel`
cancels it.

### Steering

`_kon.kitsu.red/steer` adds a message to the running turn, as Enter does while
kon works in the full-screen UI. The agent reads it before its next request,
and it is reported as a `user_message_chunk` when delivered. Several stack and
arrive together.

```json
{"jsonrpc": "2.0", "id": 7, "method": "_kon.kitsu.red/steer", "params": {"sessionId": "ses_…", "text": "use the existing helper"}}
{"jsonrpc": "2.0", "id": 7, "result": {}}
```

Steering an idle session fails; send a `session/prompt` instead.

`_kon.kitsu.red/withdraw` takes back every steering message not yet
delivered and returns them, oldest first, so a client can put them back in
its input:

```json
{"jsonrpc": "2.0", "id": 8, "method": "_kon.kitsu.red/withdraw", "params": {"sessionId": "ses_…"}}
{"jsonrpc": "2.0", "id": 8, "result": {"withdrawn": ["use the existing helper"]}}
```

### Background jobs

`_kon.kitsu.red/jobs` lists the session's background jobs, newest first, as
`/jobs` does:

```json
{"jsonrpc": "2.0", "id": 9, "method": "_kon.kitsu.red/jobs", "params": {"sessionId": "ses_…"}}
{"jsonrpc": "2.0", "id": 9, "result": {"jobs": [
  {"id": 2, "command": "go test ./...", "running": true, "output": "/…/jobs/2/output"},
  {"id": 1, "command": "kon run review this", "running": false, "exit": "0", "subagentSessionId": "ses_…"}
]}}
```

`output` is the file the job's output is written to. `exit` is how the job
ended, an exit code or why it was killed, and is absent while it runs.
`subagentSessionId` is the session of a `kon run` subagent.

`_kon.kitsu.red/kill_job` stops a running job:

```json
{"jsonrpc": "2.0", "id": 10, "method": "_kon.kitsu.red/kill_job", "params": {"sessionId": "ses_…", "id": 2}}
{"jsonrpc": "2.0", "id": 10, "result": {}}
```

## Lifetime

`kon acp` runs until stdin closes or it receives an interrupt or terminate
signal. It then cancels every turn, stops every background job, and closes
every session.
