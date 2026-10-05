// Package acp is the Agent Client Protocol as kon speaks it: the wire types
// of the parts of ACP v1 kon serves, kon's extensions under the kon.kitsu.red
// namespace, and a client for a program that drives kon acp rather than
// building an agent of its own. docs/product/acp.md is the contract.
//
// A program starts kon with [Spawn], or speaks to one it reached some other
// way with [New], and receives what kon sends on its own through a
// [Handler]:
//
//	client, err := acp.Spawn("kon", []string{"acp"}, handler)
//	if err != nil {
//		return err
//	}
//	defer client.Close()
//	if _, err := client.Initialize(ctx, "mybot", "v1.0.0"); err != nil {
//		return err
//	}
//	s, err := client.NewSession(ctx, "/work/project", "")
//	if err != nil {
//		return err
//	}
//	stop, err := client.Prompt(ctx, s.SessionID, []acp.ContentBlock{acp.TextBlock("hello")})
package acp

import (
	"encoding/json"
	"errors"
)

// The wire types below are named as the schema names them. Fields kon never
// sets are left out rather than sent empty, so a client sees only what kon
// means.

// ProtocolVersion is the ACP major version kon speaks. It answers with it
// whatever the client asks for, which the spec allows: a client that cannot
// speak it disconnects.
const ProtocolVersion = 1

// Extension is the namespace of kon's ACP extensions: the key of their
// capabilities in _meta, and with a leading underscore the prefix of their
// methods.
const Extension = "kon.kitsu.red"

// The methods and notifications of kon's extensions.
const (
	MethodTurnStart = "_" + Extension + "/turn_start"
	MethodTurnEnd   = "_" + Extension + "/turn_end"
	MethodSteer     = "_" + Extension + "/steer"
	MethodWithdraw  = "_" + Extension + "/withdraw"
	MethodJobs      = "_" + Extension + "/jobs"
	MethodKillJob   = "_" + Extension + "/kill_job"
)

// JSON-RPC error codes ACP uses.
const (
	CodeParseError     = -32700
	CodeInvalidRequest = -32600
	CodeMethodNotFound = -32601
	CodeInvalidParams  = -32602
	CodeInternalError  = -32603
	CodeNotFound       = -32002
)

// Error is a JSON-RPC error.
type Error struct {
	Code    int    `json:"code"`
	Message string `json:"message"`
}

func (e *Error) Error() string { return e.Message }

// IsCode reports whether err is a JSON-RPC error with the given code.
func IsCode(err error, code int) bool {
	var e *Error
	return errors.As(err, &e) && e.Code == code
}

type InitializeRequest struct {
	ProtocolVersion    int                `json:"protocolVersion"`
	ClientCapabilities ClientCapabilities `json:"clientCapabilities"`
	ClientInfo         Implementation     `json:"clientInfo"`
}

// ClientCapabilities carries only what kon reads. kon never calls a client's
// fs or terminal methods, so whether a client offers them is left out.
type ClientCapabilities struct {
	Meta ClientMeta `json:"_meta"`
}

type ClientMeta struct {
	Kon ClientExtensions `json:"kon.kitsu.red"`
}

// ClientExtensions is what a client opts into under the kon key of its
// capabilities' _meta.
type ClientExtensions struct {
	AgentTurns bool `json:"agentTurns"`
}

type InitializeResponse struct {
	ProtocolVersion   int               `json:"protocolVersion"`
	AgentInfo         Implementation    `json:"agentInfo"`
	AgentCapabilities AgentCapabilities `json:"agentCapabilities"`
	AuthMethods       []struct{}        `json:"authMethods"`
}

type Implementation struct {
	Name    string `json:"name"`
	Title   string `json:"title,omitempty"`
	Version string `json:"version"`
}

type AgentCapabilities struct {
	LoadSession         bool                `json:"loadSession"`
	PromptCapabilities  PromptCapabilities  `json:"promptCapabilities"`
	MCPCapabilities     MCPCapabilities     `json:"mcpCapabilities"`
	SessionCapabilities SessionCapabilities `json:"sessionCapabilities"`
	Meta                AgentMeta           `json:"_meta"`
}

type PromptCapabilities struct {
	Image           bool `json:"image"`
	Audio           bool `json:"audio"`
	EmbeddedContext bool `json:"embeddedContext"`
}

type MCPCapabilities struct {
	HTTP bool `json:"http"`
	SSE  bool `json:"sse"`
}

// SessionCapabilities advertises a method by sending an empty object for it.
type SessionCapabilities struct {
	List   struct{} `json:"list"`
	Resume struct{} `json:"resume"`
	Close  struct{} `json:"close"`
}

type AgentMeta struct {
	Kon AgentExtensions `json:"kon.kitsu.red"`
}

// AgentExtensions lists kon's extensions under its key of the agent
// capabilities' _meta. An older kon leaves out the ones it lacks.
type AgentExtensions struct {
	Steer        bool `json:"steer"`
	Jobs         bool `json:"jobs"`
	AgentTurns   bool `json:"agentTurns"`
	Instructions bool `json:"instructions"`
}

// SessionRequest covers session/new, session/load, and session/resume; only
// the last two name a session.
type SessionRequest struct {
	SessionID string `json:"sessionId,omitempty"`
	CWD       string `json:"cwd"`
	// MCPServers is required by the schema. kon has no MCP client, so it
	// leaves them undecoded and a client sends an empty list.
	MCPServers []json.RawMessage `json:"mcpServers"`
}

// NewSessionRequest is session/new, the one session request whose _meta kon
// reads. Load and resume leave theirs undecoded, so they accept whatever they
// did before the extension.
type NewSessionRequest struct {
	SessionRequest
	Meta *SessionMeta `json:"_meta,omitempty"`
}

type SessionMeta struct {
	Kon SessionExtensions `json:"kon.kitsu.red"`
}

// SessionExtensions is what a client sets under the kon key of session/new's
// _meta.
type SessionExtensions struct {
	// Instructions are added to the new session's system prompt as if they
	// came from an AGENTS.md.
	Instructions string `json:"instructions,omitempty"`
}

// SessionResponse answers session/new, session/load, and session/resume;
// only the first names the session.
type SessionResponse struct {
	SessionID     string         `json:"sessionId,omitempty"`
	ConfigOptions []ConfigOption `json:"configOptions,omitempty"`
}

type ListRequest struct {
	CWD    string `json:"cwd,omitempty"`
	Cursor string `json:"cursor,omitempty"`
}

type ListResponse struct {
	Sessions   []SessionInfo `json:"sessions"`
	NextCursor string        `json:"nextCursor,omitempty"`
}

type SessionInfo struct {
	SessionID string `json:"sessionId"`
	CWD       string `json:"cwd"`
	Title     string `json:"title,omitempty"`
	UpdatedAt string `json:"updatedAt,omitempty"`
}

// SessionParams is the params of every request that only names a session.
type SessionParams struct {
	SessionID string `json:"sessionId"`
}

type PromptRequest struct {
	SessionID string         `json:"sessionId"`
	Prompt    []ContentBlock `json:"prompt"`
}

type PromptResponse struct {
	StopReason string `json:"stopReason"`
}

// Stop reasons a turn ends with.
const (
	StopEndTurn   = "end_turn"
	StopCancelled = "cancelled"
)

// ContentBlock is every kind of ACP content block in one struct; Type says
// which fields apply.
type ContentBlock struct {
	Type     string    `json:"type"`
	Text     string    `json:"text,omitempty"`
	Data     string    `json:"data,omitempty"`
	MIMEType string    `json:"mimeType,omitempty"`
	URI      string    `json:"uri,omitempty"`
	Name     string    `json:"name,omitempty"`
	Resource *Resource `json:"resource,omitempty"`
}

// Resource is an embedded resource: Text for a text resource, Blob, in
// base64, for a binary one.
type Resource struct {
	URI      string  `json:"uri"`
	MIMEType string  `json:"mimeType,omitempty"`
	Text     *string `json:"text,omitempty"`
	Blob     string  `json:"blob,omitempty"`
}

// TextBlock is a text content block.
func TextBlock(text string) ContentBlock { return ContentBlock{Type: "text", Text: text} }

type SetConfigRequest struct {
	SessionID string `json:"sessionId"`
	ConfigID  string `json:"configId"`
	Value     string `json:"value"`
}

type SetConfigResponse struct {
	ConfigOptions []ConfigOption `json:"configOptions"`
}

// ConfigOption is a session setting kon offers.
type ConfigOption struct {
	ID           string         `json:"id"`
	Name         string         `json:"name"`
	Category     string         `json:"category"`
	Type         string         `json:"type"`
	CurrentValue string         `json:"currentValue"`
	Options      []ConfigChoice `json:"options"`
}

type ConfigChoice struct {
	Value string `json:"value"`
	Name  string `json:"name"`
}

// Config option IDs kon offers, and the effort value that means the provider
// default.
const (
	ConfigModel   = "model"
	ConfigEffort  = "effort"
	EffortDefault = "default"
)

// SessionNotification is the params of session/update. Update is one of the
// update types below, each of which names its kind in SessionUpdate.
type SessionNotification struct {
	SessionID string `json:"sessionId"`
	Update    any    `json:"update"`
}

// Kinds of session update kon sends.
const (
	UpdateUserMessage  = "user_message_chunk"
	UpdateAgentMessage = "agent_message_chunk"
	UpdateAgentThought = "agent_thought_chunk"
	UpdateToolCall     = "tool_call"
	UpdateToolProgress = "tool_call_update"
	UpdateCommands     = "available_commands_update"
	UpdateUsage        = "usage_update"
)

// ContentChunk is a piece of a user message, an agent message, or an agent
// thought.
type ContentChunk struct {
	SessionUpdate string       `json:"sessionUpdate"`
	Content       ContentBlock `json:"content"`
}

// ToolCall is both a tool_call and a tool_call_update; an update leaves out
// what has not changed.
type ToolCall struct {
	SessionUpdate string            `json:"sessionUpdate"`
	ToolCallID    string            `json:"toolCallId"`
	Title         string            `json:"title,omitempty"`
	Name          string            `json:"name,omitempty"`
	Kind          string            `json:"kind,omitempty"`
	Status        string            `json:"status,omitempty"`
	Content       []ToolCallContent `json:"content,omitempty"`
	Locations     []Location        `json:"locations,omitempty"`
	RawInput      json.RawMessage   `json:"rawInput,omitempty"`
	RawOutput     json.RawMessage   `json:"rawOutput,omitempty"`
}

// Statuses of a tool call.
const (
	StatusInProgress = "in_progress"
	StatusCompleted  = "completed"
	StatusFailed     = "failed"
)

// ToolCallContent is a content block, when Type is "content", or a diff.
type ToolCallContent struct {
	Type    string        `json:"type"`
	Content *ContentBlock `json:"content,omitempty"`
	Path    string        `json:"path,omitempty"`
	OldText *string       `json:"oldText,omitempty"`
	NewText *string       `json:"newText,omitempty"`
}

type Location struct {
	Path string `json:"path"`
}

type CommandsUpdate struct {
	SessionUpdate     string    `json:"sessionUpdate"`
	AvailableCommands []Command `json:"availableCommands"`
}

type Command struct {
	Name        string `json:"name"`
	Description string `json:"description"`
}

type UsageUpdate struct {
	SessionUpdate string `json:"sessionUpdate"`
	Used          int64  `json:"used"`
	Size          int64  `json:"size"`
	Cost          *Cost  `json:"cost,omitempty"`
}

type Cost struct {
	Amount   float64 `json:"amount"`
	Currency string  `json:"currency"`
}

// TurnEnd is the params of the turn_end extension notification: StopReason
// as a session/prompt response would carry it, or Error when the turn failed.
type TurnEnd struct {
	SessionID  string `json:"sessionId"`
	StopReason string `json:"stopReason,omitempty"`
	Error      string `json:"error,omitempty"`
}

type SteerRequest struct {
	SessionID string `json:"sessionId"`
	Text      string `json:"text"`
}

type WithdrawResponse struct {
	Withdrawn []string `json:"withdrawn"`
}

type JobsResponse struct {
	Jobs []Job `json:"jobs"`
}

// Job is one of a session's background jobs. Output is the file its output
// is written to, and Exit how it ended, empty while it runs.
type Job struct {
	ID                int    `json:"id"`
	Command           string `json:"command"`
	Running           bool   `json:"running"`
	Exit              string `json:"exit,omitempty"`
	Output            string `json:"output"`
	SubagentSessionID string `json:"subagentSessionId,omitempty"`
}

type KillJobRequest struct {
	SessionID string `json:"sessionId"`
	ID        int    `json:"id"`
}
