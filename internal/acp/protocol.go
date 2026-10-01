package acp

import "encoding/json"

// The wire types below are the parts of ACP v1 kon reads or writes, named as
// the schema names them. Fields kon never sets are left out rather than sent
// empty, so a client sees only what kon means.

// protocolVersion is the ACP major version kon speaks. It answers with it
// whatever the client asks for, which the spec allows: a client that cannot
// speak it disconnects.
const protocolVersion = 1

// extension is the namespace of kon's ACP extensions: the key of their
// capabilities in _meta, and with a leading underscore the prefix of their
// methods.
const extension = "kon.kitsu.red"

type initializeRequest struct {
	ClientCapabilities struct {
		Meta map[string]json.RawMessage `json:"_meta"`
	} `json:"clientCapabilities"`
}

// clientExtensions is what a client opts into under the kon key of its
// capabilities' _meta.
type clientExtensions struct {
	AgentTurns bool `json:"agentTurns"`
}

type initializeResponse struct {
	ProtocolVersion   int               `json:"protocolVersion"`
	AgentInfo         implementation    `json:"agentInfo"`
	AgentCapabilities agentCapabilities `json:"agentCapabilities"`
	AuthMethods       []struct{}        `json:"authMethods"`
}

type implementation struct {
	Name    string `json:"name"`
	Title   string `json:"title"`
	Version string `json:"version"`
}

type agentCapabilities struct {
	LoadSession         bool                `json:"loadSession"`
	PromptCapabilities  promptCapabilities  `json:"promptCapabilities"`
	MCPCapabilities     mcpCapabilities     `json:"mcpCapabilities"`
	SessionCapabilities sessionCapabilities `json:"sessionCapabilities"`
	Meta                map[string]any      `json:"_meta"`
}

type promptCapabilities struct {
	Image           bool `json:"image"`
	Audio           bool `json:"audio"`
	EmbeddedContext bool `json:"embeddedContext"`
}

type mcpCapabilities struct {
	HTTP bool `json:"http"`
	SSE  bool `json:"sse"`
}

// sessionCapabilities advertises a method by sending an empty object for it.
type sessionCapabilities struct {
	List   struct{} `json:"list"`
	Resume struct{} `json:"resume"`
	Close  struct{} `json:"close"`
}

// agentExtensions lists kon's extensions under its key of the agent
// capabilities' _meta.
type agentExtensions struct {
	Steer      bool `json:"steer"`
	Jobs       bool `json:"jobs"`
	AgentTurns bool `json:"agentTurns"`
}

// sessionRequest covers session/new, session/load, and session/resume; only
// the last two name a session.
type sessionRequest struct {
	SessionID string `json:"sessionId"`
	CWD       string `json:"cwd"`
}

type sessionResponse struct {
	SessionID     string         `json:"sessionId,omitempty"`
	ConfigOptions []configOption `json:"configOptions,omitempty"`
}

type listRequest struct {
	CWD    string `json:"cwd"`
	Cursor string `json:"cursor"`
}

type listResponse struct {
	Sessions   []sessionInfo `json:"sessions"`
	NextCursor string        `json:"nextCursor,omitempty"`
}

type sessionInfo struct {
	SessionID string `json:"sessionId"`
	CWD       string `json:"cwd"`
	Title     string `json:"title,omitempty"`
	UpdatedAt string `json:"updatedAt,omitempty"`
}

// sessionParams is the params of every request that only names a session.
type sessionParams struct {
	SessionID string `json:"sessionId"`
}

type promptRequest struct {
	SessionID string         `json:"sessionId"`
	Prompt    []contentBlock `json:"prompt"`
}

type promptResponse struct {
	StopReason string `json:"stopReason"`
}

const (
	stopEndTurn   = "end_turn"
	stopCancelled = "cancelled"
)

// contentBlock is every kind of ACP content block in one struct; Type says
// which fields apply.
type contentBlock struct {
	Type     string    `json:"type"`
	Text     string    `json:"text,omitempty"`
	Data     string    `json:"data,omitempty"`
	MIMEType string    `json:"mimeType,omitempty"`
	URI      string    `json:"uri,omitempty"`
	Name     string    `json:"name,omitempty"`
	Resource *resource `json:"resource,omitempty"`
}

// resource is an embedded resource: Text for a text resource, Blob for a
// binary one.
type resource struct {
	URI      string  `json:"uri"`
	MIMEType string  `json:"mimeType,omitempty"`
	Text     *string `json:"text,omitempty"`
	Blob     string  `json:"blob,omitempty"`
}

func textBlock(text string) contentBlock { return contentBlock{Type: "text", Text: text} }

type setConfigRequest struct {
	SessionID string `json:"sessionId"`
	ConfigID  string `json:"configId"`
	Value     string `json:"value"`
}

type setConfigResponse struct {
	ConfigOptions []configOption `json:"configOptions"`
}

type configOption struct {
	ID           string         `json:"id"`
	Name         string         `json:"name"`
	Category     string         `json:"category"`
	Type         string         `json:"type"`
	CurrentValue string         `json:"currentValue"`
	Options      []configChoice `json:"options"`
}

type configChoice struct {
	Value string `json:"value"`
	Name  string `json:"name"`
}

// sessionNotification is the params of session/update.
type sessionNotification struct {
	SessionID string `json:"sessionId"`
	Update    any    `json:"update"`
}

type contentChunk struct {
	SessionUpdate string       `json:"sessionUpdate"`
	Content       contentBlock `json:"content"`
}

// toolCall is both a tool_call and a tool_call_update; an update leaves out
// what has not changed.
type toolCall struct {
	SessionUpdate string            `json:"sessionUpdate"`
	ToolCallID    string            `json:"toolCallId"`
	Title         string            `json:"title,omitempty"`
	Name          string            `json:"name,omitempty"`
	Kind          string            `json:"kind,omitempty"`
	Status        string            `json:"status,omitempty"`
	Content       []toolCallContent `json:"content,omitempty"`
	Locations     []location        `json:"locations,omitempty"`
	RawInput      any               `json:"rawInput,omitempty"`
	RawOutput     json.RawMessage   `json:"rawOutput,omitempty"`
}

// toolCallContent is a content block, when Type is "content", or a diff.
type toolCallContent struct {
	Type    string        `json:"type"`
	Content *contentBlock `json:"content,omitempty"`
	Path    string        `json:"path,omitempty"`
	OldText *string       `json:"oldText,omitempty"`
	NewText *string       `json:"newText,omitempty"`
}

type location struct {
	Path string `json:"path"`
}

type commandsUpdate struct {
	SessionUpdate     string    `json:"sessionUpdate"`
	AvailableCommands []command `json:"availableCommands"`
}

type command struct {
	Name        string `json:"name"`
	Description string `json:"description"`
}

type configUpdate struct {
	SessionUpdate string         `json:"sessionUpdate"`
	ConfigOptions []configOption `json:"configOptions"`
}

type usageUpdate struct {
	SessionUpdate string `json:"sessionUpdate"`
	Used          int64  `json:"used"`
	Size          int64  `json:"size"`
	Cost          *cost  `json:"cost,omitempty"`
}

type cost struct {
	Amount   float64 `json:"amount"`
	Currency string  `json:"currency"`
}

// turnEnd is the params of the turn_end extension notification.
type turnEnd struct {
	SessionID  string `json:"sessionId"`
	StopReason string `json:"stopReason,omitempty"`
	Error      string `json:"error,omitempty"`
}

type steerRequest struct {
	SessionID string `json:"sessionId"`
	Text      string `json:"text"`
}

type withdrawResponse struct {
	Withdrawn []string `json:"withdrawn"`
}

type jobsResponse struct {
	Jobs []job `json:"jobs"`
}

type job struct {
	ID                int    `json:"id"`
	Command           string `json:"command"`
	Running           bool   `json:"running"`
	Exit              string `json:"exit,omitempty"`
	Output            string `json:"output"`
	SubagentSessionID string `json:"subagentSessionId,omitempty"`
}

type killJobRequest struct {
	SessionID string `json:"sessionId"`
	ID        int    `json:"id"`
}
