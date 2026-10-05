package acp

import (
	"bufio"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"sync"
	"time"
)

// Handler receives what the agent sends on its own. Each method is called on
// the client's read loop, in the order the agent sent them, so a handler must
// not block on a call to the same client.
type Handler interface {
	// Update is a session/update. update is a ContentChunk, a ToolCall, a
	// CommandsUpdate, or a UsageUpdate; a kind this package does not know is
	// not delivered.
	Update(sessionID string, update any)
	// TurnStart and TurnEnd bracket a turn kon started itself. errText is
	// set instead of stopReason when the turn failed.
	TurnStart(sessionID string)
	TurnEnd(sessionID, stopReason, errText string)
}

// ErrClosed is returned by calls made after the connection ended.
var ErrClosed = errors.New("acp: connection closed")

// Client is one connection to an agent. Its methods are safe for concurrent
// use; replies are matched to calls by ID.
type Client struct {
	h   Handler
	enc *json.Encoder
	wmu sync.Mutex

	mu      sync.Mutex
	nextID  int64
	pending map[int64]chan reply
	err     error

	done chan struct{}
	// stop ends the transport: it stops the subprocess for Spawn.
	stop func()
}

type reply struct {
	result json.RawMessage
	err    error
}

type outgoing struct {
	JSONRPC string `json:"jsonrpc"`
	ID      *int64 `json:"id,omitempty"`
	Method  string `json:"method"`
	Params  any    `json:"params,omitempty"`
}

// refusal answers a request from the agent, which kon never sends.
type refusal struct {
	JSONRPC string          `json:"jsonrpc"`
	ID      json.RawMessage `json:"id"`
	Error   Error           `json:"error"`
}

type incoming struct {
	ID     json.RawMessage `json:"id"`
	Method string          `json:"method"`
	Params json.RawMessage `json:"params"`
	Result json.RawMessage `json:"result"`
	Error  *Error          `json:"error"`
}

// New speaks ACP over r and w, reading until r ends. The caller owns both:
// Close does not end a connection made this way.
func New(r io.Reader, w io.Writer, h Handler) *Client {
	c := &Client{h: h, enc: json.NewEncoder(w), pending: map[int64]chan reply{}, done: make(chan struct{}), stop: func() {}}
	go c.read(r)
	return c
}

// Spawn starts command as the agent, with its stderr passed through to this
// process's, and connects to it.
func Spawn(command string, args []string, h Handler) (*Client, error) {
	cmd := exec.Command(command, args...)
	cmd.Stderr = os.Stderr
	stdin, err := cmd.StdinPipe()
	if err != nil {
		return nil, err
	}
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		return nil, err
	}
	if err := cmd.Start(); err != nil {
		return nil, fmt.Errorf("start %s: %w", command, err)
	}
	c := New(stdout, stdin, h)
	// Closing stdin is how kon acp is asked to shut down; it then cancels
	// its turns and closes its sessions before exiting. One that has not
	// exited after a grace period is killed.
	c.stop = func() {
		stdin.Close()
		select {
		case <-c.done:
		case <-time.After(5 * time.Second):
			cmd.Process.Kill()
			<-c.done
		}
	}
	go func() {
		<-c.done
		cmd.Wait()
	}()
	return c, nil
}

// Done is closed when the connection ends.
func (c *Client) Done() <-chan struct{} { return c.done }

// Err is why the connection ended, once Done is closed.
func (c *Client) Err() error {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.err
}

// Close stops a spawned agent and waits for its connection to end.
func (c *Client) Close() error {
	c.stop()
	return nil
}

func (c *Client) read(r io.Reader) {
	sc := bufio.NewScanner(r)
	// Tool results and replays can be large; a line is one whole message.
	sc.Buffer(make([]byte, 64*1024), 64*1024*1024)
	for sc.Scan() {
		var m incoming
		if err := json.Unmarshal(sc.Bytes(), &m); err != nil {
			continue
		}
		c.dispatch(m)
	}
	err := sc.Err()
	if err == nil {
		err = io.EOF
	}
	c.mu.Lock()
	c.err = err
	pending := c.pending
	c.pending = nil
	c.mu.Unlock()
	for _, ch := range pending {
		ch <- reply{err: ErrClosed}
	}
	close(c.done)
}

func (c *Client) dispatch(m incoming) {
	switch {
	case m.Method != "" && len(m.ID) > 0:
		// kon never calls the client, but a request deserves an answer.
		c.send(refusal{JSONRPC: "2.0", ID: m.ID, Error: Error{Code: CodeMethodNotFound, Message: "method not found"}})
	case m.Method != "":
		c.notification(m.Method, m.Params)
	default:
		var id int64
		if json.Unmarshal(m.ID, &id) != nil {
			return
		}
		c.mu.Lock()
		ch := c.pending[id]
		delete(c.pending, id)
		c.mu.Unlock()
		if ch == nil {
			return
		}
		if m.Error != nil {
			ch <- reply{err: m.Error}
		} else {
			ch <- reply{result: m.Result}
		}
	}
}

func (c *Client) notification(method string, params json.RawMessage) {
	switch method {
	case "session/update":
		var n struct {
			SessionID string          `json:"sessionId"`
			Update    json.RawMessage `json:"update"`
		}
		if json.Unmarshal(params, &n) != nil {
			return
		}
		if update := decodeUpdate(n.Update); update != nil {
			c.h.Update(n.SessionID, update)
		}
	case MethodTurnStart:
		var p SessionParams
		if json.Unmarshal(params, &p) == nil {
			c.h.TurnStart(p.SessionID)
		}
	case MethodTurnEnd:
		var p TurnEnd
		if json.Unmarshal(params, &p) == nil {
			c.h.TurnEnd(p.SessionID, p.StopReason, p.Error)
		}
	}
}

// decodeUpdate reads an update as the type its kind names, or returns nil
// for a kind this package does not know. The kind comes first because the
// kinds reuse keys: a chunk's content is a block and a tool call's a list.
func decodeUpdate(raw json.RawMessage) any {
	var kind struct {
		SessionUpdate string `json:"sessionUpdate"`
	}
	if json.Unmarshal(raw, &kind) != nil {
		return nil
	}
	switch kind.SessionUpdate {
	case UpdateUserMessage, UpdateAgentMessage, UpdateAgentThought:
		return decodeAs[ContentChunk](raw)
	case UpdateToolCall, UpdateToolProgress:
		return decodeAs[ToolCall](raw)
	case UpdateCommands:
		return decodeAs[CommandsUpdate](raw)
	case UpdateUsage:
		return decodeAs[UsageUpdate](raw)
	}
	return nil
}

func decodeAs[T any](raw json.RawMessage) any {
	var v T
	if json.Unmarshal(raw, &v) != nil {
		return nil
	}
	return v
}

func (c *Client) send(v any) error {
	c.wmu.Lock()
	defer c.wmu.Unlock()
	return c.enc.Encode(v)
}

// Call sends a request and decodes its result into result, which may be nil.
// A cancelled ctx abandons the wait; the agent may still carry the request
// out. The methods below cover everything kon serves, so Call is for a
// method they do not.
func (c *Client) Call(ctx context.Context, method string, params, result any) error {
	ch := make(chan reply, 1)
	c.mu.Lock()
	if c.pending == nil {
		c.mu.Unlock()
		return ErrClosed
	}
	c.nextID++
	id := c.nextID
	c.pending[id] = ch
	c.mu.Unlock()

	if err := c.send(outgoing{JSONRPC: "2.0", ID: &id, Method: method, Params: params}); err != nil {
		c.forget(id)
		return err
	}
	select {
	case r := <-ch:
		if r.err != nil {
			return r.err
		}
		if result == nil || len(r.result) == 0 {
			return nil
		}
		return json.Unmarshal(r.result, result)
	case <-ctx.Done():
		c.forget(id)
		return ctx.Err()
	}
}

func (c *Client) forget(id int64) {
	c.mu.Lock()
	delete(c.pending, id)
	c.mu.Unlock()
}

// Notify sends a notification.
func (c *Client) Notify(method string, params any) error {
	return c.send(outgoing{JSONRPC: "2.0", Method: method, Params: params})
}

// Initialize negotiates the protocol, opting into turns kon starts itself,
// which reach the Handler: a background job that exits, or steering left
// over after a turn.
func (c *Client) Initialize(ctx context.Context, name, version string) (InitializeResponse, error) {
	req := InitializeRequest{
		ProtocolVersion:    ProtocolVersion,
		ClientInfo:         Implementation{Name: name, Version: version},
		ClientCapabilities: ClientCapabilities{Meta: ClientMeta{Kon: ClientExtensions{AgentTurns: true}}},
	}
	var r InitializeResponse
	err := c.Call(ctx, "initialize", req, &r)
	return r, err
}

// NewSession starts a session in cwd, which must be absolute. instructions
// join its system prompt as if from an AGENTS.md; kon fixes the prompt when a
// session starts, so they cannot change later.
func (c *Client) NewSession(ctx context.Context, cwd, instructions string) (SessionResponse, error) {
	req := NewSessionRequest{SessionRequest: SessionRequest{CWD: cwd, MCPServers: []json.RawMessage{}}}
	if instructions != "" {
		req.Meta = &SessionMeta{Kon: SessionExtensions{Instructions: instructions}}
	}
	var s SessionResponse
	err := c.Call(ctx, "session/new", req, &s)
	return s, err
}

// LoadSession opens a saved session of cwd. The agent replays the
// conversation to the Handler before the call returns.
func (c *Client) LoadSession(ctx context.Context, id, cwd string) (SessionResponse, error) {
	return c.reopen(ctx, "session/load", id, cwd)
}

// ResumeSession opens a saved session of cwd without replaying it.
func (c *Client) ResumeSession(ctx context.Context, id, cwd string) (SessionResponse, error) {
	return c.reopen(ctx, "session/resume", id, cwd)
}

// reopen fills in the session's ID, which only session/new answers with.
func (c *Client) reopen(ctx context.Context, method, id, cwd string) (SessionResponse, error) {
	var s SessionResponse
	err := c.Call(ctx, method, SessionRequest{SessionID: id, CWD: cwd, MCPServers: []json.RawMessage{}}, &s)
	if err == nil && s.SessionID == "" {
		s.SessionID = id
	}
	return s, err
}

// ListSessions lists the saved sessions of cwd, newest first, a page at a
// time: cursor is empty for the first page and the last page's NextCursor
// for the next. An empty cwd lists the directory the agent was started in.
func (c *Client) ListSessions(ctx context.Context, cwd, cursor string) (ListResponse, error) {
	var r ListResponse
	err := c.Call(ctx, "session/list", ListRequest{CWD: cwd, Cursor: cursor}, &r)
	return r, err
}

// CloseSession cancels the session's turn, stops its jobs, and closes it.
func (c *Client) CloseSession(ctx context.Context, id string) error {
	return c.Call(ctx, "session/close", SessionParams{SessionID: id}, nil)
}

// Prompt runs one turn and returns its stop reason. A prompt sent while a
// turn runs waits behind it.
func (c *Client) Prompt(ctx context.Context, id string, prompt []ContentBlock) (string, error) {
	var r PromptResponse
	err := c.Call(ctx, "session/prompt", PromptRequest{SessionID: id, Prompt: prompt}, &r)
	return r.StopReason, err
}

// Cancel stops the running turn and every prompt waiting behind it.
func (c *Client) Cancel(id string) error {
	return c.Notify("session/cancel", SessionParams{SessionID: id})
}

// SetConfig sets a config option and returns the options as they now stand.
func (c *Client) SetConfig(ctx context.Context, id, configID, value string) ([]ConfigOption, error) {
	var r SetConfigResponse
	err := c.Call(ctx, "session/set_config_option", SetConfigRequest{SessionID: id, ConfigID: configID, Value: value}, &r)
	return r.ConfigOptions, err
}

// Steer adds a message to the running turn. It fails with CodeInvalidRequest
// when no turn is running.
func (c *Client) Steer(ctx context.Context, id, text string) error {
	return c.Call(ctx, MethodSteer, SteerRequest{SessionID: id, Text: text}, nil)
}

// Withdraw takes back the steering messages not yet delivered and returns
// them, oldest first.
func (c *Client) Withdraw(ctx context.Context, id string) ([]string, error) {
	var r WithdrawResponse
	err := c.Call(ctx, MethodWithdraw, SessionParams{SessionID: id}, &r)
	return r.Withdrawn, err
}

// Jobs lists the session's background jobs, newest first.
func (c *Client) Jobs(ctx context.Context, id string) ([]Job, error) {
	var r JobsResponse
	err := c.Call(ctx, MethodJobs, SessionParams{SessionID: id}, &r)
	return r.Jobs, err
}

// KillJob stops a running background job.
func (c *Client) KillJob(ctx context.Context, id string, job int) error {
	return c.Call(ctx, MethodKillJob, KillJobRequest{SessionID: id, ID: job}, nil)
}
