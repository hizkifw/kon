// Package acp serves kon over the Agent Client Protocol, for editors that run
// kon as a subprocess. It is a third frontend beside internal/ui and
// internal/headless: each ACP session is an app runtime, and this package
// only turns JSON-RPC into runtime calls and agent events into session
// updates. docs/product/acp.md is the contract, kon's extensions included.
package acp

import (
	"bufio"
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"path/filepath"
	"strconv"
	"sync"
	"sync/atomic"
	"time"

	"kon.kitsu.red/core/agent"
	"kon.kitsu.red/core/session"
	"kon.kitsu.red/core/tokens"
	"kon.kitsu.red/core/typedid"
	"kon.kitsu.red/internal/app"
	"kon.kitsu.red/internal/codetools"
	"kon.kitsu.red/internal/sessions"
)

// Runtime is the part of app.Runtime an ACP session drives.
type Runtime interface {
	RunPrompt(ctx context.Context, prompt agent.Prompt, inbox *agent.Inbox, emit func(agent.Event)) error
	Compact(ctx context.Context, emit func(agent.Event)) error
	Interrupt(attempt int) bool
	LiveID() typedid.SessionID
	State() app.State
	SessionHistory() []session.Entry
	ContextUsage() (tokens.Count, bool)
	SubagentUsage() session.Usage
	Models() []app.Model
	LoadCatalog()
	SwitchModel(name string) error
	SetEffort(effort string) error
	Notices() <-chan string
	Jobs() []codetools.Job
	KillJob(id int) error
	Close() error
}

// Server answers one client. Start and List are where it meets kon's
// storage, which cmd/kon wires up and tests replace.
type Server struct {
	// Start opens a runtime in cwd: on a new session for a zero id, or on
	// the saved session id.
	Start func(cwd string, id typedid.SessionID) (Runtime, error)
	// List lists the saved sessions of cwd, newest first.
	List func(cwd string) ([]sessions.Summary, error)
	// CWD is where kon acp was started, which session/list lists when the
	// client names no directory.
	CWD     string
	Version string
}

// listPage bounds a session/list response, as the spec asks an agent to.
const listPage = 100

// conn is the state of one connection: its sessions and what the client
// opted into.
type conn struct {
	server *Server
	out    *writer
	// agentTurns is the client's opt-in to turns kon starts itself, set by
	// initialize.
	agentTurns atomic.Bool

	mu       sync.Mutex
	sessions map[string]*liveSession
	// closed is set once the connection has ended, so a session opened by a
	// request still in flight is closed rather than left running.
	closed bool
	// handlers counts requests still being answered, so Serve can wait for
	// them before it returns.
	handlers sync.WaitGroup
}

// Serve reads requests from in and writes responses and notifications to out
// until in ends or ctx is cancelled. It then cancels every turn and closes
// every session.
func (s *Server) Serve(ctx context.Context, in io.Reader, out io.Writer) error {
	c := &conn{server: s, out: newWriter(out), sessions: map[string]*liveSession{}}
	lines := make(chan []byte)
	readErr := make(chan error, 1)
	go func() {
		reader := bufio.NewReader(in)
		for {
			line, err := reader.ReadBytes('\n')
			if len(line) > 0 {
				select {
				case lines <- line:
				case <-ctx.Done():
					return
				}
			}
			if err != nil {
				if errors.Is(err, io.EOF) {
					err = nil
				}
				readErr <- err
				return
			}
		}
	}()
	var err error
loop:
	for {
		select {
		case line := <-lines:
			c.dispatch(line)
		case err = <-readErr:
			break loop
		case <-ctx.Done():
			break loop
		}
	}
	closeErr := c.closeAll()
	c.handlers.Wait()
	return errors.Join(err, closeErr)
}

// handler answers one request; a nil result with a nil error is an empty
// object.
type handler func(c *conn, params json.RawMessage) (any, error)

var handlers = map[string]handler{
	"initialize":                  (*conn).initialize,
	"authenticate":                func(*conn, json.RawMessage) (any, error) { return nil, nil },
	"session/new":                 (*conn).newSession,
	"session/load":                (*conn).loadSession,
	"session/resume":              (*conn).resumeSession,
	"session/list":                (*conn).listSessions,
	"session/close":               (*conn).closeSession,
	"session/set_config_option":   (*conn).setConfigOption,
	"_" + extension + "/steer":    (*conn).steer,
	"_" + extension + "/withdraw": (*conn).withdraw,
	"_" + extension + "/jobs":     (*conn).jobs,
	"_" + extension + "/kill_job": (*conn).killJob,
}

// opened is the response of a request that opened a session, whose first
// updates must follow the response: a client cannot place updates for a
// session it has not yet been told of.
type opened struct {
	response any
	session  *liveSession
}

// dispatch handles one line. Requests are answered on their own goroutines,
// so a long one, such as a prompt waiting its turn, does not hold up the
// next. A prompt joins its session's queue here, before its goroutine
// starts, so prompts run in the order they arrived.
func (c *conn) dispatch(line []byte) {
	if len(bytes.TrimSpace(line)) == 0 {
		return
	}
	var msg incoming
	if err := json.Unmarshal(line, &msg); err != nil {
		c.out.respond(json.RawMessage("null"), nil, &rpcError{Code: codeParseError, Message: "parse error: " + err.Error()})
		return
	}
	isRequest := len(msg.ID) > 0 && string(msg.ID) != "null"
	switch {
	case msg.Method == "":
		// A response: kon sends no requests, so there is nothing to match.
		return
	case !isRequest:
		if msg.Method == "session/cancel" {
			var params sessionParams
			if json.Unmarshal(msg.Params, &params) == nil {
				if s := c.session(params.SessionID); s != nil {
					s.cancelTurns()
				}
			}
		}
		// Unknown notifications are ignored, as the spec asks.
		return
	case msg.Method == "session/prompt":
		c.prompt(msg.ID, msg.Params)
		return
	}
	handle := handlers[msg.Method]
	if handle == nil {
		c.out.respond(msg.ID, nil, &rpcError{Code: codeMethodNotFound, Message: "method not found: " + msg.Method})
		return
	}
	c.handlers.Add(1)
	go func() {
		defer c.handlers.Done()
		result, err := handle(c, msg.Params)
		if o, ok := result.(opened); ok {
			c.out.respond(msg.ID, o.response, err)
			o.session.announce()
			return
		}
		if err == nil && result == nil {
			result = struct{}{}
		}
		c.out.respond(msg.ID, result, err)
	}()
}

func decode(params json.RawMessage, v any) error {
	if len(params) == 0 {
		return nil
	}
	if err := json.Unmarshal(params, v); err != nil {
		return invalidParams(err)
	}
	return nil
}

func (c *conn) initialize(params json.RawMessage) (any, error) {
	var req initializeRequest
	if err := decode(params, &req); err != nil {
		return nil, err
	}
	var opted clientExtensions
	if raw, ok := req.ClientCapabilities.Meta[extension]; ok {
		_ = json.Unmarshal(raw, &opted)
	}
	c.agentTurns.Store(opted.AgentTurns)
	return initializeResponse{
		ProtocolVersion: protocolVersion,
		AgentInfo:       implementation{Name: "kon", Title: "kon", Version: c.server.Version},
		AgentCapabilities: agentCapabilities{
			LoadSession:        true,
			PromptCapabilities: promptCapabilities{Image: true, Audio: true, EmbeddedContext: true},
			Meta:               map[string]any{extension: agentExtensions{Steer: true, Jobs: true, AgentTurns: true}},
		},
		AuthMethods: []struct{}{},
	}, nil
}

// session returns the open session named id, or nil.
func (c *conn) session(id string) *liveSession {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.sessions[id]
}

// openSession returns the open session named id, or a not-found error.
func (c *conn) openSession(id string) (*liveSession, error) {
	if s := c.session(id); s != nil {
		return s, nil
	}
	return nil, &rpcError{Code: codeNotFound, Message: "session not found: " + id}
}

// workspace checks a client's cwd and canonicalizes it the way kon does its
// own, so a session is filed under the directory kon itself would use.
func workspace(cwd string) (string, error) {
	if !filepath.IsAbs(cwd) {
		return "", &rpcError{Code: codeInvalidParams, Message: fmt.Sprintf("cwd must be an absolute path, not %q", cwd)}
	}
	cwd = filepath.Clean(cwd)
	if canonical, err := filepath.EvalSymlinks(cwd); err == nil {
		cwd = canonical
	}
	return cwd, nil
}

func (c *conn) newSession(params json.RawMessage) (any, error) {
	var req sessionRequest
	if err := decode(params, &req); err != nil {
		return nil, err
	}
	cwd, err := workspace(req.CWD)
	if err != nil {
		return nil, err
	}
	runtime, err := c.server.Start(cwd, typedid.SessionID{})
	if err != nil {
		return nil, err
	}
	s, err := c.adopt(runtime, cwd)
	if err != nil {
		return nil, err
	}
	return opened{sessionResponse{SessionID: s.id, ConfigOptions: s.configOptions()}, s}, nil
}

func (c *conn) loadSession(params json.RawMessage) (any, error) {
	s, err := c.reopen(params)
	if err != nil {
		return nil, err
	}
	s.replay()
	return opened{sessionResponse{ConfigOptions: s.configOptions()}, s}, nil
}

func (c *conn) resumeSession(params json.RawMessage) (any, error) {
	s, err := c.reopen(params)
	if err != nil {
		return nil, err
	}
	return opened{sessionResponse{ConfigOptions: s.configOptions()}, s}, nil
}

// reopen opens a saved session for session/load or session/resume, or
// returns it when this connection already has it open.
func (c *conn) reopen(params json.RawMessage) (*liveSession, error) {
	var req sessionRequest
	if err := decode(params, &req); err != nil {
		return nil, err
	}
	cwd, err := workspace(req.CWD)
	if err != nil {
		return nil, err
	}
	id, err := typedid.ParseSessionID(req.SessionID)
	if err != nil {
		return nil, invalidParams(err)
	}
	if s := c.session(id.String()); s != nil {
		return s, nil
	}
	runtime, err := c.server.Start(cwd, id)
	if err != nil {
		return nil, &rpcError{Code: codeNotFound, Message: err.Error()}
	}
	// Another kon is writing the session, so this one could only follow it.
	if runtime.State().Following() {
		return nil, errors.Join(errors.New("session is open in another kon"), runtime.Close())
	}
	return c.adopt(runtime, cwd)
}

// adopt registers a runtime as an open session and starts listening for its
// notices.
func (c *conn) adopt(runtime Runtime, cwd string) (*liveSession, error) {
	s := newLiveSession(c, runtime, cwd)
	c.mu.Lock()
	if c.closed {
		c.mu.Unlock()
		return nil, errors.Join(app.ErrClosed, runtime.Close())
	}
	c.sessions[s.id] = s
	c.handlers.Add(1)
	c.mu.Unlock()
	go func() {
		defer c.handlers.Done()
		s.listen()
	}()
	return s, nil
}

func (c *conn) listSessions(params json.RawMessage) (any, error) {
	var req listRequest
	if err := decode(params, &req); err != nil {
		return nil, err
	}
	cwd := c.server.CWD
	if req.CWD != "" {
		var err error
		if cwd, err = workspace(req.CWD); err != nil {
			return nil, err
		}
	}
	start := 0
	if req.Cursor != "" {
		n, err := strconv.Atoi(req.Cursor)
		if err != nil || n < 0 {
			return nil, &rpcError{Code: codeInvalidParams, Message: "invalid cursor"}
		}
		start = n
	}
	summaries, err := c.server.List(cwd)
	if err != nil {
		return nil, err
	}
	resp := listResponse{Sessions: []sessionInfo{}}
	listed := 0
	for _, summary := range summaries {
		// A subagent's session is the agent's, not one a person resumes.
		if !summary.Parent.IsZero() {
			continue
		}
		if listed++; listed <= start {
			continue
		}
		if len(resp.Sessions) == listPage {
			resp.NextCursor = strconv.Itoa(start + listPage)
			break
		}
		resp.Sessions = append(resp.Sessions, sessionInfo{
			SessionID: summary.ID.String(), CWD: summary.CWD, Title: summary.Title,
			UpdatedAt: summary.CreatedAt.UTC().Format(time.RFC3339),
		})
	}
	return resp, nil
}

func (c *conn) closeSession(params json.RawMessage) (any, error) {
	var req sessionParams
	if err := decode(params, &req); err != nil {
		return nil, err
	}
	s, err := c.openSession(req.SessionID)
	if err != nil {
		return nil, err
	}
	c.mu.Lock()
	delete(c.sessions, req.SessionID)
	c.mu.Unlock()
	return nil, s.close()
}

// closeAll closes every session when the connection ends.
func (c *conn) closeAll() error {
	c.mu.Lock()
	c.closed = true
	open := make([]*liveSession, 0, len(c.sessions))
	for id, s := range c.sessions {
		open = append(open, s)
		delete(c.sessions, id)
	}
	c.mu.Unlock()
	var errs []error
	for _, s := range open {
		errs = append(errs, s.close())
	}
	return errors.Join(errs...)
}
