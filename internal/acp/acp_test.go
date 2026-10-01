package acp

import (
	"bufio"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"strings"
	"sync"
	"testing"
	"time"

	"kon.kitsu.red/core/agent"
	"kon.kitsu.red/core/session"
	"kon.kitsu.red/core/tokens"
	"kon.kitsu.red/core/typedid"
	"kon.kitsu.red/internal/app"
	"kon.kitsu.red/internal/codetools"
	"kon.kitsu.red/internal/sessions"
)

// fakeRuntime stands in for app.Runtime. run decides what a turn does; by
// default it answers "ok".
type fakeRuntime struct {
	id      typedid.SessionID
	notices chan string
	history []session.Entry
	run     func(ctx context.Context, prompt agent.Prompt, inbox *agent.Inbox, emit func(agent.Event)) error
	state   app.State

	mu         sync.Mutex
	prompts    []agent.Prompt
	compacted  int
	interrupts []int
	closed     bool
	model      string
	effort     string
}

func newFakeRuntime(t *testing.T) *fakeRuntime {
	t.Helper()
	id, err := typedid.NewSessionID()
	if err != nil {
		t.Fatal(err)
	}
	return &fakeRuntime{id: id, notices: make(chan string), state: app.State{Phase: app.PhaseReady, Active: app.Model{Name: "gpt", ContextWindow: 1000, ReasoningEfforts: []string{"low", "high"}}}}
}

func (f *fakeRuntime) RunPrompt(ctx context.Context, prompt agent.Prompt, inbox *agent.Inbox, emit func(agent.Event)) error {
	f.mu.Lock()
	f.prompts = append(f.prompts, prompt)
	f.mu.Unlock()
	if f.run != nil {
		return f.run(ctx, prompt, inbox, emit)
	}
	emit(agent.Event{Kind: agent.EventText, Text: "ok"})
	return nil
}

func (f *fakeRuntime) Compact(context.Context, func(agent.Event)) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.compacted++
	return nil
}

func (f *fakeRuntime) Interrupt(attempt int) bool {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.interrupts = append(f.interrupts, attempt)
	return true
}

func (f *fakeRuntime) LiveID() typedid.SessionID          { return f.id }
func (f *fakeRuntime) SessionHistory() []session.Entry    { return f.history }
func (f *fakeRuntime) ContextUsage() (tokens.Count, bool) { return 0, false }
func (f *fakeRuntime) SubagentUsage() session.Usage       { return session.Usage{} }
func (f *fakeRuntime) LoadCatalog()                       {}
func (f *fakeRuntime) Notices() <-chan string             { return f.notices }
func (f *fakeRuntime) Jobs() []codetools.Job              { return nil }
func (f *fakeRuntime) KillJob(int) error                  { return nil }
func (f *fakeRuntime) Models() []app.Model {
	return []app.Model{{Name: "gpt"}, {Name: "other", DisplayName: "Other"}}
}
func (f *fakeRuntime) SwitchModel(name string) error {
	f.mu.Lock()
	f.model = name
	f.mu.Unlock()
	return nil
}
func (f *fakeRuntime) SetEffort(effort string) error {
	f.mu.Lock()
	f.effort = effort
	f.mu.Unlock()
	return nil
}
func (f *fakeRuntime) State() app.State { return f.state }

func (f *fakeRuntime) Close() error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.closed = true
	return nil
}

// client drives a Server over pipes, as an editor would.
type client struct {
	t      *testing.T
	in     *io.PipeWriter
	lines  chan map[string]any
	nextID int
	done   chan error
}

func serve(t *testing.T, server *Server) *client {
	t.Helper()
	inR, inW := io.Pipe()
	outR, outW := io.Pipe()
	c := &client{t: t, in: inW, lines: make(chan map[string]any, 256), done: make(chan error, 1)}
	go func() {
		err := server.Serve(context.Background(), inR, outW)
		outW.Close()
		c.done <- err
	}()
	go func() {
		scanner := bufio.NewScanner(outR)
		scanner.Buffer(nil, 1<<20)
		for scanner.Scan() {
			var msg map[string]any
			if err := json.Unmarshal(scanner.Bytes(), &msg); err != nil {
				t.Errorf("kon wrote a line that is not JSON: %q", scanner.Text())
				continue
			}
			c.lines <- msg
		}
		close(c.lines)
	}()
	t.Cleanup(func() {
		inW.Close()
		select {
		case <-c.done:
		case <-time.After(5 * time.Second):
			t.Error("Serve did not return after stdin closed")
		}
	})
	return c
}

func (c *client) write(v any) {
	c.t.Helper()
	b, err := json.Marshal(v)
	if err != nil {
		c.t.Fatal(err)
	}
	if _, err := c.in.Write(append(b, '\n')); err != nil {
		c.t.Fatal(err)
	}
}

// request sends a request and returns its ID.
func (c *client) request(method string, params any) float64 {
	c.nextID++
	c.write(map[string]any{"jsonrpc": "2.0", "id": c.nextID, "method": method, "params": params})
	return float64(c.nextID)
}

func (c *client) notify(method string, params any) {
	c.write(map[string]any{"jsonrpc": "2.0", "method": method, "params": params})
}

func (c *client) next() map[string]any {
	c.t.Helper()
	select {
	case msg, ok := <-c.lines:
		if !ok {
			c.t.Fatal("kon closed stdout")
		}
		return msg
	case <-time.After(5 * time.Second):
		c.t.Fatal("timed out waiting for kon")
	}
	return nil
}

// until reads messages up to the response to id, returning the response and
// the notifications before it.
func (c *client) until(id float64) (map[string]any, []map[string]any) {
	c.t.Helper()
	var before []map[string]any
	for {
		msg := c.next()
		if msg["id"] == id && msg["method"] == nil {
			return msg, before
		}
		before = append(before, msg)
	}
}

// call sends a request and returns its result, failing on an error.
func (c *client) call(method string, params any) map[string]any {
	c.t.Helper()
	resp, _ := c.until(c.request(method, params))
	if resp["error"] != nil {
		c.t.Fatalf("%s: %v", method, resp["error"])
	}
	result, _ := resp["result"].(map[string]any)
	return result
}

// updates lists the sessionUpdate kinds among msgs, with each chunk's text.
func updates(msgs []map[string]any) []string {
	var kinds []string
	for _, msg := range msgs {
		switch msg["method"] {
		case "session/update":
			u := msg["params"].(map[string]any)["update"].(map[string]any)
			kind := u["sessionUpdate"].(string)
			if content, ok := u["content"].(map[string]any); ok {
				kind += ":" + fmt.Sprint(content["text"])
			}
			kinds = append(kinds, kind)
		default:
			kinds = append(kinds, msg["method"].(string))
		}
	}
	return kinds
}

func newServer(runtimes ...*fakeRuntime) *Server {
	var mu sync.Mutex
	return &Server{
		Start: func(string, typedid.SessionID) (Runtime, error) {
			mu.Lock()
			defer mu.Unlock()
			if len(runtimes) == 0 {
				return nil, errors.New("no runtime left")
			}
			r := runtimes[0]
			runtimes = runtimes[1:]
			return r, nil
		},
		List:    func(string) ([]sessions.Summary, error) { return nil, nil },
		CWD:     "/work",
		Version: "test",
	}
}

// open initializes the connection and opens one session, returning its ID
// after reading the updates that announce it.
func (c *client) open(capabilities map[string]any) string {
	c.t.Helper()
	c.call("initialize", map[string]any{"protocolVersion": 1, "clientCapabilities": capabilities})
	id := c.call("session/new", map[string]any{"cwd": "/work", "mcpServers": []any{}})["sessionId"].(string)
	if kind := updates([]map[string]any{c.next()}); kind[0] != "available_commands_update" {
		c.t.Fatalf("first update = %v", kind)
	}
	return id
}

func optIn() map[string]any {
	return map[string]any{"_meta": map[string]any{extension: map[string]any{"agentTurns": true}}}
}

func TestInitializeAdvertisesV1AndExtensions(t *testing.T) {
	c := serve(t, newServer())
	result := c.call("initialize", map[string]any{"protocolVersion": 7})
	if result["protocolVersion"] != float64(1) {
		t.Fatalf("protocol version = %v", result["protocolVersion"])
	}
	caps := result["agentCapabilities"].(map[string]any)
	if caps["loadSession"] != true {
		t.Fatalf("capabilities = %v", caps)
	}
	ext := caps["_meta"].(map[string]any)[extension].(map[string]any)
	if ext["steer"] != true || ext["jobs"] != true || ext["agentTurns"] != true {
		t.Fatalf("extensions = %v", ext)
	}
}

func TestPromptStreamsUpdatesBeforeItsResponse(t *testing.T) {
	r := newFakeRuntime(t)
	r.run = func(_ context.Context, _ agent.Prompt, _ *agent.Inbox, emit func(agent.Event)) error {
		emit(agent.Event{Kind: agent.EventThinking, Text: "hmm"})
		emit(agent.Event{Kind: agent.EventText, Text: "hello"})
		emit(agent.Event{Kind: agent.EventUsage, Tokens: 400})
		return nil
	}
	c := serve(t, newServer(r))
	id := c.open(nil)
	if id != r.id.String() {
		t.Fatalf("session id = %q, want %q", id, r.id)
	}
	resp, before := c.until(c.request("session/prompt", map[string]any{"sessionId": id, "prompt": []any{map[string]any{"type": "text", "text": "hi"}}}))
	if got := resp["result"].(map[string]any)["stopReason"]; got != "end_turn" {
		t.Fatalf("stop reason = %v", got)
	}
	want := []string{"agent_thought_chunk:hmm", "agent_message_chunk:hello", "usage_update"}
	if got := updates(before); strings.Join(got, ",") != strings.Join(want, ",") {
		t.Fatalf("updates = %v, want %v", got, want)
	}
	if r.prompts[0].Text != "hi" {
		t.Fatalf("prompt = %#v", r.prompts[0])
	}
}

func TestFailedTurnAnswersWithAnError(t *testing.T) {
	r := newFakeRuntime(t)
	r.run = func(context.Context, agent.Prompt, *agent.Inbox, func(agent.Event)) error {
		return errors.New("provider said no")
	}
	c := serve(t, newServer(r))
	id := c.open(nil)
	resp, _ := c.until(c.request("session/prompt", map[string]any{"sessionId": id, "prompt": []any{map[string]any{"type": "text", "text": "hi"}}}))
	failure, _ := resp["error"].(map[string]any)
	if failure == nil || failure["message"] != "provider said no" {
		t.Fatalf("response = %v", resp)
	}
}

// blockingRun runs until its context ends, reporting each start on started.
func blockingRun(started chan<- string) func(context.Context, agent.Prompt, *agent.Inbox, func(agent.Event)) error {
	return func(ctx context.Context, prompt agent.Prompt, _ *agent.Inbox, _ func(agent.Event)) error {
		started <- prompt.Text
		<-ctx.Done()
		return ctx.Err()
	}
}

func TestPromptsQueueAndCancelEndsThemAll(t *testing.T) {
	r := newFakeRuntime(t)
	started := make(chan string, 4)
	r.run = blockingRun(started)
	c := serve(t, newServer(r))
	id := c.open(nil)
	first := c.request("session/prompt", map[string]any{"sessionId": id, "prompt": []any{map[string]any{"type": "text", "text": "one"}}})
	if got := <-started; got != "one" {
		t.Fatalf("started %q", got)
	}
	second := c.request("session/prompt", map[string]any{"sessionId": id, "prompt": []any{map[string]any{"type": "text", "text": "two"}}})
	c.notify("session/cancel", map[string]any{"sessionId": id})
	// The waiting prompt may be answered first.
	stops := map[float64]any{}
	for len(stops) < 2 {
		if msg := c.next(); msg["method"] == nil {
			stops[msg["id"].(float64)] = msg["result"].(map[string]any)["stopReason"]
		}
	}
	if stops[first] != "cancelled" || stops[second] != "cancelled" {
		t.Fatalf("stop reasons = %v", stops)
	}
	select {
	case got := <-started:
		t.Fatalf("cancelled prompt %q ran", got)
	default:
	}
}

func TestQueuedPromptRunsAfterTheTurnAhead(t *testing.T) {
	r := newFakeRuntime(t)
	release := make(chan struct{})
	r.run = func(_ context.Context, prompt agent.Prompt, _ *agent.Inbox, emit func(agent.Event)) error {
		if prompt.Text == "one" {
			<-release
		}
		emit(agent.Event{Kind: agent.EventText, Text: prompt.Text})
		return nil
	}
	c := serve(t, newServer(r))
	id := c.open(nil)
	first := c.request("session/prompt", map[string]any{"sessionId": id, "prompt": []any{map[string]any{"type": "text", "text": "one"}}})
	second := c.request("session/prompt", map[string]any{"sessionId": id, "prompt": []any{map[string]any{"type": "text", "text": "two"}}})
	close(release)
	_, before := c.until(first)
	if got := updates(before); len(got) != 1 || got[0] != "agent_message_chunk:one" {
		t.Fatalf("first turn updates = %v", got)
	}
	_, before = c.until(second)
	if got := updates(before); len(got) != 1 || got[0] != "agent_message_chunk:two" {
		t.Fatalf("second turn updates = %v", got)
	}
}

func TestSecondCancelEscalatesToInterrupt(t *testing.T) {
	r := newFakeRuntime(t)
	started := make(chan string, 1)
	cancelled := make(chan struct{})
	r.run = func(ctx context.Context, _ agent.Prompt, _ *agent.Inbox, _ func(agent.Event)) error {
		started <- ""
		<-ctx.Done()
		<-cancelled
		return ctx.Err()
	}
	c := serve(t, newServer(r))
	id := c.open(nil)
	prompt := c.request("session/prompt", map[string]any{"sessionId": id, "prompt": []any{map[string]any{"type": "text", "text": "one"}}})
	<-started
	c.notify("session/cancel", map[string]any{"sessionId": id})
	c.notify("session/cancel", map[string]any{"sessionId": id})
	// A request answered after both notifications proves kon has read them.
	c.call("_"+extension+"/jobs", map[string]any{"sessionId": id})
	close(cancelled)
	c.until(prompt)
	r.mu.Lock()
	defer r.mu.Unlock()
	if len(r.interrupts) != 1 || r.interrupts[0] != 2 {
		t.Fatalf("interrupts = %v", r.interrupts)
	}
}

func TestCompactCommandCompactsInsteadOfPrompting(t *testing.T) {
	r := newFakeRuntime(t)
	c := serve(t, newServer(r))
	id := c.open(nil)
	_, before := c.until(c.request("session/prompt", map[string]any{"sessionId": id, "prompt": []any{map[string]any{"type": "text", "text": "/compact"}}}))
	if r.compacted != 1 || len(r.prompts) != 0 {
		t.Fatalf("compacted %d times, prompted %d", r.compacted, len(r.prompts))
	}
	if got := updates(before); len(got) != 1 || !strings.HasPrefix(got[0], "agent_message_chunk:Compacted") {
		t.Fatalf("updates = %v", got)
	}
}

func TestIdleNoticeStartsATurnForAnOptedInClient(t *testing.T) {
	r := newFakeRuntime(t)
	c := serve(t, newServer(r))
	c.open(optIn())
	r.notices <- "job 1 exited"
	var got []string
	for len(got) == 0 || got[len(got)-1] != "_"+extension+"/turn_end" {
		got = append(got, updates([]map[string]any{c.next()})...)
	}
	want := []string{"_" + extension + "/turn_start", "user_message_chunk:job 1 exited", "agent_message_chunk:ok", "_" + extension + "/turn_end"}
	if strings.Join(got, ",") != strings.Join(want, ",") {
		t.Fatalf("messages = %v, want %v", got, want)
	}
	if r.prompts[0].Text != "job 1 exited" {
		t.Fatalf("prompt = %q", r.prompts[0].Text)
	}
}

func TestIdleNoticeWaitsForTheNextPromptWithoutOptIn(t *testing.T) {
	r := newFakeRuntime(t)
	delivered := make(chan []string, 1)
	r.run = func(_ context.Context, _ agent.Prompt, inbox *agent.Inbox, _ func(agent.Event)) error {
		delivered <- inbox.Take()
		return nil
	}
	c := serve(t, newServer(r))
	id := c.open(nil)
	// Notices are handled one at a time, so once the second is received the
	// first has been placed.
	r.notices <- "job 1 exited"
	r.notices <- "job 2 exited"
	c.call("session/prompt", map[string]any{"sessionId": id, "prompt": []any{map[string]any{"type": "text", "text": "hi"}}})
	if got := <-delivered; len(got) == 0 || got[0] != "job 1 exited" {
		t.Fatalf("delivered = %q", got)
	}
}

func TestSteerReachesTheRunningTurnAndWithdrawTakesItBack(t *testing.T) {
	r := newFakeRuntime(t)
	started := make(chan string, 1)
	r.run = blockingRun(started)
	c := serve(t, newServer(r))
	id := c.open(nil)
	resp, _ := c.until(c.request("_"+extension+"/steer", map[string]any{"sessionId": id, "text": "early"}))
	if resp["error"] == nil {
		t.Fatal("steering an idle session succeeded")
	}
	prompt := c.request("session/prompt", map[string]any{"sessionId": id, "prompt": []any{map[string]any{"type": "text", "text": "one"}}})
	<-started
	c.call("_"+extension+"/steer", map[string]any{"sessionId": id, "text": "use the helper"})
	withdrawn := c.call("_"+extension+"/withdraw", map[string]any{"sessionId": id})["withdrawn"].([]any)
	if len(withdrawn) != 1 || withdrawn[0] != "use the helper" {
		t.Fatalf("withdrawn = %v", withdrawn)
	}
	c.notify("session/cancel", map[string]any{"sessionId": id})
	c.until(prompt)
}

func TestLeftoverSteeringRunsAsTheNextTurn(t *testing.T) {
	r := newFakeRuntime(t)
	started := make(chan string, 2)
	r.run = func(ctx context.Context, prompt agent.Prompt, _ *agent.Inbox, _ func(agent.Event)) error {
		started <- prompt.Text
		if prompt.Text == "one" {
			<-ctx.Done()
			return ctx.Err()
		}
		return nil
	}
	c := serve(t, newServer(r))
	id := c.open(optIn())
	prompt := c.request("session/prompt", map[string]any{"sessionId": id, "prompt": []any{map[string]any{"type": "text", "text": "one"}}})
	<-started
	c.call("_"+extension+"/steer", map[string]any{"sessionId": id, "text": "actually, two"})
	c.notify("session/cancel", map[string]any{"sessionId": id})
	c.until(prompt)
	if got := <-started; got != "actually, two" {
		t.Fatalf("next turn = %q", got)
	}
}

func TestSetConfigOptionSwitchesModelAndEffort(t *testing.T) {
	r := newFakeRuntime(t)
	c := serve(t, newServer(r))
	id := c.open(nil)
	c.call("session/set_config_option", map[string]any{"sessionId": id, "configId": "model", "value": "other"})
	c.call("session/set_config_option", map[string]any{"sessionId": id, "configId": "effort", "value": "default"})
	if r.model != "other" || r.effort != "" {
		t.Fatalf("model = %q, effort = %q", r.model, r.effort)
	}
	resp, _ := c.until(c.request("session/set_config_option", map[string]any{"sessionId": id, "configId": "mode", "value": "x"}))
	if resp["error"] == nil {
		t.Fatal("unknown option accepted")
	}
}

func TestLoadReplaysTheConversation(t *testing.T) {
	r := newFakeRuntime(t)
	call := session.Message{Role: session.RoleAssistant, Parts: []session.Part{
		{Type: session.PartReasoning, Text: "thinking"},
		{Type: session.PartText, Text: "reading"},
		{Type: session.PartToolCall, ToolCallID: "call-1", ToolName: "read", ToolInput: json.RawMessage(`{"path":"a.go"}`)},
	}}
	result := session.ToolResultMessage("call-1", "read", "package a")
	r.history = []session.Entry{
		{Message: &session.Message{Role: session.RoleSystem, Parts: []session.Part{{Type: session.PartText, Text: "system"}}}},
		{Message: &session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: "look"}}}},
		{Message: &call},
		{Message: &result},
	}
	c := serve(t, newServer(r))
	c.call("initialize", map[string]any{"protocolVersion": 1})
	resp, before := c.until(c.request("session/load", map[string]any{"sessionId": r.id.String(), "cwd": "/work", "mcpServers": []any{}}))
	if resp["error"] != nil {
		t.Fatal(resp["error"])
	}
	want := []string{"user_message_chunk:look", "agent_thought_chunk:thinking", "agent_message_chunk:reading", "tool_call", "tool_call_update"}
	if got := updates(before); strings.Join(got, ",") != strings.Join(want, ",") {
		t.Fatalf("replay = %v, want %v", got, want)
	}
	started := before[3]["params"].(map[string]any)["update"].(map[string]any)
	if started["title"] != "read a.go" || started["kind"] != "read" || started["locations"].([]any)[0].(map[string]any)["path"] != "/work/a.go" {
		t.Fatalf("tool call = %v", started)
	}
}

func TestLoadRefusesASessionAnotherKonHasOpen(t *testing.T) {
	r := newFakeRuntime(t)
	r.state.Phase = app.PhaseFollowing
	c := serve(t, newServer(r))
	resp, _ := c.until(c.request("session/load", map[string]any{"sessionId": r.id.String(), "cwd": "/work", "mcpServers": []any{}}))
	if resp["error"] == nil || !r.closed {
		t.Fatalf("response = %v, closed = %v", resp, r.closed)
	}
}

func TestListLeavesOutSubagentsAndPages(t *testing.T) {
	server := newServer()
	var summaries []sessions.Summary
	for i := range listPage + 2 {
		id, _ := typedid.NewSessionID()
		summary := sessions.Summary{ID: id, CWD: "/work", Title: fmt.Sprint(i)}
		if i == 0 {
			summary.Parent = id
		}
		summaries = append(summaries, summary)
	}
	server.List = func(cwd string) ([]sessions.Summary, error) {
		if cwd != "/work" {
			t.Errorf("listed %q", cwd)
		}
		return summaries, nil
	}
	c := serve(t, server)
	first := c.call("session/list", map[string]any{})
	if got := len(first["sessions"].([]any)); got != listPage || first["nextCursor"] == nil {
		t.Fatalf("first page: %d sessions, cursor %v", got, first["nextCursor"])
	}
	if title := first["sessions"].([]any)[0].(map[string]any)["title"]; title != "1" {
		t.Fatalf("first listed = %v", title)
	}
	second := c.call("session/list", map[string]any{"cursor": first["nextCursor"]})
	if got := len(second["sessions"].([]any)); got != 1 || second["nextCursor"] != nil {
		t.Fatalf("second page: %d sessions, cursor %v", got, second["nextCursor"])
	}
}

func TestUnknownMethodsAndBadJSON(t *testing.T) {
	c := serve(t, newServer())
	resp, _ := c.until(c.request("session/set_mode", map[string]any{}))
	if code := resp["error"].(map[string]any)["code"]; code != float64(codeMethodNotFound) {
		t.Fatalf("code = %v", code)
	}
	if _, err := c.in.Write([]byte("{not json\n")); err != nil {
		t.Fatal(err)
	}
	if code := c.next()["error"].(map[string]any)["code"]; code != float64(codeParseError) {
		t.Fatalf("code = %v", code)
	}
}

func TestClosingStdinClosesSessions(t *testing.T) {
	r := newFakeRuntime(t)
	c := serve(t, newServer(r))
	c.open(nil)
	c.in.Close()
	if err := <-c.done; err != nil {
		t.Fatal(err)
	}
	c.done <- nil
	if !r.closed {
		t.Fatal("session left open")
	}
}
