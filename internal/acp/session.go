package acp

import (
	"context"
	"encoding/json"
	"errors"
	"strings"
	"sync"

	"kon.kitsu.red/core/acp"
	"kon.kitsu.red/core/agent"
	"kon.kitsu.red/internal/app"
)

// liveSession is one open ACP session: a runtime, the inbox its turns share,
// and the queue that runs them one at a time.
type liveSession struct {
	id      string
	cwd     string
	conn    *conn
	runtime Runtime
	// inbox carries steering and notices to the running turn. One inbox
	// serves every turn, so what arrives too late for one is delivered in
	// the next.
	inbox *agent.Inbox
	// ctx ends when the session closes, which cancels every turn.
	ctx    context.Context
	cancel context.CancelFunc

	mu sync.Mutex
	// running is the turn that holds the runtime, and waiting the prompts
	// queued behind it, oldest first.
	running *turn
	waiting []*turn
	closed  bool
}

// turn is one place in a session's queue: a prompt's or one kon started.
type turn struct {
	ctx    context.Context
	cancel context.CancelFunc
	// ready is closed when the turn may run, and granted then set, under the
	// session's lock.
	ready   chan struct{}
	granted bool
	// presses counts cancels of the running turn: the first cancels its
	// context, and later ones escalate to killing a command that ignored it.
	presses int
}

func newLiveSession(c *conn, runtime Runtime, cwd string) *liveSession {
	ctx, cancel := context.WithCancel(context.Background())
	return &liveSession{id: runtime.LiveID().String(), cwd: cwd, conn: c, runtime: runtime, inbox: &agent.Inbox{}, ctx: ctx, cancel: cancel}
}

func (s *liveSession) newTurn() *turn {
	ctx, cancel := context.WithCancel(s.ctx)
	return &turn{ctx: ctx, cancel: cancel, ready: make(chan struct{})}
}

// grant hands the runtime to t. The caller holds s.mu.
func (s *liveSession) grant(t *turn) {
	t.granted = true
	s.running = t
	close(t.ready)
}

// enqueue places a turn at the back of the queue, running it at once when
// the session is idle.
func (s *liveSession) enqueue() (*turn, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.closed {
		return nil, app.ErrClosed
	}
	t := s.newTurn()
	if s.running == nil && len(s.waiting) == 0 {
		s.grant(t)
	} else {
		s.waiting = append(s.waiting, t)
	}
	return t, nil
}

// wait blocks until t may run, and reports false when it was cancelled
// first and so left the queue. A turn granted as it was cancelled still owns
// the runtime and must finish.
func (s *liveSession) wait(t *turn) bool {
	select {
	case <-t.ready:
		return true
	case <-t.ctx.Done():
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	if t.granted {
		return true
	}
	for i, w := range s.waiting {
		if w == t {
			s.waiting = append(s.waiting[:i], s.waiting[i+1:]...)
			break
		}
	}
	return false
}

// finish releases the runtime to the next prompt in line. With none waiting,
// what is left in the inbox becomes a turn of kon's own, for a client that
// accepts one, after a turn that ended cleanly or was cancelled: a failure
// would only meet the same error again.
func (s *liveSession) finish(t *turn, startNext bool) {
	t.cancel()
	s.mu.Lock()
	s.running = nil
	if len(s.waiting) > 0 {
		next := s.waiting[0]
		s.waiting = s.waiting[1:]
		s.grant(next)
		s.mu.Unlock()
		return
	}
	var text string
	var next *turn
	if startNext && !s.closed && s.conn.agentTurns.Load() {
		if pending := s.inbox.Take(); len(pending) > 0 {
			text, next = strings.Join(pending, "\n\n"), s.newTurn()
			s.grant(next)
		}
	}
	s.mu.Unlock()
	if next != nil {
		s.goAgentTurn(next, text)
	}
}

// cancelTurns cancels every waiting prompt and the running turn. Cancelling a
// turn that is already cancelling escalates, as a second Esc does.
func (s *liveSession) cancelTurns() {
	s.mu.Lock()
	for _, w := range s.waiting {
		w.cancel()
	}
	presses := 0
	if t := s.running; t != nil {
		t.presses++
		presses = t.presses
		t.cancel()
	}
	s.mu.Unlock()
	if presses > 1 {
		s.runtime.Interrupt(presses)
	}
}

// close cancels every turn and closes the runtime, which waits for the
// running one and stops the session's background jobs.
func (s *liveSession) close() error {
	s.mu.Lock()
	if s.closed {
		s.mu.Unlock()
		return nil
	}
	s.closed = true
	s.mu.Unlock()
	s.cancel()
	return s.runtime.Close()
}

// listen delivers the runtime's notices until the session closes.
func (s *liveSession) listen() {
	for {
		select {
		case text := <-s.runtime.Notices():
			s.notice(text)
		case <-s.ctx.Done():
			return
		}
	}
}

// notice passes a notice to the agent: to the running turn, or as a turn of
// its own when the session is idle and the client accepts one. Otherwise it
// waits in the inbox for the next prompt's turn.
func (s *liveSession) notice(text string) {
	s.mu.Lock()
	if s.closed || s.running != nil || len(s.waiting) > 0 || !s.conn.agentTurns.Load() {
		s.inbox.PushNotice(text)
		s.mu.Unlock()
		return
	}
	t := s.newTurn()
	s.grant(t)
	s.mu.Unlock()
	s.goAgentTurn(t, text)
}

func (s *liveSession) goAgentTurn(t *turn, text string) {
	s.conn.handlers.Add(1)
	go func() {
		defer s.conn.handlers.Done()
		s.agentTurn(t, text)
	}()
}

// agentTurn runs a turn kon started, bracketed by the extension's turn
// notifications.
func (s *liveSession) agentTurn(t *turn, text string) {
	s.conn.out.notify(acp.MethodTurnStart, acp.SessionParams{SessionID: s.id})
	s.update(acp.ContentChunk{SessionUpdate: acp.UpdateUserMessage, Content: acp.TextBlock(text)})
	stop, err := s.run(t, agent.Prompt{Text: text})
	end := acp.TurnEnd{SessionID: s.id, StopReason: stop}
	if err != nil {
		end = acp.TurnEnd{SessionID: s.id, Error: err.Error()}
	}
	s.conn.out.notify(acp.MethodTurnEnd, end)
	s.finish(t, err == nil)
}

// compactCommand is the prompt that compacts instead of reaching the model,
// as /compact does in the full-screen UI.
const compactCommand = "/compact"

// prompt answers session/prompt. It joins the queue before returning, so
// dispatch keeps prompts in the order they arrived, and answers once the
// turn has run.
func (c *conn) prompt(id json.RawMessage, params json.RawMessage) {
	var req acp.PromptRequest
	if err := decode(params, &req); err != nil {
		c.out.respond(id, nil, err)
		return
	}
	s, err := c.openSession(req.SessionID)
	if err != nil {
		c.out.respond(id, nil, err)
		return
	}
	prompt, err := s.convertPrompt(req.Prompt)
	if err != nil {
		c.out.respond(id, nil, err)
		return
	}
	t, err := s.enqueue()
	if err != nil {
		c.out.respond(id, nil, err)
		return
	}
	c.handlers.Add(1)
	go func() {
		defer c.handlers.Done()
		if !s.wait(t) {
			c.out.respond(id, acp.PromptResponse{StopReason: acp.StopCancelled}, nil)
			return
		}
		stop, err := s.run(t, prompt)
		c.out.respond(id, acp.PromptResponse{StopReason: stop}, err)
		s.finish(t, err == nil)
	}()
}

// run runs a granted turn and says how it stopped. A cancelled turn is not an
// error: the spec asks for the cancelled stop reason instead.
func (s *liveSession) run(t *turn, prompt agent.Prompt) (string, error) {
	if t.ctx.Err() != nil {
		return acp.StopCancelled, nil
	}
	tr := s.newTranslator()
	var err error
	if strings.TrimSpace(prompt.Text) == compactCommand && len(prompt.Media) == 0 {
		err = s.runtime.Compact(t.ctx, tr.event)
		switch {
		case errors.Is(err, agent.ErrNothingToCompact):
			s.say("Nothing to compact.")
			err = nil
		case err == nil:
			s.say("Compacted the conversation.")
		}
	} else {
		err = s.runtime.RunPrompt(t.ctx, prompt, s.inbox, tr.event)
	}
	if t.ctx.Err() != nil && (err == nil || errors.Is(err, context.Canceled)) {
		return acp.StopCancelled, nil
	}
	if err != nil {
		return "", err
	}
	return acp.StopEndTurn, nil
}

// say adds a message of kon's own to the transcript, such as the outcome of
// a command.
func (s *liveSession) say(text string) {
	s.update(acp.ContentChunk{SessionUpdate: acp.UpdateAgentMessage, Content: acp.TextBlock(text)})
}

func (s *liveSession) update(u any) {
	s.conn.out.notify("session/update", acp.SessionNotification{SessionID: s.id, Update: u})
}

// announce sends what a client shows for a session it has just opened: the
// commands it accepts and how full its context is.
func (s *liveSession) announce() {
	s.update(acp.CommandsUpdate{SessionUpdate: acp.UpdateCommands, AvailableCommands: []acp.Command{
		{Name: strings.TrimPrefix(compactCommand, "/"), Description: "summarize older context now"},
	}})
	if used, _ := s.runtime.ContextUsage(); used > 0 {
		s.reportUsage(int64(used))
	}
}

// reportUsage sends the context size the provider last reported, against the
// model's window, with the session's spend so far. Without a known window
// there is nothing to measure it against.
func (s *liveSession) reportUsage(used int64) {
	window := s.runtime.State().Active.ContextWindow
	if window <= 0 {
		return
	}
	u := acp.UsageUpdate{SessionUpdate: acp.UpdateUsage, Used: used, Size: int64(window)}
	if spent := s.spend(); spent > 0 {
		u.Cost = &acp.Cost{Amount: spent, Currency: "USD"}
	}
	s.update(u)
}

func (s *liveSession) setConfig(configID, value string) error {
	switch configID {
	case acp.ConfigModel:
		return s.runtime.SwitchModel(value)
	case acp.ConfigEffort:
		if value == acp.EffortDefault {
			value = ""
		}
		return s.runtime.SetEffort(value)
	}
	return &acp.Error{Code: acp.CodeInvalidParams, Message: "unknown config option: " + configID}
}

func (c *conn) setConfigOption(params json.RawMessage) (any, error) {
	var req acp.SetConfigRequest
	if err := decode(params, &req); err != nil {
		return nil, err
	}
	s, err := c.openSession(req.SessionID)
	if err != nil {
		return nil, err
	}
	if err := s.setConfig(req.ConfigID, req.Value); err != nil {
		return nil, err
	}
	return acp.SetConfigResponse{ConfigOptions: s.configOptions()}, nil
}

func (c *conn) steer(params json.RawMessage) (any, error) {
	var req acp.SteerRequest
	if err := decode(params, &req); err != nil {
		return nil, err
	}
	s, err := c.openSession(req.SessionID)
	if err != nil {
		return nil, err
	}
	if strings.TrimSpace(req.Text) == "" {
		return nil, &acp.Error{Code: acp.CodeInvalidParams, Message: "steering needs text"}
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.running == nil {
		return nil, &acp.Error{Code: acp.CodeInvalidRequest, Message: "no turn is running; send session/prompt instead"}
	}
	s.inbox.Push(req.Text)
	return nil, nil
}

func (c *conn) withdraw(params json.RawMessage) (any, error) {
	var req acp.SessionParams
	if err := decode(params, &req); err != nil {
		return nil, err
	}
	s, err := c.openSession(req.SessionID)
	if err != nil {
		return nil, err
	}
	// Taken one at a time, so a message the runner takes meanwhile is
	// delivered rather than reported withdrawn.
	withdrawn := []string{}
	for {
		text, ok := s.inbox.Remove(0)
		if !ok {
			break
		}
		withdrawn = append(withdrawn, text)
	}
	return acp.WithdrawResponse{Withdrawn: withdrawn}, nil
}

func (c *conn) jobs(params json.RawMessage) (any, error) {
	var req acp.SessionParams
	if err := decode(params, &req); err != nil {
		return nil, err
	}
	s, err := c.openSession(req.SessionID)
	if err != nil {
		return nil, err
	}
	resp := acp.JobsResponse{Jobs: []acp.Job{}}
	for _, j := range s.runtime.Jobs() {
		resp.Jobs = append(resp.Jobs, acp.Job{ID: j.ID, Command: j.Command, Running: j.Exit == "", Exit: j.Exit, Output: j.Output, SubagentSessionID: j.Session})
	}
	return resp, nil
}

func (c *conn) killJob(params json.RawMessage) (any, error) {
	var req acp.KillJobRequest
	if err := decode(params, &req); err != nil {
		return nil, err
	}
	s, err := c.openSession(req.SessionID)
	if err != nil {
		return nil, err
	}
	return nil, s.runtime.KillJob(req.ID)
}
