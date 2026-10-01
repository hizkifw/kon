package ui

import (
	"context"
	"time"

	tea "charm.land/bubbletea/v2"
	"kon.kitsu.red/core/agent"
)

// run is one runtime operation streaming into a transcript: a turn or a
// /compact into the main one, or a /btw answer into its drawer. It times
// itself for the transcript's running marker.
type run struct {
	// epoch tells this run's messages apart from those of runs that have
	// ended, so a late event or tick from one cannot reach the next.
	epoch  int
	cancel context.CancelFunc
	events <-chan runMsg
	start  time.Time
	// verb is what the running marker says the run is doing, such as
	// Working, Compacting, or Answering.
	verb string
	// compaction marks a /compact run. Its compaction block is its record,
	// so it leaves no "Worked for" line behind.
	compaction bool
	// retry is the provider retry the run is waiting on or sending, which
	// the marker shows in place of verb until the run moves on.
	retry *retryState
}

type retryState struct {
	reason       string
	attempt, max int
	// at is when the retry is sent; the marker counts down to it.
	at time.Time
}

// runMsg is one event from a run, or its end when done is set.
type runMsg struct {
	epoch int
	event agent.Event
	done  bool
	err   error
}

// runTickMsg repaints a run's marker. The marker shows whole seconds, so one
// tick per second keeps it current.
type runTickMsg struct{ epoch int }

// startRun drives fn on a goroutine, forwarding its events as runMsgs, and
// returns the run with the command that receives them and ticks its marker.
// Every run is a child of the program's context, so every exit releases it.
func (m *Model) startRun(verb string, fn func(context.Context, func(agent.Event)) error) (*run, tea.Cmd) {
	m.runEpoch++
	epoch := m.runEpoch
	ctx, cancel := context.WithCancel(m.ctx)
	events := make(chan runMsg)
	r := &run{epoch: epoch, cancel: cancel, events: events, start: time.Now(), verb: verb}
	go func() {
		defer cancel()
		forward(ctx, epoch, events, fn)
	}()
	return r, tea.Batch(r.wait(), r.tick())
}

// forward runs fn, sending its events and then its end, and closes events.
// It stops sending once ctx is done, so a run that nobody reads any more
// still returns.
func forward(ctx context.Context, epoch int, events chan<- runMsg, fn func(context.Context, func(agent.Event)) error) {
	defer close(events)
	err := fn(ctx, func(event agent.Event) {
		select {
		case events <- runMsg{epoch: epoch, event: event}:
		case <-ctx.Done():
		}
	})
	select {
	case events <- runMsg{epoch: epoch, done: true, err: err}:
	case <-ctx.Done():
	}
}

// wait receives the run's next message. A cancelled run may close its
// channel without sending its end, which then arrives as a cancellation.
func (r *run) wait() tea.Cmd {
	events, epoch := r.events, r.epoch
	return func() tea.Msg {
		msg, ok := <-events
		if !ok {
			return runMsg{epoch: epoch, done: true, err: context.Canceled}
		}
		return msg
	}
}

func (r *run) tick() tea.Cmd {
	epoch := r.epoch
	return tea.Tick(time.Second, func(time.Time) tea.Msg { return runTickMsg{epoch: epoch} })
}

// paint shows the run's running marker in t as of now.
func (r *run) paint(t *transcript, now time.Time) {
	verb := r.verb
	if r.retry != nil {
		verb = retryVerb(r.retry, now)
	}
	t.liveTimer = runningLabel(verb, now.Sub(r.start))
}

// track follows provider retries: a retry event starts one, and any other
// event means the retried request got through.
func (r *run) track(t *transcript, event agent.Event) {
	switch {
	case event.Kind == agent.EventRetrying:
		r.retry = &retryState{reason: event.Text, attempt: event.Attempt, max: event.MaxAttempts, at: time.Now().Add(event.Delay)}
	case r.retry == nil:
		return
	default:
		r.retry = nil
	}
	r.paint(t, time.Now())
}

// setVerb changes what the marker says, repainting at once rather than on
// the next tick.
func (r *run) setVerb(t *transcript, verb string) {
	if r.verb != verb {
		r.verb = verb
		r.paint(t, time.Now())
	}
}

// liveRun finds the run an epoch belongs to, with the transcript it streams
// into, or nil once that run has ended.
func (m *Model) liveRun(epoch int) (*run, *transcript) {
	switch {
	case m.turn != nil && m.turn.epoch == epoch:
		return m.turn, &m.transcript
	case m.side != nil && m.side.run != nil && m.side.run.epoch == epoch:
		return m.side.run, &m.side.transcript
	}
	return nil, nil
}

func (m Model) updateRun(msg runMsg) (tea.Model, tea.Cmd) {
	r, _ := m.liveRun(msg.epoch)
	switch {
	case r == nil:
		return m, nil
	case r == m.turn && msg.done:
		return m.finishTurn(msg.err)
	case r == m.turn:
		return m.applyTurnEvent(msg.event)
	case msg.done:
		return m.finishSideChat(msg.err)
	}
	return m.applySideEvent(msg.event)
}

func (m Model) tickRun(msg runTickMsg) (tea.Model, tea.Cmd) {
	r, t := m.liveRun(msg.epoch)
	if r == nil {
		return m, nil
	}
	r.paint(t, time.Now())
	m.refreshTranscript(true)
	return m, r.tick()
}

// scheduleFlush coalesces streamed text into one repaint per frame rather
// than one per chunk.
func (m *Model) scheduleFlush() tea.Cmd {
	if m.flushPending {
		return nil
	}
	m.flushPending = true
	return tea.Tick(streamFrameInterval, func(time.Time) tea.Msg { return flushTranscriptMsg{} })
}
