package agent

import (
	"encoding/json"
	"slices"
	"sync"

	"kon.kitsu.red/core/tool"
)

// Inbox holds messages for the running agent that arrive while a run is in
// flight: steering the user sends, and notices from the program itself, such
// as a background job exiting. The program pushes on its own goroutine and the
// runner drains at its next request, so every method is safe to call
// concurrently. A nil Inbox is empty, which suits a run with no interactive
// user.
type Inbox struct {
	mu      sync.Mutex
	pending []inboxMessage
}

type inboxMessage struct {
	text string
	// notice marks a message from kon rather than the user. Notices are
	// delivered like steering but are not the user's to withdraw.
	notice bool
	// call, when set, is a tool call the user makes; text is how it is shown
	// while it waits.
	call *Call
}

// Call is a tool call the user makes through the inbox, such as a shell
// command typed while the agent works. The runner makes it when it delivers
// the inbox, so it never runs beside the agent's own calls.
type Call struct {
	Tool      string
	Arguments json.RawMessage
	// Message turns the call's result into the text the model receives.
	Message func(tool.Result) string
}

// Push adds user steering to be delivered at the runner's next request.
func (b *Inbox) Push(text string) { b.push(inboxMessage{text: text}) }

// PushCall adds a tool call the user makes, shown as text while it waits, to
// be made and delivered at the runner's next request.
func (b *Inbox) PushCall(text string, call Call) { b.push(inboxMessage{text: text, call: &call}) }

// PushNotice adds a notice from the program to be delivered at the runner's next
// request.
func (b *Inbox) PushNotice(text string) { b.push(inboxMessage{text: text, notice: true}) }

func (b *Inbox) push(m inboxMessage) {
	b.mu.Lock()
	defer b.mu.Unlock()
	b.pending = append(b.pending, m)
}

// Take removes and returns every pending message, oldest first. A call is
// returned as the text it is shown as, and is not made.
func (b *Inbox) Take() []string {
	var texts []string
	for _, m := range b.take() {
		texts = append(texts, m.text)
	}
	return texts
}

func (b *Inbox) take() []inboxMessage {
	if b == nil {
		return nil
	}
	b.mu.Lock()
	defer b.mu.Unlock()
	taken := b.pending
	b.pending = nil
	return taken
}

// restore puts messages the runner took but could not deliver back ahead of
// any that arrived since.
func (b *Inbox) restore(ms []inboxMessage) {
	if b == nil || len(ms) == 0 {
		return
	}
	b.mu.Lock()
	defer b.mu.Unlock()
	b.pending = append(slices.Clone(ms), b.pending...)
}

// Pending returns the user's steering not yet delivered. Notices are left out:
// they are the program's, and the user cannot withdraw them.
func (b *Inbox) Pending() []string {
	if b == nil {
		return nil
	}
	b.mu.Lock()
	defer b.mu.Unlock()
	var steering []string
	for _, m := range b.pending {
		if !m.notice {
			steering = append(steering, m.text)
		}
	}
	return steering
}

// Remove withdraws the i-th pending steering message, counting as Pending
// does. It reports false when the runner has already taken it, which the
// caller cannot rule out in advance.
func (b *Inbox) Remove(i int) (string, bool) {
	b.mu.Lock()
	defer b.mu.Unlock()
	for at, m := range b.pending {
		if m.notice {
			continue
		}
		if i == 0 {
			b.pending = slices.Delete(b.pending, at, at+1)
			return m.text, true
		}
		i--
	}
	return "", false
}

// WithdrawAll removes every pending steering message, leaving notices to be
// delivered, and reports how many it removed.
func (b *Inbox) WithdrawAll() int {
	b.mu.Lock()
	defer b.mu.Unlock()
	kept := b.pending[:0]
	for _, m := range b.pending {
		if m.notice {
			kept = append(kept, m)
		}
	}
	withdrawn := len(b.pending) - len(kept)
	b.pending = kept
	return withdrawn
}
