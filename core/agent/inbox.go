package agent

import (
	"slices"
	"sync"
)

// Inbox holds messages for the running agent that arrive while a run is in
// flight: steering the user sends, and notices from kon such as a background
// job exiting. The frontend pushes on its own goroutine and the runner drains
// at its next request, so every method is safe to call concurrently. A nil
// Inbox is empty, which is what a run with no interactive user (kon run)
// passes.
type Inbox struct {
	mu      sync.Mutex
	pending []inboxMessage
}

type inboxMessage struct {
	text string
	// notice marks a message from kon rather than the user. Notices are
	// delivered like steering but are not the user's to withdraw.
	notice bool
}

// Push adds user steering to be delivered at the runner's next request.
func (b *Inbox) Push(text string) { b.push(inboxMessage{text: text}) }

// PushNotice adds a notice from kon to be delivered at the runner's next
// request.
func (b *Inbox) PushNotice(text string) { b.push(inboxMessage{text: text, notice: true}) }

func (b *Inbox) push(m inboxMessage) {
	b.mu.Lock()
	defer b.mu.Unlock()
	b.pending = append(b.pending, m)
}

// Take removes and returns every pending message, oldest first.
func (b *Inbox) Take() []string {
	if b == nil {
		return nil
	}
	b.mu.Lock()
	defer b.mu.Unlock()
	taken := make([]string, len(b.pending))
	for i, m := range b.pending {
		taken[i] = m.text
	}
	b.pending = nil
	return taken
}

// Pending returns the user's steering not yet delivered. Notices are left out:
// they are kon's, and the user cannot withdraw them.
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
