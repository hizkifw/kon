package agent

import (
	"slices"
	"sync"
)

// Inbox holds steering messages the user sends while a run is in flight. The
// UI pushes on its own goroutine and the runner drains at its next request, so
// every method is safe to call concurrently. A nil Inbox is empty, which is
// what a run with no interactive user (kon run) passes.
type Inbox struct {
	mu      sync.Mutex
	pending []string
}

// Push adds a message to be delivered at the runner's next request.
func (b *Inbox) Push(text string) {
	b.mu.Lock()
	defer b.mu.Unlock()
	b.pending = append(b.pending, text)
}

// Take removes and returns every pending message, oldest first.
func (b *Inbox) Take() []string {
	if b == nil {
		return nil
	}
	b.mu.Lock()
	defer b.mu.Unlock()
	taken := b.pending
	b.pending = nil
	return taken
}

// Pending returns a copy of the messages not yet delivered.
func (b *Inbox) Pending() []string {
	if b == nil {
		return nil
	}
	b.mu.Lock()
	defer b.mu.Unlock()
	return slices.Clone(b.pending)
}

// Remove withdraws the pending message at index i. It reports false when the
// runner has already taken it, which the caller cannot rule out in advance.
func (b *Inbox) Remove(i int) (string, bool) {
	b.mu.Lock()
	defer b.mu.Unlock()
	if i < 0 || i >= len(b.pending) {
		return "", false
	}
	text := b.pending[i]
	b.pending = slices.Delete(b.pending, i, i+1)
	return text, true
}
