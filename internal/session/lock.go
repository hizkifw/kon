package session

import (
	"errors"
	"fmt"
	"os"

	"github.com/gofrs/flock"
)

// ErrInUse reports that another kon process holds a session open for writing.
// A session has one writer at a time: each writer extends its own in-memory
// leaf, so a second one would fork the conversation silently, and repairing a
// torn tail could truncate a record the first is still writing.
var ErrInUse = errors.New("session is open in another kon")

// The lock is a separate file beside the session rather than a lock on the
// session itself, because Windows locks are mandatory and would block the
// read-only previews and discovery that must work while a session is live. The
// operating system releases it when its holder exits, so a crash leaves no
// stale lock. Lock files are left in place: unlinking one while another
// process has it open would let two processes lock different files.
func lockPath(sessionPath string) string { return sessionPath + ".lock" }

// lock takes the session's writer lock without waiting.
func lock(sessionPath string) (*flock.Flock, error) {
	l := flock.New(lockPath(sessionPath))
	ok, err := l.TryLock()
	if err != nil {
		return nil, fmt.Errorf("lock session: %w", err)
	}
	if !ok {
		return nil, ErrInUse
	}
	return l, nil
}

// InUse reports whether another process holds the session open for writing. It
// is a momentary answer for display; opening the session is what decides.
func InUse(sessionPath string) bool {
	if _, err := os.Stat(lockPath(sessionPath)); err != nil {
		return false
	}
	l := flock.New(lockPath(sessionPath))
	ok, err := l.TryRLock()
	if err != nil {
		return false
	}
	if ok {
		_ = l.Unlock()
	}
	return !ok
}
