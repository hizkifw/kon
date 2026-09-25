package app

import (
	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/typedid"
)

// openView opens a session another kon has open, to follow it read-only. The
// header names the model that session last recorded, which is the one its
// writer is running.
func (r *Runtime) openView(path string) (opened, error) {
	view, err := session.OpenView(path)
	if err != nil {
		return opened{}, err
	}
	return opened{view: view, profile: r.recordedProfile(lastModelChange(view.ActivePath()))}, nil
}

// Followed is what a followed session gained since the last look.
type Followed struct {
	// Session is the session the entries belong to, so a caller can drop an
	// answer that arrives after it moved to another session.
	Session typedid.SessionID
	Entries []session.Entry
	// Free reports that the other kon has let go, so TakeOver would succeed.
	Free bool
}

// Follow reads what the writer of a followed session has completed since the
// last call. Outside the following phase it returns an empty result.
func (r *Runtime) Follow() (Followed, error) {
	r.mu.Lock()
	defer r.mu.Unlock()
	if r.phase != PhaseFollowing {
		return Followed{}, nil
	}
	entries, err := r.view.Poll()
	return Followed{Session: r.view.ID(), Entries: entries, Free: r.view.Free()}, err
}

// TakeOver makes this runtime the writer of the session it follows, once the
// other kon has let go. It returns the entries completed since the last
// Follow, including any model change recorded on taking over, so the caller's
// transcript is whole before it writes. While the session is still held it
// returns session.ErrInUse and keeps following.
func (r *Runtime) TakeOver() ([]session.Entry, error) {
	r.mu.Lock()
	defer r.mu.Unlock()
	if r.phase != PhaseFollowing {
		return nil, nil
	}
	target, err := r.openStore(r.view.Path())
	if err != nil {
		return nil, err
	}
	if target.view != nil {
		return nil, session.ErrInUse
	}
	// The store now holds the lock, so nothing else is being written: this
	// read ends exactly where the store begins.
	missed, err := r.view.Poll()
	if err != nil {
		_ = r.closeStore(target.store)
		return nil, err
	}
	r.install(target)
	return missed, nil
}
