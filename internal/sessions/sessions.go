// Package sessions is where kon keeps sessions: one directory per working
// directory, discovery for resuming, subagent usage, and previews. The
// session format itself is core/session's.
package sessions

import (
	"fmt"
	"os"
	"path/filepath"
	"time"

	"github.com/hizkifw/kon/core/session"
	"github.com/hizkifw/kon/core/typedid"
)

// fileSuffix ends every persisted session file. Names begin with a fixed-width
// UTC timestamp; lexical order approximates creation order but only to the
// millisecond, so Discover re-sorts by the header's full-precision timestamp.
const fileSuffix = ".jsonl"

// New creates a session for cwd under root, recording parent as the session
// whose agent started it as a subagent. A zero parent makes an ordinary
// session.
func New(root, cwd, appVersion, systemPrompt string, parent typedid.SessionID) (*session.Store, error) {
	header, err := newHeader(cwd, appVersion, parent)
	if err != nil {
		return nil, err
	}
	dir, err := directoryFor(root, header.CWD)
	if err != nil {
		return nil, err
	}
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return nil, fmt.Errorf("create session directory: %w", err)
	}
	name := header.Timestamp.Format("20060102T150405.000Z") + "_" + header.ID.String() + fileSuffix
	return session.Create(filepath.Join(dir, name), header, systemPrompt)
}

// NewIncognito creates a session for cwd that is kept only in memory, so it
// can never be discovered or resumed.
func NewIncognito(cwd, appVersion, systemPrompt string) (*session.Store, error) {
	header, err := newHeader(cwd, appVersion, typedid.SessionID{})
	if err != nil {
		return nil, err
	}
	return session.NewMemory(header, systemPrompt)
}

// newHeader identifies a new session up front, because its file is named for
// its ID and creation time.
func newHeader(cwd, appVersion string, parent typedid.SessionID) (session.Header, error) {
	absCWD, err := filepath.Abs(cwd)
	if err != nil {
		return session.Header{}, fmt.Errorf("resolve working directory: %w", err)
	}
	id, err := typedid.NewSessionID()
	if err != nil {
		return session.Header{}, err
	}
	return session.Header{ID: id, AppVersion: appVersion, Timestamp: time.Now().UTC(), CWD: filepath.Clean(absCWD), Parent: parent}, nil
}

// JobsDir is where a persisted session's background jobs keep their files,
// beside the session file. An incognito session has none.
func JobsDir(store *session.Store) string {
	if store.Path() == "" {
		return ""
	}
	return store.Path() + ".jobs"
}
