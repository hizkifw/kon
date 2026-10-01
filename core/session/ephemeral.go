package session

import (
	"fmt"
	"os"
	"path/filepath"
	"time"

	"github.com/hizkifw/kon/core/typedid"
)

// NewEphemeral creates a session that lives only in memory, for an incognito
// launch. It has no file among the persisted sessions, so it can never be
// discovered or resumed, and Close discards it. Background jobs still need
// real files, because the agent reads their output with ordinary commands, so
// they get a private temporary directory that Close removes.
func NewEphemeral(cwd, appVersion, systemPrompt string) (*Store, error) {
	absCWD, err := filepath.Abs(cwd)
	if err != nil {
		return nil, fmt.Errorf("resolve working directory: %w", err)
	}
	sessionID, err := typedid.NewSessionID()
	if err != nil {
		return nil, err
	}
	scratch, err := os.MkdirTemp("", "kon-incognito-*")
	if err != nil {
		return nil, fmt.Errorf("create incognito jobs directory: %w", err)
	}
	s := &Store{
		header:  Header{Type: "session", Version: SchemaVersion, ID: sessionID, AppVersion: appVersion, Timestamp: time.Now().UTC(), CWD: filepath.Clean(absCWD)},
		file:    discard{},
		byID:    make(map[typedid.EntryID]int),
		empty:   true,
		images:  make(map[string][]byte),
		scratch: scratch,
	}
	if _, err := s.AppendMessage(TextMessage(RoleSystem, systemPrompt)); err != nil {
		_ = os.RemoveAll(scratch)
		return nil, err
	}
	return s, nil
}

// ephemeral reports whether the session lives only in memory; see
// NewEphemeral.
func (s *Store) ephemeral() bool { return s.path == "" }

// discard is an ephemeral session's file. Appends take the same path as a
// persisted session's, so both keep their entries identically, but the bytes
// go nowhere.
type discard struct{}

func (discard) Write(b []byte) (int, error)    { return len(b), nil }
func (discard) Seek(int64, int) (int64, error) { return 0, nil }
func (discard) Truncate(int64) error           { return nil }
func (discard) Sync() error                    { return nil }
func (discard) Close() error                   { return nil }
