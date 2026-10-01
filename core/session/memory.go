package session

import (
	"github.com/hizkifw/kon/core/typedid"
)

// NewMemory creates a session that lives only in memory: the same store and
// append path as a persisted session, with nothing written anywhere. Close
// discards it. header is completed as Create completes it.
func NewMemory(header Header, systemPrompt string) (*Store, error) {
	header, err := completeHeader(header)
	if err != nil {
		return nil, err
	}
	s := &Store{
		header: header,
		file:   discard{},
		byID:   make(map[typedid.EntryID]int),
		empty:  true,
		images: make(map[string][]byte),
	}
	if _, err := s.AppendMessage(TextMessage(RoleSystem, systemPrompt)); err != nil {
		return nil, err
	}
	return s, nil
}

// ephemeral reports whether the session lives only in memory; see
// NewMemory.
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
