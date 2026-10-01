package sessions

import (
	"bytes"
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"sync"

	"github.com/hizkifw/kon/core/session"
	"github.com/hizkifw/kon/core/typedid"
)

// Subagents adds up what a session's subagents have used, and their own
// subagents in turn, by reading their sessions as they are written. A
// subagent's session names the session that started it in its header, so they
// are found among the sessions of the same working directory. An incognito
// subagent writes no session and is not counted. It is safe for concurrent use.
type Subagents struct {
	mu   sync.Mutex
	dir  string
	root typedid.SessionID
	// since is the creation time the root's file name starts with. A
	// subagent's session is always created after the session that started
	// it, so a file named for an earlier time is never read.
	since string
	// family holds the root and every subagent session found beneath it.
	family map[typedid.SessionID]bool
	// checked marks the files whose header has been read, so each is
	// classified once however often the directory is listed.
	checked map[string]bool
	// followed holds each subagent session by file name.
	followed map[string]*tally
}

// tally follows one subagent session. It keeps only how far it has read and
// what the records read so far used, never the records themselves, so
// following many long sessions costs no more memory than following one.
type tally struct {
	path   string
	offset int64
	usage  session.Usage
}

// usageKey marks a record that may carry usage. Inside a string, as in a tool's
// output, its quotes would be escaped, so a record without it is skipped
// without decoding.
var usageKey = []byte(`"usage":`)

// usageRecord is the part of a session record that carries usage: an
// assistant message's, or a compaction's own.
type usageRecord struct {
	Usage   *session.Usage `json:"usage"`
	Message *struct {
		Usage *session.Usage `json:"usage"`
	} `json:"message"`
}

// read adds the usage of the records completed since the last read. A line
// without its ending is left for the next read, once the writer finishes it.
func (t *tally) read() error {
	offset, err := session.ReadLines(t.path, t.offset, func(line []byte) error {
		if !bytes.Contains(line, usageKey) {
			return nil
		}
		var record usageRecord
		if json.Unmarshal(line, &record) == nil {
			switch {
			case record.Message != nil && record.Message.Usage != nil:
				t.usage.Add(*record.Message.Usage)
			case record.Usage != nil:
				t.usage.Add(*record.Usage)
			}
		}
		return nil
	})
	t.offset = offset
	return err
}

// NewSubagents follows the subagents of the session id, whose file is at
// path. It reads nothing until the first call to Usage.
func NewSubagents(path string, id typedid.SessionID) *Subagents {
	since, _, _ := strings.Cut(filepath.Base(path), "_")
	return &Subagents{
		dir: filepath.Dir(path), root: id, since: since, family: map[typedid.SessionID]bool{id: true},
		checked: map[string]bool{}, followed: map[string]*tally{},
	}
}

// Session is the session whose subagents are counted.
func (s *Subagents) Session() typedid.SessionID { return s.root }

// Usage returns what the subagents have used so far. Each call reads only what
// their sessions appended since the last, and the sessions of subagents
// started since.
func (s *Subagents) Usage() session.Usage {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.discover()
	var total session.Usage
	for name, t := range s.followed {
		switch err := t.read(); {
		case errors.Is(err, session.ErrRemoved):
			// Only a session that never held a conversation is removed.
			delete(s.followed, name)
			continue
		case err != nil:
			// What was counted can no longer be trusted, so the session is
			// read again whole next time.
			*t = tally{path: t.path}
			continue
		}
		total.Add(t.usage)
	}
	return total
}

// discover follows each session file not seen before whose parent is in the
// family. A file name starts with the session's creation time, so sorting the
// names puts a subagent's session after the session that started it.
func (s *Subagents) discover() {
	f, err := os.Open(s.dir)
	if err != nil {
		return
	}
	names, err := f.Readdirnames(-1)
	f.Close()
	if err != nil {
		return
	}
	// Only the few files left are sorted, not the whole workspace.
	names = slices.DeleteFunc(names, func(name string) bool {
		return name < s.since || !strings.HasSuffix(name, fileSuffix) || s.checked[name]
	})
	slices.Sort(names)
	for _, name := range names {
		path := filepath.Join(s.dir, name)
		header, err := readHeader(path)
		if err != nil {
			// The header may still be being written; it is read next time.
			continue
		}
		s.checked[name] = true
		if !s.family[header.Parent] || header.Parent.IsZero() {
			continue
		}
		s.family[header.ID] = true
		s.followed[name] = &tally{path: path}
	}
}

// readHeader reads a session file's header alone. A header without its line
// ending is still being written and is an error.
func readHeader(path string) (session.Header, error) {
	var header session.Header
	err := errors.New("session header is incomplete")
	if _, readErr := session.ReadLines(path, 0, func(line []byte) error {
		header, err = session.ParseHeader(line)
		return session.ErrStop
	}); readErr != nil {
		return session.Header{}, readErr
	}
	return header, err
}
