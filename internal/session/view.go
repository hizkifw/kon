package session

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"

	"github.com/hizkifw/kon/internal/typedid"
)

// ErrRemoved reports that a followed session's file no longer exists, as when
// its writer discards a session that never held a conversation.
var ErrRemoved = errors.New("session was removed")

// View follows a session that another process is writing. It has no append
// methods, so read-only is a property of the type rather than a mode. It never
// repairs the file: a torn trailing line may be a record the writer is still
// appending, so a view reads only complete lines and picks up the rest once
// the newline lands.
type View struct {
	path    string
	header  Header
	entries []Entry
	byID    map[typedid.EntryID]int
	// offset is the byte length of the complete records read so far.
	offset int64
}

// OpenView reads a session for following without taking its writer lock.
func OpenView(path string) (*View, error) {
	parsed, err := parseSession(path)
	if err != nil {
		return nil, err
	}
	return &View{path: path, header: parsed.header, entries: parsed.entries, byID: parsed.byID, offset: parsed.size}, nil
}

func (v *View) Path() string          { return v.path }
func (v *View) ID() typedid.SessionID { return v.header.ID }

// ActivePath returns the conversation read so far. The writer only ever
// extends its own leaf, which in v4 is the final entry.
func (v *View) ActivePath() []Entry {
	if len(v.entries) == 0 {
		return nil
	}
	leaf := v.entries[len(v.entries)-1].ID
	path, err := activePath(v.entries, v.byID, &leaf)
	if err != nil {
		return nil
	}
	return path
}

// Poll returns the entries the writer has completed since the last call.
func (v *View) Poll() ([]Entry, error) {
	b, err := appended(v.path, v.offset)
	if err != nil {
		return nil, err
	}
	// Only whole lines are read. The offset advances line by line, so an
	// error leaves the view positioned at the line that failed.
	var added []Entry
	for {
		end := bytes.IndexByte(b, '\n')
		if end < 0 {
			return added, nil
		}
		line := bytes.TrimSpace(b[:end])
		if len(line) > 0 {
			var entry Entry
			if err := json.Unmarshal(line, &entry); err != nil {
				return added, fmt.Errorf("parse followed session: %w", err)
			}
			if err := checkEntry(&entry, string(line), v.byID); err != nil {
				return added, fmt.Errorf("followed session: %w", err)
			}
			v.byID[entry.ID] = len(v.entries)
			v.entries = append(v.entries, entry)
			added = append(added, entry)
		}
		v.offset += int64(end + 1)
		b = b[end+1:]
	}
}

// Free reports whether the session's writer has let go, so it can be opened
// for writing.
func (v *View) Free() bool { return !InUse(v.path) }

// errShrank reports a session file shorter than what was already read from
// it. A session file only shrinks when its writer rolls back a record that
// failed to sync, which a reader may already have read, so it is an error
// rather than something to reconcile.
var errShrank = errors.New("session file shrank while it was followed")

// appended reads what a session file holds past offset, which may end in a
// line the writer has not finished. The file is only opened once it has grown,
// so following an idle session costs one stat.
func appended(path string, offset int64) ([]byte, error) {
	info, err := os.Stat(path)
	if errors.Is(err, os.ErrNotExist) {
		return nil, ErrRemoved
	}
	if err != nil {
		return nil, fmt.Errorf("read session: %w", err)
	}
	if info.Size() < offset {
		return nil, errShrank
	}
	if info.Size() == offset {
		return nil, nil
	}
	f, err := os.Open(path)
	if errors.Is(err, os.ErrNotExist) {
		return nil, ErrRemoved
	}
	if err != nil {
		return nil, fmt.Errorf("read session: %w", err)
	}
	defer f.Close()
	// The size is known, so the bytes are read into a buffer of that size
	// rather than one grown by doubling.
	b := make([]byte, info.Size()-offset)
	n, err := f.ReadAt(b, offset)
	if err != nil && !errors.Is(err, io.EOF) {
		return nil, fmt.Errorf("read session: %w", err)
	}
	return b[:n], nil
}
