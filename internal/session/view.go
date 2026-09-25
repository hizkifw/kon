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

// Poll returns the entries the writer has completed since the last call. A
// session file only shrinks when its writer rolls back a record that failed to
// sync, which a view may already have read, so a shorter file is an error
// rather than something to reconcile.
func (v *View) Poll() ([]Entry, error) {
	f, err := os.Open(v.path)
	if errors.Is(err, os.ErrNotExist) {
		return nil, ErrRemoved
	}
	if err != nil {
		return nil, fmt.Errorf("read session: %w", err)
	}
	defer f.Close()
	info, err := f.Stat()
	if err != nil {
		return nil, fmt.Errorf("read session: %w", err)
	}
	if info.Size() < v.offset {
		return nil, errors.New("session file shrank while it was followed")
	}
	if info.Size() == v.offset {
		return nil, nil
	}
	if _, err := f.Seek(v.offset, io.SeekStart); err != nil {
		return nil, fmt.Errorf("read session: %w", err)
	}
	b, err := io.ReadAll(io.LimitReader(f, info.Size()-v.offset))
	if err != nil {
		return nil, fmt.Errorf("read session: %w", err)
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
