package session

import (
	"encoding/json"
	"fmt"

	"kon.kitsu.red/core/typedid"
)

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

// Path is the followed session's file.
func (v *View) Path() string { return v.path }

// ID is the followed session's identifier.
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

// Poll returns the entries the writer has completed since the last call. An
// error leaves the view positioned at the line that failed.
func (v *View) Poll() ([]Entry, error) {
	var added []Entry
	offset, err := ReadLines(v.path, v.offset, func(line []byte) error {
		if len(line) == 0 {
			return nil
		}
		var entry Entry
		if err := json.Unmarshal(line, &entry); err != nil {
			return fmt.Errorf("parse followed session: %w", err)
		}
		if err := checkEntry(&entry, string(line), v.byID); err != nil {
			return fmt.Errorf("followed session: %w", err)
		}
		v.byID[entry.ID] = len(v.entries)
		v.entries = append(v.entries, entry)
		added = append(added, entry)
		return nil
	})
	v.offset = offset
	return added, err
}

// Free reports whether the session's writer has let go, so it can be opened
// for writing.
func (v *View) Free() bool { return !InUse(v.path) }
