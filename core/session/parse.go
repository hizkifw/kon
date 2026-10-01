package session

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"

	"github.com/hizkifw/kon/core/typedid"
)

// parsedSession is the result of reading and validating a session file. Open
// owns the file afterwards for append; OpenView only reads it.
type parsedSession struct {
	header       Header
	entries      []Entry
	byID         map[typedid.EntryID]int
	repairOffset int64 // byte length of the valid prefix, or -1 when the file is intact
	// size is the byte length of the complete records read, where a follower
	// resumes reading.
	size int64
}

// leafID is the final entry's ID, which is the active leaf in v4.
func (p parsedSession) leafID() *typedid.EntryID {
	if len(p.entries) == 0 {
		return nil
	}
	leaf := p.entries[len(p.entries)-1].ID
	return &leaf
}

// parseSession reads a session file and validates its header and entries. An
// incomplete trailing record, cut short by a crash mid-append, is reported
// through repairOffset instead of an error, so a caller that owns the file can
// trim it while a read-only caller can ignore it.
func parseSession(path string) (parsedSession, error) {
	result := parsedSession{byID: make(map[typedid.EntryID]int), repairOffset: -1}
	number := 0
	end, err := ReadLines(path, 0, func(line []byte) error {
		number++
		if number == 1 {
			header, err := ParseHeader(line)
			result.header = header
			return err
		}
		if len(line) == 0 {
			return nil
		}
		var entry Entry
		if err := json.Unmarshal(line, &entry); err != nil {
			return fmt.Errorf("parse session line %d: %w", number, err)
		}
		if err := checkEntry(&entry, string(line), result.byID); err != nil {
			return fmt.Errorf("session line %d: %w", number, err)
		}
		result.byID[entry.ID] = len(result.entries)
		result.entries = append(result.entries, entry)
		return nil
	})
	if errors.Is(err, ErrRemoved) {
		return parsedSession{}, fmt.Errorf("read session: %w", os.ErrNotExist)
	}
	if err != nil {
		return parsedSession{}, err
	}
	if number == 0 {
		return parsedSession{}, errors.New("empty session")
	}
	info, err := os.Stat(path)
	if err != nil {
		return parsedSession{}, fmt.Errorf("read session: %w", err)
	}
	if info.Size() > end {
		result.repairOffset = end
	}
	result.size = end
	return result, nil
}

// checkEntry validates a decoded entry against the entries before it and keeps
// its raw line.
func checkEntry(entry *Entry, line string, byID map[typedid.EntryID]int) error {
	entry.Raw = append(json.RawMessage(nil), line...)
	if entry.ID.IsZero() || entry.Type == "" {
		return errors.New("invalid session entry")
	}
	if err := entry.validate(); err != nil {
		return fmt.Errorf("invalid session entry: %w", err)
	}
	if _, exists := byID[entry.ID]; exists {
		return fmt.Errorf("duplicate session entry id %q", entry.ID)
	}
	if entry.ParentID != nil {
		if _, exists := byID[*entry.ParentID]; !exists {
			return fmt.Errorf("entry %q has missing parent %q", entry.ID, *entry.ParentID)
		}
	}
	return nil
}
