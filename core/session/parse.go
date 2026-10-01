package session

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"strings"

	"github.com/hizkifw/kon/core/typedid"
)

// parsedSession is the mutable-free result of reading and validating a session
// file. Open owns the file afterwards for append; Entries only reads it.
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

// parseSession reads a session file and validates its header and entries. A
// single incomplete trailing record is reported through repairOffset instead of
// an error, so a caller that owns the file can trim it while a read-only caller
// can ignore it.
func parseSession(path string) (parsedSession, error) {
	b, err := os.ReadFile(path)
	if err != nil {
		return parsedSession{}, fmt.Errorf("read session: %w", err)
	}
	lines := strings.Split(string(b), "\n")
	last := len(lines) - 1
	for last >= 0 && strings.TrimSpace(lines[last]) == "" {
		last--
	}
	if last < 0 {
		return parsedSession{}, errors.New("empty session")
	}
	header, err := ParseHeader([]byte(lines[0]))
	if err != nil {
		return parsedSession{}, err
	}

	result := parsedSession{header: header, byID: make(map[typedid.EntryID]int), repairOffset: -1, size: int64(len(b))}
	for i := 1; i <= last; i++ {
		if strings.TrimSpace(lines[i]) == "" {
			continue
		}
		var entry Entry
		if err := json.Unmarshal([]byte(lines[i]), &entry); err != nil {
			if i == last {
				result.repairOffset = int64(len(strings.Join(lines[:i], "\n")) + 1)
				result.size = result.repairOffset
				break
			}
			return parsedSession{}, fmt.Errorf("parse session line %d: %w", i+1, err)
		}
		if err := checkEntry(&entry, lines[i], result.byID); err != nil {
			return parsedSession{}, fmt.Errorf("session line %d: %w", i+1, err)
		}
		result.byID[entry.ID] = len(result.entries)
		result.entries = append(result.entries, entry)
	}
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
