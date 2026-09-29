package session

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"strings"

	"github.com/hizkifw/kon/internal/typedid"
)

// tailBlock bounds how much of a file a tail read pulls in per step.
const tailBlock = 256 << 10

// TailEntries reads at most the last maxTurns user turns of a session for
// read-only display, without parsing the whole file. Sessions grow without
// bound, so a preview that parsed every line would scale with the transcript
// rather than the glance it is. It never repairs or opens the file for append,
// so it is safe to call against a session a live runtime is still writing.
func TailEntries(path string, maxTurns int) ([]Entry, error) {
	if maxTurns < 1 {
		maxTurns = 1
	}
	f, err := os.Open(path)
	if err != nil {
		return nil, fmt.Errorf("read session: %w", err)
	}
	defer f.Close()
	info, err := f.Stat()
	if err != nil {
		return nil, fmt.Errorf("read session: %w", err)
	}

	// Read backwards a block at a time until the buffer holds enough user
	// messages to name the window, stopping at the start of the smaller of the
	// whole file or the trailing turns.
	var data []byte
	for start := info.Size(); ; {
		readSize := min(int64(tailBlock), start)
		if readSize > 0 {
			start -= readSize
			chunk := make([]byte, readSize)
			if _, err := f.ReadAt(chunk, start); err != nil {
				return nil, fmt.Errorf("read session: %w", err)
			}
			data = append(chunk, data...)
		}
		lines := strings.Split(string(data), "\n")
		first := 0
		if start > 0 {
			// The first buffered line may begin mid-record.
			first = 1
		}
		index, found := windowStart(lines, first, maxTurns)
		if found || start == 0 {
			return parseTail(lines, index)
		}
	}
}

// windowStart returns the index of the maxTurns-th user message counting back
// from the end, scanning no earlier than first. found is false when fewer than
// maxTurns user messages are present in lines[first:].
func windowStart(lines []string, first, maxTurns int) (int, bool) {
	count := 0
	for i := len(lines) - 1; i >= first; i-- {
		var probe struct {
			Message *struct {
				Role Role `json:"role"`
			} `json:"message"`
		}
		if json.Unmarshal([]byte(strings.TrimSpace(lines[i])), &probe) != nil || probe.Message == nil {
			continue
		}
		if probe.Message.Role != RoleUser {
			continue
		}
		count++
		if count == maxTurns {
			return i, true
		}
	}
	return first, false
}

// parseTail decodes entries from lines[start:], skipping the header and any
// blank lines, then returns the active path tail: the leaf's ancestor chain
// within the window, so a branch that skips entries is still followed
// correctly. A single incomplete trailing record (a crash mid-append) is
// dropped rather than failing the whole preview.
func parseTail(lines []string, start int) ([]Entry, error) {
	if start < 1 {
		start = 1
	}
	entries := make([]Entry, 0, len(lines)-start)
	byID := make(map[typedid.EntryID]int, len(lines)-start)
	for i := start; i < len(lines); i++ {
		line := strings.TrimSpace(lines[i])
		if line == "" {
			continue
		}
		var entry Entry
		if err := json.Unmarshal([]byte(line), &entry); err != nil {
			if i == len(lines)-1 {
				break
			}
			return nil, fmt.Errorf("parse session line %d: %w", i+1, err)
		}
		if entry.ID.IsZero() || entry.Type == "" {
			continue
		}
		entry.Raw = append(json.RawMessage(nil), line...)
		byID[entry.ID] = len(entries)
		entries = append(entries, entry)
	}
	if len(entries) == 0 {
		return nil, nil
	}
	// Walk parent links from the last entry. A parent that is absent marks the
	// window's leading edge, which is where the preview starts.
	reverse := make([]Entry, 0, len(entries))
	index := len(entries) - 1
	seen := make(map[typedid.EntryID]bool, len(entries))
	for {
		entry := entries[index]
		if seen[entry.ID] {
			return nil, errors.New("cycle in session parent links")
		}
		seen[entry.ID] = true
		reverse = append(reverse, entry)
		if entry.ParentID == nil {
			break
		}
		parent, ok := byID[*entry.ParentID]
		if !ok {
			break
		}
		index = parent
	}
	for i, j := 0, len(reverse)-1; i < j; i, j = i+1, j-1 {
		reverse[i], reverse[j] = reverse[j], reverse[i]
	}
	return reverse, nil
}

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
	var header Header
	if err := json.Unmarshal([]byte(lines[0]), &header); err != nil {
		return parsedSession{}, fmt.Errorf("parse session header: %w", err)
	}
	if header.Type != "session" || header.ID.IsZero() {
		return parsedSession{}, errors.New("invalid session header")
	}
	if header.Version != SchemaVersion {
		return parsedSession{}, fmt.Errorf("unsupported session version %d", header.Version)
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

// ValidateFile checks a session without opening it for append or changing it.
// Storage migrations use this before replacing the original file.
func ValidateFile(path string) error {
	_, err := parseSession(path)
	return err
}
