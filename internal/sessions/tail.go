package sessions

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"strings"

	"github.com/hizkifw/kon/core/session"
	"github.com/hizkifw/kon/core/typedid"
)

// tailBlock bounds how much of a file a tail read pulls in per step.
const tailBlock = 256 << 10

// TailEntries reads at most the last maxTurns user turns of a session for
// read-only display, without parsing the whole file. Sessions grow without
// bound, so a preview that parsed every line would scale with the transcript
// rather than the glance it is. It never repairs or opens the file for append,
// so it is safe to call against a session a live runtime is still writing.
func TailEntries(path string, maxTurns int) ([]session.Entry, error) {
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
				Role session.Role `json:"role"`
			} `json:"message"`
		}
		if json.Unmarshal([]byte(strings.TrimSpace(lines[i])), &probe) != nil || probe.Message == nil {
			continue
		}
		if probe.Message.Role != session.RoleUser {
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
func parseTail(lines []string, start int) ([]session.Entry, error) {
	if start < 1 {
		start = 1
	}
	entries := make([]session.Entry, 0, len(lines)-start)
	byID := make(map[typedid.EntryID]int, len(lines)-start)
	for i := start; i < len(lines); i++ {
		line := strings.TrimSpace(lines[i])
		if line == "" {
			continue
		}
		var entry session.Entry
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
	reverse := make([]session.Entry, 0, len(entries))
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
