package session

import (
	"bufio"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"github.com/hizkifw/kon/internal/typedid"
)

// Summary describes a persisted session without opening it for append.
type Summary struct {
	ID        typedid.SessionID
	Path      string
	CWD       string
	CreatedAt time.Time
	// Title is the first line of the session's first user message, trimmed and
	// length-bounded, so a picker can show what a session is about without
	// opening it.
	Title string
	// InUse reports that a kon process held the session open for writing when
	// it was listed, including this one for its own live session.
	InUse bool
	// Parent is the session that started this one as a subagent, or zero.
	Parent typedid.SessionID
}

// Discover lists every readable session recorded for cwd, newest first. The
// cwd-scoped directory is derived the same way New derives its target, so a
// session is visible here exactly when a future New in cwd would be able to
// resume it. Files that are not valid sessions are skipped. Ordering uses each
// header's full-precision timestamp, because the timestamp embedded in a file
// name is truncated to milliseconds and ties on the random session ID.
func Discover(root, cwd string) ([]Summary, error) {
	dir, err := directoryFor(root, cwd)
	if err != nil {
		return nil, err
	}
	dirEntries, err := os.ReadDir(dir)
	if errors.Is(err, os.ErrNotExist) {
		return nil, nil
	}
	if err != nil {
		return nil, fmt.Errorf("list sessions: %w", err)
	}
	names := make([]string, 0, len(dirEntries))
	for _, entry := range dirEntries {
		if !entry.IsDir() && strings.HasSuffix(entry.Name(), fileSuffix) {
			names = append(names, entry.Name())
		}
	}
	summaries := make([]Summary, 0, len(names))
	for _, name := range names {
		summary, err := readSummary(filepath.Join(dir, name))
		if err != nil {
			// A single unreadable file must not hide the rest of the sessions.
			continue
		}
		summary.InUse = InUse(summary.Path)
		summaries = append(summaries, summary)
	}
	// Newest first matches how "/resume" presents choices. Ordering uses each
	// header's full-precision timestamp rather than the file name, whose
	// timestamp is truncated to milliseconds: two sessions created in the same
	// millisecond would otherwise tie on their random session ID. Descending
	// path is a total tiebreaker for equal timestamps.
	sort.SliceStable(summaries, func(i, j int) bool {
		if !summaries[i].CreatedAt.Equal(summaries[j].CreatedAt) {
			return summaries[i].CreatedAt.After(summaries[j].CreatedAt)
		}
		return summaries[i].Path > summaries[j].Path
	})
	return summaries, nil
}

// Latest returns the most recently created session for cwd.
func Latest(root, cwd string) (Summary, bool, error) {
	summaries, err := Discover(root, cwd)
	if err != nil {
		return Summary{}, false, err
	}
	// A subagent's session is never the one to resume: it would otherwise win
	// whenever the last thing the agent did was delegate.
	for _, summary := range summaries {
		if summary.Parent.IsZero() {
			return summary, true, nil
		}
	}
	return Summary{}, false, nil
}

// Find returns the session for cwd whose ID matches. Only the current working
// directory is searched because session IDs are meaningful only within it.
func Find(root, cwd string, id typedid.SessionID) (Summary, error) {
	summaries, err := Discover(root, cwd)
	if err != nil {
		return Summary{}, err
	}
	for _, summary := range summaries {
		if summary.ID == id {
			return summary, nil
		}
	}
	return Summary{}, fmt.Errorf("session %s not found for this workspace", id)
}

// readSummary parses just enough of a session file to describe it. It reads the
// header directly and scans only the leading lines, stopping once the session is
// known to hold a conversation and its first user message (the title) has been
// seen. The head is where both live — a session's first user turn follows only
// its system prompt and any model changes — so listing never scales with the
// transcript. This runs on every keystroke while completing "/resume".
func readSummary(path string) (Summary, error) {
	f, err := os.Open(path)
	if err != nil {
		return Summary{}, err
	}
	defer f.Close()
	reader := bufio.NewReaderSize(f, 32<<10)

	var header Header
	first := true
	substantive := false
	title := ""
	for {
		line, readErr := reader.ReadString('\n')
		trimmed := strings.TrimSpace(line)
		switch {
		case first:
			first = false
			if err := json.Unmarshal([]byte(trimmed), &header); err != nil {
				return Summary{}, fmt.Errorf("parse session header: %w", err)
			}
			if header.Type != "session" || header.ID.IsZero() || header.Version != SchemaVersion {
				return Summary{}, errors.New("invalid session header")
			}
		case trimmed != "":
			var entry struct {
				Type    EntryType `json:"type"`
				Message *Message  `json:"message"`
			}
			if json.Unmarshal([]byte(trimmed), &entry) == nil {
				if entry.Type == EntryTypeCompaction || (entry.Message != nil && entry.Message.Role != RoleSystem) {
					substantive = true
				}
				if title == "" && entry.Message != nil && entry.Message.Role == RoleUser {
					title = sessionTitle(entry.Message.Text())
				}
			}
		}
		if readErr != nil {
			break
		}
		if substantive && title != "" {
			break
		}
	}
	if !substantive {
		// A session with no conversation is an accidental launch that should have
		// been discarded; ignore any that survived, e.g. a crash before close.
		return Summary{}, errors.New("empty session")
	}
	return Summary{ID: header.ID, Path: path, CWD: header.CWD, CreatedAt: header.Timestamp, Title: title, Parent: header.Parent}, nil
}

// titleMaxRunes bounds a session title so a picker row stays one line.
const titleMaxRunes = 60

// sessionTitle is the first non-empty line of a session's first user message,
// collapsed to a single line and length-bounded. Not every session has one.
func sessionTitle(content string) string {
	for _, line := range strings.Split(content, "\n") {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}
		runes := []rune(line)
		if len(runes) > titleMaxRunes {
			return strings.TrimSpace(string(runes[:titleMaxRunes])) + "…"
		}
		return line
	}
	return ""
}

// directoryFor resolves the cwd-scoped session directory used by New.
func directoryFor(root, cwd string) (string, error) {
	absCWD, err := filepath.Abs(cwd)
	if err != nil {
		return "", fmt.Errorf("resolve working directory: %w", err)
	}
	absCWD = filepath.Clean(absCWD)
	digest := sha256.Sum256([]byte(absCWD))
	return filepath.Join(root, hex.EncodeToString(digest[:12])), nil
}
