// Package history stores global prompt recall as append-only JSONL.
package history

import (
	"bufio"
	"encoding/json"
	"fmt"
	"os"
	"time"
)

const maxLoaded = 500
const maxLoadBytes = 4 * 1024 * 1024

type Entry struct {
	Timestamp time.Time `json:"timestamp"`
	CWD       string    `json:"cwd"`
	Text      string    `json:"text"`
}

type Store struct {
	path string
}

func New(path string) *Store { return &Store{path: path} }

func (s *Store) Append(cwd, text string) error {
	f, err := os.OpenFile(s.path, os.O_APPEND|os.O_WRONLY|os.O_CREATE, 0o600)
	if err != nil {
		return fmt.Errorf("open prompt history: %w", err)
	}
	defer f.Close()
	b, err := json.Marshal(Entry{Timestamp: time.Now().UTC(), CWD: cwd, Text: text})
	if err != nil {
		return fmt.Errorf("encode prompt history: %w", err)
	}
	if _, err := f.Write(append(b, '\n')); err != nil {
		return fmt.Errorf("append prompt history: %w", err)
	}
	return f.Sync()
}

func (s *Store) Load() ([]Entry, error) {
	f, err := os.Open(s.path)
	if err != nil {
		return nil, fmt.Errorf("open prompt history: %w", err)
	}
	defer f.Close()
	info, err := f.Stat()
	if err != nil {
		return nil, fmt.Errorf("stat prompt history: %w", err)
	}
	start := max(int64(0), info.Size()-maxLoadBytes)
	if _, err := f.Seek(start, 0); err != nil {
		return nil, fmt.Errorf("seek prompt history: %w", err)
	}

	entries := make([]Entry, 0, maxLoaded)
	scanner := bufio.NewScanner(f)
	buf := make([]byte, 64*1024)
	scanner.Buffer(buf, 2*1024*1024)
	first := true
	for scanner.Scan() {
		if first && start > 0 {
			// The bounded read probably began in the middle of a JSONL record.
			first = false
			continue
		}
		first = false
		var entry Entry
		if json.Unmarshal(scanner.Bytes(), &entry) != nil || entry.Text == "" {
			continue
		}
		if len(entries) == maxLoaded {
			copy(entries, entries[1:])
			entries[len(entries)-1] = entry
		} else {
			entries = append(entries, entry)
		}
	}
	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("read prompt history: %w", err)
	}
	return entries, nil
}
