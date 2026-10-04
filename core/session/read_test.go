package session

import (
	"bytes"
	"os"
	"path/filepath"
	"testing"
)

func TestReadLinesKeepsRecordsLongerThanTheBuffer(t *testing.T) {
	// A record past the reader's 64 KiB buffer is gathered in two reads; the
	// first part must survive the second refilling the buffer.
	long := append([]byte(`{"text":"`), bytes.Repeat([]byte("ab"), 40<<10)...)
	long = append(long, `"}`...)
	want := [][]byte{[]byte(`{"first":1}`), long, []byte(`{"last":2}`)}
	path := filepath.Join(t.TempDir(), "session.jsonl")
	if err := os.WriteFile(path, append(bytes.Join(want, []byte("\n")), '\n'), 0o600); err != nil {
		t.Fatal(err)
	}
	var got [][]byte
	if _, err := ReadLines(path, 0, func(line []byte) error {
		got = append(got, bytes.Clone(line))
		return nil
	}); err != nil {
		t.Fatal(err)
	}
	if len(got) != len(want) {
		t.Fatalf("read %d lines, want %d", len(got), len(want))
	}
	for i := range want {
		if !bytes.Equal(got[i], want[i]) {
			t.Fatalf("line %d differs: got %d bytes starting %q", i+1, len(got[i]), got[i][:min(len(got[i]), 40)])
		}
	}
}
