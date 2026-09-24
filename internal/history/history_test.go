package history

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestLoadAcceptsAnOversizedEntry(t *testing.T) {
	// One huge paste used to exceed the scanner's line limit, which failed
	// the load and kept kon from starting.
	store := New(filepath.Join(t.TempDir(), "history.jsonl"))
	huge := strings.Repeat("x", 3*1024*1024)
	for _, text := range []string{"first", huge, "last"} {
		if err := store.Append("/work", text); err != nil {
			t.Fatal(err)
		}
	}
	entries, err := store.Load()
	if err != nil {
		t.Fatal(err)
	}
	if len(entries) != 3 || entries[0].Text != "first" || entries[1].Text != huge || entries[2].Text != "last" {
		t.Fatalf("loaded %d entries", len(entries))
	}
}

func TestLoadSkipsAnEntryLargerThanTheWindow(t *testing.T) {
	store := New(filepath.Join(t.TempDir(), "history.jsonl"))
	for _, text := range []string{"first", strings.Repeat("x", maxLoadBytes+1), "last"} {
		if err := store.Append("/work", text); err != nil {
			t.Fatal(err)
		}
	}
	entries, err := store.Load()
	if err != nil {
		t.Fatal(err)
	}
	if len(entries) != 1 || entries[0].Text != "last" {
		t.Fatalf("entries = %d, want only the one after the oversized entry", len(entries))
	}
}

func TestLoadSkipsMalformedLines(t *testing.T) {
	path := filepath.Join(t.TempDir(), "history.jsonl")
	if err := os.WriteFile(path, []byte("{\"text\":\"ok\"}\nnot json\n{\"text\":\"\"}\n{\"text\":\"torn"), 0o600); err != nil {
		t.Fatal(err)
	}
	entries, err := New(path).Load()
	if err != nil {
		t.Fatal(err)
	}
	if len(entries) != 1 || entries[0].Text != "ok" {
		t.Fatalf("entries = %#v", entries)
	}
}
