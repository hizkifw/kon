package history

import (
	"fmt"
	"os"
	"path/filepath"
	"strconv"
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

// Recall walks back from the latest prompt, so once the cap is reached the
// oldest entries are the ones to drop.
func TestLoadKeepsTheNewestEntries(t *testing.T) {
	path := filepath.Join(t.TempDir(), "history.jsonl")
	const extra = 10
	var b strings.Builder
	for i := range maxLoaded + extra {
		fmt.Fprintf(&b, "{\"text\":\"%d\"}\n", i)
	}
	if err := os.WriteFile(path, []byte(b.String()), 0o600); err != nil {
		t.Fatal(err)
	}
	entries, err := New(path).Load()
	if err != nil {
		t.Fatal(err)
	}
	if len(entries) != maxLoaded {
		t.Fatalf("loaded %d entries, want %d", len(entries), maxLoaded)
	}
	for i, entry := range entries {
		if want := strconv.Itoa(extra + i); entry.Text != want {
			t.Fatalf("entries[%d] = %q, want %q", i, entry.Text, want)
		}
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
