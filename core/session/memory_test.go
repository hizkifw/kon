package session

import (
	"bytes"
	"os"
	"testing"
)

func TestMemorySessionLeavesNothingBehind(t *testing.T) {
	cwd := t.TempDir()
	t.Chdir(cwd)
	store, err := NewMemory(Header{CWD: cwd}, "system")
	if err != nil {
		t.Fatal(err)
	}
	if _, err := store.AppendMessage(userText("hello")); err != nil {
		t.Fatal(err)
	}
	if store.Empty() {
		t.Fatal("session with a user message reports empty")
	}
	context, err := store.Context()
	if err != nil || len(context) != 2 || context[1].Message.Text() != "hello" {
		t.Fatalf("Context = (%#v, %v), want system and user messages", context, err)
	}
	image := []byte("png bytes")
	part, err := store.SaveMedia(image, "image/png")
	if err != nil {
		t.Fatal(err)
	}
	if got, err := store.ReadMedia(part.MediaHash); err != nil || !bytes.Equal(got, image) {
		t.Fatalf("ReadMedia = (%q, %v), want the saved bytes", got, err)
	}
	if err := store.Close(); err != nil {
		t.Fatal(err)
	}
	// Persisted sessions keep blobs beside their file; a memory one must not
	// fall back to paths relative to where the program runs.
	if entries, err := os.ReadDir(cwd); err != nil || len(entries) != 0 {
		t.Fatalf("working directory holds %v (%v), want nothing", entries, err)
	}
	if _, err := store.AppendMessage(userText("late")); err == nil {
		t.Fatal("append after close succeeded")
	}
}
