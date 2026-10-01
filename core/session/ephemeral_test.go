package session

import (
	"bytes"
	"errors"
	"os"
	"testing"
)

func TestEphemeralSessionLeavesNothingBehind(t *testing.T) {
	cwd := t.TempDir()
	store, err := NewEphemeral(cwd, "test", "system")
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
	part, err := store.SaveImage(image, "image/png")
	if err != nil {
		t.Fatal(err)
	}
	if got, err := store.ReadImage(part.ImageHash); err != nil || !bytes.Equal(got, image) {
		t.Fatalf("ReadImage = (%q, %v), want the saved bytes", got, err)
	}
	jobs := store.JobsDir()
	if info, err := os.Stat(jobs); err != nil || !info.IsDir() {
		t.Fatalf("jobs directory %q is not ready: %v", jobs, err)
	}

	if err := store.Close(); err != nil {
		t.Fatal(err)
	}
	if _, err := os.Stat(jobs); !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("jobs directory survived close: %v", err)
	}
	// Persisted sessions keep blobs and jobs beside their file; an ephemeral
	// one must not fall back to paths relative to where kon runs.
	if entries, err := os.ReadDir(cwd); err != nil || len(entries) != 0 {
		t.Fatalf("working directory holds %v (%v), want nothing", entries, err)
	}
	if _, err := store.AppendMessage(userText("late")); err == nil {
		t.Fatal("append after close succeeded")
	}
}
