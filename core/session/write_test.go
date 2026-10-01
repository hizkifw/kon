package session

import (
	"bytes"
	"encoding/json"
	"errors"
	"os"
	"testing"
)

// tornFile writes half of each record and then fails, as a full disk does.
type tornFile struct {
	sessionFile
	truncateErr error
}

func (f *tornFile) Write(b []byte) (int, error) {
	n, _ := f.sessionFile.Write(b[:len(b)/2])
	return n, errors.New("no space left on device")
}

func (f *tornFile) Truncate(size int64) error {
	if f.truncateErr != nil {
		return f.truncateErr
	}
	return f.sessionFile.Truncate(size)
}

func userText(text string) Message {
	return Message{Role: RoleUser, Parts: []Part{{Type: PartText, Text: text}}}
}

func TestFailedAppendLeavesNoTornLine(t *testing.T) {
	// New and Open differ: only a reopened file is in append mode, so a new
	// session's next write depends on seeking to the truncated end.
	for _, reopen := range []bool{false, true} {
		store, err := New(t.TempDir(), t.TempDir(), "test", "system")
		if err != nil {
			t.Fatal(err)
		}
		if _, err := store.AppendMessage(userText("before")); err != nil {
			t.Fatal(err)
		}
		if reopen {
			if err := store.Close(); err != nil {
				t.Fatal(err)
			}
			if store, err = Open(store.Path()); err != nil {
				t.Fatal(err)
			}
		}
		file := store.file
		store.file = &tornFile{sessionFile: file}
		if _, err := store.AppendMessage(userText("lost")); err == nil {
			t.Fatal("torn write reported success")
		}
		store.file = file
		if _, err := store.AppendMessage(userText("after")); err != nil {
			t.Fatalf("reopen=%v: append after a rolled-back failure: %v", reopen, err)
		}
		if err := store.Close(); err != nil {
			t.Fatal(err)
		}

		b, err := os.ReadFile(store.Path())
		if err != nil {
			t.Fatal(err)
		}
		for i, line := range bytes.Split(bytes.TrimSuffix(b, []byte("\n")), []byte("\n")) {
			if !json.Valid(line) {
				t.Fatalf("reopen=%v: line %d is torn: %q", reopen, i+1, line)
			}
		}
		reopened, err := Open(store.Path())
		if err != nil {
			t.Fatalf("reopen=%v: %v", reopen, err)
		}
		context, err := reopened.Context()
		reopened.Close()
		if err != nil {
			t.Fatal(err)
		}
		var texts []string
		for _, message := range context {
			texts = append(texts, message.Message.Text())
		}
		if len(texts) != 3 || texts[1] != "before" || texts[2] != "after" {
			t.Fatalf("reopen=%v: context = %q", reopen, texts)
		}
	}
}

func TestUnrecoverableAppendStopsLaterAppends(t *testing.T) {
	store, err := New(t.TempDir(), t.TempDir(), "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	if _, err := store.AppendMessage(userText("before")); err != nil {
		t.Fatal(err)
	}
	file := store.file
	store.file = &tornFile{sessionFile: file, truncateErr: errors.New("read-only file system")}
	if _, err := store.AppendMessage(userText("lost")); err == nil {
		t.Fatal("torn write reported success")
	}
	store.file = file
	// The torn line is still on disk, so a later record would join it.
	if _, err := store.AppendMessage(userText("after")); err == nil {
		t.Fatal("append after an unrecoverable failure was accepted")
	}
}
