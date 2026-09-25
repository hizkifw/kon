package session

import (
	"errors"
	"os"
	"testing"
)

// keptSession creates a session with a user message, so closing it keeps the
// file, and returns its path with the writer still open.
func keptSession(t *testing.T) *Store {
	t.Helper()
	store, err := New(t.TempDir(), t.TempDir(), "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	if _, err := store.AppendMessage(TextMessage(RoleUser, "hello")); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = store.Close() })
	return store
}

func TestOpenRefusesASessionAnotherWriterHolds(t *testing.T) {
	writer := keptSession(t)
	if !InUse(writer.Path()) {
		t.Fatal("a new session is not reported in use while its writer is open")
	}
	if _, err := Open(writer.Path()); !errors.Is(err, ErrInUse) {
		t.Fatalf("second open error = %v, want ErrInUse", err)
	}
	if err := writer.Close(); err != nil {
		t.Fatal(err)
	}
	if InUse(writer.Path()) {
		t.Fatal("a closed session is still reported in use")
	}
	reopened, err := Open(writer.Path())
	if err != nil {
		t.Fatalf("open after the writer closed: %v", err)
	}
	if err := reopened.Close(); err != nil {
		t.Fatal(err)
	}
}

// TestOpenDoesNotRepairAHeldSession guards the reason the lock exists: a torn
// tail may be a record another writer is still appending, so a refused open
// must leave the file untouched.
func TestOpenDoesNotRepairAHeldSession(t *testing.T) {
	writer := keptSession(t)
	f, err := os.OpenFile(writer.Path(), os.O_WRONLY|os.O_APPEND, 0)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := f.WriteString(`{"type":"message","id":`); err != nil {
		t.Fatal(err)
	}
	f.Close()
	before, err := os.Stat(writer.Path())
	if err != nil {
		t.Fatal(err)
	}
	if _, err := Open(writer.Path()); !errors.Is(err, ErrInUse) {
		t.Fatalf("open error = %v, want ErrInUse", err)
	}
	after, err := os.Stat(writer.Path())
	if err != nil {
		t.Fatal(err)
	}
	if after.Size() != before.Size() {
		t.Fatalf("refused open changed the file from %d to %d bytes", before.Size(), after.Size())
	}
}

func TestFailedOpenReleasesTheLock(t *testing.T) {
	writer := keptSession(t)
	path := writer.Path()
	if err := writer.Close(); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte("not a session\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	if _, err := Open(path); err == nil || errors.Is(err, ErrInUse) {
		t.Fatalf("open error = %v, want a parse error", err)
	}
	if InUse(path) {
		t.Fatal("a failed open kept the session locked")
	}
}

func TestDiscardedSessionRemovesItsLock(t *testing.T) {
	store, err := New(t.TempDir(), t.TempDir(), "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	if err := store.Close(); err != nil {
		t.Fatal(err)
	}
	if _, err := os.Stat(lockPath(store.Path())); !os.IsNotExist(err) {
		t.Fatalf("lock file of a discarded session: %v", err)
	}
}

func TestInUseDoesNotCreateALockFile(t *testing.T) {
	path := keptSession(t).Path() + ".other"
	if InUse(path) {
		t.Fatal("a session with no lock file is reported in use")
	}
	if _, err := os.Stat(lockPath(path)); !os.IsNotExist(err) {
		t.Fatalf("InUse created a lock file: %v", err)
	}
}

func TestDiscoverReportsSessionsInUse(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	store, err := New(root, cwd, "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	if _, err := store.AppendMessage(TextMessage(RoleUser, "hello")); err != nil {
		t.Fatal(err)
	}
	summaries, err := Discover(root, cwd)
	if err != nil || len(summaries) != 1 || !summaries[0].InUse {
		t.Fatalf("summaries while open = %+v, %v", summaries, err)
	}
	if err := store.Close(); err != nil {
		t.Fatal(err)
	}
	summaries, err = Discover(root, cwd)
	if err != nil || len(summaries) != 1 || summaries[0].InUse {
		t.Fatalf("summaries after close = %+v, %v", summaries, err)
	}
}
