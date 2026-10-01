package sessions

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	"kon.kitsu.red/core/session"
	"kon.kitsu.red/core/typedid"
)

func TestDiscoverFindsSessionsNewestFirst(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	first, err := New(root, cwd, "test", "system", typedid.SessionID{})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := first.AppendMessage(session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: "one"}}}); err != nil {
		t.Fatal(err)
	}
	first.Close()
	second, err := New(root, cwd, "test", "system", typedid.SessionID{})
	if err != nil {
		t.Fatal(err)
	}
	secondID := second.ID()
	if _, err := second.AppendMessage(session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: "two"}}}); err != nil {
		t.Fatal(err)
	}
	second.Close()

	// Another workspace must not leak into this one's results. It needs a
	// message of its own, or Close discards it and there is nothing to leak.
	other, err := New(root, t.TempDir(), "test", "system", typedid.SessionID{})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := other.AppendMessage(session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: "elsewhere"}}}); err != nil {
		t.Fatal(err)
	}
	other.Close()

	summaries, err := Discover(root, cwd)
	if err != nil {
		t.Fatal(err)
	}
	got := make([]typedid.SessionID, len(summaries))
	for i, summary := range summaries {
		got[i] = summary.ID
	}
	if want := []typedid.SessionID{secondID, first.ID()}; !slices.Equal(got, want) {
		t.Fatalf("Discover = %v, want %v without %s from the other workspace", got, want, other.ID())
	}
	if summaries[0].Title != "two" {
		t.Fatalf("newest session title = %q, want %q", summaries[0].Title, "two")
	}
	for _, summary := range summaries {
		if summary.CWD != cwd {
			t.Fatalf("summary working directory = %q, want %q", summary.CWD, cwd)
		}
	}
}

// TestDiscoverOrdersByHeaderWhenNamesCollide covers two sessions created in the
// same millisecond. Their file names share a timestamp prefix, so name order
// falls back to the random session ID; only the header's full-precision
// timestamp can order them. The fixture forces name order to be the reverse of
// time order so the test fails without reading the header.
func TestDiscoverOrdersByHeaderWhenNamesCollide(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	dir, err := directoryFor(root, cwd)
	if err != nil {
		t.Fatal(err)
	}
	if err := os.MkdirAll(dir, 0o700); err != nil {
		t.Fatal(err)
	}
	// The "older" session sorts later by name than the "newer" one, so a
	// name-only ordering would surface it first.
	older, err := typedid.ParseSessionID("ses_zzzzzzzzzzzzzzzzzzzz")
	if err != nil {
		t.Fatal(err)
	}
	newer, err := typedid.ParseSessionID("ses_00000000000000000000")
	if err != nil {
		t.Fatal(err)
	}
	writeSessionWithHeaderTime(t, dir, older, "older", time.Date(2026, 1, 1, 0, 0, 0, 1, time.UTC))
	writeSessionWithHeaderTime(t, dir, newer, "newer", time.Date(2026, 1, 1, 0, 0, 0, 2, time.UTC))

	summaries, err := Discover(root, cwd)
	if err != nil {
		t.Fatal(err)
	}
	if len(summaries) != 2 {
		t.Fatalf("Discover returned %d sessions, want 2", len(summaries))
	}
	if summaries[0].ID != newer || summaries[1].ID != older {
		t.Fatalf("Discover order = [%s %s], want [%s %s]", summaries[0].ID, summaries[1].ID, newer, older)
	}
}

// writeSessionWithHeaderTime writes a minimal valid session file whose name
// carries a fixed millisecond prefix (so names collide) while the header records
// the given full-precision timestamp.
func writeSessionWithHeaderTime(t *testing.T, dir string, sessionID typedid.SessionID, title string, created time.Time) {
	t.Helper()
	entryID, err := typedid.NewEntryID()
	if err != nil {
		t.Fatal(err)
	}
	header := session.Header{Type: "session", Version: session.SchemaVersion, ID: sessionID, AppVersion: "test", Timestamp: created, CWD: "/tmp"}
	message := session.TextMessage(session.RoleUser, title)
	path := filepath.Join(dir, created.UTC().Format("20060102T150405.000Z")+"_"+sessionID.String()+fileSuffix)
	var b strings.Builder
	writeRecord := func(v any) {
		data, err := json.Marshal(v)
		if err != nil {
			t.Fatal(err)
		}
		b.Write(data)
		b.WriteByte('\n')
	}
	writeRecord(header)
	writeRecord(session.Entry{Type: session.EntryTypeMessage, ID: entryID, Timestamp: created, Message: &message})
	if err := os.WriteFile(path, []byte(b.String()), 0o600); err != nil {
		t.Fatal(err)
	}
}

// TestDiscoverReadsOnlySessionHead proves listing stops at the head: a line that
// would only fail validation deep in the tail must not hide the session, because
// Discover never reads that far.
func TestDiscoverReadsOnlySessionHead(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	store, err := New(root, cwd, "test", "system", typedid.SessionID{})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := store.AppendMessage(session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: "opening question"}}}); err != nil {
		t.Fatal(err)
	}
	// Pad past the head window with valid turns, then append a corrupt record
	// directly to the file.
	for i := 0; i < 50; i++ {
		store.AppendMessage(session.Message{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartText, Text: strings.Repeat("x", 200)}}})
	}
	path := store.Path()
	if err := store.Close(); err != nil {
		t.Fatal(err)
	}
	f, err := os.OpenFile(path, os.O_APPEND|os.O_WRONLY, 0)
	if err != nil {
		t.Fatal(err)
	}
	f.WriteString("{not valid json at all\n")
	f.Close()

	summaries, err := Discover(root, cwd)
	if err != nil {
		t.Fatal(err)
	}
	if len(summaries) != 1 || summaries[0].Title != "opening question" {
		t.Fatalf("Discover = %#v", summaries)
	}
}

func TestTailEntriesReadsTrailingTurnsWithoutOpening(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	store, err := New(root, cwd, "test", "system", typedid.SessionID{})
	if err != nil {
		t.Fatal(err)
	}
	for i := range 5 {
		store.AppendMessage(session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: fmt.Sprintf("question %d", i)}}})
		store.AppendMessage(session.Message{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartText, Text: fmt.Sprintf("answer %d", i)}}})
	}
	path := store.Path()
	store.Close()

	entries, err := TailEntries(path, 2)
	if err != nil {
		t.Fatal(err)
	}
	// The last two user turns, each with its answer, and nothing from earlier
	// turns.
	got := make([]string, len(entries))
	for i, entry := range entries {
		got[i] = entry.Message.Text()
	}
	if want := []string{"question 3", "answer 3", "question 4", "answer 4"}; !slices.Equal(got, want) {
		t.Fatalf("TailEntries = %q, want %q", got, want)
	}
}

func TestTailEntriesReturnsWholeSessionWhenShorter(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	store, err := New(root, cwd, "test", "system", typedid.SessionID{})
	if err != nil {
		t.Fatal(err)
	}
	store.AppendMessage(session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: "only question"}}})
	store.AppendMessage(session.Message{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartText, Text: "only answer"}}})
	path := store.Path()
	store.Close()

	entries, err := TailEntries(path, 5)
	if err != nil {
		t.Fatal(err)
	}
	if len(entries) != 3 || entries[0].Message.Role != session.RoleSystem || entries[2].Message.Text() != "only answer" {
		t.Fatalf("TailEntries = %#v", entries)
	}
}

func TestTailEntriesFollowsParentChainInWindow(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	store, err := New(root, cwd, "test", "system", typedid.SessionID{})
	if err != nil {
		t.Fatal(err)
	}
	store.AppendMessage(session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: "first"}}})
	store.AppendMessage(session.Message{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartText, Text: "answer one"}}})
	store.AppendMessage(session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: "second"}}})
	store.AppendMessage(session.Message{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartText, Text: "answer two"}}})
	path := store.Path()
	store.Close()

	// A window of two turns must return the parent chain, not just the last two
	// records, and must preserve conversation order.
	entries, err := TailEntries(path, 2)
	if err != nil {
		t.Fatal(err)
	}
	got := make([]string, 0, len(entries))
	for _, entry := range entries {
		if entry.Message != nil {
			got = append(got, string(entry.Message.Role)+":"+entry.Message.Text())
		}
	}
	want := []string{"user:first", "assistant:answer one", "user:second", "assistant:answer two"}
	if strings.Join(got, ",") != strings.Join(want, ",") {
		t.Fatalf("TailEntries = %v, want %v", got, want)
	}
}

// TestTailEntriesReadsAcrossBlocks puts the requested turn more than one block
// back: its answer alone is longer than a block, so the backwards reader must
// keep every block it reads to return the whole answer. The fixture also lands
// a block boundary exactly where the question's line begins. The reader cannot
// tell that line from one cut mid-record, so it must not count it until a read
// reaches further back.
func TestTailEntriesReadsAcrossBlocks(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	store, err := New(root, cwd, "test", "system", typedid.SessionID{})
	if err != nil {
		t.Fatal(err)
	}
	store.AppendMessage(session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: "old question"}}})
	oldAnswer, err := store.AppendMessage(session.Message{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartText, Text: "old answer"}}})
	if err != nil {
		t.Fatal(err)
	}
	path := store.Path()
	store.Close()

	// The recent turn is written by hand with a fixed timestamp, so its line
	// lengths are known before it is written. session.Entry IDs have a fixed length, and
	// each "x" adds one byte, so the answer can be sized to make the question
	// and answer lines span exactly two blocks.
	record := func(parent typedid.EntryID, role session.Role, text string) (typedid.EntryID, []byte) {
		id, err := typedid.NewEntryID()
		if err != nil {
			t.Fatal(err)
		}
		message := session.TextMessage(role, text)
		b, err := json.Marshal(session.Entry{Type: session.EntryTypeMessage, ID: id, ParentID: &parent, Timestamp: time.Date(2026, 1, 1, 0, 0, 0, 0, time.UTC), Message: &message})
		if err != nil {
			t.Fatal(err)
		}
		return id, append(b, '\n')
	}
	questionID, question := record(oldAnswer, session.RoleUser, "recent question")
	_, probe := record(questionID, session.RoleAssistant, "x")
	large := strings.Repeat("x", 2*tailBlock-len(question)-len(probe)+1)
	_, answer := record(questionID, session.RoleAssistant, large)
	if len(question)+len(answer) != 2*tailBlock {
		t.Fatalf("recent turn is %d bytes, want %d", len(question)+len(answer), 2*tailBlock)
	}
	f, err := os.OpenFile(path, os.O_APPEND|os.O_WRONLY, 0)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := f.Write(append(question, answer...)); err != nil {
		t.Fatal(err)
	}
	if err := f.Close(); err != nil {
		t.Fatal(err)
	}

	entries, err := TailEntries(path, 1)
	if err != nil {
		t.Fatal(err)
	}
	if len(entries) != 2 || entries[0].Message.Text() != "recent question" || entries[1].Message.Text() != large {
		got := make([]string, len(entries))
		for i, entry := range entries {
			got[i] = fmt.Sprintf("%s:%.20q (%d bytes)", entry.Message.Role, entry.Message.Text(), len(entry.Message.Text()))
		}
		t.Fatalf("TailEntries across blocks = %v, want the recent question and its whole answer", got)
	}
}

func TestSummaryTitleIsFirstUserMessage(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	store, err := New(root, cwd, "test", "system", typedid.SessionID{})
	if err != nil {
		t.Fatal(err)
	}
	store.AppendMessage(session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: "\n  Fix the flaky test  \nmore detail"}}})
	store.Close()

	summaries, err := Discover(root, cwd)
	if err != nil {
		t.Fatal(err)
	}
	if len(summaries) != 1 || summaries[0].Title != "Fix the flaky test" {
		t.Fatalf("title = %#v", summaries)
	}
}

func TestLatestAndFind(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	if _, ok, err := Latest(root, cwd); err != nil || ok {
		t.Fatalf("Latest on empty root = (%v, %v), want (zero, false)", ok, err)
	}
	first, err := New(root, cwd, "test", "system", typedid.SessionID{})
	if err != nil {
		t.Fatal(err)
	}
	firstID := first.ID()
	if _, err := first.AppendMessage(session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: "keep me"}}}); err != nil {
		t.Fatal(err)
	}
	first.Close()

	latest, ok, err := Latest(root, cwd)
	if err != nil || !ok || latest.ID != firstID {
		t.Fatalf("Latest = (%#v, %v, %v), want the only session", latest, ok, err)
	}
	found, err := Find(root, cwd, firstID)
	if err != nil || found.Path != latest.Path {
		t.Fatalf("Find = (%#v, %v)", found, err)
	}
	missing, err := typedid.ParseSessionID("ses_00000000000000000000")
	if err != nil {
		t.Fatal(err)
	}
	if _, err := Find(root, cwd, missing); err == nil {
		t.Fatal("Find returned a session that does not exist")
	}
}

func TestDiscoverSkipsUnreadableFiles(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	store, err := New(root, cwd, "test", "system", typedid.SessionID{})
	if err != nil {
		t.Fatal(err)
	}
	path := store.Path()
	if _, err := store.AppendMessage(session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: "content"}}}); err != nil {
		t.Fatal(err)
	}
	store.Close()
	if err := os.WriteFile(path[:len(path)-len(".jsonl")]+"-corrupt.jsonl", []byte("not a session\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	summaries, err := Discover(root, cwd)
	if err != nil {
		t.Fatal(err)
	}
	if len(summaries) != 1 {
		t.Fatalf("Discover returned %d sessions, want the one valid file", len(summaries))
	}
}

// An empty session is written without fsync, so a power loss can leave it
// truncated anywhere. Resume must still find the last real session.
func TestLatestSkipsTornEmptySessions(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	kept, err := New(root, cwd, "test", "system", typedid.SessionID{})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := kept.AppendMessage(session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: "content"}}}); err != nil {
		t.Fatal(err)
	}
	kept.Close()

	torn, err := New(root, cwd, "test", "system", typedid.SessionID{})
	if err != nil {
		t.Fatal(err)
	}
	path := torn.Path()
	b, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	for _, size := range []int{0, len(b) / 2} {
		if err := os.WriteFile(path, b[:size], 0o600); err != nil {
			t.Fatal(err)
		}
		summary, ok, err := Latest(root, cwd)
		if err != nil || !ok || summary.ID != kept.ID() {
			t.Fatalf("Latest with %d-byte torn session = %v, %v, %v; want %s", size, summary.ID, ok, err, kept.ID())
		}
	}
	torn.Close()
}

func TestSubagentSessionRecordsParentAndIsNotLatest(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	parent, err := New(root, cwd, "test", "system", typedid.SessionID{})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := parent.AppendMessage(session.TextMessage(session.RoleUser, "parent task")); err != nil {
		t.Fatal(err)
	}
	if err := parent.Close(); err != nil {
		t.Fatal(err)
	}
	child, err := New(root, cwd, "test", "system", parent.ID())
	if err != nil {
		t.Fatal(err)
	}
	if _, err := child.AppendMessage(session.TextMessage(session.RoleUser, "subtask")); err != nil {
		t.Fatal(err)
	}
	if err := child.Close(); err != nil {
		t.Fatal(err)
	}
	found, err := Find(root, cwd, child.ID())
	if err != nil || found.Parent != parent.ID() {
		t.Fatalf("child summary = %#v, %v", found, err)
	}
	latest, ok, err := Latest(root, cwd)
	if err != nil || !ok || latest.ID != parent.ID() {
		t.Fatalf("latest = %s, %v, %v; want the parent, not the newer subagent", latest.ID, ok, err)
	}
}

func TestDiscoverReportsSessionsInUse(t *testing.T) {
	root, cwd := t.TempDir(), t.TempDir()
	store, err := New(root, cwd, "test", "system", typedid.SessionID{})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := store.AppendMessage(session.TextMessage(session.RoleUser, "hello")); err != nil {
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
