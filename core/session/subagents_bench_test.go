package session

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
	"time"

	"github.com/hizkifw/kon/core/typedid"
)

// The fixture is a workspace that has seen many sessions, beside a parent
// that delegated tool-heavy work to subagents, half of which delegated again.
const (
	benchUnrelated = 2000
	benchSubagents = 50
	benchTurns     = 200    // tool calls in each subagent session
	benchToolBytes = 10_000 // bytes of output per tool call
)

// benchBody is a session's records after its header: a task, then turns
// that each call a tool and get its output back.
func benchBody(tb testing.TB, turns, toolBytes int) []byte {
	tb.Helper()
	var out bytes.Buffer
	var parent *typedid.EntryID
	add := func(message Message) {
		id, err := typedid.NewEntryID()
		if err != nil {
			tb.Fatal(err)
		}
		line, err := json.Marshal(Entry{Type: EntryTypeMessage, ID: id, ParentID: parent, Timestamp: time.Unix(0, 0).UTC(), Message: &message})
		if err != nil {
			tb.Fatal(err)
		}
		out.Write(append(line, '\n'))
		parent = &id
	}
	add(TextMessage(RoleSystem, "system"))
	add(TextMessage(RoleUser, "task"))
	output := strings.Repeat("x", toolBytes)
	for i := range turns {
		call := typedid.ToolCallID(fmt.Sprintf("call-%d", i))
		add(Message{Role: RoleAssistant, Parts: []Part{{Type: PartToolCall, ToolCallID: call, ToolName: "read", ToolInput: json.RawMessage(`{"path":"a"}`)}},
			Usage: &Usage{PromptTokens: 1000, CompletionTokens: 10, TotalTokens: 1010, Cost: 0.001}})
		add(ToolResultMessage(call, "read", output))
	}
	return out.Bytes()
}

// writeSession writes body under a fresh header in dir, named for a time n
// seconds into 2026, and returns the new session's ID.
func writeSession(tb testing.TB, dir string, n int, parent typedid.SessionID, body []byte) typedid.SessionID {
	tb.Helper()
	id, err := typedid.NewSessionID()
	if err != nil {
		tb.Fatal(err)
	}
	created := time.Date(2026, 1, 1, 0, 0, 0, 0, time.UTC).Add(time.Duration(n) * time.Second)
	header, err := json.Marshal(Header{Type: "session", Version: SchemaVersion, ID: id, AppVersion: "bench", Timestamp: created, CWD: "/w", Parent: parent})
	if err != nil {
		tb.Fatal(err)
	}
	name := created.Format("20060102T150405.000Z") + "_" + id.String() + fileSuffix
	if err := os.WriteFile(filepath.Join(dir, name), append(append(header, '\n'), body...), 0o600); err != nil {
		tb.Fatal(err)
	}
	return id
}

// benchWorkspace builds the fixture and returns the parent session's path
// and ID.
func benchWorkspace(b *testing.B) (string, typedid.SessionID) {
	b.Helper()
	root, cwd := b.TempDir(), b.TempDir()
	dir, err := directoryFor(root, cwd)
	if err != nil {
		b.Fatal(err)
	}
	if err := os.MkdirAll(dir, 0o700); err != nil {
		b.Fatal(err)
	}
	small, large := benchBody(b, 3, 200), benchBody(b, benchTurns, benchToolBytes)
	n := 0
	for range benchUnrelated {
		writeSession(b, dir, n, typedid.SessionID{}, small)
		n++
	}
	parentAt := n
	parent := writeSession(b, dir, n, typedid.SessionID{}, small)
	n++
	for i := range benchSubagents {
		p := parent
		if i%2 == 1 {
			p = writeSession(b, dir, n, parent, large)
			n++
		}
		writeSession(b, dir, n, p, large)
		n++
	}
	name := time.Date(2026, 1, 1, 0, 0, 0, 0, time.UTC).Add(time.Duration(parentAt)*time.Second).Format("20060102T150405.000Z") + "_" + parent.String() + fileSuffix
	return filepath.Join(dir, name), parent
}

// BenchmarkSubagentsFirstRead is opening a session whose subagents have
// already run: every header in the workspace, then every subagent session
// whole. It runs off the UI goroutine.
func BenchmarkSubagentsFirstRead(b *testing.B) {
	path, parent := benchWorkspace(b)
	for b.Loop() {
		NewSubagents(path, parent).Usage()
	}
}

// BenchmarkSubagentsIdleRead is the once-a-second read while nothing new
// was written. retained-MiB is what following every subagent keeps in
// memory, which must not grow with their sessions.
func BenchmarkSubagentsIdleRead(b *testing.B) {
	path, parent := benchWorkspace(b)
	var before, after runtime.MemStats
	runtime.GC()
	runtime.ReadMemStats(&before)
	subagents := NewSubagents(path, parent)
	want := subagents.Usage()
	runtime.GC()
	runtime.ReadMemStats(&after)
	for b.Loop() {
		if got := subagents.Usage(); got != want {
			b.Fatalf("usage = %+v, want %+v", got, want)
		}
	}
	b.ReportMetric(float64(after.HeapAlloc-before.HeapAlloc)/(1<<20), "retained-MiB")
}
