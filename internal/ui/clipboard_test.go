package ui

import (
	"testing"

	"github.com/hizkifw/kon/internal/codetools"
)

// The returned commands are never run here: running one would write to the
// clipboard of whoever runs the tests, and inside tmux to their paste buffers.

func TestCopyTakesTheLatestReplySource(t *testing.T) {
	m := newTestModel(t)
	m.transcript.add(block{kind: blockUser, text: "first"})
	m.transcript.add(block{kind: blockAssistant, text: "old reply"})
	m.transcript.add(block{kind: blockUser, text: "second"})
	m.transcript.add(block{kind: blockAssistant, text: "Looking."})
	m.transcript.add(block{kind: blockResult, name: "read", display: codetools.Display{State: codetools.StateDone}})
	m.transcript.add(block{kind: blockAssistant, text: "Use `x`:\n\n```go\nx := 1\n```"})
	m.transcript.add(block{kind: blockElapsed, text: "Worked for 2s"})

	if got, want := m.transcript.lastReply(), "Use `x`:\n\n```go\nx := 1\n```"; got != want {
		t.Fatalf("last reply = %q, want %q", got, want)
	}
	for _, input := range []string{"/copy", "/copy last", "/copy all"} {
		m.input.SetValue(input)
		updated, cmd := m.submit()
		if cmd == nil || updated.(Model).input.Value() != "" {
			t.Fatalf("%s: cmd = %v, input = %q", input, cmd, updated.(Model).input.Value())
		}
	}
}

func TestCopyWithNothingOrAnUnknownTargetCopiesNothing(t *testing.T) {
	m := newTestModel(t)
	m.transcript.add(block{kind: blockUser, text: "hi"})
	for _, input := range []string{"/copy", "/copy everything"} {
		m.input.SetValue(input)
		updated, cmd := m.submit()
		if cmd != nil || updated.(Model).message == "" {
			t.Fatalf("%s: cmd = %v, status = %q", input, cmd, updated.(Model).message)
		}
	}
}

func TestConversationKeepsTheExchangeOnly(t *testing.T) {
	var tr transcript
	tr.add(block{kind: blockUser, text: "fix it\n\nplease"})
	tr.add(block{kind: blockThinking, text: "hmm"})
	tr.add(block{kind: blockTool, name: "read"})
	tr.add(block{kind: blockResult, name: "read", display: codetools.Display{State: codetools.StateDone, Summary: "a.go", Note: "3 lines"}})
	tr.add(block{kind: blockTool, name: "shell"})
	tr.add(block{kind: blockResult, name: "shell", display: codetools.Display{State: codetools.StateFailed, Summary: "make"}})
	tr.add(block{kind: blockAssistant, text: "Done."})
	tr.add(block{kind: blockElapsed, text: "Worked for 2s"})
	tr.add(block{kind: blockContext, text: "compacted 1k tokens"})
	tr.add(block{kind: blockError, text: "boom\x1b[31m"})

	want := "> fix it\n>\n> please\n\n✓ read a.go · 3 lines\n✗ shell make\n\nDone.\n\nerror: boom"
	if got := tr.conversation(); got != want {
		t.Fatalf("conversation =\n%s\nwant\n%s", got, want)
	}
}
