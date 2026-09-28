package ui

import (
	"context"
	"os"
	"os/exec"
	"runtime"
	"strings"
	"time"

	tea "charm.land/bubbletea/v2"
	"github.com/hizkifw/kon/internal/tools"
)

// copyUsage is /copy's usage line, shown when its argument is not one it
// knows.
const copyUsage = "/copy [last|all]"

// completeCopy offers /copy's two targets.
func completeCopy(_ Model, prefix string) []menuItem {
	var items []menuItem
	for _, item := range []menuItem{
		{Value: "last", Description: "the latest reply, as Markdown"},
		{Value: "all", Description: "the whole conversation"},
	} {
		if strings.HasPrefix(item.Value, prefix) {
			items = append(items, item)
		}
	}
	return items
}

// copyTranscript runs /copy. The screen shows replies wrapped and styled, and
// the mouse is kept for scrolling, so selecting text with it copies neither
// cleanly; this copies the source instead. Only finished messages count: a
// reply still streaming is left out until it ends.
func (m Model) copyTranscript(args []string) (tea.Model, tea.Cmd) {
	target := "last"
	if len(args) > 0 {
		target = args[0]
	}
	var text, name string
	switch target {
	case "last":
		text, name = m.transcript.lastReply(), "the last reply"
	case "all":
		text, name = m.transcript.conversation(), "the conversation"
	default:
		m.status = usageError(copyUsage).Error()
		return m, nil
	}
	m.input.Reset()
	if text == "" {
		m.status = "nothing to copy yet"
		return m, nil
	}
	m.status = "copied " + name
	return m, copyToClipboard(text)
}

// lastReply returns the latest finished assistant message as the Markdown the
// model wrote.
func (t *transcript) lastReply() string {
	for i := len(t.blocks) - 1; i >= 0; i-- {
		if t.blocks[i].kind == blockAssistant {
			return t.blocks[i].text
		}
	}
	return ""
}

// conversation returns the transcript as text to paste elsewhere: prompts
// quoted, replies as the model wrote them, and each finished tool call as the
// line kon run prints for it, a run of calls kept together as on screen.
// Thinking and kon's own notices are not part of the exchange and stay out.
func (t *transcript) conversation() string {
	var parts []string
	inRun := false
	for _, b := range t.blocks {
		switch b.kind {
		case blockUser:
			parts, inRun = append(parts, quoted(b.text)), false
		case blockAssistant:
			parts, inRun = append(parts, b.text), false
		case blockError:
			parts, inRun = append(parts, "error: "+sanitize(b.text)), false
		case blockResult:
			if inRun {
				parts[len(parts)-1] += "\n" + toolLine(b)
			} else {
				parts, inRun = append(parts, toolLine(b)), true
			}
		}
	}
	return strings.Join(parts, "\n\n")
}

// quoted marks each line of a prompt as a Markdown quote, so a pasted
// conversation keeps the user's words apart from the replies.
func quoted(text string) string {
	lines := strings.Split(text, "\n")
	for i, line := range lines {
		if line == "" {
			lines[i] = ">"
		} else {
			lines[i] = "> " + line
		}
	}
	return strings.Join(lines, "\n")
}

// toolLine renders a finished call as one plain line.
func toolLine(b block) string {
	line := "✓ " + b.name
	if b.display.State == tools.StateFailed {
		line = "✗ " + b.name
	}
	if b.display.Summary != "" {
		line += " " + b.display.Summary
	}
	if b.display.Note != "" {
		line += " · " + b.display.Note
	}
	return line
}

// clipboardTimeout bounds a clipboard helper that hangs, such as xclip
// waiting on a display that no longer answers.
const clipboardTimeout = 2 * time.Second

// copyToClipboard puts text on the clipboard of the terminal the user is
// looking at, which over SSH is not the machine kon runs on. The terminal is
// reached with OSC 52, which travels over SSH like any other output; inside
// tmux, which ignores OSC 52 from programs unless set-clipboard is on, tmux
// is asked to send it on instead. Terminals without OSC 52, such as macOS
// Terminal, are covered locally by this machine's own clipboard tool. Neither
// path can confirm the copy landed, so every path that applies is tried.
func copyToClipboard(text string) tea.Cmd {
	return func() tea.Msg {
		ctx, cancel := context.WithTimeout(context.Background(), clipboardTimeout)
		defer cancel()
		if argv := systemClipboard(); argv != nil {
			_ = pipeTo(ctx, text, argv...)
		}
		// load-buffer -w needs tmux 3.2; an older one falls back to OSC 52.
		if os.Getenv("TMUX") != "" && pipeTo(ctx, text, "tmux", "load-buffer", "-w", "-") == nil {
			return nil
		}
		return tea.SetClipboard(text)()
	}
}

// systemClipboard returns the command that writes to this machine's own
// clipboard, or nil when there is none to reach, as on a headless server.
func systemClipboard() []string {
	switch {
	case runtime.GOOS == "darwin":
		return []string{"pbcopy"}
	case os.Getenv("WAYLAND_DISPLAY") != "":
		return []string{"wl-copy"}
	case os.Getenv("DISPLAY") != "":
		if _, err := exec.LookPath("xclip"); err == nil {
			return []string{"xclip", "-selection", "clipboard"}
		}
		return []string{"xsel", "--clipboard", "--input"}
	}
	return nil
}

// pipeTo runs a command with text on its stdin. Its output is discarded
// rather than captured: xclip and wl-copy leave a child behind to serve the
// clipboard, and a captured stdout would keep the command from finishing
// until that child exits.
func pipeTo(ctx context.Context, text string, argv ...string) error {
	cmd := exec.CommandContext(ctx, argv[0], argv[1:]...)
	cmd.Stdin = strings.NewReader(text)
	return cmd.Run()
}
