package ui

import (
	"strings"
	"testing"

	"github.com/charmbracelet/x/ansi"
)

// TestInputGrowsWithContent drives the DynamicHeight textarea through the
// resize path: a long prompt grows the input (soft-wrapped rows included),
// the viewport yields the difference, and clearing the prompt shrinks both
// back. The cap bounds the input so the transcript keeps its rows.
func TestInputGrowsWithContent(t *testing.T) {
	model := newTestModel(t)
	if model.input.Height() != 1 || model.viewport.Height() != 21 {
		t.Fatalf("initial layout: input %d, viewport %d", model.input.Height(), model.viewport.Height())
	}

	// A long single line soft-wraps into five visual rows at width 80 (the
	// prompt column shifts the wrap point).
	model.input.SetValue(strings.Repeat("word ", 16*4))
	model.resize()
	if got := model.input.Height(); got != 5 {
		t.Fatalf("soft-wrapped prompt: input height = %d, want 5", got)
	}
	if got := model.viewport.Height(); got != 17 {
		t.Fatalf("soft-wrapped prompt: viewport height = %d, want 17", got)
	}

	// The input shrinks again when the prompt shrinks.
	model.input.SetValue("short")
	model.resize()
	if got := model.input.Height(); got != 1 {
		t.Fatalf("short prompt: input height = %d, want 1", got)
	}

	// The cap bounds the input no matter how much is typed; the viewport keeps
	// at least the rows the frame needs for bars and the input itself.
	model.input.SetValue(strings.Repeat("line\n", 30))
	model.resize()
	if got := model.input.Height(); got != maxInputLines {
		t.Fatalf("capped prompt: input height = %d, want %d", got, maxInputLines)
	}

	// A short window shrinks the cap rather than squeezing the transcript to
	// nothing: the viewport keeps at least one row.
	small := newTestModel(t)
	small.width, small.height = 80, 6
	small.input.SetValue(strings.Repeat("line\n", 30))
	small.resize()
	if got := small.input.Height(); got > maxInputLines {
		t.Fatalf("small window: input height = %d, over the cap", got)
	}
	if got := small.viewport.Height(); got < 1 {
		t.Fatalf("small window: viewport height = %d, want at least 1", got)
	}
}

// TestInputViewInsetsAndAligns pins the input's frame alignment: no ">" prompt
// glyph, one cell of padding on each side, and every line filled to the same
// width so the block reads as one slab.
func TestInputViewInsetsAndAligns(t *testing.T) {
	model := newTestModel(t)
	model.input.SetValue("first\nsecond")
	model.resize()
	full := inputView(model.input.View(), model.width)
	lines := strings.Split(full, "\n")
	if len(lines) != 2 {
		t.Fatalf("input view = %d lines, want 2", len(lines))
	}
	for i, line := range lines {
		if got := ansi.StringWidth(line); got != model.width {
			t.Fatalf("line %d width = %d, want %d (full viewport width)", i, got, model.width)
		}
		if !strings.HasPrefix(line, " ") {
			t.Fatalf("line %d lacks the left inset: %q", i, plain(line))
		}
		if strings.Contains(line, ">") {
			t.Fatalf("line %d still shows a prompt glyph: %q", i, plain(line))
		}
	}
	// There is no prompt glyph: the block's left inset is the only padding, so
	// the caret column sits one cell from the edge, inside the two-cell slab
	// inset above.
	if model.input.Prompt != "" {
		t.Fatalf("input prompt = %q, want empty", model.input.Prompt)
	}
	if got := plain(lines[0]); !strings.HasPrefix(got, " first") {
		t.Fatalf("line 0 = %q, want text one cell in", got)
	}
}
