package tui

import (
	"math/rand"
	"strings"
	"testing"

	"charm.land/lipgloss/v2"
)

// TestPlainWrapperMatchesLipgloss checks the streaming word wrapper against
// lipgloss.Wrap as a reference, so its handling of wide characters, hyphens,
// and hard wraps stays conventional as the fast paths change.
func TestPlainWrapperMatchesLipgloss(t *testing.T) {
	inputs := []string{
		"",
		"hello",
		"hello world",
		"word ",
		"a  b   c",
		strings.Repeat("word ", 40),
		"supercalifragilisticexpialidocious",
		strings.Repeat("a", 50),
		"one\ntwo\n\nthree",
		"trailing spaces   \nnext",
		"  leading",
		"tab\there",
		"hyphen-ated words here",
		"end-with-hyphen-",
		"你好 世界 你好 世界 foo",
		"emoji 😀 test 🎉 here now",
	}
	rng := rand.New(rand.NewSource(1))
	for i := 0; i < 300; i++ {
		var b strings.Builder
		for j := 0; j < rng.Intn(60); j++ {
			switch rng.Intn(6) {
			case 0:
				b.WriteByte(' ')
			case 1:
				b.WriteByte('\n')
			case 2:
				b.WriteByte('-')
			case 3:
				b.WriteString("你好")
			default:
				b.WriteByte(byte('a' + rng.Intn(26)))
			}
		}
		inputs = append(inputs, b.String())
	}
	for _, width := range []int{1, 2, 5, 10, 17, 40, 118} {
		for _, in := range inputs {
			want := strings.Split(lipgloss.Wrap(in, width, ""), "\n")
			got := WrapPlain(in, width)
			if !equalLines(got, want) {
				t.Fatalf("width=%d input=%q\n got=%q\nwant=%q", width, in, got, want)
			}
		}
	}
}

func equalLines(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

// TestStripperDropsEscapes pins what a Stripper removes: escapes and control
// bytes go, newlines and tabs survive, and plain text passes through
// untouched.
func TestStripperDropsEscapes(t *testing.T) {
	cases := []struct{ in, want string }{
		{"plain", "plain"},
		{"\x1b[31mred\x1b[0m", "red"},
		{"a\x1b[0mb", "ab"},
		{"\x1b]8;;http://x\x1b\\link\x1b[m", "link"},
		{"bell\x07end", "bellend"},
		{"keep\nnewlines\r\nand\ttabs", "keep\nnewlines\r\nand\ttabs"},
		{"\x1b", ""},                           // bare escape ends the delta
		{"\x1b[", ""},                          // unterminated CSI ends the delta
		{"\x1b]8;;unterminated", ""},           // unterminated OSC ends the delta
		{"\x1bMtwo-byte", "two-byte"},          // ESC M consumes only itself
		{"\x1b(Bintermediate", "intermediate"}, // ESC ( B is a three-byte escape
	}
	for _, tc := range cases {
		var s Stripper
		if got := s.Strip(tc.in); got != tc.want {
			t.Errorf("Strip(%q) = %q, want %q", tc.in, got, tc.want)
		}
	}
}
