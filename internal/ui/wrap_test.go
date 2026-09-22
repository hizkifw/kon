package ui

import (
	"math/rand"
	"strings"
	"testing"

	"charm.land/lipgloss/v2"
)

// TestPlainWrapperMatchesLipgloss guards the streaming word wrapper against
// lipgloss.Wrap, which the block renderers use. If they diverge, a message
// repainted live and later folded into a stable block would shift.
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
			got := wrapPlain(in, width)
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
