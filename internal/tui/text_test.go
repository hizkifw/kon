package tui

import "testing"

func TestSanitizeRemovesTerminalEscapes(t *testing.T) {
	for _, test := range []struct{ name, in, want string }{
		{"csi and bel", "plain\x1b[31mred\x1b[0m\x07", "plainred"},
		{"osc title ended by bel", "a\x1b]0;evil title\x07b", "ab"},
		{"osc hyperlink ended by st", "see \x1b]8;;https://x.test\x1b\\link\x1b]8;;\x1b\\ now", "see link now"},
		{"two-byte escapes", "a\x1b(Bb\x1bcc", "abc"},
		{"c1 csi and del", "a\u009b2Jb\x7fc", "a2Jbc"},
		{"carriage return", "line\r\nnext", "line\nnext"},
		{"text kept", "tab\tnew\nline °é", "tab\tnew\nline °é"},
	} {
		if got := Sanitize(test.in); got != test.want {
			t.Errorf("%s: Sanitize(%q) = %q, want %q", test.name, test.in, got, test.want)
		}
	}
}

func TestFitHonorsCellWidth(t *testing.T) {
	// Wide runes take two cells each, so counting runes would overflow the
	// width with them.
	for _, test := range []struct {
		in    string
		width int
		want  string
	}{
		{"123456", 4, "123…"},
		{"你好世界", 5, "你好…"},
		{"你好世界", 4, "你…"},
	} {
		if got := Fit(test.in, test.width); got != test.want {
			t.Errorf("Fit(%q, %d) = %q, want %q", test.in, test.width, got, test.want)
		}
	}
}
