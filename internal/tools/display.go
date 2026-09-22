package tools

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// maxToolLines bounds how many body lines of a tool call the transcript shows.
// Each tool trims its own display to this tail; the full output still reaches
// the model through Result.Content.
const maxToolLines = 6

// DisplayState classifies a tool call for the transcript painter: the icon and
// body-line colors derive from it, not from the tool's name.
type DisplayState uint8

const (
	StateRunning DisplayState = iota // call in flight
	StateDone                        // completed successfully
	StateFailed                      // completed with an error
)

// Display is one tool call's presentation snapshot, owned by the tool. The
// transcript renders it verbatim: Summary and Note are short single-line
// strings (painted on the request line), Lines is the trimmed body, and More
// counts the lines Lines omits. Tools build snapshots either from a finished
// call (Describe) or, for long-running tools, while the call is in flight.
type Display struct {
	Summary string       // request-line rendering of the call, e.g. "internal/ui/view.go from 100"
	Note    string       // outcome tail on the request line, e.g. "12 lines" or "exit 0 · took 1.2s"
	State   DisplayState // running, done, or failed
	Lines   []string     // body lines below the request line, already trimmed by the tool
	More    int          // how many body lines Lines omits
}

// Displayer lets a tool own how its calls are presented. Both methods are pure
// functions of their arguments — no filesystem access, no clock — so the
// transcript can rebuild the same display when replaying a persisted session.
// The optional interface is satisfied by the built-in tools; a tool that does
// not implement it gets the generic fallback in Describe.
type Displayer interface {
	// Summarize renders the one-line request summary from the raw arguments.
	// It is shown while the call runs and reused as the request line of the
	// finished display.
	Summarize(args json.RawMessage, cwd string) string
	// Describe renders a finished call. result is the persisted Result.Content
	// verbatim and failed reports whether the call errored.
	Describe(args json.RawMessage, result string, failed bool, cwd string) Display
}

// Describe resolves the display for a tool call through the tool that owns it,
// falling back to a generic rendering for unknown names and tools without a
// Displayer. cwd anchors relative path shortening.
func Describe(name string, args json.RawMessage, result string, failed bool, cwd string) Display {
	if tool, ok := defaultDisplays.Lookup(name); ok {
		if d, ok := tool.(Displayer); ok {
			return d.Describe(args, result, failed, cwd)
		}
	}
	summary := FallbackSummary(args)
	if failed {
		return failureDisplay(summary, result)
	}
	lines, more := tailLines(result, maxToolLines)
	return Display{State: StateDone, Summary: summary, Lines: lines, More: more}
}

// FallbackSummary renders arguments the generic way: compact JSON when
// possible, the raw string otherwise. It is also the summary of last resort
// inside tools whose arguments do not parse.
func FallbackSummary(args json.RawMessage) string {
	var raw map[string]any
	if err := json.Unmarshal(args, &raw); err != nil {
		return string(args)
	}
	if compact, err := json.Marshal(raw); err == nil {
		return string(compact)
	}
	return string(args)
}

// failureDisplay is the shared failed-call rendering: the request summary over
// the error message tail. Tools with structured failures (shell's exit code)
// build their own.
func failureDisplay(summary, result string) Display {
	lines, more := tailLines(result, maxToolLines)
	return Display{State: StateFailed, Summary: summary, Lines: lines, More: more}
}

// tailLines returns at most n lines, dropping trailing blank lines. It is the
// display-side trim every tool applies to its body.
func tailLines(text string, n int) ([]string, int) {
	lines := splitDisplayLines(text)
	for len(lines) > 0 && strings.TrimSpace(lines[len(lines)-1]) == "" {
		lines = lines[:len(lines)-1]
	}
	if len(lines) <= n {
		return lines, 0
	}
	kept := lines[len(lines)-n:]
	return kept, len(lines) - n
}

// splitDisplayLines splits text on line breaks, treating CRLF as a single
// newline and a lone CR (spinner-style \r output) as a line break.
func splitDisplayLines(text string) []string {
	text = strings.ReplaceAll(text, "\r\n", "\n")
	text = strings.ReplaceAll(text, "\r", "\n")
	return strings.Split(text, "\n")
}

// normalizeDisplay trims raw tool output for display: trailing spaces per
// line, leading and trailing blank lines, and blank-run collapse are left to
// the transcript's text normalizer; here CR variants become line breaks and
// nothing else, so output keeps its shape.
func normalizeDisplay(text string) string {
	return strings.Join(splitDisplayLines(text), "\n")
}

// prettyPath shortens a path for display: relative to the working directory
// when possible, otherwise with the home directory abbreviated.
func prettyPath(path, cwd string) string {
	if path == "" {
		return ""
	}
	if cwd != "" {
		if rel, err := filepath.Rel(cwd, path); err == nil && rel != ".." && !strings.HasPrefix(rel, ".."+string(filepath.Separator)) {
			return rel
		}
	}
	return abbreviateHome(path)
}

// abbreviateHome replaces the user's home directory prefix with ~.
func abbreviateHome(path string) string {
	if home, err := os.UserHomeDir(); err == nil && (path == home || strings.HasPrefix(path, home+string(os.PathSeparator))) {
		return "~" + strings.TrimPrefix(path, home)
	}
	return path
}

// countLines counts lines in a string the way tools count argument sizes.
func countLines(s string) int {
	if s == "" {
		return 0
	}
	return strings.Count(s, "\n") + 1
}

// humanBytes renders a byte count for a request line, e.g. "1.2KiB".
func humanBytes(n int) string {
	switch {
	case n >= 1<<20:
		return fmt.Sprintf("%.1fMiB", float64(n)/(1<<20))
	case n >= 1<<10:
		return fmt.Sprintf("%.1fKiB", float64(n)/(1<<10))
	default:
		return fmt.Sprintf("%dB", n)
	}
}
