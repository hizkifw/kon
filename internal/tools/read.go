package tools

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"
	"unicode/utf8"

	"github.com/hizkifw/kon/internal/session"
)

const (
	maxReadBytes  = 1024 * 1024
	maxReadLines  = 2000
	maxImageBytes = 5 * 1024 * 1024
)

// readTool reads a bounded window of a UTF-8 text file with line numbers, or
// loads an image whole for models with vision.
type readTool struct{}

type readDetails struct {
	LineCount int  `json:"line_count"`
	EmptyFile bool `json:"empty_file,omitempty"`
}

func (readTool) Definition() session.ToolDefinition {
	return session.ToolDefinition{
		Name:        "read",
		Description: "Read a UTF-8 text file with one-based line offsets, or load an image (png, jpeg, gif, webp) whole for models with vision.",
		Parameters:  json.RawMessage(`{"type":"object","properties":{"path":{"type":"string"},"offset":{"type":"integer","minimum":1},"limit":{"type":"integer","minimum":1,"maximum":2000}},"required":["path"],"additionalProperties":false}`),
	}
}

// Interrupt is a no-op: reading is synchronous and nothing runs in the
// background between calls.
func (readTool) Interrupt(int) bool { return false }

// Summarize renders the request line: the path relative to cwd, plus the
// offset when the model started reading mid-file.
func (readTool) Summarize(raw json.RawMessage, cwd string) string {
	args := struct {
		Path   string `json:"path"`
		Offset int    `json:"offset"`
	}{Offset: 1}
	if err := json.Unmarshal(raw, &args); err != nil {
		return FallbackSummary(raw)
	}
	summary := prettyPath(args.Path, cwd)
	if args.Offset > 1 {
		summary += fmt.Sprintf(" from line %d", args.Offset)
	}
	return summary
}

// Describe renders the call: successful reads collapse to a line count and
// carry no body, so file contents never reach the transcript. Older sessions
// without details still derive the count from numbered rows.
func (t readTool) Describe(raw json.RawMessage, result string, failed bool, details json.RawMessage, cwd string) Display {
	summary := t.Summarize(raw, cwd)
	if failed {
		return failureDisplay(summary, result)
	}
	note := ""
	var meta readDetails
	if len(details) > 0 && json.Unmarshal(details, &meta) == nil && meta.LineCount > 0 {
		switch {
		case meta.EmptyFile:
			note = "empty file"
		case meta.LineCount == 1:
			note = "1 line"
		default:
			note = fmt.Sprintf("%d lines", meta.LineCount)
		}
	} else {
		note = readNote(result)
	}
	if note == "" {
		lines, more := tailLines(result, maxToolLines)
		return Display{State: StateDone, Summary: summary, Lines: lines, More: more}
	}
	return Display{State: StateDone, Summary: summary, Note: note}
}

// readNote summarizes a successful text read by its line count from the
// numbered rows the tool renders. Rows look like "    12  text"; a trailing
// "… N more lines" marks truncated output. An empty note means the result
// could not be summarized (an offset notice, say) and is shown as-is.
func readNote(text string) string {
	lines := strings.Split(text, "\n")
	extra := 0
	if last := lines[len(lines)-1]; strings.HasPrefix(last, "…") && strings.HasSuffix(last, "more lines") {
		fields := strings.Fields(last)
		if len(fields) >= 2 {
			if count, err := strconv.Atoi(strings.TrimPrefix(fields[1], "…")); err == nil {
				extra = count
				lines = lines[:len(lines)-1]
			}
		}
	}
	if len(lines) == 0 {
		return ""
	}
	lastRow := lines[len(lines)-1]
	if len(lastRow) < 6 {
		return ""
	}
	number, err := strconv.Atoi(strings.TrimSpace(lastRow[:6]))
	if err != nil {
		return ""
	}
	if number == 1 && extra == 0 && len(lastRow) <= 8 {
		return "empty file"
	}
	total := number + extra
	if total == 1 {
		return "1 line"
	}
	return fmt.Sprintf("%d lines", total)
}

func (t readTool) Run(_ context.Context, env Env, raw json.RawMessage) (Result, error) {
	args := struct {
		Path   string `json:"path"`
		Offset int    `json:"offset"`
		Limit  int    `json:"limit"`
	}{Offset: 1, Limit: maxReadLines}
	if err := decodeArgs(raw, &args); err != nil {
		return Result{}, err
	}
	path, err := env.Resolve(args.Path)
	if err != nil {
		return Result{}, err
	}
	// A file larger than any mode can accept is rejected from its size and a
	// short prefix, never loaded whole: reading a multi-gigabyte file just to
	// refuse it would spike memory for nothing. The prefix still carries the
	// image magic, so an oversized image is named as such rather than as an
	// oversized text file.
	if info, statErr := os.Stat(path); statErr == nil && info.Mode().IsRegular() && info.Size() > maxImageBytes {
		if err := rejectOversize(path); err != nil {
			return Result{}, err
		}
	}
	// Read once and route by content, not extension: any real image is an
	// image whatever it is named (or not named), and binary data never reaches
	// the text limits or the UTF-8 check with a confusing message.
	b, err := os.ReadFile(path)
	if err != nil {
		return Result{}, err
	}
	if mime := detectImageMIME(b); mime != "" {
		return t.imageResult(path, b, mime, env.vision)
	}
	if _, format := detectUnsendableImageMIME(b); format != "" {
		return Result{}, fmt.Errorf("%s is a %s image; kon can attach png, jpeg, gif, or webp images — convert it first", path, format)
	}
	return t.readText(args, path, b)
}

// rejectOversize fails a file too large for any read mode, naming it as an
// unsupported image when its prefix is a known-but-unsendable format and as an
// oversized text file otherwise. It reads only a short prefix, so a huge file is
// never loaded to be refused.
func rejectOversize(path string) error {
	prefix, err := readPrefix(path, 512)
	if err != nil {
		return err
	}
	if _, format := detectUnsendableImageMIME(prefix); format != "" {
		return fmt.Errorf("%s is a %s image; kon can attach png, jpeg, gif, or webp images — convert it first", path, format)
	}
	if mime := detectImageMIME(prefix); mime != "" {
		return fmt.Errorf("%s is larger than %d bytes", path, maxImageBytes)
	}
	return fmt.Errorf("file is larger than %d bytes", maxReadBytes)
}

// readPrefix reads at most n leading bytes of a file.
func readPrefix(path string, n int) ([]byte, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	buf := make([]byte, n)
	read, err := io.ReadFull(f, buf)
	if err != nil && !errors.Is(err, io.EOF) && !errors.Is(err, io.ErrUnexpectedEOF) {
		return nil, err
	}
	return buf[:read], nil
}

// imageResult attaches the whole file as an image part. Offset and limit do
// not apply to binary content. A vision-disabled configuration returns a text
// notice instead of the bytes, so the model can react rather than retry.
func (t readTool) imageResult(path string, b []byte, mime string, vision bool) (Result, error) {
	if len(b) > maxImageBytes {
		return Result{}, fmt.Errorf("%s is larger than %d bytes", path, maxImageBytes)
	}
	if !vision {
		return Result{Content: fmt.Sprintf("%s is an image; the active model is not configured with vision, so its pixels cannot be inspected. Treat it as an opaque binary file.", path)}, nil
	}
	return Result{
		Content: fmt.Sprintf("loaded image %s (%s, %d bytes)", path, mime, len(b)),
		Images:  []Image{{Data: b, MIME: mime}},
	}, nil
}

// readText renders the numbered line window of a UTF-8 text file.
func (t readTool) readText(args struct {
	Path   string `json:"path"`
	Offset int    `json:"offset"`
	Limit  int    `json:"limit"`
}, path string, b []byte) (Result, error) {
	if args.Offset < 1 || args.Limit < 1 || args.Limit > maxReadLines {
		return Result{}, errors.New("offset must be at least 1 and limit must be between 1 and 2000")
	}
	if len(b) > maxReadBytes {
		return Result{}, fmt.Errorf("file is larger than %d bytes", maxReadBytes)
	}
	if bytes.IndexByte(b, 0) >= 0 || !utf8.Valid(b) {
		return Result{}, errors.New("file is not UTF-8 text")
	}
	lines := strings.Split(string(b), "\n")
	start := args.Offset - 1
	if start >= len(lines) {
		return Result{Content: fmt.Sprintf("(offset %d is beyond end of file; %d lines)", args.Offset, len(lines))}, nil
	}
	end := min(start+args.Limit, len(lines))
	var out strings.Builder
	for i := start; i < end; i++ {
		fmt.Fprintf(&out, "%6d  %s", i+1, lines[i])
		if i+1 < end {
			out.WriteByte('\n')
		}
	}
	if end < len(lines) {
		fmt.Fprintf(&out, "\n… %d more lines", len(lines)-end)
	}
	details, _ := json.Marshal(readDetails{LineCount: len(lines), EmptyFile: len(b) == 0})
	return Result{Content: out.String(), Details: details}, nil
}
