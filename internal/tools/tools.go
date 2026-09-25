// Package tools contains kon's deliberately small coding tools. Every tool
// implements the Tool interface and registers itself in the central registry
// (registry.go); the Executor resolves model tool calls against that registry.
package tools

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"path/filepath"

	"github.com/hizkifw/kon/internal/session"
)

// Tool is the common interface every coding tool implements. A tool describes
// itself to the model exactly once and interprets its own arguments, so adding
// one means implementing the interface in a new file and registering it in
// defaultRegistry. A tool that also implements Displayer owns its transcript
// presentation (see display.go).
type Tool interface {
	// Definition is the model-facing schema for the tool: its name, a one-line
	// description, and its JSON Schema parameters.
	Definition() session.ToolDefinition
	// Run executes one tool call. arguments is the raw JSON object the model
	// produced. A returned error is reported to the model as a failed tool
	// result; with a nil error the result is the successful output.
	Run(ctx context.Context, env Env, arguments json.RawMessage) (Result, error)
	// Interrupt escalates cancellation for the tool's currently running call:
	// attempt is the number of consecutive interrupt presses, so 1 is the polite
	// "stop cleanly" press and 2 is the harder "you were asked twice" press.
	// Most tools are synchronous and return false; a long-running tool such as
	// shell reports whether it had something to escalate against.
	Interrupt(attempt int) bool
}

// Image is one binary attachment produced by a tool call. The executor encodes
// it for the session and the provider maps it to the wire format for models
// with vision; text-only models see the textual content instead.
type Image struct {
	Data []byte // raw bytes, encoded at the agent boundary
	MIME string // e.g. image/png
}

// Result is one tool call's output. Content is the text the model and the
// transcript see; it stands alone, so providers that cannot send attachments
// (or a vision-disabled configuration) lose nothing textual. Images, when
// non-empty, additionally attach binary content for vision models. Details
// carries tool-owned display metadata so replay need not parse Content.
type Result struct {
	Content string
	Images  []Image
	Details json.RawMessage
	IsError bool
}

// Env is the environment one tool call runs in. Tools keep their runtime
// state on their own instance; Env carries only the workspace context that
// every tool shares. report, when set, is the streaming channel for a
// long-running tool's live display snapshots; the agent coalesces them, so a
// tool may report as often as it likes.
type Env struct {
	cwd string
	// vision reports whether the active model accepts image content. Tools
	// attach images only when it is set; otherwise they describe them in the
	// text result so the model can react (e.g. convert or inspect another way).
	vision bool
	// report receives display snapshots while the call runs. It may be nil
	// (tool calls run fine without a transcript). It must not block; calls
	// happen from the tool's own goroutine.
	report func(Display)
}

// Report publishes one display snapshot for the running call, if a transcript
// is listening. The callback is asynchronous downstream, so this returns as
// soon as the snapshot is handed off.
func (e Env) Report(d Display) {
	if e.report != nil {
		e.report(d)
	}
}

// Resolve turns a tool-supplied path into a clean absolute path under the
// workspace root, rejecting an empty path.
func (e Env) Resolve(path string) (string, error) {
	if path == "" {
		return "", errors.New("path must not be empty")
	}
	if !filepath.IsAbs(path) {
		path = filepath.Join(e.cwd, path)
	}
	return filepath.Clean(path), nil
}

// decodeArgs decodes the one JSON object the model sent into dst, rejecting
// unknown fields and trailing garbage so schema drift fails loudly instead of
// silently ignoring a mistyped argument.
func decodeArgs(raw json.RawMessage, dst any) error {
	dec := json.NewDecoder(bytes.NewReader(raw))
	dec.DisallowUnknownFields()
	if err := dec.Decode(dst); err != nil {
		return fmt.Errorf("invalid arguments: %w", err)
	}
	var extra any
	if err := dec.Decode(&extra); !errors.Is(err, io.EOF) {
		if err == nil {
			return errors.New("invalid arguments: multiple JSON values")
		}
		return fmt.Errorf("invalid arguments: %w", err)
	}
	return nil
}
