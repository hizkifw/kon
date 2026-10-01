// Package tool is the contract between the agent loop and the tools it runs.
// A program builds a Registry of its own tools and hands the agent an
// Executor over it; the agent sees nothing else. kon's coding tools are one
// such set.
package tool

import (
	"context"
	"encoding/json"
	"errors"
	"path/filepath"
	"slices"

	"kon.kitsu.red/core/session"
)

// Tool is one capability the model can call. A tool describes itself to the
// model exactly once and interprets its own arguments. It keeps any runtime
// state on its own instance; Env carries only what every call shares.
type Tool interface {
	// Definition is the model-facing schema for the tool: its name, a one-line
	// description, and its JSON Schema parameters.
	Definition() session.ToolDefinition
	// Run executes one tool call. arguments is the raw JSON object the model
	// produced. A returned error is reported to the model as a failed tool
	// result; with a nil error the result is the successful output.
	Run(ctx context.Context, env Env, arguments json.RawMessage) (Result, error)
}

// Interrupter is implemented by a tool whose calls can outlive a cancelled
// context, such as a shell command that ignores its first signal. Synchronous
// tools need not implement it.
type Interrupter interface {
	// Interrupt escalates cancellation for the tool's currently running call:
	// attempt is the number of consecutive interrupt requests, so 1 is the
	// polite "stop cleanly" request and 2 is the harder "you were asked twice"
	// one. It reports whether there was anything to escalate against.
	Interrupt(attempt int) bool
}

// Media is one binary attachment produced by a tool call: an image, audio
// clip, video, or document. The agent stores it beside the session and the
// provider maps it to the wire format for models that accept its modality;
// other models see the textual content instead.
type Media struct {
	Data []byte // raw bytes, encoded at the agent boundary
	MIME string // e.g. image/png; its type decides the modality
}

// Result is one tool call's output. Content is the text the model and the
// transcript see; it stands alone, so a model that cannot take an attachment
// loses nothing textual. Media, when non-empty, additionally attaches binary
// content. Details carries tool-owned metadata, persisted with the result,
// so a later replay need not parse Content.
type Result struct {
	Content string
	Media   []Media
	Details json.RawMessage
	IsError bool
}

// Env is the environment one tool call runs in.
type Env struct {
	// CWD is the working directory relative paths resolve against.
	CWD string
	// Inputs lists the media the active model accepts. Tools attach media
	// only of these modalities; otherwise they describe it in the text result
	// so the model can react (e.g. convert or inspect another way).
	Inputs []session.Modality
	// Progress receives live snapshots while a long-running call runs. Their
	// type is the tool's own, agreed with whoever presents them. It may be
	// nil, and it must not block: calls happen from the tool's own goroutine.
	Progress func(snapshot any)
}

// Accepts reports whether the active model takes media of modality m.
func (e Env) Accepts(m session.Modality) bool {
	return slices.Contains(e.Inputs, m)
}

// Report publishes one progress snapshot for the running call, if anything is
// listening. The agent coalesces snapshots downstream, so a tool may report as
// often as it likes.
func (e Env) Report(snapshot any) {
	if e.Progress != nil {
		e.Progress(snapshot)
	}
}

// Resolve turns a tool-supplied path into a clean absolute path under the
// working directory, rejecting an empty path.
func (e Env) Resolve(path string) (string, error) {
	if path == "" {
		return "", errors.New("path must not be empty")
	}
	if !filepath.IsAbs(path) {
		path = filepath.Join(e.CWD, path)
	}
	return filepath.Clean(path), nil
}
