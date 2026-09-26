// Package headless runs one prompt without a terminal UI, for scripts and
// pipelines. It is a second frontend beside internal/ui: the runtime does the
// work, and this package only turns agent events into output, either plain
// text streamed as the model writes or one JSON object per line.
package headless

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"strings"
	"time"

	"github.com/hizkifw/kon/internal/agent"
	"github.com/hizkifw/kon/internal/app"
	"github.com/hizkifw/kon/internal/tools"
	"github.com/hizkifw/kon/internal/typedid"
)

// Runtime is the part of app.Runtime a headless run drives.
type Runtime interface {
	Run(ctx context.Context, prompt string, inbox *agent.Inbox, emit func(agent.Event)) error
	SessionID() typedid.SessionID
	State() app.State
}

type Format string

const (
	// FormatText streams the assistant's messages to stdout as they are
	// written, separated by blank lines.
	FormatText Format = "text"
	// FormatJSON writes one JSON event per line; see docs/product/usage.md.
	FormatJSON Format = "json"
)

// Output says where a run's output goes.
type Output struct {
	Format Format
	Stdout io.Writer
	// Progress receives tool activity and the resume hint in text mode. It
	// is nil when nobody is watching, so logs hold only the answer.
	Progress io.Writer
	// CWD shortens paths in tool summaries.
	CWD string
	// Started, when set, receives the session ID once the session has its
	// first message and so an ID that will persist.
	Started func(typedid.SessionID)
}

// Run sends one prompt and writes the run's output as it happens. The error
// is the run's own: a cancelled run returns the context's error after the
// partial answer has been written.
func Run(ctx context.Context, runtime Runtime, prompt string, out Output) error {
	var w writer
	switch out.Format {
	case FormatText:
		w = &textWriter{out: out}
	case FormatJSON:
		w = &jsonWriter{enc: json.NewEncoder(out.Stdout), runtime: runtime, cwd: out.CWD}
	default:
		return fmt.Errorf("unknown format %q", out.Format)
	}
	start := time.Now()
	emit := w.event
	if out.Started != nil {
		started := false
		emit = func(e agent.Event) {
			if !started {
				started = true
				out.Started(runtime.SessionID())
			}
			w.event(e)
		}
	}
	// Nobody can steer a headless run, so it has no inbox.
	err := runtime.Run(ctx, prompt, nil, emit)
	if finishErr := w.finish(runtime.SessionID(), err, time.Since(start)); finishErr != nil && err == nil {
		err = finishErr
	}
	return err
}

type writer interface {
	event(agent.Event)
	finish(id typedid.SessionID, err error, elapsed time.Duration) error
}

// textWriter streams assistant text as it arrives. Tool calls and
// compactions are reported on Progress, one line each, so stdout stays the
// conversation itself.
type textWriter struct {
	out Output
	// open marks an assistant message whose text has started, and wrote marks
	// that some message has, so the next one is set off by a blank line.
	open, wrote bool
	// newline records whether the output so far ends a line.
	newline bool
	err     error
}

func (w *textWriter) write(s string) {
	if w.err != nil || s == "" {
		return
	}
	_, w.err = io.WriteString(w.out.Stdout, s)
	w.newline = strings.HasSuffix(s, "\n")
}

func (w *textWriter) progress(format string, args ...any) {
	if w.out.Progress != nil {
		fmt.Fprintf(w.out.Progress, format+"\n", args...)
	}
}

// endMessage finishes the current message's line, so whatever follows, on
// stdout or on a shared terminal, starts on its own.
func (w *textWriter) endMessage() {
	if w.open && !w.newline {
		w.write("\n")
	}
	w.open = false
}

func (w *textWriter) event(e agent.Event) {
	switch e.Kind {
	case agent.EventText:
		if !w.open {
			if w.wrote {
				w.write("\n")
			}
			w.open, w.wrote = true, true
		}
		w.write(e.Text)
	case agent.EventAssistantDone:
		w.endMessage()
	case agent.EventToolDone:
		d := tools.Describe(e.Tool, json.RawMessage(e.Arguments), e.Text, e.IsError, e.Details, w.out.CWD)
		mark := "✓"
		if e.IsError {
			mark = "✗"
		}
		line := mark + " " + e.Tool
		if d.Summary != "" {
			line += " " + d.Summary
		}
		if d.Note != "" {
			line += " · " + d.Note
		}
		w.progress("%s", line)
	case agent.EventCompacted:
		w.progress("compacted %s tokens", e.Tokens)
	}
}

func (w *textWriter) finish(id typedid.SessionID, _ error, _ time.Duration) error {
	// A cancelled stream never sent its done event.
	w.endMessage()
	if !id.IsZero() {
		w.progress("resume with: kon --resume %s", id)
	}
	return w.err
}

// jsonWriter reports whole messages rather than deltas: each line is a
// complete event a consumer can act on without reassembling a stream.
type jsonWriter struct {
	enc     *json.Encoder
	runtime Runtime
	cwd     string
	// started marks that the session event has been written. It waits for
	// the first agent event, when the session has its first message and so
	// an ID worth reporting.
	started bool
	// text and reasoning collect the message being streamed; last is the
	// text of the newest finished message, which the result repeats.
	text, reasoning strings.Builder
	last            string
	err             error
}

func (w *jsonWriter) write(v any) {
	if w.err == nil {
		w.err = w.enc.Encode(v)
	}
}

func (w *jsonWriter) start() {
	if w.started {
		return
	}
	w.started = true
	w.write(sessionEvent{Type: "session", SessionID: w.runtime.SessionID().String(), Model: w.runtime.State().Active.Name, CWD: w.cwd})
}

func (w *jsonWriter) event(e agent.Event) {
	w.start()
	switch e.Kind {
	case agent.EventText:
		w.text.WriteString(e.Text)
	case agent.EventThinking:
		w.reasoning.WriteString(e.Text)
	case agent.EventAssistantDone:
		w.flush(false)
	case agent.EventToolStart:
		w.write(toolStartEvent{Type: "tool_start", CallID: e.CallID.String(), Tool: e.Tool, Arguments: arguments(e.Arguments)})
	case agent.EventToolDone:
		w.write(toolDoneEvent{Type: "tool_done", CallID: e.CallID.String(), Tool: e.Tool, IsError: e.IsError, Output: e.Text, Details: e.Details})
	case agent.EventCompacted:
		w.write(compactedEvent{Type: "compacted", TokensBefore: int64(e.Tokens), Estimated: e.Estimated})
	case agent.EventUsage:
		// A negative count only resets the estimate after a compaction.
		if e.Tokens >= 0 {
			w.write(usageEvent{Type: "usage", ContextTokens: int64(e.Tokens)})
		}
	}
}

// flush writes the message collected so far. A partial one is what a
// cancelled stream produced before it stopped.
func (w *jsonWriter) flush(partial bool) {
	if w.text.Len() == 0 && w.reasoning.Len() == 0 {
		return
	}
	w.last = w.text.String()
	w.write(assistantEvent{Type: "assistant", Text: w.last, Reasoning: w.reasoning.String(), Partial: partial})
	w.text.Reset()
	w.reasoning.Reset()
}

func (w *jsonWriter) finish(id typedid.SessionID, runErr error, elapsed time.Duration) error {
	w.flush(true)
	result := resultEvent{Type: "result", Text: w.last, DurationMS: elapsed.Milliseconds()}
	if !id.IsZero() {
		result.SessionID = id.String()
	}
	if runErr != nil {
		result.Error = runErr.Error()
	}
	w.write(result)
	return w.err
}

// arguments passes tool arguments through as JSON when they are JSON, and as
// a string when a model sent something else.
func arguments(raw string) any {
	if json.Valid([]byte(raw)) {
		return json.RawMessage(raw)
	}
	return raw
}

type sessionEvent struct {
	Type      string `json:"type"`
	SessionID string `json:"session_id"`
	Model     string `json:"model"`
	CWD       string `json:"cwd"`
}

type assistantEvent struct {
	Type      string `json:"type"`
	Text      string `json:"text"`
	Reasoning string `json:"reasoning,omitempty"`
	Partial   bool   `json:"partial,omitempty"`
}

type toolStartEvent struct {
	Type      string `json:"type"`
	CallID    string `json:"call_id"`
	Tool      string `json:"tool"`
	Arguments any    `json:"arguments"`
}

type toolDoneEvent struct {
	Type    string          `json:"type"`
	CallID  string          `json:"call_id"`
	Tool    string          `json:"tool"`
	IsError bool            `json:"is_error"`
	Output  string          `json:"output"`
	Details json.RawMessage `json:"details,omitempty"`
}

type compactedEvent struct {
	Type         string `json:"type"`
	TokensBefore int64  `json:"tokens_before"`
	Estimated    bool   `json:"estimated"`
}

type usageEvent struct {
	Type          string `json:"type"`
	ContextTokens int64  `json:"context_tokens"`
}

type resultEvent struct {
	Type       string `json:"type"`
	SessionID  string `json:"session_id,omitempty"`
	Text       string `json:"text"`
	Error      string `json:"error,omitempty"`
	DurationMS int64  `json:"duration_ms"`
}
