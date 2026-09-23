package tools

import (
	"context"
	"encoding/json"
	"fmt"

	"github.com/hizkifw/kon/internal/provider"
)

// Executor runs model tool calls against the central registry. It is the only
// piece the agent sees.
type Executor struct {
	cwd      string
	vision   bool
	registry *Registry
}

// New returns an Executor with the default (built-in) tools registered,
// rooted at cwd. vision reports whether the active model accepts image
// content and gates whether tools attach images to their results.
func New(cwd string, vision bool) *Executor {
	return &Executor{cwd: cwd, vision: vision, registry: defaultRegistry()}
}

// Interrupt escalates cancellation of the tool the agent is currently running.
// attempt is the number of consecutive Ctrl+C presses; see Tool.Interrupt.
func (e *Executor) Interrupt(attempt int) bool {
	return e.registry.InterruptAll(attempt)
}

// Definitions returns the model-facing schema of every registered tool.
func (e *Executor) Definitions() []provider.Tool {
	return e.registry.Definitions()
}

// Execute runs one tool call. The second result reports whether the call
// failed, so the model sees the error as a failed tool result instead of the
// turn ending. report, when set, receives live display snapshots from the
// running tool.
func (e *Executor) Execute(ctx context.Context, name string, arguments json.RawMessage, report func(Display)) (Result, bool) {
	tool, ok := e.registry.Lookup(name)
	if !ok {
		return Result{Content: fmt.Sprintf("error: unknown tool %q", name)}, true
	}
	result, err := tool.Run(ctx, Env{cwd: e.cwd, vision: e.vision, report: report}, arguments)
	if err != nil {
		return Result{Content: "error: " + err.Error()}, true
	}
	return result, result.IsError
}
