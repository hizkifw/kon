package tool

import (
	"context"
	"encoding/json"
	"fmt"

	"kon.kitsu.red/core/session"
)

// Executor runs model tool calls against a registry. It is the only piece the
// agent sees.
type Executor struct {
	cwd      string
	inputs   []session.Modality
	registry *Registry
}

// NewExecutor returns an Executor that runs the tools in registry with cwd as
// their working directory. inputs lists the media the active model accepts
// and gates what tools attach to their results.
func NewExecutor(registry *Registry, cwd string, inputs []session.Modality) *Executor {
	return &Executor{cwd: cwd, inputs: inputs, registry: registry}
}

// Interrupt escalates cancellation of the tool call in flight. attempt is the
// number of consecutive interrupt requests; see Interrupter.
func (e *Executor) Interrupt(attempt int) bool {
	return e.registry.InterruptAll(attempt)
}

// Definitions returns the model-facing schema of every registered tool.
func (e *Executor) Definitions() []session.ToolDefinition {
	return e.registry.Definitions()
}

// Execute runs one tool call. The second result reports whether the call
// failed, so the model sees the error as a failed tool result instead of the
// turn ending. progress, when set, receives the running tool's live snapshots.
func (e *Executor) Execute(ctx context.Context, name string, arguments json.RawMessage, progress func(any)) (Result, bool) {
	tool, ok := e.registry.Lookup(name)
	if !ok {
		return Result{Content: fmt.Sprintf("error: unknown tool %q", name)}, true
	}
	result, err := tool.Run(ctx, Env{CWD: e.cwd, Inputs: e.inputs, Progress: progress}, arguments)
	if err != nil {
		return Result{Content: "error: " + err.Error()}, true
	}
	return result, result.IsError
}
