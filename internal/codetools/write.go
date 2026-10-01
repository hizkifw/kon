package codetools

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"

	"github.com/hizkifw/kon/core/session"
	"github.com/hizkifw/kon/core/tool"
)

// writeTool creates or replaces a whole file atomically.
type writeTool struct{}

func (writeTool) Definition() session.ToolDefinition {
	return session.ToolDefinition{
		Name:        "write",
		Description: "Create or replace a text file. Parent directories must already exist.",
		Parameters:  json.RawMessage(`{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"],"additionalProperties":false}`),
	}
}

// Summarize renders the request line: the path relative to cwd plus the size
// of the content being written.
func (writeTool) Summarize(raw json.RawMessage, cwd string) string {
	args := struct {
		Path    string `json:"path"`
		Content string `json:"content"`
	}{}
	if err := json.Unmarshal(raw, &args); err != nil {
		return FallbackSummary(raw)
	}
	return prettyPath(args.Path, cwd) + " · " + humanBytes(len(args.Content))
}

// Describe renders the call. The tool never echoes the file body; a failure
// shows the error message.
func (writeTool) Describe(raw json.RawMessage, result string, failed bool, _ json.RawMessage, cwd string) Display {
	summary := writeTool{}.Summarize(raw, cwd)
	if failed {
		return failureDisplay(summary, result)
	}
	return Display{State: StateDone, Summary: summary}
}

func (writeTool) Run(_ context.Context, env tool.Env, raw json.RawMessage) (tool.Result, error) {
	var args struct {
		Path    string `json:"path"`
		Content string `json:"content"`
	}
	if err := decodeArgs(raw, &args); err != nil {
		return tool.Result{}, err
	}
	path, err := env.Resolve(args.Path)
	if err != nil {
		return tool.Result{}, err
	}
	mode := os.FileMode(0o644)
	if info, statErr := os.Stat(path); statErr == nil {
		mode = info.Mode().Perm()
	} else if !errors.Is(statErr, os.ErrNotExist) {
		return tool.Result{}, statErr
	}
	if err := atomicWrite(path, []byte(args.Content), mode); err != nil {
		return tool.Result{}, err
	}
	return tool.Result{Content: fmt.Sprintf("wrote %d bytes to %s", len(args.Content), path)}, nil
}
