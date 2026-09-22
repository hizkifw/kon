package tools

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"

	"github.com/hizkifw/kon/internal/provider"
)

// writeTool creates or replaces a whole file atomically.
type writeTool struct{}

func (writeTool) Definition() provider.Tool {
	return provider.Tool{
		Name:        "write",
		Description: "Create or replace a text file. Parent directories must already exist.",
		Parameters:  json.RawMessage(`{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"],"additionalProperties":false}`),
	}
}

// Interrupt is a no-op: writing is synchronous and nothing runs in the
// background between calls.
func (writeTool) Interrupt(int) bool { return false }

func (writeTool) Run(_ context.Context, env Env, raw json.RawMessage) (Result, error) {
	var args struct {
		Path    string `json:"path"`
		Content string `json:"content"`
	}
	if err := decodeArgs(raw, &args); err != nil {
		return Result{}, err
	}
	path, err := env.Resolve(args.Path)
	if err != nil {
		return Result{}, err
	}
	mode := os.FileMode(0o644)
	if info, statErr := os.Stat(path); statErr == nil {
		mode = info.Mode().Perm()
	} else if !errors.Is(statErr, os.ErrNotExist) {
		return Result{}, statErr
	}
	if err := atomicWrite(path, []byte(args.Content), mode); err != nil {
		return Result{}, err
	}
	return Result{Content: fmt.Sprintf("wrote %d bytes to %s", len(args.Content), path)}, nil
}
