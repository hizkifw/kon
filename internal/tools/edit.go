package tools

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"

	"github.com/hizkifw/kon/internal/provider"
)

// editTool replaces exactly one occurrence of old_text with new_text.
type editTool struct{}

func (editTool) Definition() provider.Tool {
	return provider.Tool{
		Name:        "edit",
		Description: "Replace exactly one occurrence of old_text in a text file.",
		Parameters:  json.RawMessage(`{"type":"object","properties":{"path":{"type":"string"},"old_text":{"type":"string"},"new_text":{"type":"string"}},"required":["path","old_text","new_text"],"additionalProperties":false}`),
	}
}

// Interrupt is a no-op: editing is synchronous and nothing runs in the
// background between calls.
func (editTool) Interrupt(int) bool { return false }

// Summarize renders the request line: the path relative to cwd plus the edit
// size as removed and added line counts.
func (editTool) Summarize(raw json.RawMessage, cwd string) string {
	args := struct {
		Path    string `json:"path"`
		OldText string `json:"old_text"`
		NewText string `json:"new_text"`
	}{}
	if err := json.Unmarshal(raw, &args); err != nil {
		return FallbackSummary(raw)
	}
	if args.OldText == "" && args.NewText == "" {
		return prettyPath(args.Path, cwd)
	}
	return prettyPath(args.Path, cwd) + fmt.Sprintf(" · -%d +%d lines", countLines(args.OldText), countLines(args.NewText))
}

// Describe renders the call. Like write, the edit never echoes file bodies.
func (editTool) Describe(raw json.RawMessage, result string, failed bool, _ json.RawMessage, cwd string) Display {
	summary := editTool{}.Summarize(raw, cwd)
	if failed {
		return failureDisplay(summary, result)
	}
	return Display{State: StateDone, Summary: summary}
}

func (editTool) Run(_ context.Context, env Env, raw json.RawMessage) (Result, error) {
	var args struct {
		Path    string `json:"path"`
		OldText string `json:"old_text"`
		NewText string `json:"new_text"`
	}
	if err := decodeArgs(raw, &args); err != nil {
		return Result{}, err
	}
	if args.OldText == "" {
		return Result{}, errors.New("old_text must not be empty")
	}
	path, err := env.Resolve(args.Path)
	if err != nil {
		return Result{}, err
	}
	b, err := os.ReadFile(path)
	if err != nil {
		return Result{}, err
	}
	count := bytes.Count(b, []byte(args.OldText))
	if count != 1 {
		return Result{}, fmt.Errorf("old_text must occur exactly once; found %d occurrences", count)
	}
	info, err := os.Stat(path)
	if err != nil {
		return Result{}, err
	}
	updated := bytes.Replace(b, []byte(args.OldText), []byte(args.NewText), 1)
	if err := atomicWrite(path, updated, info.Mode().Perm()); err != nil {
		return Result{}, err
	}
	return Result{Content: fmt.Sprintf("edited %s", path)}, nil
}
