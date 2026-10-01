package tool

import (
	"context"
	"encoding/json"
	"errors"
	"path/filepath"
	"testing"

	"github.com/hizkifw/kon/core/session"
)

type echoTool struct{ name string }

func (t echoTool) Definition() session.ToolDefinition {
	return session.ToolDefinition{Name: t.name, Parameters: json.RawMessage(`{"type":"object"}`)}
}

func (echoTool) Run(_ context.Context, env Env, arguments json.RawMessage) (Result, error) {
	env.Report("running")
	if string(arguments) == `"fail"` {
		return Result{}, errors.New("asked to fail")
	}
	return Result{Content: env.CWD + " " + string(arguments)}, nil
}

// stubbornTool reports an interrupt it can escalate against.
type stubbornTool struct {
	echoTool
	attempts []int
}

func (t *stubbornTool) Interrupt(attempt int) bool {
	t.attempts = append(t.attempts, attempt)
	return true
}

func TestExecutorRunsRegisteredTools(t *testing.T) {
	executor := NewExecutor(NewRegistry(echoTool{"echo"}), "/work", false)
	var snapshots []any
	result, failed := executor.Execute(context.Background(), "echo", json.RawMessage(`1`), func(s any) { snapshots = append(snapshots, s) })
	if failed || result.Content != "/work 1" || len(snapshots) != 1 || snapshots[0] != "running" {
		t.Fatalf("result = %+v, failed = %v, snapshots = %v", result, failed, snapshots)
	}
	if result, failed := executor.Execute(context.Background(), "echo", json.RawMessage(`"fail"`), nil); !failed || result.Content != "error: asked to fail" {
		t.Fatalf("failing call = %+v, failed = %v", result, failed)
	}
	if result, failed := executor.Execute(context.Background(), "missing", nil, nil); !failed || result.Content != `error: unknown tool "missing"` {
		t.Fatalf("unknown tool = %+v, failed = %v", result, failed)
	}
}

func TestInterruptReachesOnlyInterrupters(t *testing.T) {
	stubborn := &stubbornTool{echoTool: echoTool{"shell"}}
	executor := NewExecutor(NewRegistry(echoTool{"echo"}, stubborn), "", false)
	if !executor.Interrupt(2) || len(stubborn.attempts) != 1 || stubborn.attempts[0] != 2 {
		t.Fatalf("interrupt attempts = %v", stubborn.attempts)
	}
	if NewExecutor(NewRegistry(echoTool{"echo"}), "", false).Interrupt(1) {
		t.Fatal("a registry without interrupters reported an interrupt")
	}
}

func TestRegistryRejectsDuplicates(t *testing.T) {
	defer func() {
		if recover() == nil {
			t.Fatal("duplicate tool name did not panic")
		}
	}()
	NewRegistry(echoTool{"echo"}, echoTool{"echo"})
}

func TestResolveAnchorsRelativePaths(t *testing.T) {
	env := Env{CWD: filepath.FromSlash("/work")}
	if path, err := env.Resolve("a/../b.txt"); err != nil || path != filepath.Join(env.CWD, "b.txt") {
		t.Fatalf("Resolve = %q, %v", path, err)
	}
	if _, err := env.Resolve(""); err == nil {
		t.Fatal("empty path resolved")
	}
}
