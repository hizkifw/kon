package agent_test

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"time"

	"github.com/hizkifw/kon/core/agent"
	"github.com/hizkifw/kon/core/provider"
	"github.com/hizkifw/kon/core/provider/wire"
	"github.com/hizkifw/kon/core/session"
	"github.com/hizkifw/kon/core/tool"
)

// clockTool tells the model the time: a whole tool is a schema and a Run.
type clockTool struct{}

func (clockTool) Definition() session.ToolDefinition {
	return session.ToolDefinition{
		Name:        "clock",
		Description: "Report the current time.",
		Parameters:  json.RawMessage(`{"type":"object","properties":{},"additionalProperties":false}`),
	}
}

func (clockTool) Run(context.Context, tool.Env, json.RawMessage) (tool.Result, error) {
	return tool.Result{Content: time.Now().Format(time.RFC1123)}, nil
}

// A bot built on core brings its own prompt, tools, and model; the runner
// handles tool calls, retries, and compaction.
func Example() {
	store, err := session.NewMemory(session.Header{AppVersion: "clockbot"}, "You are a terse assistant in a chat room.")
	if err != nil {
		panic(err)
	}
	defer store.Close()
	client, err := provider.New(provider.Spec{
		Format:    wire.Anthropic,
		ModelID:   "claude-sonnet-5-5",
		APIKey:    os.Getenv("ANTHROPIC_API_KEY"),
		UserAgent: "clockbot/1.0",
	}, store.ReadImage)
	if err != nil {
		panic(err)
	}
	runner := agent.New(agent.Config{
		Limits:   agent.Limits{ContextWindow: 200_000, OutputLimit: 32_000},
		Provider: client,
		Store:    store,
		Tools:    tool.NewExecutor(tool.NewRegistry(clockTool{}), ".", false),
	})
	err = runner.Run(context.Background(), "What time is it?", nil, func(event agent.Event) {
		if event.Kind == agent.EventText {
			fmt.Print(event.Text)
		}
	})
	if err != nil {
		panic(err)
	}
}
