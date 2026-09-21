package provider

import (
	"context"
	"encoding/json"
	"testing"

	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/typedid"
	"github.com/zendev-sh/goai"
	goaiprovider "github.com/zendev-sh/goai/provider"
)

func TestMessageRoundTripPreservesProviderToolMetadata(t *testing.T) {
	result := &goai.TextResult{
		ToolCalls: []goaiprovider.ToolCall{{ID: "call-1", Name: "read", Input: json.RawMessage(`{"path":"x"}`), Metadata: map[string]any{"thoughtSignature": "sig"}}},
		Steps: []goai.StepResult{{Text: "checking", Content: []goaiprovider.Part{
			{Type: goaiprovider.PartText, Text: "checking"},
			{Type: goaiprovider.PartToolCall, ToolCallID: "call-1", ToolName: "read", ToolInput: json.RawMessage(`{"path":"x"}`), ProviderOptions: map[string]any{"thoughtSignature": "sig"}},
		}}},
		FinishReason: goaiprovider.FinishToolCalls,
	}
	message, err := fromResult(result, typedid.ExternalModelID("model"))
	if err != nil {
		t.Fatal(err)
	}
	converted := toMessages([]session.Message{message})
	if got := converted[0].Content[1].ProviderOptions["thoughtSignature"]; got != "sig" {
		t.Fatalf("metadata = %v", got)
	}
}

func TestFromResultPreservesThinkingForReplay(t *testing.T) {
	// Mirrors what goai assembles for a streamed thinking-model response:
	// reasoning parts (with the provider signature) come first in
	// ResponseMessages, and StepResult.Content carries no reasoning.
	result := &goai.TextResult{
		Text:  "checking",
		Steps: []goai.StepResult{{Text: "checking"}},
		ResponseMessages: []goaiprovider.Message{{Role: goaiprovider.RoleAssistant, Content: []goaiprovider.Part{
			{Type: goaiprovider.PartReasoning, Text: "let me look", ProviderOptions: map[string]any{"signature": "sig-1"}},
			{Type: goaiprovider.PartText, Text: "checking"},
			{Type: goaiprovider.PartToolCall, ToolCallID: "call-1", ToolName: "read", ToolInput: json.RawMessage(`{"path":"x"}`)},
		}}},
		ToolCalls:    []goaiprovider.ToolCall{{ID: "call-1", Name: "read", Input: json.RawMessage(`{"path":"x"}`)}},
		FinishReason: goaiprovider.FinishToolCalls,
	}
	message, err := fromResult(result, typedid.ExternalModelID("model"))
	if err != nil {
		t.Fatal(err)
	}
	if len(message.Parts) != 3 || message.Parts[0].Type != "reasoning" || message.Parts[0].Text != "let me look" {
		t.Fatalf("parts = %#v", message.Parts)
	}
	converted := toMessages([]session.Message{message})
	if len(converted[0].Content) != 3 {
		t.Fatalf("content = %#v", converted[0].Content)
	}
	reasoning := converted[0].Content[0]
	if reasoning.Type != goaiprovider.PartReasoning || reasoning.Text != "let me look" {
		t.Fatalf("reasoning part = %#v", reasoning)
	}
	if got := reasoning.ProviderOptions["signature"]; got != "sig-1" {
		t.Fatalf("signature = %v", got)
	}
}

type streamModel struct {
	chunks []goaiprovider.StreamChunk
}

func (m *streamModel) ModelID() string { return "test-model" }

func (m *streamModel) DoGenerate(context.Context, goaiprovider.GenerateParams) (*goaiprovider.GenerateResult, error) {
	return nil, nil
}

func (m *streamModel) DoStream(context.Context, goaiprovider.GenerateParams) (*goaiprovider.StreamResult, error) {
	stream := make(chan goaiprovider.StreamChunk, len(m.chunks))
	for _, chunk := range m.chunks {
		stream <- chunk
	}
	close(stream)
	return &goaiprovider.StreamResult{Stream: stream}, nil
}

func TestStreamKeepsThinkingTraceForProviderReplay(t *testing.T) {
	model := &streamModel{chunks: []goaiprovider.StreamChunk{
		{Type: goaiprovider.ChunkReasoning, Text: "let me look"},
		{Type: goaiprovider.ChunkReasoning, Metadata: map[string]any{"signature": "sig-1"}},
		{Type: goaiprovider.ChunkText, Text: "checking"},
		{Type: goaiprovider.ChunkToolCall, ToolCallID: "call-1", ToolName: "read", ToolInput: `{"path":"x"}`},
		{Type: goaiprovider.ChunkFinish, FinishReason: goaiprovider.FinishToolCalls},
	}}
	client := &Client{model: model, modelID: typedid.ExternalModelID("test-model")}
	message, err := client.Stream(context.Background(), []session.Message{{Role: session.RoleUser, Content: "hi"}}, nil, func(Event) {})
	if err != nil {
		t.Fatal(err)
	}
	if len(message.Parts) != 3 || message.Parts[0].Type != "reasoning" || message.Parts[0].Text != "let me look" {
		t.Fatalf("parts = %#v", message.Parts)
	}
	if len(message.ToolCalls) != 1 || message.ToolCalls[0].ID.String() != "call-1" {
		t.Fatalf("tool calls = %#v", message.ToolCalls)
	}
	// Replay the stored assistant message the way the next provider call does.
	converted := toMessages([]session.Message{message})
	if len(converted[0].Content) != 3 {
		t.Fatalf("content = %#v", converted[0].Content)
	}
	reasoning := converted[0].Content[0]
	if reasoning.Type != goaiprovider.PartReasoning || reasoning.Text != "let me look" {
		t.Fatalf("replayed reasoning part = %#v", reasoning)
	}
	if got := reasoning.ProviderOptions["signature"]; got != "sig-1" {
		t.Fatalf("replayed signature = %v", got)
	}
}

func TestChunkEventMapsReasoningDeltas(t *testing.T) {
	text, ok := chunkEvent(goaiprovider.StreamChunk{Type: goaiprovider.ChunkText, Text: "hi"})
	if !ok || text.Thinking || text.Text != "hi" {
		t.Fatalf("text chunk = %#v, ok = %v", text, ok)
	}
	thinking, ok := chunkEvent(goaiprovider.StreamChunk{Type: goaiprovider.ChunkReasoning, Text: "hmm"})
	if !ok || !thinking.Thinking || thinking.Text != "hmm" {
		t.Fatalf("reasoning chunk = %#v, ok = %v", thinking, ok)
	}
	if _, ok := chunkEvent(goaiprovider.StreamChunk{Type: goaiprovider.ChunkReasoning}); ok {
		t.Fatal("empty reasoning chunk was emitted")
	}
	if _, ok := chunkEvent(goaiprovider.StreamChunk{Type: goaiprovider.ChunkToolCall}); ok {
		t.Fatal("tool-call chunk was emitted as text")
	}
}

func TestContextOverflowUsesGoAIDetection(t *testing.T) {
	if !IsContextOverflow(&goai.ContextOverflowError{Message: "prompt is too long"}) {
		t.Fatal("overflow was not detected")
	}
}
