package agent

import (
	"context"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/hizkifw/kon/internal/provider"
	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/tokens"
	"github.com/hizkifw/kon/internal/tools"
)

// toolCallingProvider asks for the named tool call once, then finishes.
type toolCallingProvider struct {
	toolName    string
	arguments   string
	asked       int
	completed   int
	streamCalls int
}

func (p *toolCallingProvider) Stream(_ context.Context, messages []session.Message, _ []session.ToolDefinition, _ func(provider.Event)) (session.Message, error) {
	p.streamCalls++
	p.asked++
	if p.asked == 1 {
		return session.Message{
			Role:   session.RoleAssistant,
			Parts:  []session.Part{{Type: session.PartToolCall, ToolCallID: "call-1", ToolName: p.toolName, ToolInput: json.RawMessage(p.arguments)}},
			Finish: "tool_calls",
			Usage:  &session.Usage{PromptTokens: 30, CompletionTokens: 5, TotalTokens: 35},
		}, nil
	}
	return session.Message{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartText, Text: "saw it"}}, Finish: "stop", Usage: &session.Usage{PromptTokens: 2000, CompletionTokens: 5, TotalTokens: 2005}}, nil
}

func (p *toolCallingProvider) Complete(_ context.Context, _ []session.Message, _ []session.ToolDefinition, _ tokens.Count) (session.Message, error) {
	p.completed++
	return session.Message{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartText, Text: "summary"}}}, nil
}

// TestImageResultDoesNotForceCompaction pins the compaction behavior for
// image tool results. The provider-reported token count is the only measure
// of an image's real cost; estimating from the base64 payload overcounts by
// orders of magnitude and compacted every image read.
func TestImageResultDoesNotForceCompaction(t *testing.T) {
	dir := t.TempDir()
	// Large enough that a base64-derived token estimate blows past the window
	// threshold (a 1.3 MB image once projected ~450k tokens), while the true
	// vision cost reported by the provider stays a rounding error.
	image := append([]byte(nil), pngHeader...)
	image = append(image, make([]byte, 600_000)...)
	if err := os.WriteFile(filepath.Join(dir, "shot.png"), image, 0o644); err != nil {
		t.Fatal(err)
	}
	store, err := session.New(t.TempDir(), dir, "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	fake := &toolCallingProvider{toolName: "read", arguments: `{"path":"shot.png"}`}
	limits := testLimits
	limits.ContextWindow = 100_000
	limits.ReserveTokens = 16_384
	limits.KeepRecentTokens = 20_000
	runner := New(limits, fake, store, tools.New(dir, true, nil))
	if err := runner.Run(context.Background(), "look at shot.png", nil, func(Event) {}); err != nil {
		t.Fatal(err)
	}
	// The run must go straight to the final answer: no compaction summary
	// request, exactly two stream calls (tool call turn, then final answer).
	if fake.completed != 0 || fake.streamCalls != 2 {
		t.Fatalf("image result triggered compaction: complete=%d stream=%d", fake.completed, fake.streamCalls)
	}
	items, err := store.Context()
	if err != nil {
		t.Fatal(err)
	}
	for _, item := range items {
		if item.Summary {
			t.Fatal("a compaction summary was projected after an image read")
		}
	}
}

// pngHeader is enough of a PNG for the read tool's magic-byte sniff.
var pngHeader = []byte{
	0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A, 0x00, 0x00, 0x00, 0x0D,
	0x49, 0x48, 0x44, 0x52, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01,
	0x08, 0x06, 0x00, 0x00, 0x00, 0x1F, 0x15, 0xC4, 0x89, 0x00, 0x00, 0x00,
	0x0D, 0x49, 0x44, 0x41, 0x54, 0x78, 0x9C, 0x62, 0x00, 0x01, 0x00, 0x00,
	0x05, 0x00, 0x01, 0x0D, 0x0A, 0x2D, 0xB4, 0x00, 0x00, 0x00, 0x00, 0x49,
	0x45, 0x4E, 0x44, 0xAE, 0x42, 0x60, 0x82,
}

func TestRunPersistsImagePartsFromRead(t *testing.T) {
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, "shot.png"), pngHeader, 0o644); err != nil {
		t.Fatal(err)
	}
	store, err := session.New(t.TempDir(), dir, "test", "system")
	if err != nil {
		t.Fatal(err)
	}
	defer store.Close()
	fake := &toolCallingProvider{toolName: "read", arguments: `{"path":"shot.png"}`}
	limits := testLimits
	runner := New(limits, fake, store, tools.New(dir, true, nil))
	if err := runner.Run(context.Background(), "look at shot.png", nil, func(Event) {}); err != nil {
		t.Fatal(err)
	}
	items, err := store.Context()
	if err != nil {
		t.Fatal(err)
	}
	var tool *session.Message
	for i := range items {
		if items[i].Message.Role == session.RoleTool {
			tool = &items[i].Message
		}
	}
	if tool == nil {
		t.Fatal("no tool result was persisted")
	}
	if len(tool.Parts) != 2 || tool.Parts[1].Type != session.PartImage || tool.Parts[1].ImageMIME != "image/png" || tool.Parts[1].ImageHash == "" || tool.Parts[1].Text != "" {
		t.Fatalf("tool parts = %#v", tool.Parts)
	}
	data, err := store.ReadImage(tool.Parts[1].ImageHash)
	if err != nil || string(data) != string(pngHeader) {
		t.Fatalf("stored image = %x, %v", data, err)
	}
	if tool.Text() == "" || strings.Contains(tool.Text(), "base64") {
		t.Fatalf("tool content should describe, not inline, the image: %q", tool.Text())
	}
}
