package provider

import (
	"encoding/json"
	"strings"
	"testing"

	"github.com/hizkifw/kon/internal/session"
)

func TestToChatMessagesMapsImagePartsOnToolResults(t *testing.T) {
	dataURI := "data:image/png;base64,aGVsbG8="
	messages, err := toChatMessages([]session.Message{
		{Role: session.RoleUser, Content: "look"},
		{Role: session.RoleAssistant, ToolCalls: []session.ToolCall{{ID: "1", Function: session.ToolFunction{Name: "read", Arguments: json.RawMessage(`{"path":"p.png"}`)}}}},
		{
			Role: session.RoleTool, Content: "loaded image p.png", ToolCallID: "1", Name: "read",
			Parts: []session.Part{{Type: session.PartImage, Text: dataURI}},
		},
	})
	if err != nil {
		t.Fatal(err)
	}
	wire := messages[2]
	if wire.ToolCallID != "1" {
		t.Fatalf("tool call ID = %q", wire.ToolCallID)
	}
	parts, ok := wire.Content.(*[]chatContentPart)
	if !ok {
		t.Fatalf("tool content = %#v, want multimodal parts", wire.Content)
	}
	if len(*parts) != 2 {
		t.Fatalf("parts = %#v", *parts)
	}
	if (*parts)[0].Type != "text" || (*parts)[0].Text != "loaded image p.png" {
		t.Fatalf("text part = %#v", (*parts)[0])
	}
	if (*parts)[1].Type != "image_url" || (*parts)[1].ImageURL == nil || (*parts)[1].ImageURL.URL != dataURI {
		t.Fatalf("image part = %#v", (*parts)[1])
	}
}

func TestToChatMessagesIgnoresNonImageParts(t *testing.T) {
	messages, err := toChatMessages([]session.Message{
		{Role: session.RoleTool, Content: "done", ToolCallID: "1", Name: "shell",
			Parts: []session.Part{{Type: session.PartReasoning, Text: "thoughts"}}},
	})
	if err != nil {
		t.Fatal(err)
	}
	if _, ok := messages[0].Content.(*string); !ok {
		t.Fatalf("reasoning parts must not force multimodal content: %#v", messages[0].Content)
	}
}

func TestToChatMessagesToolWithoutPartsKeepsString(t *testing.T) {
	messages, err := toChatMessages([]session.Message{
		{Role: session.RoleTool, Content: "done", ToolCallID: "1", Name: "shell"},
	})
	if err != nil {
		t.Fatal(err)
	}
	content, ok := messages[0].Content.(*string)
	if !ok || *content != "done" {
		t.Fatalf("content = %#v", messages[0].Content)
	}
}

func TestToChatMessagesEncodesImagePartsOnTheWire(t *testing.T) {
	// The encoded JSON must match the chat-completions multimodal shape.
	messages, err := toChatMessages([]session.Message{
		{Role: session.RoleTool, Content: "loaded", ToolCallID: "1", Name: "read",
			Parts: []session.Part{{Type: session.PartImage, Text: "data:image/png;base64,AAA="}}},
	})
	if err != nil {
		t.Fatal(err)
	}
	encoded, err := json.Marshal(messages[0])
	if err != nil {
		t.Fatal(err)
	}
	var decoded struct {
		Content []struct {
			Type     string `json:"type"`
			Text     string `json:"text"`
			ImageURL *struct {
				URL string `json:"url"`
			} `json:"image_url"`
		} `json:"content"`
	}
	if err := json.Unmarshal(encoded, &decoded); err != nil {
		t.Fatal(err)
	}
	if len(decoded.Content) != 2 || decoded.Content[1].ImageURL == nil || !strings.HasPrefix(decoded.Content[1].ImageURL.URL, "data:image/png;base64,") {
		t.Fatalf("wire content = %s", encoded)
	}
}
