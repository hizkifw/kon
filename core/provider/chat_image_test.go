package provider

import (
	"context"
	"encoding/json"
	"errors"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/hizkifw/kon/core/provider/wire"
	"github.com/hizkifw/kon/core/session"
)

func TestToChatMessagesMapsImagePartsOnToolResults(t *testing.T) {
	const hash = "0000000000000000000000000000000000000000000000000000000000000000"
	messages, err := toChatMessages([]session.Message{
		session.TextMessage(session.RoleUser, "look"),
		{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartToolCall, ToolCallID: "1", ToolName: "read", ToolInput: json.RawMessage(`{"path":"p.png"}`)}}},
		{
			Role:  session.RoleTool,
			Parts: []session.Part{{Type: session.PartToolResult, ToolCallID: "1", ToolName: "read", ToolOutput: "loaded image p.png"}, {Type: session.PartImage, ImageHash: hash, ImageMIME: "image/png"}},
		},
	}, chatReplay{}, func(string) ([]byte, error) { return []byte("hello"), nil })
	if err != nil {
		t.Fatal(err)
	}
	encoded, err := json.Marshal(messages[2])
	if err != nil {
		t.Fatal(err)
	}
	// The encoded JSON must match the chat-completions multimodal shape.
	assertWireJSON(t, encoded, `{
		"role": "tool",
		"tool_call_id": "1",
		"content": [
			{"type": "text", "text": "loaded image p.png"},
			{"type": "image_url", "image_url": {"url": "data:image/png;base64,aGVsbG8="}}
		]
	}`)
}

func TestToChatMessagesReplacesUnreadableImageBlob(t *testing.T) {
	// A missing blob must not fail the request: every later turn, compaction
	// included, would carry the same image and fail the same way.
	message := session.Message{Role: session.RoleTool, Parts: []session.Part{
		{Type: session.PartToolResult, ToolCallID: "1", ToolName: "read", ToolOutput: "loaded"},
		{Type: session.PartImage, ImageHash: "0000000000000000000000000000000000000000000000000000000000000000", ImageMIME: "image/png"},
	}}
	messages, err := toChatMessages([]session.Message{message}, chatReplay{}, func(string) ([]byte, error) { return nil, errors.New("missing") })
	if err != nil {
		t.Fatal(err)
	}
	content, ok := messages[0].Content.(*string)
	if !ok || *content != "loaded\n"+imageUnavailableText {
		t.Fatalf("content = %#v, want text with a placeholder", messages[0].Content)
	}
}

func TestToChatMessagesKeepsReadableImagesBesideAnUnreadableOne(t *testing.T) {
	good := strings.Repeat("1", 64)
	message := session.Message{Role: session.RoleUser, Parts: []session.Part{
		{Type: session.PartText, Text: "compare"},
		{Type: session.PartImage, ImageHash: good, ImageMIME: "image/png"},
		{Type: session.PartImage, ImageHash: strings.Repeat("2", 64), ImageMIME: "image/png"},
	}}
	messages, err := toChatMessages([]session.Message{message}, chatReplay{}, func(hash string) ([]byte, error) {
		if hash != good {
			return nil, errors.New("missing")
		}
		return []byte("hello"), nil
	})
	if err != nil {
		t.Fatal(err)
	}
	parts, ok := messages[0].Content.(*[]chatContentPart)
	if !ok || len(*parts) != 3 {
		t.Fatalf("content = %#v", messages[0].Content)
	}
	if (*parts)[1].Type != "image_url" || (*parts)[2].Type != "text" || (*parts)[2].Text != imageUnavailableText {
		t.Fatalf("parts = %#v", *parts)
	}
}

func TestToChatMessagesOmitsImagesWithoutVision(t *testing.T) {
	message := session.Message{Role: session.RoleTool, Parts: []session.Part{
		{Type: session.PartToolResult, ToolCallID: "1", ToolName: "read", ToolOutput: "loaded"},
		{Type: session.PartImage, ImageHash: strings.Repeat("0", 64), ImageMIME: "image/png"},
	}}
	messages, err := toChatMessages([]session.Message{message}, chatReplay{}, nil)
	if err != nil {
		t.Fatal(err)
	}
	content, ok := messages[0].Content.(*string)
	if !ok || *content != "loaded\n"+imageOmittedText {
		t.Fatalf("content = %#v, want text with a placeholder", messages[0].Content)
	}
}

// TestModelWithoutVisionSendsNoImages covers a /model switch: the session
// still holds an image read by an earlier model, and the new model's profile
// has no vision.
func TestModelWithoutVisionSendsNoImages(t *testing.T) {
	for _, vision := range []bool{false, true} {
		var body string
		server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			b, _ := io.ReadAll(r.Body)
			body = string(b)
			_, _ = io.WriteString(w, okStream)
		}))
		model, err := newChatModel(Spec{Name: "m", Format: wire.OpenAICompatible, ModelID: "m", BaseURL: server.URL, Vision: vision}, wire.Spec{ReasoningField: "reasoning_content"}, func(string) ([]byte, error) { return []byte("hello"), nil })
		if err != nil {
			t.Fatal(err)
		}
		messages := []session.Message{{Role: session.RoleUser, Parts: []session.Part{
			{Type: session.PartText, Text: "look"},
			{Type: session.PartImage, ImageHash: strings.Repeat("0", 64), ImageMIME: "image/png"},
		}}}
		_, err = model.Complete(context.Background(), messages, nil, 0, nil)
		server.Close()
		if err != nil {
			t.Fatal(err)
		}
		if sent := strings.Contains(body, `"image_url"`); sent != vision {
			t.Fatalf("vision=%v: image sent = %v in %s", vision, sent, body)
		}
	}
}
