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

	"github.com/hizkifw/kon/internal/config"
	"github.com/hizkifw/kon/internal/session"
)

func TestToChatMessagesMapsImagePartsOnToolResults(t *testing.T) {
	dataURI := "data:image/png;base64,aGVsbG8="
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
			_, _ = io.WriteString(w, `{"choices":[{"index":0,"message":{"content":"ok"},"finish_reason":"stop"}]}`)
		}))
		profile := config.Model{Name: "m", Type: "openai-compatible", ModelID: "m", BaseURL: server.URL, Vision: vision}
		model := newChatModel(profile, func(string) ([]byte, error) { return []byte("hello"), nil })
		messages := []session.Message{{Role: session.RoleUser, Parts: []session.Part{
			{Type: session.PartText, Text: "look"},
			{Type: session.PartImage, ImageHash: strings.Repeat("0", 64), ImageMIME: "image/png"},
		}}}
		_, err := model.Complete(context.Background(), messages, nil, 0)
		server.Close()
		if err != nil {
			t.Fatal(err)
		}
		if sent := strings.Contains(body, `"image_url"`); sent != vision {
			t.Fatalf("vision=%v: image sent = %v in %s", vision, sent, body)
		}
	}
}

func TestToChatMessagesIgnoresNonImageParts(t *testing.T) {
	messages, err := toChatMessages([]session.Message{
		{Role: session.RoleTool, Parts: []session.Part{{Type: session.PartToolResult, ToolCallID: "1", ToolName: "shell", ToolOutput: "done"}, {Type: session.PartReasoning, Text: "thoughts"}}},
	}, chatReplay{}, nil)
	if err != nil {
		t.Fatal(err)
	}
	if _, ok := messages[0].Content.(*string); !ok {
		t.Fatalf("reasoning parts must not force multimodal content: %#v", messages[0].Content)
	}
}

func TestToChatMessagesToolWithoutPartsKeepsString(t *testing.T) {
	messages, err := toChatMessages([]session.Message{
		session.ToolResultMessage("1", "shell", "done"),
	}, chatReplay{}, nil)
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
	const hash = "0000000000000000000000000000000000000000000000000000000000000000"
	messages, err := toChatMessages([]session.Message{
		{Role: session.RoleTool, Parts: []session.Part{{Type: session.PartToolResult, ToolCallID: "1", ToolName: "read", ToolOutput: "loaded"}, {Type: session.PartImage, ImageHash: hash, ImageMIME: "image/png"}}},
	}, chatReplay{}, func(string) ([]byte, error) { return []byte{0}, nil })
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
