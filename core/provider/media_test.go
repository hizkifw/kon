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

	"kon.kitsu.red/core/provider/wire"
	"kon.kitsu.red/core/session"
)

var allInputs = session.Modalities()

// readHello is a media store that holds the same bytes under every hash.
func readHello(string) ([]byte, error) { return []byte("hello"), nil }

func mediaPart(hash, mime string) session.Part {
	return session.Part{Type: session.PartMedia, MediaHash: hash, MediaMIME: mime}
}

func TestToChatMessagesMapsImagePartsOnToolResults(t *testing.T) {
	const hash = "0000000000000000000000000000000000000000000000000000000000000000"
	messages, err := toChatMessages([]session.Message{
		session.TextMessage(session.RoleUser, "look"),
		{Role: session.RoleAssistant, Parts: []session.Part{{Type: session.PartToolCall, ToolCallID: "1", ToolName: "read", ToolInput: json.RawMessage(`{"path":"p.png"}`)}}},
		{
			Role:  session.RoleTool,
			Parts: []session.Part{{Type: session.PartToolResult, ToolCallID: "1", ToolName: "read", ToolOutput: "loaded image p.png"}, mediaPart(hash, "image/png")},
		},
	}, chatReplay{}, mediaReader{inputs: allInputs, read: readHello})
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

func TestToChatMessagesMapsEachModality(t *testing.T) {
	hash := strings.Repeat("ab", 32)
	messages, err := toChatMessages([]session.Message{{Role: session.RoleUser, Parts: []session.Part{
		{Type: session.PartText, Text: "listen"},
		mediaPart(hash, "audio/mpeg"),
		mediaPart(hash, "audio/wav"),
		mediaPart(hash, "video/mp4"),
		mediaPart(hash, "application/pdf"),
	}}}, chatReplay{}, mediaReader{inputs: allInputs, read: readHello})
	if err != nil {
		t.Fatal(err)
	}
	encoded, err := json.Marshal(messages[0])
	if err != nil {
		t.Fatal(err)
	}
	assertWireJSON(t, encoded, `{
		"role": "user",
		"content": [
			{"type": "text", "text": "listen"},
			{"type": "input_audio", "input_audio": {"data": "aGVsbG8=", "format": "mp3"}},
			{"type": "input_audio", "input_audio": {"data": "aGVsbG8=", "format": "wav"}},
			{"type": "video_url", "video_url": {"url": "data:video/mp4;base64,aGVsbG8="}},
			{"type": "file", "file": {"filename": "abababababababab.pdf", "file_data": "data:application/pdf;base64,aGVsbG8="}}
		]
	}`)
}

func TestToChatMessagesReplacesUnreadableBlob(t *testing.T) {
	// A missing blob must not fail the request: every later turn, compaction
	// included, would carry the same media and fail the same way.
	message := session.Message{Role: session.RoleTool, Parts: []session.Part{
		{Type: session.PartToolResult, ToolCallID: "1", ToolName: "read", ToolOutput: "loaded"},
		mediaPart(strings.Repeat("0", 64), "image/png"),
	}}
	messages, err := toChatMessages([]session.Message{message}, chatReplay{}, mediaReader{inputs: allInputs, read: func(string) ([]byte, error) { return nil, errors.New("missing") }})
	if err != nil {
		t.Fatal(err)
	}
	content, ok := messages[0].Content.(*string)
	if !ok || *content != "loaded\n[image unavailable: its stored copy could not be read]" {
		t.Fatalf("content = %#v, want text with a placeholder", messages[0].Content)
	}
}

func TestToChatMessagesKeepsReadableImagesBesideAnUnreadableOne(t *testing.T) {
	good := strings.Repeat("1", 64)
	message := session.Message{Role: session.RoleUser, Parts: []session.Part{
		{Type: session.PartText, Text: "compare"},
		mediaPart(good, "image/png"),
		mediaPart(strings.Repeat("2", 64), "image/png"),
	}}
	messages, err := toChatMessages([]session.Message{message}, chatReplay{}, mediaReader{inputs: allInputs, read: func(hash string) ([]byte, error) {
		if hash != good {
			return nil, errors.New("missing")
		}
		return []byte("hello"), nil
	}})
	if err != nil {
		t.Fatal(err)
	}
	parts, ok := messages[0].Content.(*[]chatContentPart)
	if !ok || len(*parts) != 3 {
		t.Fatalf("content = %#v", messages[0].Content)
	}
	if (*parts)[1].Type != "image_url" || (*parts)[2].Type != "text" || (*parts)[2].Text != unavailableText(session.ModalityImage) {
		t.Fatalf("parts = %#v", *parts)
	}
}

// TestToChatMessagesOmitsMediaTheModelDoesNotAccept checks that each part is
// gated by its own modality: an image model still gets the image, and the
// audio beside it becomes a placeholder.
func TestToChatMessagesOmitsMediaTheModelDoesNotAccept(t *testing.T) {
	message := session.Message{Role: session.RoleTool, Parts: []session.Part{
		{Type: session.PartToolResult, ToolCallID: "1", ToolName: "read", ToolOutput: "loaded"},
		mediaPart(strings.Repeat("0", 64), "image/png"),
		mediaPart(strings.Repeat("0", 64), "audio/wav"),
	}}
	messages, err := toChatMessages([]session.Message{message}, chatReplay{}, mediaReader{inputs: []session.Modality{session.ModalityImage}, read: readHello})
	if err != nil {
		t.Fatal(err)
	}
	parts, ok := messages[0].Content.(*[]chatContentPart)
	if !ok || len(*parts) != 3 || (*parts)[1].Type != "image_url" || (*parts)[2].Text != "[audio omitted: the active model does not accept audio input]" {
		t.Fatalf("content = %#v", messages[0].Content)
	}
}

func TestToChatMessagesCollapsesToTextWithoutAcceptedMedia(t *testing.T) {
	message := session.Message{Role: session.RoleTool, Parts: []session.Part{
		{Type: session.PartToolResult, ToolCallID: "1", ToolName: "read", ToolOutput: "loaded"},
		mediaPart(strings.Repeat("0", 64), "image/png"),
	}}
	messages, err := toChatMessages([]session.Message{message}, chatReplay{}, mediaReader{read: readHello})
	if err != nil {
		t.Fatal(err)
	}
	// The image placeholder keeps the bytes sessions sent before media
	// generalized images, so their cached prefixes survive the upgrade.
	content, ok := messages[0].Content.(*string)
	if !ok || *content != "loaded\n[image omitted: the active model does not accept image input]" {
		t.Fatalf("content = %#v, want text with a placeholder", messages[0].Content)
	}
}

// TestModelWithoutImageInputSendsNoImages covers a /model switch: the session
// still holds an image read by an earlier model, and the new model's profile
// does not accept images.
func TestModelWithoutImageInputSendsNoImages(t *testing.T) {
	for _, inputs := range [][]session.Modality{nil, {session.ModalityImage}} {
		var body string
		server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			b, _ := io.ReadAll(r.Body)
			body = string(b)
			_, _ = io.WriteString(w, okStream)
		}))
		spec := Spec{Name: "m", Format: wire.OpenAICompatible, ModelID: "m", BaseURL: server.URL, Inputs: inputs}
		model, err := newChatModel(spec, wire.Spec{ReasoningField: "reasoning_content"}, mediaReader{inputs: spec.Inputs, read: readHello})
		if err != nil {
			t.Fatal(err)
		}
		messages := []session.Message{{Role: session.RoleUser, Parts: []session.Part{
			{Type: session.PartText, Text: "look"},
			mediaPart(strings.Repeat("0", 64), "image/png"),
		}}}
		_, err = model.Complete(context.Background(), messages, nil, 0, nil)
		server.Close()
		if err != nil {
			t.Fatal(err)
		}
		if sent, want := strings.Contains(body, `"image_url"`), len(inputs) > 0; sent != want {
			t.Fatalf("inputs=%v: image sent = %v in %s", inputs, sent, body)
		}
	}
}

// TestContentBlocksMapsDocumentsAndOmitsAudio covers the Messages API, which
// carries PDFs as document blocks and has no block for audio or video.
func TestContentBlocksMapsDocumentsAndOmitsAudio(t *testing.T) {
	hash := strings.Repeat("0", 64)
	message := session.Message{Role: session.RoleUser, Parts: []session.Part{
		{Type: session.PartText, Text: "read"},
		mediaPart(hash, "application/pdf"),
		mediaPart(hash, "audio/wav"),
	}}
	encoded, err := json.Marshal(contentBlocks(message, mediaReader{inputs: allInputs, read: readHello}))
	if err != nil {
		t.Fatal(err)
	}
	assertWireJSON(t, encoded, `[
		{"type": "text", "text": "read"},
		{"type": "document", "source": {"type": "base64", "media_type": "application/pdf", "data": "aGVsbG8="}},
		{"type": "text", "text": "[audio omitted: the active model does not accept audio input]"}
	]`)
}

// TestResponsesContentMapsFilesAndOmitsVideo covers the Responses API, which
// carries PDFs as input_file and has no input for audio or video.
func TestResponsesContentMapsFilesAndOmitsVideo(t *testing.T) {
	hash := strings.Repeat("cd", 32)
	model := &responsesModel{media: mediaReader{inputs: allInputs, read: readHello}}
	encoded, err := json.Marshal(model.responsesContent(session.Message{Role: session.RoleUser, Parts: []session.Part{
		{Type: session.PartText, Text: "read"},
		mediaPart(hash, "application/pdf"),
		mediaPart(hash, "video/mp4"),
	}}))
	if err != nil {
		t.Fatal(err)
	}
	assertWireJSON(t, encoded, `[
		{"type": "input_text", "text": "read"},
		{"type": "input_file", "filename": "cdcdcdcdcdcdcdcd.pdf", "file_data": "data:application/pdf;base64,aGVsbG8="},
		{"type": "input_text", "text": "[video omitted: the active model does not accept video input]"}
	]`)
}
