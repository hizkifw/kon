package acp

import (
	"encoding/base64"
	"encoding/json"
	"testing"

	"kon.kitsu.red/core/acp"
)

func TestConvertPromptJoinsTextMentionsAndAttachments(t *testing.T) {
	s := &liveSession{cwd: "/work"}
	contents := "func main() {}\n"
	prompt, err := s.convertPrompt([]acp.ContentBlock{
		{Type: "text", Text: "compare "},
		{Type: "resource_link", URI: "file:///work/a.go", Name: "a.go"},
		{Type: "text", Text: " with "},
		{Type: "resource", Resource: &acp.Resource{URI: "file:///elsewhere/my b.go", Text: &contents}},
		{Type: "image", Data: base64.StdEncoding.EncodeToString([]byte("png")), MIMEType: "image/png"},
	})
	if err != nil {
		t.Fatal(err)
	}
	want := "compare @a.go with @\"/elsewhere/my b.go\"\n\n@\"/elsewhere/my b.go\"\n```\nfunc main() {}\n```"
	if prompt.Text != want {
		t.Fatalf("text = %q, want %q", prompt.Text, want)
	}
	if len(prompt.Media) != 1 || string(prompt.Media[0].Data) != "png" || prompt.Media[0].MIME != "image/png" {
		t.Fatalf("media = %#v", prompt.Media)
	}
}

func TestConvertPromptRefusesWhatItCannotSend(t *testing.T) {
	s := &liveSession{cwd: "/work"}
	for name, blocks := range map[string][]acp.ContentBlock{
		"empty":        {{Type: "text", Text: "  "}},
		"unknown type": {{Type: "video"}},
		"bad base64":   {{Type: "image", Data: "%%%", MIMEType: "image/png"}},
		"no mime":      {{Type: "audio", Data: "AAAA"}},
	} {
		if _, err := s.convertPrompt(blocks); err == nil {
			t.Errorf("%s: accepted", name)
		}
	}
}

func TestFenceOutrunsBackticksInside(t *testing.T) {
	if got := fence("a ```` b"); got != "`````\na ```` b\n`````" {
		t.Fatalf("fence = %q", got)
	}
}

func TestEditCallCarriesItsDiffUntilItFails(t *testing.T) {
	s := &liveSession{cwd: "/work"}
	args := json.RawMessage(`{"path":"a.go","old_text":"x","new_text":"y"}`)
	started := s.startedCall("call-1", "edit", args)
	if started.Kind != "edit" || len(started.Content) != 1 {
		t.Fatalf("started = %#v", started)
	}
	diff := started.Content[0]
	if diff.Type != "diff" || diff.Path != "/work/a.go" || *diff.OldText != "x" || *diff.NewText != "y" {
		t.Fatalf("diff = %#v", diff)
	}
	if done := s.finishedCall("call-1", "edit", args, "edited a.go", false, nil); done.Status != "completed" || done.Content != nil {
		t.Fatalf("finished = %#v", done)
	}
	failed := s.finishedCall("call-1", "edit", args, "old_text not found", true, nil)
	if failed.Status != "failed" || failed.Content[0].Content.Text != "old_text not found" {
		t.Fatalf("failed = %#v", failed)
	}
}
