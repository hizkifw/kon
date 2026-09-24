package ui

import (
	"context"
	"strings"
	"testing"

	"github.com/hizkifw/kon/internal/app"
	"github.com/hizkifw/kon/internal/history"
	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/typedid"
)

func modelChange(name, externalID string) session.Entry {
	return session.Entry{Type: session.EntryTypeModelChange, Model: &session.ModelSelection{Name: name, WireFormat: "openai", ExternalID: typedid.ExternalModelID(externalID)}}
}

func userEntry(text string) session.Entry {
	message := session.TextMessage(session.RoleUser, text)
	return session.Entry{Type: session.EntryTypeMessage, Message: &message}
}

func modelBlocks(t *transcript) []string {
	var texts []string
	for _, b := range t.blocks {
		if b.kind == blockModel {
			texts = append(texts, b.text)
		}
	}
	return texts
}

func TestReplayShowsModelSwitchesButNotTheStartingModel(t *testing.T) {
	models := []app.Model{
		{Name: "fast", WireFormat: "openai", ExternalID: "gpt"},
		{Name: "review", ConnectionID: "fireworks-ai", DisplayName: "DeepSeek V4.1 Flash", ExternalID: "deepseek", ReasoningEfforts: []string{"low"}},
	}
	runtime := &fakeRuntime{
		state:  app.State{Active: models[1], Phase: app.PhaseReady},
		models: models,
		entries: []session.Entry{
			modelChange("fast", "gpt"), userEntry("one"),
			modelChange("review", "deepseek"), userEntry("two"),
			// A resume that restores the same model records nothing new, but
			// a repeated record must not render as a switch either.
			modelChange("review", "deepseek"),
		},
	}
	m := New(context.Background(), "/tmp", "/tmp/config.json", runtime, history.New(t.TempDir()+"/history.jsonl"), nil)
	got := modelBlocks(&m.transcript)
	want := " Model changed to fireworks-ai · DeepSeek V4.1 Flash · default"
	if len(got) != 1 || got[0] != want {
		t.Fatalf("replayed model changes = %q, want [%q]", got, want)
	}
}

func TestReplayedModelChangeIsRetitledWhenTheCatalogLoads(t *testing.T) {
	runtime := &fakeRuntime{
		state:   app.State{Phase: app.PhaseReady},
		entries: []session.Entry{modelChange("fast", "gpt"), modelChange("fireworks-ai/deepseek", "deepseek")},
	}
	m := New(context.Background(), "/tmp", "/tmp/config.json", runtime, history.New(t.TempDir()+"/history.jsonl"), nil)
	m.width, m.height = 80, 24
	m.resize()
	m.refreshTranscript(false)
	if got := modelBlocks(&m.transcript); len(got) != 1 || !strings.Contains(got[0], "deepseek") {
		t.Fatalf("before the catalog = %q", got)
	}

	// The catalog supplies the display name the header uses.
	runtime.models = []app.Model{{Name: "fireworks-ai/deepseek", ConnectionID: "fireworks-ai", DisplayName: "DeepSeek V4"}}
	updated, _ := m.Update(catalogLoadedMsg{})
	m = updated.(Model)
	want := " Model changed to fireworks-ai · DeepSeek V4"
	if got := modelBlocks(&m.transcript); len(got) != 1 || got[0] != want {
		t.Fatalf("after the catalog = %q, want %q", got, want)
	}
	if rendered := strings.Join(m.transcript.linesFor(m.width), "\n"); !strings.Contains(rendered, "DeepSeek V4") {
		t.Fatalf("cached rendering kept the old title:\n%s", rendered)
	}
}
