package ui

import (
	"strings"
	"testing"

	"kon.kitsu.red/core/session"
	"kon.kitsu.red/internal/app"
)

func TestModelsListsResolvedMediaInputs(t *testing.T) {
	m := newTestModel(t)
	runtime := m.runtime.(*fakeRuntime)
	runtime.models = []app.Model{
		{Name: "local/vision", ConnectionID: "local", DisplayName: "vision", ExternalID: "vision", Source: "provider list", Inputs: []session.Modality{session.ModalityImage}},
		{Name: "local/text", ConnectionID: "local", DisplayName: "text", ExternalID: "text", Source: "provider list"},
	}
	updated, _ := m.listModels()
	model := updated.(Model)
	got := model.transcript.blocks[len(model.transcript.blocks)-1].text
	if !strings.Contains(got, "local · vision  vision  [provider list]  [image]") || strings.Contains(got, "local · text  text  [provider list]  [image]") {
		t.Fatalf("model listing = %q", got)
	}
}
