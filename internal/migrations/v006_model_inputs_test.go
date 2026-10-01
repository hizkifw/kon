package migrations

import (
	"bytes"
	"context"
	"encoding/json"
	"reflect"
	"testing"

	"kon.kitsu.red/core/session"
	"kon.kitsu.red/internal/config"
)

func TestModelVisionBecomesInputs(t *testing.T) {
	paths := instructionsPaths(t, `{"default_model": "seeing", "models": [
		{"name": "seeing", "model": "a", "type": "openai", "api_key": "", "vision": true},
		{"name": "blind", "model": "b", "type": "openai", "api_key": "", "vision": false},
		{"name": "listed", "model": "c", "type": "openai", "api_key": "", "vision": true, "inputs": ["audio"]},
		{"name": "plain", "model": "d", "type": "openai", "api_key": ""}
	]}`)
	for range 2 {
		if err := (modelInputsV6{}).Run(context.Background(), paths); err != nil {
			t.Fatal(err)
		}
	}
	var cfg struct {
		Models []map[string]json.RawMessage `json:"models"`
	}
	if err := json.Unmarshal([]byte(readFile(t, paths.ConfigFile)), &cfg); err != nil {
		t.Fatal(err)
	}
	want := []string{`["image"]`, "", `["audio"]`, ""}
	for i, model := range cfg.Models {
		if _, ok := model["vision"]; ok {
			t.Fatalf("models[%d] still has vision", i)
		}
		var compact bytes.Buffer
		if raw := model["inputs"]; raw != nil {
			if err := json.Compact(&compact, raw); err != nil {
				t.Fatal(err)
			}
		}
		if got := compact.String(); got != want[i] {
			t.Fatalf("models[%d].inputs = %q, want %q", i, got, want[i])
		}
	}
	loaded, err := config.Load(paths.ConfigFile)
	if err != nil {
		t.Fatalf("migrated config does not load: %v", err)
	}
	if got := loaded.Models[0].Inputs; !reflect.DeepEqual(got, []session.Modality{session.ModalityImage}) {
		t.Fatalf("loaded inputs = %v", got)
	}
}

func TestModelInputsLeavesConfigWithoutVisionUntouched(t *testing.T) {
	const contents = `{"models": [{"name": "plain", "model": "d"}]}`
	paths := instructionsPaths(t, contents)
	if err := (modelInputsV6{}).Run(context.Background(), paths); err != nil {
		t.Fatal(err)
	}
	if got := readFile(t, paths.ConfigFile); got != contents {
		t.Fatalf("config rewritten:\n%s", got)
	}
}
