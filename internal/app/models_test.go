package app

import (
	"bytes"
	"encoding/base64"
	"encoding/json"
	"fmt"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"slices"
	"testing"

	"kon.kitsu.red/core/agent"
	"kon.kitsu.red/core/provider/wire"
	"kon.kitsu.red/core/session"
	"kon.kitsu.red/internal/config"
	"kon.kitsu.red/internal/login"
)

// Exercise the same login, listing, selection, and restart path as the TUI,
// then have the model read an image and inspect the next provider request.
func TestDiscoveredInputsReachReadAndProviderAfterRestart(t *testing.T) {
	for _, tc := range []struct {
		name, architecture string
		image              bool
	}{
		{"vision", `,"architecture":{"input_modalities":["text","image"],"output_modalities":["text"]}`, true},
		{"text", `,"architecture":{"input_modalities":["text"]}`, false},
		{"id-only", "", false},
	} {
		t.Run(tc.name, func(t *testing.T) {
			const id = "Qwen3.8-27B-IQ3_S-3.23bpw"
			var requests [][]byte
			server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
				switch req.URL.Path {
				case "/v1/models":
					fmt.Fprintf(w, `{"data":[{"id":"cached","status":{"value":"unloaded"}},{"id":%q,"status":{"value":"loaded"}%s}]}`, id, tc.architecture)
				case "/v1/chat/completions":
					var body json.RawMessage
					if err := json.NewDecoder(req.Body).Decode(&body); err != nil {
						t.Error(err)
					}
					requests = append(requests, body)
					w.Header().Set("Content-Type", "text/event-stream")
					if len(requests) == 1 {
						fmt.Fprint(w, "data: {\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"read-image\",\"type\":\"function\",\"function\":{\"name\":\"read\",\"arguments\":\"{\\\"path\\\":\\\"pixel.png\\\"}\"}}]},\"finish_reason\":\"tool_calls\"}]}\n\ndata: [DONE]\n\n")
					} else {
						fmt.Fprint(w, "data: {\"choices\":[{\"delta\":{\"content\":\"done\"},\"finish_reason\":\"stop\"}]}\n\ndata: [DONE]\n\n")
					}
				default:
					t.Errorf("unexpected request: %s", req.URL.Path)
					w.WriteHeader(http.StatusNotFound)
				}
			}))
			defer server.Close()
			root, cwd := t.TempDir(), t.TempDir()
			paths := config.Paths{ConfigFile: filepath.Join(root, "config.json"), Sessions: filepath.Join(root, "sessions"), ProviderModels: filepath.Join(root, "provider-models.json")}
			cfg := config.Default()
			if err := cfg.Save(paths.ConfigFile); err != nil {
				t.Fatal(err)
			}
			r, err := New(cfg, paths, cwd, "test")
			if err != nil {
				t.Fatal(err)
			}
			t.Cleanup(func() { _ = r.Close() })
			connection := config.Provider{ID: "openai-compatible", Type: wire.OpenAICompatible, BaseURL: server.URL + "/v1"}
			if count, verified, err := r.Login(t.Context(), connection); err != nil || !verified || count != 2 {
				t.Fatalf("login = %d, %v, %v", count, verified, err)
			}
			name := connection.ID + "/" + id
			found := false
			for _, model := range r.Models() {
				if model.Name == name {
					found = true
					if slices.Contains(model.Inputs, session.ModalityImage) != tc.image {
						t.Fatalf("listed inputs = %v", model.Inputs)
					}
				} else if len(model.Inputs) != 0 {
					t.Fatalf("capabilities leaked to cached model: %+v", model)
				}
			}
			if !found {
				t.Fatal("discovered model is missing from /models")
			}
			if err := r.SwitchModel(name); err != nil {
				t.Fatal(err)
			}
			if err := r.Close(); err != nil {
				t.Fatal(err)
			}
			saved, err := config.Load(paths.ConfigFile)
			if err != nil {
				t.Fatal(err)
			}
			if len(saved.Models) != 0 {
				t.Fatal("discovery materialized an explicit profile")
			}
			reopened, err := New(saved, paths, cwd, "test")
			if err != nil {
				t.Fatal(err)
			}
			defer reopened.Close()
			if reopened.catalog.Load() != nil || slices.Contains(reopened.State().Active.Inputs, session.ModalityImage) != tc.image {
				t.Fatal("startup did not use cached inputs without loading the catalog")
			}
			pixel, err := base64.StdEncoding.DecodeString("iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+aXioAAAAASUVORK5CYII=")
			if err != nil {
				t.Fatal(err)
			}
			if err := os.WriteFile(filepath.Join(cwd, "pixel.png"), pixel, 0600); err != nil {
				t.Fatal(err)
			}
			if err := reopened.Run(t.Context(), "Read pixel.png", nil, func(agent.Event) {}); err != nil {
				t.Fatal(err)
			}
			if len(requests) != 2 {
				t.Fatalf("chat requests = %d, want 2", len(requests))
			}
			uri := []byte("data:image/png;base64," + base64.StdEncoding.EncodeToString(pixel))
			if got := bytes.Contains(requests[1], uri); got != tc.image {
				t.Fatalf("image attached = %v, want %v: %s", got, tc.image, requests[1])
			}
			if !tc.image && !bytes.Contains(requests[1], []byte("does not accept image input")) {
				t.Fatalf("missing unsupported-image notice: %s", requests[1])
			}
		})
	}
}

func TestProviderInputsPrecedence(t *testing.T) {
	const id = "accounts/fireworks/models/deepseek-v4p1-flash"
	for _, tc := range []struct {
		name     string
		reported []string
		image    bool
	}{
		{"absent", nil, true},
		{"image", []string{"text", "image"}, true},
		{"text", []string{"text"}, false},
		{"empty", []string{}, false},
		{"unknown", []string{"other"}, false},
	} {
		t.Run(tc.name, func(t *testing.T) {
			cfg := config.Default()
			cfg.Providers = []config.Provider{{ID: "local", CatalogProvider: "fireworks-ai", Type: wire.OpenAICompatible, BaseURL: "https://example.test/v1"}}
			cfg.Models = []config.Model{{Name: "explicit", ModelID: id}, {Name: "explicit-image", ModelID: id, Inputs: []session.Modality{session.ModalityImage}}}
			r := &Runtime{config: cfg, providerModels: map[string][]login.Model{"local": {{ID: id, InputModalities: tc.reported}}}}
			derived, _ := r.resolveModel("local/" + id)
			if slices.Contains(derived.Inputs, session.ModalityImage) != tc.image {
				t.Fatalf("derived inputs = %v", derived.Inputs)
			}
			for _, name := range []string{"explicit", "explicit-image"} {
				profile, _ := r.resolveModel(name)
				if slices.Contains(profile.Inputs, session.ModalityImage) != (name == "explicit-image") {
					t.Fatalf("explicit inputs changed: %+v", profile)
				}
			}
			other, _ := r.resolveModel("local/other")
			if len(other.Inputs) != 0 {
				t.Fatalf("inputs leaked to another model: %v", other.Inputs)
			}
		})
	}
}

func TestLoginRefreshesActiveInputs(t *testing.T) {
	for _, tc := range []struct {
		name, listing   string
		verified, image bool
	}{
		{"vision", `{"data":[{"id":"model","architecture":{"input_modalities":["text","image"]}}]}`, true, true},
		{"text", `{"data":[{"id":"model","architecture":{"input_modalities":["text"]}}]}`, true, false},
		{"id-only", `{"data":[{"id":"model"}]}`, true, false},
		{"unavailable", "", false, false},
	} {
		t.Run(tc.name, func(t *testing.T) {
			server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
				if tc.listing == "" {
					w.WriteHeader(http.StatusNotFound)
					return
				}
				fmt.Fprint(w, tc.listing)
			}))
			defer server.Close()
			root := t.TempDir()
			paths := config.Paths{ConfigFile: filepath.Join(root, "config.json"), Sessions: filepath.Join(root, "sessions"), ProviderModels: filepath.Join(root, "provider-models.json")}
			cfg := config.Default()
			connection := config.Provider{ID: "openai-compatible", Type: wire.OpenAICompatible, BaseURL: server.URL}
			cfg.Providers, cfg.DefaultModel = []config.Provider{connection}, "openai-compatible/model"
			if err := cfg.Save(paths.ConfigFile); err != nil {
				t.Fatal(err)
			}
			if err := saveProviderModels(paths.ProviderModels, map[string][]login.Model{connection.ID: {{ID: "model", InputModalities: []string{"image"}}}}); err != nil {
				t.Fatal(err)
			}
			r, err := New(cfg, paths, t.TempDir(), "test")
			if err != nil {
				t.Fatal(err)
			}
			defer r.Close()
			r.LoadCatalog()
			before := r.runner
			history := len(r.SessionHistory())
			if _, verified, err := r.Login(t.Context(), connection); err != nil || verified != tc.verified {
				t.Fatalf("login = %v, %v", verified, err)
			}
			if slices.Contains(r.State().Active.Inputs, session.ModalityImage) != tc.image || r.runner == before {
				t.Fatalf("active runner was not refreshed: inputs = %v", r.State().Active.Inputs)
			}
			if len(r.SessionHistory()) != history {
				t.Fatal("capability refresh changed the durable session")
			}
			cached := loadProviderModels(paths.ProviderModels)[connection.ID]
			if !tc.verified && len(cached) != 0 {
				t.Fatal("unverified login retained stale capabilities")
			}
		})
	}
}

func TestProviderModelCacheRetainsUnknownAndEmptyInputs(t *testing.T) {
	path := filepath.Join(t.TempDir(), "provider-models.json")
	if err := os.WriteFile(path, []byte(`{"local":["vision","text"]}`), 0600); err != nil {
		t.Fatal(err)
	}
	models := loadProviderModels(path)
	if len(models["local"]) != 2 || models["local"][0].ID != "vision" || models["local"][0].InputModalities != nil {
		t.Fatalf("legacy cache = %+v", models)
	}
	models["local"][1].InputModalities = []string{}
	if err := saveProviderModels(path, models); err != nil {
		t.Fatal(err)
	}
	loaded := loadProviderModels(path)["local"]
	if len(loaded) != 2 || loaded[0].InputModalities != nil || loaded[1].InputModalities == nil || len(loaded[1].InputModalities) != 0 {
		t.Fatalf("cache lost unknown versus empty inputs: %+v", loaded)
	}
}
