package app

import (
	"path/filepath"
	"testing"

	"github.com/hizkifw/kon/internal/config"
	"github.com/hizkifw/kon/internal/session"
	"github.com/hizkifw/kon/internal/typedid"
)

// twoModels configures "fast" as the default and "review" as a second model.
func twoModels() config.Config {
	cfg := configured("gpt-4o")
	cfg.DefaultModel = "fast"
	cfg.Models[0].Name = "fast"
	cfg.Models = append(cfg.Models, config.Model{Name: "review", Type: "openai-compatible", ModelID: "gpt-5", BaseURL: "https://example.test/v1"})
	return cfg
}

// sessionOnReview leaves a persisted session whose last recorded model is
// review, while the config's default stays fast.
func sessionOnReview(t *testing.T, paths config.Paths, cwd string) typedid.SessionID {
	t.Helper()
	runtime, err := New(twoModels(), paths, cwd, "test")
	if err != nil {
		t.Fatal(err)
	}
	if _, err := runtime.store.AppendMessage(session.Message{Role: session.RoleUser, Parts: []session.Part{{Type: session.PartText, Text: "hello"}}}); err != nil {
		t.Fatal(err)
	}
	if err := runtime.SwitchModel("review"); err != nil {
		t.Fatal(err)
	}
	id := runtime.SessionID()
	if err := runtime.Close(); err != nil {
		t.Fatal(err)
	}
	return id
}

func modelChanges(entries []session.Entry) []string {
	var names []string
	for _, entry := range entries {
		if entry.Type == session.EntryTypeModelChange && entry.Model != nil {
			names = append(names, entry.Model.Name)
		}
	}
	return names
}

func resumePaths(t *testing.T) config.Paths {
	dir := t.TempDir()
	return config.Paths{Sessions: filepath.Join(dir, "sessions"), ConfigFile: filepath.Join(dir, "config.json")}
}

func TestResumeRestoresTheSessionsLastModel(t *testing.T) {
	paths, cwd := resumePaths(t), t.TempDir()
	id := sessionOnReview(t, paths, cwd)

	runtime, err := NewResumedID(twoModels(), paths, cwd, "test", id)
	if err != nil {
		t.Fatal(err)
	}
	defer runtime.Close()
	if state := runtime.State(); !state.Ready() || state.Active.Name != "review" {
		t.Fatalf("resumed on %q, want the session's last model", state.Active.Name)
	}
	// Restoring the recorded model is not a switch, so nothing is appended.
	if got := modelChanges(runtime.SessionHistory()); len(got) != 2 || got[1] != "review" {
		t.Fatalf("model changes = %q", got)
	}
}

func TestResumeCommandRestoresTheSessionsLastModel(t *testing.T) {
	paths, cwd := resumePaths(t), t.TempDir()
	id := sessionOnReview(t, paths, cwd)

	runtime, err := New(twoModels(), paths, cwd, "test")
	if err != nil {
		t.Fatal(err)
	}
	defer runtime.Close()
	if err := runtime.Resume(id); err != nil {
		t.Fatal(err)
	}
	if active := runtime.State().Active; active.Name != "review" {
		t.Fatalf("/resume left the model on %q", active.Name)
	}
}

func TestResumeRecordsAFallbackWhenTheModelIsGone(t *testing.T) {
	paths, cwd := resumePaths(t), t.TempDir()
	id := sessionOnReview(t, paths, cwd)

	cfg := twoModels()
	cfg.Models = cfg.Models[:1]
	runtime, err := NewResumedID(cfg, paths, cwd, "test", id)
	if err != nil {
		t.Fatal(err)
	}
	defer runtime.Close()
	if active := runtime.State().Active; active.Name != "fast" {
		t.Fatalf("resumed on %q, want the default", active.Name)
	}
	// The session now says which model answers from here on.
	if got := modelChanges(runtime.SessionHistory()); len(got) != 3 || got[2] != "fast" {
		t.Fatalf("model changes = %q", got)
	}
}

func TestDescribeSelectionNamesARemovedModel(t *testing.T) {
	runtime, err := New(twoModels(), resumePaths(t), t.TempDir(), "test")
	if err != nil {
		t.Fatal(err)
	}
	defer runtime.Close()
	gone := session.ModelSelection{Name: "old/model-x", WireFormat: "openai", ConnectionID: "old", ExternalID: typedid.ExternalModelID("model-x")}
	if got := runtime.DescribeSelection(gone); got.Name != gone.Name || got.ConnectionID != "old" || got.DisplayName != "model-x" {
		t.Fatalf("DescribeSelection = %#v", got)
	}
	if got := runtime.DescribeSelection(session.ModelSelection{Name: "review"}); got.Name != "review" || got.ExternalID != "gpt-5" {
		t.Fatalf("DescribeSelection(review) = %#v", got)
	}
}
