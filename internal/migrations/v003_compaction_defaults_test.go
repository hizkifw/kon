package migrations

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/hizkifw/kon/internal/config"
)

func runCompactionDefaults(t *testing.T, contents string) (string, error) {
	t.Helper()
	paths := config.Paths{ConfigFile: filepath.Join(t.TempDir(), "config.json")}
	if err := os.WriteFile(paths.ConfigFile, []byte(contents), 0o600); err != nil {
		t.Fatal(err)
	}
	err := compactionDefaultsV3{}.Run(context.Background(), paths)
	b, readErr := os.ReadFile(paths.ConfigFile)
	if readErr != nil {
		t.Fatal(readErr)
	}
	return string(b), err
}

func TestCompactionDefaultsAreDropped(t *testing.T) {
	written := `{
  "default_model": "fast",
  "models": [{"name": "fast", "type": "openai", "model": "gpt", "api_key": "k", "context_window_tokens": 128000}],
  "compaction": {"reserve_tokens": 16384, "keep_recent_tokens": 20000}
}`
	got, err := runCompactionDefaults(t, written)
	if err != nil {
		t.Fatal(err)
	}
	if strings.Contains(got, "compaction") || !strings.Contains(got, `"default_model": "fast"`) || !strings.Contains(got, `"context_window_tokens": 128000`) {
		t.Fatalf("migrated config:\n%s", got)
	}
	// A retry after a run that already finished changes nothing.
	again, err := runCompactionDefaults(t, got)
	if err != nil || again != got {
		t.Fatalf("second run: err %v, config\n%s", err, again)
	}
}

func TestCompactionBudgetsTheUserChangedStay(t *testing.T) {
	got, err := runCompactionDefaults(t, `{"models": [], "compaction": {"reserve_tokens": 40000, "keep_recent_tokens": 20000}}`)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(got, `"reserve_tokens": 40000`) || strings.Contains(got, "keep_recent_tokens") {
		t.Fatalf("migrated config:\n%s", got)
	}
}

func TestCompactionDefaultsLeaveAnUnreadableConfig(t *testing.T) {
	broken := `{"compaction": {"reserve_tokens": 16384}, "unknown": true}`
	if got, err := runCompactionDefaults(t, broken); err != nil || got != broken {
		t.Fatalf("err %v, config\n%s", err, got)
	}
	paths := config.Paths{ConfigFile: filepath.Join(t.TempDir(), "missing.json")}
	if err := (compactionDefaultsV3{}).Run(context.Background(), paths); err != nil {
		t.Fatalf("a missing config failed the migration: %v", err)
	}
}

func TestCompactionDefaultsKeepInstructionsForTheNextStep(t *testing.T) {
	got, err := runCompactionDefaults(t, `{"models": [], "compaction": {"reserve_tokens": 16384}, "instructions": "be terse"}`)
	if err != nil {
		t.Fatal(err)
	}
	if strings.Contains(got, "compaction") || !strings.Contains(got, `"instructions": "be terse"`) {
		t.Fatalf("migrated config:\n%s", got)
	}
}
