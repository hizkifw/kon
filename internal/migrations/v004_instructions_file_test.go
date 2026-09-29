package migrations

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/hizkifw/kon/internal/config"
)

func instructionsPaths(t *testing.T, contents string) config.Paths {
	t.Helper()
	dir := t.TempDir()
	paths := config.Paths{ConfigDir: dir, ConfigFile: filepath.Join(dir, "config.json")}
	if err := os.WriteFile(paths.ConfigFile, []byte(contents), 0o600); err != nil {
		t.Fatal(err)
	}
	return paths
}

func readFile(t *testing.T, path string) string {
	t.Helper()
	b, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	return string(b)
}

func TestInstructionsMoveToAgentsFile(t *testing.T) {
	paths := instructionsPaths(t, `{"default_model": "", "models": [], "instructions": "  be terse\n"}`)
	agents := filepath.Join(paths.ConfigDir, "AGENTS.md")
	if err := os.WriteFile(agents, []byte("existing rules\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	for range 2 {
		if err := (instructionsFileV4{}).Run(context.Background(), paths); err != nil {
			t.Fatal(err)
		}
	}
	if got := readFile(t, agents); got != "existing rules\n\nbe terse\n" {
		t.Fatalf("AGENTS.md:\n%q", got)
	}
	if _, err := config.Load(paths.ConfigFile); err != nil {
		t.Fatalf("migrated config does not load: %v", err)
	}
}

func TestInstructionsRetryAfterAgentsFileWritten(t *testing.T) {
	paths := instructionsPaths(t, `{"models": [], "instructions": "be terse"}`)
	agents := filepath.Join(paths.ConfigDir, "AGENTS.md")
	// A crash after the first write leaves the text in AGENTS.md and the config.
	if err := os.WriteFile(agents, []byte("be terse\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := (instructionsFileV4{}).Run(context.Background(), paths); err != nil {
		t.Fatal(err)
	}
	if got := readFile(t, agents); got != "be terse\n" {
		t.Fatalf("AGENTS.md:\n%q", got)
	}
	if strings.Contains(readFile(t, paths.ConfigFile), "instructions") {
		t.Fatal("instructions field was kept")
	}
}

func TestEmptyInstructionsCreateNoAgentsFile(t *testing.T) {
	paths := instructionsPaths(t, `{"default_model": "", "models": [], "instructions": ""}`)
	if err := (instructionsFileV4{}).Run(context.Background(), paths); err != nil {
		t.Fatal(err)
	}
	if _, err := os.Stat(filepath.Join(paths.ConfigDir, "AGENTS.md")); !os.IsNotExist(err) {
		t.Fatalf("AGENTS.md created for empty instructions: %v", err)
	}
	if _, err := config.Load(paths.ConfigFile); err != nil {
		t.Fatalf("migrated config does not load: %v", err)
	}
}

func TestInstructionsLeaveAnUnreadableConfig(t *testing.T) {
	for _, broken := range []string{`{"instructions": "x"`, `{"instructions": 5}`} {
		paths := instructionsPaths(t, broken)
		if err := (instructionsFileV4{}).Run(context.Background(), paths); err != nil {
			t.Fatal(err)
		}
		if got := readFile(t, paths.ConfigFile); got != broken {
			t.Fatalf("config changed:\n%s", got)
		}
	}
}

func TestInstructionsKeepFieldsTheyDoNotOwn(t *testing.T) {
	paths := instructionsPaths(t, `{"instructions": "be terse", "unknown": true}`)
	if err := (instructionsFileV4{}).Run(context.Background(), paths); err != nil {
		t.Fatal(err)
	}
	if got := readFile(t, paths.ConfigFile); strings.Contains(got, "instructions") || !strings.Contains(got, `"unknown": true`) {
		t.Fatalf("migrated config:\n%s", got)
	}
}
