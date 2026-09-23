package main

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestModelsListsBundledCatalogWithoutWritingCache(t *testing.T) {
	cachePath := filepath.Join(t.TempDir(), "models.json.gz")
	var output bytes.Buffer
	if err := runModels(false, cachePath, &output); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(output.String(), "openai/") {
		t.Fatalf("bundled models missing from output: %q", output.String())
	}
	if _, err := os.Stat(cachePath); !os.IsNotExist(err) {
		t.Fatalf("listing wrote a cache: %v", err)
	}
}

func TestModelsRejectsUnexpectedArguments(t *testing.T) {
	configHome, dataHome := t.TempDir(), t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", configHome)
	t.Setenv("XDG_DATA_HOME", dataHome)
	for _, args := range [][]string{{"--unknown"}, {"--refresh", "extra"}} {
		if err := runModelsCmd(args); err == nil {
			t.Fatalf("args %q: expected an error", args)
		}
	}
}
