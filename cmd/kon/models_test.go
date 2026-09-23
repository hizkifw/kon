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
	if err := runModels(nil, cachePath, &output); err != nil {
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
	for _, args := range [][]string{{"--unknown"}, {"--refresh", "extra"}} {
		if err := runModels(args, "", &bytes.Buffer{}); err == nil || !strings.Contains(err.Error(), "usage: kon models") {
			t.Fatalf("args %q: error = %v", args, err)
		}
	}
}
