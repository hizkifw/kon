package tui

import (
	"go/parser"
	"go/token"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
)

// TestImportsNoKonPackage keeps tui generic: a widget that reached into kon's
// conversation, sessions, or runtime would belong in internal/ui instead.
func TestImportsNoKonPackage(t *testing.T) {
	files, err := filepath.Glob("*.go")
	if err != nil {
		t.Fatal(err)
	}
	for _, path := range files {
		file, err := parser.ParseFile(token.NewFileSet(), path, nil, parser.ImportsOnly)
		if err != nil {
			t.Fatal(err)
		}
		for _, spec := range file.Imports {
			imported, _ := strconv.Unquote(spec.Path.Value)
			if strings.HasPrefix(imported, "github.com/hizkifw/kon/") {
				t.Errorf("%s imports %s", path, imported)
			}
		}
	}
}
