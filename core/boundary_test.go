package core

import (
	"go/parser"
	"go/token"
	"io/fs"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
)

// TestCoreImportsNoInternalPackage keeps core usable from another module: Go
// forbids importing internal packages across modules, so one import from
// internal or cmd would make core unusable outside kon. Test files count too,
// since core's tests should prove it works without the CLI.
func TestCoreImportsNoInternalPackage(t *testing.T) {
	const module = "kon.kitsu.red/"
	err := filepath.WalkDir(".", func(path string, entry fs.DirEntry, err error) error {
		if err != nil || entry.IsDir() || !strings.HasSuffix(path, ".go") {
			return err
		}
		file, err := parser.ParseFile(token.NewFileSet(), path, nil, parser.ImportsOnly)
		if err != nil {
			return err
		}
		for _, spec := range file.Imports {
			imported, _ := strconv.Unquote(spec.Path.Value)
			if strings.HasPrefix(imported, module) && !strings.HasPrefix(imported, module+"core/") {
				t.Errorf("%s imports %s", path, imported)
			}
		}
		return nil
	})
	if err != nil {
		t.Fatal(err)
	}
}
