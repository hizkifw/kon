package productdocs

import (
	"os"
	"path/filepath"
	"testing"
	"testing/fstest"
)

func TestExtractTracksBundledVersion(t *testing.T) {
	dataDir := t.TempDir()
	first := fstest.MapFS{
		"index.md": &fstest.MapFile{Data: []byte("first index")},
		"old.md":   &fstest.MapFile{Data: []byte("old page")},
	}
	firstDir, err := extract(first, dataDir)
	if err != nil {
		t.Fatal(err)
	}
	assertDocs(t, firstDir, map[string]string{"index.md": "first index", "old.md": "old page"})

	second := fstest.MapFS{
		"index.md": &fstest.MapFile{Data: []byte("revised index")},
		"new.md":   &fstest.MapFile{Data: []byte("new page")},
	}
	secondDir, err := extract(second, dataDir)
	if err != nil {
		t.Fatal(err)
	}
	if secondDir == firstDir {
		t.Fatal("changed bundle reused the old directory")
	}
	assertDocs(t, secondDir, map[string]string{"index.md": "revised index", "new.md": "new page"})
	assertDocs(t, firstDir, map[string]string{"index.md": "first index", "old.md": "old page"})
}

func TestExtractRepairsModifiedDirectory(t *testing.T) {
	dataDir := t.TempDir()
	source := fstest.MapFS{"index.md": &fstest.MapFile{Data: []byte("bundled")}}
	dir, err := extract(source, dataDir)
	if err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "index.md"), []byte("modified"), 0600); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "stale.md"), []byte("stale"), 0600); err != nil {
		t.Fatal(err)
	}
	again, err := extract(source, dataDir)
	if err != nil {
		t.Fatal(err)
	}
	if again != dir {
		t.Fatalf("extraction path changed: %q != %q", again, dir)
	}
	assertDocs(t, dir, map[string]string{"index.md": "bundled"})
}

func assertDocs(t *testing.T, dir string, want map[string]string) {
	t.Helper()
	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Fatal(err)
	}
	if len(entries) != len(want) {
		t.Fatalf("got %d files, want %d", len(entries), len(want))
	}
	for _, entry := range entries {
		content, err := os.ReadFile(filepath.Join(dir, entry.Name()))
		if err != nil {
			t.Fatal(err)
		}
		if string(content) != want[entry.Name()] {
			t.Fatalf("%s = %q, want %q", entry.Name(), content, want[entry.Name()])
		}
	}
}
