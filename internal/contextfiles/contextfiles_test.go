package contextfiles

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// within filters discovered files to those under root, so a test is not
// perturbed by instruction files that legitimately exist above the temp dir.
func within(files []File, root string) []File {
	var out []File
	for _, file := range files {
		if rel, err := filepath.Rel(root, file.Path); err == nil && !strings.HasPrefix(rel, "..") {
			out = append(out, file)
		}
	}
	return out
}

func writeFile(t *testing.T, path, content string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		t.Fatal(err)
	}
}

func paths(files []File) []string {
	out := make([]string, len(files))
	for i, file := range files {
		out[i] = file.Path
	}
	return out
}

func TestLoadOrdersOutermostToInnermost(t *testing.T) {
	root := t.TempDir()
	outer := filepath.Join(root, "outer")
	inner := filepath.Join(outer, "inner")
	writeFile(t, filepath.Join(root, "AGENTS.md"), "root")
	writeFile(t, filepath.Join(outer, "AGENTS.md"), "outer")
	writeFile(t, filepath.Join(inner, "AGENTS.md"), "inner")

	files, err := Load(inner)
	if err != nil {
		t.Fatal(err)
	}
	got := within(files, root)
	want := []string{
		filepath.Join(root, "AGENTS.md"),
		filepath.Join(outer, "AGENTS.md"),
		filepath.Join(inner, "AGENTS.md"),
	}
	if diff := compare(paths(got), want); diff != "" {
		t.Fatalf("order mismatch:\n got %v\nwant %v", paths(got), want)
	}
}

func TestLoadPrefersOverridePerDirectory(t *testing.T) {
	root := t.TempDir()
	dir := filepath.Join(root, "project")
	writeFile(t, filepath.Join(dir, "AGENTS.override.md"), "override")
	writeFile(t, filepath.Join(dir, "AGENTS.md"), "plain")
	writeFile(t, filepath.Join(dir, "CLAUDE.md"), "claude")

	files, err := Load(dir)
	if err != nil {
		t.Fatal(err)
	}
	got := within(files, root)
	if len(got) != 1 {
		t.Fatalf("expected one file per directory, got %v", paths(got))
	}
	if filepath.Base(got[0].Path) != "AGENTS.override.md" || got[0].Content != "override" {
		t.Fatalf("override did not win: %#v", got[0])
	}
}

func TestLoadPrefersAgentsOverClaude(t *testing.T) {
	root := t.TempDir()
	dir := filepath.Join(root, "project")
	writeFile(t, filepath.Join(dir, "AGENTS.md"), "agents")
	writeFile(t, filepath.Join(dir, "CLAUDE.md"), "claude")

	files, err := Load(dir)
	if err != nil {
		t.Fatal(err)
	}
	got := within(files, root)
	if len(got) != 1 || filepath.Base(got[0].Path) != "AGENTS.md" {
		t.Fatalf("AGENTS.md did not win: %v", paths(got))
	}
}

func TestLoadFallsBackToClaude(t *testing.T) {
	root := t.TempDir()
	dir := filepath.Join(root, "project")
	writeFile(t, filepath.Join(dir, "CLAUDE.md"), "claude")

	files, err := Load(dir)
	if err != nil {
		t.Fatal(err)
	}
	got := within(files, root)
	if len(got) != 1 || filepath.Base(got[0].Path) != "CLAUDE.md" || got[0].Content != "claude" {
		t.Fatalf("CLAUDE.md fallback failed: %#v", got)
	}
}

func TestLoadSkipsEmptyFiles(t *testing.T) {
	root := t.TempDir()
	dir := filepath.Join(root, "project")
	writeFile(t, filepath.Join(dir, "AGENTS.md"), "   \n")
	writeFile(t, filepath.Join(dir, "CLAUDE.md"), "claude")

	files, err := Load(dir)
	if err != nil {
		t.Fatal(err)
	}
	got := within(files, root)
	if len(got) != 1 || filepath.Base(got[0].Path) != "CLAUDE.md" {
		t.Fatalf("empty AGENTS.md should not shadow CLAUDE.md: %v", paths(got))
	}
}

func TestLoadIgnoresDirectoriesNamedLikeCandidates(t *testing.T) {
	root := t.TempDir()
	dir := filepath.Join(root, "project")
	if err := os.MkdirAll(filepath.Join(dir, "AGENTS.md"), 0o755); err != nil {
		t.Fatal(err)
	}
	writeFile(t, filepath.Join(dir, "CLAUDE.md"), "claude")

	files, err := Load(dir)
	if err != nil {
		t.Fatal(err)
	}
	got := within(files, root)
	if len(got) != 1 || filepath.Base(got[0].Path) != "CLAUDE.md" {
		t.Fatalf("directory named AGENTS.md should be skipped: %v", paths(got))
	}
}

func TestLoadSkipsHiddenDirectories(t *testing.T) {
	root := t.TempDir()
	hidden := filepath.Join(root, ".hidden", "service")
	writeFile(t, filepath.Join(root, ".hidden", "AGENTS.md"), "hidden")
	writeFile(t, filepath.Join(root, "AGENTS.md"), "root")
	if err := os.MkdirAll(hidden, 0o755); err != nil {
		t.Fatal(err)
	}

	files, err := Load(hidden)
	if err != nil {
		t.Fatal(err)
	}
	got := within(files, root)
	want := []string{filepath.Join(root, "AGENTS.md")}
	if diff := compare(paths(got), want); diff != "" {
		t.Fatalf("hidden directory contributed context:\n got %v\nwant %v", paths(got), want)
	}
}

func TestLoadStripsBOM(t *testing.T) {
	root := t.TempDir()
	dir := filepath.Join(root, "project")
	writeFile(t, filepath.Join(dir, "AGENTS.md"), "\xef\xbb\xbf# Title")

	files, err := Load(dir)
	if err != nil {
		t.Fatal(err)
	}
	got := within(files, root)
	if len(got) != 1 || got[0].Content != "# Title" {
		t.Fatalf("BOM not stripped: %q", got[0].Content)
	}
}

func TestLoadReturnsNoFilesWhenNoneExist(t *testing.T) {
	root := t.TempDir()
	// A bare temp dir has no candidates of its own; any result must come from
	// ancestors, so assert only that nothing under root was found.
	files, err := Load(filepath.Join(root, "empty"))
	if err != nil {
		t.Fatal(err)
	}
	if got := within(files, root); len(got) != 0 {
		t.Fatalf("unexpected files: %v", paths(got))
	}
}

// compare reports a human-readable difference, or "" when equal.
func compare(got, want []string) string {
	if len(got) != len(want) {
		return "length mismatch"
	}
	for i := range got {
		if got[i] != want[i] {
			return "element mismatch"
		}
	}
	return ""
}
