package projectfiles

import (
	"context"
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
	"time"
)

func writeFile(t *testing.T, root, name string) {
	t.Helper()
	name = filepath.Join(root, filepath.FromSlash(name))
	if err := os.MkdirAll(filepath.Dir(name), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(name, []byte("test\n"), 0o600); err != nil {
		t.Fatal(err)
	}
}

func TestListRespectsGitIgnoresAndScope(t *testing.T) {
	if _, err := exec.LookPath("git"); err != nil {
		t.Skip("git is not installed")
	}
	root := t.TempDir()
	git := func(args ...string) {
		t.Helper()
		cmd := exec.Command("git", append([]string{"-C", root}, args...)...)
		if out, err := cmd.CombinedOutput(); err != nil {
			t.Fatalf("git %v: %s: %v", args, out, err)
		}
	}
	git("init", "--quiet")
	for _, name := range []string{"tracked.go", "deleted.go", "untracked.go", "docs/space name.md", "docs/你好.md", "node_modules/ignored.js", "build.log"} {
		writeFile(t, root, name)
	}
	if err := os.WriteFile(filepath.Join(root, ".gitignore"), []byte("node_modules/\n*.log\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	git("add", "tracked.go", "deleted.go", "docs")
	if err := os.Remove(filepath.Join(root, "deleted.go")); err != nil {
		t.Fatal(err)
	}
	files, err := List(context.Background(), root)
	want := []string{".gitignore", "deleted.go", "docs/space name.md", "docs/你好.md", "tracked.go", "untracked.go"}
	if err != nil || !reflect.DeepEqual(files, want) {
		t.Fatalf("Git files = %v, %v; want %v", files, err, want)
	}
	files, err = List(context.Background(), filepath.Join(root, "docs"))
	if err != nil || !reflect.DeepEqual(files, []string{"space name.md", "你好.md"}) {
		t.Fatalf("subdirectory files = %v, %v", files, err)
	}
}

func TestListWithoutGit(t *testing.T) {
	root := t.TempDir()
	for _, name := range []string{"main.go", "docs/my file.md", ".github/workflow.yml", ".git/objects/hidden", "node_modules/hidden", ".venv/hidden"} {
		writeFile(t, root, name)
	}
	t.Setenv("PATH", t.TempDir())
	files, err := List(context.Background(), root)
	want := []string{".github/workflow.yml", "docs/my file.md", "main.go"}
	if err != nil || !reflect.DeepEqual(files, want) {
		t.Fatalf("plain files = %v, %v; want %v", files, err, want)
	}
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	if _, err := walkFiles(ctx, root); err == nil {
		t.Fatal("cancelled discovery succeeded")
	}
}

func TestListDisablesRepositoryFSMonitor(t *testing.T) {
	if _, err := exec.LookPath("git"); err != nil {
		t.Skip("git is not installed")
	}
	root := t.TempDir()
	git := func(args ...string) {
		t.Helper()
		cmd := exec.Command("git", append([]string{"-C", root}, args...)...)
		if out, err := cmd.CombinedOutput(); err != nil {
			t.Fatalf("git %v: %s: %v", args, out, err)
		}
	}
	git("init", "--quiet")
	writeFile(t, root, "main.go")
	git("add", "main.go")
	hook := filepath.Join(root, ".git", "fsmonitor-test")
	if err := os.WriteFile(hook, []byte("#!/bin/sh\n: > fsmonitor-ran\nprintf 'token\\0'\n"), 0o755); err != nil {
		t.Fatal(err)
	}
	git("config", "core.fsmonitor", "./.git/fsmonitor-test")
	// Prove the fixture can run before checking that completion disables it.
	git("ls-files", "--others")
	marker := filepath.Join(root, "fsmonitor-ran")
	if err := os.Remove(marker); err != nil {
		t.Fatalf("fsmonitor fixture did not run: %v", err)
	}
	files, err := List(context.Background(), root)
	if err != nil || !reflect.DeepEqual(files, []string{"main.go"}) {
		t.Fatalf("files = %v, %v", files, err)
	}
	if _, err := os.Stat(marker); !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("completion ran fsmonitor: %v", err)
	}
}

func TestReadPathsReturnsPartialListAtLimit(t *testing.T) {
	input := strings.Repeat("src/main.go\x00", maxFiles+1)
	files, err := readPaths(context.Background(), strings.NewReader(input))
	if len(files) != maxFiles || !errors.Is(err, errLimit) {
		t.Fatalf("limited list = %d files, %v", len(files), err)
	}
	files, err = readPaths(context.Background(), strings.NewReader(input[:len(input)-len("src/main.go\x00")]))
	if len(files) != maxFiles || err != nil {
		t.Fatalf("exact limit = %d files, %v", len(files), err)
	}
}

func TestReadPathsPreservesNamesAndSkipsControls(t *testing.T) {
	input := "docs/space name.md\x00src/你好.go\x00nested/\x00bad\nname\x00bad\x1bname\x00bad\xffname\x00"
	files, err := readPaths(context.Background(), strings.NewReader(input))
	if err != nil || !reflect.DeepEqual(files, []string{"docs/space name.md", "src/你好.go"}) {
		t.Fatalf("paths = %v, %v", files, err)
	}
}

func TestListExplainsTimeoutAndPreservesCancellation(t *testing.T) {
	ctx, cancel := context.WithDeadline(context.Background(), time.Now().Add(-time.Second))
	defer cancel()
	if _, err := List(ctx, t.TempDir()); !errors.Is(err, errTimeout) {
		t.Fatalf("expired discovery = %v, want %v", err, errTimeout)
	}
	ctx, cancel = context.WithCancel(context.Background())
	cancel()
	if _, err := List(ctx, t.TempDir()); !errors.Is(err, context.Canceled) {
		t.Fatalf("cancelled discovery = %v", err)
	}
}

func TestListSkipsUntrackedNestedRepository(t *testing.T) {
	if _, err := exec.LookPath("git"); err != nil {
		t.Skip("git is not installed")
	}
	root := t.TempDir()
	writeFile(t, root, "main.go")
	writeFile(t, root, "nested/inner.go")
	for _, dir := range []string{root, filepath.Join(root, "nested")} {
		if out, err := exec.Command("git", "-C", dir, "init", "--quiet").CombinedOutput(); err != nil {
			t.Fatalf("git init: %s: %v", out, err)
		}
	}
	files, err := List(context.Background(), root)
	if err != nil || !reflect.DeepEqual(files, []string{"main.go"}) {
		t.Fatalf("nested repository files = %v, %v", files, err)
	}
}
