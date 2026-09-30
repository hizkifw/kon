//go:build !windows

package tools

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// fakeKon writes an executable script standing in for a kon binary.
func fakeKon(t *testing.T, dir, says string) string {
	t.Helper()
	path := filepath.Join(dir, "kon")
	if err := os.WriteFile(path, []byte("#!/bin/sh\necho "+says+"\n"), 0o755); err != nil {
		t.Fatal(err)
	}
	return path
}

func TestKonDirHoldsOnlyALinkToTheExecutable(t *testing.T) {
	data := t.TempDir()
	executable := fakeKon(t, t.TempDir(), "this")
	dir, err := KonDir(data, executable)
	if err != nil {
		t.Fatal(err)
	}
	entries, err := os.ReadDir(dir)
	if err != nil || len(entries) != 1 || entries[0].Name() != "kon" {
		t.Fatalf("entries = %v, %v", entries, err)
	}
	got, _ := os.Stat(filepath.Join(dir, "kon"))
	want, _ := os.Stat(executable)
	if !os.SameFile(got, want) {
		t.Fatal("link does not reach the executable")
	}
	if again, err := KonDir(data, executable); err != nil || again != dir {
		t.Fatalf("second call = %q, %v; want %q", again, err, dir)
	}
	other, err := KonDir(data, fakeKon(t, t.TempDir(), "other"))
	if err != nil || other == dir {
		t.Fatalf("another executable shared the directory: %q, %v", other, err)
	}
}

func TestKonDirRenewsAStaleLink(t *testing.T) {
	data := t.TempDir()
	executable := fakeKon(t, t.TempDir(), "this")
	dir, err := KonDir(data, executable)
	if err != nil {
		t.Fatal(err)
	}
	link := filepath.Join(dir, "kon")
	if err := os.Remove(link); err != nil {
		t.Fatal(err)
	}
	if err := os.Symlink(fakeKon(t, t.TempDir(), "stale"), link); err != nil {
		t.Fatal(err)
	}
	if _, err := KonDir(data, executable); err != nil {
		t.Fatal(err)
	}
	got, _ := os.Stat(link)
	want, _ := os.Stat(executable)
	if !os.SameFile(got, want) {
		t.Fatal("stale link was kept")
	}
}

func TestJobsEnvPutsThisKonFirstOnPath(t *testing.T) {
	// Another kon already on PATH must not be the one that runs.
	t.Setenv("PATH", filepath.Dir(fakeKon(t, t.TempDir(), "other"))+string(os.PathListSeparator)+os.Getenv("PATH"))
	dir, err := KonDir(t.TempDir(), fakeKon(t, t.TempDir(), "this"))
	if err != nil {
		t.Fatal(err)
	}
	jobs := NewJobs(t.TempDir(), "ses_x", false, dir, nil)
	defer jobs.Close()
	cmd := exec.Command("/bin/sh", "-c", "kon")
	cmd.Env = append(os.Environ(), jobs.Env()...)
	out, err := cmd.Output()
	if err != nil || strings.TrimSpace(string(out)) != "this" {
		t.Fatalf("kon ran %q, %v", out, err)
	}
}
