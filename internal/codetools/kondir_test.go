package codetools

import (
	"bufio"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestKonDirHoldsOnlyALinkToTheExecutable(t *testing.T) {
	data := t.TempDir()
	executable := fakeKon(t, t.TempDir(), "this")
	dir, err := KonDir(data, executable)
	if err != nil {
		t.Fatal(err)
	}
	entries, err := os.ReadDir(dir)
	if err != nil || len(entries) != 1 || entries[0].Name() != konName() {
		t.Fatalf("entries = %v, %v", entries, err)
	}
	got, _ := os.Stat(filepath.Join(dir, konName()))
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

// A kon started through the link may report the link as its executable, as a
// hard link does on Windows. A nested kon run must reuse the directory rather
// than make one more for each level.
func TestKonDirReusesTheLinkItRunsFrom(t *testing.T) {
	data := t.TempDir()
	dir, err := KonDir(data, fakeKon(t, t.TempDir(), "this"))
	if err != nil {
		t.Fatal(err)
	}
	if nested, err := KonDir(data, filepath.Join(dir, konName())); err != nil || nested != dir {
		t.Fatalf("through the link = %q, %v; want %q", nested, err, dir)
	}
	if entries, _ := os.ReadDir(filepath.Join(data, "bin")); len(entries) != 1 {
		t.Fatalf("bin holds %d directories, want 1", len(entries))
	}
}

func TestKonDirRenewsAStaleLink(t *testing.T) {
	data := t.TempDir()
	executable := fakeKon(t, t.TempDir(), "this")
	dir, err := KonDir(data, executable)
	if err != nil {
		t.Fatal(err)
	}
	link := filepath.Join(dir, konName())
	if err := os.Remove(link); err != nil {
		t.Fatal(err)
	}
	if err := os.Link(fakeKon(t, t.TempDir(), "stale"), link); err != nil {
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

// After kon upgrade a subagent may still run from the old link, which Windows
// refuses to replace.
func TestKonDirRenewsALinkThatIsRunning(t *testing.T) {
	data := t.TempDir()
	executable := fakeKon(t, t.TempDir(), "old")
	dir, err := KonDir(data, executable)
	if err != nil {
		t.Fatal(err)
	}
	running := runKon(t, "kon wait", "PATH="+dir+string(os.PathListSeparator)+os.Getenv("PATH"))
	stdin, err := running.StdinPipe()
	if err != nil {
		t.Fatal(err)
	}
	stdout, err := running.StdoutPipe()
	if err != nil {
		t.Fatal(err)
	}
	if err := running.Start(); err != nil {
		t.Fatal(err)
	}
	if line, err := bufio.NewReader(stdout).ReadString('\n'); err != nil || strings.TrimSpace(line) != "running" {
		t.Fatalf("kon wait said %q, %v", line, err)
	}
	// An upgrade moves the running executable aside, as selfupdate does.
	if err := os.Rename(executable, executable+".old"); err != nil {
		t.Fatal(err)
	}
	fakeKon(t, filepath.Dir(executable), "new")
	if again, err := KonDir(data, executable); err != nil || again != dir {
		t.Fatalf("renew = %q, %v; want %q", again, err, dir)
	}
	got, _ := os.Stat(filepath.Join(dir, konName()))
	want, _ := os.Stat(executable)
	if !os.SameFile(got, want) {
		t.Fatal("link still reaches the old executable")
	}
	stdin.Close()
	if err := running.Wait(); err != nil {
		t.Fatal(err)
	}
	if err := PruneKonDirs(data); err != nil {
		t.Fatal(err)
	}
	if _, err := os.Stat(staleKon(dir)); !os.IsNotExist(err) {
		t.Fatalf("moved-aside link kept: %v", err)
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
	out, err := runKon(t, "kon", jobs.Env()...).Output()
	if err != nil || strings.TrimSpace(string(out)) != "this" {
		t.Fatalf("kon ran %q, %v", out, err)
	}
}

func TestPruneKonDirsRemovesOnlyDanglingLinks(t *testing.T) {
	data := t.TempDir()
	kept, err := KonDir(data, fakeKon(t, t.TempDir(), "kept"))
	if err != nil {
		t.Fatal(err)
	}
	goneDir := t.TempDir()
	gone, err := KonDir(data, fakeKon(t, goneDir, "gone"))
	if err != nil {
		t.Fatal(err)
	}
	if info, err := os.Lstat(filepath.Join(gone, konName())); err != nil || info.Mode()&os.ModeSymlink == 0 {
		t.Skip("symbolic links are refused here, and a hard link never dangles")
	}
	if err := os.RemoveAll(goneDir); err != nil {
		t.Fatal(err)
	}
	if err := PruneKonDirs(data); err != nil {
		t.Fatal(err)
	}
	if _, err := os.Stat(kept); err != nil {
		t.Fatalf("live link removed: %v", err)
	}
	if _, err := os.Stat(gone); !os.IsNotExist(err) {
		t.Fatalf("dangling link kept: %v", err)
	}
	if err := PruneKonDirs(t.TempDir()); err != nil {
		t.Fatalf("no bin directory: %v", err)
	}
}
