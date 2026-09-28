package migrate

import (
	"bufio"
	"context"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/gofrs/flock"
	"github.com/hizkifw/kon/internal/config"
)

func testPaths(root string) config.Paths {
	return config.Paths{DataDir: root, Sessions: filepath.Join(root, "sessions")}
}

type testStep struct {
	version int
	name    string
	run     func(context.Context, config.Paths) error
}

func (s testStep) Version() int { return s.version }
func (s testStep) Name() string { return s.name }
func (s testStep) Run(ctx context.Context, paths config.Paths) error {
	if s.run != nil {
		return s.run(ctx, paths)
	}
	return nil
}

func testRegistry() []Step {
	return []Step{testStep{version: 1, name: "baseline"}, testStep{version: 2, name: "next"}}
}

func TestRegistryRejectsOutOfOrderSteps(t *testing.T) {
	paths := testPaths(filepath.Join(t.TempDir(), "data"))
	_, err := Enter(context.Background(), paths, []Step{testStep{version: 2, name: "second"}}, nil)
	if err == nil {
		t.Fatal("out-of-order registry was accepted")
	}
	if _, statErr := os.Stat(paths.DataDir); !os.IsNotExist(statErr) {
		t.Fatalf("invalid registry touched storage: %v", statErr)
	}
}

func TestMigrationRetriesWithoutAdvancingVersion(t *testing.T) {
	paths := testPaths(t.TempDir())
	var calls int
	registry := []Step{testStep{version: 1, name: "baseline"}, testStep{version: 2, name: "retry", run: func(_ context.Context, _ config.Paths) error {
		calls++
		if calls == 1 {
			return errors.New("interrupted")
		}
		return nil
	}}}
	if err := run(context.Background(), paths, 0, registry, nil); err == nil {
		t.Fatal("first migration succeeded")
	}
	version, err := readVersion(paths)
	if err != nil || version != 1 {
		t.Fatalf("version after failure = %d, %v", version, err)
	}
	if err := run(context.Background(), paths, version, registry, nil); err != nil {
		t.Fatal(err)
	}
	version, err = readVersion(paths)
	if err != nil || version != 2 || calls != 2 {
		t.Fatalf("version, calls after retry = %d, %d (%v)", version, calls, err)
	}
}

// Only migration steps read sessions, so a start at the current version must
// run none of them; otherwise every start would cost a scan of all sessions.
func TestCurrentVersionDoesNotScanSessions(t *testing.T) {
	paths := testPaths(t.TempDir())
	var runs int
	count := func(context.Context, config.Paths) error {
		runs++
		return nil
	}
	registry := []Step{testStep{version: 1, name: "baseline", run: count}, testStep{version: 2, name: "next", run: count}}
	g, err := Enter(context.Background(), paths, registry, nil)
	if err != nil {
		t.Fatal(err)
	}
	if err := g.Close(); err != nil {
		t.Fatal(err)
	}
	if runs != 2 {
		t.Fatalf("first start ran %d steps, want 2", runs)
	}
	g, err = Enter(context.Background(), paths, registry, nil)
	if err != nil {
		t.Fatal(err)
	}
	if err := g.Close(); err != nil {
		t.Fatal(err)
	}
	if runs != 2 {
		t.Fatalf("a start at the current version ran %d steps", runs-2)
	}
}

// Storage stamped by a newer kon may hold formats this one cannot read, so it
// must refuse to start rather than use or rewrite that storage.
func TestNewerStorageVersionIsRefused(t *testing.T) {
	paths := testPaths(t.TempDir())
	if err := os.WriteFile(versionPath(paths), []byte("1\n2\n3\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	g, err := Enter(context.Background(), paths, testRegistry(), nil)
	if err == nil {
		g.Close()
		t.Fatal("storage from a newer kon was accepted")
	}
	if !strings.Contains(err.Error(), "newer") {
		t.Fatalf("Enter error = %v, want a newer-version refusal", err)
	}
}

func TestIncompleteVersionRecordIsRetried(t *testing.T) {
	paths := testPaths(t.TempDir())
	if err := os.WriteFile(versionPath(paths), []byte("1\n2"), 0o600); err != nil {
		t.Fatal(err)
	}
	version, err := readVersion(paths)
	if err != nil || version != 1 {
		t.Fatalf("version = %d, %v", version, err)
	}
	if err := writeVersion(paths, 2); err != nil {
		t.Fatal(err)
	}
	b, err := os.ReadFile(versionPath(paths))
	if err != nil || string(b) != "1\n2\n" {
		t.Fatalf("version records = %q, %v", b, err)
	}
}

func TestExclusiveWaitsForOtherInstanceAndClosesGate(t *testing.T) {
	paths := testPaths(t.TempDir())
	g, err := Enter(context.Background(), paths, testRegistry(), nil)
	if err != nil {
		t.Fatal(err)
	}
	defer g.Close()

	cmd := exec.Command(os.Args[0], "-test.run=^TestMigrateHelper$")
	cmd.Env = append(os.Environ(), "KON_MIGRATE_HELPER_ROOT="+paths.DataDir)
	stdin, err := cmd.StdinPipe()
	if err != nil {
		t.Fatal(err)
	}
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		t.Fatal(err)
	}
	if err := cmd.Start(); err != nil {
		t.Fatal(err)
	}
	defer func() {
		_ = stdin.Close()
		_ = cmd.Process.Kill()
		_ = cmd.Wait()
	}()
	ready, err := bufio.NewReader(stdout).ReadString('\n')
	if err != nil || ready != "ready\n" {
		t.Fatalf("helper readiness = %q, %v", ready, err)
	}

	ctx, cancel := context.WithTimeout(context.Background(), 3*time.Second)
	defer cancel()
	done := make(chan error, 1)
	go func() { done <- g.Exclusive(ctx, nil, func(context.Context) error { return nil }) }()
	gate := flock.New(filepath.Join(paths.DataDir, "upgrade.gate.lock"))
	deadline := time.After(time.Second)
	for {
		locked, err := gate.TryRLock()
		if err != nil {
			t.Fatal(err)
		}
		if !locked {
			break
		}
		if err := gate.Unlock(); err != nil {
			t.Fatal(err)
		}
		select {
		case <-deadline:
			t.Fatal("upgrade did not close gate")
		case <-time.After(10 * time.Millisecond):
		}
	}
	select {
	case err := <-done:
		t.Fatalf("upgrade ran while another instance was active: %v", err)
	default:
	}
	blockedCtx, blockedCancel := context.WithTimeout(context.Background(), 50*time.Millisecond)
	defer blockedCancel()
	if _, err := Enter(blockedCtx, paths, testRegistry(), nil); !errors.Is(err, context.DeadlineExceeded) {
		t.Fatalf("new instance entered during upgrade: %v", err)
	}
	if err := stdin.Close(); err != nil {
		t.Fatal(err)
	}
	if err := cmd.Wait(); err != nil {
		t.Fatal(err)
	}
	select {
	case err := <-done:
		if err != nil {
			t.Fatal(err)
		}
	case <-ctx.Done():
		t.Fatal("upgrade did not run after other instance exited")
	}
}

func TestMigrateHelper(t *testing.T) {
	root := os.Getenv("KON_MIGRATE_HELPER_ROOT")
	if root == "" {
		return
	}
	g, err := Enter(context.Background(), testPaths(root), testRegistry(), nil)
	if err != nil {
		t.Fatal(err)
	}
	defer g.Close()
	fmt.Fprintln(os.Stdout, "ready")
	_, _ = os.Stdin.Read(make([]byte, 1))
}
