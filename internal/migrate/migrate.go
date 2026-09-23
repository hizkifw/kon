// Package migrate coordinates storage upgrades before kon opens durable state.
package migrate

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"time"

	"github.com/gofrs/flock"
	"github.com/hizkifw/kon/internal/config"
)

// Step may change any durable kon file. It must be safe to retry after an
// interrupted run; the version marker advances only after Run succeeds.
type Step interface {
	// Version is the storage version after this step succeeds.
	Version() int
	Name() string
	Run(context.Context, config.Paths) error
}

// Guard holds an instance lease until Close. All processes that access kon's
// durable state must hold one, including short-lived commands.
type Guard struct {
	gate   *flock.Flock
	active *flock.Flock
}

// Enter waits for any migration, checks the storage version, and returns a
// lifetime lease. The current-version path touches only two locks and a small
// marker file; it never scans sessions.
func Enter(ctx context.Context, paths config.Paths, registry []Step, report func(string)) (*Guard, error) {
	if err := validateRegistry(registry); err != nil {
		return nil, err
	}
	if err := os.MkdirAll(paths.DataDir, 0o700); err != nil {
		return nil, fmt.Errorf("create data directory: %w", err)
	}
	g := &Guard{
		gate:   flock.New(filepath.Join(paths.DataDir, "upgrade.gate.lock")),
		active: flock.New(filepath.Join(paths.DataDir, "instances.lock")),
	}
	if err := waitShared(ctx, g.gate, report, "waiting for another kon upgrade"); err != nil {
		return nil, fmt.Errorf("wait for upgrade gate: %w", err)
	}
	if err := waitShared(ctx, g.active, report, "waiting for a kon upgrade to finish"); err != nil {
		_ = g.gate.Unlock()
		return nil, fmt.Errorf("join active instances: %w", err)
	}
	version, err := readVersion(paths)
	if err != nil {
		return nil, errors.Join(err, g.Close(), g.gate.Unlock())
	}
	if version == targetVersion(registry) {
		if err := g.gate.Unlock(); err != nil {
			return nil, errors.Join(err, g.Close())
		}
		return g, nil
	}
	if version > targetVersion(registry) {
		return nil, errors.Join(fmt.Errorf("storage version %d is newer than this kon supports (%d)", version, targetVersion(registry)), g.Close(), g.gate.Unlock())
	}
	if err := errors.Join(g.Close(), g.gate.Unlock()); err != nil {
		return nil, err
	}

	// Recheck under the exclusive gate: another process may have migrated while
	// this one was changing locks. The gate stops new instances from joining.
	if err := waitExclusive(ctx, g.gate, report, "waiting for another kon upgrade"); err != nil {
		return nil, fmt.Errorf("wait for upgrade gate: %w", err)
	}
	if err := waitExclusive(ctx, g.active, report, "waiting for other kon instances to exit before migrating storage"); err != nil {
		return nil, errors.Join(fmt.Errorf("wait for other kon instances: %w", err), g.gate.Unlock())
	}
	version, err = readVersion(paths)
	if err == nil {
		err = run(ctx, paths, version, registry, report)
	}
	if err != nil {
		return nil, errors.Join(err, g.active.Unlock(), g.gate.Unlock())
	}
	if err := g.active.Unlock(); err != nil {
		return nil, errors.Join(err, g.gate.Unlock())
	}
	if err := shared(ctx, g.active); err != nil {
		return nil, errors.Join(err, g.gate.Unlock())
	}
	if err := g.gate.Unlock(); err != nil {
		return nil, errors.Join(err, g.Close())
	}
	return g, nil
}

// Exclusive reserves the upgrade gate, drains other instances, and runs work
// alone. It releases this instance's shared lease before waiting for the
// exclusive lease, then restores it before reopening the gate.
func (g *Guard) Exclusive(ctx context.Context, report func(string), work func(context.Context) error) (err error) {
	if err := waitExclusive(ctx, g.gate, report, "waiting for another kon upgrade"); err != nil {
		return err
	}
	if err := g.active.Unlock(); err != nil {
		return errors.Join(err, g.gate.Unlock())
	}
	if err := waitExclusive(ctx, g.active, report, "waiting for other kon instances to exit before upgrading"); err != nil {
		return errors.Join(err, shared(context.Background(), g.active), g.gate.Unlock())
	}
	defer func() {
		unlockErr := g.active.Unlock()
		var rejoinErr error
		if unlockErr == nil {
			rejoinErr = shared(context.Background(), g.active)
		}
		err = errors.Join(err, unlockErr, rejoinErr, g.gate.Unlock())
	}()
	return work(ctx)
}

func (g *Guard) Close() error { return g.active.Unlock() }

func shared(ctx context.Context, lock *flock.Flock) error {
	_, err := lock.TryRLockContext(ctx, 25*time.Millisecond)
	return err
}

func exclusive(ctx context.Context, lock *flock.Flock) error {
	_, err := lock.TryLockContext(ctx, 25*time.Millisecond)
	return err
}

func waitShared(ctx context.Context, lock *flock.Flock, report func(string), message string) error {
	if ok, err := lock.TryRLock(); ok || err != nil {
		return err
	}
	if report != nil {
		report(message)
	}
	return shared(ctx, lock)
}

func waitExclusive(ctx context.Context, lock *flock.Flock, report func(string), message string) error {
	if ok, err := lock.TryLock(); ok || err != nil {
		return err
	}
	if report != nil {
		report(message)
	}
	return exclusive(ctx, lock)
}

func targetVersion(registry []Step) int { return len(registry) }

func validateRegistry(registry []Step) error {
	if len(registry) == 0 {
		return errors.New("no storage migrations registered")
	}
	for i, step := range registry {
		if step == nil || step.Version() != i+1 || step.Name() == "" {
			return fmt.Errorf("invalid storage migration at position %d", i+1)
		}
	}
	return nil
}

func versionPath(paths config.Paths) string { return filepath.Join(paths.DataDir, "storage-version") }

func readVersion(paths config.Paths) (int, error) {
	b, err := os.ReadFile(versionPath(paths))
	if errors.Is(err, os.ErrNotExist) {
		return 0, nil
	}
	if err != nil {
		return 0, fmt.Errorf("read storage version: %w", err)
	}
	version, _, err := parseVersions(b)
	if err != nil {
		return 0, fmt.Errorf("invalid storage version in %s: %w", versionPath(paths), err)
	}
	return version, nil
}

// Ignore an incomplete final line left by a crash. A later exclusive writer
// truncates that tail before recording the next completed migration.
func parseVersions(b []byte) (version, complete int, err error) {
	for _, line := range strings.SplitAfter(string(b), "\n") {
		if !strings.HasSuffix(line, "\n") {
			break
		}
		value, parseErr := strconv.Atoi(strings.TrimSuffix(line, "\n"))
		if parseErr != nil || value != version+1 {
			return 0, 0, errors.New("non-sequential version record")
		}
		version = value
		complete += len(line)
	}
	return version, complete, nil
}

func run(ctx context.Context, paths config.Paths, version int, registry []Step, report func(string)) error {
	target := targetVersion(registry)
	if version > target {
		return fmt.Errorf("storage version %d is newer than this kon supports (%d)", version, target)
	}
	for version < target {
		if err := ctx.Err(); err != nil {
			return err
		}
		step := registry[version]
		if report != nil {
			report(fmt.Sprintf("migrating storage to version %d: %s", version+1, step.Name()))
		}
		if err := step.Run(ctx, paths); err != nil {
			return fmt.Errorf("migrate storage to version %d: %w", version+1, err)
		}
		if err := writeVersion(paths, version+1); err != nil {
			return err
		}
		version++
	}
	return nil
}

func writeVersion(paths config.Paths, version int) (err error) {
	f, err := os.OpenFile(versionPath(paths), os.O_RDWR|os.O_CREATE, 0o600)
	if err != nil {
		return fmt.Errorf("open storage version: %w", err)
	}
	defer f.Close()
	b, err := io.ReadAll(f)
	if err != nil {
		return fmt.Errorf("read storage version: %w", err)
	}
	previous, complete, err := parseVersions(b)
	if err != nil || previous+1 != version {
		return fmt.Errorf("cannot record storage version %d after %d: %w", version, previous, err)
	}
	if err := f.Truncate(int64(complete)); err != nil {
		return err
	}
	if _, err := f.Seek(int64(complete), io.SeekStart); err != nil {
		return err
	}
	if _, err := fmt.Fprintf(f, "%d\n", version); err != nil {
		return err
	}
	return f.Sync()
}
