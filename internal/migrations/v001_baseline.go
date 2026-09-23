package migrations

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"

	"github.com/hizkifw/kon/internal/config"
	"github.com/hizkifw/kon/internal/session"
)

type baselineV1 struct{}

func (baselineV1) Version() int { return 1 }
func (baselineV1) Name() string { return "check unmarked storage" }

// The first marker release also recorded version 1 for existing v4 sessions.
// An unmarked installation may therefore hold either v0.1.1 or v4 files.
func (baselineV1) Run(ctx context.Context, paths config.Paths) error {
	err := filepath.WalkDir(paths.Sessions, func(path string, entry fs.DirEntry, walkErr error) error {
		if walkErr != nil {
			return walkErr
		}
		if err := ctx.Err(); err != nil {
			return err
		}
		if entry.IsDir() || (!strings.HasSuffix(entry.Name(), ".jsonl") && !strings.HasSuffix(entry.Name(), ".jsonl"+legacyBackupSuffix)) {
			return nil
		}
		version, err := readSessionVersion(path)
		if err != nil {
			return fmt.Errorf("read session header %s: %w", path, err)
		}
		if version != 1 && version != session.SchemaVersion {
			return fmt.Errorf("session %s has unsupported version %d; expected 1 or %d", path, version, session.SchemaVersion)
		}
		return nil
	})
	if errors.Is(err, os.ErrNotExist) {
		return nil
	}
	return err
}
