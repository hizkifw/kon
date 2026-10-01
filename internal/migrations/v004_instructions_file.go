package migrations

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"kon.kitsu.red/internal/config"
)

type instructionsFileV4 struct{}

func (instructionsFileV4) Version() int { return 4 }
func (instructionsFileV4) Name() string { return "move configured instructions to AGENTS.md" }

// Run appends configured instructions to AGENTS.md in the config directory,
// which kon now loads into every session, then drops the field. The file is
// written first and skipped when it already holds the text, so a retry after
// a crash between the two writes does not repeat it.
func (instructionsFileV4) Run(_ context.Context, paths config.Paths) error {
	cfg, ok, err := readConfigObject(paths.ConfigFile)
	if !ok {
		return err
	}
	raw, present := cfg["instructions"]
	if !present {
		return nil
	}
	var instructions *string
	if json.Unmarshal(raw, &instructions) != nil {
		return nil
	}
	if instructions != nil {
		if text := strings.TrimSpace(*instructions); text != "" {
			if err := appendInstructions(filepath.Join(paths.ConfigDir, "AGENTS.md"), text); err != nil {
				return err
			}
		}
	}
	delete(cfg, "instructions")
	return config.WriteJSON(paths.ConfigFile, cfg)
}

// appendInstructions adds text to the end of path. Opening with O_APPEND means
// a crash can leave at most a partial tail, never a truncated file, and writes
// through a symlinked AGENTS.md instead of replacing the link.
func appendInstructions(path, text string) error {
	existing, err := os.ReadFile(path)
	if err != nil && !errors.Is(err, os.ErrNotExist) {
		return fmt.Errorf("read %s: %w", path, err)
	}
	if strings.Contains(string(existing), text) {
		return nil
	}
	content := text + "\n"
	switch {
	case len(existing) == 0:
	case strings.HasSuffix(string(existing), "\n\n"):
	case strings.HasSuffix(string(existing), "\n"):
		content = "\n" + content
	default:
		content = "\n\n" + content
	}
	f, err := os.OpenFile(path, os.O_WRONLY|os.O_APPEND|os.O_CREATE, 0o600)
	if err != nil {
		return fmt.Errorf("open %s: %w", path, err)
	}
	if _, err := f.WriteString(content); err != nil {
		f.Close()
		return fmt.Errorf("write %s: %w", path, err)
	}
	if err := f.Close(); err != nil {
		return fmt.Errorf("close %s: %w", path, err)
	}
	return nil
}
