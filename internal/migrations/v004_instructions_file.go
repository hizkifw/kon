package migrations

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/hizkifw/kon/internal/config"
)

// instructionsConfig is a config as kon wrote it before the instructions field
// moved to AGENTS.md in the config directory. Config rejects the field now, so
// a step that reads an older config decodes this instead.
type instructionsConfig struct {
	config.Config
	Instructions *string `json:"instructions,omitempty"`
}

// readInstructionsConfig decodes the config at path, reporting false for a
// missing or unreadable one so the step leaves it for loading to report.
func readInstructionsConfig(path string) (instructionsConfig, bool, error) {
	b, err := os.ReadFile(path)
	if errors.Is(err, os.ErrNotExist) {
		return instructionsConfig{}, false, nil
	}
	if err != nil {
		return instructionsConfig{}, false, fmt.Errorf("read config for migration: %w", err)
	}
	cfg := instructionsConfig{Config: config.Default()}
	dec := json.NewDecoder(bytes.NewReader(b))
	dec.DisallowUnknownFields()
	if dec.Decode(&cfg) != nil {
		return instructionsConfig{}, false, nil
	}
	return cfg, true, nil
}

type instructionsFileV4 struct{}

func (instructionsFileV4) Version() int { return 4 }
func (instructionsFileV4) Name() string { return "move configured instructions to AGENTS.md" }

// Run appends configured instructions to AGENTS.md in the config directory,
// which kon now loads into every session, then drops the field. The file is
// written first and skipped when it already holds the text, so a retry after
// a crash between the two writes does not repeat it.
func (instructionsFileV4) Run(_ context.Context, paths config.Paths) error {
	cfg, ok, err := readInstructionsConfig(paths.ConfigFile)
	if !ok || cfg.Instructions == nil {
		return err
	}
	if text := strings.TrimSpace(*cfg.Instructions); text != "" {
		if err := appendInstructions(filepath.Join(paths.ConfigDir, "AGENTS.md"), text); err != nil {
			return err
		}
	}
	return cfg.Config.Save(paths.ConfigFile)
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
