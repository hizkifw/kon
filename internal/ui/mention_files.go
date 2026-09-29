package ui

import (
	"bufio"
	"bytes"
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strings"
	"time"
	"unicode"
	"unicode/utf8"
)

const maxMentionFiles = 50_000

var errMentionLimit = errors.New("file search limit reached")

func listMentionFiles(ctx context.Context, cwd string) ([]string, error) {
	ctx, cancel := context.WithTimeout(ctx, 5*time.Second)
	defer cancel()
	files, err := gitMentionFiles(ctx, cwd)
	if err != nil && ctx.Err() == nil && !errors.Is(err, errMentionLimit) {
		// Git is optional. Plain directories still have file completion, and
		// neither case adds a process or directory walk to startup.
		files, err = walkMentionFiles(ctx, cwd)
	}
	slices.Sort(files)
	return slices.Compact(files), err
}

func gitMentionFiles(ctx context.Context, cwd string) ([]string, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	cmd := exec.CommandContext(ctx, "git", "-C", cwd, "ls-files", "-z", "--cached", "--others", "--exclude-standard", "--", ".")
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		return nil, err
	}
	if err := cmd.Start(); err != nil {
		return nil, err
	}
	scanner := bufio.NewScanner(stdout)
	scanner.Split(splitMentionPath)
	var files []string
	seen := 0
	for scanner.Scan() {
		if err := ctx.Err(); err != nil {
			cancel()
			_ = cmd.Wait()
			return files, err
		}
		seen++
		if seen > maxMentionFiles {
			cancel()
			_ = cmd.Wait()
			return files, errMentionLimit
		}
		name := scanner.Text()
		if !mentionPathOK(name) {
			continue
		}
		// The index can still contain a deleted file or a submodule directory.
		if info, err := os.Stat(filepath.Join(cwd, filepath.FromSlash(name))); err == nil && info.Mode().IsRegular() {
			files = append(files, name)
		}
	}
	if err := scanner.Err(); err != nil {
		cancel()
		_ = cmd.Wait()
		return files, err
	}
	return files, cmd.Wait()
}

func splitMentionPath(data []byte, atEOF bool) (int, []byte, error) {
	if at := bytes.IndexByte(data, 0); at >= 0 {
		return at + 1, data[:at], nil
	}
	if atEOF && len(data) > 0 {
		return len(data), data, nil
	}
	return 0, nil, nil
}

func walkMentionFiles(ctx context.Context, cwd string) ([]string, error) {
	var files []string
	seen := 0
	err := filepath.WalkDir(cwd, func(name string, entry fs.DirEntry, err error) error {
		if ctx.Err() != nil {
			return ctx.Err()
		}
		if err != nil {
			if name == cwd {
				return err
			}
			return nil
		}
		seen++
		if seen > maxMentionFiles {
			return errMentionLimit
		}
		if entry.IsDir() {
			if name != cwd {
				switch entry.Name() {
				case ".git", ".hg", ".svn", "node_modules", ".venv", "venv", "__pycache__":
					return filepath.SkipDir
				}
			}
			return nil
		}
		// WalkDir does not follow directory symlinks, keeping discovery inside
		// the project even when a link points at a parent or a large tree.
		if !entry.Type().IsRegular() {
			return nil
		}
		rel, err := filepath.Rel(cwd, name)
		if err != nil {
			return err
		}
		rel = filepath.ToSlash(rel)
		if mentionPathOK(rel) {
			files = append(files, rel)
		}
		return nil
	})
	if err != nil {
		return files, fmt.Errorf("find files: %w", err)
	}
	return files, nil
}

// Terminal controls cannot be represented faithfully in the prompt editor.
func mentionPathOK(name string) bool {
	return name != "" && utf8.ValidString(name) && !strings.ContainsFunc(name, unicode.IsControl)
}
