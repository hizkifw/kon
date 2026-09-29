// Package projectfiles discovers project paths without reading file contents.
package projectfiles

import (
	"bufio"
	"bytes"
	"context"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os/exec"
	"path/filepath"
	"slices"
	"strings"
	"time"
	"unicode"
	"unicode/utf8"
)

const maxFiles = 50_000

var (
	errLimit   = errors.New("file search limit reached")
	errTimeout = errors.New("file search timed out")
)

// List returns sorted paths relative to cwd. A non-nil error may accompany a
// partial list when discovery reaches its time or entry limit.
func List(ctx context.Context, cwd string) ([]string, error) {
	ctx, cancel := context.WithTimeout(ctx, 5*time.Second)
	defer cancel()
	files, err := gitFiles(ctx, cwd)
	if err != nil && ctx.Err() == nil && !errors.Is(err, errLimit) {
		// Git is optional. Plain directories still have file completion, and
		// neither case adds a process or directory walk to startup.
		files, err = walkFiles(ctx, cwd)
	}
	// A killed Git process may report its exit status instead of the deadline.
	// Use the same discovery error regardless of which operation timed out.
	if ctx.Err() != nil {
		err = ctx.Err()
		if errors.Is(err, context.DeadlineExceeded) {
			err = errTimeout
		}
	}
	slices.Sort(files)
	return slices.Compact(files), err
}

func gitFiles(ctx context.Context, cwd string) ([]string, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	// Completion is implicit, so a repository's fsmonitor hook must not run.
	cmd := exec.CommandContext(ctx, "git", "-c", "core.fsmonitor=false", "-C", cwd, "ls-files", "-z", "--cached", "--others", "--exclude-standard", "--", ".")
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		return nil, err
	}
	if err := cmd.Start(); err != nil {
		return nil, err
	}
	files, readErr := readPaths(ctx, stdout)
	if readErr != nil {
		cancel()
	}
	err = cmd.Wait()
	if readErr != nil {
		return files, readErr
	}
	return files, err
}

func readPaths(ctx context.Context, r io.Reader) ([]string, error) {
	scanner := bufio.NewScanner(r)
	scanner.Split(splitPath)
	var files []string
	seen := 0
	for scanner.Scan() {
		if err := ctx.Err(); err != nil {
			return files, err
		}
		seen++
		if seen > maxFiles {
			return files, errLimit
		}
		name := scanner.Text()
		// Git reports untracked nested repositories as directory entries.
		if strings.HasSuffix(name, "/") || !pathOK(name) {
			continue
		}
		// Trust Git's index instead of statting every path. Deleted tracked
		// files and submodules can remain useful references for the agent.
		files = append(files, name)
	}
	return files, scanner.Err()
}

func splitPath(data []byte, atEOF bool) (int, []byte, error) {
	if at := bytes.IndexByte(data, 0); at >= 0 {
		return at + 1, data[:at], nil
	}
	if atEOF && len(data) > 0 {
		return len(data), data, nil
	}
	return 0, nil, nil
}

func walkFiles(ctx context.Context, cwd string) ([]string, error) {
	// WalkDir does not descend into a symlinked root, and the working
	// directory keeps whatever links the user cd'd through.
	cwd, err := filepath.EvalSymlinks(cwd)
	if err != nil {
		return nil, fmt.Errorf("find files: %w", err)
	}
	var files []string
	seen := 0
	err = filepath.WalkDir(cwd, func(name string, entry fs.DirEntry, err error) error {
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
		if seen > maxFiles {
			return errLimit
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
		if pathOK(rel) {
			files = append(files, rel)
		}
		return nil
	})
	if err != nil {
		return files, fmt.Errorf("find files: %w", err)
	}
	return files, nil
}

// Completion consumers cannot faithfully represent control characters in a path.
func pathOK(name string) bool {
	return name != "" && utf8.ValidString(name) && !strings.ContainsFunc(name, unicode.IsControl)
}
