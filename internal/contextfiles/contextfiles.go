// Package contextfiles discovers AGENTS.md-style project instruction files.
//
// The AGENTS.md standard places a Markdown file at a repository root and,
// optionally, in nested directories. An agent walks up from the working
// directory, collects every file it finds, and concatenates them so the
// innermost, most specific instructions come last. CLAUDE.md is accepted as a
// compatibility alias, and AGENTS.override.md replaces the plain file in the
// directory that holds it.
package contextfiles

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"strings"
)

// File is one instruction file. Path is absolute, or empty for instructions
// given inline rather than read from a file.
type File struct {
	Path    string
	Content string
}

// candidates lists the filenames checked in each directory, in precedence
// order. The first regular, non-empty file wins, so AGENTS.override.md shadows
// the other names in the same directory.
var candidates = []string{"AGENTS.override.md", "AGENTS.md", "AGENTS.MD", "CLAUDE.md", "CLAUDE.MD"}

// Global returns the user's own instruction file from kon's config directory,
// which applies to every session regardless of where kon runs. The same file
// names are accepted there as in a project directory. An empty configDir has
// no global file rather than meaning the current directory.
func Global(configDir string) (File, bool) {
	if configDir == "" {
		return File{}, false
	}
	return loadFromDir(filepath.Clean(configDir))
}

// Load returns the instruction files that apply to cwd, ordered outermost
// (filesystem root side) to innermost (cwd), which is the order a model should
// read inherited instructions in.
//
// For each directory on the path from cwd to the filesystem root, the first
// matching candidate that is a regular, non-empty file is used. Directories
// whose name begins with "." are skipped, as are files that cannot be read.
// Every returned path is absolute and unique.
func Load(cwd string) ([]File, error) {
	abs, err := filepath.Abs(cwd)
	if err != nil {
		return nil, fmt.Errorf("resolve working directory: %w", err)
	}
	abs = filepath.Clean(abs)

	var files []File
	seen := make(map[string]bool)
	dir := abs
	for {
		if !isHidden(dir) {
			if file, ok := loadFromDir(dir); ok && !seen[file.Path] {
				seen[file.Path] = true
				files = append(files, file)
			}
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	// The walk collects innermost first; reverse so inherited context precedes
	// the more specific files that override it.
	slices.Reverse(files)
	return files, nil
}

// Read reads one instruction file named explicitly rather than discovered.
// Its path is made absolute and symlinks are resolved, as a discovered file's
// directory is, so the same file reached two ways has one path. Unlike
// discovery, an empty file is an error: it was asked for by name.
func Read(path string) (File, error) {
	abs, err := filepath.Abs(path)
	if err != nil {
		return File{}, err
	}
	if canonical, err := filepath.EvalSymlinks(abs); err == nil {
		abs = canonical
	}
	content, err := os.ReadFile(abs)
	if err != nil {
		return File{}, err
	}
	content = stripBOM(content)
	if strings.TrimSpace(string(content)) == "" {
		return File{}, fmt.Errorf("%s is empty", path)
	}
	return File{Path: abs, Content: string(content)}, nil
}

// loadFromDir returns the highest-precedence instruction file in dir. It
// requires a regular file, so a directory named AGENTS.md is ignored rather
// than read, and treats an empty file as absent so it does not shadow a
// non-empty sibling such as CLAUDE.md.
func loadFromDir(dir string) (File, bool) {
	for _, name := range candidates {
		path := filepath.Join(dir, name)
		info, err := os.Stat(path)
		if err != nil || !info.Mode().IsRegular() {
			continue
		}
		content, err := os.ReadFile(path)
		if err != nil {
			continue
		}
		content = stripBOM(content)
		if strings.TrimSpace(string(content)) == "" {
			continue
		}
		return File{Path: path, Content: string(content)}, true
	}
	return File{}, false
}

// isHidden reports whether the last path element starts with a dot. Absolute
// paths never end in "." or "..", and a filesystem root's base does not begin
// with a dot, so no extra guarding is needed.
func isHidden(dir string) bool {
	return strings.HasPrefix(filepath.Base(dir), ".")
}

// stripBOM removes a leading UTF-8 byte order mark. Some editors write one, and
// it would otherwise appear as a stray character at the start of a prompt.
func stripBOM(b []byte) []byte {
	return bytes.TrimPrefix(b, []byte{0xEF, 0xBB, 0xBF})
}
