package productdocs

import (
	"bytes"
	"crypto/sha256"
	"embed"
	"encoding/hex"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"sync"
)

//go:embed *.md
var bundled embed.FS

var extractMu sync.Mutex

// Extract returns a local directory containing exactly the bundled product docs.
// The content address changes when pages are added, renamed, removed, or edited.
func Extract(dataDir string) (string, error) {
	extractMu.Lock()
	defer extractMu.Unlock()
	return extract(bundled, dataDir)
}

// Prune removes every extracted version of the docs but this binary's. A kon
// that still bundles an older version extracts it again when kon docs next
// runs there.
func Prune(dataDir string) error {
	extractMu.Lock()
	defer extractMu.Unlock()
	return prune(bundled, dataDir)
}

func prune(source fs.FS, dataDir string) error {
	_, version, err := bundle(source)
	if err != nil {
		return err
	}
	root := filepath.Join(dataDir, "docs")
	entries, err := os.ReadDir(root)
	if errors.Is(err, fs.ErrNotExist) {
		return nil
	}
	if err != nil {
		return fmt.Errorf("list extracted docs: %w", err)
	}
	var errs []error
	for _, entry := range entries {
		// A dot directory is an extraction another kon may be staging now.
		if entry.Name() == version || strings.HasPrefix(entry.Name(), ".") {
			continue
		}
		errs = append(errs, os.RemoveAll(filepath.Join(root, entry.Name())))
	}
	return errors.Join(errs...)
}

// bundle reads the bundled pages and names their version: a hash of every
// page's name and content.
func bundle(source fs.FS) (map[string][]byte, string, error) {
	entries, err := fs.ReadDir(source, ".")
	if err != nil {
		return nil, "", fmt.Errorf("read bundled docs: %w", err)
	}
	files := make(map[string][]byte, len(entries))
	hash := sha256.New()
	for _, entry := range entries {
		if entry.IsDir() {
			return nil, "", fmt.Errorf("bundled docs contain unexpected directory %q", entry.Name())
		}
		content, err := fs.ReadFile(source, entry.Name())
		if err != nil {
			return nil, "", fmt.Errorf("read bundled doc %q: %w", entry.Name(), err)
		}
		files[entry.Name()] = content
		fmt.Fprintf(hash, "%d:%s:%d:", len(entry.Name()), entry.Name(), len(content))
		hash.Write(content)
	}
	return files, hex.EncodeToString(hash.Sum(nil))[:16], nil
}

func extract(source fs.FS, dataDir string) (string, error) {
	files, version, err := bundle(source)
	if err != nil {
		return "", err
	}
	root := filepath.Join(dataDir, "docs")
	target := filepath.Join(root, version)
	if exact, err := matches(target, files); err != nil {
		return "", err
	} else if exact {
		return target, nil
	}
	if err := os.MkdirAll(root, 0700); err != nil {
		return "", fmt.Errorf("create docs directory: %w", err)
	}
	staging, err := os.MkdirTemp(root, ".extract-")
	if err != nil {
		return "", fmt.Errorf("stage bundled docs: %w", err)
	}
	defer os.RemoveAll(staging)
	for name, content := range files {
		if err := os.WriteFile(filepath.Join(staging, name), content, 0600); err != nil {
			return "", fmt.Errorf("write bundled doc %q: %w", name, err)
		}
	}
	// Only a generated, content-addressed directory is replaced. This removes
	// stale or locally modified pages without touching other extracted versions.
	if err := os.RemoveAll(target); err != nil {
		return "", fmt.Errorf("replace extracted docs: %w", err)
	}
	if err := os.Rename(staging, target); err != nil {
		if exact, checkErr := matches(target, files); checkErr == nil && exact {
			return target, nil
		}
		return "", fmt.Errorf("publish bundled docs: %w", err)
	}
	return target, nil
}

func matches(dir string, files map[string][]byte) (bool, error) {
	entries, err := os.ReadDir(dir)
	if os.IsNotExist(err) {
		return false, nil
	}
	if err != nil {
		return false, fmt.Errorf("inspect extracted docs: %w", err)
	}
	if len(entries) != len(files) {
		return false, nil
	}
	for _, entry := range entries {
		expected, ok := files[entry.Name()]
		if !ok || !entry.Type().IsRegular() {
			return false, nil
		}
		actual, err := os.ReadFile(filepath.Join(dir, entry.Name()))
		if err != nil {
			return false, fmt.Errorf("inspect extracted doc %q: %w", entry.Name(), err)
		}
		if !bytes.Equal(actual, expected) {
			return false, nil
		}
	}
	return true, nil
}
