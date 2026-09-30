package tools

import (
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"runtime"
	"strconv"
)

// KonDir returns a directory under dataDir that holds only a `kon` running
// executable. The shell tool puts it first on PATH, so `kon` in the agent's
// commands is the kon that runs the agent, even when PATH names another or
// none. Prepending the executable's own directory would do the same, but also
// move everything else installed beside it ahead of the user's PATH.
//
// Each executable path gets its own directory, so kons installed in different
// places never replace each other's link.
func KonDir(dataDir, executable string) (string, error) {
	sum := sha256.Sum256([]byte(executable))
	dir := filepath.Join(dataDir, "bin", hex.EncodeToString(sum[:8]))
	link := filepath.Join(dir, konName())
	want, err := os.Stat(executable)
	if err != nil {
		return "", err
	}
	if got, err := os.Stat(link); err == nil && os.SameFile(got, want) {
		return dir, nil
	}
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return "", err
	}
	// The link is made aside and renamed into place, so another kon starting
	// at the same moment never finds it missing or half made.
	tmp := link + "." + strconv.Itoa(os.Getpid())
	_ = os.Remove(tmp)
	// A symbolic link keeps following an executable that kon upgrade replaces.
	// Windows refuses one without developer mode; a hard link works there, and
	// the SameFile check above renews it after an upgrade.
	if err := os.Symlink(executable, tmp); err != nil {
		if err := os.Link(executable, tmp); err != nil {
			return "", err
		}
	}
	if err := os.Rename(tmp, link); err != nil {
		_ = os.Remove(tmp)
		return "", err
	}
	return dir, nil
}

// PruneKonDirs removes the KonDir directories whose executable is gone, such
// as the one each `go run` build leaves. A hard link, used where symbolic
// links are refused, never dangles, so its directory is kept.
func PruneKonDirs(dataDir string) error {
	root := filepath.Join(dataDir, "bin")
	entries, err := os.ReadDir(root)
	if errors.Is(err, fs.ErrNotExist) {
		return nil
	}
	if err != nil {
		return err
	}
	var errs []error
	for _, entry := range entries {
		if !entry.IsDir() {
			continue
		}
		dir := filepath.Join(root, entry.Name())
		if _, err := os.Stat(filepath.Join(dir, konName())); errors.Is(err, fs.ErrNotExist) {
			errs = append(errs, os.RemoveAll(dir))
		}
	}
	return errors.Join(errs...)
}

func konName() string {
	if runtime.GOOS == "windows" {
		return "kon.exe"
	}
	return "kon"
}
