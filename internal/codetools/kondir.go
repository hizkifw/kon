package codetools

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
	want, err := os.Stat(executable)
	if err != nil {
		return "", err
	}
	root := filepath.Join(dataDir, "bin")
	// A kon started through the link may report the link's own path, and a
	// hard link has no target to resolve it to. Hashing that path would add a
	// directory for every level of nested kon run, so the link is used as is.
	if dir := filepath.Dir(executable); sameDir(filepath.Dir(dir), root) && holds(dir, want) {
		return dir, nil
	}
	sum := sha256.Sum256([]byte(executable))
	dir := filepath.Join(root, hex.EncodeToString(sum[:8]))
	if holds(dir, want) {
		return dir, nil
	}
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return "", err
	}
	// The link is made aside and renamed into place, so another kon starting
	// at the same moment never finds it missing or half made.
	link := filepath.Join(dir, konName())
	tmp := link + "." + strconv.Itoa(os.Getpid())
	_ = os.Remove(tmp)
	// A symbolic link keeps following an executable that kon upgrade replaces.
	// Windows refuses one without developer mode; a hard link works there, and
	// the holds check above renews it after an upgrade.
	if err := os.Symlink(executable, tmp); err != nil {
		if err := os.Link(executable, tmp); err != nil {
			return "", err
		}
	}
	err = os.Rename(tmp, link)
	// Windows refuses to replace a link that a kon still runs from, such as a
	// subagent started before kon upgrade, but allows moving it aside.
	// PruneKonDirs removes it once that kon has exited.
	if err != nil && os.Rename(link, staleKon(dir)) == nil {
		err = os.Rename(tmp, link)
	}
	if err != nil {
		_ = os.Remove(tmp)
		return "", err
	}
	return dir, nil
}

// PruneKonDirs removes the KonDir directories whose executable is gone, such
// as the one each `go run` build leaves, and the links KonDir moved aside. A
// hard link, used where symbolic links are refused, never dangles, so its
// directory is kept.
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
			continue
		}
		// A kon may still run from the moved-aside link, which Windows refuses
		// to remove; a later prune gets it.
		_ = os.Remove(staleKon(dir))
	}
	return errors.Join(errs...)
}

// holds reports whether dir's kon is the file want.
func holds(dir string, want fs.FileInfo) bool {
	got, err := os.Stat(filepath.Join(dir, konName()))
	return err == nil && os.SameFile(got, want)
}

// sameDir compares by file identity rather than by path, which differs in
// case or separators on Windows for the same directory.
func sameDir(a, b string) bool {
	ai, err := os.Stat(a)
	if err != nil {
		return false
	}
	bi, err := os.Stat(b)
	return err == nil && os.SameFile(ai, bi)
}

// staleKon is where KonDir moves a link aside when it cannot replace it.
func staleKon(dir string) string {
	return filepath.Join(dir, konName()+".old")
}

func konName() string {
	if runtime.GOOS == "windows" {
		return "kon.exe"
	}
	return "kon"
}
