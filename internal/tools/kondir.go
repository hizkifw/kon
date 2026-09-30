package tools

import (
	"crypto/sha256"
	"encoding/hex"
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

func konName() string {
	if runtime.GOOS == "windows" {
		return "kon.exe"
	}
	return "kon"
}
