//go:build !windows

package codetools

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

// fakeKon writes an executable script standing in for a kon binary. It says
// its words, or with the argument wait, says it is running and waits for its
// standard input to close.
func fakeKon(t *testing.T, dir, says string) string {
	t.Helper()
	path := filepath.Join(dir, "kon")
	script := "#!/bin/sh\n[ \"$1\" = wait ] && echo running && exec cat >/dev/null\necho " + says + "\n"
	if err := os.WriteFile(path, []byte(script), 0o755); err != nil {
		t.Fatal(err)
	}
	return path
}

// runKon prepares command to run through the shell with env added.
func runKon(t *testing.T, command string, env ...string) *exec.Cmd {
	t.Helper()
	cmd := exec.Command("/bin/sh", "-c", command)
	cmd.Env = append(os.Environ(), env...)
	return cmd
}
