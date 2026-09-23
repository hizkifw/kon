//go:build !windows

package tools

import (
	"os"
	"os/exec"
	"path/filepath"
	"syscall"
)

// configureProcessGroup runs the command in its own process group, so an
// interrupt reaches the shell and everything it spawned instead of being
// limited to the shell process alone.
func configureProcessGroup(cmd *exec.Cmd) {
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
}

// resolveShell selects the POSIX interpreter shell commands run through.
func resolveShell() shellBackend {
	return resolveShellFor(os.Getenv("SHELL"), exec.LookPath)
}

// resolveShellFor prefers the shell the user actually runs ($SHELL), so command
// syntax matches what they type, and falls back to /bin/sh, which every
// Unix-like system has. A configured shell that cannot be found is ignored
// rather than failing every command.
func resolveShellFor(configured string, lookPath func(string) (string, error)) shellBackend {
	if configured != "" {
		if path, err := lookPath(configured); err == nil {
			return shellBackend{path: path, args: []string{"-c"}, name: filepath.Base(path)}
		}
	}
	return shellBackend{path: "/bin/sh", args: []string{"-c"}, name: "/bin/sh"}
}

// interruptProcess sends SIGINT to the command's process group: the polite
// "please stop" that a terminal Ctrl+C would deliver. A process may handle it
// to clean up and exit on its own terms.
func interruptProcess(cmd *exec.Cmd) error {
	return syscall.Kill(-cmd.Process.Pid, syscall.SIGINT)
}

// killProcess sends SIGKILL to the command's process group. Processes cannot
// opt out of SIGKILL.
func killProcess(cmd *exec.Cmd) error {
	return syscall.Kill(-cmd.Process.Pid, syscall.SIGKILL)
}
