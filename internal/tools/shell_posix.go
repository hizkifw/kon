//go:build !windows

package tools

import (
	"os/exec"
	"syscall"
)

// configureProcessGroup runs the command in its own process group, so an
// interrupt reaches the shell and everything it spawned instead of being
// limited to the shell process alone.
func configureProcessGroup(cmd *exec.Cmd) {
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
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
