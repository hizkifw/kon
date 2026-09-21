//go:build windows

package tools

import (
	"os/exec"
	"syscall"
)

const ctrlBreakEvent = 1

var generateConsoleCtrlEvent = syscall.NewLazyDLL("kernel32.dll").NewProc("GenerateConsoleCtrlEvent")

// configureProcessGroup runs the command in its own process group, so console
// control events can be directed at the command alone.
func configureProcessGroup(cmd *exec.Cmd) {
	cmd.SysProcAttr = &syscall.SysProcAttr{CreationFlags: syscall.CREATE_NEW_PROCESS_GROUP}
}

// interruptProcess delivers a CTRL_BREAK_EVENT to the command's process
// group: the closest Windows equivalent to a terminal Ctrl+C. Go child
// processes see it as os.Interrupt, and native children run their default
// break handling.
func interruptProcess(cmd *exec.Cmd) error {
	r1, _, callErr := generateConsoleCtrlEvent.Call(ctrlBreakEvent, uintptr(cmd.Process.Pid))
	if r1 == 0 {
		return callErr
	}
	return nil
}

// killProcess terminates the shell process. Windows has no group-wide
// equivalent of SIGKILL, so processes it spawned may outlive it.
func killProcess(cmd *exec.Cmd) error {
	return cmd.Process.Kill()
}
