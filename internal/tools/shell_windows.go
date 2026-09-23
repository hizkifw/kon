//go:build windows

package tools

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"syscall"
)

const ctrlBreakEvent = 1

var generateConsoleCtrlEvent = syscall.NewLazyDLL("kernel32.dll").NewProc("GenerateConsoleCtrlEvent")

// configureProcessGroup runs the command in its own process group, so console
// control events can be directed at the command alone.
func configureProcessGroup(cmd *exec.Cmd) {
	cmd.SysProcAttr = &syscall.SysProcAttr{CreationFlags: syscall.CREATE_NEW_PROCESS_GROUP}
}

// resolveShell selects the interpreter shell commands run through on Windows.
// Git Bash is preferred for its POSIX command syntax, then PowerShell, and
// finally cmd.exe, which is always present. It falls back in that order so a
// command written for one shell still runs on a machine without the others.
func resolveShell() shellBackend {
	if path := findGitBash(); path != "" {
		return shellBackend{path: path, args: []string{"-c"}, name: "Git Bash"}
	}
	for _, name := range []string{"powershell.exe", "powershell"} {
		if path, err := exec.LookPath(name); err == nil {
			return shellBackend{path: path, args: []string{"-NoProfile", "-Command"}, name: "PowerShell"}
		}
	}
	path := os.Getenv("COMSPEC")
	if path == "" {
		path = "cmd.exe"
	}
	return shellBackend{path: path, args: []string{"/d", "/s", "/c"}, name: "cmd.exe"}
}

// findGitBash locates a bash.exe for Git Bash. It checks the default install
// roots before PATH: a Git installation that was not added to PATH is common,
// and PATH's `bash` may instead be the WSL launcher, which is not the intended
// POSIX environment.
func findGitBash() string {
	for _, root := range []string{os.Getenv("ProgramFiles"), os.Getenv("ProgramFiles(x86)"), os.Getenv("LocalAppData")} {
		if root == "" {
			continue
		}
		for _, rel := range []string{`Git\bin\bash.exe`, `Git\usr\bin\bash.exe`} {
			path := filepath.Join(root, rel)
			if info, err := os.Stat(path); err == nil && !info.IsDir() {
				return path
			}
		}
	}
	for _, name := range []string{"bash.exe", "bash"} {
		if path, err := exec.LookPath(name); err == nil && !isWSLBash(path) {
			return path
		}
	}
	return ""
}

// isWSLBash reports whether path is Windows' WSL launcher, which runs a command
// inside a Linux filesystem rather than the Windows workspace. It is skipped so
// a machine with WSL but no Git Bash falls through to PowerShell or cmd.exe.
func isWSLBash(path string) bool {
	systemRoot := os.Getenv("SystemRoot")
	if systemRoot == "" {
		systemRoot = `C:\Windows`
	}
	return strings.EqualFold(filepath.Dir(path), filepath.Join(systemRoot, "System32"))
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
