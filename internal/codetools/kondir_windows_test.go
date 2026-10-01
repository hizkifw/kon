//go:build windows

package codetools

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

// fakeKonEnv makes the test binary act as the stand-in kon that fakeKon makes.
const fakeKonEnv = "KON_TEST_FAKE_KON"

// saysMarker precedes the words a stand-in kon says, appended past the end of
// its image, where the loader ignores them.
const saysMarker = "\nkon-test-says:"

func TestMain(m *testing.M) {
	if os.Getenv(fakeKonEnv) != "" {
		os.Exit(runFakeKon())
	}
	os.Exit(m.Run())
}

// runFakeKon says the words appended to its executable, or with the argument
// wait, says it is running and waits for its standard input to close.
func runFakeKon() int {
	if len(os.Args) > 1 && os.Args[1] == "wait" {
		fmt.Println("running")
		_, _ = io.Copy(io.Discard, os.Stdin)
		return 0
	}
	self, err := os.Executable()
	if err != nil {
		return 1
	}
	image, err := os.ReadFile(self)
	if err != nil {
		return 1
	}
	i := bytes.LastIndex(image, []byte(saysMarker))
	if i < 0 {
		return 1
	}
	fmt.Println(string(image[i+len(saysMarker):]))
	return 0
}

// fakeKon copies this test binary to stand in for a kon binary. Windows runs
// only real executables, so a script will not do.
func fakeKon(t *testing.T, dir, says string) string {
	t.Helper()
	self, err := os.Executable()
	if err != nil {
		t.Fatal(err)
	}
	image, err := os.ReadFile(self)
	if err != nil {
		t.Fatal(err)
	}
	path := filepath.Join(dir, "kon.exe")
	if err := os.WriteFile(path, append(image, saysMarker+says...), 0o755); err != nil {
		t.Fatal(err)
	}
	return path
}

// runKon prepares command to run through cmd.exe with env added.
func runKon(t *testing.T, command string, env ...string) *exec.Cmd {
	t.Helper()
	cmd := exec.Command("cmd.exe", "/d", "/c", command)
	cmd.Env = append(os.Environ(), append(env, fakeKonEnv+"=1")...)
	return cmd
}
