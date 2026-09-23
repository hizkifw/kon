package buildinfo

import (
	"runtime"
	"testing"
)

func TestUserAgent(t *testing.T) {
	want := "kon/dev (" + runtime.GOOS + "; " + runtime.GOARCH + "; +https://github.com/hizkifw/kon)"
	if got := UserAgent(); got != want {
		t.Fatalf("UserAgent() = %q, want %q", got, want)
	}
}
