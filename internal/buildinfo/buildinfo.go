// Package buildinfo reports the identity of this kon build. It is a leaf
// package so any network client can name kon without the version being
// threaded through its constructors.
package buildinfo

import "runtime"

// version is set at build time with
// -ldflags "-X github.com/hizkifw/kon/internal/buildinfo.version=...".
var version = "dev"

const projectURL = "https://github.com/hizkifw/kon"

func Version() string { return version }

// UserAgent identifies kon to every server it contacts.
func UserAgent() string {
	return "kon/" + version + " (" + runtime.GOOS + "; " + runtime.GOARCH + "; +" + projectURL + ")"
}
