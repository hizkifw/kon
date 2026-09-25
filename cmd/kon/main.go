package main

import (
	"errors"
	"fmt"
	"os"
)

func main() {
	if err := run(os.Args[1:]); err != nil {
		code := 1
		var exit *exitError
		if errors.As(err, &exit) {
			code = exit.code
		}
		if message := err.Error(); message != "" {
			fmt.Fprintln(os.Stderr, "kon:", message)
		}
		os.Exit(code)
	}
}

// exitError carries an exit status other than 1, for commands scripts branch
// on. Its message is the wrapped error's, and empty when the status says it
// all, as for an interrupted run.
type exitError struct {
	code int
	err  error
}

func (e *exitError) Error() string {
	if e.err == nil {
		return ""
	}
	return e.err.Error()
}

func (e *exitError) Unwrap() error { return e.err }

// usageError marks a malformed command line, which exits with status 2.
func usageError(err error) error { return &exitError{code: 2, err: err} }
