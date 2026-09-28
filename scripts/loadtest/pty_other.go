//go:build unix && !linux

package main

import (
	"errors"
	"os"
)

func openPTY(cols, rows uint16) (master, slave *os.File, err error) {
	return nil, nil, errors.New("first-paint scenarios need Linux")
}
