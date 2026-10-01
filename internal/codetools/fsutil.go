package codetools

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sync"
)

// atomicWrite writes content to a temporary file in the target directory and
// renames it into place, so a reader never observes a half-written file.
func atomicWrite(path string, content []byte, mode os.FileMode) error {
	tmp, err := os.CreateTemp(filepath.Dir(path), ".kon-*")
	if err != nil {
		return err
	}
	tmpPath := tmp.Name()
	cleanup := func() {
		tmp.Close()
		_ = os.Remove(tmpPath)
	}
	if err := tmp.Chmod(mode); err != nil {
		cleanup()
		return err
	}
	if _, err := tmp.Write(content); err != nil {
		cleanup()
		return err
	}
	if err := tmp.Sync(); err != nil {
		cleanup()
		return err
	}
	if err := tmp.Close(); err != nil {
		_ = os.Remove(tmpPath)
		return err
	}
	if err := os.Rename(tmpPath, path); err != nil {
		_ = os.Remove(tmpPath)
		return err
	}
	return nil
}

// headTailWriter keeps the first and last limit bytes of everything written to
// it, collapsing the middle, so oversized command output stays bounded while
// both the beginning (what happened) and the end (the outcome) survive.
type headTailWriter struct {
	mu    sync.Mutex
	limit int
	total int
	all   bytes.Buffer
	head  []byte
	tail  []byte
}

func (w *headTailWriter) Write(p []byte) (int, error) {
	w.mu.Lock()
	defer w.mu.Unlock()
	n := len(p)
	w.total += n
	if w.all.Len()+n <= w.limit {
		_, _ = w.all.Write(p)
		return n, nil
	}
	half := w.limit / 2
	if w.head == nil {
		combined := append(w.all.Bytes(), p...)
		w.head = append([]byte(nil), combined[:min(half, len(combined))]...)
		w.tail = append([]byte(nil), combined[max(0, len(combined)-half):]...)
		w.all.Reset()
	} else {
		w.tail = append(w.tail, p...)
		if len(w.tail) > half {
			w.tail = append([]byte(nil), w.tail[len(w.tail)-half:]...)
		}
	}
	return n, nil
}

func (w *headTailWriter) String() string {
	w.mu.Lock()
	defer w.mu.Unlock()
	if w.head == nil {
		return w.all.String()
	}
	omitted := w.total - len(w.head) - len(w.tail)
	return string(w.head) + fmt.Sprintf("\n… %d bytes omitted …\n", omitted) + string(w.tail)
}

// Tail returns at most n trailing complete lines of the captured output, for a
// live display snapshot. It never mutates the buffer, so it is safe to call
// from the reporting goroutine while the reader goroutine writes.
func (w *headTailWriter) Tail(n int) []string {
	w.mu.Lock()
	defer w.mu.Unlock()
	text := w.all.String()
	if w.head != nil {
		text = string(w.tail)
	}
	lines := splitDisplayLines(text)
	if len(lines) > 0 && lines[len(lines)-1] == "" {
		lines = lines[:len(lines)-1]
	}
	if len(lines) > n {
		lines = lines[len(lines)-n:]
	}
	return lines
}

var _ io.Writer = (*headTailWriter)(nil)
