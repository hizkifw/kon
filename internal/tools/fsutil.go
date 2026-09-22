package tools

import (
	"bytes"
	"encoding/base64"
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

// detectImageMIME identifies the image format of b from its magic bytes. It
// returns "" for anything kon does not support sending.
func detectImageMIME(b []byte) string {
	switch {
	case len(b) >= 8 && bytes.Equal(b[:8], []byte("\x89PNG\r\n\x1a\n")):
		return "image/png"
	case len(b) >= 3 && b[0] == 0xFF && b[1] == 0xD8 && b[2] == 0xFF:
		return "image/jpeg"
	case len(b) >= 6 && (bytes.Equal(b[:6], []byte("GIF87a")) || bytes.Equal(b[:6], []byte("GIF89a"))):
		return "image/gif"
	case len(b) >= 12 && bytes.Equal(b[:4], []byte("RIFF")) && bytes.Equal(b[8:12], []byte("WEBP")):
		return "image/webp"
	default:
		return ""
	}
}

// detectUnsendableImageMIME identifies common image formats kon cannot send
// (BMP, TIFF, ICO, HEIF, AVIF). format is a human-readable name for error
// messages; ok is false for non-images.
func detectUnsendableImageMIME(b []byte) (mime, format string) {
	switch {
	case len(b) >= 2 && b[0] == 'B' && b[1] == 'M':
		return "image/bmp", "BMP"
	case len(b) >= 4 && (bytes.Equal(b[:4], []byte("II*\x00")) || bytes.Equal(b[:4], []byte("MM\x00*"))):
		return "image/tiff", "TIFF"
	case len(b) >= 4 && bytes.Equal(b[:4], []byte("\x00\x00\x01\x00")):
		return "image/x-icon", "ICO"
	case len(b) >= 12 && (bytes.Equal(b[4:8], []byte("ftypheic")) || bytes.Equal(b[4:8], []byte("ftypheif")) ||
		bytes.Equal(b[4:8], []byte("ftypmif1")) || bytes.Equal(b[4:8], []byte("ftyphevc"))):
		return "image/heic", "HEIF"
	case len(b) >= 12 && bytes.Equal(b[4:8], []byte("ftypavif")):
		return "image/avif", "AVIF"
	default:
		return "", ""
	}
}

// EncodeImages converts tool image attachments into base64 data URIs for
// session persistence. Each value is self-describing, so the provider mapping
// and session readers need no side table.
func EncodeImages(images []Image) []string {
	if len(images) == 0 {
		return nil
	}
	uris := make([]string, 0, len(images))
	for _, image := range images {
		uris = append(uris, fmt.Sprintf("data:%s;base64,%s", image.MIME, base64.StdEncoding.EncodeToString(image.Data)))
	}
	return uris
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
