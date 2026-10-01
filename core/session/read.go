package session

import (
	"bufio"
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
)

// ErrRemoved reports that a followed session's file no longer exists, as when
// its writer discards a session that never held a conversation.
var ErrRemoved = errors.New("session was removed")

// ErrStop, returned by a ReadLines callback, ends the read early without an
// error.
var ErrStop = errors.New("stop reading")

// errShrank reports a session file shorter than what was already read from
// it. A session file only shrinks when its writer rolls back a record that
// failed to sync, which a reader may already have read, so it is an error
// rather than something to reconcile.
var errShrank = errors.New("session file shrank while it was followed")

// ParseHeader decodes and checks a session file's first line.
func ParseHeader(line []byte) (Header, error) {
	var header Header
	if err := json.Unmarshal(line, &header); err != nil {
		return Header{}, fmt.Errorf("parse session header: %w", err)
	}
	if header.Type != "session" || header.ID.IsZero() {
		return Header{}, errors.New("invalid session header")
	}
	if header.Version != SchemaVersion {
		return Header{}, fmt.Errorf("unsupported session version %d", header.Version)
	}
	return header, nil
}

// ReadLines calls fn with each complete line the session file at path holds
// past offset, and returns the offset just past the last line it consumed. It
// is how a session another process is writing gets followed: a trailing line
// without its newline may be a record still being appended, so it is left
// for a later read. A file that has not grown since offset costs one stat.
//
// fn may return ErrStop to end the read after its line. Any other error ends
// it before that line, so a caller can retry from the returned offset. The
// line is only valid during the call.
func ReadLines(path string, offset int64, fn func(line []byte) error) (int64, error) {
	info, err := os.Stat(path)
	if errors.Is(err, os.ErrNotExist) {
		return offset, ErrRemoved
	}
	if err != nil {
		return offset, fmt.Errorf("read session: %w", err)
	}
	if info.Size() < offset {
		return offset, errShrank
	}
	if info.Size() == offset {
		return offset, nil
	}
	f, err := os.Open(path)
	if errors.Is(err, os.ErrNotExist) {
		return offset, ErrRemoved
	}
	if err != nil {
		return offset, fmt.Errorf("read session: %w", err)
	}
	defer f.Close()
	reader := bufio.NewReaderSize(io.NewSectionReader(f, offset, info.Size()-offset), 64<<10)
	for {
		line, err := reader.ReadSlice('\n')
		if errors.Is(err, bufio.ErrBufferFull) {
			// A record longer than the buffer is gathered whole.
			rest, restErr := reader.ReadBytes('\n')
			line, err = append(bytes.Clone(line), rest...), restErr
		}
		if err != nil {
			// Whatever is left has no newline yet.
			if errors.Is(err, io.EOF) {
				return offset, nil
			}
			return offset, fmt.Errorf("read session: %w", err)
		}
		switch err := fn(bytes.TrimSpace(line)); {
		case errors.Is(err, ErrStop):
			return offset + int64(len(line)), nil
		case err != nil:
			return offset, err
		}
		offset += int64(len(line))
	}
}
