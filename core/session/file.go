package session

import (
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"

	"github.com/gofrs/flock"
)

// Create starts a session file at path, which must not exist, holding header
// and the root system message. The header's type and version are the
// format's own; a zero ID or timestamp is filled in. The store is the file's only writer
// until Close.
func Create(path string, header Header, systemPrompt string) (*Store, error) {
	// Lock before the file exists, so no other process can find the session
	// unlocked and open it as a second writer.
	l, err := lock(path)
	if err != nil {
		return nil, err
	}
	f, err := os.OpenFile(path, os.O_RDWR|os.O_CREATE|os.O_EXCL, 0o600)
	if err != nil {
		_ = l.Unlock()
		return nil, fmt.Errorf("create session: %w", err)
	}
	b := &fileBackend{path: path, file: f, lock: l}
	s, err := start(path, header, b, systemPrompt)
	if err != nil {
		return nil, errors.Join(err, b.close(true))
	}
	return s, nil
}

// Open opens a persisted session as its only writer. It returns ErrInUse while
// another process has the session open.
func Open(path string) (_ *Store, err error) {
	// The lock comes before parsing: an incomplete tail is only safe to trim
	// once no other writer can be partway through appending it.
	l, err := lock(path)
	if err != nil {
		return nil, err
	}
	defer func() {
		if err != nil {
			_ = l.Unlock()
		}
	}()
	parsed, err := parseSession(path)
	if err != nil {
		return nil, err
	}
	if parsed.repairOffset >= 0 {
		if err := os.Truncate(path, parsed.repairOffset); err != nil {
			return nil, fmt.Errorf("repair incomplete session tail: %w", err)
		}
	}
	f, err := os.OpenFile(path, os.O_RDWR|os.O_APPEND, 0o600)
	if err != nil {
		return nil, fmt.Errorf("open session for append: %w", err)
	}
	s := &Store{
		header:  parsed.header,
		path:    path,
		backend: &fileBackend{path: path, file: f, lock: l},
		entries: parsed.entries,
		byID:    parsed.byID,
		leafID:  parsed.leafID(),
		// A session holding only its root system message and structural
		// entries has no conversation to keep.
		empty: true,
	}
	for _, entry := range parsed.entries {
		if entry.Type == EntryTypeCompaction || (entry.Message != nil && entry.Message.Role != RoleSystem) {
			s.empty = false
			break
		}
	}
	return s, nil
}

// fileBackend appends records to a JSONL file and keeps media beside it, one
// file per hash.
type fileBackend struct {
	path string
	file sessionFile
	lock *flock.Flock
	// broken is set when a failed append could not be rolled back. The file
	// then ends in a torn line, and appending after it would bury that line
	// mid-file where Open refuses it, so every later append fails instead.
	broken error
}

// sessionFile is the part of *os.File the backend writes through, so tests
// can simulate a write that fails partway.
type sessionFile interface {
	Write([]byte) (int, error)
	Seek(offset int64, whence int) (int64, error)
	Truncate(size int64) error
	Sync() error
	Close() error
}

// write appends one record as a line. A failed write or sync truncates the
// file back to where the record began, so a partial line from a full disk is
// never followed by the next record.
func (b *fileBackend) write(record []byte, sync bool) error {
	if b.broken != nil {
		return b.broken
	}
	offset, err := b.file.Seek(0, io.SeekEnd)
	if err != nil {
		return fmt.Errorf("find session end: %w", err)
	}
	if _, err := b.file.Write(append(record, '\n')); err != nil {
		return b.rollback(offset, fmt.Errorf("append session entry: %w", err))
	}
	if !sync {
		return nil
	}
	if err := b.file.Sync(); err != nil {
		return b.rollback(offset, fmt.Errorf("sync session entry: %w", err))
	}
	return nil
}

// rollback removes a record that failed partway. The next write seeks to the
// new end, which also covers a new session's file, opened without append
// mode.
func (b *fileBackend) rollback(offset int64, cause error) error {
	if err := b.file.Truncate(offset); err != nil {
		b.broken = fmt.Errorf("session file has an incomplete record: %w", errors.Join(cause, err))
		return b.broken
	}
	return cause
}

func (b *fileBackend) blobDir() string { return b.path + ".blobs" }

// saveMedia writes the bytes whole under a temporary name and renames them into
// place, so a crash never leaves a partial blob under its hash.
func (b *fileBackend) saveMedia(hash string, data []byte) error {
	dir := b.blobDir()
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return fmt.Errorf("create media blob directory: %w", err)
	}
	path := filepath.Join(dir, hash)
	if _, err := os.Stat(path); err == nil {
		return nil
	} else if !errors.Is(err, os.ErrNotExist) {
		return fmt.Errorf("stat media blob: %w", err)
	}
	tmp, err := os.CreateTemp(dir, ".blob-*")
	if err != nil {
		return fmt.Errorf("create media blob: %w", err)
	}
	defer os.Remove(tmp.Name())
	if err := tmp.Chmod(0o600); err != nil {
		tmp.Close()
		return err
	}
	if _, err := tmp.Write(data); err != nil {
		tmp.Close()
		return fmt.Errorf("write media blob: %w", err)
	}
	if err := tmp.Sync(); err != nil {
		tmp.Close()
		return fmt.Errorf("sync media blob: %w", err)
	}
	if err := tmp.Close(); err != nil {
		return fmt.Errorf("close media blob: %w", err)
	}
	if err := os.Rename(tmp.Name(), path); err != nil {
		if _, statErr := os.Stat(path); statErr != nil {
			return fmt.Errorf("install media blob: %w", err)
		}
	}
	return nil
}

// readMedia loads a blob and checks its digest, which catches missing or
// corrupted data before it reaches the wire.
func (b *fileBackend) readMedia(hash string) ([]byte, error) {
	f, err := os.Open(filepath.Join(b.blobDir(), hash))
	if err != nil {
		return nil, fmt.Errorf("open media blob: %w", err)
	}
	defer f.Close()
	data, err := io.ReadAll(io.LimitReader(f, maxMediaBytes+1))
	if err != nil {
		return nil, fmt.Errorf("read media blob: %w", err)
	}
	if len(data) > maxMediaBytes {
		return nil, errors.New("media blob is too large")
	}
	digest := sha256.Sum256(data)
	if hex.EncodeToString(digest[:]) != hash {
		return nil, errors.New("media blob hash mismatch")
	}
	return data, nil
}

func (b *fileBackend) close(discard bool) error {
	err := b.file.Sync()
	closeErr := b.file.Close()
	// A session that never grew past its root system message is an accidental
	// launch: remove it so it does not become the newest resume target.
	if discard {
		if removeErr := os.Remove(b.path); removeErr != nil && !errors.Is(removeErr, os.ErrNotExist) {
			err = errors.Join(err, fmt.Errorf("discard empty session: %w", removeErr))
		}
		if removeErr := os.RemoveAll(b.blobDir()); removeErr != nil {
			err = errors.Join(err, fmt.Errorf("discard empty session blobs: %w", removeErr))
		}
	}
	// Release only after the file is closed or discarded, so the next writer
	// never sees it mid-close. A discarded session's lock file goes with it;
	// with the session gone, nothing can lock it again.
	closeErr = errors.Join(closeErr, b.lock.Unlock())
	if discard {
		_ = os.Remove(lockPath(b.path))
	}
	return errors.Join(err, closeErr)
}
