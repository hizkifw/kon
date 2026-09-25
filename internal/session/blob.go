package session

import (
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
)

const maxBlobBytes = 20 << 20

func validImageHash(hash string) bool {
	if len(hash) != 64 {
		return false
	}
	for _, c := range hash {
		if c < '0' || c > '9' {
			if c < 'a' || c > 'f' {
				return false
			}
		}
	}
	return true
}

func (s *Store) blobDir() string { return s.path + ".blobs" }

// JobsDir is where the session's background jobs keep their files. The
// session only names it; internal/tools owns what goes inside.
func (s *Store) JobsDir() string { return s.path + ".jobs" }

// SaveImage writes an image before its session entry can refer to it. Equal
// bytes share one file within a session, and the JSONL keeps only the hash.
func (s *Store) SaveImage(data []byte, mime string) (Part, error) {
	if len(data) == 0 || len(data) > maxBlobBytes || mime == "" {
		return Part{}, errors.New("image requires bounded bytes and a MIME type")
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.file == nil {
		return Part{}, errors.New("session is closed")
	}
	digest := sha256.Sum256(data)
	hash := hex.EncodeToString(digest[:])
	dir := s.blobDir()
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return Part{}, fmt.Errorf("create image blob directory: %w", err)
	}
	path := filepath.Join(dir, hash)
	if _, err := os.Stat(path); err == nil {
		return Part{Type: PartImage, ImageHash: hash, ImageMIME: mime}, nil
	} else if !errors.Is(err, os.ErrNotExist) {
		return Part{}, fmt.Errorf("stat image blob: %w", err)
	}
	tmp, err := os.CreateTemp(dir, ".blob-*")
	if err != nil {
		return Part{}, fmt.Errorf("create image blob: %w", err)
	}
	defer os.Remove(tmp.Name())
	if err := tmp.Chmod(0o600); err != nil {
		tmp.Close()
		return Part{}, err
	}
	if _, err := tmp.Write(data); err != nil {
		tmp.Close()
		return Part{}, fmt.Errorf("write image blob: %w", err)
	}
	if err := tmp.Sync(); err != nil {
		tmp.Close()
		return Part{}, fmt.Errorf("sync image blob: %w", err)
	}
	if err := tmp.Close(); err != nil {
		return Part{}, fmt.Errorf("close image blob: %w", err)
	}
	if err := os.Rename(tmp.Name(), path); err != nil {
		if _, statErr := os.Stat(path); statErr != nil {
			return Part{}, fmt.Errorf("install image blob: %w", err)
		}
	}
	return Part{Type: PartImage, ImageHash: hash, ImageMIME: mime}, nil
}

// ReadImage loads a referenced blob on demand for a provider request. The
// digest check catches missing or corrupted data before it reaches the wire.
func (s *Store) ReadImage(hash string) ([]byte, error) {
	if !validImageHash(hash) {
		return nil, errors.New("invalid image blob hash")
	}
	f, err := os.Open(filepath.Join(s.blobDir(), hash))
	if err != nil {
		return nil, fmt.Errorf("open image blob: %w", err)
	}
	defer f.Close()
	data, err := io.ReadAll(io.LimitReader(f, maxBlobBytes+1))
	if err != nil {
		return nil, fmt.Errorf("read image blob: %w", err)
	}
	if len(data) > maxBlobBytes {
		return nil, errors.New("image blob is too large")
	}
	digest := sha256.Sum256(data)
	if hex.EncodeToString(digest[:]) != hash {
		return nil, errors.New("image blob hash mismatch")
	}
	return data, nil
}
