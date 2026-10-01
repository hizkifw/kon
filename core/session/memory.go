package session

import (
	"bytes"
	"errors"
	"sync"
)

// NewMemory creates a session that lives only in memory: the same store and
// append path as a persisted session, with nothing written anywhere. Close
// discards it. The header is completed as Create completes it.
func NewMemory(header Header, systemPrompt string) (*Store, error) {
	return start("", header, &memoryBackend{media: make(map[string][]byte)}, systemPrompt)
}

// memoryBackend keeps no records, since the store already holds every entry,
// and keeps media by hash.
type memoryBackend struct {
	mu    sync.Mutex
	media map[string][]byte
}

func (*memoryBackend) write([]byte, bool) error { return nil }

func (m *memoryBackend) saveMedia(hash string, data []byte) error {
	m.mu.Lock()
	defer m.mu.Unlock()
	m.media[hash] = bytes.Clone(data)
	return nil
}

func (m *memoryBackend) readMedia(hash string) ([]byte, error) {
	m.mu.Lock()
	defer m.mu.Unlock()
	data, ok := m.media[hash]
	if !ok {
		return nil, errors.New("media blob not found")
	}
	return data, nil
}

func (m *memoryBackend) close(bool) error {
	m.mu.Lock()
	defer m.mu.Unlock()
	m.media = nil
	return nil
}
