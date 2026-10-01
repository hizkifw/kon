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
	return start("", header, &memoryBackend{images: make(map[string][]byte)}, systemPrompt)
}

// memoryBackend keeps no records, since the store already holds every entry,
// and keeps images by hash.
type memoryBackend struct {
	mu     sync.Mutex
	images map[string][]byte
}

func (*memoryBackend) write([]byte, bool) error { return nil }

func (m *memoryBackend) saveImage(hash string, data []byte) error {
	m.mu.Lock()
	defer m.mu.Unlock()
	m.images[hash] = bytes.Clone(data)
	return nil
}

func (m *memoryBackend) readImage(hash string) ([]byte, error) {
	m.mu.Lock()
	defer m.mu.Unlock()
	data, ok := m.images[hash]
	if !ok {
		return nil, errors.New("image blob not found")
	}
	return data, nil
}

func (m *memoryBackend) close(bool) error {
	m.mu.Lock()
	defer m.mu.Unlock()
	m.images = nil
	return nil
}
