package session

import (
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"sync"
	"time"

	"kon.kitsu.red/core/tokens"
	"kon.kitsu.red/core/typedid"
)

// Store is a session open for appending: an append-only, parent-linked list
// of entries, kept in memory and in a backend. Create and Open keep it in a
// file, and NewMemory nowhere. It is safe for concurrent use.
type Store struct {
	mu      sync.Mutex
	header  Header
	path    string
	backend backend
	entries []Entry
	byID    map[typedid.EntryID]int
	leafID  *typedid.EntryID
	// empty is true while the session holds nothing beyond its root system
	// message. Such a session is discarded on close so an accidental launch does
	// not leave a resumable file behind, which would otherwise shadow an earlier
	// session that actually has content.
	empty  bool
	closed bool
}

// backend is where a store's records and media go.
type backend interface {
	// write appends one encoded record, syncing it to stable storage when
	// sync is set. A record that fails to write must leave no trace.
	write(record []byte, sync bool) error
	saveMedia(hash string, data []byte) error
	readMedia(hash string) ([]byte, error)
	// close releases the backend, first discarding everything it holds when
	// discard is set.
	close(discard bool) error
}

// maxMediaBytes bounds one stored media file.
const maxMediaBytes = 20 << 20

// start begins a new session in b: its header, then its root system message.
func start(path string, header Header, b backend, systemPrompt string) (*Store, error) {
	header.Type, header.Version = "session", SchemaVersion
	if header.ID.IsZero() {
		id, err := typedid.NewSessionID()
		if err != nil {
			return nil, err
		}
		header.ID = id
	}
	if header.Timestamp.IsZero() {
		header.Timestamp = time.Now().UTC()
	}
	record, err := json.Marshal(header)
	if err != nil {
		return nil, fmt.Errorf("encode session header: %w", err)
	}
	if err := b.write(record, false); err != nil {
		return nil, err
	}
	s := &Store{header: header, path: path, backend: b, byID: make(map[typedid.EntryID]int), empty: true}
	if _, err := s.AppendMessage(TextMessage(RoleSystem, systemPrompt)); err != nil {
		return nil, err
	}
	return s, nil
}

// Path is the session's file, or "" for a session kept in memory.
func (s *Store) Path() string { return s.path }

// CWD is the working directory the header records.
func (s *Store) CWD() string { return s.header.CWD }

// ID is the stable session identifier persisted in the header.
func (s *Store) ID() typedid.SessionID { return s.header.ID }

// Empty reports whether the session still holds only structural entries (the
// root system message and model changes), with no conversation to keep. Such a
// session is deleted when closed.
func (s *Store) Empty() bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.empty
}

// CreatedAt is the session's creation time from the header.
func (s *Store) CreatedAt() time.Time { return s.header.Timestamp }

// ActivePath returns the entries from the root to the active leaf in
// conversation order. It is a snapshot used for read-only display.
func (s *Store) ActivePath() []Entry {
	s.mu.Lock()
	defer s.mu.Unlock()
	path, err := s.activePathLocked()
	if err != nil {
		return nil
	}
	return path
}

// Close releases the session. A session that never held a conversation is
// discarded rather than kept.
func (s *Store) Close() error {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.closed {
		return nil
	}
	s.closed = true
	return s.backend.close(s.empty)
}

// AppendMessage appends message after the active leaf.
func (s *Store) AppendMessage(message Message) (typedid.EntryID, error) {
	if err := message.Validate(); err != nil {
		return typedid.EntryID{}, fmt.Errorf("append message: %w", err)
	}
	return s.append(Entry{Type: EntryTypeMessage, Message: &message})
}

// AppendCompaction records a summary that replaces everything before
// firstKeptID in the projected context. tokensBefore is the context's size
// when it was compacted, and estimated whether that size is a guess; usage is
// what writing the summary cost.
func (s *Store) AppendCompaction(summary string, firstKeptID typedid.EntryID, tokensBefore tokens.Count, estimated bool, usage *Usage) (typedid.EntryID, error) {
	if firstKeptID.IsZero() {
		return typedid.EntryID{}, errors.New("compaction requires a retained entry")
	}
	return s.append(Entry{
		Type:                  EntryTypeCompaction,
		Summary:               summary,
		FirstKeptEntryID:      &firstKeptID,
		TokensBefore:          tokensBefore,
		TokensBeforeEstimated: estimated,
		Usage:                 usage,
	})
}

// AppendModelChange records the model the conversation continues with.
func (s *Store) AppendModelChange(selection ModelSelection) (typedid.EntryID, error) {
	if selection.Name == "" || selection.WireFormat == "" || selection.ExternalID.String() == "" {
		return typedid.EntryID{}, errors.New("model change requires name, wire format, and external ID")
	}
	return s.append(Entry{Type: EntryTypeModelChange, Model: &selection})
}

// AppendTurnStart marks the start of a turn.
func (s *Store) AppendTurnStart() (typedid.EntryID, error) {
	return s.append(Entry{Type: EntryTypeTurnStart})
}

// AppendTurnEnd marks the end of a turn that took duration.
func (s *Store) AppendTurnEnd(duration time.Duration) (typedid.EntryID, error) {
	if duration < 0 {
		return typedid.EntryID{}, errors.New("turn end requires a non-negative duration")
	}
	return s.append(Entry{Type: EntryTypeTurnEnd, DurationMS: duration.Milliseconds()})
}

func (s *Store) append(entry Entry) (typedid.EntryID, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.closed {
		return typedid.EntryID{}, errors.New("session is closed")
	}
	id, err := typedid.NewEntryID()
	if err != nil {
		return typedid.EntryID{}, err
	}
	entry.ID = id
	entry.Timestamp = time.Now().UTC()
	if s.leafID != nil {
		parent := *s.leafID
		entry.ParentID = &parent
	}
	record, err := json.Marshal(entry)
	if err != nil {
		return typedid.EntryID{}, fmt.Errorf("encode session entry: %w", err)
	}
	// The root system message is written through this path on creation, and model
	// changes and turn starts are structural; none makes the session worth
	// keeping. The first appended conversation or compaction entry does.
	//
	// An empty session skips the sync: Close discards it, discovery ignores it
	// if a crash leaves it behind, and each fsync costs milliseconds of
	// startup. The first substantive entry's sync makes every earlier line
	// durable along with it.
	substantive := len(s.entries) > 0 && entry.Type != EntryTypeModelChange && entry.Type != EntryTypeTurnStart
	if err := s.backend.write(record, !s.empty || substantive); err != nil {
		return typedid.EntryID{}, err
	}
	if substantive {
		s.empty = false
	}
	s.byID[id] = len(s.entries)
	s.entries = append(s.entries, entry)
	s.leafID = &id
	return id, nil
}

// SaveMedia stores an image, audio clip, video, or document before a session
// entry can refer to it, and returns the part that does. Equal bytes are
// stored once per session, and the entry keeps only their hash.
func (s *Store) SaveMedia(data []byte, mime string) (Part, error) {
	if len(data) == 0 || len(data) > maxMediaBytes || mime == "" {
		return Part{}, errors.New("media requires bounded bytes and a MIME type")
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.closed {
		return Part{}, errors.New("session is closed")
	}
	digest := sha256.Sum256(data)
	hash := hex.EncodeToString(digest[:])
	if err := s.backend.saveMedia(hash, data); err != nil {
		return Part{}, err
	}
	return Part{Type: PartMedia, MediaHash: hash, MediaMIME: mime}, nil
}

// ReadMedia loads stored media by hash, as a provider request needs it.
func (s *Store) ReadMedia(hash string) ([]byte, error) {
	if !validBlobHash(hash) {
		return nil, errors.New("invalid media blob hash")
	}
	return s.backend.readMedia(hash)
}

func validBlobHash(hash string) bool {
	if len(hash) != 64 {
		return false
	}
	for _, c := range hash {
		if (c < '0' || c > '9') && (c < 'a' || c > 'f') {
			return false
		}
	}
	return true
}

func (s *Store) activePathLocked() ([]Entry, error) {
	return activePath(s.entries, s.byID, s.leafID)
}

// activePath follows parent links from leaf back to the root and returns the
// path in conversation order.
func activePath(entries []Entry, byID map[typedid.EntryID]int, leaf *typedid.EntryID) ([]Entry, error) {
	if leaf == nil {
		return nil, nil
	}
	var reverse []Entry
	current := *leaf
	seen := make(map[typedid.EntryID]bool)
	for {
		if seen[current] {
			return nil, errors.New("cycle in session parent links")
		}
		seen[current] = true
		idx, ok := byID[current]
		if !ok {
			return nil, fmt.Errorf("missing session entry %q", current)
		}
		entry := entries[idx]
		reverse = append(reverse, entry)
		if entry.ParentID == nil {
			break
		}
		current = *entry.ParentID
	}
	path := make([]Entry, len(reverse))
	for i := range reverse {
		path[len(reverse)-1-i] = reverse[i]
	}
	return path, nil
}
