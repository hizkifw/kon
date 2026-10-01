package session

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sync"
	"time"

	"github.com/hizkifw/kon/core/tokens"
	"github.com/hizkifw/kon/core/typedid"

	"github.com/gofrs/flock"
)

type Store struct {
	mu      sync.Mutex
	header  Header
	path    string
	file    sessionFile
	lock    *flock.Flock
	entries []Entry
	byID    map[typedid.EntryID]int
	leafID  *typedid.EntryID
	// empty is true while the session holds nothing beyond its root system
	// message. Such a session is discarded on close so an accidental launch does
	// not leave a resumable file behind, which would otherwise shadow an earlier
	// session that actually has content.
	empty bool
	// broken is set when a failed append could not be rolled back. The file
	// then ends in a torn line, and appending after it would bury that line
	// mid-file where Open refuses it, so every later append fails instead.
	broken error
	// images and scratch hold what an ephemeral session would otherwise keep
	// beside its file: image bytes by hash, and the temporary directory its
	// background jobs use. Both are unset for a persisted session.
	images  map[string][]byte
	scratch string
}

// sessionFile is the part of *os.File the store writes through, so tests can
// simulate a write that fails partway.
type sessionFile interface {
	Write([]byte) (int, error)
	Seek(offset int64, whence int) (int64, error)
	Truncate(size int64) error
	Sync() error
	Close() error
}

func New(root, cwd, appVersion, systemPrompt string) (*Store, error) {
	return NewChild(root, cwd, appVersion, systemPrompt, typedid.SessionID{})
}

// NewChild creates a session that records parent as the session that started
// it. A zero parent makes an ordinary session.
func NewChild(root, cwd, appVersion, systemPrompt string, parent typedid.SessionID) (*Store, error) {
	dir, err := directoryFor(root, cwd)
	if err != nil {
		return nil, err
	}
	absCWD, err := filepath.Abs(cwd)
	if err != nil {
		return nil, fmt.Errorf("resolve working directory: %w", err)
	}
	absCWD = filepath.Clean(absCWD)
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return nil, fmt.Errorf("create session directory: %w", err)
	}

	sessionID, err := typedid.NewSessionID()
	if err != nil {
		return nil, err
	}
	now := time.Now().UTC()
	name := now.Format("20060102T150405.000Z") + "_" + sessionID.String() + fileSuffix
	path := filepath.Join(dir, name)
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
	s := &Store{
		header: Header{Type: "session", Version: SchemaVersion, ID: sessionID, AppVersion: appVersion, Timestamp: now, CWD: absCWD, Parent: parent},
		path:   path,
		file:   f,
		lock:   l,
		byID:   make(map[typedid.EntryID]int),
		empty:  true,
	}
	if err := s.writeLine(s.header, false); err != nil {
		f.Close()
		_ = l.Unlock()
		return nil, err
	}
	if _, err := s.AppendMessage(TextMessage(RoleSystem, systemPrompt)); err != nil {
		f.Close()
		_ = l.Unlock()
		return nil, err
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
	s := &Store{header: parsed.header, path: path, file: f, lock: l, entries: parsed.entries, byID: parsed.byID}
	// A session holding only its root system message and structural entries has
	// no conversation to keep.
	s.empty = true
	for _, entry := range parsed.entries {
		if entry.Type == EntryTypeCompaction || (entry.Message != nil && entry.Message.Role != RoleSystem) {
			s.empty = false
			break
		}
	}
	s.leafID = parsed.leafID()
	return s, nil
}

func (s *Store) Path() string { return s.path }
func (s *Store) CWD() string  { return s.header.CWD }

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

func (s *Store) Close() error {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.file == nil {
		return nil
	}
	if s.ephemeral() {
		s.file, s.images = nil, nil
		if err := os.RemoveAll(s.scratch); err != nil {
			return fmt.Errorf("remove incognito jobs directory: %w", err)
		}
		return nil
	}
	err := s.file.Sync()
	closeErr := s.file.Close()
	s.file = nil
	// A session that never grew past its root system message is an accidental
	// launch: remove it so it does not become the newest resume target.
	if s.empty {
		if removeErr := os.Remove(s.path); removeErr != nil && !errors.Is(removeErr, os.ErrNotExist) {
			err = errors.Join(err, fmt.Errorf("discard empty session: %w", removeErr))
		}
		if removeErr := os.RemoveAll(s.blobDir()); removeErr != nil {
			err = errors.Join(err, fmt.Errorf("discard empty session blobs: %w", removeErr))
		}
	}
	// Release only after the file is closed or discarded, so the next writer
	// never sees it mid-close. A discarded session's lock file goes with it;
	// with the session gone, nothing can lock it again.
	if s.lock != nil {
		closeErr = errors.Join(closeErr, s.lock.Unlock())
		if s.empty {
			_ = os.Remove(lockPath(s.path))
		}
	}
	return errors.Join(err, closeErr)
}

func (s *Store) AppendMessage(message Message) (typedid.EntryID, error) {
	if err := message.Validate(); err != nil {
		return typedid.EntryID{}, fmt.Errorf("append message: %w", err)
	}
	return s.append(Entry{Type: EntryTypeMessage, Message: &message})
}

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

func (s *Store) AppendModelChange(selection ModelSelection) (typedid.EntryID, error) {
	if selection.Name == "" || selection.WireFormat == "" || selection.ExternalID.String() == "" {
		return typedid.EntryID{}, errors.New("model change requires name, wire format, and external ID")
	}
	return s.append(Entry{Type: EntryTypeModelChange, Model: &selection})
}

func (s *Store) AppendTurnStart() (typedid.EntryID, error) {
	return s.append(Entry{Type: EntryTypeTurnStart})
}

func (s *Store) AppendTurnEnd(duration time.Duration) (typedid.EntryID, error) {
	if duration < 0 {
		return typedid.EntryID{}, errors.New("turn end requires a non-negative duration")
	}
	return s.append(Entry{Type: EntryTypeTurnEnd, DurationMS: duration.Milliseconds()})
}

func (s *Store) append(entry Entry) (typedid.EntryID, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.file == nil {
		return typedid.EntryID{}, errors.New("session is closed")
	}
	if s.broken != nil {
		return typedid.EntryID{}, s.broken
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
	// The root system message is written through this path on creation, and model
	// changes and turn starts are structural; none makes the session worth
	// keeping. The first appended conversation or compaction entry does.
	substantive := len(s.entries) > 0 && entry.Type != EntryTypeModelChange && entry.Type != EntryTypeTurnStart
	if err := s.writeLine(entry, !s.empty || substantive); err != nil {
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

// writeLine appends one record, syncing it when sync is set. An empty session
// skips the sync: Close discards it, discovery ignores it if a crash leaves it
// behind, and each fsync costs milliseconds of startup. The first substantive
// entry's sync makes every earlier line durable along with it.
//
// A failed write or sync truncates the file back to where the record began.
// The entry is not added in memory either, so file and memory stay in step,
// and a partial line from a full disk is never followed by the next record.
func (s *Store) writeLine(value any, sync bool) error {
	b, err := json.Marshal(value)
	if err != nil {
		return fmt.Errorf("encode session entry: %w", err)
	}
	offset, err := s.file.Seek(0, io.SeekEnd)
	if err != nil {
		return fmt.Errorf("find session end: %w", err)
	}
	if _, err := s.file.Write(append(b, '\n')); err != nil {
		return s.rollback(offset, fmt.Errorf("append session entry: %w", err))
	}
	if !sync {
		return nil
	}
	if err := s.file.Sync(); err != nil {
		return s.rollback(offset, fmt.Errorf("sync session entry: %w", err))
	}
	return nil
}

// rollback removes a record that failed partway. The next writeLine seeks to
// the new end, which also covers a new session's file, opened without append
// mode.
func (s *Store) rollback(offset int64, cause error) error {
	if err := s.file.Truncate(offset); err != nil {
		s.broken = fmt.Errorf("session file has an incomplete record: %w", errors.Join(cause, err))
		return s.broken
	}
	return cause
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
