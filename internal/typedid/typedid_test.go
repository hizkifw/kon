package typedid

import (
	"encoding/json"
	"strings"
	"testing"
)

func TestOwnedIDsArePrefixedBase62AndUnique(t *testing.T) {
	sessionA, err := NewSessionID()
	if err != nil {
		t.Fatal(err)
	}
	sessionB, err := NewSessionID()
	if err != nil {
		t.Fatal(err)
	}
	entry, err := NewEntryID()
	if err != nil {
		t.Fatal(err)
	}
	if sessionA == sessionB {
		t.Fatal("generated duplicate session IDs")
	}
	if !strings.HasPrefix(sessionA.String(), "ses_") || len(sessionA.String()) != 24 {
		t.Fatalf("session ID = %q", sessionA)
	}
	if !strings.HasPrefix(entry.String(), "ent_") || len(entry.String()) != 24 {
		t.Fatalf("entry ID = %q", entry)
	}
}

func TestOwnedIDJSONRejectsWrongTypeAndAlphabet(t *testing.T) {
	for _, input := range []string{`"ent_0123456789ABCDEFGHIJ"`, `"ses_0123456789ABCDEFGH-I"`, `"bare"`} {
		var id SessionID
		if err := json.Unmarshal([]byte(input), &id); err == nil {
			t.Fatalf("accepted %s", input)
		}
	}
}

func TestOwnedIDJSONRoundTrip(t *testing.T) {
	want, err := NewEntryID()
	if err != nil {
		t.Fatal(err)
	}
	b, err := json.Marshal(want)
	if err != nil {
		t.Fatal(err)
	}
	var got EntryID
	if err := json.Unmarshal(b, &got); err != nil {
		t.Fatal(err)
	}
	if got != want {
		t.Fatalf("round trip = %q, want %q", got, want)
	}
}

func TestExternalIDsRoundTripWithoutValidation(t *testing.T) {
	for _, value := range []string{"call-1", "", "provider/id with spaces"} {
		id := ExternalToolCallID(value)
		if id.String() != value {
			t.Fatalf("tool call ID = %q, want %q", id, value)
		}
	}
}
