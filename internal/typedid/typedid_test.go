package typedid

import (
	"encoding/json"
	"slices"
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
	// Parsing checks the base62 alphabet, and it is what reading a session
	// applies, so an ID that fails it would be written but never read back.
	if _, err := ParseSessionID(sessionA.String()); err != nil {
		t.Fatalf("generated session ID does not parse: %v", err)
	}
	if _, err := ParseEntryID(entry.String()); err != nil {
		t.Fatalf("generated entry ID does not parse: %v", err)
	}
}

func TestOwnedIDJSONRejectsWrongTypeAndAlphabet(t *testing.T) {
	for _, input := range []string{
		`"ent_0123456789ABCDEFGHIJ"`,
		`"ses_0123456789ABCDEFGH-I"`,
		`"bare"`,
		// The right prefix and alphabet, but one character short or long.
		`"ses_0123456789ABCDEFGHI"`,
		`"ses_0123456789ABCDEFGHIJK"`,
	} {
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

// TestExternalIDsRoundTripWithoutValidation decodes and re-encodes IDs that a
// provider owns. kon must accept whatever a provider sends and send it back
// byte for byte, or a later request could not refer to the same call or model.
func TestExternalIDsRoundTripWithoutValidation(t *testing.T) {
	type record struct {
		Calls []ToolCallID `json:"calls"`
		Model ModelID      `json:"model"`
	}
	const input = `{"calls":["","call 1"," ","functions/read:0","呼び出し☃"],"model":"accounts/acme/models/llama 3 ☃"}`
	var got record
	if err := json.Unmarshal([]byte(input), &got); err != nil {
		t.Fatal(err)
	}
	want := record{
		Calls: []ToolCallID{"", "call 1", " ", "functions/read:0", "呼び出し☃"},
		Model: "accounts/acme/models/llama 3 ☃",
	}
	if !slices.Equal(got.Calls, want.Calls) || got.Model != want.Model {
		t.Fatalf("decoded = %q, want %q", got, want)
	}
	b, err := json.Marshal(got)
	if err != nil {
		t.Fatal(err)
	}
	if string(b) != input {
		t.Fatalf("encoded = %s, want %s", b, input)
	}
}
