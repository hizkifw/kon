package tokens

import (
	"encoding/json"
	"testing"
)

func TestCountString(t *testing.T) {
	for _, test := range []struct {
		count Count
		want  string
	}{
		{0, "0"},
		{950, "950"},
		{1_000, "1.0k"},
		{12_400, "12.4k"},
		{1_000_000, "1.0m"},
		{1_300_000, "1.3m"},
	} {
		if got := test.count.String(); got != test.want {
			t.Errorf("Count(%d).String() = %q, want %q", int(test.count), got, test.want)
		}
	}
}

func TestCountJSONIsPlainInteger(t *testing.T) {
	data, err := json.Marshal(struct {
		N Count `json:"n"`
	}{12_400})
	if err != nil {
		t.Fatal(err)
	}
	if string(data) != `{"n":12400}` {
		t.Fatalf("marshal = %s", data)
	}
}
