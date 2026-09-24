package wire

import "testing"

func TestBaseURLAppliesDefaultsAndAPIPath(t *testing.T) {
	for _, test := range []struct {
		format     Format
		configured string
		want       string
	}{
		{OpenAI, "", "https://api.openai.com/v1"},
		{OpenAI, "https://proxy.test/v1/", "https://proxy.test/v1"},
		{OpenRouter, "", "https://openrouter.ai/api/v1"},
		{Ollama, "", "http://localhost:11434/v1"},
		{Ollama, "http://gpu:11434/", "http://gpu:11434/v1"},
		{Ollama, "http://gpu:11434/v1", "http://gpu:11434/v1"},
		{OpenAICompatible, "http://localhost:8080/v1", "http://localhost:8080/v1"},
	} {
		spec, ok := Lookup(test.format)
		if !ok {
			t.Fatalf("%s is not implemented", test.format)
		}
		got, err := spec.BaseURL(test.configured)
		if err != nil || got != test.want {
			t.Fatalf("%s BaseURL(%q) = %q, %v; want %q", test.format, test.configured, got, err, test.want)
		}
	}
}

func TestCompatibleRequiresBaseURL(t *testing.T) {
	spec, _ := Lookup(OpenAICompatible)
	if !spec.RequiresBaseURL() {
		t.Fatal("openai-compatible has no canonical server")
	}
	if _, err := spec.BaseURL(" "); err == nil {
		t.Fatal("empty base URL was accepted")
	}
}

func TestDefaultIsImplemented(t *testing.T) {
	if _, ok := Lookup(Default); !ok {
		t.Fatalf("default format %q is not in the table", Default)
	}
	if _, ok := Lookup("anthropic"); ok {
		t.Fatal("an unimplemented format was found")
	}
}
