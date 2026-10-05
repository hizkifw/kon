package login

import (
	"context"
	"net/http"
	"net/http/httptest"
	"reflect"
	"slices"
	"testing"

	"kon.kitsu.red/core/provider/wire"
	"kon.kitsu.red/internal/buildinfo"
	"kon.kitsu.red/internal/config"
)

func TestDiscoverOpenRouterVerifiesKeyBeforeListing(t *testing.T) {
	var paths []string
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		paths = append(paths, r.URL.Path)
		if got := r.Header.Get("User-Agent"); got != buildinfo.UserAgent() {
			t.Errorf("%s User-Agent = %q, want %q", r.URL.Path, got, buildinfo.UserAgent())
		}
		if r.Header.Get("Authorization") != "Bearer secret" {
			w.WriteHeader(http.StatusUnauthorized)
			return
		}
		switch r.URL.Path {
		case "/v1/key":
			w.Write([]byte(`{"data":{}}`))
		case "/v1/models":
			w.Write([]byte(`{"data":[{"id":"z/model"},{"id":"a/model"},{"id":"a/model"}]}`))
		default:
			w.WriteHeader(http.StatusNotFound)
		}
	}))
	defer server.Close()
	models, verified, err := Discover(context.Background(), config.Provider{ID: "openrouter", Type: "openrouter", BaseURL: server.URL + "/v1", APIKey: "secret"})
	if err != nil || !verified || !reflect.DeepEqual(models, []Model{{ID: "a/model"}, {ID: "z/model"}}) {
		t.Fatalf("models=%q verified=%v error=%v", models, verified, err)
	}
	if !slices.Equal(paths, []string{"/v1/key", "/v1/models"}) {
		t.Fatalf("request paths = %q", paths)
	}
}

func TestOpenRouterWireProviderDoesNotAssumeOpenRouterKeyEndpoint(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path != "/v1/models" {
			t.Errorf("unexpected path %q", r.URL.Path)
		}
		w.Write([]byte(`{"data":[{"id":"model"}]}`))
	}))
	defer server.Close()
	models, verified, err := Discover(context.Background(), config.Provider{ID: "standardcompute", Type: "openrouter", BaseURL: server.URL + "/v1", APIKey: "secret"})
	if err != nil || !verified || !reflect.DeepEqual(models, []Model{{ID: "model"}}) {
		t.Fatalf("models=%q verified=%v error=%v", models, verified, err)
	}
}

func TestDiscoverRejectsInvalidKey(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusUnauthorized)
	}))
	defer server.Close()
	// A format whose listing is optional excuses only a missing endpoint; a
	// server that answers 401 has rejected the key.
	for _, format := range []wire.Format{wire.OpenAI, wire.OpenAICompatible, wire.Anthropic} {
		_, verified, err := Discover(context.Background(), config.Provider{ID: "custom", Type: format, BaseURL: server.URL, APIKey: "bad"})
		if err == nil || verified {
			t.Fatalf("%s invalid key: verified=%v error=%v", format, verified, err)
		}
	}
}

func TestDiscoverCompatibleCanLackModelListing(t *testing.T) {
	server := httptest.NewServer(http.NotFoundHandler())
	defer server.Close()
	models, verified, err := Discover(context.Background(), config.Provider{ID: "custom", Type: "openai-compatible", BaseURL: server.URL})
	if err != nil || verified || len(models) != 0 {
		t.Fatalf("models=%q verified=%v error=%v", models, verified, err)
	}
}

func TestDiscoverOllamaUsesCompatibleModelEndpoint(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path != "/v1/models" {
			t.Errorf("path = %q", r.URL.Path)
		}
		w.Write([]byte(`{"data":[{"id":"coder:latest"}]}`))
	}))
	defer server.Close()
	models, verified, err := Discover(context.Background(), config.Provider{ID: "ollama", Type: "ollama", BaseURL: server.URL})
	if err != nil || !verified || !reflect.DeepEqual(models, []Model{{ID: "coder:latest"}}) {
		t.Fatalf("models=%q verified=%v error=%v", models, verified, err)
	}
}

func TestDiscoverDoesNotForwardCredentialsOnRedirect(t *testing.T) {
	forwarded := false
	destination := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		forwarded = true
	}))
	defer destination.Close()
	source := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		http.Redirect(w, r, destination.URL, http.StatusFound)
	}))
	defer source.Close()
	_, _, err := Discover(context.Background(), config.Provider{ID: "openai", Type: "openai", BaseURL: source.URL, APIKey: "secret"})
	if err == nil || forwarded {
		t.Fatalf("redirect followed: forwarded=%v error=%v", forwarded, err)
	}
}

func TestDiscoverInputModalitiesBelongToEachModel(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte(`{"data":[
			{"id":"cached","status":{"value":"unloaded"}},
			{"id":"vision","status":{"value":"loaded"},"architecture":{"input_modalities":["text","image"],"output_modalities":["text"]}},
			{"id":"text","architecture":{"input_modalities":["text"],"output_modalities":["image"]}},
			{"id":"unknown","architecture":{"input_modalities":["other"]}},
			{"id":"empty","architecture":{"input_modalities":[]}},
			{"id":"output-only","architecture":{"output_modalities":["image"]}},
			{"id":"Qwen-vision"}
		]}`))
	}))
	defer server.Close()
	models, verified, err := Discover(t.Context(), config.Provider{ID: "local", Type: wire.OpenAICompatible, BaseURL: server.URL})
	want := []Model{
		{ID: "Qwen-vision"}, {ID: "cached"}, {ID: "empty", InputModalities: []string{}},
		{ID: "output-only"}, {ID: "text", InputModalities: []string{"text"}},
		{ID: "unknown", InputModalities: []string{"other"}}, {ID: "vision", InputModalities: []string{"text", "image"}},
	}
	if err != nil || !verified || !reflect.DeepEqual(models, want) {
		t.Fatalf("models=%+v verified=%v error=%v, want %+v", models, verified, err, want)
	}
}
