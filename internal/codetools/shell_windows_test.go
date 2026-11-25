package codetools

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func TestGitBashPreservesCommandText(t *testing.T) {
	if shellName() != "Git Bash" {
		t.Skip("requires Git Bash")
	}
	// The read must see EOF even when more script lines follow it.
	const pattern = `^C:\\Users\\Example\\\.cargo\\bin$`
	const command = "if read -r line; then exit 9; fi\ncat <<'END'\n" + pattern + "\nEND\nexit 7"
	t.Run("foreground", func(t *testing.T) {
		result, _ := newExecutor(t.TempDir(), nil, nil).Execute(context.Background(), "shell", raw(map[string]any{"command": command, "timeout": 10}), nil)
		if !strings.HasPrefix(result.Content, pattern+"\n") || !strings.Contains(result.Content, "exit code: 7") {
			t.Fatalf("shell result = %q", result.Content)
		}
	})
	t.Run("background", func(t *testing.T) {
		dir := t.TempDir()
		done := make(chan string, 1)
		jobs := NewJobs(dir, "session", false, "", func(s string) { done <- s })
		defer jobs.Close()
		if _, _, err := jobs.Start(command, dir); err != nil {
			t.Fatal(err)
		}
		select {
		case <-done:
		case <-time.After(10 * time.Second):
			t.Fatal("job did not finish")
		}
		for name, want := range map[string]string{"output": pattern, "exit": "7"} {
			b, err := os.ReadFile(filepath.Join(dir, "1", name))
			if err != nil || strings.TrimSpace(string(b)) != want {
				t.Fatalf("%s = %q, %v; want %q", name, b, err, want)
			}
		}
	})
}
