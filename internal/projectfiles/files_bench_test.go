package projectfiles

import (
	"context"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

func BenchmarkList(b *testing.B) {
	for _, n := range []int{1_000, 10_000, 50_000, 100_000} {
		b.Run(fmt.Sprintf("files=%d", n), func(b *testing.B) {
			root := b.TempDir()
			// Keep the plain-directory case outside any enclosing repository,
			// including when TMPDIR points inside the checkout.
			b.Setenv("GIT_CEILING_DIRECTORIES", filepath.Dir(root))
			depths := []string{"", "src", "web/components"}
			names := []string{"Model%03d.go", "Handler%03d.go", "README%03d.md", "Button%03d.tsx", "input%03d.json"}
			for i := range n {
				group := "tracked"
				if i >= n/2 {
					group = "untracked"
				}
				dir := filepath.Join(root, group, depths[(i/100)%len(depths)], fmt.Sprintf("Package%05d", i/100))
				if i%100 == 0 {
					if err := os.MkdirAll(dir, 0o755); err != nil {
						b.Fatal(err)
					}
				}
				name := fmt.Sprintf(names[i%len(names)], i%100)
				if err := os.WriteFile(filepath.Join(dir, name), nil, 0o600); err != nil {
					b.Fatal(err)
				}
			}
			b.Run("directory", func(b *testing.B) {
				// The walk counts directory entries as well as files.
				benchmarkList(b, root, n, n >= maxFiles)
			})
			b.Run("git", func(b *testing.B) {
				if _, err := exec.LookPath("git"); err != nil {
					b.Skip("git is not installed")
				}
				for _, args := range [][]string{{"init", "--quiet"}, {"add", "--", "tracked"}} {
					cmd := exec.Command("git", append([]string{"-c", "core.fsmonitor=false", "-C", root}, args...)...)
					if out, err := cmd.CombinedOutput(); err != nil {
						b.Fatalf("git %v: %s: %v", args, out, err)
					}
				}
				benchmarkList(b, root, n, n > maxFiles)
			})
		})
	}
}

func benchmarkList(b *testing.B, root string, n int, partial bool) {
	b.Helper()
	ctx := context.Background()
	var files []string
	var err error
	b.ReportAllocs()
	for b.Loop() {
		files, err = List(ctx, root)
		if (partial && !errors.Is(err, errLimit)) || (!partial && err != nil) {
			b.Fatalf("discovery error = %v, want partial = %v", err, partial)
		}
		if len(files) == 0 || len(files) > maxFiles || (!partial && len(files) != n) {
			b.Fatalf("discovered %d of %d files", len(files), n)
		}
	}
	b.ReportMetric(float64(len(files)), "files/op")
}
