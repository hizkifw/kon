package ui

import (
	"fmt"
	"slices"
	"testing"
)

func BenchmarkMentionCandidates(b *testing.B) {
	for _, n := range []int{1_000, 10_000, 50_000} {
		b.Run(fmt.Sprintf("files=%d", n), func(b *testing.B) {
			m := newTestModel(b)
			files := make([]string, n)
			shapes := []string{
				"README%05d.md",
				"cmd/tool%05d/main.go",
				"internal/service%05d/handler.go",
				"web/src/components/group%05d/Button.tsx",
				"docs/guides/topic%05d/intro.md",
				"testdata/fixture%05d/input.json",
			}
			for i := range files {
				files[i] = fmt.Sprintf(shapes[i%len(shapes)], i)
				// Sparse matches exercise the usual case alongside the broad @ query.
				if i%1000 == 0 {
					files[i] = fmt.Sprintf("internal/model/Model%05d.go", i)
				}
			}
			slices.Sort(files)
			m.mentions.files = indexMentionFiles(files)
			for _, query := range []string{"@", "@m", "@mo", "@mod", "@mode", "@model", "@i/m/mod", "@missing"} {
				b.Run(query, func(b *testing.B) {
					m.input.SetValue(query)
					var items []menuItem
					b.ReportAllocs()
					for b.Loop() {
						items = (mentionSource{}).Candidates(m, query)
					}
					b.ReportMetric(float64(len(items)), "matches/op")
				})
			}
		})
	}
}
