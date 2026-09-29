package ui

import (
	"fmt"
	"testing"
)

func BenchmarkMentionCandidates(b *testing.B) {
	for _, n := range []int{1_000, 10_000, 50_000} {
		b.Run(fmt.Sprintf("files=%d", n), func(b *testing.B) {
			m := newTestModel(b)
			files := make([]string, n)
			for i := range files {
				files[i] = fmt.Sprintf("internal/Package%05d/Model.go", i)
			}
			m.mentions.files = indexMentionFiles(files)
			for _, query := range []string{"@", "@m", "@mo", "@mod", "@mode", "@model", "@i/p/model", "@missing"} {
				b.Run(query, func(b *testing.B) {
					m.input.SetValue(query)
					b.ReportAllocs()
					for b.Loop() {
						_ = (mentionSource{}).Candidates(m, query)
					}
				})
			}
		})
	}
}
