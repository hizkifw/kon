package ui

import (
	"fmt"
	"testing"
)

func BenchmarkMentionCandidates(b *testing.B) {
	m := newTestModel(b)
	var files []string
	for i := range 50_000 {
		files = append(files, fmt.Sprintf("internal/Package%05d/Model.go", i))
	}
	m.mentions.files = indexMentionFiles(files)
	for _, query := range []string{"@", "@model", "@i/p/model", "@missing"} {
		b.Run(query, func(b *testing.B) {
			m.input.SetValue(query)
			b.ReportAllocs()
			b.ResetTimer()
			for b.Loop() {
				_ = (mentionSource{}).Candidates(m, query)
			}
		})
	}
}
