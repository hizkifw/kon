package ui

import (
	"context"
	"path"
	"slices"
	"strconv"
	"strings"
	"unicode"
	"unicode/utf8"

	tea "charm.land/bubbletea/v2"
	"github.com/hizkifw/kon/internal/projectfiles"
)

const maxMentionMatches = 100

type fileMentions struct {
	files     []mentionFile
	loaded    bool
	loading   bool
	dismissed bool
	epoch     int
	cancel    context.CancelFunc
	err       error
}

type mentionFilesMsg struct {
	epoch int
	files []mentionFile
	err   error
}

type mentionFile struct {
	path, lower string
}

func indexMentionFiles(paths []string) []mentionFile {
	files := make([]mentionFile, len(paths))
	for i, path := range paths {
		files[i] = mentionFile{path: path, lower: strings.ToLower(path)}
	}
	return files
}

func (m *Model) resetMentions() {
	if m.mentions.cancel != nil {
		m.mentions.cancel()
	}
	m.mentions = fileMentions{epoch: m.mentions.epoch + 1}
}

// Files are discovered only while completing a mention. A fresh list for each
// new mention includes files created by the agent without a watcher or startup
// scan, and filtering that list never waits on the filesystem.
func (m *Model) loadMentionFiles() tea.Cmd {
	_, _, _, active := mentionAt(m.input.Value(), m.cursorOffset())
	if !active {
		m.resetMentions()
		return nil
	}
	m.mentions.dismissed = false
	if m.mentions.loaded || m.mentions.loading {
		return nil
	}
	ctx, cancel := context.WithCancel(m.ctx)
	m.mentions.cancel = cancel
	m.mentions.loading = true
	m.mentions.epoch++
	epoch, cwd := m.mentions.epoch, m.cwd
	return func() tea.Msg {
		defer cancel()
		files, err := projectfiles.List(ctx, cwd)
		// Normalize once off the update loop; keystrokes only score paths.
		return mentionFilesMsg{epoch: epoch, files: indexMentionFiles(files), err: err}
	}
}

func (m Model) applyMentionFiles(msg mentionFilesMsg) (tea.Model, tea.Cmd) {
	if msg.epoch != m.mentions.epoch {
		return m, nil
	}
	m.mentions.files, m.mentions.err = msg.files, msg.err
	m.mentions.loaded, m.mentions.loading, m.mentions.cancel = true, false, nil
	// A result must not reopen a dismissed popup or replace another surface.
	if !m.mentions.dismissed && m.search == nil && m.login == nil && m.drawers.Len() == 0 {
		m.openMenu()
		m.resize()
	}
	return m, nil
}

type mentionSource struct{}

var _ menuSource = mentionSource{}

func (mentionSource) Candidates(m Model, input string) []menuItem {
	_, _, query, ok := mentionAt(input, m.cursorOffset())
	if !ok {
		return nil
	}
	query = strings.ToLower(strings.ReplaceAll(query, `\`, "/"))
	query = strings.TrimPrefix(query, "./")
	type match struct {
		file  string
		score int
	}
	compare := func(a, b match) int {
		if a.score != b.score {
			return a.score - b.score
		}
		return strings.Compare(a.file, b.file)
	}
	matches := make([]match, 0, maxMentionMatches)
	for _, file := range m.mentions.files {
		score := mentionScore(file.lower, query)
		if score < 0 {
			continue
		}
		candidate := match{file.path, score}
		at, _ := slices.BinarySearchFunc(matches, candidate, compare)
		if at == maxMentionMatches {
			continue
		}
		// Keep only the best visible candidates instead of sorting every match.
		if len(matches) < maxMentionMatches {
			matches = append(matches, match{})
		}
		copy(matches[at+1:], matches[at:])
		matches[at] = candidate
	}
	items := make([]menuItem, 0, len(matches))
	for _, match := range matches {
		items = append(items, menuItem{Value: match.file, Label: mentionText(match.file)})
	}
	return items
}

// Prefer names and contiguous paths before looser subsequences, so both
// "model.go" and "i/u/mod" lead to useful results without a fuzzy-search dependency.
func mentionScore(file, query string) int {
	base := path.Base(file)
	switch {
	case query == "":
		return strings.Count(file, "/")
	case file == query || base == query:
		return 0
	case strings.HasPrefix(base, query):
		return 1
	case strings.HasPrefix(file, query):
		return 2
	case strings.Contains(base, query):
		return 3
	case strings.Contains(file, query):
		return 4
	}
	// A path separator makes fuzzy intent explicit; prose such as @alice
	// should not be replaced by a path that only shares scattered letters.
	if !strings.Contains(query, "/") {
		return -1
	}
	for _, r := range query {
		at := strings.IndexRune(file, r)
		if at < 0 {
			return -1
		}
		file = file[at+utf8.RuneLen(r):]
	}
	return 5
}

func (mentionSource) Accept(m *Model, file string) {
	input := m.input.Value()
	start, end, _, ok := mentionAt(input, m.cursorOffset())
	if !ok {
		return
	}
	prefix := input[:start] + mentionText(file) + " "
	m.input.SetValue(prefix + strings.TrimPrefix(input[end:], " "))
	m.setCursorOffset(len(prefix))
}

func mentionText(file string) string {
	// Quote opening delimiters too, so paired punctuation reads as part of
	// the filename even though only closing delimiters end an unquoted token.
	if strings.ContainsAny(file, "\"\\`()[]{},;") || strings.ContainsFunc(file, unicode.IsSpace) {
		return "@" + strconv.Quote(file)
	}
	return "@" + file
}

// mentionAt returns the whole token for replacement, but only the part before
// the cursor for searching. Requiring a word boundary leaves email addresses
// and inline code alone. Quoting keeps paths with spaces one editable token.
func mentionAt(input string, cursor int) (start, end int, query string, ok bool) {
	if cursor < 0 || cursor > len(input) {
		return
	}
	for at := 0; at < cursor; {
		i := strings.IndexByte(input[at:cursor], '@')
		if i < 0 {
			return
		}
		start = at + i
		at = start + 1
		if start > 0 {
			previous, _ := utf8.DecodeLastRuneInString(input[:start])
			if !unicode.IsSpace(previous) && !strings.ContainsRune("([{", previous) {
				continue
			}
		}
		from := start + 1
		if from < len(input) && input[from] == '"' {
			from++
			end = from
			for end < len(input) && input[end] != '"' && input[end] != '\n' {
				if input[end] == '\\' && end+1 < len(input) {
					end++
				}
				end++
			}
			if cursor < from || cursor > end {
				at = min(end+1, len(input))
				continue
			}
			query = input[from:cursor]
			if decoded, err := strconv.Unquote(`"` + query + `"`); err == nil {
				query = decoded
			}
			if end < len(input) && input[end] == '"' {
				end++
			}
			return start, end, query, true
		}
		end = from
		for end < len(input) {
			r, size := utf8.DecodeRuneInString(input[end:])
			if unicode.IsSpace(r) || strings.ContainsRune("\"`)]},;", r) {
				break
			}
			end += size
		}
		if cursor <= end {
			return start, end, input[from:cursor], true
		}
		at = end
	}
	return 0, 0, "", false
}
