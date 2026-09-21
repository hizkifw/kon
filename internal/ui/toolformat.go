package ui

import (
	"encoding/json"
	"fmt"
	"path/filepath"
	"strconv"
	"strings"
)

// toolSummary renders a tool call's arguments as a short readable line, e.g.
// "internal/ui/view.go from 100" instead of a raw JSON dump.
func toolSummary(name, args, cwd string) string {
	var raw map[string]any
	if err := json.Unmarshal([]byte(args), &raw); err != nil {
		return args
	}
	value := func(key string) string { v, _ := raw[key].(string); return v }
	number := func(key string) int { v, _ := raw[key].(float64); return int(v) }
	switch name {
	case "read":
		summary := prettyPath(value("path"), cwd)
		if offset := number("offset"); offset > 1 {
			summary += fmt.Sprintf(" from line %d", offset)
		}
		return summary
	case "write":
		summary := prettyPath(value("path"), cwd)
		if content, ok := raw["content"].(string); ok {
			summary += " · " + humanBytes(len(content))
		}
		return summary
	case "edit":
		summary := prettyPath(value("path"), cwd)
		if oldText, newText := value("old_text"), value("new_text"); oldText != "" || newText != "" {
			summary += fmt.Sprintf(" · -%d +%d lines", countLines(oldText), countLines(newText))
		}
		return summary
	case "shell":
		return strings.ReplaceAll(value("command"), "\n", "; ")
	default:
		if compact, err := json.Marshal(raw); err == nil {
			return string(compact)
		}
		return args
	}
}

// prettyPath shortens a path for display: relative to the working directory
// when possible, otherwise with the home directory abbreviated.
func prettyPath(path, cwd string) string {
	if path == "" {
		return ""
	}
	if cwd != "" {
		if rel, err := filepath.Rel(cwd, path); err == nil && rel != ".." && !strings.HasPrefix(rel, ".."+string(filepath.Separator)) {
			return rel
		}
	}
	return abbreviateHome(path)
}

// readNote summarizes a successful read result by its line count. Rows look
// like "    12  text"; a trailing "… N more lines" marks truncated output.
// An empty note means the result could not be summarized and is shown as-is.
func readNote(text string) string {
	lines := strings.Split(text, "\n")
	extra := 0
	if last := lines[len(lines)-1]; strings.HasPrefix(last, "…") && strings.HasSuffix(last, "more lines") {
		fields := strings.Fields(last)
		if len(fields) >= 2 {
			if count, err := strconv.Atoi(strings.TrimPrefix(fields[1], "…")); err == nil {
				extra = count
				lines = lines[:len(lines)-1]
			}
		}
	}
	if len(lines) == 0 {
		return ""
	}
	lastRow := lines[len(lines)-1]
	if len(lastRow) < 6 {
		return ""
	}
	number, err := strconv.Atoi(strings.TrimSpace(lastRow[:6]))
	if err != nil {
		return ""
	}
	if number == 1 && extra == 0 && len(lastRow) <= 8 {
		return "empty file"
	}
	total := number + extra
	if total == 1 {
		return "1 line"
	}
	return fmt.Sprintf("%d lines", total)
}

// splitExitCode separates the trailing "exit code: N (took D)" marker that
// the shell tool appends from the command output that precedes it. took is
// empty when the marker carries no duration.
func splitExitCode(text string) (code, took, output string) {
	const marker = "exit code: "
	at := strings.LastIndex(text, marker)
	if at < 0 {
		return "", "", text
	}
	rest, tail := text[at+len(marker):], ""
	if open := strings.LastIndex(rest, " (took "); open >= 0 && strings.HasSuffix(rest, ")") {
		rest, tail = rest[:open], rest[open+len(" (took "):len(rest)-1]
	}
	if rest == "" || strings.Trim(rest, "0123456789") != "" {
		return "", "", text
	}
	return rest, tail, strings.TrimRight(text[:at], "\n")
}

func countLines(s string) int {
	if s == "" {
		return 0
	}
	return strings.Count(s, "\n") + 1
}

func humanBytes(n int) string {
	switch {
	case n >= 1<<20:
		return fmt.Sprintf("%.1fMiB", float64(n)/(1<<20))
	case n >= 1<<10:
		return fmt.Sprintf("%.1fKiB", float64(n)/(1<<10))
	default:
		return fmt.Sprintf("%dB", n)
	}
}
