// Package markdown renders CommonMark (plus GFM tables, strikethrough, task
// lists, and autolinks) into transcript-ready lines and styled spans.
//
// The design goal mirrors internal/ui's streaming invariants: a message
// rendered incrementally while it streams must be byte-identical to the same
// message rendered from scratch after it finalizes. The package therefore
// provides one core (blockRenderer) used by both paths:
//
//   - Render parses once and returns finished lines.
//   - Stream wraps the same core in append-only frozen blocks, so the stream's
//     published lines are already finished and never revised. Finish closes
//     the stream (freezing the open tail) before the result folds into
//     stable history.
//
// Blocks expose Boundary() (their byte offset after the last closed block) so
// a caller can re-parse only the grown tail.
//
// Styling is presentation-free: each span carries a Style token, and the
// rendering layer (internal/ui) maps tokens to colors and attributes. Block
// structure and inline markdown (emphasis, strong, inline code,
// strikethrough, links, image alt text, autolinks) both produce tokens, and
// Text node content is unescaped so entities and backslash escapes render as
// their literal characters. Every emitted line fits the width given to Render
// or NewStream (guarded by TestLinesFitWidth): prose, headings, code, and
// tables wrap, and block prefixes (list markers, quote gutters) are accounted
// for in the wrap width, so a caller painting a fixed-width slab never has to
// truncate text away. GFM tables render as aligned columns: columns size to
// their widest cell when the table fits, and a table too wide shrinks its
// widest columns and wraps text inside each cell, so the tabular shape holds at
// any width (a width below one cell per column falls back to wrapped rows).
// Adjacent blocks are separated by a blank line (see appendBlocks), giving headings, paragraphs, lists, tables, quotes, code,
// and rules room to breathe; the blank lines are output only and never
// re-enter the parsed source. Spans are emitted after wrapping, annotating
// whole display lines, so a span never crosses a line break. Links render as
// their label (styled as a link) followed by the destination in a faint URL
// style, so the target is visible and copy-pasteable; the label also carries
// the destination on the span (Styled.Link) so the rendering layer can make
// it clickable with an OSC 8 hyperlink. A link whose label already is its
// destination renders the destination once. Raw HTML renders as its literal
// text (a tag the model wrote shows as written).
package markdown

import "strings"

// Style identifies one visual role. The rendering layer maps these to actual
// colors and attributes, keeping this package free of presentation choices.
type Style uint8

const (
	StyleNone          Style = iota // no styling (spans are omitted for this)
	StyleText                       // ordinary prose
	StyleHeading                    // ATX and setext headings
	StyleFaint                      // rules and horizontal filler
	StyleCodeBlock                  // fenced and indented code block text
	StyleCodeInline                 // inline code spans
	StyleQuote                      // blockquote prose
	StyleQuoteMark                  // blockquote gutter bar
	StyleListBullet                 // bullet or ordered-list marker
	StyleEmph                       // emphasis
	StyleStrong                     // strong emphasis
	StyleLink                       // link text
	StyleLinkURL                    // link destination
	StyleStrikethrough              // ~~struck~~ text
	StyleTask                       // task-list checkbox
	StyleTableHeader                // table header row
	StyleTableRowAlt                // alternate (even) table body row
)

// Theme optionally remaps styles to other styles (for example, rendering
// emphasis as plain prose in a theme that reserves color for structure). The
// zero Theme is the identity mapping.
type Theme struct {
	Overrides map[Style]Style
}

// Resolve returns the style a role renders as.
func (t Theme) Resolve(s Style) Style {
	if t.Overrides != nil {
		if o, ok := t.Overrides[s]; ok {
			return o
		}
	}
	return s
}

// Styled is one styled region of a line. Link carries the hyperlink
// destination when the region is part of a link, so the rendering layer can
// make it clickable; it is empty for ordinary spans.
type Styled struct {
	Text  string
	Style Style
	Link  string
}

// Line is one display line with its styled regions. Plain lines carry no
// spans; callers must treat text as authoritative when spans are absent.
type Line struct {
	Text  string
	Spans []Styled
}

// Plain builds an unstyled line.
func Plain(s string) Line { return Line{Text: s} }

// oscStripper removes OSC 8 hyperlink sequences from a stream of deltas.
// It is the OSC counterpart of ui.ansiStripper (which also drops CSI and
// C0 controls): stateful across calls, so a sequence split between deltas
// is dropped whole. kon strips CSI/control bytes at the transcript boundary
// before text reaches this package, so only OSC 8 markers — which are
// legitimate markdown content from the model — can appear here.
type oscStripper struct {
	state uint8
}

const (
	oscGround = iota // plain text
	oscEsc           // saw ESC
	oscOSC           // inside OSC; consume through BEL or ST (ESC \)
	oscOSCESC        // inside OSC; saw the ESC of an ST terminator
)

// strip returns text with OSC 8 hyperlink sequences removed. The result may be
// short when text ends inside a sequence; the rest of that sequence is dropped
// when it arrives in a later delta.
func (s *oscStripper) strip(text string) string {
	if s.state == oscGround {
		i := strings.IndexByte(text, 0x1b)
		if i < 0 {
			return text
		}
	}
	var out strings.Builder
	out.Grow(len(text) + 16)
	for i := 0; i < len(text); i++ {
		b := text[i]
		switch s.state {
		case oscGround:
			switch b {
			case 0x1b:
				s.state = oscEsc
			default:
				out.WriteByte(b)
			}
		case oscEsc:
			switch b {
			case ']':
				s.state = oscOSC
			default:
				out.WriteByte(0x1b)
				out.WriteByte(b)
				s.state = oscGround
			}
		case oscOSC:
			switch b {
			case 0x07: // BEL terminator
				s.state = oscGround
			case 0x1b:
				s.state = oscOSCESC
			}
		case oscOSCESC:
			// ST is ESC \. Anything else after ESC inside an OSC is invalid;
			// consume the byte and resume ground either way.
			s.state = oscGround
		}
	}
	return out.String()
}
