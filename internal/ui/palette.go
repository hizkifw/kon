package ui

import "charm.land/lipgloss/v2"

// Transcript palette. The base is neutral grey: message slabs differ by
// lightness rather than hue. Green is reserved for success (colorOK); red for
// failure (colorFail); amber for a warning (colorWarn). lipgloss degrades these automatically on terminals with
// smaller color profiles.
var (
	// colorAccent is the muted red kon brand accent (header wordmark).
	colorAccent = lipgloss.Color("#C98A8A")

	colorUserBg = lipgloss.Color("#313131")
	colorUserFg = lipgloss.Color("#DEDEDE")

	// Agent messages render on the default terminal background: NoColor draws
	// no background at all, so only the user slab (and tool/error slabs) are
	// painted.
	colorAgentBg = lipgloss.NoColor{}
	colorAgentFg = lipgloss.Color("#EAEAEA")

	colorToolBg   = lipgloss.Color("#2B2B2B")
	colorToolFg   = lipgloss.Color("#909090")
	colorToolName = lipgloss.Color("#C9C9C9")
	colorToolNote = lipgloss.Color("#707070")

	colorErrorBg = lipgloss.Color("#5A2120")
	colorErrorFg = lipgloss.Color("#F2DCD8")

	colorFaint = lipgloss.Color("#757575")
	colorOK    = lipgloss.Color("#79C98B")
	colorRun   = colorFaint // in-flight tools stay quiet
	colorFail  = lipgloss.Color("#E06C6C")
	colorWarn  = lipgloss.Color("#D7B56D")

	// Header and status bar share one background so the top and bottom edges
	// read as a single frame.
	colorBarBg = lipgloss.Color("#1C1C1C")
	colorBarFg = lipgloss.Color("#C8C8C8")
)

// Markdown palette additions: colors used only by markdown roles.
var (
	colorHeadingFg = lipgloss.Color("#FFFFFF")
	// Code shares the kon brand accent so code and the wordmark read as one
	// family; the muted red is legible on both the default and tool slabs.
	colorCodeFg  = colorAccent
	colorQuoteFg = lipgloss.Color("#B0B0B0")
	colorLink    = lipgloss.Color("#7FB3D5")

	// Table tints: subtle neutral backgrounds so a table reads as a distinct
	// block without borders. The row band makes wide or wrapped tables easy to
	// follow across lines.
	colorTableHeaderBg = lipgloss.Color("#3A3A3A")
	colorTableRowBg    = lipgloss.Color("#262626")
)

// colorDim paints everything under the top drawer.
var colorDim = lipgloss.Color("#4A4A4A")

// selectedRowStyle marks the highlighted row of the popup and of a drawer's
// list.
var selectedRowStyle = lipgloss.NewStyle().Foreground(lipgloss.Color("#DADADA")).Background(lipgloss.Color("#333333"))
