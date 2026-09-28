// kon's banners from internal/ui/banner.go, drawn as vector strokes.
// Web font subsets leave out box-drawing characters, and a fallback font would
// break the grid, so each glyph becomes up to four arms from its cell's center
// to its edges.
export const BANNER = [
  "┌──┐              ┌──┐",
  "│  ├──┬─────┬─────┤  │",
  "│  ┌─<│  _  │     ├──┤",
  "└──┴──┴─────┴──┴──┴──┘",
];

// The same mark as kon draws it in an incognito session, its long runs dashed.
export const INCOGNITO_BANNER = [
  "┌╌╌┐              ┌╌╌┐",
  "╎  ├╌╌┬╌╌╌╌╌┬╌╌╌╌╌┤  ╎",
  "╎  ┌╌<╎  _  ╎     ├╌╌┤",
  "└╌╌┴╌╌┴╌╌╌╌╌┴╌╌┴╌╌┴╌╌┘",
];

type Arm = "u" | "d" | "l" | "r";

const ARMS: Record<string, Arm[]> = {
  "┌": ["r", "d"],
  "┐": ["l", "d"],
  "└": ["u", "r"],
  "┘": ["u", "l"],
  "─": ["l", "r"],
  "│": ["u", "d"],
  "├": ["u", "d", "r"],
  "┤": ["u", "d", "l"],
  "┬": ["l", "r", "d"],
  "┴": ["l", "r", "u"],
};

// A terminal cell in tenths of an em: monospace glyphs advance 0.6em, and the
// terminal mock uses a 1.5 line height, so the banner lines up with its text.
const CW = 6;
const CH = 15;

export function bannerPath(rows: readonly string[]): string {
  const d: string[] = [];
  rows.forEach((line, row) => {
    [...line].forEach((glyph, col) => {
      const x = col * CW;
      const y = row * CH;
      const cx = x + CW / 2;
      const cy = y + CH / 2;
      for (const arm of ARMS[glyph] ?? []) {
        if (arm === "l") d.push(`M${x} ${cy}H${cx}`);
        if (arm === "r") d.push(`M${cx} ${cy}H${x + CW}`);
        if (arm === "u") d.push(`M${cx} ${y}V${cy}`);
        if (arm === "d") d.push(`M${cx} ${cy}V${y + CH}`);
      }
      if (glyph === "<") {
        const h = CH * 0.16;
        d.push(`M${x + CW * 0.78} ${cy - h}L${x + CW * 0.22} ${cy}L${x + CW * 0.78} ${cy + h}`);
      }
      if (glyph === "_") {
        d.push(`M${x + CW * 0.08} ${y + CH * 0.74}H${x + CW * 0.92}`);
      }
    });
  });
  return d.join("");
}

// dashPath draws the dashed glyphs, two dashes to a cell as a terminal draws
// them. They take butt caps, because square caps would close the gaps.
export function dashPath(rows: readonly string[]): string {
  const d: string[] = [];
  rows.forEach((line, row) => {
    [...line].forEach((glyph, col) => {
      const x = col * CW;
      const y = row * CH;
      for (const [from, to] of [
        [1 / 8, 3 / 8],
        [5 / 8, 7 / 8],
      ]) {
        if (glyph === "╌") d.push(`M${x + CW * from} ${y + CH / 2}H${x + CW * to}`);
        if (glyph === "╎") d.push(`M${x + CW / 2} ${y + CH * from}V${y + CH * to}`);
      }
    });
  });
  return d.join("");
}

// Both marks are drawn once, not on every frame of the terminal demo.
const MARKS = {
  welcome: { solid: bannerPath(BANNER), dashed: "" },
  incognito: { solid: bannerPath(INCOGNITO_BANNER), dashed: dashPath(INCOGNITO_BANNER) },
};
const COLS = BANNER[0].length;

// Banner sizes itself in ems, so the parent's font size sets its scale.
// Small renderings need a heavier stroke to stay visible.
export function Banner({
  className,
  strokeWidth = 1.1,
  incognito = false,
}: {
  className?: string;
  strokeWidth?: number;
  incognito?: boolean;
}) {
  const mark = incognito ? MARKS.incognito : MARKS.welcome;
  return (
    <svg
      role="img"
      aria-label={incognito ? "kon, incognito" : "kon"}
      className={className}
      width={`${COLS * 0.6}em`}
      height={`${BANNER.length * 1.5}em`}
      viewBox={`0 0 ${COLS * CW} ${BANNER.length * CH}`}
      fill="none"
      stroke="currentColor"
      strokeWidth={strokeWidth}
      strokeLinecap="square"
    >
      <path d={mark.solid} />
      {mark.dashed && <path d={mark.dashed} strokeLinecap="butt" />}
    </svg>
  );
}
