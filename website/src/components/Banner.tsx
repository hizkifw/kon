// kon's welcome banner from internal/ui/banner.go, drawn as vector strokes.
// Web font subsets leave out box-drawing characters, and a fallback font would
// break the grid, so each glyph becomes up to four arms from its cell's center
// to its edges.
export const BANNER = [
  "┌──┐              ┌──┐",
  "│  ├──┬─────┬─────┤  │",
  "│  ┌─<│  _  │     ├──┤",
  "└──┴──┴─────┴──┴──┴──┘",
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

const PATH = bannerPath(BANNER);
const COLS = BANNER[0].length;

// Banner sizes itself in ems, so the parent's font size sets its scale.
// Small renderings need a heavier stroke to stay visible.
export function Banner({ className, strokeWidth = 1.1 }: { className?: string; strokeWidth?: number }) {
  return (
    <svg
      role="img"
      aria-label="kon"
      className={className}
      width={`${COLS * 0.6}em`}
      height={`${BANNER.length * 1.5}em`}
      viewBox={`0 0 ${COLS * CW} ${BANNER.length * CH}`}
      fill="none"
      stroke="currentColor"
      strokeWidth={strokeWidth}
      strokeLinecap="square"
    >
      <path d={PATH} />
    </svg>
  );
}
