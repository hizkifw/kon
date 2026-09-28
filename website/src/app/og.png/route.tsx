import { readFile } from "node:fs/promises";
import { join } from "node:path";
import type { ReactNode } from "react";
import { ImageResponse } from "next/og";
import { BANNER, bannerPath } from "@/components/Banner";
import { ogImage, siteUrl, startupMs } from "@/lib/site";

// Static export has no server, so the image is rendered into out/og.png at
// build time.
export const dynamic = "force-static";

// ImageResponse cannot read the WOFF2 files next/font serves, so it takes the
// same faces from Fontsource's WOFF builds, Latin subset only.
const fontDir = join(process.cwd(), "node_modules/@fontsource");
const [geist, mono, monoBold] = await Promise.all([
  readFile(join(fontDir, "geist-sans/files/geist-sans-latin-600-normal.woff")),
  readFile(join(fontDir, "jetbrains-mono/files/jetbrains-mono-latin-400-normal.woff")),
  readFile(join(fontDir, "jetbrains-mono/files/jetbrains-mono-latin-700-normal.woff")),
]);

// The page palette from globals.css and kon's own from internal/ui/transcript.go;
// ImageResponse cannot read CSS variables.
const c = {
  page: "#080808",
  screen: "#0c0c0c",
  chrome: "#1f1f1f",
  border: "#2e2e2e",
  fg: "#eaeaea",
  muted: "#a8a8a8",
  faint: "#757575",
  accent: "#c98a8a",
  ok: "#79c98b",
  bar: "#1c1c1c",
  barFg: "#c8c8c8",
  user: "#313131",
  userFg: "#dedede",
  tool: "#2b2b2b",
  toolName: "#c9c9c9",
  toolFg: "#909090",
  toolNote: "#707070",
};

// The terminal's cell: monospace glyphs advance 0.6em and rows are 1.5em, the
// same grid the site's terminal and the banner's path use.
const FONT = 19;
const CW = FONT * 0.6;
const LH = FONT * 1.5;

export function GET() {
  return new ImageResponse(
    (
      <div
        style={{
          display: "flex",
          flexDirection: "column",
          alignItems: "center",
          width: "100%",
          height: "100%",
          paddingTop: 50,
          background: c.page,
          backgroundImage: "radial-gradient(circle at 50% 120%, rgba(201,138,138,0.22), rgba(8,8,8,0) 62%)",
          fontFamily: "Geist",
          color: c.fg,
        }}
      >
        <div style={{ display: "flex", fontSize: 54, letterSpacing: -1.6, lineHeight: 1.1 }}>
          <span>{`Starts in ${startupMs} ms.`}</span>
          <span style={{ marginLeft: 15, color: c.muted }}>One binary. Every OS.</span>
        </div>
        <div style={{ display: "flex", marginTop: 14, fontFamily: "JetBrains Mono", fontSize: 21 }}>
          <span style={{ color: c.muted }}>A coding agent for your terminal</span>
          <span style={{ color: c.faint, whiteSpace: "pre" }}>{"  ·  "}</span>
          <span style={{ color: c.accent }}>{new URL(siteUrl).host}</span>
        </div>

        {/* The window rises off the bottom edge, so it reads as a real
            screen rather than a framed thumbnail. */}
        <div
          style={{
            display: "flex",
            flexDirection: "column",
            flex: 1,
            width: 1040,
            marginTop: 36,
            overflow: "hidden",
            background: c.screen,
            borderTop: `1px solid ${c.border}`,
            borderLeft: `1px solid ${c.border}`,
            borderRight: `1px solid ${c.border}`,
            borderTopLeftRadius: 14,
            borderTopRightRadius: 14,
            boxShadow: "0 0 80px rgba(201,138,138,0.14)",
          }}
        >
          <WindowsTerminalChrome />
          <div style={{ display: "flex", flexDirection: "column", padding: CW, fontFamily: "JetBrains Mono", fontSize: FONT }}>
            <Row bg={c.bar}>
              <span style={{ color: c.accent, fontWeight: 700 }}>kon</span>
              <span style={{ color: c.barFg, whiteSpace: "pre" }}>{" · qwen3.8-27b"}</span>
            </Row>
            <Row />
            <div style={{ display: "flex", paddingLeft: CW }}>
              <svg
                width={BANNER[0].length * CW}
                height={BANNER.length * LH}
                viewBox={`0 0 ${BANNER[0].length * 6} ${BANNER.length * 15}`}
                fill="none"
                stroke={c.accent}
                strokeWidth={1.1}
                strokeLinecap="square"
              >
                <path d={bannerPath(BANNER)} />
              </svg>
            </div>
            <Row>
              <span style={{ color: c.faint, whiteSpace: "pre" }}>harness for foxes =</span>
              <FoxFace />
              <span style={{ color: c.faint }}>=</span>
            </Row>
            <Row />
            <Row bg={c.user}>
              <span style={{ color: c.userFg }}>the date tests fail, fix them</span>
            </Row>
            <Row />
            <Tool name="read" summary="src/dates.ts" note="88 lines" />
            <Tool name="edit" summary="src/dates.ts" note="-2 +3 lines" />
            <Tool name="shell" summary="npm test" />
            <Row bg={c.tool}>
              <span style={{ color: c.toolNote, whiteSpace: "pre" }}>{"  Tests  42 passed (42)"}</span>
            </Row>
            <Row bg={c.tool}>
              <span style={{ color: c.ok, whiteSpace: "pre" }}>{"  exit 0 · took 1.4s"}</span>
            </Row>
          </div>
        </div>
      </div>
    ),
    {
      width: ogImage.width,
      height: ogImage.height,
      fonts: [
        { name: "Geist", data: geist, weight: 600, style: "normal" },
        { name: "JetBrains Mono", data: mono, weight: 400, style: "normal" },
        { name: "JetBrains Mono", data: monoBold, weight: 700, style: "normal" },
      ],
    },
  );
}

// Row is one terminal line; slabs fill the width and inset their text a cell.
// Satori calls toString on any background it is given, so a plain row must
// leave the key out rather than pass undefined.
function Row({ bg, children }: { bg?: string; children?: ReactNode }) {
  return (
    <div style={{ display: "flex", alignItems: "center", height: LH, paddingLeft: CW, ...(bg && { background: bg }) }}>
      {children}
    </div>
  );
}

function Tool({ name, summary, note }: { name: string; summary: string; note?: string }) {
  return (
    <Row bg={c.tool}>
      <Check />
      <span style={{ color: c.toolName, fontWeight: 700, whiteSpace: "pre" }}>{` ${name.padEnd(6)}`}</span>
      <span style={{ color: c.toolFg, whiteSpace: "pre" }}>{` ${summary}`}</span>
      {note && <span style={{ color: c.toolNote, whiteSpace: "pre" }}>{` · ${note}`}</span>}
    </Row>
  );
}

// The font subset leaves out ✓, ˄, and ▾, so they are drawn one cell wide to
// keep the grid, as on the site.
function Check() {
  return (
    <svg width={CW} height={LH} viewBox="0 0 6 15" fill="none" stroke={c.ok}>
      <path d="M1.1 7.9 2.5 9.4 5 5.9" strokeWidth={0.9} strokeLinecap="round" strokeLinejoin="round" />
    </svg>
  );
}

function FoxFace() {
  return (
    <svg width={CW * 3} height={LH} viewBox="0 0 18 15" fill="none" stroke={c.faint}>
      <path d="M1.4 7.6 3 5.2 4.6 7.6M13.4 7.6 15 5.2 16.6 7.6" strokeWidth={0.8} strokeLinejoin="round" />
      <path d="M7.4 6.4h3.2L9 8.8z" fill={c.faint} stroke="none" />
    </svg>
  );
}

// Windows Terminal's tab strip, as on the site for Windows visitors: kon's
// tab, the new-tab buttons, and the window controls.
function WindowsTerminalChrome() {
  return (
    <div style={{ display: "flex", alignItems: "stretch", height: 46, background: c.chrome, color: "#d6d6d6" }}>
      <div
        style={{
          display: "flex",
          alignItems: "center",
          width: 190,
          marginTop: 8,
          marginLeft: 8,
          paddingLeft: 14,
          paddingRight: 14,
          background: c.screen,
          borderTopLeftRadius: 8,
          borderTopRightRadius: 8,
          fontSize: 15,
        }}
      >
        <svg viewBox="0 0 16 16" width="16" height="16">
          <rect width="16" height="16" rx="3" fill="#2c5a9e" />
          <path d="M4 5l3 3-3 3M8 11h4" stroke="#fff" strokeWidth="1.4" fill="none" strokeLinecap="round" />
        </svg>
        <span style={{ marginLeft: 10 }}>kon</span>
        <div style={{ display: "flex", marginLeft: "auto" }}>
          <svg viewBox="0 0 10 10" width="9" height="9">
            <path d="M1 1l8 8M9 1l-8 8" stroke={c.faint} strokeWidth="1.2" />
          </svg>
        </div>
      </div>
      <div style={{ display: "flex", alignItems: "center", paddingLeft: 16, color: c.faint, fontSize: 20 }}>
        <span>+</span>
        <div style={{ display: "flex", marginLeft: 14 }}>
          <svg viewBox="0 0 10 6" width="10" height="6">
            <path d="M1 1l4 4 4-4" stroke={c.faint} strokeWidth="1.2" fill="none" />
          </svg>
        </div>
      </div>
      <div style={{ display: "flex", flex: 1 }} />
      {[
        <path key="min" d="M0 5h10" stroke="#bdbdbd" />,
        <rect key="max" x=".5" y=".5" width="9" height="9" stroke="#bdbdbd" fill="none" />,
        <path key="close" d="M0 0l10 10M10 0 0 10" stroke="#bdbdbd" />,
      ].map((icon) => (
        <div key={icon.key} style={{ display: "flex", alignItems: "center", justifyContent: "center", width: 52 }}>
          <svg viewBox="0 0 10 10" width="11" height="11">
            {icon}
          </svg>
        </div>
      ))}
    </div>
  );
}
