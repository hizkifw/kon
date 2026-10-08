import type { ReactNode } from "react";
import { Inline } from "./Inline";
import { SectionHeading } from "./SectionHeading";

const SOURCE = [
  "# Release notes",
  "",
  "Tables **line up** now. Try `kon upgrade`.",
  "",
  "- Fixed the date parser",
  "- See the [guide](https://kon.kitsu.red)",
  "",
  "> Tag it before Friday.",
  "",
  "| Build | Size |",
  "| --- | --- |",
  "| Linux x64 | 11.6 MB |",
  "| Windows x64 | 12.0 MB |",
];

const POINTS = [
  {
    title: "Files or pipes",
    body: "`kon md README.md` reads a file. Name none, and it reads whatever you pipe in.",
  },
  {
    title: "Renders as it arrives",
    body: "Each block prints as soon as it closes, so streamed input doesn't wait for the end.",
  },
  {
    title: "Plain when redirected",
    body: "Color and links go only to a terminal. `kon md notes.md > out.txt` writes plain wrapped text.",
  },
  {
    title: "Wraps where you say",
    body: "80 columns by default. `--width` changes that, and `--width 0` leaves it to the terminal.",
  },
];

export function Markdown() {
  return (
    <section id="markdown" className="border-t border-line">
      <div className="mx-auto max-w-6xl px-4 py-20 sm:px-6 lg:py-28">
        <SectionHeading
          eyebrow="Markdown"
          title={
            <>
              Stop running <code className="font-mono text-[0.9em] whitespace-nowrap">cat README.md</code>.
            </>
          }
        >
          <p>
            Markdown was meant to be rendered, not squinted at. <code className="font-mono text-fg">kon md</code>{" "}
            draws it right in your terminal: headings, emphasis, lists, quotes, code, tables, and clickable links.
          </p>
        </SectionHeading>

        <div className="mt-14 grid grid-cols-1 gap-4 lg:grid-cols-2">
          <Source />
          <Rendered />
        </div>

        <dl className="mt-14 grid grid-cols-1 gap-x-8 gap-y-7 sm:grid-cols-2 lg:grid-cols-4">
          {POINTS.map((point) => (
            <div key={point.title} className="border-l-2 border-accent/50 pl-4">
              <dt className="font-medium text-fg">{point.title}</dt>
              <dd className="mt-1.5 text-sm leading-relaxed text-muted">
                <Inline text={point.body} />
              </dd>
            </div>
          ))}
        </dl>
      </div>
    </section>
  );
}

// Source is what `cat` prints for the file, so the terminal beside it has
// something to be compared with.
function Source() {
  return (
    <div
      role="img"
      aria-label="cat notes.md prints the file as written: a heading, a sentence with bold text and code, a list with a link, a quote, and a table, all in raw Markdown."
      className="overflow-hidden rounded-xl border border-line bg-screen"
    >
      <Chrome />
      <div aria-hidden className={`${SCREEN} text-muted`}>
        <Prompt command="cat notes.md" />
        {SOURCE.map((line, i) => (
          <Row key={i}>{line}</Row>
        ))}
      </div>
    </div>
  );
}

// Rendered is what `kon md notes.md` prints for Source, in the colors kon's
// renderer uses on a terminal.
function Rendered() {
  return (
    <div
      role="img"
      aria-label="The same file printed by kon md: a bold heading, styled text, bullets, a link followed by its address, a quote behind a bar, and an aligned table with a shaded header."
      className="overflow-hidden rounded-xl border border-line bg-screen shadow-2xl shadow-black/60"
    >
      <Chrome />
      <div aria-hidden className={`${SCREEN} text-agent`}>
        <Prompt command="kon md notes.md" />
        <Row className="font-bold text-white">Release notes</Row>
        <Row />
        <Row>
          Tables <span className="font-bold">line up</span> now. Try <span className="text-accent">kon upgrade</span>.
        </Row>
        <Row />
        <Row>
          <span className="text-bar-fg">•</span> Fixed the date parser
        </Row>
        <Row>
          <span className="text-bar-fg">•</span> See the <span className="text-link">guide</span>
          <span className="text-faint"> (https://kon.kitsu.red)</span>
        </Row>
        <Row />
        <Row>
          {/* The quote bar is a border, because the web font's subset has no
              block-drawing glyphs. */}
          <span className="inline-block w-[1.2em] border-l border-faint"> </span>
          Tag it before Friday.
        </Row>
        <Row />
        <Row>
          <span className="bg-[#3a3a3a] font-bold">{" Build        Size    "}</span>
        </Row>
        <Row>{" Linux x64    11.6 MB "}</Row>
        <Row>
          <span className="bg-[#262626]">{" Windows x64  12.0 MB "}</span>
        </Row>
      </div>
    </div>
  );
}

function Chrome() {
  return (
    <div className="relative flex h-10 items-center bg-[#1c1c1c] px-3.5 text-[12px]">
      <div className="flex gap-2" aria-hidden>
        <span className="size-3 rounded-full bg-[#ff5f57]" />
        <span className="size-3 rounded-full bg-[#febc2e]" />
        <span className="size-3 rounded-full bg-[#28c840]" />
      </div>
      <span className="pointer-events-none absolute inset-x-0 text-center text-faint">zsh</span>
    </div>
  );
}

function Prompt({ command }: { command: string }) {
  return (
    <Row>
      <span className="text-link">~/code/kon</span>
      <span className="text-faint"> $ </span>
      <span className="text-agent">{command}</span>
    </Row>
  );
}

const SCREEN = "overflow-x-auto p-[1.2em] font-mono text-[11px] leading-[1.5] sm:text-[13px]";

function Row({ className = "", children }: { className?: string; children?: ReactNode }) {
  return <div className={`h-[1.5em] whitespace-pre ${className}`}>{children}</div>;
}
