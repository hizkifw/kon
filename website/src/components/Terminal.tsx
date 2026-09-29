"use client";

import { Fragment, useEffect, useRef, useState, useSyncExternalStore, type ReactNode } from "react";
import { Banner } from "./Banner";

export type Platform = "windows" | "unix";

const TASK = "the date tests fail, fix them";
const REPLY =
  "Fixed. parseDate read two-digit years as 19xx; it now pivots at 50 as the spec says, and all 42 tests pass.";
const WORDS = REPLY.split(" ");
const CODE = new Set(["parseDate"]);

const TOOLS = [
  { name: "read", summary: "src/dates.ts", note: "88 lines" },
  { name: "edit", summary: "src/dates.ts · -2 +3 lines" },
  { name: "shell", summary: "npm test" },
];

// Milliseconds from the start of the demo. kon's first frame lands the moment
// Enter goes down, with no loading state in between: the real startup is too
// short to show as a delay.
const T = {
  typeKon: 600,
  launch: 1300,
  typeTask: 2300,
  send: 3600,
  tools: [
    [4000, 4350],
    [4500, 4900],
    [5050, 6450],
  ],
  reply: 6650,
};
const KEY_MS = 110;
const TASK_KEY_MS = 38;
const WORD_MS = 45;
const END = T.reply + WORDS.length * WORD_MS + 250;
const WORKED_S = Math.round((END - T.send) / 1000);

// typed counts the characters (or words) of an n-long run that are visible
// at time t, one per step starting at start.
function typed(t: number, start: number, step: number, n: number): number {
  return t < start ? 0 : Math.min(n, Math.floor((t - start) / step) + 1);
}

function contextUsed(t: number): string {
  const steps: [number, string][] = [
    [T.send, "1.9k"],
    [T.tools[0][1], "3.2k"],
    [T.tools[1][1], "3.9k"],
    [T.tools[2][1], "4.6k"],
    [END, "4.8k"],
  ];
  let used = "0";
  for (const [at, label] of steps) if (t >= at) used = label;
  return `ctx ${used}/400.0k`;
}

const MOTION = "(prefers-reduced-motion: reduce)";

function subscribeMotion(onChange: () => void) {
  const query = matchMedia(MOTION);
  query.addEventListener("change", onChange);
  return () => query.removeEventListener("change", onChange);
}

export function Terminal({ platform }: { platform: Platform }) {
  const reduced = useSyncExternalStore(
    subscribeMotion,
    () => matchMedia(MOTION).matches,
    () => false,
  );
  const [run, setRun] = useState(0);
  const [clock, setClock] = useState(0);
  const t = reduced ? END : clock;

  useEffect(() => {
    if (reduced) return;
    const began = performance.now();
    const id = setInterval(() => {
      const now = performance.now() - began;
      setClock(Math.min(now, END));
      if (now >= END) clearInterval(id);
    }, 30);
    return () => clearInterval(id);
  }, [reduced, run]);

  // kon keeps the newest line in view, jumping to it as a terminal does.
  const scroller = useRef<HTMLDivElement>(null);
  useEffect(() => {
    const el = scroller.current;
    if (el) el.scrollTop = el.scrollHeight;
  });

  const launched = t >= T.launch;
  const sent = t >= T.send;
  const done = t >= END;

  return (
    <div className="relative">
      <div
        aria-hidden
        className="absolute -inset-8 -z-10 rounded-[2rem] bg-[radial-gradient(closest-side,rgba(201,138,138,0.16),transparent)] blur-2xl"
      />
      <figure
        aria-label={`A scripted demo: kon opens instantly in ${platform === "windows" ? "PowerShell" : "a Unix shell"}, reads a file, edits it, and runs the tests.`}
        className="overflow-hidden rounded-xl border border-line bg-screen shadow-2xl shadow-black/60"
      >
        <Chrome
          platform={platform}
          title={launched ? "kon" : platform === "windows" ? "PowerShell" : "zsh"}
        >
          {done && !reduced && (
            <button
              type="button"
              onClick={() => {
                setClock(0);
                setRun((n) => n + 1);
              }}
              className="rounded px-2 py-0.5 text-[12px] text-muted transition-colors hover:bg-white/5 hover:text-fg"
            >
              Replay
            </button>
          )}
        </Chrome>
        <div aria-hidden className="h-[30em] p-[0.6em] font-mono text-[11px] leading-[1.5] text-agent sm:text-[13px]">
          {launched ? (
            <div className="flex h-full flex-col">
              <Row className="bg-bar">
                <span className="font-bold text-accent">kon</span>
                <span className="text-bar-fg"> · qwen3.8-27b</span>
              </Row>
              <div ref={scroller} className="min-h-0 flex-1 overflow-hidden">
                <Blank />
                <div className="px-[0.6em] text-accent">
                  <Banner />
                </div>
                <Row className="text-faint">harness for foxes =˄▾˄=</Row>
                <Blank />
                {sent && (
                  <>
                    <Row className="bg-user text-user-fg">{TASK}</Row>
                    <Blank />
                  </>
                )}
                <Tools t={t} />
                {t >= T.reply && (
                  <>
                    <div className="px-[0.6em]">
                      {WORDS.slice(0, typed(t, T.reply, WORD_MS, WORDS.length)).map((word, i) => (
                        <Fragment key={i}>
                          {i > 0 && " "}
                          <span className={CODE.has(word) ? "text-accent" : undefined}>{word}</span>
                        </Fragment>
                      ))}
                    </div>
                    <Blank />
                  </>
                )}
                {sent && <Progress t={t} done={done} />}
              </div>
              <Row className="bg-bar text-[#757575]">
                ~/code/dates · {contextUsed(t)} · {sent && !done ? "working" : "ready"}
              </Row>
              <div className="h-[1.5em] px-[0.6em] whitespace-pre">
                {t >= T.typeTask && !sent ? (
                  <>
                    {TASK.slice(0, typed(t, T.typeTask, TASK_KEY_MS, TASK.length))}
                    <Cursor />
                  </>
                ) : (
                  <>
                    <Cursor char="A" blink />
                    <span className="text-faint">sk kon…</span>
                  </>
                )}
              </div>
            </div>
          ) : (
            <div className="whitespace-pre text-[#cccccc]">
              {platform === "windows" ? (
                <span>PS C:\code\dates&gt; </span>
              ) : (
                <>
                  <span className="text-link">~/code/dates</span>
                  <span className="text-faint"> $ </span>
                </>
              )}
              {"kon".slice(0, typed(t, T.typeKon, KEY_MS, 3))}
              <Cursor blink={t < T.typeKon} />
            </div>
          )}
        </div>
      </figure>
    </div>
  );
}

function Tools({ t }: { t: number }) {
  const shown = TOOLS.filter((_, i) => t >= T.tools[i][0]);
  if (shown.length === 0) return null;
  return (
    <>
      {shown.map((tool, i) => {
        const [start, end] = T.tools[i];
        const finished = t >= end;
        return (
          <Fragment key={tool.name}>
            <Row className="bg-tool">
              <Glyph kind={finished ? "check" : "dot"} className={finished ? "text-ok" : "text-faint"} />
              <span className="font-bold text-tool-name"> {tool.name.padEnd(6)}</span>
              <span className="text-tool-fg"> {tool.summary}</span>
              {finished && tool.note && <span className="text-tool-note"> · {tool.note}</span>}
            </Row>
            {tool.name === "shell" &&
              (finished ? (
                <>
                  <Row className="bg-tool text-tool-note">{"  Tests  42 passed (42)"}</Row>
                  <Row className="bg-tool text-ok">{`  exit 0 · took ${((end - start) / 1000).toFixed(1)}s`}</Row>
                </>
              ) : (
                <Row className="bg-tool text-tool-note">{`  ${((t - start) / 1000).toFixed(1)}s / 2m0s`}</Row>
              ))}
          </Fragment>
        );
      })}
      <Blank />
    </>
  );
}

function Progress({ t, done }: { t: number; done: boolean }) {
  const seconds = Math.floor((t - T.send) / 1000);
  return (
    <Row className="text-faint">
      <Glyph kind={!done && seconds % 2 === 1 ? "ring" : "dot"} />
      {done ? ` Worked for ${WORKED_S}s` : ` Working… ${seconds}s`}
    </Row>
  );
}

// Row is one terminal line. Slabs such as the header and tool calls fill the
// whole width, and their text sits one cell in, as in kon.
function Row({ className = "", children }: { className?: string; children: ReactNode }) {
  return <div className={`h-[1.5em] overflow-hidden px-[0.6em] whitespace-pre ${className}`}>{children}</div>;
}

function Blank() {
  return <div className="h-[1.5em]" />;
}

function Cursor({ char = " ", blink = false }: { char?: string; blink?: boolean }) {
  return <span className={`bg-[#d8d8d8] text-screen ${blink ? "animate-blink" : ""}`}>{char}</span>;
}

// Status glyphs are drawn rather than typed, because the web font's subset
// leaves them out and a fallback glyph would not keep the cell width.
function Glyph({ kind, className = "" }: { kind: "check" | "dot" | "ring"; className?: string }) {
  return (
    <svg
      viewBox="0 0 6 10"
      width="0.6em"
      height="1em"
      className={`inline-block align-[-0.2em] ${className}`}
      fill="none"
      stroke="currentColor"
    >
      {kind === "check" && <path d="M1.1 5.3 2.5 6.8 5 3.3" strokeWidth={0.9} strokeLinecap="round" strokeLinejoin="round" />}
      {kind === "dot" && <circle cx={3} cy={5} r={1.5} fill="currentColor" stroke="none" />}
      {kind === "ring" && <circle cx={3} cy={5} r={1.3} strokeWidth={0.6} />}
    </svg>
  );
}

// Chrome is the terminal window around kon: Windows Terminal on Windows, and a
// macOS-style title bar everywhere else, so the demo matches the install tab.
function Chrome({ platform, title, children }: { platform: Platform; title: string; children: ReactNode }) {
  if (platform === "windows") {
    return (
      <div className="flex h-10 items-stretch bg-[#1f1f1f] text-[12px] text-[#d6d6d6]">
        <div className="mt-1.5 ml-1.5 flex items-center gap-2 rounded-t-md bg-screen px-3 sm:min-w-[10rem]">
          <svg viewBox="0 0 16 16" width="14" height="14" aria-hidden>
            <rect width="16" height="16" rx="3" fill="#2c5a9e" />
            <path d="M4 5l3 3-3 3M8 11h4" stroke="#fff" strokeWidth="1.4" fill="none" strokeLinecap="round" />
          </svg>
          <span>{title}</span>
          <svg viewBox="0 0 10 10" width="8" height="8" className="ml-auto hidden text-faint sm:block" aria-hidden>
            <path d="M1 1l8 8M9 1l-8 8" stroke="currentColor" strokeWidth="1.2" />
          </svg>
        </div>
        <div className="hidden items-center gap-3 px-3 text-faint sm:flex" aria-hidden>
          <span className="text-base leading-none">+</span>
          <svg viewBox="0 0 10 6" width="9" height="6">
            <path d="M1 1l4 4 4-4" stroke="currentColor" strokeWidth="1.2" fill="none" />
          </svg>
        </div>
        <div className="ml-auto flex items-center gap-2 px-2">{children}</div>
        <div className="flex text-[#bdbdbd]" aria-hidden>
          <span className="hidden w-11 items-center justify-center sm:flex">
            <svg viewBox="0 0 10 10" width="10" height="10">
              <path d="M0 5h10" stroke="currentColor" />
            </svg>
          </span>
          <span className="hidden w-11 items-center justify-center sm:flex">
            <svg viewBox="0 0 10 10" width="10" height="10">
              <rect x=".5" y=".5" width="9" height="9" stroke="currentColor" fill="none" />
            </svg>
          </span>
          <span className="flex w-11 items-center justify-center">
            <svg viewBox="0 0 10 10" width="10" height="10">
              <path d="M0 0l10 10M10 0 0 10" stroke="currentColor" />
            </svg>
          </span>
        </div>
      </div>
    );
  }
  return (
    <div className="relative flex h-10 items-center bg-[#1c1c1c] px-3.5 text-[12px]">
      <div className="flex gap-2" aria-hidden>
        <span className="size-3 rounded-full bg-[#ff5f57]" />
        <span className="size-3 rounded-full bg-[#febc2e]" />
        <span className="size-3 rounded-full bg-[#28c840]" />
      </div>
      <span className="pointer-events-none absolute inset-x-0 text-center text-faint">{title}</span>
      <div className="relative ml-auto flex items-center gap-2">{children}</div>
    </div>
  );
}
