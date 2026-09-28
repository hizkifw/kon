import type { ReactNode } from "react";
import { Banner } from "./Banner";
import { Inline } from "./Inline";
import { SectionHeading } from "./SectionHeading";

const POINTS = [
  {
    title: "No paper trail",
    body: "The conversation lives in memory and dies when you quit. Your Up-arrow history never hears about it.",
  },
  {
    title: "Sunglasses on",
    body: "A faint, dashed banner, so you always know which kon is listening.",
  },
  {
    title: "Accomplices included",
    body: "Subagents go incognito too, and background jobs clean up after themselves.",
  },
  {
    title: "Throwaway scripts",
    body: "`kon run --incognito` sends one prompt and leaves nothing behind.",
  },
];

export function Incognito() {
  return (
    <section id="incognito" className="border-t border-line">
      <div className="mx-auto max-w-6xl px-4 py-20 sm:px-6 lg:py-28">
        <div className="grid grid-cols-1 gap-12 lg:grid-cols-[minmax(0,1fr)_minmax(0,1.05fr)] lg:items-center">
          {/* The text column dissolves into the grid on narrow screens, so the
              parting words follow the session instead of preceding it. */}
          <div className="contents lg:block">
            <SectionHeading eyebrow="Incognito" title="Leave no paw prints.">
              <p className="text-pretty">
                Some prompts aren&rsquo;t for the record. What a monad is, for the fifth time this week. Centering
                that div, again. Run <code className="font-mono whitespace-nowrap text-fg">kon --incognito</code> and
                the fox slips on its sunglasses: the session is never saved, and there&rsquo;s nothing to resume.
              </p>
            </SectionHeading>
            <div
              aria-label="After quitting, the shell shows: incognito session discarded; nothing to resume."
              className="order-last max-w-xl rounded-xl border border-dashed border-line px-5 py-4 font-mono text-[11px] leading-[1.5] sm:text-[13px] lg:mt-10"
            >
              <div aria-hidden className="whitespace-pre">
                <span className="text-link">~/code/site</span>
                <span className="text-faint"> $ </span>
                <span className="text-fg">kon --incognito</span>
                {"\n\n"}
                <span className="text-muted">incognito session discarded; nothing to resume</span>
              </div>
            </div>
          </div>
          <Window />
        </div>

        <dl className="mt-14 grid grid-cols-1 gap-x-8 gap-y-7 sm:grid-cols-2 lg:grid-cols-4">
          {POINTS.map((point) => (
            <div key={point.title} className="border-l-2 border-dashed border-faint/60 pl-4">
              <dt className="font-medium text-fg">
                <Inline text={point.title} />
              </dt>
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

// Window is a still frame of kon in an incognito session, drawn in kon's
// palette like the hero demo but with no script to run.
function Window() {
  return (
    <div
      role="img"
      aria-label="kon in an incognito session: a faint, dashed banner reading 'incognito · session not saved', and a user asking kon to center a div again and tell no one."
      className="overflow-hidden rounded-xl border border-line bg-screen shadow-2xl shadow-black/60"
    >
      <div className="relative flex h-10 items-center bg-[#1c1c1c] px-3.5 text-[12px]">
        <div className="flex gap-2" aria-hidden>
          <span className="size-3 rounded-full bg-[#ff5f57]" />
          <span className="size-3 rounded-full bg-[#febc2e]" />
          <span className="size-3 rounded-full bg-[#28c840]" />
        </div>
        <span className="pointer-events-none absolute inset-x-0 text-center text-faint">kon</span>
      </div>
      <div aria-hidden className="p-[0.6em] font-mono text-[11px] leading-[1.5] text-agent sm:text-[13px]">
        <Row className="bg-bar">
          <span className="font-bold text-accent">kon</span>
          <span className="text-bar-fg"> · qwen3.8-27b</span>
        </Row>
        <Blank />
        <div className="px-[0.6em] text-faint">
          <Banner incognito />
        </div>
        <Row className="text-faint">incognito · session not saved =˄⌐■▾■˄=</Row>
        <Blank />
        <Row className="bg-user text-user-fg">center this div. again. don&rsquo;t tell anyone</Row>
        <Blank />
        <div className="px-[0.6em]">
          Your secret&rsquo;s safe: this session won&rsquo;t be saved. Put{" "}
          <span className="whitespace-nowrap text-accent">display: grid</span> and{" "}
          <span className="whitespace-nowrap text-accent">place-items: center</span> on the parent.
        </div>
        <Blank />
        <Row className="bg-bar text-[#757575]">~/code/site · ctx 1.2k/400.0k · ready</Row>
        <div className="h-[1.5em] whitespace-pre">
          <span className="animate-blink bg-[#d8d8d8] text-screen">A</span>
          <span className="text-faint">sk kon…</span>
        </div>
      </div>
    </div>
  );
}

function Row({ className = "", children }: { className?: string; children: ReactNode }) {
  return <div className={`h-[1.5em] overflow-hidden px-[0.6em] whitespace-pre ${className}`}>{children}</div>;
}

function Blank() {
  return <div className="h-[1.5em]" />;
}
