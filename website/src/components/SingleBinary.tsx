import { Inline } from "./Inline";
import { SectionHeading } from "./SectionHeading";
import { binaries, binaryMb, downloadMb } from "@/lib/site";

const INSIDE = [
  "The agent and its full-screen terminal UI",
  "Four tools: `read`, `write`, `edit`, and `shell`",
  "Backends for OpenAI, Anthropic, OpenRouter, Ollama, and any OpenAI-compatible server",
  "A models.dev catalog snapshot, so `kon models` works offline",
  "The user guide, which `kon docs` unpacks as plain Markdown",
  "A self-updater that verifies every download: `kon upgrade`",
];

const NOT_NEEDED = [
  "Node.js, Python, or any other runtime",
  "A package manager or a global dependency tree",
  "A background daemon or service",
  "WSL, Cygwin, or MSYS on Windows",
  "An account, or a network call at startup",
];

export function SingleBinary() {
  return (
    <section id="binary" className="border-t border-line">
      <div className="mx-auto max-w-6xl px-4 py-20 sm:px-6 lg:py-28">
        <div className="grid grid-cols-1 gap-12 lg:grid-cols-[minmax(0,1fr)_minmax(0,1fr)] lg:items-end">
          <SectionHeading eyebrow="Single binary" title="One file is the whole install.">
            <p>
              kon is one statically linked executable: about {binaryMb} MB on disk and {downloadMb} MB to download.
              The installer checks its SHA-256, puts it on your PATH, and that&rsquo;s it. There is nothing else to
              install, keep running, or keep in sync.
            </p>
          </SectionHeading>
          <ul className="grid grid-cols-1 gap-3">
            {binaries.map((bin) => (
              <li
                key={bin.platform}
                className="flex items-center gap-4 rounded-lg border border-line bg-panel px-4 py-3"
              >
                <FileIcon />
                <span className="font-mono text-sm text-fg">{bin.file}</span>
                <span className="text-sm text-muted">{bin.platform}</span>
                <span className="ml-auto font-mono text-sm text-fg tabular-nums">{bin.mb.toFixed(1)} MB</span>
              </li>
            ))}
          </ul>
        </div>

        <div className="mt-14 grid grid-cols-1 gap-4 md:grid-cols-2">
          <List title="In the binary" mark="✓" markClass="text-ok" items={INSIDE} />
          <List title="Not required" mark="–" markClass="text-faint" items={NOT_NEEDED} />
        </div>
      </div>
    </section>
  );
}

function List({ title, mark, markClass, items }: { title: string; mark: string; markClass: string; items: string[] }) {
  return (
    <div className="rounded-xl border border-line p-6">
      <h3 className="font-medium text-fg">{title}</h3>
      <ul className="mt-4 space-y-2.5 text-sm leading-relaxed text-muted">
        {items.map((item) => (
          <li key={item} className="flex gap-3">
            <span aria-hidden className={`w-3 shrink-0 ${markClass}`}>
              {mark}
            </span>
            <span>
              <Inline text={item} />
            </span>
          </li>
        ))}
      </ul>
    </div>
  );
}

function FileIcon() {
  return (
    <svg viewBox="0 0 20 24" width="18" height="22" aria-hidden className="shrink-0 text-accent">
      <path d="M2 1h11l5 5v17H2z" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinejoin="round" />
      <path d="M13 1v5h5" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinejoin="round" />
    </svg>
  );
}
