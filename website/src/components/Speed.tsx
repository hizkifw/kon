import { SectionHeading } from "./SectionHeading";
import { StartupChart } from "./StartupChart";

const REASONS = [
  {
    title: "Nothing runs that doesn't have to",
    body: "No update check, no catalog refresh, and no scan of your sessions on launch.",
  },
  {
    title: "Native code, no runtime to boot",
    body: "Statically linked Go. No interpreter to start and no JIT to warm up.",
  },
  {
    title: "Snappy after startup too",
    body: "Replies stream as they arrive, and 50,000 streamed deltas render in 0.67 s.",
  },
  {
    title: "Long sessions stay fast",
    body: "Redrawing a frame takes 11 µs, however long the conversation gets.",
  },
];

export function Speed() {
  return (
    <section id="speed" className="mx-auto max-w-6xl px-4 py-20 sm:px-6 lg:py-28">
      <div className="grid grid-cols-1 gap-12 lg:grid-cols-[minmax(0,1fr)_minmax(0,1.2fr)] lg:items-center">
        <SectionHeading eyebrow="Startup" title="Faster than you can notice.">
          <p>
            No splash screen, no spinner. Type <Kbd>kon</Kbd> and it&rsquo;s there.
          </p>
        </SectionHeading>
        <StartupChart />
      </div>

      <div className="mt-16 grid grid-cols-1 gap-px overflow-hidden rounded-xl border border-line bg-line sm:grid-cols-2 lg:grid-cols-4">
        {REASONS.map((reason) => (
          <div key={reason.title} className="bg-screen p-6">
            <h3 className="font-medium text-fg">{reason.title}</h3>
            <p className="mt-2 text-sm leading-relaxed text-muted">{reason.body}</p>
          </div>
        ))}
      </div>
    </section>
  );
}

function Kbd({ children }: { children: string }) {
  return (
    <kbd className="rounded border border-line bg-panel px-1.5 py-0.5 font-mono text-[0.85em] text-fg">{children}</kbd>
  );
}
