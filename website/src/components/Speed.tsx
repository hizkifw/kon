import { SectionHeading } from "./SectionHeading";
import { StartupChart } from "./StartupChart";
import { startupMs } from "@/lib/site";

const REASONS = [
  {
    title: "Nothing runs that doesn't have to",
    body: "kon never checks for updates or refreshes its model catalog on launch. Storage migrations read one small version marker instead of scanning your sessions.",
  },
  {
    title: "Native code, no runtime to boot",
    body: "kon is compiled Go, statically linked. There is no interpreter to start, no JIT to warm up, and no dependency tree to resolve before the first frame.",
  },
  {
    title: "Snappy after startup too",
    body: "Replies stream as they arrive, and redraws are capped at 20 frames a second, so a fast model never outruns your terminal. 50,000 streamed deltas render in 0.67 s.",
  },
  {
    title: "Milliseconds are tracked",
    body: "A dependency was quietly building a Unicode table at init. Finding it cut startup from 50 ms to 21 ms, and the load test is there to catch the next one.",
  },
];

export function Speed() {
  return (
    <section id="speed" className="mx-auto max-w-6xl px-4 py-20 sm:px-6 lg:py-28">
      <div className="grid grid-cols-1 gap-12 lg:grid-cols-[minmax(0,1fr)_minmax(0,1.2fr)] lg:items-center">
        <SectionHeading eyebrow="Startup" title="Faster than you can notice.">
          <p>
            From launching the process to streaming back a finished reply, kon takes {startupMs} ms. That&rsquo;s
            about one frame on a 60 Hz screen, and a fraction of the 100 ms where a delay starts to register.
          </p>
          <p>No splash screen, no spinner, nothing to wait for. Type <Kbd>kon</Kbd> and it&rsquo;s there.</p>
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

      <div className="mt-8 flex flex-col gap-3 rounded-xl border border-dashed border-line px-5 py-4 text-sm sm:flex-row sm:items-center sm:gap-6">
        <span className="text-muted">Measure it yourself:</span>
        <code className="font-mono text-[13px] whitespace-nowrap text-fg">
          <span className="text-faint select-none">PS&gt; </span>Measure-Command {"{"} kon --version {"}"}
        </code>
        <code className="font-mono text-[13px] whitespace-nowrap text-fg">
          <span className="text-faint select-none">$ </span>time kon --version
        </code>
      </div>
    </section>
  );
}

function Kbd({ children }: { children: string }) {
  return (
    <kbd className="rounded border border-line bg-panel px-1.5 py-0.5 font-mono text-[0.85em] text-fg">{children}</kbd>
  );
}
