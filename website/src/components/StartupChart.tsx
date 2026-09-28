import { Fragment } from "react";
import { startupMachine, startupScale } from "@/lib/site";

const MAX_MS = 400;
const TICKS = [0, 100, 200, 300, 400];

// StartupChart sets kon's startup beside timings people already have a feel
// for. Only kon's bar takes the accent; the rest are neutral references. Every
// bar carries its label and value as text, so it reads without color and works
// as its own table.
export function StartupChart() {
  return (
    <figure className="rounded-xl border border-line bg-panel p-5 sm:p-7">
      <figcaption>
        <span className="block font-medium text-fg">Startup, beside things you can feel</span>
        <span className="mt-1 block text-sm text-muted">Milliseconds; shorter is faster</span>
      </figcaption>
      <dl className="mt-6 grid grid-cols-1 gap-x-5 sm:grid-cols-[14.5rem_minmax(0,1fr)]">
        {startupScale.map((row) => (
          <Fragment key={row.label}>
            <dt className={`pt-3 text-sm sm:flex sm:items-center sm:justify-end sm:pt-0 sm:text-right ${row.kon ? "font-medium text-fg" : "text-muted"}`}>
              {row.label}
            </dt>
            <dd
              tabIndex={0}
              className="group relative flex h-11 items-center border-r border-line bg-[linear-gradient(to_right,var(--color-line)_1px,transparent_1px)] bg-[length:25%_100%] outline-none focus-visible:ring-1 focus-visible:ring-accent"
            >
              <span
                className="h-3 rounded-r-[4px]"
                style={{
                  width: `${(row.ms / MAX_MS) * 100}%`,
                  background: row.kon ? "var(--color-accent)" : "#666666",
                }}
              />
              <span className={`ml-2 font-mono text-sm ${row.kon ? "text-fg" : "text-muted"}`}>{row.value}</span>
              <span
                role="tooltip"
                className="pointer-events-none absolute bottom-full left-0 z-10 mb-1 hidden rounded-md border border-line bg-[#1a1a1a] px-2.5 py-1.5 text-xs whitespace-nowrap text-muted shadow-lg group-hover:block group-focus-visible:block"
              >
                <span className="text-fg">{row.value}</span> · {row.source}
              </span>
            </dd>
          </Fragment>
        ))}
        <div aria-hidden className="hidden sm:block" />
        <div aria-hidden className="relative mt-2 h-4 font-mono text-[11px] text-faint">
          {TICKS.map((ms) => (
            <span
              key={ms}
              className="absolute top-0 -translate-x-1/2 first:translate-x-0 last:-translate-x-full"
              style={{ left: `${(ms / MAX_MS) * 100}%` }}
            >
              {ms}
            </span>
          ))}
        </div>
      </dl>
      <p className="mt-6 border-t border-line pt-4 text-xs leading-relaxed text-faint">
        kon&rsquo;s time is the median of 50 launches in <code className="font-mono">make loadtest</code>, on{" "}
        {startupMachine}.
      </p>
    </figure>
  );
}
