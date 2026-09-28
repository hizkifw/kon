import { binaryMb, downloadMb, nativeBuilds, startupMs, startupRssMb } from "@/lib/site";

const STATS = [
  { label: "Startup", value: `${startupMs} ms`, detail: "launch to first frame" },
  { label: "Binary", value: `${binaryMb} MB`, detail: `about ${downloadMb} MB to download` },
  { label: "Memory at startup", value: `${startupRssMb} MB`, detail: "peak resident set at first frame" },
  { label: "Native builds", value: `${nativeBuilds}`, detail: "Windows, macOS, and Linux on x64 and ARM64" },
];

export function Stats() {
  return (
    <section aria-label="kon at a glance" className="border-y border-line bg-panel/60">
      {/* The 1px gap shows the list's background as dividers between tiles only. */}
      <dl className="mx-auto grid max-w-6xl grid-cols-2 gap-px bg-line lg:grid-cols-4">
        {STATS.map((stat) => (
          <div key={stat.label} className="bg-panel px-4 py-8 sm:px-6 lg:py-10">
            <dt className="text-sm text-muted">{stat.label}</dt>
            <dd className="mt-2 text-4xl font-semibold tracking-tight sm:text-5xl">{stat.value}</dd>
            <dd className="mt-2 text-sm text-faint">{stat.detail}</dd>
          </div>
        ))}
      </dl>
    </section>
  );
}
