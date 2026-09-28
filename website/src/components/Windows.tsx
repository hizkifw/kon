import Image from "next/image";
import { Inline } from "./Inline";
import { SectionHeading } from "./SectionHeading";
import screenshot from "@/assets/kon-windows-terminal.png";

const POINTS = [
  {
    title: "A native `kon.exe`",
    body: "x64 and ARM64, from the same code as every other platform. No WSL, no emulation layer.",
  },
  {
    title: "A shell tool that knows its shell",
    body: "Git Bash if you have it, then PowerShell, then `cmd.exe`, and the model knows which one it got.",
  },
  {
    title: "Updates itself",
    body: "`kon upgrade` swaps in the latest verified release. No installer to run again.",
  },
];

export function Windows() {
  return (
    <section id="windows" className="border-t border-line">
      <div className="mx-auto max-w-6xl px-4 py-20 sm:px-6 lg:py-28">
        <SectionHeading eyebrow="Windows" title="First-class on Windows, not ported to it.">
          <p>
            Terminal tools often treat Windows as an afterthought: install WSL, assume a POSIX shell, hope the paths
            work out. kon doesn&rsquo;t.
          </p>
        </SectionHeading>

        <div className="mt-14 grid grid-cols-1 gap-12 lg:grid-cols-[minmax(0,1.05fr)_minmax(0,1fr)]">
          <figure>
            <div className="overflow-hidden rounded-xl border border-line shadow-2xl shadow-black/60">
              <Image
                src={screenshot}
                alt="kon running in Windows Terminal: the header shows the model, the banner, a short exchange, and the status bar."
                className="h-auto w-full"
              />
            </div>
            <figcaption className="mt-3 text-sm text-faint">kon in Windows Terminal.</figcaption>
          </figure>

          <dl className="grid grid-cols-1 content-center gap-x-8 gap-y-7 sm:grid-cols-3 lg:grid-cols-1">
            {POINTS.map((point) => (
              <div key={point.title} className="border-l-2 border-accent/50 pl-4">
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
      </div>
    </section>
  );
}
