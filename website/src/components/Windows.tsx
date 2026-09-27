import Image from "next/image";
import { Inline } from "./Inline";
import { SectionHeading } from "./SectionHeading";
import screenshot from "@/assets/kon-windows-terminal.png";

const POINTS = [
  {
    title: "A native `kon.exe`",
    body: "x64 and ARM64 builds of the same code as every other platform. No WSL, no Cygwin, no emulation layer.",
  },
  {
    title: "One-line PowerShell install",
    body: "`iwr … | iex` verifies the SHA-256, installs to `%LOCALAPPDATA%\\Programs\\kon`, and picks the right architecture, even on older Windows PowerShell.",
  },
  {
    title: "A shell tool that knows its shell",
    body: "Commands run through Git Bash when you have it, then PowerShell, then `cmd.exe`, and kon tells the model which one it got, so it writes commands that run.",
  },
  {
    title: "Windows paths, Windows conventions",
    body: "Config lives in `%APPDATA%\\kon` and sessions in `%LOCALAPPDATA%\\kon`, where Windows expects them.",
  },
  {
    title: "Built around Windows file locking",
    body: "Windows locks are mandatory, so each session's lock lives in its own file. A second kon can still preview and follow a live session.",
  },
  {
    title: "Upgrades in place",
    body: "Windows won't overwrite a running `.exe`. `kon upgrade` renames it aside, swaps in the verified release, and cleans up next time.",
  },
];

export function Windows() {
  return (
    <section id="windows" className="border-t border-line">
      <div className="mx-auto max-w-6xl px-4 py-20 sm:px-6 lg:py-28">
        <SectionHeading eyebrow="Windows" title="First-class on Windows, not ported to it.">
          <p>
            Terminal tools often treat Windows as an afterthought: install WSL, assume a POSIX shell, hope the paths
            work out. kon is built for Windows with the same care as macOS and Linux, and it knows which one it&rsquo;s
            running on.
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
            <figcaption className="mt-3 text-sm text-faint">
              kon in Windows Terminal. Every commit also cross-builds for Windows x64 and ARM64.
            </figcaption>
          </figure>

          <dl className="grid grid-cols-1 gap-x-8 gap-y-7 sm:grid-cols-2 lg:grid-cols-1 xl:grid-cols-2">
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
