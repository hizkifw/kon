"use client";

import { Install, useOs } from "./Install";
import { Terminal } from "./Terminal";
import { binaryMb, docsUrl, repo, startupMs } from "@/lib/site";

// Hero shares one platform between the install tabs and the terminal demo, so
// choosing Windows shows kon running in Windows Terminal.
export function Hero() {
  const [os, setOs] = useOs();
  return (
    <section id="top" className="relative isolate overflow-hidden">
      <div
        aria-hidden
        className="absolute inset-x-0 top-0 -z-10 h-[44rem] bg-[radial-gradient(60rem_32rem_at_15%_-10%,rgba(201,138,138,0.13),transparent)]"
      />
      <div className="mx-auto grid max-w-6xl grid-cols-1 items-center gap-14 px-4 pt-14 pb-20 sm:px-6 sm:pt-20 lg:grid-cols-[minmax(0,1fr)_minmax(0,33rem)] lg:gap-12 lg:pt-24 lg:pb-28">
        <div>
          <p className="font-mono text-xs tracking-[0.2em] text-accent uppercase">A coding agent for your terminal</p>
          <h1 className="mt-5 text-[2.75rem] leading-[1.04] font-semibold tracking-tight sm:text-6xl lg:text-[4.25rem]">
            Starts in {startupMs}&nbsp;ms.
            <span className="block text-muted">One binary. Every&nbsp;OS.</span>
          </h1>
          <p className="mt-6 max-w-xl text-lg leading-relaxed text-muted">
            kon reads, edits, and runs commands in your project until the work is done. One {binaryMb}&nbsp;MB
            executable: no runtime, no daemon, no warm-up.
          </p>
          <div className="mt-8 max-w-xl">
            <Install os={os} onSelect={setOs} />
          </div>
          <div className="mt-5 flex flex-wrap gap-x-6 gap-y-2 text-sm">
            <a href={docsUrl} className="text-fg underline decoration-line underline-offset-4 hover:decoration-accent">
              Read the guide
            </a>
            <a href={repo} className="text-muted transition-colors hover:text-fg">
              Source on GitHub →
            </a>
          </div>
        </div>
        <Terminal platform={os === "windows" ? "windows" : "unix"} />
      </div>
    </section>
  );
}
