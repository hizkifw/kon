"use client";

import { useRef, useState, useSyncExternalStore } from "react";
import { installs, type Os } from "@/lib/site";

const ORDER: Os[] = ["windows", "unix", "go"];

const noSubscribe = () => () => {};

// useOs starts every visitor on the tab for their own platform. The server has
// no platform to read, so it renders the macOS and Linux tab and the client
// switches after hydration; a tab the visitor picks always wins.
export function useOs(): [Os, (os: Os) => void] {
  const windows = useSyncExternalStore(
    noSubscribe,
    () => /Windows/i.test(navigator.userAgent),
    () => false,
  );
  const [picked, setPicked] = useState<Os | null>(null);
  return [picked ?? (windows ? "windows" : "unix"), setPicked];
}

export function Install({ os, onSelect }: { os: Os; onSelect: (os: Os) => void }) {
  const [copied, setCopied] = useState(false);
  const commandRef = useRef<HTMLSpanElement>(null);
  const { prompt, command } = installs[os];

  async function copy() {
    try {
      await navigator.clipboard.writeText(command);
      setCopied(true);
      setTimeout(() => setCopied(false), 1600);
    } catch {
      // Clipboard access can be denied, for example in an embedded frame.
      // Selecting the command leaves the visitor one keystroke from a copy.
      const range = document.createRange();
      if (commandRef.current) range.selectNodeContents(commandRef.current);
      getSelection()?.removeAllRanges();
      getSelection()?.addRange(range);
    }
  }

  return (
    <div className="overflow-hidden rounded-lg border border-line bg-panel text-left">
      <div role="tablist" aria-label="Install for" className="flex border-b border-line text-sm">
        {ORDER.map((id) => (
          <button
            key={id}
            role="tab"
            type="button"
            aria-selected={id === os}
            onClick={() => onSelect(id)}
            className={`-mb-px border-b-2 px-4 py-2.5 transition-colors ${
              id === os ? "border-accent text-fg" : "border-transparent text-muted hover:text-fg"
            }`}
          >
            {installs[id].tab}
          </button>
        ))}
      </div>
      <div role="tabpanel" className="flex items-center gap-3 py-2 pr-2 pl-4">
        {/* A long command scrolls sideways under a fade rather than a scrollbar;
            Copy is the main way to take it. */}
        <code className="min-w-0 flex-1 overflow-x-auto py-1.5 pr-6 font-mono text-[13px] whitespace-nowrap [mask-image:linear-gradient(to_right,black_calc(100%-1.5rem),transparent)] [scrollbar-width:none]">
          <span className="text-faint select-none">{prompt} </span>
          <span ref={commandRef}>{command}</span>
        </code>
        <button
          type="button"
          onClick={copy}
          className="shrink-0 rounded-md border border-line px-3 py-1.5 text-sm text-muted transition-colors hover:border-accent hover:text-fg"
        >
          <span aria-live="polite">{copied ? "Copied" : "Copy"}</span>
        </button>
      </div>
    </div>
  );
}

// InstallBlock is Install with its own platform state, for places on the page
// that are not tied to the hero terminal.
export function InstallBlock() {
  const [os, setOs] = useOs();
  return <Install os={os} onSelect={setOs} />;
}
