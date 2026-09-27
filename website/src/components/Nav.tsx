import { Banner } from "./Banner";
import { docsUrl, repo } from "@/lib/site";

const LINKS = [
  { href: "#speed", label: "Speed" },
  { href: "#binary", label: "Single binary" },
  { href: "#windows", label: "Windows" },
  { href: docsUrl, label: "Docs" },
];

export function Nav() {
  return (
    <header className="sticky top-0 z-50 border-b border-line/70 bg-screen/80 backdrop-blur-md">
      <div className="mx-auto flex h-14 max-w-6xl items-center gap-8 px-4 sm:px-6">
        <a href="#top" aria-label="kon, back to top" className="text-accent transition-colors hover:text-accent-bright">
          <Banner className="text-[5.5px]" strokeWidth={2.4} />
        </a>
        <nav aria-label="Sections" className="hidden items-center gap-7 text-sm text-muted md:flex">
          {LINKS.map((link) => (
            <a key={link.href} href={link.href} className="transition-colors hover:text-fg">
              {link.label}
            </a>
          ))}
        </nav>
        <a
          href={repo}
          className="ml-auto flex items-center gap-2 rounded-md border border-line px-3 py-1.5 text-sm text-fg transition-colors hover:border-accent"
        >
          <svg viewBox="0 0 16 16" width="16" height="16" fill="currentColor" aria-hidden>
            <path d="M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0016 8c0-4.42-3.58-8-8-8z" />
          </svg>
          GitHub
        </a>
      </div>
    </header>
  );
}
