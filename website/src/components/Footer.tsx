import { Banner } from "./Banner";
import { docsUrl, repo } from "@/lib/site";

export function Footer() {
  return (
    <footer className="border-t border-line">
      <div className="mx-auto flex max-w-6xl flex-col gap-6 px-4 py-10 sm:flex-row sm:items-end sm:px-6">
        <div>
          <Banner className="text-[7px] text-accent" strokeWidth={2} />
          <p className="mt-3 font-mono text-xs text-faint">harness for foxes =˄▾˄=</p>
        </div>
        <nav aria-label="Footer" className="flex gap-6 text-sm text-muted sm:ml-auto">
          <a href={docsUrl} className="hover:text-fg">
            Docs
          </a>
          <a href={`${repo}/releases`} className="hover:text-fg">
            Releases
          </a>
          <a href={repo} className="hover:text-fg">
            GitHub
          </a>
          <a href={`${repo}/blob/main/LICENSE`} className="hover:text-fg">
            MIT License
          </a>
        </nav>
      </div>
    </footer>
  );
}
