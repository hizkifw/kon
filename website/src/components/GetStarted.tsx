import { InstallBlock } from "./Install";
import { docsUrl, repo } from "@/lib/site";

export function GetStarted() {
  return (
    <section id="install" className="relative isolate overflow-hidden border-t border-line">
      <div
        aria-hidden
        className="absolute inset-x-0 bottom-0 -z-10 h-[30rem] bg-[radial-gradient(50rem_24rem_at_50%_110%,rgba(201,138,138,0.12),transparent)]"
      />
      <div className="mx-auto max-w-3xl px-4 py-24 text-center sm:px-6 lg:py-32">
        <h2 className="text-4xl font-semibold tracking-tight text-balance sm:text-5xl">Try it. It&rsquo;s already open.</h2>
        <p className="mx-auto mt-5 max-w-xl text-lg leading-relaxed text-muted">
          Run <code className="font-mono text-fg">kon</code> in any project. Connect a provider with{" "}
          <code className="font-mono text-fg">/login</code>, pick a model with{" "}
          <code className="font-mono text-fg">/model</code>, and start typing.
        </p>
        <div className="mx-auto mt-10 max-w-2xl">
          <InstallBlock />
        </div>
        <div className="mt-6 flex flex-wrap justify-center gap-x-6 gap-y-2 text-sm">
          <a href={docsUrl} className="text-fg underline decoration-line underline-offset-4 hover:decoration-accent">
            Read the guide
          </a>
          <a href={repo} className="text-muted transition-colors hover:text-fg">
            Source on GitHub →
          </a>
        </div>
      </div>
    </section>
  );
}
