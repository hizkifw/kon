import { Inline } from "./Inline";
import { SectionHeading } from "./SectionHeading";

const FEATURES = [
  {
    title: "Four sharp tools",
    body: "`read`, `write`, `edit`, and `shell`, after pi's four-tool philosophy. The model spends its context on your code, not on a tool menu.",
  },
  {
    title: "Sessions are plain files",
    body: "Every conversation is JSONL you can read, grep, and keep. Pick one back up with `kon --resume`.",
  },
  {
    title: "Bring any model",
    body: "OpenAI, Anthropic, OpenRouter, Groq, xAI, Ollama, or any OpenAI-compatible endpoint. Connect one with `/login`.",
  },
  {
    title: "Made for scripts",
    body: "`kon run` streams one turn to stdout, so `git diff | kon run --stdin review this` just works. Add `--format json` for events.",
  },
  {
    title: "Steer while it works",
    body: "Enter steers the running turn the next time kon calls the model. Tab queues a prompt for when the turn is done.",
  },
  {
    title: "Jobs and subagents",
    body: "Servers, watchers, and long builds run as background jobs. Ask, and kon hands a self-contained task to a subagent.",
  },
];

export function Features() {
  return (
    <section className="border-t border-line">
      <div className="mx-auto max-w-6xl px-4 py-20 sm:px-6 lg:py-28">
        <SectionHeading eyebrow="Also in the box" title="Small, not bare." />
        <div className="mt-12 grid grid-cols-1 gap-4 sm:grid-cols-2 lg:grid-cols-3">
          {FEATURES.map((feature) => (
            <div key={feature.title} className="rounded-xl border border-line bg-panel p-6">
              <h3 className="font-medium text-fg">{feature.title}</h3>
              <p className="mt-2 text-sm leading-relaxed text-muted">
                <Inline text={feature.body} />
              </p>
            </div>
          ))}
        </div>
      </div>
    </section>
  );
}
