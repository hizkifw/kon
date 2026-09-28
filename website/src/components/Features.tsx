import { Inline } from "./Inline";
import { SectionHeading } from "./SectionHeading";

const FEATURES = [
  {
    title: "Four sharp tools",
    body: "`read`, `write`, `edit`, and `shell`, after pi. Context goes to your code, not a tool menu.",
  },
  {
    title: "Sessions are plain files",
    body: "Every conversation is JSONL you can read and grep. Pick one back up with `kon --resume`.",
  },
  {
    title: "Bring any model",
    body: "OpenAI, Anthropic, OpenRouter, Groq, xAI, Ollama, or any OpenAI-compatible endpoint. Connect one with `/login`.",
  },
  {
    title: "Made for scripts",
    body: "`git diff | kon run --stdin review this` just works. Add `--format json` for events.",
  },
  {
    title: "Steer while it works",
    body: "Enter steers the running turn. Tab queues a prompt for when it's done.",
  },
  {
    title: "Jobs and subagents",
    body: "Servers and long builds run in the background. Ask, and kon hands a task to a subagent.",
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
