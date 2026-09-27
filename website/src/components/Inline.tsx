import { Fragment } from "react";

// Inline renders the `backticked` spans of a plain string as code, so section
// copy can stay in simple string arrays.
export function Inline({ text }: { text: string }) {
  return text.split("`").map((part, i) =>
    i % 2 === 1 ? (
      <code key={i} className="font-mono text-[0.9em] text-fg">
        {part}
      </code>
    ) : (
      <Fragment key={i}>{part}</Fragment>
    ),
  );
}
