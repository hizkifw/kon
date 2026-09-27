import type { ReactNode } from "react";

export function SectionHeading({
  eyebrow,
  title,
  children,
}: {
  eyebrow: string;
  title: ReactNode;
  children?: ReactNode;
}) {
  return (
    <div>
      <p className="font-mono text-xs tracking-[0.2em] text-accent uppercase">{eyebrow}</p>
      <h2 className="mt-4 text-3xl font-semibold tracking-tight text-balance sm:text-[2.5rem] sm:leading-tight">
        {title}
      </h2>
      {children && <div className="mt-5 max-w-xl space-y-4 text-lg leading-relaxed text-muted">{children}</div>}
    </div>
  );
}
