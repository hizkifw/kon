<!-- BEGIN:nextjs-agent-rules -->

# This is NOT the Next.js you know

This version has breaking changes — APIs, conventions, and file structure may all differ from your training data. Read the relevant guide in `node_modules/next/dist/docs/` (resolved from this file's directory; in monorepos the `next` package may not be visible from the repo root) before writing any code. Heed deprecation notices.

This block is written and re-added by `next dev` — verify at `node_modules/next/dist/server/lib/generate-agent-files.js`. Removing it from a diff only re-creates the uncommitted change; committing it with your work keeps the tree clean.

<!-- END:nextjs-agent-rules -->

# kon website

- The headline figures (startup, sizes, memory, build count) and the links
  live in `src/lib/site.ts`, next to where they were measured. Every other
  claim in section copy must trace to kon's docs, scripts, or git history; do
  not add one that cannot.
- The page is a static export (`output: "export"`). Keep it that way: no
  route handlers, server actions, or image optimization.
- Client JavaScript is limited to the install tabs and the terminal demo. A
  site about startup speed should load fast too.
- Run `npm run lint` and `npm run build` before considering a change done.
