# kon website

The landing page for kon: a Next.js App Router site with TypeScript and
Tailwind CSS, exported as plain static files.

```sh
npm install
npm run dev     # http://localhost:3000
npm run lint
npm run build   # writes the static site to out/
```

`out/` has no server component, so any static host can serve it, including
GitHub Pages.

## Claims

The headline figures live in `src/lib/site.ts`, beside where they were
measured, so each is updated in one place. The rest of the copy cites kon's
own docs:

| Claim | Source |
| --- | --- |
| 21 ms startup, 11 MB memory | the `startup` scenario of `make loadtest`, in `docs/development/benchmarking.md` |
| 12 MB binary, 5 MB download | a `-s -w` release build, as `scripts/release.sh` makes it |
| 6 native builds | the target list in `scripts/release.sh` |
| 50 ms to 21 ms, streaming throughput | `docs/development/benchmarking.md` and commit `acab5ff` |
| Windows behavior | `docs/product/usage.md`, `docs/development/index.md`, `docs/development/session-format.md`, `docs/development/architecture.md` |

Re-measure before a release that could move them.

## Layout

| Path | Holds |
| --- | --- |
| `src/app/` | the root layout, the page, the theme, and the favicon |
| `src/lib/site.ts` | links, install commands, and every number |
| `src/components/Terminal.tsx` | the scripted hero demo, drawn in kon's own palette |
| `src/components/Banner.tsx` | kon's banner from `internal/ui/banner.go`, as SVG strokes |
| `src/components/StartupChart.tsx` | kon's startup beside familiar timings |
| `src/components/*.tsx` | one component per page section |

The colors come from `internal/ui/transcript.go`, so the site looks like the
product. The terminal demo is a pure function of elapsed time, which is how it
honors `prefers-reduced-motion` by showing its last frame.
