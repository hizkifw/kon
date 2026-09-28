# kon website

The landing page for kon: a Next.js App Router site with TypeScript and
Tailwind CSS, exported as plain static files.

```sh
npm install
npm run dev     # http://localhost:3000
npm run lint
npm run build   # writes the static site to out/
```

`out/` has no server component, so any static host can serve it. The site is
deployed on Vercel at https://kon.kitsu.red, with this directory as the
project's root.

`vercel.json` answers `/install.sh` and `/install.ps1` with 307 redirects to
the scripts on `main`, so the page's install commands are
`curl -fsSL https://kon.kitsu.red/install.sh | sh` and
`iwr -useb https://kon.kitsu.red/install.ps1 | iex` while the scripts keep one
home. `curl -L` and `Invoke-WebRequest` both follow the redirect. The redirects
live in Vercel's config because a static export cannot use Next's
`redirects()`, and they are temporary so the target can change without clients
holding a cached permanent one.

## Claims

The headline figures live in `src/lib/site.ts`, beside where they were
measured, so each is updated in one place. The rest of the copy cites kon's
own docs:

| Claim | Source |
| --- | --- |
| 27 ms startup, 16 MB memory | the `first-paint` scenario of `make loadtest`, in `docs/development/benchmarking.md` |
| 12 MB binary, 5 MB download | a `-s -w` release build, as `scripts/release.sh` makes it |
| 6 native builds | the target list in `scripts/release.sh` |
| streaming throughput | `docs/development/benchmarking.md` |
| 11 µs frames at any session length | Phase 2b of `docs/development/rendering-performance.md` |
| Windows behavior | `docs/product/usage.md`, `docs/development/index.md`, `docs/development/session-format.md`, `docs/development/architecture.md` |
| Incognito behavior | the Incognito and Scripting sections of `docs/product/usage.md` |

Re-measure before a release that could move them.

## Layout

| Path | Holds |
| --- | --- |
| `src/app/` | the root layout, the page, the theme, and the favicon |
| `src/app/og.png/route.tsx` | the 1200×630 share image, rendered to `out/og.png` at build time |
| `src/lib/site.ts` | the canonical URL, links, install commands, and every number |
| `src/components/Terminal.tsx` | the scripted hero demo, drawn in kon's own palette |
| `src/components/Banner.tsx` | kon's banner from `internal/ui/banner.go`, as SVG strokes |
| `src/components/StartupChart.tsx` | kon's startup beside familiar timings |
| `src/components/*.tsx` | one component per page section |

The share image is a plain route rather than Next's `opengraph-image`
convention, because a static export writes that one without a file extension
and static hosts would serve it without an image content type. It draws with
the page's own faces from `@fontsource`, since `ImageResponse` cannot read the
WOFF2 files `next/font` serves. Social sites cache it, so re-scrape
`https://kon.kitsu.red` in their debuggers after changing it.

The colors come from `internal/ui/transcript.go`, so the site looks like the
product. The terminal demo is a pure function of elapsed time, which is how it
honors `prefers-reduced-motion` by showing its last frame.
