// Every claim on the page reads from here, next to where it was measured, so a
// number can be checked or updated in one place. Re-measure before a release
// that could move them.

// The canonical address. Shared links and the Open Graph image resolve
// against it, so crawlers never see a localhost URL.
export const siteUrl = "https://kon.kitsu.red";

export const repo = "https://github.com/hizkifw/kon";
export const docsUrl = `${repo}/blob/main/docs/product/index.md`;

export type Os = "windows" | "unix" | "go";

// The site's install URLs redirect to the scripts on main (see vercel.json),
// so the commands stay short and the scripts keep one home.
export const installs: Record<Os, { tab: string; prompt: string; command: string }> = {
  windows: {
    tab: "Windows",
    prompt: "PS>",
    command: `iwr -useb ${siteUrl}/install.ps1 | iex`,
  },
  unix: {
    tab: "macOS & Linux",
    prompt: "$",
    command: `curl -fsSL ${siteUrl}/install.sh | sh`,
  },
  go: {
    tab: "Go",
    prompt: "$",
    command: "go install github.com/hizkifw/kon/cmd/kon@latest",
  },
};

// The `first-paint` scenario of `make loadtest` (docs/development/benchmarking.md):
// exec to the first frame of the full-screen UI, the median of seven runs of
// 50 launches each, on an Intel i5-8500T running Linux. Peak RSS at that
// frame is 15.9 MB in the same runs.
export const startupMs = 27;
export const startupRssMb = 16;
export const startupMachine = "an Intel i5-8500T running Linux";

// `-s -w` release builds, as scripts/release.sh makes them. The windows/amd64
// zip the installer downloads is 4.9 MB.
export const binaries = [
  { file: "kon.exe", platform: "Windows x64", mb: 12.0 },
  { file: "kon", platform: "Linux x64", mb: 11.6 },
  { file: "kon", platform: "macOS Apple silicon", mb: 11.0 },
];
export const binaryMb = 12;
export const downloadMb = 5;

// scripts/release.sh: linux, darwin, and windows, each on amd64 and arm64.
export const nativeBuilds = 6;

// Reference points for the startup chart. Only kon's own row is a measurement
// of kon; the rest are well-known human and display timings.
export const startupScale = [
  { label: "One frame on a 60 Hz display", ms: 16.7, value: "16.7 ms", source: "1,000 ms ÷ 60" },
  {
    label: "kon, launch to first frame",
    ms: startupMs,
    value: `${startupMs} ms`,
    source: "make loadtest, first-paint scenario",
    kon: true,
  },
  {
    label: "Where a delay stops feeling instant",
    ms: 100,
    value: "100 ms",
    source: "Nielsen, Usability Engineering (1993)",
  },
  {
    label: "Reacting to something on screen",
    ms: 250,
    value: "~250 ms",
    source: "typical simple visual reaction time",
  },
];

// src/app/og.png/route.tsx renders this image. It is a plain route rather than
// the opengraph-image convention because a static export writes that one
// without a file extension, and static hosts would then serve it without an
// image content type.
export const ogImage = {
  url: "/og.png",
  width: 1200,
  height: 630,
  alt: `kon: starts in ${startupMs} ms. One binary. Every OS. Below, kon runs in Windows Terminal, fixing a failing test.`,
};
