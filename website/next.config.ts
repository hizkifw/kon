import type { NextConfig } from "next";

const nextConfig: NextConfig = {
  // The site is plain static files in out/, so any static host can serve it
  // without a Node server. Export has no image optimizer, so images ship as-is.
  output: "export",
  images: { unoptimized: true },
};

export default nextConfig;
