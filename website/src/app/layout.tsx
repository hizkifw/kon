import type { Metadata, Viewport } from "next";
import { Geist, JetBrains_Mono } from "next/font/google";
import { binaryMb, goImport, ogImage, siteUrl, startupMs } from "@/lib/site";
import "./globals.css";

const geist = Geist({
  variable: "--font-geist",
  subsets: ["latin"],
});

const jetbrains = JetBrains_Mono({
  variable: "--font-jetbrains",
  subsets: ["latin"],
});

const title = `kon · coding harness for foxes =˄▾˄=`;
const description = `kon is a terminal coding agent in one native binary. It starts in ${startupMs} ms, weighs ${binaryMb} MB, and runs natively on Windows, macOS, and Linux.`;

export const metadata: Metadata = {
  metadataBase: new URL(siteUrl),
  title,
  description,
  alternates: { canonical: "/" },
  openGraph: { title, description, url: "/", siteName: "kon", type: "website", images: [ogImage] },
  twitter: { card: "summary_large_image", title, description, images: [ogImage] },
  // Every page, including the 404, carries the tag, so the go command resolves
  // kon.kitsu.red/... import paths to the repository.
  other: { "go-import": goImport },
};

export const viewport: Viewport = {
  themeColor: "#0c0c0c",
  colorScheme: "dark",
};

export default function RootLayout({ children }: LayoutProps<"/">) {
  return (
    <html lang="en" className={`${geist.variable} ${jetbrains.variable} antialiased`}>
      <body className="min-h-full font-sans">{children}</body>
    </html>
  );
}
