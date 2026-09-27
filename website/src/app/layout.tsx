import type { Metadata, Viewport } from "next";
import { Geist, JetBrains_Mono } from "next/font/google";
import "./globals.css";

const geist = Geist({
  variable: "--font-geist",
  subsets: ["latin"],
});

const jetbrains = JetBrains_Mono({
  variable: "--font-jetbrains",
  subsets: ["latin"],
});

export const metadata: Metadata = {
  title: "kon · the coding agent that starts in 21 ms",
  description:
    "kon is a terminal coding agent in one native binary. It starts in about 20 ms, weighs 12 MB, and runs natively on Windows, macOS, and Linux.",
  openGraph: {
    title: "kon · the coding agent that starts in 21 ms",
    description: "One 12 MB native binary. First-class on Windows, macOS, and Linux.",
    type: "website",
  },
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
