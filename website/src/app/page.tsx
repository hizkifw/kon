import { Features } from "@/components/Features";
import { Footer } from "@/components/Footer";
import { GetStarted } from "@/components/GetStarted";
import { Hero } from "@/components/Hero";
import { Incognito } from "@/components/Incognito";
import { Markdown } from "@/components/Markdown";
import { Nav } from "@/components/Nav";
import { SingleBinary } from "@/components/SingleBinary";
import { Speed } from "@/components/Speed";
import { Stats } from "@/components/Stats";
import { Windows } from "@/components/Windows";

export default function Home() {
  return (
    <>
      <Nav />
      <main>
        <Hero />
        <Stats />
        <Speed />
        <SingleBinary />
        <Windows />
        <Features />
        <Markdown />
        <Incognito />
        <GetStarted />
      </main>
      <Footer />
    </>
  );
}
