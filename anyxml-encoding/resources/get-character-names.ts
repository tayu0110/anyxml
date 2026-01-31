// deno run -A resources/get-character-names.ts
import * as path from "jsr:@std/path@1.1.0";
import { exists } from "jsr:@std/fs@1.0.18";

const cwd = Deno.cwd();
const manifest = path.resolve(Deno.cwd(), "Cargo.toml");
if (path.basename(cwd) !== "anyxml-encoding" || !await exists(manifest)) {
    console.error("Please execute crate root (anyxml-encoding/ directory).");
    Deno.exit(1);
}

// document library: https://standards.iso.org/iso-iec/10646/ed-6/en/
const text = await fetch(
    "https://standards.iso.org/iso-iec/10646/ed-6/en/Allnames.txt",
).then((res) => res.text());

const mapping: [number, string][] = [];
for (const line of text.split("\n").map((line) => line.trim())) {
    if (line.startsWith(";") || line.length === 0) {
        continue;
    }

    const split = line.split(/\s/).filter((s) => s.length > 0);
    const code = split.shift() as string;
    const name = split.join(" ");

    mapping.push([parseInt(code, 16), name]);
}

mapping.sort((l, r) => l[0] - r[0]);
for (const [code, name] of mapping) {
    console.log(`${code.toString(16).padStart(5, "0")}\t${name}`);
}
