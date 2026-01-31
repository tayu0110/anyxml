// deno run -A resources/generate-jisx0212-kanji.ts
import * as path from "jsr:@std/path@1.1.0";
import { exists } from "jsr:@std/fs@1.0.18";
import { assert } from "node:console";

const cwd = Deno.cwd();
const manifest = path.resolve(Deno.cwd(), "Cargo.toml");
if (path.basename(cwd) !== "anyxml-encoding" || !await exists(manifest)) {
    console.error("Please execute crate root (anyxml-encoding/ directory).");
    Deno.exit(1);
}

const text = await fetch(
    "https://www.unicode.org/Public/MAPPINGS/OBSOLETE/EASTASIA/JIS/JIS0212.TXT",
).then((res) => res.text());

const mapping: [number, number, string][] = [];
for (const line of text.split("\n").map((line) => line.trim())) {
    if (line.startsWith("#") || line.length === 0) {
        continue;
    }

    const split = line.split(/\s/).filter((s) => s.length > 0);
    const jiscode = parseInt(split.shift() as string, 16);
    const ucscode = parseInt(split.shift() as string, 16);
    split.shift();
    const ucsname = split.join(" ");

    assert(jiscode >= 0x2020);

    const ku = (jiscode - 0x2020) >> 8;
    const ten = (jiscode - 0x2020) & 0xFF;
    if (ucsname === "<CJK>") {
        const name = `CJK UNIFIED IDEOGRAPH-${
            ucscode.toString(16).toUpperCase()
        }`;
        mapping.push([ku, ten, name]);
    } else if (ku == 9 && ten == 1) {
        // In the Unicode mapping table, it is listed as LATIN CAPITAL LIGATURE AE,
        // but UCS (edition 6) contains no character with that name.
        // Based on the table's code number, it should map to UCS 0xC6,
        // whose name is “LATIN CAPITAL LETTER AE”.
        mapping.push([ku, ten, "LATIN CAPITAL LETTER AE"]);
    } else if (ku == 9 && ten == 33) {
        // Similar to ku=9,ten=1, the name of the mapping destination is incorrect.
        // The name of the mapping destination, UCS 0xE6, is “LATIN SMALL LETTER AE”.
        mapping.push([ku, ten, "LATIN SMALL LETTER AE"]);
    } else {
        mapping.push([ku, ten, ucsname]);
    }
}

mapping.sort((l, r) => {
    if (l[0] != r[0]) {
        return l[0] - r[0];
    } else {
        return l[1] - r[1];
    }
});
for (const [ku, ten, ucsname] of mapping) {
    console.log(
        `${ku} ${ten} ${ucsname}`,
    );
}
