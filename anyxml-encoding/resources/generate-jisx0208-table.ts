// deno run -A resources/generate-jisx0212-kanji.ts
import * as path from "jsr:@std/path@1.1.0";
import { exists } from "jsr:@std/fs@1.0.18";

const cwd = Deno.cwd();
const manifest = path.resolve(Deno.cwd(), "Cargo.toml");
if (path.basename(cwd) !== "anyxml-encoding" || !await exists(manifest)) {
    console.error("Please execute crate root (anyxml-encoding/ directory).");
    Deno.exit(1);
}

const text = await fetch(
    "https://www.unicode.org/Public/MAPPINGS/OBSOLETE/EASTASIA/JIS/JIS0208.TXT",
).then((res) => res.text());

const mapping: [number, number, string][] = [];
for (const line of text.split("\n").map((line) => line.trim())) {
    if (line.startsWith("#") || line.length === 0) {
        continue;
    }

    const split = line.split(/\s/).filter((s) => s.length > 0);
    split.shift(); // skip Shift_JIS code
    const jiscode = parseInt(split.shift() as string, 16);
    const ucscode = parseInt(split.shift() as string, 16);
    split.shift(); // skip '#'
    const ucsname = split.join(" ");

    if (jiscode < 0x2020) {
        continue;
    }

    const ku = (jiscode - 0x2020) >> 8;
    const ten = (jiscode - 0x2020) & 0xFF;
    if (ucsname === "<CJK>") {
        const name = `CJK UNIFIED IDEOGRAPH-${
            ucscode.toString(16).toUpperCase()
        }`;
        mapping.push([ku, ten, name]);
    } else if (ku == 1 && ten == 29 && ucsname !== "EM DASH") {
        // Although it is labeled as HORIZONTAL BAR in the Unicode mapping table,
        // it is corrected to EM DASH in accordance with the JIS specification.
        mapping.push([ku, ten, "EM DASH"]);
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
