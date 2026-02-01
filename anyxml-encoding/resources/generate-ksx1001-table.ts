// deno run -A resources/generate-jiscode-table.ts > src/jisx.rs && rustfmt src/jisx.rs
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

const mapping = new Map<string, number>();
for (const line of text.split("\n").map((line) => line.trim())) {
    if (line.startsWith(";") || line.length === 0) {
        continue;
    }

    const split = line.split(/\s/).filter((s) => s.length > 0);
    const code = parseInt(split.shift() as string, 16);
    const name = split.join(" ");

    mapping.set(name, code);
}

async function generateKSX1001Table(
    _spec: string,
    _nameMap: Map<string, number>,
): Promise<string> {
    let ret = `\npub(crate) static KS_X_1001_DECODE_TABLE: [&[char]; 96] = [`;
    const table = await fetch(
        "https://www.unicode.org/Public/MAPPINGS/OBSOLETE/EASTASIA/KSC/KSX1001.TXT",
    ).then((res) => res.text());
    const toUCS: number[][] = [];
    for (let i = 0; i < 96; i++) {
        const arr = Array<number>(96);
        arr.fill(-1);
        toUCS.push(arr);
    }
    const fromUCS = [];
    for (const line of table.split("\n").map((line) => line.trim())) {
        if (line.startsWith("#") || line.length == 0) {
            continue;
        }

        const split = line.split(/\s/).filter((s) => s.length > 0);
        const ksxcode = parseInt(split.shift() as string, 16);
        const ucscode = parseInt(split.shift() as string, 16);
        const ku = (ksxcode - 0x2020) >> 8;
        const ten = (ksxcode - 0x2020) & 0xFF;
        split.shift(); // skip '#'
        // const name = split.join(" ");
        if (name === "<CJK>") {
            toUCS[ku][ten] = ucscode;
            fromUCS.push([ucscode, ksxcode]);
        } else {
            // const mapped = nameMap.get(name);
            // if (!mapped || (ucscode !== mapped)) {
            //     console.error(`[${spec}] '${line}' is unmappable.`);
            //     continue;
            // }

            toUCS[ku][ten] = ucscode;
            fromUCS.push([ucscode, ksxcode]);
        }
    }
    for (let i = 0; i < 96; i++) {
        if (toUCS[i].every((value) => value < 0)) {
            ret += "&[],";
            continue;
        }
        ret += "&[";
        for (let j = 0; j < 96; j++) {
            if (toUCS[i][j] < 0) {
                ret += "char::REPLACEMENT_CHARACTER,";
            } else {
                ret += `'\\u{${toUCS[i][j].toString(16)}}',`;
            }
        }
        ret += "],";
    }
    ret += `];\n\npub(crate) static KS_X_1001_ENCODE_TABLE: &[(u16, u16)] = &[`;
    fromUCS.sort((l, r) => l[0] - r[0]);
    for (const [ucs, jis] of fromUCS) {
        ret += `(${ucs}, ${jis}),`;
    }
    ret += "];\n";

    return ret;
}

const ret = await Promise.all([
    generateKSX1001Table("KS X 1001", mapping),
]);
console.log(`${ret.join("\n")}`);
