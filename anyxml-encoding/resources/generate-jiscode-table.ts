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

async function generateJISX0201Table(
    nameMap: Map<string, number>,
): Promise<string> {
    let ret = "";
    for (
        const [upper, pascal] of [["KATAKANA", "Katakana"], ["LATIN", "Latin"]]
    ) {
        ret +=
            `\npub(crate) static JIS_X_0201_${upper}_DECODE_TABLE: [char; 94] = [`;
        const katakana = await Deno.readTextFile(
            `resources/jis-code-tables/JIS_X_0201-${pascal}.txt`,
        );
        const toUCS = Array<number>(94);
        toUCS.fill(-1);
        const fromUCS = [];
        for (const line of katakana.split("\n").map((line) => line.trim())) {
            if (line.startsWith(";") || line.length == 0) {
                continue;
            }

            const split = line.split(/\s/).filter((s) => s.length > 0);
            const codepoint = parseInt(split.shift() as string, 16);
            const name = split.join(" ");
            const ucscode = nameMap.get(name);
            if (!ucscode) {
                console.error(
                    `[JIS X 0201 ${pascal}] '${line}' is unmappable.`,
                );
                continue;
            }

            toUCS[codepoint - 32] = ucscode;
            fromUCS.push([ucscode, codepoint]);
        }
        for (let i = 0; i < 94; i++) {
            if (toUCS[i] < 0) {
                ret += "char::REPLACEMENT_CHARACTER,";
            } else {
                ret += `'\\u{${toUCS[i].toString(16)}}',`;
            }
        }
        ret +=
            `];\npub(crate) static JIS_X_0201_${upper}_ENCODE_TABLE: &[(u16, u8)] = &[`;
        fromUCS.sort((l, r) => l[0] - r[0]);
        for (const [ucs, jis] of fromUCS) {
            ret += `(${ucs}, ${jis}),`;
        }
        ret += "];\n";
    }

    return ret;
}

async function generateJISX02xxTable(
    spec: string,
    nameMap: Map<string, number>,
): Promise<string> {
    let ret =
        `\npub(crate) static JIS_X_${spec}_DECODE_TABLE: [[char; 94]; 94] = [`;
    const katakana = await Deno.readTextFile(
        `resources/jis-code-tables/JIS_X_${spec}.txt`,
    );
    const toUCS: number[][] = [];
    for (let i = 0; i < 94; i++) {
        const arr = Array<number>(94);
        arr.fill(-1);
        toUCS.push(arr);
    }
    const fromUCS = [];
    for (const line of katakana.split("\n").map((line) => line.trim())) {
        if (line.startsWith(";") || line.length == 0) {
            continue;
        }

        const split = line.split(/\s/).filter((s) => s.length > 0);
        const ku = parseInt(split.shift() as string);
        const ten = parseInt(split.shift() as string);
        const name = split.join(" ");
        const jiscode = (ku + 32) * 256 + (ten + 32);
        if (name.startsWith("CJK UNIFIED IDEOGRAPH-")) {
            const ucscode = parseInt(name.substring(22), 16);
            toUCS[ku - 1][ten - 1] = ucscode;
            fromUCS.push([ucscode, jiscode]);
        } else {
            const ucscode = nameMap.get(name);
            if (!ucscode) {
                console.error(`[JIS X ${spec}] '${line}' is unmappable.`);
                continue;
            }

            toUCS[ku - 1][ten - 1] = ucscode;
            fromUCS.push([ucscode, jiscode]);
        }
    }
    for (let i = 0; i < 94; i++) {
        ret += "[";
        if (toUCS[i].every((value) => value < 0)) {
            ret += "char::REPLACEMENT_CHARACTER; 94],";
            continue;
        }
        for (let j = 0; j < 94; j++) {
            if (toUCS[i][j] < 0) {
                ret += "char::REPLACEMENT_CHARACTER,";
            } else {
                ret += `'\\u{${toUCS[i][j].toString(16)}}',`;
            }
        }
        ret += "],";
    }
    ret +=
        `];\npub(crate) static JIS_X_${spec}_ENCODE_TABLE: &[(u16, u16)] = &[`;
    fromUCS.sort((l, r) => l[0] - r[0]);
    for (const [ucs, jis] of fromUCS) {
        ret += `(${ucs}, ${jis}),`;
    }
    ret += "];\n";

    return ret;
}

const ret = await Promise.all([
    generateJISX0201Table(mapping),
    generateJISX02xxTable("0208", mapping),
    generateJISX02xxTable("0212", mapping),
]);
console.log(`${ret.join("\n")}`);
