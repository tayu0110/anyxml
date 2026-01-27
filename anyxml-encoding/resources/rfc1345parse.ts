// deno run -A resources/rfc1345parse.ts CHARSET_NAME ...
import * as path from "jsr:@std/path@1.1.0";
import { exists } from "jsr:@std/fs@1.0.18";
import { parseArgs } from "node:util";

async function createMnemonicTable(): Promise<Map<string, number>> {
    const buffer = await Deno.readTextFile("resources/rfc1345/mnemonics.txt");
    const map = new Map<string, number>();
    const lines = buffer
        .split("\n")
        .map((line) => line.trim())
        .filter((line) => line.length > 0);
    for (const line of lines) {
        const [mnemonic, code, ..._] = line.split(/\s/).filter((s) =>
            s.length > 0
        );
        const value = parseInt(code, 16);
        if (isNaN(value)) {
            continue;
        }
        map.set(mnemonic, value);
    }

    return map;
}

async function createCharsetTable(
    enc: string,
    mnemonics: Map<string, number>,
): Promise<string> {
    const buffer = await Deno.readTextFile(`resources/rfc1345/${enc}.txt`);

    const lines = buffer
        .split("\n")
        .map((line) => line.trim())
        .filter((line) => line.length > 0);
    let count = 0;
    let charset = "unknown";
    const toUnicode: number[] = Array(256);
    toUnicode.fill(-1);
    for (const line of lines) {
        if (line.startsWith("&charset")) {
            const ch = line.split(/\s/)[1].trim();
            charset = ch;
        } else if (line.startsWith("&rem")) continue;
        else if (line.startsWith("&alias")) continue;
        else if (line.startsWith("&code ")) {
            const num = parseInt(line.split(/\s/)[1]);
            count += num;
        } else if (/^\&[^\s]/.test(line)) {
            console.error(`unknown keyword '${line}' (file: ${enc}.txt)`);
        } else {
            const mnes = line
                .split(/\s/)
                .filter((line) => line.length > 0);
            for (const mne of mnes) {
                const value = mnemonics.get(mne);
                if (value || value === 0) {
                    toUnicode[count] = value;
                }
                count += 1;
            }
        }
    }

    const encUpper = enc.replaceAll("-", "_").toUpperCase();
    const fromUnicode: [number, number][] = [];
    let ret = `\nconst ${encUpper}: &str = "${charset}";\n`;
    ret += `const ${encUpper}_DECODE_TABLE: [char; 256] = [`;
    for (let i = 0; i < toUnicode.length; i++) {
        if (toUnicode[i] < 0) {
            ret += "char::REPLACEMENT_CHARACTER,";
        } else {
            ret += `'\\u{${toUnicode[i].toString(16)}}',`;
            fromUnicode.push([toUnicode[i], i]);
        }
    }
    ret += `];\nconst ${encUpper}_ENCODE_TABLE: &[(u16, u8)] = &[`;
    fromUnicode.sort((a, b) => a[0] - b[0]);
    for (const [uni, chr] of fromUnicode) {
        ret += `(${uni}, ${chr}),`;
    }
    ret += "];";
    return ret;
}

const { positionals } = parseArgs({
    allowPositionals: true,
});

const cwd = Deno.cwd();
const manifest = path.resolve(Deno.cwd(), "Cargo.toml");
if (path.basename(cwd) !== "anyxml-encoding" || !await exists(manifest)) {
    console.error("Please execute crate root (anyxml-encoding/ directory).");
    Deno.exit(1);
}

const mnemonics = await createMnemonicTable();

const promise: Promise<string>[] = [];
for (const arg of positionals) {
    const enc = arg.replaceAll(/[^a-zA-Z0-9-]/g, "_");
    promise.push(createCharsetTable(enc, mnemonics));
}

const ret = await Promise.all(promise);
console.log(ret.join("\n"));
