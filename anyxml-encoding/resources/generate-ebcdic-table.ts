// deno run -A resources/generate-ebcdic-table.ts
import * as path from "jsr:@std/path@1.1.0";
import { exists } from "jsr:@std/fs@1.0.18";
import unzipper from "npm:unzipper@0.12.3";
import { parseArgs } from "node:util";
import { globSync } from "node:fs";

const CDRA_REPOSITORY: string = `cdctables`;
const CDRA_REPOSITORY_FILENAME: string = `${CDRA_REPOSITORY}.zip`;
const CDRA_REPOSITORY_FILE_FULLNAME: string =
    `resources/${CDRA_REPOSITORY_FILENAME}`;
const CDRA_REPOSITORY_DIR_FULLNAME: string = `resources/${CDRA_REPOSITORY}`;
const CDRA_REPOSITORY_URL: string =
    `https://download.boulder.ibm.com/ibmdl/pub/software/dw/java/cdctables.zip`;

const cwd = Deno.cwd();
const manifest = path.resolve(Deno.cwd(), "Cargo.toml");
if (path.basename(cwd) !== "anyxml-encoding" || !await exists(manifest)) {
    console.error("Please execute crate root (anyxml-encoding/ directory).");
    Deno.exit(1);
}

if (!await exists(CDRA_REPOSITORY_FILE_FULLNAME)) {
    const resp = await fetch(CDRA_REPOSITORY_URL);
    console.error(`successfully downloaded ${CDRA_REPOSITORY_URL}`);
    const xmlts_zip = await resp.bytes();
    await Deno.writeFile(CDRA_REPOSITORY_FILE_FULLNAME, xmlts_zip);
    console.error(`successfully made ${CDRA_REPOSITORY_FILE_FULLNAME}`);
}
if (await exists(CDRA_REPOSITORY_DIR_FULLNAME)) {
    console.error(`remove ${CDRA_REPOSITORY_DIR_FULLNAME}`);
    await Deno.remove(CDRA_REPOSITORY_DIR_FULLNAME, { recursive: true });
}
await Deno.mkdir(CDRA_REPOSITORY_DIR_FULLNAME);

const dir = await unzipper.Open.file(CDRA_REPOSITORY_FILE_FULLNAME);
await dir.extract({ path: CDRA_REPOSITORY_DIR_FULLNAME });
console.error(
    `successfully extracted ${CDRA_REPOSITORY_FILE_FULLNAME}`,
);

const package1 = await unzipper.Open.file(
    `${CDRA_REPOSITORY_DIR_FULLNAME}/Package1.zip`,
);
await Deno.mkdir(`${CDRA_REPOSITORY_DIR_FULLNAME}/Package1`);
await package1.extract({ path: `${CDRA_REPOSITORY_DIR_FULLNAME}/Package1` });
console.error(
    `successfully extracted ${CDRA_REPOSITORY_FILE_FULLNAME}/Package1.zip`,
);

const { positionals } = parseArgs({
    allowPositionals: true,
});

const promise: Promise<string | null>[] = [];
for (const enc of positionals) {
    promise.push(createCharsetTable(enc));
}

const ret = await Promise.all(promise);
console.log(ret.filter((ret) => ret != null).join("\n"));

async function createCharsetTable(enc: string): Promise<string | null> {
    const REPO_FILENAME: string =
        `${CDRA_REPOSITORY_DIR_FULLNAME}/Package1/${enc}`;
    if (!await exists(REPO_FILENAME)) {
        if (!await exists(`${REPO_FILENAME}.zip`)) {
            console.error(`${enc} is not registered to CDRA Repository`);
            return null;
        }

        await Deno.mkdir(`${REPO_FILENAME}`);
        const crepo = await unzipper.Open.file(`${REPO_FILENAME}.zip`);
        await crepo.extract({ path: `${REPO_FILENAME}` });
    }

    const mapping = globSync(`${REPO_FILENAME}/*.TPMAP100`).concat(
        globSync(`${REPO_FILENAME}/*.TXMAP110`),
    )[0];
    if (!mapping) {
        console.error(`Mapping data for ${enc} is not found.`);
        return null;
    }

    const toUnicode: number[] = Array(256);
    toUnicode.fill(-1);
    const buf = await Deno.readTextFile(mapping);
    for (const line of buf.split("\n").map((line) => line.trim())) {
        if (line.length == 0 || line.startsWith("*")) {
            continue;
        }

        const codes = line.split(/\s/).filter((code) => code.length > 0);
        const from = parseInt(codes[0], 16);
        const to = parseInt(codes[1], 16);
        if (isNaN(from) || isNaN(to)) {
            continue;
        }
        toUnicode[from] = to;
    }

    const encUpper = enc.replaceAll("-", "_").toUpperCase();
    const fromUnicode: [number, number][] = [];
    let ret = `\nconst ${encUpper}_DECODE_TABLE: [char; 256] = [`;
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
