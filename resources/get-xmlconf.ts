// deno run -A resources/get-xmlconf.ts
import * as path from "jsr:@std/path";
import { exists } from "jsr:@std/fs/exists";
import unzipper from "npm:unzipper";

const XMLTS_BASENAME: string = "xmlts20130923";
const XMLTS_FILENAME: string = `${XMLTS_BASENAME}.zip`;
const XMLTS_FILE_FULLNAME: string = `resources/${XMLTS_FILENAME}`;
const XMLTS_URL: string = `https://www.w3.org/XML/Test/${XMLTS_FILENAME}`;

const cwd = Deno.cwd();
const manifest = path.resolve(Deno.cwd(), "Cargo.toml");
if (path.basename(cwd) !== "anyxml" || !await exists(manifest)) {
    console.error("Please execute crate root (anyxml/ directory).");
    Deno.exit(1);
}

if (await exists(XMLTS_FILE_FULLNAME)) {
    console.log(`remove ${XMLTS_FILE_FULLNAME}`);
    await Deno.remove(XMLTS_FILE_FULLNAME);
}

const resp = await fetch(XMLTS_URL);
const xmlts_zip = await resp.bytes();
await Deno.writeFile(XMLTS_FILE_FULLNAME, xmlts_zip);

const dir = await unzipper.Open.file(XMLTS_FILE_FULLNAME);
await dir.extract({ path: `resources` });
