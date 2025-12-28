// deno run -A resources/get-xinclude-testsuite.ts
import * as path from "jsr:@std/path@1.1.0";
import { copy, exists } from "jsr:@std/fs@1.0.18";
import unzipper from "npm:unzipper@0.12.3";

const tempdir = await Deno.makeTempDir();
const XINCLTS_BASENAME: string = `XIncl20060927`;
const XINCLTS_FILENAME: string = `${XINCLTS_BASENAME}.zip`;
const XINCLTS_FILE_FULLNAME: string = `resources/${XINCLTS_FILENAME}`;
const XINCLTS_DIR_FULLNAME: string = `resources/${XINCLTS_BASENAME}`;
// The original content is already lost, but it can be retrieved from the Wayback Machine.
const XINCLTS_URL: string =
    `https://www.w3.org/XML/Test/XInclude/${XINCLTS_FILENAME}`;

const cwd = Deno.cwd();
const manifest = path.resolve(Deno.cwd(), "Cargo.toml");
if (path.basename(cwd) !== "anyxml" || !await exists(manifest)) {
    console.error("Please execute crate root (anyxml/ directory).");
    Deno.exit(1);
}

if (!await exists(XINCLTS_FILE_FULLNAME)) {
    const resp = await fetch(XINCLTS_URL);
    console.log(`successfully downloaded ${XINCLTS_URL}`);
    const xmlts_zip = await resp.bytes();
    await Deno.writeFile(XINCLTS_FILE_FULLNAME, xmlts_zip);
    console.log(`successfully made ${XINCLTS_FILE_FULLNAME}`);
}
if (await exists(XINCLTS_DIR_FULLNAME)) {
    console.log(`remove ${XINCLTS_DIR_FULLNAME}`);
    await Deno.remove(XINCLTS_DIR_FULLNAME, { recursive: true });
}

const dir = await unzipper.Open.file(XINCLTS_FILE_FULLNAME);
await dir.extract({ path: tempdir });
console.log(`successfully extracted ${XINCLTS_FILE_FULLNAME} to ${tempdir}`);

await copy(`${tempdir}/${XINCLTS_BASENAME}`, XINCLTS_DIR_FULLNAME);
console.log(
    `successfully renamed ${tempdir}/${XINCLTS_BASENAME} to ${XINCLTS_DIR_FULLNAME}`,
);
await Deno.remove(tempdir, { recursive: true });
console.log(`remove ${tempdir}`);
