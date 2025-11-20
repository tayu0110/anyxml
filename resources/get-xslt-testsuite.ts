// deno run -A resources/get-xslt-testsuite.ts
import * as path from "jsr:@std/path@1.1.0";
import { copy, exists } from "jsr:@std/fs@1.0.18";
import unzipper from "npm:unzipper@0.12.3";

const tempdir = await Deno.makeTempDir();
const XSLTTS_BASENAME: string = `XSLT-testsuite-04`;
const XSLTTS_FILENAME: string = `${XSLTTS_BASENAME}.ZIP`;
const XSLTTS_FILE_FULLNAME: string = `resources/${XSLTTS_FILENAME}`;
const XSLTTS_DIR_FULLNAME: string = `resources/${XSLTTS_BASENAME}`;
// The original content is already lost, but it can be retrieved from the Wayback Machine.
const XSLTTS_URL: string =
    `https://web.archive.org/web/20240412143937/https://www.oasis-open.org/committees/download.php/12171/${XSLTTS_FILENAME}`;

const cwd = Deno.cwd();
const manifest = path.resolve(Deno.cwd(), "Cargo.toml");
if (path.basename(cwd) !== "anyxml" || !await exists(manifest)) {
    console.error("Please execute crate root (anyxml/ directory).");
    Deno.exit(1);
}

if (!await exists(XSLTTS_FILE_FULLNAME)) {
    const resp = await fetch(XSLTTS_URL);
    console.log(`successfully downloaded ${XSLTTS_URL}`);
    const xmlts_zip = await resp.bytes();
    await Deno.writeFile(XSLTTS_FILE_FULLNAME, xmlts_zip);
    console.log(`successfully made ${XSLTTS_FILE_FULLNAME}`);
}
if (await exists(XSLTTS_DIR_FULLNAME)) {
    console.log(`remove ${XSLTTS_DIR_FULLNAME}`);
    await Deno.remove(XSLTTS_DIR_FULLNAME, { recursive: true });
}

const dir = await unzipper.Open.file(XSLTTS_FILE_FULLNAME);
await dir.extract({ path: tempdir });
console.log(`successfully extracted ${XSLTTS_FILE_FULLNAME} to ${tempdir}`);

await copy(`${tempdir}/testsuite`, XSLTTS_DIR_FULLNAME);
console.log(
    `successfully renamed ${tempdir}/testsuite to ${XSLTTS_DIR_FULLNAME}`,
);
await Deno.remove(tempdir, { recursive: true });
console.log(`remove ${tempdir}`);
