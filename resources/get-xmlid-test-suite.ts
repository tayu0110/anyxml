// deno run -A resources/get-xmlid-test-suite.ts
import * as path from "jsr:@std/path@1.1.0";
import { exists } from "jsr:@std/fs@1.0.18";
import { DOMParser } from "npm:xmldom@0.6.0";

const MANIFEST_BASE_URI: string = "https://www.w3.org/XML/2005/01/xml-id";
const MANIFEST_URI: string = `${MANIFEST_BASE_URI}/test-suite.xml`;
const DIR_NAME: string = "resources/xml-id";

const cwd = Deno.cwd();
const manifest = path.resolve(Deno.cwd(), "Cargo.toml");
if (path.basename(cwd) !== "anyxml" || !await exists(manifest)) {
    console.error("Please execute crate root (anyxml/ directory).");
    Deno.exit(1);
}

if (await exists(DIR_NAME)) {
    console.log(`remove ${DIR_NAME}`);
    await Deno.remove(DIR_NAME, { recursive: true });
}

await Deno.mkdir(`${DIR_NAME}/tests`, {
    recursive: true,
});
const resp = await fetch(MANIFEST_URI);
const test_manifest: string = await resp.text();
await Deno.writeTextFile(`${DIR_NAME}/test-suite.xml`, test_manifest);

const doc = new DOMParser().parseFromString(test_manifest, "text/xml");
console.error(doc);
const test_cases = doc.getElementsByTagName("test-case");
for (let i = 0; i < test_cases.length; i++) {
    const test_case = test_cases.item(i);
    const dir = test_case.getElementsByTagName("file-path")[0].textContent;
    const input_file =
        test_case.getElementsByTagName("input-file")[0].textContent;

    const resp = await fetch(`${MANIFEST_BASE_URI}/${dir}/${input_file}`);
    const test_file: string = await resp.text();
    await Deno.writeTextFile(`${DIR_NAME}/tests/${input_file}`, test_file);
}
