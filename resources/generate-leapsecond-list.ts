// deno run -A resources/generate-leapsecond-list.ts

import * as path from "jsr:@std/path@1.1.0";
import { exists } from "jsr:@std/fs@1.0.18";

const IANA_LIST_URL: string =
    "https://data.iana.org/time-zones/tzdb/leap-seconds.list";
const listText = await fetch(IANA_LIST_URL).then((res) => res.text());

function convert_to_leapsecond(
    year: string,
    month: string,
    day: string,
): [number, number, number] {
    const y = parseInt(year);
    const d = parseInt(day);

    if (month == "Jan" && d == 1) {
        return [y - 1, 12, 31];
    } else if (month == "Jul" && d == 1) {
        return [y, 6, 30];
    } else {
        console.error(
            `unknown leapsecond: year: ${year}, month: ${month}, day: ${day}`,
        );
        Deno.exit(1);
    }
}

const cwd = Deno.cwd();
const manifest = path.resolve(Deno.cwd(), "Cargo.toml");
if (path.basename(cwd) !== "anyxml" || !await exists(manifest)) {
    console.error("Please execute crate root (anyxml/ directory).");
    Deno.exit(1);
}

let previousDTAI = 0;
const insertion: [number, number, number][] = [];
const removal: [number, number, number][] = [];
for (
    const line of listText.split("\n").map((line) => line.trim()).filter(
        (line) => !line.startsWith("#") && line.length != 0,
    )
) {
    const [_ntpTime, dtaiStr, _sep, day, month, year] = line.split(" ")
        .filter((e) => e.length != 0);

    const dtai = parseInt(dtaiStr);
    if (previousDTAI < dtai) {
        insertion.push(convert_to_leapsecond(year, month, day));
    } else {
        removal.push(convert_to_leapsecond(year, month, day));
    }
    previousDTAI = dtai;
}

await Promise.all([
    Deno.writeTextFile(
        "anyxml-datetime/resources/inserted-leapseconds.rs",
        `&[${insertion.map(([y, m, d]) => `(${y}, ${m}, ${d})`).join(",")}]`,
    ),
    Deno.writeTextFile(
        "anyxml-datetime/resources/removed-leapseconds.rs",
        `&[${removal.map(([y, m, d]) => `(${y}, ${m}, ${d})`).join(",")}]`,
    ),
]);
