const ENCODINGS: string[] = [
    "iso-8859-2",
    "iso-8859-3",
    "iso-8859-4",
    "iso-8859-5",
    "iso-8859-6",
    "iso-8859-7",
    "iso-8859-8",
    "iso-8859-9",
    "iso-8859-10",
    "iso-8859-11",
    "iso-8859-13",
    "iso-8859-14",
    "iso-8859-15",
    "iso-8859-16",
];

async function generageISO8859Table(encName: string): Promise<string> {
    const upperEncName = encName.toUpperCase().replaceAll("-", "_");
    const encID = encName.split("-")[2];
    const mappingData = await fetch(
        `https://www.unicode.org/Public/MAPPINGS/ISO8859/8859-${encID}.TXT`,
    ).then((resp) => resp.text());
    const fromISO: number[] = Array(256);
    fromISO.fill(-1);
    const toISO: number[][] = [];
    for (const line of mappingData.split("\n").map((line) => line.trim())) {
        if (line.startsWith("#")) {
            continue;
        }
        const map = line.split(/\s/).filter((data) => data.length != 0);
        if (map.length < 2) {
            continue;
        }

        const iso = parseInt(map[0], 16);
        const unicode = parseInt(map[1], 16);
        if (iso >= 128) {
            fromISO[iso] = unicode;
            toISO.push([unicode, iso]);
        }
    }
    toISO.sort((l, r) => l[0] - r[0]);

    let buffer: string = `const ${upperEncName}_TO_UNICODE: [char; 128] = [`;
    for (let i = 128; i < 256; i++) {
        if (fromISO[i] < 0) {
            buffer += `char::REPLACEMENT_CHARACTER,`;
        } else {
            buffer += `'\\u{${fromISO[i].toString(16)}}',`;
        }
    }
    buffer += "];\n\n";
    buffer += `const UNICODE_TO_${upperEncName}: &[(u16, u8)] = &[`;
    for (const [from, to] of toISO) {
        if (from >= 0) {
            buffer += `(${from}, ${to}),`;
        }
    }
    buffer += "];\n";
    return buffer;
}

const result = await Promise.all(ENCODINGS.map(generageISO8859Table));
for (const buf of result) {
    console.log(buf);
}
