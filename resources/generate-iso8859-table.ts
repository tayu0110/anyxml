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

for (const encName of ENCODINGS) {
    const upperEncName: string = encName.toUpperCase().replaceAll("-", "_");

    console.log(`const ${upperEncName}_TO_UNICODE: [char; 128] = [`);
    const decoder = new TextDecoder(encName, { fatal: true });
    const fromISO: number[] = Array(256);
    const buffer = new Uint8Array(1);
    let maxCodePoint = 0;
    for (let i = 128; i < 256; i++) {
        buffer[0] = i;
        try {
            const c = decoder.decode(buffer);
            const codePoint: number = Array.from(c).map((char) =>
                char.codePointAt(0)
            )[0] as number;
            console.log(`'\\u{${codePoint.toString(16)}}',`);
            fromISO[i] = codePoint;
            maxCodePoint = Math.max(maxCodePoint, fromISO[i]);
        } catch {
            console.log("char::REPLACEMENT_CHARACTER,");
            fromISO[i] = -1;
        }
    }
    console.log("];");

    const toISO: number[][] = [];
    for (let i = 128; i < 256; i++) {
        toISO.push([fromISO[i], i]);
    }
    toISO.sort((l, r) => l[0] - r[0]);
    console.log(`const UNICODE_TO_${upperEncName}: &[(u16, u8)] = &[`);
    for (const [from, to] of toISO) {
        if (from >= 0) {
            console.log(`(${from}, ${to}),`);
        }
    }
    console.log("];");
}
