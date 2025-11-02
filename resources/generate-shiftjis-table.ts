const ENCODING_NAME: string = "Shift_JIS";

const upperEncName = ENCODING_NAME.toUpperCase().replaceAll("-", "_");
const mappingData = await fetch(
    "https://www.unicode.org/Public/MAPPINGS/OBSOLETE/EASTASIA/JIS/SHIFTJIS.TXT",
).then((resp) => resp.text());

const singleByteArea: number[] = new Array(256);
singleByteArea.fill(-1);
const multiByteArea1: number[][] = new Array(31);
const multiByteArea2: number[][] = new Array(29);
const toShiftJIS: number[][] = [];

for (let i = 0; i < 0x20; i++) {
    singleByteArea[i] = i;
    toShiftJIS.push([i, i]);

    if (i < multiByteArea1.length) {
        multiByteArea1[i] = new Array(192);
        multiByteArea1[i].fill(-1);
    }
    if (i < multiByteArea2.length) {
        multiByteArea2[i] = new Array(192);
        multiByteArea2[i].fill(-1);
    }
}
singleByteArea[0x7F] = 0x7F;
toShiftJIS.push([0x7F, 0x7F]);

for (const line of mappingData.split("\n").map((line) => line.trim())) {
    if (line.startsWith("#")) {
        continue;
    }
    const map = line.split(/\s/).filter((data) => data.length != 0);
    if (map.length < 2) {
        continue;
    }

    const shiftjis = parseInt(map[0], 16);
    const unicode = parseInt(map[1], 16);

    if (shiftjis < 256) {
        singleByteArea[shiftjis] = unicode;
    } else {
        const upper = shiftjis >> 8;
        const lower = shiftjis & 0xFF;
        if (upper < 0xA0) {
            multiByteArea1[upper - 0x81][lower - 0x40] = unicode;
        } else {
            multiByteArea2[upper - 0xE0][lower - 0x40] = unicode;
        }
    }
    toShiftJIS.push([unicode, shiftjis]);
}
for (const table of [multiByteArea1, multiByteArea2]) {
    while (table.length > 0 && table[table.length - 1].every((b) => b < 0)) {
        table.pop();
    }
}

toShiftJIS.sort((l, r) => l[0] - r[0]);

let buffer: string =
    `const ${upperEncName}_SINGLEBYTE_TO_UNICODE: [char; 256] = [`;
for (let i = 0; i < 256; i++) {
    if (singleByteArea[i] < 0) {
        buffer += `char::REPLACEMENT_CHARACTER,`;
    } else {
        buffer += `'\\u{${singleByteArea[i].toString(16)}}',`;
    }
}
buffer += "];\n\n";

[multiByteArea1, multiByteArea2].forEach((mtable, i) => {
    buffer +=
        `const ${upperEncName}_MULTIBYTE${i}_TO_UNICODE: [&[char]; ${mtable.length}] = [`;
    for (const table of mtable) {
        buffer += "&[";
        for (const codepoint of table) {
            if (codepoint < 0) {
                buffer += `char::REPLACEMENT_CHARACTER,`;
            } else {
                buffer += `'\\u{${codepoint.toString(16)}}',`;
            }
        }
        buffer += "],\n";
    }
    buffer += "];\n\n";
});

buffer += `const UNICODE_TO_${upperEncName}: &[(u16, u16)] = &[`;
for (const [from, to] of toShiftJIS) {
    if (from >= 0) {
        buffer += `(${from}, ${to}),`;
    }
}
buffer += "];\n";

console.log(buffer);
