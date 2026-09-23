#!/usr/bin/env -S cargo +nightly -Zscript
---cargo
[package]
edition = "2024"
[dependencies]
tokio = { version = "=1.52.3", features = ["full"] }
reqwest = { version = "=0.13.3" }
zip = { version = "8.6.0" }
anyxml-base64 = { version = "0.1.1" }
---

use anyxml_base64::Base64Binary;
use tokio::{fs::File, io::AsyncWriteExt};

// UNICODE mapping table source is lost now.
// Since I can't find the source in raw text format, I'll extract the data from the HTML source.
const MAPPING_TABLE_URL: &str = "https://android.googlesource.com/platform/external/python/cpython3/+/24cdfe16599af4bed00f790584d696493c45afab/Tools/unicode/python-mappings/GB2312.TXT?format=TEXT";

async fn get_gb2312() -> Result<(), Box<dyn std::error::Error>> {
    let text = reqwest::get(MAPPING_TABLE_URL).await?.text().await?;

    let contents = Base64Binary::from_encoded(text.bytes(), false)?
        .decode()
        .map(|c| c as char)
        .collect::<String>();

    let mut file = File::options()
        .create(true)
        .write(true)
        .open("anyxml-encoding/resources/GB2312.TXT")
        .await?;
    file.write_all(contents.as_bytes()).await?;
    file.flush().await?;
    let mut from_ucs = vec![];
    let mut to_ucs = [[char::REPLACEMENT_CHARACTER; 96]; 96];

    for line in contents.lines() {
        let line = line.trim();

        if line.is_empty() || line.starts_with("#") {
            continue;
        }

        let segs = line
            .split_ascii_whitespace()
            .filter(|s| !s.is_empty())
            .collect::<Vec<_>>();
        let gb2312 = u16::from_str_radix(segs[0].strip_prefix("0x").unwrap(), 16)?;
        let ucs = u16::from_str_radix(segs[1].strip_prefix("0x").unwrap(), 16)?;

        from_ucs.push((ucs, gb2312));
        let ku = (gb2312 as usize - 0x2020) >> 8;
        let ten = (gb2312 as usize - 0x2020) & 0xFF;
        to_ucs[ku][ten] = char::from_u32(ucs as u32).unwrap();
    }

    from_ucs.sort_unstable();
    println!("const UCS_TO_GB2312: &[(u16, u16)] = &[");
    for (ucs, gb2312) in from_ucs {
        println!("(0x{:X}, 0x{:X}),", ucs, gb2312);
    }
    println!("];");

    println!("const GB2312_TO_UCS: [&[char]; 96] = [");
    for row in to_ucs {
        print!("&[");
        if row.iter().any(|&c| c != char::REPLACEMENT_CHARACTER) {
            for c in row {
                if c == char::REPLACEMENT_CHARACTER {
                    print!("char::REPLACEMENT_CHARACTER,");
                } else {
                    print!("'\\u{{{:X}}}',", c as u32);
                }
            }
        }
        println!("],");
    }
    println!("];");

    Ok(())
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cwd = std::env::current_dir().unwrap();
    assert!(cwd.ends_with("anyxml"));
    get_gb2312().await?;
    Ok(())
}
