#!/usr/bin/env -S cargo +nightly -Zscript
---cargo
[package]
edition = "2024"
[dependencies]
tokio = { version = "=1.52.3", features = ["full"] }
reqwest = { version = "=0.13.3" }
---

// or https://datatracker.ietf.org/doc/html/rfc1489
const MAPPING_TABLE_URL: &str = "https://www.unicode.org/Public/MAPPINGS/VENDORS/MISC/KOI8-R.TXT";

async fn get_cns11643() -> Result<(), Box<dyn std::error::Error>> {
    let text = reqwest::get(MAPPING_TABLE_URL).await?.text().await?;

    let mut table = vec![];
    for line in text.lines() {
        let line = line.trim();
        if line.is_empty() || line.starts_with("#") {
            continue;
        }

        let seg = line
            .split_ascii_whitespace()
            .filter(|s| !s.is_empty())
            .collect::<Vec<_>>();
        let koi = seg[0]
            .strip_prefix("0x")
            .or_else(|| seg[0].strip_prefix("0X"))
            .unwrap_or(seg[0]);
        let ucs = seg[1]
            .strip_prefix("0x")
            .or_else(|| seg[1].strip_prefix("0X"))
            .unwrap_or(seg[1]);
        let koi = u16::from_str_radix(koi, 16)?;
        let ucs = u16::from_str_radix(ucs, 16)?;
        if koi < 0x80 {
            assert_eq!(koi, ucs);
            continue;
        }

        table.push((koi, ucs));
    }

    table.sort_unstable();
    assert_eq!(table.len(), 128);
    assert_eq!(table[0].0, 0x80);
    assert_eq!(table[127].0, 0xFF);

    println!("const KOI8R_TO_UCS: [u16; 128] = [");
    for &(_, ucs) in &table {
        print!("0x{:04X},", ucs);
    }
    println!("];");

    table.sort_unstable_by_key(|t| t.1);
    println!("const UCS_TO_KOI8R: [(u16, u8); 128] = [");
    for (koi, ucs) in table {
        println!("(0x{:04X}, 0x{:X}),", ucs, koi);
    }
    println!("];");
    Ok(())
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cwd = std::env::current_dir().unwrap();
    assert!(cwd.ends_with("anyxml"));
    get_cns11643().await?;
    Ok(())
}
