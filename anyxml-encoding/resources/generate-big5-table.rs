#!/usr/bin/env -S cargo +nightly -Zscript
---cargo
[package]
edition = "2024"
[dependencies]
tokio = { version = "=1.52.3", features = ["full"] }
reqwest = { version = "=0.13.3" }
zip = { version = "8.6.0" }
---

use std::io::Cursor;

// https://data.gov.tw/dataset/5961
const MAPPING_TABLE_URL: &str = "https://www.cns11643.gov.tw/opendata/MapingTables.zip";

fn cns2table(
    content: &str,
    buf: &mut Vec<(u16, u16, u32)>,
) -> Result<(), Box<dyn std::error::Error>> {
    for line in content.lines().map(|l| l.trim()) {
        if line.is_empty() {
            continue;
        }
        let (cns, big5) = line.split_once(|c: char| c.is_ascii_whitespace()).unwrap();
        let (cns_men, cns_kukaku) = cns.split_once('-').unwrap();
        buf.push((
            cns_men.parse::<u16>()?,
            u16::from_str_radix(cns_kukaku, 16)?,
            u32::from_str_radix(big5, 16)?,
        ));
    }
    Ok(())
}

fn make_table(from: &[(u16, u16, u32)], to: &[(u16, u16, u32)]) -> Vec<(u32, u32)> {
    let mut ret = vec![];
    for &(men, kukaku, code) in from {
        if let Ok(pos) = to.binary_search_by_key(&(men, kukaku), |v| (v.0, v.1)) {
            ret.push((code, to[pos].2));
        }
    }
    ret.sort_unstable();
    ret
}

async fn get_cns11643() -> Result<(), Box<dyn std::error::Error>> {
    let bytes = reqwest::get(MAPPING_TABLE_URL)
        .await?
        .bytes()
        .await?
        .to_vec();

    let mut zip = zip::ZipArchive::new(Cursor::new(bytes.as_slice()))?;
    let mut cns2big5_names = vec![];
    let mut cns2ucs_names = vec![];
    for name in zip.file_names() {
        if name.starts_with("Big5/CNS2BIG5") {
            cns2big5_names.push(name.to_owned());
        } else if name.starts_with("Unicode/CNS2UNICODE") {
            cns2ucs_names.push(name.to_owned());
        }
    }
    assert_eq!(cns2big5_names.len(), 4, "{cns2big5_names:?}");
    assert_eq!(cns2ucs_names.len(), 4, "{cns2ucs_names:?}");

    let mut cns2big5 = vec![];
    let mut cns2big5e = vec![];
    for name in cns2big5_names {
        let file = zip.by_name(&name)?;
        let content = std::io::read_to_string(file)?;
        if name.contains("Big5E") {
            cns2table(&content, &mut cns2big5e)?;
        } else {
            cns2table(&content, &mut cns2big5)?;
        }
    }
    cns2big5.sort_unstable();
    cns2big5e.sort_unstable();

    let mut cns2ucs = vec![];
    for name in cns2ucs_names {
        let file = zip.by_name(&name)?;
        let content = std::io::read_to_string(file)?;
        cns2table(&content, &mut cns2ucs)?;
    }
    cns2ucs.sort_unstable();

    let big52ucs = make_table(&cns2big5, &cns2ucs);
    let ucs2big5 = make_table(&cns2ucs, &cns2big5);

    println!("const BIG5_TO_UCS: &[(u16, u16)] = &[");
    for (big5, ucs) in big52ucs {
        println!("    (0x{big5:X}, 0x{ucs:X}),");
    }
    println!("];");

    println!("const UCS_TO_BIG5: &[(u16, u16)] = &[");
    for (ucs, big5) in ucs2big5 {
        println!("    (0x{ucs:X}, 0x{big5:X}),");
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
