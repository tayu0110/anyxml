#!/usr/bin/env -S cargo +nightly -Zscript
---cargo
[package]
edition = "2024"
[dependencies]
tokio = { version = "=1.52.3", features = ["full"] }
reqwest = { version = "=0.13.3" }
---

use std::process::Stdio;

use tokio::process::Command;

// https://www.w3.org/XML/2004/xml-schema-test-suite/index.html
const XSTS_URL: &str =
    "https://www.w3.org/XML/2004/xml-schema-test-suite/xmlschema2006-11-06/xsts-2007-06-20.tar.gz";
const XSTS_FILENAME: &str = "resources/xsts-2007-06-20.tar.gz";
const XSTS_EXPANDED_DIRNAME: &str = "resources/xmlschema2006-11-06";

async fn get_xsts() -> Result<(), Box<dyn std::error::Error>> {
    tokio::try_join!(
        tokio::fs::remove_dir_all(XSTS_EXPANDED_DIRNAME),
        tokio::fs::remove_file(XSTS_FILENAME)
    )
    .ok();
    let bytes = reqwest::get(XSTS_URL).await?.bytes().await?;
    tokio::fs::write(XSTS_FILENAME, bytes).await?;
    let _ = Command::new("tar")
        .arg("xvf")
        .arg(XSTS_FILENAME)
        .arg("-C")
        .arg("resources/")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .output()
        .await?;
    Ok(())
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cwd = std::env::current_dir().unwrap();
    assert!(cwd.ends_with("anyxml"));
    get_xsts().await?;
    Ok(())
}
