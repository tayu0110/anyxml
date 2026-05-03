#!/usr/bin/env -S cargo +nightly -Zscript
---cargo
[package]
edition = "2024"
[dependencies]
tokio = { version = "=1.52.1", features = ["full"] }
reqwest = { version = "=0.13.3" }
convert_case = { version = "=0.11.0" }
---

use std::{
    fmt::Write as _,
    io::Write as _,
    process::{Command, Stdio},
};

use convert_case::ccase;

const APPLICATION: &str = "https://www.iana.org/assignments/media-types/application.csv";
const AUDIO: &str = "https://www.iana.org/assignments/media-types/audio.csv";
const FONT: &str = "https://www.iana.org/assignments/media-types/font.csv";
const HAPTICS: &str = "https://www.iana.org/assignments/media-types/haptics.csv";
const IMAGE: &str = "https://www.iana.org/assignments/media-types/image.csv";
const MESSAGE: &str = "https://www.iana.org/assignments/media-types/message.csv";
const MODEL: &str = "https://www.iana.org/assignments/media-types/model.csv";
const MULTIPART: &str = "https://www.iana.org/assignments/media-types/multipart.csv";
const TEXT: &str = "https://www.iana.org/assignments/media-types/text.csv";
const VIDEO: &str = "https://www.iana.org/assignments/media-types/video.csv";
const UE: &str = concat!(env!("CARGO_PKG_NAME"), "/", env!("CARGO_PKG_VERSION"),);

async fn subtypes(typename: &str, url: Option<&str>) -> Result<String, Box<dyn std::error::Error>> {
    let mut def = format!(
        "#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]\npub enum {typename} {{\n"
    );
    let mut buf = vec![];
    if let Some(url) = url {
        let client = reqwest::Client::builder().user_agent(UE).build()?;
        let csv = client.get(url).send().await?.text().await?;
        for line in csv.lines().skip(1) {
            let [name, template, _reference, ..] = line.split(',').collect::<Vec<&str>>()[..]
            else {
                unreachable!("line: {line}")
            };
            let template = template.trim().split_once('/').unwrap().1;
            let mut variant = ccase!(
                pascal,
                name.split_once('(')
                    .map(|s| s.0)
                    .unwrap_or(name)
                    .trim()
                    .replace('+', "_plus_")
                    .replace('.', "_dot_")
                    .replace(|c: char| !c.is_ascii_alphanumeric(), "_")
            );
            if variant.starts_with(|c: char| c.is_ascii_digit()) {
                variant.insert(0, '_');
            }

            buf.push((variant, template.to_owned()));
        }
        buf.sort_unstable_by_key(|b| b.1.to_ascii_lowercase());
        for (variant, template) in &buf {
            writeln!(def, "/// {template}\n{variant},")?;
        }
    }
    writeln!(def, "/// private\nPrivate(PrivateSubtype),\n}}")?;
    if buf.len() > 20 {
        binary_search_strategy(typename, &buf, &mut def)?;
    } else {
        lenear_strategy(typename, &buf, &mut def)?;
    }
    Ok(def)
}

fn lenear_strategy(
    typename: &str,
    buf: &[(String, String)],
    def: &mut String,
) -> Result<(), Box<dyn std::error::Error>> {
    writeln!(def, "impl {} {{", typename)?;
    writeln!(def, "    pub fn to_str(&self) -> &str {{")?;
    writeln!(def, "        match self {{")?;
    for (variant, template) in buf {
        writeln!(
            def,
            "{}Self::{variant} => \"{}\",",
            " ".repeat(12),
            template
        )?;
    }
    writeln!(def, "            Self::Private(sub) => sub.to_str(),")?;
    writeln!(def, "        }}")?;
    writeln!(def, "    }}")?;
    writeln!(def, "}}")?;
    writeln!(def, "impl FromStr for {} {{", typename)?;
    writeln!(def, "    type Err = crate::MediaTypeError;")?;
    writeln!(def, "    fn from_str(s:&str)->Result<Self,Self::Err>{{")?;
    for (variant, template) in buf {
        writeln!(
            def,
            "if s.eq_ignore_ascii_case(\"{}\") {{ return Ok(Self::{}); }}",
            template, variant
        )?;
    }
    writeln!(def, "            Ok(Self::Private(s.parse()?))")?;
    writeln!(def, "    }}")?;
    writeln!(def, "}}")?;
    Ok(())
}

fn binary_search_strategy(
    typename: &str,
    buf: &[(String, String)],
    def: &mut String,
) -> Result<(), Box<dyn std::error::Error>> {
    let pre = ccase!(constant, typename);
    let vlist = format!("{}_VLIST", pre);
    let slist = format!("{}_SLIST", pre);
    writeln!(def, "const {}: &[{}] = &[", vlist, typename)?;
    for (variant, _) in buf {
        writeln!(def, "    {}::{},", typename, variant)?;
    }
    writeln!(def, "];")?;
    writeln!(def, "const {}: &[&str] = &[", slist)?;
    for (_, template) in buf {
        writeln!(def, "    \"{}\",", template)?;
    }
    writeln!(def, "];")?;
    writeln!(def, "impl {} {{", typename)?;
    writeln!(def, "    pub fn to_str(&self) -> &str {{")?;
    writeln!(def, "        if let Self::Private(sub) = self {{")?;
    writeln!(def, "            return sub.to_str()")?;
    writeln!(def, "        }}")?;
    writeln!(def, "        let p = {vlist}.binary_search(self).unwrap();")?;
    writeln!(def, "        {}[p]", slist)?;
    writeln!(def, "    }}")?;
    writeln!(def, "}}")?;
    writeln!(def, "impl FromStr for {} {{", typename)?;
    writeln!(def, "    type Err = MediaTypeError;")?;
    writeln!(def, "    fn from_str(s: &str) -> Result<Self,Self::Err> {{")?;
    writeln!(def, "        let lower = s.to_ascii_lowercase();")?;
    writeln!(
        def,
        "        if let Ok(pos) = {}.binary_search_by_key(&lower, |t| t.to_ascii_lowercase()) {{",
        slist
    )?;
    writeln!(def, "            return Ok({}[pos].clone());", vlist)?;
    writeln!(def, "        }}")?;
    writeln!(def, "        Ok(Self::Private(s.parse()?))")?;
    writeln!(def, "    }}")?;
    writeln!(def, "}}")?;
    Ok(())
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let (app, audio, example, font, hap, image, mess, model, multi, text, video) = tokio::try_join!(
        subtypes("ApplicationSubtype", Some(APPLICATION)),
        subtypes("AudioSubtype", Some(AUDIO)),
        subtypes("ExampleSubtype", None),
        subtypes("FontSubtype", Some(FONT)),
        subtypes("HapticsSubtype", Some(HAPTICS)),
        subtypes("ImageSubtype", Some(IMAGE)),
        subtypes("MessageSubtype", Some(MESSAGE)),
        subtypes("ModelSubtype", Some(MODEL)),
        subtypes("MultipartSubtype", Some(MULTIPART)),
        subtypes("TextSubtype", Some(TEXT)),
        subtypes("VideoSubtype", Some(VIDEO)),
    )?;

    let mut rustfmt = Command::new("rustfmt")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()?;
    let mut stdin = rustfmt.stdin.take().unwrap();
    writeln!(stdin, "use std::str::FromStr;")?;
    writeln!(stdin)?;
    writeln!(stdin, "use crate::{{MediaTypeError, PrivateSubtype}};")?;
    writeln!(stdin)?;
    writeln!(stdin, "{app}")?;
    writeln!(stdin, "{audio}")?;
    writeln!(stdin, "{example}")?;
    writeln!(stdin, "{font}")?;
    writeln!(stdin, "{hap}")?;
    writeln!(stdin, "{image}")?;
    writeln!(stdin, "{mess}")?;
    writeln!(stdin, "{model}")?;
    writeln!(stdin, "{multi}")?;
    writeln!(stdin, "{text}")?;
    writeln!(stdin, "{video}")?;
    drop(stdin);
    let output = rustfmt.wait_with_output()?;
    let stdout = String::from_utf8(output.stdout)?;
    println!("{stdout}");
    Ok(())
}
