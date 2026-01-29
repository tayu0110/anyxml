//! Provide a unified interface for encoders and decoders,
//! and a default implementation for some encodings.
//!
//! If it is necessary to provide a custom decoder for XML processor, a type implementing
//! the [`Decoder`] trait can be registered using the [`register_decoder`] function.
//!
//! By default, the encoding name provided to the [`register_encoder`] or [`register_decoder`]
//! function is used to search for encoders and decoders.  \
//! If it is necessary to assign multiple names to a single encoder or decoder,
//! it is possible to set aliases for encoding names using [`register_encoding_alias`].
//!
//! The default encoding names and aliases are based on
//! [IANA registrations](https://www.iana.org/assignments/character-sets/character-sets.xhtml).

mod ebcdic;
mod iso_8859;
mod shift_jis;
mod ucs4;
mod us_ascii;
mod utf16;
mod utf8;

use std::{
    borrow::Cow,
    collections::BTreeMap,
    sync::{LazyLock, RwLock},
};

pub use ebcdic::*;
pub use iso_8859::*;
pub use shift_jis::*;
pub use ucs4::*;
pub use us_ascii::*;
pub use utf8::*;
pub use utf16::*;

#[derive(Debug, Clone)]
pub enum EncodeError {
    /// Input buffer is empty.
    InputIsEmpty,
    /// The length of the output buffer is too short.  
    /// If this error is returned, it is guaranteed that the encoder is consuming the input buffer.
    OutputTooShort,
    /// A UTF-8 character `c` cannot map any codepoints of the target encoding.
    ///
    /// The input and output buffer have consumed `read` and `write` bytes respectively.  
    /// `read` includes the length of `c`. Thus, the correctly read length is `read - c.len_utf8()`.  
    /// `write` does not include the length of `c` because encoder cannot write unmapped characters.
    Unmappable { read: usize, write: usize, c: char },
    /// Other errors.
    Other { msg: Cow<'static, str> },
}

impl std::fmt::Display for EncodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl std::error::Error for EncodeError {}

pub trait Encoder {
    fn name(&self) -> &'static str;
    /// If no error occurs, return `Ok((read_bytes, write_bytes))`.
    fn encode(
        &mut self,
        src: &str,
        dst: &mut [u8],
        finish: bool,
    ) -> Result<(usize, usize), EncodeError>;
}

#[derive(Debug, Clone)]
pub enum DecodeError {
    /// Input buffer is empty.
    InputIsEmpty,
    /// The length of the output buffer is too short.  
    /// If this error is returned, it is guaranteed that the decoder is consuming the input buffer.
    OutputTooShort,
    /// Malformed byte sequence is found.  
    ///
    /// The input and output buffer have consumed `read` and `write` bytes respectively.  
    /// Malformed sequence occurs `input[read-length-offset..read-offset]`.  
    Malformed {
        read: usize,
        write: usize,
        length: usize,
        offset: usize,
    },
    /// Other errors.
    Other { msg: Cow<'static, str> },
}

impl std::fmt::Display for DecodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl std::error::Error for DecodeError {}

pub trait Decoder {
    fn name(&self) -> &'static str;
    /// If no error occurs, return `Ok((read_bytes, write_bytes))`.
    fn decode(
        &mut self,
        src: &[u8],
        dst: &mut String,
        finish: bool,
    ) -> Result<(usize, usize), DecodeError>;
}

/// Supported encodings.  
///
/// Encoding names are listed in lexical order.
pub const DEFAULT_SUPPORTED_ENCODINGS: &[&str] = {
    const NAMES: &[&str] = &[
        IBM037,
        IBM1026,
        IBM273,
        IBM274,
        IBM275,
        IBM277,
        IBM278,
        IBM280,
        IBM284,
        IBM285,
        IBM290,
        IBM297,
        IBM420,
        IBM423,
        IBM424,
        IBM437,
        IBM500,
        IBM850,
        IBM851,
        IBM852,
        IBM855,
        IBM857,
        IBM860,
        IBM861,
        IBM862,
        IBM863,
        IBM864,
        IBM865,
        IBM868,
        IBM869,
        IBM870,
        IBM871,
        IBM880,
        IBM891,
        IBM903,
        IBM904,
        IBM905,
        IBM918,
        ISO_8859_10_NAME,
        ISO_8859_13_NAME,
        ISO_8859_14_NAME,
        ISO_8859_15_NAME,
        ISO_8859_16_NAME,
        ISO_8859_1_NAME,
        ISO_8859_2_NAME,
        ISO_8859_3_NAME,
        ISO_8859_4_NAME,
        ISO_8859_5_NAME,
        ISO_8859_6_NAME,
        ISO_8859_7_NAME,
        ISO_8859_8_NAME,
        ISO_8859_9_NAME,
        SHIFT_JIS_NAME,
        ISO_8859_11_NAME,
        US_ASCII_NAME,
        UTF16_NAME,
        UTF16BE_NAME,
        UTF16LE_NAME,
        UTF32_NAME,
        UTF32BE_NAME,
        UTF32LE_NAME,
        UTF8_NAME,
    ];
    let len = NAMES.len();
    let mut i = 0;
    while i + 1 < len {
        let x = NAMES[i].as_bytes();
        let y = NAMES[i + 1].as_bytes();
        let mut j = 0;
        while j < x.len() {
            assert!(x[j] <= y[j]);
            if x[j] < y[j] {
                break;
            }
            j += 1;
            if j == x.len() {
                break;
            }
            assert!(j < y.len());
        }
        i += 1;
    }
    NAMES
};
/// Manage aliases for encoding names.
pub static ENCODING_ALIASES: LazyLock<RwLock<BTreeMap<Cow<'static, str>, &'static str>>> =
    LazyLock::new(|| {
        // To perform case-insensitive comparisons, capitalize all aliases.
        RwLock::new(BTreeMap::from([
            ("UTF8".into(), UTF8_NAME),
            ("UTF16".into(), UTF16_NAME),
            ("UTF16BE".into(), UTF16BE_NAME),
            ("UTF16LE".into(), UTF16LE_NAME),
            ("ISO-IR-100".into(), ISO_8859_1_NAME),
            ("ISO_8859-1".into(), ISO_8859_1_NAME),
            ("ISO-8859-1".into(), ISO_8859_1_NAME),
            ("LATIN1".into(), ISO_8859_1_NAME),
            ("L1".into(), ISO_8859_1_NAME),
            ("IBM819".into(), ISO_8859_1_NAME),
            ("CP819".into(), ISO_8859_1_NAME),
            ("ISOLATIN1".into(), ISO_8859_1_NAME),
            ("ISO-IR-101".into(), ISO_8859_2_NAME),
            ("ISO_8859-2".into(), ISO_8859_2_NAME),
            ("ISO-8859-2".into(), ISO_8859_2_NAME),
            ("LATIN2".into(), ISO_8859_2_NAME),
            ("L2".into(), ISO_8859_2_NAME),
            ("ISOLATIN2".into(), ISO_8859_2_NAME),
            ("ISO-IR-109".into(), ISO_8859_3_NAME),
            ("ISO_8859-3".into(), ISO_8859_3_NAME),
            ("ISO-8859-3".into(), ISO_8859_3_NAME),
            ("LATIN3".into(), ISO_8859_3_NAME),
            ("L3".into(), ISO_8859_3_NAME),
            ("ISOLATIN3".into(), ISO_8859_3_NAME),
            ("ISO-IR-110".into(), ISO_8859_4_NAME),
            ("ISO_8859-4".into(), ISO_8859_4_NAME),
            ("ISO-8859-4".into(), ISO_8859_4_NAME),
            ("LATIN4".into(), ISO_8859_4_NAME),
            ("L4".into(), ISO_8859_4_NAME),
            ("ISOLATIN4".into(), ISO_8859_4_NAME),
            ("ISO-IR-144".into(), ISO_8859_5_NAME),
            ("ISO_8859-5".into(), ISO_8859_5_NAME),
            ("ISO-8859-5".into(), ISO_8859_5_NAME),
            ("CYRILLIC".into(), ISO_8859_5_NAME),
            ("ISOLATINCYRILLIC".into(), ISO_8859_5_NAME),
            ("ISO-IR-127".into(), ISO_8859_6_NAME),
            ("ISO_8859-6".into(), ISO_8859_6_NAME),
            ("ISO-8859-6".into(), ISO_8859_6_NAME),
            ("ECMA-114".into(), ISO_8859_6_NAME),
            ("ASMO-708".into(), ISO_8859_6_NAME),
            ("ARABIC".into(), ISO_8859_6_NAME),
            ("ISOLATINARABIC".into(), ISO_8859_6_NAME),
            ("ISO-IR-126".into(), ISO_8859_7_NAME),
            ("ISO_8859-7".into(), ISO_8859_7_NAME),
            ("ISO-8859-7".into(), ISO_8859_7_NAME),
            ("ELOT_928".into(), ISO_8859_7_NAME),
            ("ECMA-118".into(), ISO_8859_7_NAME),
            ("GREEK".into(), ISO_8859_7_NAME),
            ("GREEK8".into(), ISO_8859_7_NAME),
            ("ISOLATINGREEK".into(), ISO_8859_7_NAME),
            ("ISO-IR-138".into(), ISO_8859_8_NAME),
            ("ISO_8859-8".into(), ISO_8859_8_NAME),
            ("ISO-8859-8".into(), ISO_8859_8_NAME),
            ("HEBREW".into(), ISO_8859_8_NAME),
            ("ISOLATINHEBREW".into(), ISO_8859_8_NAME),
            ("ISO-IR-148".into(), ISO_8859_9_NAME),
            ("ISO_8859-9".into(), ISO_8859_9_NAME),
            ("ISO-8859-9".into(), ISO_8859_9_NAME),
            ("LATIN5".into(), ISO_8859_9_NAME),
            ("L5".into(), ISO_8859_9_NAME),
            ("ISOLATIN5".into(), ISO_8859_9_NAME),
            ("ISO-IR-157".into(), ISO_8859_10_NAME),
            ("L6".into(), ISO_8859_10_NAME),
            ("ISO_8859-10:1992".into(), ISO_8859_10_NAME),
            ("ISOLATIN6".into(), ISO_8859_10_NAME),
            ("LATIN6".into(), ISO_8859_10_NAME),
            ("TIS620".into(), ISO_8859_11_NAME),
            ("ISO-8859-11".into(), ISO_8859_11_NAME),
            ("ISO885913".into(), ISO_8859_13_NAME),
            ("ISO-IR-199".into(), ISO_8859_14_NAME),
            ("ISO_8859-14:1998".into(), ISO_8859_14_NAME),
            ("ISO_8859-14".into(), ISO_8859_14_NAME),
            ("LATIN8".into(), ISO_8859_14_NAME),
            ("ISO-CELTIC".into(), ISO_8859_14_NAME),
            ("L8".into(), ISO_8859_14_NAME),
            ("ISO885914".into(), ISO_8859_14_NAME),
            ("ISO_8859-15".into(), ISO_8859_15_NAME),
            ("LATIN-9".into(), ISO_8859_15_NAME),
            ("ISO885915".into(), ISO_8859_15_NAME),
            ("ISO-IR-226".into(), ISO_8859_16_NAME),
            ("ISO_8859-16:2001".into(), ISO_8859_16_NAME),
            ("ISO_8859-16".into(), ISO_8859_16_NAME),
            ("LATIN10".into(), ISO_8859_16_NAME),
            ("L10".into(), ISO_8859_16_NAME),
            ("ISO885916".into(), ISO_8859_16_NAME),
            ("UTF32".into(), UTF32_NAME),
            ("UTF32BE".into(), UTF32BE_NAME),
            ("UTF32LE".into(), UTF32LE_NAME),
            ("MS_KANJI".into(), SHIFT_JIS_NAME),
            ("SHIFTJIS".into(), SHIFT_JIS_NAME),
            ("ISO-IR-6".into(), US_ASCII_NAME),
            ("ANSI_X3.4-1968".into(), US_ASCII_NAME),
            ("ANSI_X3.4-1986".into(), US_ASCII_NAME),
            ("ISO_646.IRV:1991".into(), US_ASCII_NAME),
            ("ISO646-US".into(), US_ASCII_NAME),
            ("US-ASCII".into(), US_ASCII_NAME),
            ("US".into(), US_ASCII_NAME),
            ("IBM367".into(), US_ASCII_NAME),
            ("CP367".into(), US_ASCII_NAME),
            ("ASCII".into(), US_ASCII_NAME),
            ("CP037".into(), IBM037),
            ("EBCDIC-CP-US".into(), IBM037),
            ("EBCDIC-CP-CA".into(), IBM037),
            ("EBCDIC-CP-WT".into(), IBM037),
            ("EBCDIC-CP-NL".into(), IBM037),
            ("CP273".into(), IBM273),
            ("EBCDIC-BE".into(), IBM274),
            ("CP274".into(), IBM274),
            ("EBCDIC-BR".into(), IBM275),
            ("CP275".into(), IBM275),
            ("EBCDIC-CP-DK".into(), IBM277),
            ("EBCDIC-CP-NO".into(), IBM277),
            ("CP278".into(), IBM278),
            ("EBCDIC-CP-FI".into(), IBM278),
            ("EBCDIC-CP-SE".into(), IBM278),
            ("CP280".into(), IBM280),
            ("EBCDIC-CP-IT".into(), IBM280),
            ("CP284".into(), IBM284),
            ("EBCDIC-CP-ES".into(), IBM284),
            ("CP285".into(), IBM285),
            ("EBCDIC-CP-GB".into(), IBM285),
            ("CP290".into(), IBM290),
            ("EBCDIC-JP-KANA".into(), IBM290),
            ("CP297".into(), IBM297),
            ("EBCDIC-CP-FR".into(), IBM297),
            ("CP420".into(), IBM420),
            ("EBCDIC-CP-AR1".into(), IBM420),
            ("CP423".into(), IBM423),
            ("EBCDIC-CP-GR".into(), IBM423),
            ("CP424".into(), IBM424),
            ("EBCDIC-CP-HE".into(), IBM424),
            ("CP437".into(), IBM437),
            ("437".into(), IBM437),
            ("PC8CODEPAGE437".into(), IBM437),
            ("CP500".into(), IBM500),
            ("EBCDIC-CP-BE".into(), IBM500),
            ("EBCDIC-CP-CH".into(), IBM500),
            ("CP851".into(), IBM851),
            ("851".into(), IBM851),
            ("CP852".into(), IBM852),
            ("852".into(), IBM852),
            ("PCP852".into(), IBM852),
            ("CP855".into(), IBM855),
            ("855".into(), IBM855),
            ("CP857".into(), IBM857),
            ("857".into(), IBM857),
            ("CP860".into(), IBM860),
            ("860".into(), IBM860),
            ("CP861".into(), IBM861),
            ("861".into(), IBM861),
            ("CP-IS".into(), IBM861),
            ("CP863".into(), IBM863),
            ("863".into(), IBM863),
            ("CP864".into(), IBM864),
            ("CP865".into(), IBM865),
            ("865".into(), IBM865),
            ("CP868".into(), IBM868),
            ("CP-AR".into(), IBM868),
            ("CP869".into(), IBM869),
            ("869".into(), IBM869),
            ("CP-GR".into(), IBM869),
            ("CP870".into(), IBM870),
            ("EBCDIC-CP-ROECE".into(), IBM870),
            ("EBCDIC-CP-YU".into(), IBM870),
            ("CP871".into(), IBM871),
            ("EBCDIC-CP-IS".into(), IBM871),
            ("CP880".into(), IBM880),
            ("EBCDIC-CYRILLIC".into(), IBM880),
            ("CP891".into(), IBM891),
            ("CP903".into(), IBM903),
            ("CP904".into(), IBM904),
            ("904".into(), IBM904),
            // is this correct ????
            // But since it really says "IBBM", I'll just list it for now...
            ("IBBM904".into(), IBM904),
            ("CP905".into(), IBM905),
            ("EBCDIC-CP-TR".into(), IBM905),
            ("CP918".into(), IBM918),
            ("EBCDIC-CP-AR2".into(), IBM918),
            ("CP1026".into(), IBM1026),
        ]))
    });
/// Register `alias` as an alias for the encoding name `real`.  \
/// If `alias` is already an alias for another encoding name, overwrite it and return
/// the encoding name before the overwrite.
///
/// It is assumed that real names and aliases will be linked based on the IANA list,
/// but this is not required.  \
/// However, since aliases do not redirect multiple times, `real` must be the name registered
/// with the encoder/decoder.
///
/// If an encoding name becomes both a real name and an alias, searches may not work properly.
///
/// Reference: [Charcter sets registered by IANA](https://www.iana.org/assignments/character-sets/character-sets.xhtml)
pub fn register_encoding_alias(alias: &'static str, real: &'static str) -> Option<&'static str> {
    let mut table = ENCODING_ALIASES.write().unwrap();
    if alias.chars().all(|c| c.is_ascii_uppercase()) {
        table.insert(alias.into(), real)
    } else {
        table.insert(alias.to_ascii_uppercase().into(), real)
    }
}
/// Unregister `alias` if it is registerd as an alias for an encoding name.  \
/// If successfully removed, return the real name.
pub fn unregister_encoding_alias(alias: &'static str) -> Option<&'static str> {
    ENCODING_ALIASES
        .write()
        .unwrap()
        .remove(alias.to_ascii_uppercase().as_str())
}
/// Retrieve the encoding name from `alias`, which is an alias for a certain encoding name.  \
/// If retrieval fails, returns [`None`].
///
/// Alias comparisons are case-insensitive.
pub fn resolve_encoding_alias(alias: &str) -> Option<&'static str> {
    let aliases = ENCODING_ALIASES.read().unwrap();
    aliases
        .get(alias)
        .or_else(|| aliases.get(alias.to_ascii_uppercase().as_str()))
        .copied()
}

pub type EncoderFactory = fn() -> Box<dyn Encoder>;
pub static ENCODER_TABLE: LazyLock<RwLock<BTreeMap<&'static str, EncoderFactory>>> =
    LazyLock::new(|| {
        let mut map = BTreeMap::<&'static str, EncoderFactory>::new();
        map.insert(UTF8_NAME, || Box::new(UTF8Encoder));
        map.insert(UTF16_NAME, || Box::new(UTF16Encoder::default()));
        map.insert(UTF16BE_NAME, || Box::new(UTF16BEEncoder));
        map.insert(UTF16LE_NAME, || Box::new(UTF16LEEncoder));
        map.insert(ISO_8859_1_NAME, || Box::new(ISO8859_1Encoder));
        map.insert(ISO_8859_2_NAME, || Box::new(ISO8859_2Encoder));
        map.insert(ISO_8859_3_NAME, || Box::new(ISO8859_3Encoder));
        map.insert(ISO_8859_4_NAME, || Box::new(ISO8859_4Encoder));
        map.insert(ISO_8859_5_NAME, || Box::new(ISO8859_5Encoder));
        map.insert(ISO_8859_6_NAME, || Box::new(ISO8859_6Encoder));
        map.insert(ISO_8859_7_NAME, || Box::new(ISO8859_7Encoder));
        map.insert(ISO_8859_8_NAME, || Box::new(ISO8859_8Encoder));
        map.insert(ISO_8859_9_NAME, || Box::new(ISO8859_9Encoder));
        map.insert(ISO_8859_10_NAME, || Box::new(ISO8859_10Encoder));
        map.insert(ISO_8859_11_NAME, || Box::new(ISO8859_11Encoder));
        map.insert(ISO_8859_13_NAME, || Box::new(ISO8859_13Encoder));
        map.insert(ISO_8859_14_NAME, || Box::new(ISO8859_14Encoder));
        map.insert(ISO_8859_15_NAME, || Box::new(ISO8859_15Encoder));
        map.insert(ISO_8859_16_NAME, || Box::new(ISO8859_16Encoder));
        map.insert(UTF32_NAME, || Box::new(UTF32Encoder::default()));
        map.insert(UTF32BE_NAME, || Box::new(UTF32BEEncoder));
        map.insert(UTF32LE_NAME, || Box::new(UTF32LEEncoder));
        map.insert(SHIFT_JIS_NAME, || Box::new(ShiftJISEncoder));
        map.insert(US_ASCII_NAME, || Box::new(USASCIIEncoder));
        RwLock::new(map)
    });
pub fn find_encoder(encoding_name: &str) -> Option<Box<dyn Encoder>> {
    let table = ENCODER_TABLE.read().unwrap();
    if let Some(factory) = table.get(encoding_name) {
        return Some(factory());
    }
    if let Some(factory) = table.get(encoding_name.to_ascii_uppercase().as_str()) {
        return Some(factory());
    }

    let alias = resolve_encoding_alias(encoding_name)?;
    table.get(alias).map(|f| f())
}
pub fn register_encoder(
    encoding_name: &'static str,
    factory: EncoderFactory,
) -> Option<EncoderFactory> {
    ENCODER_TABLE
        .write()
        .unwrap()
        .insert(encoding_name, factory)
}
pub fn unregister_encoder(encoding_name: &str) -> Option<EncoderFactory> {
    ENCODER_TABLE.write().unwrap().remove(encoding_name)
}

pub type DecoderFactory = fn() -> Box<dyn Decoder>;
pub static DECODER_TABLE: LazyLock<RwLock<BTreeMap<&'static str, DecoderFactory>>> =
    LazyLock::new(|| {
        let mut map = BTreeMap::<&'static str, DecoderFactory>::new();
        map.insert(UTF8_NAME, || Box::new(UTF8Decoder));
        map.insert(UTF16_NAME, || Box::new(UTF16Decoder::default()));
        map.insert(UTF16BE_NAME, || Box::new(UTF16BEDecoder));
        map.insert(UTF16LE_NAME, || Box::new(UTF16LEDecoder));
        map.insert(ISO_8859_1_NAME, || Box::new(ISO8859_1Decoder));
        map.insert(ISO_8859_2_NAME, || Box::new(ISO8859_2Decoder));
        map.insert(ISO_8859_3_NAME, || Box::new(ISO8859_3Decoder));
        map.insert(ISO_8859_4_NAME, || Box::new(ISO8859_4Decoder));
        map.insert(ISO_8859_5_NAME, || Box::new(ISO8859_5Decoder));
        map.insert(ISO_8859_6_NAME, || Box::new(ISO8859_6Decoder));
        map.insert(ISO_8859_7_NAME, || Box::new(ISO8859_7Decoder));
        map.insert(ISO_8859_8_NAME, || Box::new(ISO8859_8Decoder));
        map.insert(ISO_8859_9_NAME, || Box::new(ISO8859_9Decoder));
        map.insert(ISO_8859_10_NAME, || Box::new(ISO8859_10Decoder));
        map.insert(ISO_8859_11_NAME, || Box::new(ISO8859_11Decoder));
        map.insert(ISO_8859_13_NAME, || Box::new(ISO8859_13Decoder));
        map.insert(ISO_8859_14_NAME, || Box::new(ISO8859_14Decoder));
        map.insert(ISO_8859_15_NAME, || Box::new(ISO8859_15Decoder));
        map.insert(ISO_8859_16_NAME, || Box::new(ISO8859_16Decoder));
        map.insert(UTF32_NAME, || Box::new(UTF32Decoder::default()));
        map.insert(UTF32BE_NAME, || Box::new(UTF32BEDecoder));
        map.insert(UTF32LE_NAME, || Box::new(UTF32LEDecoder));
        map.insert(SHIFT_JIS_NAME, || Box::new(ShiftJISDecoder));
        map.insert(US_ASCII_NAME, || Box::new(USASCIIDecoder));
        RwLock::new(map)
    });
pub fn find_decoder(encoding_name: &str) -> Option<Box<dyn Decoder>> {
    let table = DECODER_TABLE.read().unwrap();
    if let Some(factory) = table.get(encoding_name) {
        return Some(factory());
    }
    if let Some(factory) = table.get(encoding_name.to_ascii_uppercase().as_str()) {
        return Some(factory());
    }

    let alias = resolve_encoding_alias(encoding_name)?;
    table.get(alias).map(|f| f())
}
pub fn register_decoder(
    encoding_name: &'static str,
    factory: DecoderFactory,
) -> Option<DecoderFactory> {
    DECODER_TABLE
        .write()
        .unwrap()
        .insert(encoding_name, factory)
}
pub fn unregister_decoder(encoding_name: &str) -> Option<DecoderFactory> {
    DECODER_TABLE.write().unwrap().remove(encoding_name)
}
