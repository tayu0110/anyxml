mod iso_8859;
mod utf16;

use std::{
    borrow::Cow,
    collections::BTreeMap,
    str::{from_utf8, from_utf8_unchecked},
    sync::{LazyLock, RwLock},
};

use crate::encoding::iso_8859::{
    ISO_8859_1_NAME, ISO_8859_2_NAME, ISO_8859_3_NAME, ISO_8859_4_NAME, ISO_8859_5_NAME,
    ISO_8859_6_NAME, ISO_8859_7_NAME, ISO_8859_8_NAME, ISO_8859_9_NAME, ISO_8859_10_NAME,
    ISO_8859_11_NAME, ISO_8859_13_NAME, ISO_8859_14_NAME, ISO_8859_15_NAME, ISO_8859_16_NAME,
    ISO8859_1Decoder, ISO8859_1Encoder, ISO8859_2Decoder, ISO8859_2Encoder, ISO8859_3Decoder,
    ISO8859_3Encoder, ISO8859_4Decoder, ISO8859_4Encoder, ISO8859_5Decoder, ISO8859_5Encoder,
    ISO8859_6Decoder, ISO8859_6Encoder, ISO8859_7Decoder, ISO8859_7Encoder, ISO8859_8Decoder,
    ISO8859_8Encoder, ISO8859_9Decoder, ISO8859_9Encoder, ISO8859_10Decoder, ISO8859_10Encoder,
    ISO8859_11Decoder, ISO8859_11Encoder, ISO8859_13Decoder, ISO8859_13Encoder, ISO8859_14Decoder,
    ISO8859_14Encoder, ISO8859_15Decoder, ISO8859_15Encoder, ISO8859_16Decoder, ISO8859_16Encoder,
};
pub use crate::encoding::utf16::{
    UTF16_NAME, UTF16BE_NAME, UTF16BEDecoder, UTF16BEEncoder, UTF16Decoder, UTF16Encoder,
    UTF16LE_NAME, UTF16LEDecoder, UTF16LEEncoder,
};

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

#[derive(Debug)]
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
#[derive(Debug)]
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

pub const UTF8_NAME: &str = "UTF-8";

pub struct UTF8Encoder;
impl Encoder for UTF8Encoder {
    fn name(&self) -> &'static str {
        UTF8_NAME
    }

    fn encode(
        &mut self,
        src: &str,
        dst: &mut [u8],
        finish: bool,
    ) -> Result<(usize, usize), EncodeError> {
        if src.is_empty() {
            return if finish {
                Ok((0, 0))
            } else {
                Err(EncodeError::InputIsEmpty)
            };
        }

        if finish && src.len() > dst.len() {
            return Err(EncodeError::OutputTooShort);
        }

        let len = src.len().min(dst.len());
        dst[..len].copy_from_slice(&src.as_bytes()[..len]);
        Ok((len, len))
    }
}

pub struct UTF8Decoder;
impl Decoder for UTF8Decoder {
    fn name(&self) -> &'static str {
        UTF8_NAME
    }

    fn decode(
        &mut self,
        src: &[u8],
        dst: &mut String,
        finish: bool,
    ) -> Result<(usize, usize), DecodeError> {
        if src.is_empty() {
            return Err(DecodeError::InputIsEmpty);
        }
        let len = dst.capacity() - dst.len();
        if len < 4 {
            return Err(DecodeError::OutputTooShort);
        }

        let len = len.min(src.len());
        match from_utf8(&src[..len]) {
            Ok(s) => {
                dst.push_str(s);
                Ok((len, len))
            }
            Err(err) => {
                let up_to = err.valid_up_to();
                dst.push_str(unsafe {
                    // # Safety
                    // This operation is safe due to the `Utf8Error` constraint.
                    from_utf8_unchecked(&src[..up_to])
                });
                match err.error_len() {
                    Some(len) => Err(DecodeError::Malformed {
                        read: up_to + len,
                        write: up_to,
                        length: len,
                        offset: 0,
                    }),
                    None => {
                        if finish {
                            Err(DecodeError::Malformed {
                                read: len,
                                write: up_to,
                                length: len - up_to,
                                offset: 0,
                            })
                        } else {
                            Ok((up_to, up_to))
                        }
                    }
                }
            }
        }
    }
}

/// Supported encodings.  
///
/// Encoding names are listed in lexical order.
pub const DEFAULT_SUPPORTED_ENCODINGS: &[&str] = {
    const NAMES: &[&str] = &[
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
        ISO_8859_11_NAME,
        UTF16_NAME,
        UTF16BE_NAME,
        UTF16LE_NAME,
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
pub static ENCODING_ALIASES: LazyLock<RwLock<BTreeMap<&'static str, &'static str>>> =
    LazyLock::new(|| {
        RwLock::new(BTreeMap::from([
            ("UTF8", UTF8_NAME),
            ("UTF16", UTF16_NAME),
            ("UTF16BE", UTF16BE_NAME),
            ("UTF16LE", UTF16LE_NAME),
            ("iso-ir-100", ISO_8859_1_NAME),
            ("ISO_8859-1", ISO_8859_1_NAME),
            ("ISO-8859-1", ISO_8859_1_NAME),
            ("latin1", ISO_8859_1_NAME),
            ("l1", ISO_8859_1_NAME),
            ("IBM819", ISO_8859_1_NAME),
            ("CP819", ISO_8859_1_NAME),
            ("ISOLatin1", ISO_8859_1_NAME),
            ("iso-ir-101", ISO_8859_2_NAME),
            ("ISO_8859-2", ISO_8859_2_NAME),
            ("ISO-8859-2", ISO_8859_2_NAME),
            ("latin2", ISO_8859_2_NAME),
            ("l2", ISO_8859_2_NAME),
            ("ISOLatin2", ISO_8859_2_NAME),
            ("iso-ir-109", ISO_8859_3_NAME),
            ("ISO_8859-3", ISO_8859_3_NAME),
            ("ISO-8859-3", ISO_8859_3_NAME),
            ("latin3", ISO_8859_3_NAME),
            ("l3", ISO_8859_3_NAME),
            ("ISOLatin3", ISO_8859_3_NAME),
            ("iso-ir-110", ISO_8859_4_NAME),
            ("ISO_8859-4", ISO_8859_4_NAME),
            ("ISO-8859-4", ISO_8859_4_NAME),
            ("latin4", ISO_8859_4_NAME),
            ("l4", ISO_8859_4_NAME),
            ("ISOLatin4", ISO_8859_4_NAME),
            ("iso-ir-144", ISO_8859_5_NAME),
            ("ISO_8859-5", ISO_8859_5_NAME),
            ("ISO-8859-5", ISO_8859_5_NAME),
            ("cyrillic", ISO_8859_5_NAME),
            ("ISOLatinCyrillic", ISO_8859_5_NAME),
            ("iso-ir-127", ISO_8859_6_NAME),
            ("ISO_8859-6", ISO_8859_6_NAME),
            ("ISO-8859-6", ISO_8859_6_NAME),
            ("ECMA-114", ISO_8859_6_NAME),
            ("ASMO-708", ISO_8859_6_NAME),
            ("arabic", ISO_8859_6_NAME),
            ("ISOLatinArabic", ISO_8859_6_NAME),
            ("iso-ir-126", ISO_8859_7_NAME),
            ("ISO_8859-7", ISO_8859_7_NAME),
            ("ISO-8859-7", ISO_8859_7_NAME),
            ("ELOT_928", ISO_8859_7_NAME),
            ("ECMA-118", ISO_8859_7_NAME),
            ("greek", ISO_8859_7_NAME),
            ("greek8", ISO_8859_7_NAME),
            ("ISOLatinGreek", ISO_8859_7_NAME),
            ("iso-ir-138", ISO_8859_8_NAME),
            ("ISO_8859-8", ISO_8859_8_NAME),
            ("ISO-8859-8", ISO_8859_8_NAME),
            ("hebrew", ISO_8859_8_NAME),
            ("ISOLatinHebrew", ISO_8859_8_NAME),
            ("iso-ir-148", ISO_8859_9_NAME),
            ("ISO_8859-9", ISO_8859_9_NAME),
            ("ISO-8859-9", ISO_8859_9_NAME),
            ("latin5", ISO_8859_9_NAME),
            ("l5", ISO_8859_9_NAME),
            ("ISOLatin5", ISO_8859_9_NAME),
            ("iso-ir-157", ISO_8859_10_NAME),
            ("l6", ISO_8859_10_NAME),
            ("ISO_8859-10:1992", ISO_8859_10_NAME),
            ("ISOLatin6", ISO_8859_10_NAME),
            ("latin6", ISO_8859_10_NAME),
            ("TIS620", ISO_8859_11_NAME),
            ("ISO-8859-11", ISO_8859_11_NAME),
            ("ISO885913", ISO_8859_13_NAME),
            ("iso-ir-199", ISO_8859_14_NAME),
            ("ISO_8859-14:1998", ISO_8859_14_NAME),
            ("ISO_8859-14", ISO_8859_14_NAME),
            ("latin8", ISO_8859_14_NAME),
            ("iso-celtic", ISO_8859_14_NAME),
            ("l8", ISO_8859_14_NAME),
            ("ISO885914", ISO_8859_14_NAME),
            ("ISO_8859-15", ISO_8859_15_NAME),
            ("Latin-9", ISO_8859_15_NAME),
            ("ISO885915", ISO_8859_15_NAME),
            ("iso-ir-226", ISO_8859_16_NAME),
            ("ISO_8859-16:2001", ISO_8859_16_NAME),
            ("ISO_8859-16", ISO_8859_16_NAME),
            ("latin10", ISO_8859_16_NAME),
            ("l10", ISO_8859_16_NAME),
            ("ISO885916", ISO_8859_16_NAME),
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
    ENCODING_ALIASES.write().unwrap().insert(alias, real)
}
/// Unregister `alias` if it is registerd as an alias for an encoding name.  \
/// If successfully removed, return the real name.
pub fn unregister_encoding_alias(alias: &'static str) -> Option<&'static str> {
    ENCODING_ALIASES.write().unwrap().remove(alias)
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
        RwLock::new(map)
    });
pub fn find_encoder(encoding_name: &str) -> Option<Box<dyn Encoder>> {
    let table = ENCODER_TABLE.read().unwrap();
    if let Some(factory) = table.get(encoding_name) {
        return Some(factory());
    }

    let &alias = ENCODING_ALIASES.read().unwrap().get(encoding_name)?;
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
        RwLock::new(map)
    });
pub fn find_decoder(encoding_name: &str) -> Option<Box<dyn Decoder>> {
    let table = DECODER_TABLE.read().unwrap();
    if let Some(factory) = table.get(encoding_name) {
        return Some(factory());
    }

    let &alias = ENCODING_ALIASES.read().unwrap().get(encoding_name)?;
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
