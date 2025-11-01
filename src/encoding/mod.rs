mod utf16;

use std::{
    borrow::Cow,
    collections::BTreeMap,
    str::{from_utf8, from_utf8_unchecked},
    sync::{LazyLock, RwLock},
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
pub const DEFAULT_SUPPORTED_ENCODINGS: &[&str] =
    &[UTF16_NAME, UTF16BE_NAME, UTF16LE_NAME, UTF8_NAME];
/// Manage aliases for encoding names.
pub static ENCODING_ALIASES: LazyLock<RwLock<BTreeMap<&'static str, &'static str>>> =
    LazyLock::new(|| {
        RwLock::new(BTreeMap::from([
            ("UTF8", UTF8_NAME),
            ("UTF16", UTF16_NAME),
            ("UTF16BE", UTF16BE_NAME),
            ("UTF16LE", UTF16LE_NAME),
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
