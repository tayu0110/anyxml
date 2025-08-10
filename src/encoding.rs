use std::{
    borrow::Cow,
    iter::once,
    str::{from_utf8, from_utf8_unchecked},
};

pub trait Encoder {
    fn name(&self) -> &'static str;
    /// Determines whether this encoder is the encoder specified by `name`.
    ///
    /// Reference: [Character Sets registered by IANA](https://www.iana.org/assignments/character-sets/character-sets.xhtml)
    fn is_match(&self, name: &str) -> bool;
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
    /// Determines whether this decoder is the decoder specified by `name`.
    ///
    /// Reference: [Character Sets registered by IANA](https://www.iana.org/assignments/character-sets/character-sets.xhtml)
    fn is_match(&self, name: &str) -> bool;
    /// If no error occurs, return `Ok((read_bytes, write_bytes))`.
    fn decode(
        &mut self,
        src: &[u8],
        dst: &mut String,
        finish: bool,
    ) -> Result<(usize, usize), DecodeError>;
}

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

const UTF8_NAME: &str = "UTF-8";
const fn is_match_with_utf8(name: &str) -> bool {
    matches!(name.as_bytes(), b"UTF-8" | b"UTF8")
}

pub struct UTF8Encoder;
impl Encoder for UTF8Encoder {
    fn name(&self) -> &'static str {
        UTF8_NAME
    }

    fn is_match(&self, name: &str) -> bool {
        is_match_with_utf8(name)
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

    fn is_match(&self, name: &str) -> bool {
        is_match_with_utf8(name)
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

        if finish && len < src.len() {
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

const UTF16_NAME: &str = "UTF-16";
const fn is_match_with_utf16(name: &str) -> bool {
    matches!(name.as_bytes(), b"UTF-16" | b"UTF16")
}

pub struct UTF16Encoder {
    init: bool,
}
impl Encoder for UTF16Encoder {
    fn name(&self) -> &'static str {
        UTF16_NAME
    }

    fn is_match(&self, name: &str) -> bool {
        is_match_with_utf16(name)
    }

    fn encode(
        &mut self,
        src: &str,
        dst: &mut [u8],
        finish: bool,
    ) -> Result<(usize, usize), EncodeError> {
        if src.is_empty() {
            return Err(EncodeError::InputIsEmpty);
        }
        if dst.len() < 4 {
            return Err(EncodeError::OutputTooShort);
        }

        if !self.init {
            self.init = true;
            // Write BOM as LE
            dst[0] = 0xFF;
            dst[1] = 0xFE;
            return Ok((0, 2));
        }
        UTF16LEEncoder.encode(src, dst, finish)
    }
}

pub struct UTF16Decoder {
    read: usize,
    top: [u8; 2],
    be: bool,
}
impl Decoder for UTF16Decoder {
    fn name(&self) -> &'static str {
        UTF16_NAME
    }

    fn is_match(&self, name: &str) -> bool {
        is_match_with_utf16(name)
    }

    fn decode(
        &mut self,
        mut src: &[u8],
        dst: &mut String,
        finish: bool,
    ) -> Result<(usize, usize), DecodeError> {
        if src.is_empty() {
            return Err(DecodeError::InputIsEmpty);
        }
        if dst.len() < 4 {
            return Err(DecodeError::OutputTooShort);
        }

        let mut base = 0;
        if self.read < 2 {
            let orig = src.len();
            while self.read < 2 && !src.is_empty() {
                self.top[self.read] = src[0];
                src = &src[1..];
                self.read += 1;
            }
            base = orig - src.len();
            if self.read == 2 {
                // If the first 2 bytes of the buffer are 0xFF, 0xFE, it is LE; otherwise, it is BE.
                if matches!(self.top[..], [0xFF, 0xFE]) {
                    self.be = false;
                    return Ok((base, 0));
                } else if matches!(self.top[..], [0xFE, 0xFF]) {
                    self.be = true;
                    return Ok((base, 0));
                } else {
                    self.be = true;
                    // Since the first two bytes were not BOM,
                    // try decoding using the first two bytes that have already been acquired.
                };
            } else {
                return Ok((base, 0));
            }
        }

        if self.be && !matches!(self.top[..], [0xFE, 0xFF]) {
            let mut read = 0;
            let mut write = 0;
            for c in char::decode_utf16(
                once(((self.top[0] as u16) << 8) | self.top[1] as u16).chain(
                    src.chunks_exact(2)
                        .map(|v| ((v[0] as u16) << 8) | v[1] as u16),
                ),
            ) {
                if let Ok(c) = c {
                    read += c.len_utf16();
                    write += c.len_utf8();
                    dst.push(c);
                } else {
                    let rem = src.len() - (read - 2);
                    if !finish && rem < 4 {
                        // If this is not the last buffer and the unread buffer is less than 2 bytes,
                        // return `Ok` because the corresponding surrogate pair may be at the beginning of the next buffer to be input.
                        break;
                    } else {
                        // If this is the last buffer, or if there is sufficient data to form a surrogate pair but an error occurs,
                        // it is simply an invalid byte sequence.
                        return Err(DecodeError::Malformed {
                            read: read + 2,
                            write,
                            length: 2,
                            offset: 0,
                        });
                    }
                }

                if dst.capacity() - dst.len() < 4 {
                    break;
                }
            }
            return if read > 0 {
                self.top = [0xFE, 0xFF];
                read -= 2 - base;
                Ok((read, write))
            } else {
                Ok((base, 0))
            };
        }

        if self.be {
            UTF16BEDecoder.decode(src, dst, finish)
        } else {
            UTF16LEDecoder.decode(src, dst, finish)
        }
    }
}

const UTF16BE_NAME: &str = "UTF-16BE";
const fn is_match_with_utf16be(name: &str) -> bool {
    matches!(name.as_bytes(), b"UTF-16BE" | b"UTF16BE")
}

pub struct UTF16BEEncoder;
impl Encoder for UTF16BEEncoder {
    fn name(&self) -> &'static str {
        UTF16BE_NAME
    }

    fn is_match(&self, name: &str) -> bool {
        is_match_with_utf16be(name)
    }

    fn encode(
        &mut self,
        src: &str,
        mut dst: &mut [u8],
        _finish: bool,
    ) -> Result<(usize, usize), EncodeError> {
        if src.is_empty() {
            return Err(EncodeError::InputIsEmpty);
        }
        if dst.len() < 4 {
            return Err(EncodeError::OutputTooShort);
        }

        let mut buf = [0u16; 2];
        let mut read = 0;
        let mut write = 0;
        for c in src.chars() {
            read += c.len_utf8();
            let b = c.encode_utf16(&mut buf);
            dst[..2].copy_from_slice(&b[0].to_be_bytes());
            dst = &mut dst[2..];
            write += 2;
            if b.len() == 2 {
                dst[..2].copy_from_slice(&b[1].to_be_bytes());
                dst = &mut dst[2..];
                write += 2;
            }
            if dst.len() < 4 {
                break;
            }
        }
        Ok((read, write))
    }
}

pub struct UTF16BEDecoder;
impl Decoder for UTF16BEDecoder {
    fn name(&self) -> &'static str {
        UTF16BE_NAME
    }

    fn is_match(&self, name: &str) -> bool {
        is_match_with_utf16be(name)
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
        let cap = dst.capacity() - dst.len();
        if cap < 4 {
            return Err(DecodeError::OutputTooShort);
        }

        let mut read = 0;
        let mut write = 0;
        for c in char::decode_utf16(
            src.chunks_exact(2)
                .map(|v| u16::from_be_bytes([v[0], v[1]])),
        ) {
            if let Ok(c) = c {
                read += c.len_utf16();
                write += c.len_utf8();
                dst.push(c);
            } else {
                let rem = src.len() - read;
                if !finish && rem < 4 {
                    break;
                } else {
                    return Err(DecodeError::Malformed {
                        read: read + 2,
                        write,
                        length: 2,
                        offset: 0,
                    });
                }
            }

            if dst.capacity() - dst.len() < 4 {
                break;
            }
        }

        Ok((read, write))
    }
}

const UTF16LE_NAME: &str = "UTF-16LE";
const fn is_match_with_utf16le(name: &str) -> bool {
    matches!(name.as_bytes(), b"UTF-16LE" | b"UTF16LE")
}

pub struct UTF16LEEncoder;
impl Encoder for UTF16LEEncoder {
    fn name(&self) -> &'static str {
        UTF16LE_NAME
    }

    fn is_match(&self, name: &str) -> bool {
        is_match_with_utf16le(name)
    }

    fn encode(
        &mut self,
        src: &str,
        mut dst: &mut [u8],
        _finish: bool,
    ) -> Result<(usize, usize), EncodeError> {
        if src.is_empty() {
            return Err(EncodeError::InputIsEmpty);
        }
        if dst.len() < 4 {
            return Err(EncodeError::OutputTooShort);
        }

        let mut buf = [0u16; 2];
        let mut read = 0;
        let mut write = 0;
        for c in src.chars() {
            read += c.len_utf8();
            let b = c.encode_utf16(&mut buf);
            dst[..2].copy_from_slice(&b[0].to_le_bytes());
            dst = &mut dst[2..];
            write += 2;
            if b.len() == 2 {
                dst[..2].copy_from_slice(&b[1].to_le_bytes());
                dst = &mut dst[2..];
                write += 2;
            }
            if dst.len() < 4 {
                break;
            }
        }
        Ok((read, write))
    }
}

pub struct UTF16LEDecoder;
impl Decoder for UTF16LEDecoder {
    fn name(&self) -> &'static str {
        UTF16LE_NAME
    }

    fn is_match(&self, name: &str) -> bool {
        is_match_with_utf16le(name)
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
        let cap = dst.capacity() - dst.len();
        if cap < 4 {
            return Err(DecodeError::OutputTooShort);
        }

        let mut read = 0;
        let mut write = 0;
        for c in char::decode_utf16(
            src.chunks_exact(2)
                .map(|v| u16::from_le_bytes([v[0], v[1]])),
        ) {
            if let Ok(c) = c {
                read += c.len_utf16();
                write += c.len_utf8();
                dst.push(c);
            } else {
                let rem = src.len() - read;
                if !finish && rem < 4 {
                    break;
                } else {
                    return Err(DecodeError::Malformed {
                        read: read + 2,
                        write,
                        length: 2,
                        offset: 0,
                    });
                }
            }

            if dst.capacity() - dst.len() < 4 {
                break;
            }
        }

        Ok((read, write))
    }
}
