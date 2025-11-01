use std::iter::once;

use crate::encoding::{DecodeError, Decoder, EncodeError, Encoder};

pub const UTF16_NAME: &str = "UTF-16";

#[derive(Debug, Default)]
pub struct UTF16Encoder {
    init: bool,
}
impl Encoder for UTF16Encoder {
    fn name(&self) -> &'static str {
        UTF16_NAME
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

    fn decode(
        &mut self,
        mut src: &[u8],
        dst: &mut String,
        finish: bool,
    ) -> Result<(usize, usize), DecodeError> {
        if src.is_empty() {
            return Err(DecodeError::InputIsEmpty);
        }
        if dst.capacity() - dst.len() < 4 {
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
                    read += c.len_utf16() * 2;
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

impl Default for UTF16Decoder {
    fn default() -> Self {
        Self {
            read: 0,
            top: [0; 2],
            be: true,
        }
    }
}

pub const UTF16BE_NAME: &str = "UTF-16BE";

pub struct UTF16BEEncoder;
impl Encoder for UTF16BEEncoder {
    fn name(&self) -> &'static str {
        UTF16BE_NAME
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
                read += c.len_utf16() * 2;
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

pub const UTF16LE_NAME: &str = "UTF-16LE";

pub struct UTF16LEEncoder;
impl Encoder for UTF16LEEncoder {
    fn name(&self) -> &'static str {
        UTF16LE_NAME
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
                read += c.len_utf16() * 2;
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
