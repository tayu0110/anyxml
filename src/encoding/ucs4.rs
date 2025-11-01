use crate::encoding::{DecodeError, Decoder, EncodeError, Encoder};

pub const UTF32_NAME: &str = "UTF-32";

#[derive(Debug, Default)]
pub struct UTF32Encoder {
    init: bool,
}
impl Encoder for UTF32Encoder {
    fn name(&self) -> &'static str {
        UTF32_NAME
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
            dst[2] = 0x00;
            dst[3] = 0x00;
            return Ok((0, 4));
        }
        UTF32LEEncoder.encode(src, dst, finish)
    }
}

pub struct UTF32Decoder {
    read: usize,
    top: [u8; 4],
    be: bool,
}
impl Decoder for UTF32Decoder {
    fn name(&self) -> &'static str {
        UTF32_NAME
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
        if self.read < 4 {
            let orig = src.len();
            while self.read < 4 && !src.is_empty() {
                self.top[self.read] = src[0];
                src = &src[1..];
                self.read += 1;
            }
            base = orig - src.len();
            if self.read == 4 {
                // If the first 4 bytes of the buffer are 0xFF, 0xFE, 0x00, 0x00, it is LE;
                // otherwise, it is BE.
                if matches!(self.top[..], [0xFF, 0xFE, 0x00, 0x00]) {
                    self.be = false;
                    return Ok((base, 0));
                } else if matches!(self.top[..], [0x00, 0x00, 0xFE, 0xFF]) {
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

        if self.be && !matches!(self.top[..], [0x00, 0x00, 0xFE, 0xFF]) {
            let codepoint = u32::from_be_bytes(self.top);
            let mut write = 0;
            match char::from_u32(codepoint) {
                Some(c) => {
                    write += c.len_utf8();
                    dst.push(c);
                }
                None => {
                    // If this is the last buffer, or if there is sufficient data to form a surrogate pair but an error occurs,
                    // it is simply an invalid byte sequence.
                    return Err(DecodeError::Malformed {
                        read: 4,
                        write,
                        length: 4,
                        offset: 0,
                    });
                }
            }

            self.top = [0x00, 0x00, 0xFE, 0xFF];
            return Ok((base, write));
        }

        if self.be {
            UTF32BEDecoder.decode(src, dst, finish)
        } else {
            UTF32LEDecoder.decode(src, dst, finish)
        }
    }
}

impl Default for UTF32Decoder {
    fn default() -> Self {
        Self {
            read: 0,
            top: [0; 4],
            be: true,
        }
    }
}

pub const UTF32BE_NAME: &str = "UTF-32BE";

pub struct UTF32BEEncoder;
impl Encoder for UTF32BEEncoder {
    fn name(&self) -> &'static str {
        UTF32BE_NAME
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

        let mut read = 0;
        let mut write = 0;
        for c in src.chars() {
            read += c.len_utf8();
            dst[..4].copy_from_slice(&(c as u32).to_be_bytes()[..]);
            dst = &mut dst[2..];
            write += 4;
            if dst.len() < 4 {
                break;
            }
        }
        Ok((read, write))
    }
}

pub struct UTF32BEDecoder;
impl Decoder for UTF32BEDecoder {
    fn name(&self) -> &'static str {
        UTF32BE_NAME
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
        for bytes in src.chunks_exact(4) {
            read += 4;
            let codepoint = u32::from_be_bytes(bytes.try_into().unwrap());
            match char::from_u32(codepoint) {
                Some(c) => {
                    write += c.len_utf8();
                    dst.push(c);
                }
                None => {
                    return Err(DecodeError::Malformed {
                        read,
                        write,
                        length: 4,
                        offset: 0,
                    });
                }
            }
            if dst.capacity() - dst.len() < 4 {
                break;
            }
        }

        let rem = src.len() - read;
        if finish && rem < 4 && rem != 0 && dst.capacity() - dst.len() >= 4 {
            return Err(DecodeError::Malformed {
                read: src.len(),
                write,
                length: src.len() - read,
                offset: 0,
            });
        }

        Ok((read, write))
    }
}

pub const UTF32LE_NAME: &str = "UTF-32LE";

pub struct UTF32LEEncoder;
impl Encoder for UTF32LEEncoder {
    fn name(&self) -> &'static str {
        UTF32LE_NAME
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

        let mut read = 0;
        let mut write = 0;
        for c in src.chars() {
            read += c.len_utf8();
            dst[..4].copy_from_slice(&(c as u32).to_le_bytes()[..]);
            dst = &mut dst[2..];
            write += 4;
            if dst.len() < 4 {
                break;
            }
        }
        Ok((read, write))
    }
}

pub struct UTF32LEDecoder;
impl Decoder for UTF32LEDecoder {
    fn name(&self) -> &'static str {
        UTF32LE_NAME
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
        for bytes in src.chunks_exact(4) {
            read += 4;
            let codepoint = u32::from_le_bytes(bytes.try_into().unwrap());
            match char::from_u32(codepoint) {
                Some(c) => {
                    write += c.len_utf8();
                    dst.push(c);
                }
                None => {
                    return Err(DecodeError::Malformed {
                        read,
                        write,
                        length: 4,
                        offset: 0,
                    });
                }
            }
            if dst.capacity() - dst.len() < 4 {
                break;
            }
        }

        let rem = src.len() - read;
        if finish && rem < 4 && rem != 0 && dst.capacity() - dst.len() >= 4 {
            return Err(DecodeError::Malformed {
                read: src.len(),
                write,
                length: src.len() - read,
                offset: 0,
            });
        }

        Ok((read, write))
    }
}
