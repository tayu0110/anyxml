use crate::encoding::{DecodeError, Decoder, EncodeError, Encoder};

pub const US_ASCII_NAME: &str = "US-ASCII";

pub struct USASCIIEncoder;
impl Encoder for USASCIIEncoder {
    fn name(&self) -> &'static str {
        US_ASCII_NAME
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

        if dst.is_empty() {
            return Err(EncodeError::OutputTooShort);
        }

        let (mut read, mut write) = (0, 0);
        for c in src.chars() {
            let b = c as u32;
            read += c.len_utf8();
            if b >= 128 {
                return Err(EncodeError::Unmappable { read, write, c });
            }
            dst[write] = b as u8;
            write += 1;
            if write == dst.len() {
                break;
            }
        }
        Ok((read, write))
    }
}

pub struct USASCIIDecoder;
impl Decoder for USASCIIDecoder {
    fn name(&self) -> &'static str {
        US_ASCII_NAME
    }

    fn decode(
        &mut self,
        src: &[u8],
        dst: &mut String,
        finish: bool,
    ) -> Result<(usize, usize), DecodeError> {
        if src.is_empty() {
            return if finish {
                Ok((0, 0))
            } else {
                Err(DecodeError::InputIsEmpty)
            };
        }
        let len = dst.capacity() - dst.len();
        if len == 0 {
            return Err(DecodeError::OutputTooShort);
        }

        let (mut read, mut write) = (0, 0);
        for &b in src {
            read += 1;
            if b >= 128 {
                return Err(DecodeError::Malformed {
                    read,
                    write,
                    length: 1,
                    offset: 0,
                });
            }
            let c = b as char;
            dst.push(c);
            write += 1;
            if write == len {
                break;
            }
        }
        Ok((read, write))
    }
}
