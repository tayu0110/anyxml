use crate::encoding::{DecodeError, Decoder, EncodeError, Encoder};

pub const ISO_8859_1_NAME: &str = "ISO_8859-1:1987";

pub struct ISO8859_1Encoder;
impl Encoder for ISO8859_1Encoder {
    fn name(&self) -> &'static str {
        ISO_8859_1_NAME
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
            if b >= 256 {
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

pub struct ISO8859_1Decoder;
impl Decoder for ISO8859_1Decoder {
    fn name(&self) -> &'static str {
        ISO_8859_1_NAME
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
        if len < 2 {
            return Err(DecodeError::OutputTooShort);
        }

        let (mut read, mut write) = (0, 0);
        for &b in src {
            let c = b as char;
            let l = c.len_utf8();
            if write + l > len {
                break;
            }
            dst.push(c);
            read += 1;
            write += l;
        }
        Ok((read, write))
    }
}

pub const ISO_8859_2_NAME: &str = "ISO_8859-2:1987";
pub const ISO_8859_3_NAME: &str = "ISO_8859-3:1988";
pub const ISO_8859_4_NAME: &str = "ISO_8859-4:1988";
pub const ISO_8859_5_NAME: &str = "ISO_8859-5:1988";
pub const ISO_8859_6_NAME: &str = "ISO_8859-6:1987";
pub const ISO_8859_7_NAME: &str = "ISO_8859-7:1987";
pub const ISO_8859_8_NAME: &str = "ISO_8859-8:1988";
pub const ISO_8859_9_NAME: &str = "ISO_8859-9:1989";
pub const ISO_8859_10_NAME: &str = "ISO-8859-10";
pub const ISO_8859_11_NAME: &str = "TIS-620";
pub const ISO_8859_13_NAME: &str = "ISO-8859-13";
pub const ISO_8859_14_NAME: &str = "ISO-8859-14";
pub const ISO_8859_15_NAME: &str = "ISO-8859-15";
pub const ISO_8859_16_NAME: &str = "ISO-8859-16";
