use std::str::{from_utf8, from_utf8_unchecked};

use crate::{DecodeError, Decoder, EncodeError, Encoder};

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
