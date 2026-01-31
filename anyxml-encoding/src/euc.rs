use crate::{DecodeError, Decoder, EncodeError, Encoder, jisx};

pub struct EUCDecoder {
    // G0 implicitly designate ASCII.
    // Each buffer is assumed to be either 94, 96, 94^n, or 96^n.

    // G1 buffer
    g1: &'static [&'static [char]],
    // G2 buffer
    g2: &'static [&'static [char]],
    // G3 buffer
    g3: &'static [&'static [char]],
}

impl EUCDecoder {
    /// If no error occurs, return `Ok((read_bytes, write_bytes))`.
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
        if len < 4 {
            return Err(DecodeError::OutputTooShort);
        }

        let (mut read, mut write) = (0, 0);
        macro_rules! malformed {
            ( $length:expr ) => {
                Err(DecodeError::Malformed {
                    read,
                    write,
                    length: $length,
                    offset: 0,
                })
            };
        }

        while read < src.len() {
            match src[read..] {
                [0x8E, ..] | [0x8F, ..] => {
                    // SS2/SS3
                    let g = if src[read] == 0x8E {
                        &self.g2
                    } else {
                        &self.g3
                    };

                    read += 1;
                    if read >= src.len() {
                        if finish {
                            return malformed!(1);
                        }
                        break;
                    }

                    if src[read] < 0xA0 {
                        read += 1;
                        return malformed!(1);
                    }
                    let s = src[read] as usize - 0xA0;
                    read += 1;
                    if g.len() == 1 {
                        let c = g[0][s];
                        if g[0].len() <= s || g[0][s] == char::REPLACEMENT_CHARACTER {
                            return malformed!(2);
                        }
                        dst.push(c);
                        write += c.len_utf8();
                    } else {
                        if read >= src.len() {
                            if finish {
                                return malformed!(2);
                            }
                            break;
                        }
                        if src[read] < 0xA0 {
                            read += 1;
                            return malformed!(3);
                        }
                        let t = src[read] as usize - 0xA0;
                        read += 1;
                        if g.len() <= s || g[s].len() <= t || g[s][t] == char::REPLACEMENT_CHARACTER
                        {
                            return malformed!(3);
                        }
                        let c = g[s][t];
                        dst.push(c);
                        write += c.len_utf8();
                    }
                }
                _ => {
                    if src[read] < 0xA0 {
                        // GL (G0, ASCII) and CR
                        // I'm not sure if this is the correct way to handle CR...
                        // uconv seems to return the original value for EUC-JP/CN/KR.
                        // However, iconv appears to return the original value for EUC-JP/KR,
                        // but returns an error for EUC-CN.
                        let c = src[read] as char;
                        dst.push(c);
                        read += 1;
                        write += c.len_utf8();
                    } else {
                        // GR (G1)
                        if self.g1.len() > 1 {
                            // multibyte character
                            if read + 1 == src.len() {
                                // The second byte of multibyte characters cannot be read.
                                if finish {
                                    read += 1;
                                    return malformed!(1);
                                }
                                break;
                            }
                            let s = src[read] as usize - 0xA0;
                            if src[read + 1] < 0xA0 {
                                read += 2;
                                return malformed!(2);
                            }
                            let t = src[read + 1] as usize - 0xA0;
                            read += 2;
                            if self.g1.len() <= s
                                || self.g1[s].len() <= t
                                || self.g1[s][t] == char::REPLACEMENT_CHARACTER
                            {
                                return malformed!(2);
                            }
                            let c = self.g1[s][t];
                            dst.push(c);
                            write += c.len_utf8();
                        } else {
                            // singlebyte character
                            let c = self.g1[0][src[read] as usize - 0xA0];
                            read += 1;
                            if c == char::REPLACEMENT_CHARACTER {
                                return malformed!(1);
                            } else {
                                dst.push(c);
                                write += c.len_utf8();
                            }
                        }
                    }
                }
            }

            if dst.capacity() - dst.len() < 4 {
                break;
            }
        }

        Ok((read, write))
    }
}

pub struct EUCEncoder<
    G1From,
    G1To,
    G2From,
    G2To,
    G3From,
    G3To,
    const G1DIM: u8,
    const G2DIM: u8,
    const G3DIM: u8,
> where
    G1From: Into<u32> + 'static,
    G1To: Into<u32> + 'static,
    G2From: Into<u32> + 'static,
    G2To: Into<u32> + 'static,
    G3From: Into<u32> + 'static,
    G3To: Into<u32> + 'static,
{
    // G0 implicitly designate ASCII.

    // G1 buffer
    g1: &'static [(G1From, G1To)],
    // G2 buffer
    g2: &'static [(G2From, G2To)],
    // G3 buffer
    g3: &'static [(G3From, G3To)],
}

impl<G1From, G1To, G2From, G2To, G3From, G3To, const G1DIM: u8, const G2DIM: u8, const G3DIM: u8>
    EUCEncoder<G1From, G1To, G2From, G2To, G3From, G3To, G1DIM, G2DIM, G3DIM>
where
    G1From: Into<u32> + Copy,
    G1To: Into<u32> + Copy,
    G2From: Into<u32> + Copy,
    G2To: Into<u32> + Copy,
    G3From: Into<u32> + Copy,
    G3To: Into<u32> + Copy,
{
    /// If no error occurs, return `Ok((read_bytes, write_bytes))`.
    fn encode(
        &mut self,
        src: &str,
        dst: &mut [u8],
        finish: bool,
    ) -> Result<(usize, usize), EncodeError> {
        assert!(0 < G1DIM && G1DIM <= 2 && 0 < G2DIM && G2DIM <= 2 && 0 < G3DIM && G3DIM <= 2);
        if src.is_empty() {
            return if finish {
                Ok((0, 0))
            } else {
                Err(EncodeError::InputIsEmpty)
            };
        }

        if dst.len() < 3 {
            return Err(EncodeError::OutputTooShort);
        }

        let (mut read, mut write) = (0, 0);
        macro_rules! write_buffer {
            ( $dim:expr, $to:expr ) => {
                if $dim == 1 {
                    assert!($to < u8::MAX as u32);
                    dst[write] = $to as u8 + 0xA0;
                    write += 1;
                } else if $dim == 2 {
                    let to = $to + 0x8080;
                    dst[write] = (to >> 8) as u8;
                    dst[write + 1] = (to & 0xFF) as u8;
                    write += 2;
                } else {
                    unreachable!();
                }
            };
        }
        for c in src.chars() {
            read += c.len_utf8();
            if c.is_ascii() {
                dst[read] = c as u8;
                write += 1;
            } else if let Ok(pos) = self.g1.binary_search_by_key(&(c as u32), |e| e.0.into()) {
                let to = self.g1[pos].1.into();
                write_buffer!(G1DIM, to);
            } else if let Ok(pos) = self.g2.binary_search_by_key(&(c as u32), |e| e.0.into()) {
                dst[write] = 0x8E;
                write += 1;
                let to = self.g2[pos].1.into();
                write_buffer!(G2DIM, to);
            } else if let Ok(pos) = self.g3.binary_search_by_key(&(c as u32), |e| e.0.into()) {
                dst[write] = 0x8F;
                write += 1;
                let to = self.g3[pos].1.into();
                write_buffer!(G3DIM, to);
            } else {
                return Err(EncodeError::Unmappable { read, write, c });
            }

            if dst[write..].len() < 3 {
                break;
            }
        }

        Ok((read, write))
    }
}

pub const EUCJP_NAME: &str = "EUC-JP";
pub struct EUCJPDecoder {
    decoder: EUCDecoder,
}

pub(crate) fn eucjp_decoder_factory() -> Box<dyn Decoder> {
    static G2: &[&[char]] = &[jisx::JIS_X_0201_KATAKANA_DECODE_TABLE];
    Box::new(EUCJPDecoder {
        decoder: EUCDecoder {
            g1: &jisx::JIS_X_0208_DECODE_TABLE,
            g2: G2,
            g3: &jisx::JIS_X_0212_DECODE_TABLE,
        },
    })
}

impl Decoder for EUCJPDecoder {
    fn name(&self) -> &'static str {
        EUCJP_NAME
    }

    fn decode(
        &mut self,
        src: &[u8],
        dst: &mut String,
        finish: bool,
    ) -> Result<(usize, usize), DecodeError> {
        self.decoder.decode(src, dst, finish)
    }
}

pub struct EUCJPEncoder {
    encoder: EUCEncoder<u16, u16, u16, u8, u16, u16, 2, 1, 2>,
}

pub(crate) fn eucjp_encoder_factory() -> Box<dyn Encoder> {
    Box::new(EUCJPEncoder {
        encoder: EUCEncoder {
            g1: jisx::JIS_X_0208_ENCODE_TABLE,
            g2: jisx::JIS_X_0201_KATAKANA_ENCODE_TABLE,
            g3: jisx::JIS_X_0212_ENCODE_TABLE,
        },
    })
}

impl Encoder for EUCJPEncoder {
    fn name(&self) -> &'static str {
        EUCJP_NAME
    }

    fn encode(
        &mut self,
        src: &str,
        dst: &mut [u8],
        finish: bool,
    ) -> Result<(usize, usize), EncodeError> {
        self.encoder.encode(src, dst, finish)
    }
}
