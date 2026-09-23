use crate::{
    DecodeError, Decoder, EncodeError, Encoder,
    jisx::{JIS_X_0201_LATIN_DECODE_TABLE, JIS_X_0208_DECODE_TABLE, JIS_X_0208_ENCODE_TABLE},
};

/// Encoding name for ISO-2022-JP
pub const ISO_2022_JP_NAME: &str = "ISO-2022-JP";

#[derive(Clone, Copy, Default)]
enum State {
    /// 1B 28 42
    #[default]
    ISO646,
    /// 1B 28 4A
    JISX0201Latin,
    /// 1B 24 42
    JISX0208,
    /// 1B 24 40
    JISX0208Old,
}

const ESCSEQ_ISO646: &[u8] = b"\x1B\x28\x42";
const ESCSEQ_JISX0201_LATIN: &[u8] = b"\x1B\x28\x4A";
const ESCSEQ_JISX0208: &[u8] = b"\x1B\x24\x42";
const ESCSEQ_JISX0208_OLD: &[u8] = b"\x1B\x24\x40";

/// Encoder for ISO-2022-JP
#[derive(Default)]
pub struct ISO2022JPEncoder {
    state: State,
}

impl Encoder for ISO2022JPEncoder {
    fn name(&self) -> &'static str {
        ISO_2022_JP_NAME
    }

    fn encode(
        &mut self,
        src: &str,
        dst: &mut [u8],
        finish: bool,
    ) -> Result<(usize, usize), crate::EncodeError> {
        if src.is_empty() {
            return if finish {
                Ok((0, 0))
            } else {
                Err(EncodeError::InputIsEmpty)
            };
        }

        let (mut read, mut write) = (0, 0);
        for c in src.chars() {
            if c.is_ascii() {
                if !matches!(self.state, State::ISO646) {
                    if dst.len() < write + 4 {
                        break;
                    }
                    dst[write..write + 3].copy_from_slice(ESCSEQ_ISO646);
                    write += 3;
                    self.state = State::ISO646;
                }

                if dst.len() < write + 1 {
                    break;
                }
                dst[write] = c as u8;
                write += 1;
                read += 1;
            } else {
                let Ok(pos) =
                    JIS_X_0208_ENCODE_TABLE.binary_search_by_key(&(c as u32), |v| v.0 as u32)
                else {
                    return Err(EncodeError::Unmappable {
                        read: read + c.len_utf8(),
                        write,
                        c,
                    });
                };

                // Since the end of the encoded data must be terminated in ISO/IEC 646, if
                // the last character of the source data is not ASCII, the encoder must check
                // whether there is sufficient space to store the escape sequence used to
                // switch to ISO/IEC 646.
                // If there is insufficient space, the encoder must not terminate the encoding
                // by omitting the last character.
                let close = (read + c.len_utf8() == src.len() && finish) as usize * 3;

                if !matches!(self.state, State::JISX0208) {
                    if dst.len() < write + 5 + close {
                        break;
                    }
                    dst[write..write + 3].copy_from_slice(ESCSEQ_JISX0208);
                    write += 3;
                    self.state = State::JISX0208;
                }
                if dst.len() < write + 2 + close {
                    break;
                }
                let d = JIS_X_0208_ENCODE_TABLE[pos].1;
                dst[write] = (d >> 8) as u8;
                dst[write + 1] = (d & 0xFF) as u8;
                write += 2;
                read += c.len_utf8();
            }
        }

        if finish
            && read == src.len()
            && !matches!(self.state, State::ISO646)
            && write + 3 <= dst.len()
        {
            dst[write..write + 3].copy_from_slice(ESCSEQ_ISO646);
            self.state = State::ISO646;
            write += 3;
        }

        Ok((read, write))
    }
}

/// Decoder name for ISO-2022-JP
#[derive(Default)]
pub struct ISO2022JPDecoder {
    state: State,
}

impl Decoder for ISO2022JPDecoder {
    fn name(&self) -> &'static str {
        ISO_2022_JP_NAME
    }

    fn decode(
        &mut self,
        src: &[u8],
        dst: &mut String,
        finish: bool,
    ) -> Result<(usize, usize), crate::DecodeError> {
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

        macro_rules! check_capacity {
            ($len:expr) => {
                if dst.capacity() - dst.len() < $len {
                    break;
                }
            };
        }

        let (mut read, mut write) = (0, 0);
        while read < src.len() {
            if src[read] >= 0x80 {
                return Err(DecodeError::Malformed {
                    read: read + 1,
                    write,
                    length: 1,
                    offset: 0,
                });
            }

            if src[read] == b'\x1B' {
                if read + 3 > src.len() {
                    break;
                }

                match &src[read..read + 3] {
                    ESCSEQ_ISO646 => self.state = State::ISO646,
                    ESCSEQ_JISX0201_LATIN => self.state = State::JISX0201Latin,
                    ESCSEQ_JISX0208 => self.state = State::JISX0208,
                    ESCSEQ_JISX0208_OLD => self.state = State::JISX0208Old,
                    _ => {
                        // unknown escape sequence
                        return Err(DecodeError::Malformed {
                            read: read + 3,
                            write,
                            length: 3,
                            offset: 0,
                        });
                    }
                }
                read += 3;
            } else {
                match self.state {
                    State::ISO646 => {
                        check_capacity!(1);
                        dst.push(src[read] as char);
                        read += 1;
                        write += 1;
                    }
                    State::JISX0201Latin => {
                        check_capacity!(1);
                        if src[read].is_ascii_control() {
                            dst.push(src[read] as char);
                        } else {
                            dst.push(JIS_X_0201_LATIN_DECODE_TABLE[src[read] as usize - 0x20]);
                        }
                        read += 1;
                        write += 1;
                    }
                    State::JISX0208 | State::JISX0208Old => {
                        if read + 1 >= src.len() {
                            break;
                        }
                        let mut ku = src[read] as usize - 0x20;
                        let mut ten = src[read + 1] as usize - 0x20;
                        if matches!(self.state, State::JISX0208Old) {
                            // JIS X 0208:1997 附属書2（規定）RFC1468符号化表現 4.1 f)
                            // JIS X 0208:1997 附属書2（規定）RFC1468符号化表現 表1
                            (ku, ten) = match (ku, ten) {
                                (15, 18) => (81, 44),
                                (17, 8) => (81, 83),
                                (18, 33) => (72, 57),
                                (18, 40) => (56, 87),
                                (18, 85) => (66, 61),
                                (19, 34) => (61, 84),
                                (19, 49) => (74, 60),
                                (22, 58) => (79, 83),
                                (24, 59) => (65, 71),
                                (27, 40) => (72, 1),
                                (30, 56) => (79, 54),
                                (32, 7) => (75, 44),
                                (35, 58) => (51, 67),
                                (36, 54) => (65, 73),
                                (36, 77) => (58, 76),
                                (36, 82) => (61, 24),
                                (37, 85) => (76, 77),
                                (38, 71) => (73, 3),
                                (40, 15) => (58, 55),
                                (42, 88) => (47, 53),
                                (43, 88) => (72, 13),
                                (46, 21) => (67, 37),
                                (21, 37) => (83, 0),
                                (42, 73) => (83, 1),
                                (44, 57) => (83, 2),
                                (63, 85) => (83, 3),
                                // reverse conversion
                                (81, 44) => (15, 18),
                                (81, 83) => (17, 8),
                                (72, 57) => (18, 33),
                                (56, 87) => (18, 40),
                                (66, 61) => (18, 85),
                                (61, 84) => (19, 34),
                                (74, 60) => (19, 49),
                                (79, 83) => (22, 58),
                                (65, 71) => (24, 59),
                                (72, 1) => (27, 40),
                                (79, 54) => (30, 56),
                                (75, 44) => (32, 7),
                                (51, 67) => (35, 58),
                                (65, 73) => (36, 54),
                                (58, 76) => (36, 77),
                                (61, 24) => (36, 82),
                                (76, 77) => (37, 85),
                                (73, 3) => (38, 71),
                                (58, 55) => (40, 15),
                                (47, 53) => (42, 88),
                                (72, 13) => (43, 88),
                                (67, 37) => (46, 21),
                                (83, 0) => (21, 37),
                                (83, 1) => (42, 73),
                                (83, 2) => (44, 57),
                                (83, 3) => (63, 85),
                                _ => (ku, ten),
                            };
                        }
                        if JIS_X_0208_DECODE_TABLE[ku].len() <= ten
                            || JIS_X_0208_DECODE_TABLE[ku][ten] == char::REPLACEMENT_CHARACTER
                        {
                            return Err(DecodeError::Malformed {
                                read: read + 2,
                                write,
                                length: 2,
                                offset: 0,
                            });
                        }

                        let c = JIS_X_0208_DECODE_TABLE[ku][ten];
                        check_capacity!(c.len_utf8());
                        dst.push(c);
                        read += 2;
                        write += c.len_utf8();
                    }
                }
            }
        }
        Ok((read, write))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn iso2022jp_tests() {
        let src = include_bytes!("../resources/jistests/日本国憲法前文_iso2022jp.txt").as_slice();
        let mut buf = String::with_capacity(1 << 15);
        ISO2022JPDecoder::default()
            .decode(src, &mut buf, true)
            .unwrap();
        let ucs = include_str!("../resources/jistests/日本国憲法前文_unicode.txt");
        assert_eq!(buf, ucs);

        let mut dst = vec![0; 1 << 15];
        let (_, write) = ISO2022JPEncoder::default()
            .encode(&buf, &mut dst, true)
            .unwrap();
        dst.truncate(write);
        assert_eq!(src, dst);
    }
}
