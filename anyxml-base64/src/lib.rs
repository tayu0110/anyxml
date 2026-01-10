//! A simple implementation of Base64 encoding as defined in
//! [RFC 2045 6.8. Base64 Content-Transfer-Encoding](https://datatracker.ietf.org/doc/html/rfc2045#section-6.8).

/// ```text
/// Table 1: The Base64 Alphabet
///
/// Value Encoding  Value Encoding  Value Encoding  Value Encoding
///     0 A            17 R            34 i            51 z
///     1 B            18 S            35 j            52 0
///     2 C            19 T            36 k            53 1
///     3 D            20 U            37 l            54 2
///     4 E            21 V            38 m            55 3
///     5 F            22 W            39 n            56 4
///     6 G            23 X            40 o            57 5
///     7 H            24 Y            41 p            58 6
///     8 I            25 Z            42 q            59 7
///     9 J            26 a            43 r            60 8
///    10 K            27 b            44 s            61 9
///    11 L            28 c            45 t            62 +
///    12 M            29 d            46 u            63 /
///    13 N            30 e            47 v
///    14 O            31 f            48 w         (pad) =
///    15 P            32 g            49 x
///    16 Q            33 h            50 y
/// ```
const ENCODING_TABLE: &[u8; 1 << 6] =
    b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
const DECODING_TABLE: [u8; 1 << 8] = {
    let mut table = [u8::MAX; 1 << 8];
    let mut i = 0;
    while i < ENCODING_TABLE.len() {
        table[ENCODING_TABLE[i] as usize] = i as u8;
        i += 1;
    }
    table
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Base64Error {
    MalformedByte { byte: u8, position: usize },
    InsufficientPadding,
}

/// Base64-encoded byte sequence.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Base64Binary {
    bin: Vec<u8>,
}

impl Base64Binary {
    /// Encode a binary sequence to Base64 binary.
    ///
    /// # Example
    /// ```rust
    /// use anyxml_base64::Base64Binary;
    ///
    /// let encoded = Base64Binary::encode("Hello".bytes());
    /// assert_eq!(encoded.to_string(), "SGVsbG8=");
    /// ```
    pub fn encode(iter: impl IntoIterator<Item = u8>) -> Self {
        iter.into_iter().collect()
    }

    /// Decode the Base64 binary back into the original binary sequence.
    ///
    /// # Example
    /// ```rust
    /// use anyxml_base64::Base64Binary;
    ///
    /// let encoded = Base64Binary::from_encoded(*b"SGVsbG8=", false).unwrap();
    /// let decoded = encoded.decode().map(|b| b as char).collect::<String>();
    /// assert_eq!(decoded, "Hello");
    /// ```
    pub fn decode(&self) -> impl Iterator<Item = u8> + '_ {
        assert!(self.bin.len() % 4 == 0);
        self.bin.chunks_exact(4).flat_map(|chunk| {
            let b0 = DECODING_TABLE[chunk[0] as usize];
            let b1 = DECODING_TABLE[chunk[1] as usize];
            let b2 = DECODING_TABLE[chunk[2] as usize];
            let b3 = DECODING_TABLE[chunk[3] as usize];
            let mut r0 = Some((b0 << 2) | (b1 >> 4));
            let mut r1 = (b2 != u8::MAX).then_some(b1.wrapping_shl(4) | (b2 >> 2));
            let mut r2 = (b3 != u8::MAX).then_some(b2.wrapping_shl(6) | b3);
            std::iter::from_fn(move || r0.take().or_else(|| r1.take()).or_else(|| r2.take()))
        })
    }

    /// Construct a [`Base64Binary`] from a Base64-encoded byte sequence.
    ///
    /// When `allow_whitespace` is true, bytes for which [`u8::is_ascii_whitespace`]
    /// returns true are ignored during byte sequence validation.
    ///
    /// Returns an error if the `iter` is invalid as a Base64 byte sequence.
    pub fn from_encoded(
        iter: impl IntoIterator<Item = u8>,
        allow_whitespace: bool,
    ) -> Result<Self, Base64Error> {
        let mut bin = vec![];
        let mut pad = None;
        for (position, byte) in iter.into_iter().enumerate() {
            if allow_whitespace && byte.is_ascii_whitespace() {
                continue;
            }
            if byte == b'=' {
                pad.get_or_insert((bin.len(), position));
            } else if DECODING_TABLE[byte as usize] == u8::MAX {
                return Err(Base64Error::MalformedByte { byte, position });
            }
            bin.push(byte);
        }

        if bin.len() % 4 != 0 {
            return Err(Base64Error::InsufficientPadding);
        }

        if let Some((pad, position)) = pad
            && (bin.len() - pad > 2 || bin[pad..].iter().any(|&b| b != b'='))
        {
            return Err(Base64Error::MalformedByte {
                byte: b'=',
                position,
            });
        }

        Ok(Base64Binary { bin })
    }
}

impl FromIterator<u8> for Base64Binary {
    fn from_iter<T: IntoIterator<Item = u8>>(iter: T) -> Self {
        let mut iter = iter.into_iter();
        let mut bin = vec![];
        while let Some(b0) = iter.next() {
            bin.push(ENCODING_TABLE[(b0 >> 2) as usize]);
            match iter.next() {
                Some(b1) => {
                    bin.push(ENCODING_TABLE[(((b0 & 0x3) << 4) | (b1 >> 4)) as usize]);
                    match iter.next() {
                        Some(b2) => {
                            bin.push(ENCODING_TABLE[(((b1 & 0xF) << 2) | (b2 >> 6)) as usize]);
                            bin.push(ENCODING_TABLE[(b2 & 0x3F) as usize]);
                        }
                        None => {
                            bin.push(ENCODING_TABLE[((b1 & 0xF) << 2) as usize]);
                            bin.push(b'=');
                        }
                    }
                }
                None => {
                    bin.push(ENCODING_TABLE[((b0 & 0x3) << 4) as usize]);
                    bin.push(b'=');
                    bin.push(b'=');
                }
            }
        }
        Base64Binary { bin }
    }
}

impl From<&str> for Base64Binary {
    fn from(value: &str) -> Self {
        value.bytes().collect()
    }
}

macro_rules! impl_from_str_for_base64_binary {
    ( $( $t:ty ),* ) => {
        $(
            impl From<$t> for Base64Binary {
                fn from(value: $t) -> Self {
                    value.bytes().collect()
                }
            }
            impl From<&$t> for Base64Binary {
                fn from(value: &$t) -> Self {
                    value.bytes().collect()
                }
            }
        )*
    };
}
impl_from_str_for_base64_binary!(
    String,
    Box<str>,
    std::rc::Rc<str>,
    std::sync::Arc<str>,
    std::borrow::Cow<'_, str>
);

impl From<&[u8]> for Base64Binary {
    fn from(value: &[u8]) -> Self {
        value.iter().copied().collect()
    }
}
macro_rules! impl_from_bytes_for_base64_binary {
    ( $( $t:ty ),* ) => {
        $(
            impl From<$t> for Base64Binary {
                fn from(value: $t) -> Self {
                    value.iter().copied().collect()
                }
            }
            impl From<&$t> for Base64Binary {
                fn from(value: &$t) -> Self {
                    value.iter().copied().collect()
                }
            }
        )*
    };
}
impl_from_bytes_for_base64_binary!(
    Vec<u8>,
    Box<[u8]>,
    std::rc::Rc<[u8]>,
    std::sync::Arc<[u8]>,
    std::borrow::Cow<'_, [u8]>
);

impl std::fmt::Debug for Base64Binary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl std::fmt::Display for Base64Binary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe {
            // # Safety
            // `self.bin` is a Base64 binary consisting entirely of ASCII characters,
            // so UTF-8 validation will not fail.
            write!(f, "{}", std::str::from_utf8_unchecked(&self.bin))
        }
    }
}

#[cfg(test)]
mod tests {
    use std::hash::{BuildHasher, Hasher, RandomState};

    use super::*;

    fn xor_shift32(seed: u64) -> impl Iterator<Item = u32> {
        let mut random = seed as u32;

        std::iter::repeat_with(move || {
            random ^= random << 13;
            random ^= random >> 17;
            random ^= random << 5;
            random
        })
    }

    fn bytes(seed: u64) -> impl Iterator<Item = u8> {
        let mut generator = xor_shift32(seed);
        let mut counter = 0;
        let mut buf = [0u8; 4];
        std::iter::from_fn(move || {
            if counter == 4 {
                let val = generator.next().unwrap();
                buf = val.to_le_bytes();
                counter = 0;
            }
            let ret = buf[counter];
            counter += 1;
            Some(ret)
        })
    }

    #[test]
    fn regression_tests() {
        let state = RandomState::new().build_hasher();
        let seed = state.finish();
        let mut bytes = bytes(seed);
        for _ in 0..10000 {
            let len = bytes.next().unwrap() as usize;
            let bytes = (0..len).map(|_| bytes.next().unwrap()).collect::<Vec<_>>();

            let encoded = bytes.iter().copied().collect::<Base64Binary>();
            let decoded = encoded.decode().collect::<Vec<_>>();

            assert_eq!(bytes, decoded, "len: {},{}", bytes.len(), decoded.len());
            let pad = encoded.bin.iter().filter(|c| **c == b'=').count();
            match encoded.bin.as_slice() {
                [.., b'=', b'='] => assert_eq!(pad, 2),
                [.., b'='] => assert_eq!(pad, 1),
                [..] => assert_eq!(pad, 0),
            }

            let encoded2 = Base64Binary::from_encoded(encoded.to_string().bytes(), false).unwrap();
            assert_eq!(encoded, encoded2);
        }
    }

    #[test]
    fn encoded_bytes_tests() {
        assert!(Base64Binary::from_encoded(*b"", false).is_ok());
        let state = RandomState::new().build_hasher();
        let seed = state.finish();
        let mut bytes = bytes(seed);
        for _ in 0..10000 {
            let len = bytes.next().unwrap() as usize;
            let len = len.div_ceil(4) * 4;
            let bytes = bytes
                .by_ref()
                .filter(|b| DECODING_TABLE[*b as usize] != u8::MAX)
                .take(len)
                .collect::<Vec<_>>();

            let encoded = Base64Binary::from_encoded(bytes, false);
            assert!(encoded.is_ok());
        }
    }

    #[test]
    fn erroneous_encoded_bytes_tests() {
        assert!(Base64Binary::from_encoded(*b"a", false).is_err());
        assert!(Base64Binary::from_encoded(*b"aa", false).is_err());
        assert!(Base64Binary::from_encoded(*b"aaa", false).is_err());
        assert!(Base64Binary::from_encoded(*b"aaaaa", false).is_err());
        assert!(Base64Binary::from_encoded(*b"aaaaaa", false).is_err());
        assert!(Base64Binary::from_encoded(*b"aaaaaaa", false).is_err());

        assert!(Base64Binary::from_encoded(*b"=", false).is_err());
        assert!(Base64Binary::from_encoded(*b"==", false).is_err());
        assert!(Base64Binary::from_encoded(*b"===", false).is_err());
        assert!(Base64Binary::from_encoded(*b"====", false).is_err());

        assert!(Base64Binary::from_encoded(*b"a=", false).is_err());
        assert!(Base64Binary::from_encoded(*b"a==", false).is_err());
        assert!(Base64Binary::from_encoded(*b"a===", false).is_err());
    }
}
