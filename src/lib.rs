pub mod encoding;
pub mod error;
mod parse;
pub mod sax;

use std::marker::PhantomData;

use crate::sax::source::InputSource;

/// Maximum length of XML version numbers accepted by the parser
const XML_VERSION_NUM_LIMIT_LENGTH: usize = 128;
/// Maximum length of encoding names accepted by the parser
const ENCODING_NAME_LIMIT_LENGTH: usize = 128;
/// Approximate chunk length when the parser reports character data
const CHARDATA_CHUNK_LENGTH: usize = 4096;

const XML_XML_NAMESPACE: &str = "http://www.w3.org/XML/1998/namespace";
const XML_NS_NAMESPACE: &str = "http://www.w3.org/2000/xmlns/";

pub trait ParserSpec {
    type Reader;
}

pub struct DefaultParserSpec<'a> {
    _phantom: PhantomData<&'a ()>,
}

impl<'a> ParserSpec for DefaultParserSpec<'a> {
    type Reader = InputSource<'a>;
}

pub struct ProgressiveParserSpec;

impl ParserSpec for ProgressiveParserSpec {
    type Reader = InputSource<'static>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
pub enum XMLVersion {
    /// XML 1.0
    #[default]
    XML10,
    /// Unknown version. Treat as specified in XML 1.0.  
    Unknown,
}

impl XMLVersion {
    pub fn is_char(&self, c: impl Into<u32>) -> bool {
        fn _is_char(_version: XMLVersion, c: u32) -> bool {
            matches!(
                c,
                0x9
                    | 0xA
                    | 0xD
                    | 0x20..= 0xD7FF
                    | 0xE000..= 0xFFFD
                    | 0x10000..= 0x10FFFF
            )
        }
        _is_char(*self, c.into())
    }

    pub fn is_name_start_char(&self, c: impl Into<u32>) -> bool {
        fn _is_name_start_char(_version: XMLVersion, c: u32) -> bool {
            matches!(c,
                0x3A // ':'
                | 0x41..=0x5A // 'A'..='Z'
                | 0x5F // '_'
                | 0x61..=0x7A // 'a'..='z'
                | 0xC0..=0xD6
                | 0xD8..=0xF6
                | 0xF8..=0x2FF
                | 0x370..=0x37D
                | 0x37F..=0x1FFF
                | 0x200C..=0x200D
                | 0x2070..=0x218F
                | 0x2C00..=0x2FEF
                | 0x3001..=0xD7FF
                | 0xF900..=0xFDCF
                | 0xFDF0..=0xFFFD
                | 0x10000..=0xEFFFF
            )
        }
        _is_name_start_char(*self, c.into())
    }

    pub fn is_name_char(&self, c: impl Into<u32>) -> bool {
        fn _is_name_char(_version: XMLVersion, c: u32) -> bool {
            matches!(c,
                0x2D..=0x2E // '-', '.'
                | 0x30..=0x3A // '0'..='9', ':'
                | 0x41..=0x5A // 'A'..='Z'
                | 0x5F // '_'
                | 0x61..=0x7A // 'a'..='z'
                | 0xB7
                | 0xC0..=0xD6
                | 0xD8..=0xF6
                | 0xF8..=0x37D
                | 0x37F..=0x1FFF
                | 0x200C..=0x200D
                | 0x203F..=0x2040
                | 0x2070..=0x218F
                | 0x2C00..=0x2FEF
                | 0x3001..=0xD7FF
                | 0xF900..=0xFDCF
                | 0xFDF0..=0xFFFD
                | 0x10000..=0xEFFFF
            )
        }
        _is_name_char(*self, c.into())
    }

    /// [13] PubidChar ::= #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
    pub fn is_pubid_char(&self, c: impl Into<u32>) -> bool {
        fn _is_pubid_char(_version: XMLVersion, c: u32) -> bool {
            matches!(c,
                0xA
                | 0xD
                | 0x20..=0x21 // SP, '!'
                | 0x23..=0x25 // [#$%]
                | 0x27..=0x3B // ['()*+,-./], '0'..='9', [:;]
                | 0x3D // '='
                | 0x3F..=0x5A // [?@], 'A'..='Z'
                | 0x5F // '_'
                | 0x61..=0x7A // 'a'..='z'
            )
        }
        _is_pubid_char(*self, c.into())
    }

    pub fn is_whitespace(&self, c: impl Into<u32>) -> bool {
        let c: u32 = c.into();
        matches!(c, 0x20 | 0x9 | 0xD | 0xA)
    }
}
