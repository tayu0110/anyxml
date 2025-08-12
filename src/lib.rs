pub mod encoding;
pub mod error;
mod parse;
pub mod sax;

use std::{borrow::Cow, marker::PhantomData};

use crate::sax::source::InputSource;

/// Maximum length of XML version numbers accepted by the parser
const XML_VERSION_NUM_LIMIT_LENGTH: usize = 128;
/// Maximum length of encoding names accepted by the parser
const ENCODING_NAME_LIMIT_LENGTH: usize = 128;
/// Approximate chunk length when the parser reports character data
const CHARDATA_CHUNK_LENGTH: usize = 4096;

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

pub struct Attribute<'a> {
    uri: Option<Cow<'a, str>>,
    local_name: Option<Cow<'a, str>>,
    qname: Cow<'a, str>,
    value: Cow<'a, str>,
    // 0: is declared in DTD
    // 1: is specified explicitly (in other words, `value` is not the default value provided by DTD)
    flag: u8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AttributeType {
    // todo!!!!
}

pub enum DefaultDecl {
    // todo!!!!
}

pub enum ContentSpec {
    // todo!!!!
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
        let c: u32 = c.into();
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

    pub fn is_whitespace(&self, c: impl Into<u32>) -> bool {
        let c: u32 = c.into();
        matches!(c, 0x20 | 0x9 | 0xD | 0xA)
    }
}
