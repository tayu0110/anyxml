#![doc = include_str!("../README.md")]

pub mod automata;
pub mod catalog;
pub mod encoding;
pub mod error;
mod parse;
pub mod relaxng;
mod save;
pub mod sax;
pub mod stax;
pub mod tree;
pub mod uri;
pub mod xpath;

use std::{convert::Infallible, marker::PhantomData, str::FromStr};

use crate::sax::{parser::ParserSubState, source::InputSource};

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
    type SpecificContext;
}

pub struct DefaultParserSpec<'a> {
    _phantom: PhantomData<&'a ()>,
}

impl<'a> ParserSpec for DefaultParserSpec<'a> {
    type Reader = InputSource<'a>;
    type SpecificContext = ();
}

pub struct ProgressiveParserSpec;

impl ParserSpec for ProgressiveParserSpec {
    type Reader = InputSource<'static>;
    type SpecificContext = ProgressiveParserSpecificContext;
}

#[derive(Debug, Default)]
pub struct ProgressiveParserSpecificContext {
    pub(crate) seen: usize,
    pub(crate) quote: u8,
    pub(crate) sub_state: ParserSubState,
    // (QName, prefix length, namespace stack length)
    pub(crate) element_stack: Vec<(String, usize, usize)>,
    // (old element stack length, old xml version, old encoding)
    pub(crate) entity_stack: Vec<(usize, XMLVersion, Option<String>)>,
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
    /// ```text
    /// // XML 1.0
    /// [2] Char ::= #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF] /* any Unicode character, excluding the surrogate blocks, FFFE, and FFFF. */
    /// ```
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

    /// ```text
    /// // XML 1.0
    /// [4] NameStartChar ::= ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
    /// ```
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

    /// ```text
    /// // XML 1.0
    /// [4a] NameChar ::= NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F] | [#x203F-#x2040]
    /// ```
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

    /// ```text
    /// // XML 1.0
    /// [13] PubidChar ::= #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
    /// ```
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

    /// ```text
    /// // XML 1.0
    /// [3] S ::= (#x20 | #x9 | #xD | #xA)+
    /// ```
    pub fn is_whitespace(&self, c: impl Into<u32>) -> bool {
        let c: u32 = c.into();
        matches!(c, 0x20 | 0x9 | 0xD | 0xA)
    }

    /// ```text
    /// // XML 1.0
    /// [5] Name ::= NameStartChar (NameChar)*
    /// ```
    pub fn validate_name(&self, s: &str) -> bool {
        let mut chars = s.chars();
        chars.next().is_some_and(|c| self.is_name_start_char(c))
            && chars.all(|c| self.is_name_char(c))
    }

    /// ```text
    /// // Namespaces in XML 1.0
    /// [4] NCName ::= Name - (Char* ':' Char*) /* An XML Name, minus the ":" */
    /// ```
    pub fn validate_ncname(&self, s: &str) -> bool {
        let mut chars = s.chars();
        chars
            .next()
            .is_some_and(|c| c != ':' && self.is_name_start_char(c))
            && chars.all(|c| c != ':' && self.is_name_char(c))
    }

    /// ```text
    /// // Namespaces in XML 1.0
    /// [7]  QName          ::= PrefixedName | UnprefixedName
    /// [8]  PrefixedName   ::= Prefix ':' LocalPart
    /// [9]  UnprefixedName ::= LocalPart
    /// [10] Prefix         ::= NCName
    /// [11] LocalPart      ::= NCName
    /// ```
    pub fn validate_qname(&self, s: &str) -> bool {
        let mut chars = s.chars();
        if chars
            .next()
            .is_none_or(|c| c == ':' || !self.is_name_start_char(c))
        {
            return false;
        }
        while let Some(c) = chars.next() {
            if c == ':' {
                return self.validate_ncname(chars.as_str());
            }

            if !self.is_name_char(c) {
                return false;
            }
        }
        true
    }
}

impl std::fmt::Display for XMLVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            XMLVersion::XML10 => write!(f, "1.0"),
            XMLVersion::Unknown => write!(f, "1.0"),
        }
    }
}

impl FromStr for XMLVersion {
    type Err = Infallible;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "1.0" => Ok(XMLVersion::XML10),
            _ => Ok(XMLVersion::Unknown),
        }
    }
}
