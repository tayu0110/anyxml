use std::{io::Read, sync::Arc};

use crate::{
    DefaultParserSpec, ParserSpec,
    error::XMLError,
    sax::{
        handler::{
            ContentHandler, DTDHandler, DeclHandler, DefaultSAXHandler, EntityResolver,
            ErrorHandler, LexicalHandler,
        },
        source::InputSource,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ParserOption {
    ExternalGeneralEntities = 0,
    ExternalParameterEntities = 1,
    Namespaces = 2,
    ResolveDTDURIs = 3,
    Validation = 4,
}

impl std::ops::BitOr<Self> for ParserOption {
    type Output = ParserConfig;

    fn bitor(self, rhs: Self) -> Self::Output {
        ParserConfig {
            flags: (1 << self as i32) | (1 << rhs as i32),
        }
    }
}

impl std::ops::BitOr<ParserConfig> for ParserOption {
    type Output = ParserConfig;

    fn bitor(self, rhs: ParserConfig) -> Self::Output {
        ParserConfig {
            flags: rhs.flags | (1 << self as i32),
        }
    }
}

pub struct ParserConfig {
    flags: u64,
}

impl Default for ParserConfig {
    fn default() -> Self {
        ParserOption::Namespaces | ParserOption::ResolveDTDURIs
    }
}

impl std::ops::BitOr<Self> for ParserConfig {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        ParserConfig {
            flags: self.flags | rhs.flags,
        }
    }
}

impl std::ops::BitOr<ParserOption> for ParserConfig {
    type Output = Self;

    fn bitor(self, rhs: ParserOption) -> Self::Output {
        ParserConfig {
            flags: self.flags | (1 << rhs as i32),
        }
    }
}

impl std::ops::BitOrAssign<ParserOption> for ParserConfig {
    fn bitor_assign(&mut self, rhs: ParserOption) {
        self.flags |= 1 << rhs as i32;
    }
}

impl std::ops::BitOrAssign<Self> for ParserConfig {
    fn bitor_assign(&mut self, rhs: Self) {
        self.flags |= rhs.flags;
    }
}

impl ParserConfig {
    pub fn get_option(&self, option: ParserOption) -> bool {
        (self.flags >> option as i32) & 1 != 0
    }

    pub fn set_option(&mut self, option: ParserOption, flag: bool) {
        if flag {
            self.flags |= 1 << (option as i32);
        } else {
            self.flags &= !(1 << (option as i32));
        }
    }
}

pub struct XMLReader<Spec: ParserSpec> {
    source: Box<Spec::Reader>,
    content_handler: Arc<dyn ContentHandler>,
    decl_handler: Arc<dyn DeclHandler>,
    dtd_handler: Arc<dyn DTDHandler>,
    entity_resolver: Arc<dyn EntityResolver>,
    error_handler: Arc<dyn ErrorHandler>,
    lexical_handler: Arc<dyn LexicalHandler>,
    config: ParserConfig,
}

impl<'a> XMLReader<DefaultParserSpec<'a>> {
    pub fn parse_uri(&mut self, uri: &str, encoding: Option<&str>) -> Result<(), XMLError> {
        todo!()
    }

    pub fn parse_reader(
        &mut self,
        reader: impl Read + 'a,
        encoding: Option<&str>,
        uri: Option<&str>,
    ) -> Result<(), XMLError> {
        self.source = Box::new(InputSource::from_reader(reader, encoding)?);
        todo!()
    }

    pub fn parse_str(&mut self, str: &str) -> Result<(), XMLError> {
        self.source = Box::new(InputSource::from_content(str));
        todo!()
    }

    pub fn reset(&mut self) {
        self.source = Box::new(InputSource::default());
        let handler = Arc::new(DefaultSAXHandler);
        self.content_handler = handler.clone();
        self.decl_handler = handler.clone();
        self.dtd_handler = handler.clone();
        self.entity_resolver = handler.clone();
        self.error_handler = handler.clone();
        self.lexical_handler = handler.clone();
        self.config = ParserConfig::default();
    }
}
