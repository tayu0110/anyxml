use std::{
    collections::HashMap,
    io::Read,
    mem::replace,
    path::{Path, PathBuf},
    sync::{Arc, RwLock, atomic::AtomicUsize},
};

use crate::{
    DefaultParserSpec, EntityDecl, ParserSpec, XML_XML_NAMESPACE, XMLVersion,
    encoding::UTF8_NAME,
    error::XMLError,
    sax::{
        Locator,
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

impl ParserConfig {
    pub fn is_enable(&self, option: ParserOption) -> bool {
        self.flags & (1 << option as i32) != 0
    }

    pub fn set_option(&mut self, option: ParserOption, flag: bool) {
        if flag {
            self.flags |= 1 << (option as i32);
        } else {
            self.flags &= !(1 << (option as i32));
        }
    }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParserState {
    BeforeStart,
    InXMLDeclaration,
    InInternalSubset,
    InExternalSubset,
    InTextDeclaration,
    Parsing,
    FatalErrorOccurred,
    Finished,
}

pub struct XMLReader<Spec: ParserSpec> {
    pub(crate) source: Box<Spec::Reader>,
    pub(crate) content_handler: Arc<dyn ContentHandler>,
    pub(crate) decl_handler: Arc<dyn DeclHandler>,
    pub(crate) dtd_handler: Arc<dyn DTDHandler>,
    pub(crate) entity_resolver: Arc<dyn EntityResolver>,
    pub(crate) error_handler: Arc<dyn ErrorHandler>,
    pub(crate) lexical_handler: Arc<dyn LexicalHandler>,
    pub(crate) locator: Arc<Locator>,
    pub(crate) config: ParserConfig,
    pub(crate) base_uri: Arc<Path>,
    pub(crate) entity_name: Option<Arc<str>>,

    // Entity Stack
    source_stack: Vec<Box<Spec::Reader>>,
    locator_stack: Vec<Locator>,
    base_uri_stack: Vec<Arc<Path>>,
    pub(crate) entity_name_stack: Vec<Option<Arc<str>>>,

    // Parser Context
    pub(crate) state: ParserState,
    pub(crate) version: XMLVersion,
    pub(crate) encoding: Option<String>,
    pub(crate) standalone: Option<bool>,
    pub(crate) has_internal_subset: bool,
    pub(crate) has_external_subset: bool,
    // (prefix, namespace name, before overwrite)
    // Namespaces declared closer to the document element appear earlier in the list.
    // The second `usize` is the position of the namespace that bound the same prefix until
    // the namespace declaration appeared. If there is no such namespace, it is `usize::MAX`.
    pub(crate) namespaces: Vec<(Arc<str>, Arc<str>, usize)>,
    // (prefix, position in `namespaces`)
    pub(crate) prefix_map: HashMap<Arc<str>, usize>,
    pub(crate) entities: HashMap<Box<str>, EntityDecl>,
}

impl<Spec: ParserSpec> XMLReader<Spec> {
    pub fn content_handler(&self) -> Arc<dyn ContentHandler> {
        self.content_handler.clone()
    }
    pub fn decl_handler(&self) -> Arc<dyn DeclHandler> {
        self.decl_handler.clone()
    }
    pub fn dtd_handler(&self) -> Arc<dyn DTDHandler> {
        self.dtd_handler.clone()
    }
    pub fn entity_resolver(&self) -> Arc<dyn EntityResolver> {
        self.entity_resolver.clone()
    }
    pub fn error_handler(&self) -> Arc<dyn ErrorHandler> {
        self.error_handler.clone()
    }
    pub fn lexical_handler(&self) -> Arc<dyn LexicalHandler> {
        self.lexical_handler.clone()
    }

    pub fn set_content_handler(
        &mut self,
        handler: Arc<dyn ContentHandler>,
    ) -> Arc<dyn ContentHandler> {
        replace(&mut self.content_handler, handler)
    }
    pub fn set_decl_handler(&mut self, handler: Arc<dyn DeclHandler>) -> Arc<dyn DeclHandler> {
        replace(&mut self.decl_handler, handler)
    }
    pub fn set_dtd_handler(&mut self, handler: Arc<dyn DTDHandler>) -> Arc<dyn DTDHandler> {
        replace(&mut self.dtd_handler, handler)
    }
    pub fn set_entity_resolver(
        &mut self,
        handler: Arc<dyn EntityResolver>,
    ) -> Arc<dyn EntityResolver> {
        replace(&mut self.entity_resolver, handler)
    }
    pub fn set_error_handler(&mut self, handler: Arc<dyn ErrorHandler>) -> Arc<dyn ErrorHandler> {
        replace(&mut self.error_handler, handler)
    }
    pub fn set_lexical_handler(
        &mut self,
        handler: Arc<dyn LexicalHandler>,
    ) -> Arc<dyn LexicalHandler> {
        replace(&mut self.lexical_handler, handler)
    }
}

impl<'a> XMLReader<DefaultParserSpec<'a>> {
    pub fn parse_uri(&mut self, uri: &str, encoding: Option<&str>) -> Result<(), XMLError> {
        self.base_uri = PathBuf::from(uri).into();
        self.locator = Arc::new(Locator::new(self.base_uri.clone(), None, 1, 1));
        self.encoding = encoding.map(|enc| enc.to_owned());
        todo!()
    }

    pub fn parse_reader(
        &mut self,
        reader: impl Read + 'a,
        encoding: Option<&str>,
        uri: Option<&str>,
    ) -> Result<(), XMLError> {
        self.encoding = encoding.map(|enc| enc.to_owned());
        self.source = Box::new(InputSource::from_reader(reader, encoding)?);
        if let Some(uri) = uri {
            self.base_uri = PathBuf::from(uri).into();
        } else {
            self.base_uri = std::env::current_exe()?.into();
        }
        self.locator = Arc::new(Locator::new(self.base_uri.clone(), None, 1, 1));
        todo!()
    }

    pub fn parse_str(&mut self, str: &str, uri: Option<&str>) -> Result<(), XMLError> {
        self.encoding = Some(UTF8_NAME.into());
        self.source = Box::new(InputSource::from_content(str));
        if let Some(uri) = uri {
            self.base_uri = PathBuf::from(uri).into();
        } else {
            self.base_uri = std::env::current_exe()?.into();
        }
        self.locator = Arc::new(Locator::new(self.base_uri.clone(), None, 1, 1));
        self.parse_document()?;
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
        self.base_uri = PathBuf::from("").into();
        self.locator = Arc::new(Locator::new(self.base_uri.clone(), None, 1, 1));
        self.entity_name = None;

        // reset Entity Stack
        self.source_stack.clear();
        self.locator_stack.clear();
        self.base_uri_stack.clear();
        self.entity_name_stack.clear();

        // reset Parser Context
        self.state = ParserState::BeforeStart;
        self.version = XMLVersion::default();
        self.encoding = None;
        self.standalone = None;
        self.has_internal_subset = false;
        self.has_external_subset = false;
        self.namespaces.clear();
        // 'xml' prefix
        let xml: Arc<str> = "xml".into();
        self.namespaces
            .push((xml.clone(), XML_XML_NAMESPACE.into(), usize::MAX));
        self.prefix_map.clear();
        self.prefix_map.insert(xml, 0);
        self.entities.clear();
    }

    pub(crate) fn grow(&mut self) -> Result<(), XMLError> {
        let ret = self.source.grow();
        if (self.state == ParserState::InXMLDeclaration && self.encoding.is_none())
            || self.state == ParserState::InTextDeclaration
        {
            // Until the XML declaration (especially the encoding declaration) is read completely,
            // it may not be possible to set the decoder appropriately,
            // and `self.source.grow` may throw an error.
            // Such errors should be suppressed.
            Ok(())
        } else {
            // If external encoding is specified or XML declaration has already read,
            // decoding should not fail, so the error should be reported as is.
            ret
        }
    }

    pub(crate) fn push_source(
        &mut self,
        source: Box<InputSource<'a>>,
        base_uri: Arc<Path>,
        entity_name: Option<Arc<str>>,
        system_id: Arc<Path>,
        public_id: Option<Arc<str>>,
    ) -> Result<(), XMLError> {
        self.source_stack.push(replace(&mut self.source, source));
        self.base_uri_stack
            .push(replace(&mut self.base_uri, base_uri));
        self.entity_name_stack
            .push(replace(&mut self.entity_name, entity_name));
        self.locator_stack.push(Locator {
            system_id: RwLock::new(self.locator.system_id()),
            public_id: RwLock::new(self.locator.public_id()),
            line: AtomicUsize::new(self.locator.line()),
            column: AtomicUsize::new(self.locator.column()),
        });
        self.locator.set_system_id(system_id);
        self.locator.set_public_id(public_id);
        self.locator.set_line(1);
        self.locator.set_column(1);
        Ok(())
    }

    pub(crate) fn pop_source(&mut self) -> Result<(), XMLError> {
        if self.source_stack.is_empty() {
            return Err(XMLError::InternalError);
        }

        self.source = self.source_stack.pop().unwrap();
        self.base_uri = self.base_uri_stack.pop().unwrap();
        self.entity_name = self.entity_name_stack.pop().unwrap();

        let locator = self.locator_stack.pop().unwrap();
        self.locator.set_system_id(locator.system_id());
        self.locator.set_public_id(locator.public_id());
        self.locator.set_line(locator.line());
        self.locator.set_column(locator.column());

        Ok(())
    }
}
