use std::{
    collections::{HashMap, HashSet},
    io::Read,
    mem::{replace, take},
    sync::{Arc, RwLock, atomic::AtomicUsize},
};

use anyxml_uri::uri::{URIStr, URIString};

use crate::{
    DefaultParserSpec, ParserSpec, ProgressiveParserSpec, ProgressiveParserSpecificContext,
    XMLVersion,
    encoding::UTF8_NAME,
    error::XMLError,
    sax::{
        AttlistDeclMap, ElementDeclMap, EntityMap, Locator, NamespaceStack, Notation,
        contentspec::ContentSpecValidationContext,
        error::fatal_error,
        handler::{DefaultSAXHandler, SAXHandler},
        source::{INPUT_CHUNK, InputSource},
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
    InMiscAfterXMLDeclaration,
    InInternalSubset,
    InExternalSubset,
    InTextDeclaration,
    InMiscAfterDOCTYPEDeclaration,
    DocumentElement,
    InContent,
    InMiscAfterDocumentElement,
    Finished,
}

pub struct XMLReader<Spec: ParserSpec, H: SAXHandler = DefaultSAXHandler> {
    pub(crate) source: Box<Spec::Reader>,
    pub handler: H,
    pub(crate) locator: Arc<Locator>,
    pub(crate) config: ParserConfig,
    default_base_uri: Option<Arc<URIStr>>,
    pub(crate) base_uri: Arc<URIStr>,
    pub(crate) entity_name: Option<Arc<str>>,

    // Parser Specific Context
    pub(crate) specific_context: Spec::SpecificContext,

    // Entity Stack
    source_stack: Vec<Box<Spec::Reader>>,
    locator_stack: Vec<Locator>,
    base_uri_stack: Vec<Arc<URIStr>>,
    entity_name_stack: Vec<Option<Arc<str>>>,

    // Parser Context
    pub(crate) state: ParserState,
    pub(crate) fatal_error_occurred: bool,
    pub(crate) version: XMLVersion,
    pub(crate) encoding: Option<String>,
    pub(crate) standalone: Option<bool>,
    pub(crate) dtd_name: String,
    pub(crate) has_internal_subset: bool,
    pub(crate) has_external_subset: bool,
    pub(crate) has_parameter_entity: bool,
    pub(crate) namespaces: NamespaceStack,
    pub(crate) entities: EntityMap,
    pub(crate) notations: HashMap<Box<str>, Notation>,
    pub(crate) elementdecls: ElementDeclMap,
    pub(crate) attlistdecls: AttlistDeclMap,
    pub(crate) validation_stack: Vec<Option<(Box<str>, ContentSpecValidationContext)>>,
    // key: element name
    // value: attribute name declared as ID,
    pub(crate) idattr_decls: HashMap<Box<str>, Box<str>>,
    pub(crate) specified_ids: HashSet<Box<str>>,
    pub(crate) unresolved_ids: HashSet<Box<str>>,
}

impl<Spec: ParserSpec, H: SAXHandler> XMLReader<Spec, H> {
    pub fn default_base_uri(&self) -> Result<Arc<URIStr>, XMLError> {
        if let Some(base_uri) = self.default_base_uri.clone() {
            return Ok(base_uri);
        }

        let mut pwd = std::env::current_dir()?;
        if !pwd.is_absolute() {
            pwd = pwd.canonicalize()?;
        }
        Ok(URIString::parse_file_path(pwd)?.into())
    }

    pub fn set_default_base_uri(
        &mut self,
        base_uri: impl Into<Arc<URIStr>>,
    ) -> Result<(), XMLError> {
        let base_uri: Arc<URIStr> = base_uri.into();
        if base_uri.is_absolute() {
            self.default_base_uri = Some(base_uri);
            self.base_uri = self.default_base_uri()?;
            Ok(())
        } else {
            Err(XMLError::URIBaseURINotAbsolute)
        }
    }

    pub fn handler(&self) -> &H {
        &self.handler
    }

    pub fn replace_handler(&mut self, handler: H) -> H {
        replace(&mut self.handler, handler)
    }

    pub fn entity_name(&self) -> Option<Arc<str>> {
        self.entity_name.clone()
    }

    pub fn reset_context(&mut self) {
        self.entity_name = None;

        // reset Entity Stack
        self.source_stack.clear();
        self.locator_stack.clear();
        self.base_uri_stack.clear();
        self.entity_name_stack.clear();

        // reset Parser Context
        self.state = ParserState::BeforeStart;
        self.fatal_error_occurred = false;
        self.version = XMLVersion::default();
        self.encoding = None;
        self.standalone = None;
        self.dtd_name.clear();
        self.has_internal_subset = false;
        self.has_external_subset = false;
        self.has_parameter_entity = false;
        self.namespaces.clear();
        self.entities.clear();
        self.notations.clear();
        self.elementdecls.clear();
        self.attlistdecls.clear();
        self.validation_stack.clear();
        self.idattr_decls.clear();
        self.specified_ids.clear();
        self.unresolved_ids.clear();
    }
}

impl<Spec: ParserSpec, H: SAXHandler + Default> XMLReader<Spec, H> {
    pub fn take_handler(&mut self) -> H {
        take(&mut self.handler)
    }
}

impl<'a, H: SAXHandler> XMLReader<DefaultParserSpec<'a>, H> {
    pub fn parse_uri(
        &mut self,
        uri: impl AsRef<URIStr>,
        encoding: Option<&str>,
    ) -> Result<(), XMLError> {
        self.reset_context();
        self.encoding = encoding.map(|enc| enc.to_owned());
        self.base_uri = self.default_base_uri()?;
        self.source = Box::new(self.handler.resolve_entity(
            "[document]",
            None,
            &self.base_uri,
            uri.as_ref(),
        )?);
        if let Some(system_id) = self.source.system_id() {
            let mut base_uri = self.base_uri.resolve(system_id);
            base_uri.normalize();
            self.base_uri = base_uri.into();
        }
        self.locator = Arc::new(Locator::new(self.base_uri.clone(), None, 1, 1));
        self.parse_document().inspect_err(|&err| {
            fatal_error!(self, err, "Unrecoverable error: {}", err);
        })
    }

    pub fn parse_reader(
        &mut self,
        reader: impl Read + 'a,
        encoding: Option<&str>,
        uri: Option<&URIStr>,
    ) -> Result<(), XMLError> {
        self.reset_context();
        self.encoding = encoding.map(|enc| enc.to_owned());
        self.base_uri = self.default_base_uri()?;
        self.source = Box::new(InputSource::from_reader(reader, encoding)?);
        if let Some(uri) = uri {
            let mut base_uri = self.base_uri.resolve(uri);
            base_uri.normalize();
            self.base_uri = base_uri.into();
        }
        self.locator = Arc::new(Locator::new(self.base_uri.clone(), None, 1, 1));
        self.parse_document().inspect_err(|&err| {
            fatal_error!(self, err, "Unrecoverable error: {}", err);
        })
    }

    pub fn parse_str(&mut self, str: &str, uri: Option<&URIStr>) -> Result<(), XMLError> {
        self.reset_context();
        self.encoding = Some(UTF8_NAME.into());
        self.base_uri = self.default_base_uri()?;
        self.source = Box::new(InputSource::from_content(str));
        if let Some(uri) = uri {
            let mut base_uri = self.base_uri.resolve(uri);
            base_uri.normalize();
            self.base_uri = base_uri.into();
        }
        self.locator = Arc::new(Locator::new(self.base_uri.clone(), None, 1, 1));
        self.parse_document().inspect_err(|&err| {
            fatal_error!(self, err, "Unrecoverable error: {}", err);
        })
    }

    pub fn reset(&mut self) -> Result<(), XMLError> {
        self.source = Box::new(InputSource::default());
        self.config = ParserConfig::default();
        self.default_base_uri = None;
        self.base_uri = URIString::parse("")?.into();
        self.locator = Arc::new(Locator::new(self.base_uri.clone(), None, 1, 1));

        self.reset_context();
        Ok(())
    }
}

impl<H: SAXHandler> XMLReader<ProgressiveParserSpec, H> {
    pub fn reset_source(&mut self) -> Result<(), XMLError> {
        self.source = Box::new(InputSource::default());
        self.source.set_progressive_mode();
        self.base_uri = self.default_base_uri()?;
        self.specific_context.seen = 0;
        self.specific_context.quote = 0;
        self.specific_context.in_markup = false;
        self.specific_context.element_stack.clear();
        self.reset_context();
        Ok(())
    }

    pub fn parse_chunk(&mut self, chunk: impl AsRef<[u8]>, finish: bool) -> Result<(), XMLError> {
        (|| {
            let chunk = chunk.as_ref();
            for bytes in chunk.chunks(INPUT_CHUNK) {
                self.source.push_bytes(bytes, false)?;
                while self.parse_event_once(false)? {}
            }
            if finish {
                self.source.push_bytes([], true)?;
                while self.parse_event_once(true)? {}
            }
            Ok(())
        })()
        .inspect_err(|&err| {
            fatal_error!(self, err, "Unrecoverable error: {}", err);
        })
    }
}

impl<'a, Spec: ParserSpec<Reader = InputSource<'a>>, H: SAXHandler> XMLReader<Spec, H> {
    pub(crate) fn push_source(
        &mut self,
        source: Box<InputSource<'a>>,
        mut base_uri: Arc<URIStr>,
        entity_name: Option<Arc<str>>,
        system_id: Arc<URIStr>,
        public_id: Option<Arc<str>>,
    ) -> Result<(), XMLError> {
        self.source_stack.push(replace(&mut self.source, source));
        // The only instance where `base_uri` is not an absolute URI should be
        // the pseudo-base URI used within the library...
        if !base_uri.is_absolute() {
            base_uri = self.base_uri.clone();
        }
        base_uri = base_uri.resolve(&system_id).into();
        self.base_uri_stack
            .push(replace(&mut self.base_uri, base_uri.clone()));
        self.entity_name_stack
            .push(replace(&mut self.entity_name, entity_name));
        self.locator_stack.push(Locator {
            system_id: RwLock::new(self.locator.system_id()),
            public_id: RwLock::new(self.locator.public_id()),
            line: AtomicUsize::new(self.locator.line()),
            column: AtomicUsize::new(self.locator.column()),
        });
        self.locator.set_system_id(base_uri);
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

    /// Return `true` if it is already inside an entity named `name`.  \
    /// Otherwise, return `false`.
    pub(crate) fn entity_recursion_check(&self, name: &str) -> bool {
        self.entity_name_stack
            .iter()
            .any(|prev| prev.as_deref() == Some(name))
    }

    /// Returns `true` if the current entity is either an external DTD subset or a parameter entity.
    ///
    /// If this method returns `true` when the markup declaration appears,
    /// then that markup declaration is an external markup declaration.
    ///
    /// # Reference
    /// [2.9 Standalone Document Declaration](https://www.w3.org/TR/2008/REC-xml-20081126/#sec-rmd)
    /// ```text
    /// [Definition: An external markup declaration is defined as a markup declaration occurring in the external subset or in a parameter entity (external or internal, the latter being included because non-validating processors are not required to read them).]
    /// ```
    pub(crate) fn is_external_markup(&self) -> bool {
        self.state == ParserState::InExternalSubset
            || self
                .entity_name
                .as_deref()
                .is_some_and(|name| name.starts_with('%'))
    }
}

impl<'a> Default for XMLReader<DefaultParserSpec<'a>> {
    fn default() -> Self {
        let base_uri: Arc<URIStr> = URIString::parse("").unwrap().into();
        Self {
            source: Box::new(InputSource::default()),
            handler: DefaultSAXHandler,
            locator: Arc::new(Locator::new(base_uri.clone(), None, 1, 1)),
            config: ParserConfig::default(),
            default_base_uri: None,
            base_uri,
            entity_name: None,
            specific_context: (),
            source_stack: vec![],
            locator_stack: vec![],
            base_uri_stack: vec![],
            entity_name_stack: vec![],
            state: ParserState::BeforeStart,
            fatal_error_occurred: false,
            version: XMLVersion::default(),
            encoding: None,
            standalone: None,
            dtd_name: String::new(),
            has_internal_subset: false,
            has_external_subset: false,
            has_parameter_entity: false,
            namespaces: Default::default(),
            entities: Default::default(),
            notations: Default::default(),
            elementdecls: Default::default(),
            attlistdecls: Default::default(),
            validation_stack: vec![],
            idattr_decls: HashMap::new(),
            specified_ids: HashSet::new(),
            unresolved_ids: HashSet::new(),
        }
    }
}

pub struct XMLReaderBuilder<'a, H: SAXHandler = DefaultSAXHandler> {
    reader: XMLReader<DefaultParserSpec<'a>, H>,
}

impl<'a> XMLReaderBuilder<'a> {
    pub fn new() -> Self {
        Self {
            reader: Default::default(),
        }
    }
}

impl<'a, H: SAXHandler> XMLReaderBuilder<'a, H> {
    pub fn set_default_base_uri(
        mut self,
        base_uri: impl Into<Arc<URIStr>>,
    ) -> Result<Self, XMLError> {
        self.reader.set_default_base_uri(base_uri)?;
        Ok(self)
    }

    pub fn set_handler<I: SAXHandler>(self, handler: I) -> XMLReaderBuilder<'a, I> {
        XMLReaderBuilder {
            reader: XMLReader {
                source: self.reader.source,
                handler,
                locator: self.reader.locator,
                config: self.reader.config,
                default_base_uri: self.reader.default_base_uri,
                base_uri: self.reader.base_uri,
                entity_name: self.reader.entity_name,
                specific_context: self.reader.specific_context,
                source_stack: self.reader.source_stack,
                locator_stack: self.reader.locator_stack,
                base_uri_stack: self.reader.base_uri_stack,
                entity_name_stack: self.reader.entity_name_stack,
                state: self.reader.state,
                fatal_error_occurred: self.reader.fatal_error_occurred,
                version: self.reader.version,
                encoding: self.reader.encoding,
                standalone: self.reader.standalone,
                dtd_name: self.reader.dtd_name,
                has_internal_subset: self.reader.has_internal_subset,
                has_external_subset: self.reader.has_external_subset,
                has_parameter_entity: self.reader.has_parameter_entity,
                namespaces: self.reader.namespaces,
                entities: self.reader.entities,
                notations: self.reader.notations,
                elementdecls: self.reader.elementdecls,
                attlistdecls: self.reader.attlistdecls,
                validation_stack: self.reader.validation_stack,
                idattr_decls: self.reader.idattr_decls,
                specified_ids: self.reader.specified_ids,
                unresolved_ids: self.reader.unresolved_ids,
            },
        }
    }

    pub fn set_parser_config(mut self, config: ParserConfig) -> Self {
        self.reader.config = config;
        self
    }
    pub fn enable_option(mut self, option: ParserOption) -> Self {
        self.reader.config.set_option(option, true);
        self
    }
    pub fn disable_option(mut self, option: ParserOption) -> Self {
        self.reader.config.set_option(option, false);
        self
    }

    pub fn progressive_parser(self) -> XMLProgressiveReaderBuilder<H> {
        let mut source = Box::new(InputSource::default());
        source.set_progressive_mode();
        XMLProgressiveReaderBuilder {
            reader: XMLReader::<ProgressiveParserSpec, H> {
                source,
                handler: self.reader.handler,
                locator: self.reader.locator,
                config: self.reader.config,
                default_base_uri: self.reader.default_base_uri,
                base_uri: self.reader.base_uri,
                entity_name: self.reader.entity_name,
                specific_context: ProgressiveParserSpecificContext::default(),
                source_stack: vec![],
                locator_stack: self.reader.locator_stack,
                base_uri_stack: self.reader.base_uri_stack,
                entity_name_stack: self.reader.entity_name_stack,
                state: self.reader.state,
                fatal_error_occurred: self.reader.fatal_error_occurred,
                version: self.reader.version,
                encoding: self.reader.encoding,
                standalone: self.reader.standalone,
                dtd_name: self.reader.dtd_name,
                has_internal_subset: self.reader.has_internal_subset,
                has_external_subset: self.reader.has_external_subset,
                has_parameter_entity: self.reader.has_parameter_entity,
                namespaces: self.reader.namespaces,
                entities: self.reader.entities,
                notations: self.reader.notations,
                elementdecls: self.reader.elementdecls,
                attlistdecls: self.reader.attlistdecls,
                validation_stack: self.reader.validation_stack,
                idattr_decls: self.reader.idattr_decls,
                specified_ids: self.reader.specified_ids,
                unresolved_ids: self.reader.unresolved_ids,
            },
        }
    }

    pub fn build(self) -> XMLReader<DefaultParserSpec<'a>, H> {
        self.reader
    }
}

impl<'a> Default for XMLReaderBuilder<'a> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct XMLProgressiveReaderBuilder<H: SAXHandler = DefaultSAXHandler> {
    reader: XMLReader<ProgressiveParserSpec, H>,
}

impl<H: SAXHandler> XMLProgressiveReaderBuilder<H> {
    pub fn set_default_base_uri(
        mut self,
        base_uri: impl Into<Arc<URIStr>>,
    ) -> Result<Self, XMLError> {
        self.reader.set_default_base_uri(base_uri)?;
        Ok(self)
    }

    pub fn set_handler<I: SAXHandler>(self, handler: I) -> XMLProgressiveReaderBuilder<I> {
        XMLProgressiveReaderBuilder {
            reader: XMLReader {
                source: self.reader.source,
                handler,
                locator: self.reader.locator,
                config: self.reader.config,
                default_base_uri: self.reader.default_base_uri,
                base_uri: self.reader.base_uri,
                entity_name: self.reader.entity_name,
                specific_context: self.reader.specific_context,
                source_stack: self.reader.source_stack,
                locator_stack: self.reader.locator_stack,
                base_uri_stack: self.reader.base_uri_stack,
                entity_name_stack: self.reader.entity_name_stack,
                state: self.reader.state,
                fatal_error_occurred: self.reader.fatal_error_occurred,
                version: self.reader.version,
                encoding: self.reader.encoding,
                standalone: self.reader.standalone,
                dtd_name: self.reader.dtd_name,
                has_internal_subset: self.reader.has_internal_subset,
                has_external_subset: self.reader.has_external_subset,
                has_parameter_entity: self.reader.has_parameter_entity,
                namespaces: self.reader.namespaces,
                entities: self.reader.entities,
                notations: self.reader.notations,
                elementdecls: self.reader.elementdecls,
                attlistdecls: self.reader.attlistdecls,
                validation_stack: self.reader.validation_stack,
                idattr_decls: self.reader.idattr_decls,
                specified_ids: self.reader.specified_ids,
                unresolved_ids: self.reader.unresolved_ids,
            },
        }
    }

    pub fn set_parser_config(mut self, config: ParserConfig) -> Self {
        self.reader.config = config;
        self
    }
    pub fn enable_option(mut self, option: ParserOption) -> Self {
        self.reader.config.set_option(option, true);
        self
    }
    pub fn disable_option(mut self, option: ParserOption) -> Self {
        self.reader.config.set_option(option, false);
        self
    }

    pub fn build(self) -> XMLReader<ProgressiveParserSpec, H> {
        self.reader
    }
}
