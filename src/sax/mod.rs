pub mod error;
pub mod handler;
pub mod parser;
pub mod source;

use std::{
    borrow::Cow,
    collections::HashMap,
    sync::{
        Arc, LazyLock, RwLock,
        atomic::{AtomicUsize, Ordering},
    },
};

use anyxml_uri::uri::{URIStr, URIString};

use crate::error::XMLError;

pub struct Attribute {
    pub(crate) uri: Option<Arc<str>>,
    pub(crate) local_name: Option<Box<str>>,
    pub(crate) qname: Box<str>,
    pub(crate) value: Box<str>,
    // 0: is declared in DTD
    // 1: is specified explicitly (in other words, `value` is not the default value provided by DTD)
    // 2: is namespace declaration attribute
    pub(crate) flag: u8,
}

impl Attribute {
    pub(crate) fn set_declared(&mut self) {
        self.flag |= 1 << 0;
    }
    pub(crate) fn set_specified(&mut self) {
        self.flag |= 1 << 1;
    }
    pub(crate) fn set_nsdecl(&mut self) {
        self.flag |= 1 << 2;
    }

    pub fn is_declared(&self) -> bool {
        self.flag & (1 << 0) != 0
    }
    pub fn is_specified(&self) -> bool {
        self.flag & (1 << 1) != 0
    }
    pub fn is_nsdecl(&self) -> bool {
        self.flag & (1 << 2) != 0
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub enum AttributeType {
    #[default]
    CDATA,
    ID,
    IDREF,
    IDREFS,
    ENTITY,
    ENTITIES,
    NMTOKEN,
    NMTOKENS,
    NOTATION(Vec<Box<str>>),
    Enumeration(Vec<Box<str>>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DefaultDecl {
    REQUIRED,
    IMPLIED,
    FIXED(Box<str>),
    None(Box<str>),
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct AttlistDeclMap(
    HashMap<(Cow<'static, str>, Cow<'static, str>), (AttributeType, DefaultDecl)>,
);

impl AttlistDeclMap {
    /// Returns `true` if newly inserted, and `false` if an element or attribute with
    /// the same name is already registered.
    pub fn insert(
        &mut self,
        elem_name: impl Into<String>,
        attr_name: impl Into<String>,
        att_type: AttributeType,
        default_decl: DefaultDecl,
    ) -> bool {
        use std::collections::hash_map::Entry::*;
        let elem_name: String = elem_name.into();
        let attr_name: String = attr_name.into();
        match self.0.entry((Cow::Owned(elem_name), Cow::Owned(attr_name))) {
            Vacant(entry) => {
                entry.insert((att_type, default_decl));
                true
            }
            Occupied(_) => false,
        }
    }

    pub fn get<'a>(
        &'a self,
        elem_name: &'a str,
        attr_name: &'a str,
    ) -> Option<&'a (AttributeType, DefaultDecl)> {
        self.0
            .get(&(Cow::Borrowed(elem_name), Cow::Borrowed(attr_name)))
    }

    pub fn contains(&self, elem_name: &str, attr_name: &str) -> bool {
        self.get(elem_name, attr_name).is_some()
    }

    pub fn clear(&mut self) {
        self.0.clear();
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ContentSpec {
    EMPTY,
    ANY,
    Mixed(Vec<Box<str>>),
    Children(Box<str>),
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ElementDeclMap(HashMap<Box<str>, ContentSpec>);

impl ElementDeclMap {
    pub fn insert(
        &mut self,
        name: impl Into<Box<str>>,
        contentspec: ContentSpec,
    ) -> Result<(), XMLError> {
        use std::collections::hash_map::Entry::*;
        let name: Box<str> = name.into();
        match self.0.entry(name) {
            Occupied(_) => Err(XMLError::ParserDuplicateElementDecl),
            Vacant(entry) => {
                entry.insert(contentspec);
                Ok(())
            }
        }
    }

    pub fn get(&self, name: &str) -> Option<&ContentSpec> {
        self.0.get(name)
    }

    pub fn clear(&mut self) {
        self.0.clear();
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EntityDecl {
    InternalGeneralEntity {
        base_uri: Arc<URIStr>,
        replacement_text: Box<str>,
    },
    InternalParameterEntity {
        base_uri: Arc<URIStr>,
        replacement_text: Box<str>,
    },
    ExternalGeneralParsedEntity {
        base_uri: Arc<URIStr>,
        system_id: Box<URIStr>,
        public_id: Option<Box<str>>,
    },
    ExternalGeneralUnparsedEntity {
        base_uri: Arc<URIStr>,
        system_id: Box<URIStr>,
        public_id: Option<Box<str>>,
        notation_name: Box<str>,
    },
    ExternalParameterEntity {
        base_uri: Arc<URIStr>,
        system_id: Box<URIStr>,
        public_id: Option<Box<str>>,
    },
}

static PREDEFINED_ENTITY_LT: LazyLock<EntityDecl> =
    LazyLock::new(|| EntityDecl::InternalGeneralEntity {
        base_uri: URIString::parse("#predefined").unwrap().into(),
        replacement_text: "&#60;".into(), // '<'
    });
static PREDEFINED_ENTITY_GT: LazyLock<EntityDecl> =
    LazyLock::new(|| EntityDecl::InternalGeneralEntity {
        base_uri: URIString::parse("#predefined").unwrap().into(),
        replacement_text: "&#62;".into(), // '>'
    });
static PREDEFINED_ENTITY_AMP: LazyLock<EntityDecl> =
    LazyLock::new(|| EntityDecl::InternalGeneralEntity {
        base_uri: URIString::parse("#predefined").unwrap().into(),
        replacement_text: "&#38;".into(), // '&'
    });
static PREDEFINED_ENTITY_APOS: LazyLock<EntityDecl> =
    LazyLock::new(|| EntityDecl::InternalGeneralEntity {
        base_uri: URIString::parse("#predefined").unwrap().into(),
        replacement_text: "&#39;".into(), // '''
    });
static PREDEFINED_ENTITY_QUOT: LazyLock<EntityDecl> =
    LazyLock::new(|| EntityDecl::InternalGeneralEntity {
        base_uri: URIString::parse("#predefined").unwrap().into(),
        replacement_text: "&#34;".into(), // '"'
    });

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct EntityMap(HashMap<Box<str>, EntityDecl>);

impl EntityMap {
    pub fn insert(&mut self, name: impl Into<Box<str>>, decl: EntityDecl) -> Result<(), XMLError> {
        use std::collections::hash_map::Entry::*;
        let name: Box<str> = name.into();
        match self.0.entry(name) {
            Occupied(_) => Err(XMLError::ParserDuplicateEntityDecl),
            Vacant(entry) => {
                entry.insert(decl);
                Ok(())
            }
        }
    }

    pub fn get(&self, name: &str) -> Option<&EntityDecl> {
        if let Some(decl) = self.0.get(name) {
            return Some(decl);
        }

        match name {
            "lt" => Some(&PREDEFINED_ENTITY_LT),
            "gt" => Some(&PREDEFINED_ENTITY_GT),
            "amp" => Some(&PREDEFINED_ENTITY_AMP),
            "apos" => Some(&PREDEFINED_ENTITY_APOS),
            "quot" => Some(&PREDEFINED_ENTITY_QUOT),
            _ => None,
        }
    }

    pub fn clear(&mut self) {
        self.0.clear();
    }
}

pub struct Locator {
    system_id: RwLock<Arc<URIStr>>,
    public_id: RwLock<Option<Arc<str>>>,
    line: AtomicUsize,
    column: AtomicUsize,
}

impl Locator {
    pub(crate) fn new(
        system_id: Arc<URIStr>,
        public_id: Option<Arc<str>>,
        line: usize,
        column: usize,
    ) -> Self {
        Self {
            system_id: RwLock::new(system_id),
            public_id: RwLock::new(public_id),
            line: line.into(),
            column: column.into(),
        }
    }

    pub fn system_id(&self) -> Arc<URIStr> {
        self.system_id.read().unwrap().clone()
    }

    pub fn public_id(&self) -> Option<Arc<str>> {
        self.public_id.read().unwrap().clone()
    }

    pub fn line(&self) -> usize {
        self.line.load(Ordering::Acquire)
    }

    pub fn column(&self) -> usize {
        self.column.load(Ordering::Acquire)
    }

    pub(crate) fn set_system_id(&self, system_id: Arc<URIStr>) {
        *self.system_id.write().unwrap() = system_id;
    }

    pub(crate) fn set_public_id(&self, public_id: Option<Arc<str>>) {
        *self.public_id.write().unwrap() = public_id;
    }

    pub(crate) fn set_line(&self, line: usize) {
        self.line.store(line, Ordering::Release);
    }

    pub(crate) fn set_column(&self, column: usize) {
        self.column.store(column, Ordering::Release);
    }

    pub(crate) fn update_line(&self, f: impl Fn(usize) -> usize) {
        while self
            .line
            .fetch_update(Ordering::Release, Ordering::Acquire, |line| Some(f(line)))
            .is_err()
        {}
    }

    pub(crate) fn update_column(&self, f: impl Fn(usize) -> usize) {
        while self
            .column
            .fetch_update(Ordering::Release, Ordering::Acquire, |column| {
                Some(f(column))
            })
            .is_err()
        {}
    }
}
