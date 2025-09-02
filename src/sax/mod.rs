pub mod contentspec;
pub mod error;
pub mod handler;
pub mod parser;
pub mod source;

use std::{
    collections::{HashMap, HashSet},
    sync::{
        Arc, LazyLock, RwLock,
        atomic::{AtomicUsize, Ordering},
    },
};

use anyxml_uri::uri::{URIStr, URIString};

use crate::{error::XMLError, sax::contentspec::ContentSpec};

pub struct Attribute {
    pub uri: Option<Arc<str>>,
    pub local_name: Option<Box<str>>,
    pub qname: Box<str>,
    pub value: Box<str>,
    // 0: is declared in DTD
    // 1: is specified explicitly (in other words, `value` is not the default value provided by DTD)
    // 2: is namespace declaration attribute
    // 3: has declaration dependency normalization
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
    pub(crate) fn set_declaration_dependent_normalization(&mut self) {
        self.flag |= 1 << 3;
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
    /// Check if this attribute's value is modified by
    pub(crate) fn has_declaration_dependent_normalization(&self) -> bool {
        self.flag & (1 << 3) != 0
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
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
    NOTATION(HashSet<Box<str>>),
    Enumeration(HashSet<Box<str>>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DefaultDecl {
    REQUIRED,
    IMPLIED,
    FIXED(Box<str>),
    None(Box<str>),
}

#[allow(clippy::type_complexity)]
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct AttlistDeclMap(
    // (attribute type, default value declaration, is external markup declaration)
    HashMap<Box<str>, HashMap<Box<str>, (AttributeType, DefaultDecl, bool)>>,
);

impl AttlistDeclMap {
    /// Returns `true` if newly inserted, and `false` if an element or attribute with
    /// the same name is already registered.
    pub fn insert(
        &mut self,
        elem_name: impl Into<Box<str>>,
        attr_name: impl Into<Box<str>>,
        att_type: AttributeType,
        default_decl: DefaultDecl,
        is_external_markup: bool,
    ) -> bool {
        use std::collections::hash_map::Entry::*;
        let elem_name: Box<str> = elem_name.into();
        let attr_name: Box<str> = attr_name.into();
        match self.0.entry(elem_name) {
            Vacant(entry) => {
                entry.insert(HashMap::from([(
                    attr_name,
                    (att_type, default_decl, is_external_markup),
                )]));
            }
            Occupied(mut entry) => {
                let map = entry.get_mut();
                match map.entry(attr_name) {
                    Vacant(entry) => {
                        entry.insert((att_type, default_decl, is_external_markup));
                    }
                    Occupied(_) => return false,
                }
            }
        }
        true
    }

    pub fn get(
        &self,
        elem_name: &str,
        attr_name: &str,
    ) -> Option<&(AttributeType, DefaultDecl, bool)> {
        self.0.get(elem_name)?.get(attr_name)
    }

    pub fn contains(&self, elem_name: &str, attr_name: &str) -> bool {
        self.get(elem_name, attr_name).is_some()
    }

    pub fn clear(&mut self) {
        self.0.clear();
    }

    pub fn attlist(
        &self,
        elem_name: &str,
    ) -> Option<impl Iterator<Item = (&str, &(AttributeType, DefaultDecl, bool))>> {
        self.0
            .get(elem_name)
            .map(|map| map.iter().map(|(attr, value)| (attr.as_ref(), value)))
    }

    pub fn iter_all(
        &self,
    ) -> impl Iterator<Item = (&str, &str, &(AttributeType, DefaultDecl, bool))> {
        self.0.iter().flat_map(|(elem, map)| {
            map.iter()
                .map(move |(attr, value)| (elem.as_ref(), attr.as_ref(), value))
        })
    }
}

#[derive(Debug, Clone, Default)]
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

    pub fn get_mut(&mut self, name: &str) -> Option<&mut ContentSpec> {
        self.0.get_mut(name)
    }

    pub fn contains(&self, name: &str) -> bool {
        self.0.contains_key(name)
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
        in_external_markup: bool,
    },
    InternalParameterEntity {
        base_uri: Arc<URIStr>,
        replacement_text: Box<str>,
    },
    ExternalGeneralParsedEntity {
        base_uri: Arc<URIStr>,
        system_id: Box<URIStr>,
        public_id: Option<Box<str>>,
        in_external_markup: bool,
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
        in_external_markup: false,
    });
static PREDEFINED_ENTITY_GT: LazyLock<EntityDecl> =
    LazyLock::new(|| EntityDecl::InternalGeneralEntity {
        base_uri: URIString::parse("#predefined").unwrap().into(),
        replacement_text: "&#62;".into(), // '>'
        in_external_markup: false,
    });
static PREDEFINED_ENTITY_AMP: LazyLock<EntityDecl> =
    LazyLock::new(|| EntityDecl::InternalGeneralEntity {
        base_uri: URIString::parse("#predefined").unwrap().into(),
        replacement_text: "&#38;".into(), // '&'
        in_external_markup: false,
    });
static PREDEFINED_ENTITY_APOS: LazyLock<EntityDecl> =
    LazyLock::new(|| EntityDecl::InternalGeneralEntity {
        base_uri: URIString::parse("#predefined").unwrap().into(),
        replacement_text: "&#39;".into(), // '''
        in_external_markup: false,
    });
static PREDEFINED_ENTITY_QUOT: LazyLock<EntityDecl> =
    LazyLock::new(|| EntityDecl::InternalGeneralEntity {
        base_uri: URIString::parse("#predefined").unwrap().into(),
        replacement_text: "&#34;".into(), // '"'
        in_external_markup: false,
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

    pub fn iter(&self) -> impl Iterator<Item = (&str, &EntityDecl)> {
        self.0.iter().map(|(name, decl)| (name.as_ref(), decl))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Hash)]
pub struct Notation {
    pub name: Box<str>,
    pub system_id: Option<Box<str>>,
    pub public_id: Option<Box<str>>,
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
