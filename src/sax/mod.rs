pub mod error;
pub mod handler;
pub mod parser;
pub mod source;

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::{
        Arc, LazyLock, RwLock,
        atomic::{AtomicUsize, Ordering},
    },
};

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
    pub(crate) fn unset_declared(&mut self) {
        self.flag &= !(1 << 0);
    }

    pub(crate) fn set_specified(&mut self) {
        self.flag |= 1 << 1;
    }
    pub(crate) fn unset_specified(&mut self) {
        self.flag &= !(1 << 1);
    }

    pub(crate) fn set_nsdecl(&mut self) {
        self.flag |= 1 << 2;
    }
    pub(crate) fn unset_nsdecl(&mut self) {
        self.flag &= !(1 << 2);
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ContentSpec {
    EMPTY,
    ANY,
    Mixed(Vec<Box<str>>),
    Children(Box<str>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EntityDecl {
    InternalGeneralEntity {
        base_uri: Arc<Path>,
        replacement_text: Box<str>,
    },
    InternalParameterEntity {
        base_uri: Arc<Path>,
        replacement_text: Box<str>,
    },
    ExternalGeneralParsedEntity {
        base_uri: Arc<Path>,
        system_id: Box<str>,
        public_id: Option<Box<str>>,
    },
    ExternalGeneralUnparsedEntity {
        base_uri: Arc<Path>,
        system_id: Box<str>,
        public_id: Option<Box<str>>,
    },
    ExternalParameterEntity {
        base_uri: Arc<Path>,
        system_id: Box<str>,
        public_id: Option<Box<str>>,
        notation_name: Box<str>,
    },
}

static PREDEFINED_ENTITY_LT: LazyLock<EntityDecl> =
    LazyLock::new(|| EntityDecl::InternalGeneralEntity {
        base_uri: PathBuf::from("#predefined").into(),
        replacement_text: "&#60;".into(), // '<'
    });
static PREDEFINED_ENTITY_GT: LazyLock<EntityDecl> =
    LazyLock::new(|| EntityDecl::InternalGeneralEntity {
        base_uri: PathBuf::from("#predefined").into(),
        replacement_text: "&#62;".into(), // '>'
    });
static PREDEFINED_ENTITY_AMP: LazyLock<EntityDecl> =
    LazyLock::new(|| EntityDecl::InternalGeneralEntity {
        base_uri: PathBuf::from("#predefined").into(),
        replacement_text: "&#38;".into(), // '&'
    });
static PREDEFINED_ENTITY_APOS: LazyLock<EntityDecl> =
    LazyLock::new(|| EntityDecl::InternalGeneralEntity {
        base_uri: PathBuf::from("#predefined").into(),
        replacement_text: "&#39;".into(), // '''
    });
static PREDEFINED_ENTITY_QUOT: LazyLock<EntityDecl> =
    LazyLock::new(|| EntityDecl::InternalGeneralEntity {
        base_uri: PathBuf::from("#predefined").into(),
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
    system_id: RwLock<Arc<Path>>,
    public_id: RwLock<Option<Arc<str>>>,
    line: AtomicUsize,
    column: AtomicUsize,
}

impl Locator {
    pub(crate) fn new(
        system_id: Arc<Path>,
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

    pub fn system_id(&self) -> Arc<Path> {
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

    pub(crate) fn set_system_id(&self, system_id: Arc<Path>) {
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
