//! Provide resource resolution APIs based on the OASIS standard
//! [XML Catalogs V1.1](https://groups.oasis-open.org/higherlogic/ws/public/download/14810/xml-catalogs.pdf/latest).

use std::{borrow::Cow, io::Read, sync::Arc};

use crate::{
    XMLVersion,
    error::XMLError,
    sax::{
        Locator,
        attributes::Attributes,
        error::SAXParseError,
        handler::{DefaultSAXHandler, EntityResolver, ErrorHandler, SAXHandler},
        source::InputSource,
    },
    uri::{URIStr, URIString},
};

pub const XML_CATALOG_NAMESPACE: &str = "urn:oasis:names:tc:entity:xmlns:xml:catalog";
pub const XML_CATALOG_PUBLICID: &str = "-//OASIS//DTD XML Catalogs V1.1//EN";

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
pub enum PreferMode {
    #[default]
    Public,
    System,
}

pub struct Catalog {
    entry_files: Vec<()>,
    entry_list: Vec<()>,
}

impl Catalog {
    pub fn resolve_external_id(
        &mut self,
        public_id: Option<&str>,
        system_id: Option<&URIStr>,
    ) -> Option<Arc<URIStr>> {
        todo!()
    }

    pub fn resolve_uri(&mut self, uri: impl AsRef<URIStr>) -> Option<Arc<URIStr>> {
        todo!()
    }
}

pub struct CatalogEntryFile {
    base_uri: Arc<URIStr>,
    next_catalog: Vec<Arc<URIStr>>,
}

impl CatalogEntryFile {
    pub fn parse_uri(
        uri: impl AsRef<URIStr>,
        encoding: Option<&str>,
    ) -> Result<CatalogEntryFile, XMLError> {
        todo!()
    }

    pub fn parse_reader<'a>(
        reader: impl Read + 'a,
        encoding: Option<&str>,
        uri: impl AsRef<URIStr>,
    ) -> Result<CatalogEntryFile, XMLError> {
        todo!()
    }

    pub fn parse_str(catalog: &str, uri: impl AsRef<URIStr>) -> Result<CatalogEntryFile, XMLError> {
        todo!()
    }
}

impl Default for CatalogEntryFile {
    fn default() -> Self {
        CatalogEntryFile {
            base_uri: URIString::parse("").unwrap().into(),
            next_catalog: vec![],
        }
    }
}

struct CatalogParseHandler<
    Resolver: EntityResolver = DefaultSAXHandler,
    Reporter: ErrorHandler = DefaultSAXHandler,
> {
    entry_file: CatalogEntryFile,
    // (namespace name, local name)
    name_stack: Vec<(Box<str>, Box<str>)>,
    base_uri_stack: Vec<(Arc<URIStr>, usize)>,
    prefer_mode_stack: Vec<(PreferMode, usize)>,
    ignored_depth: usize,
    resource_failure: bool,
    entity_resolver: Option<Resolver>,
    error_handler: Option<Reporter>,
}

impl<Resolver: EntityResolver, Reporter: ErrorHandler> CatalogParseHandler<Resolver, Reporter> {
    fn with_handler(entity_resolver: Option<Resolver>, error_handler: Option<Reporter>) -> Self {
        Self {
            entry_file: CatalogEntryFile::default(),
            name_stack: vec![],
            base_uri_stack: vec![],
            prefer_mode_stack: vec![(PreferMode::Public, 0)],
            ignored_depth: 0,
            resource_failure: false,
            entity_resolver,
            error_handler,
        }
    }

    fn prefer(&self) -> PreferMode {
        self.prefer_mode_stack
            .last()
            .map(|ret| ret.0)
            .unwrap_or_default()
    }

    fn push_base_uri(&mut self, reference: &URIStr, depth: usize) {
        let base = self.base_uri_stack.last().unwrap().0.resolve(reference);
        self.base_uri_stack.push((base.into(), depth));
    }

    fn resolve_uri(&self, reference: Option<URIString>) -> URIString {
        let reference = reference.unwrap_or_else(|| URIString::parse("").unwrap());
        if reference.is_absolute() {
            // remove fragment
            reference.resolve(&URIString::parse("").unwrap())
        } else {
            self.base_uri_stack.last().unwrap().0.resolve(&reference)
        }
    }
}

impl SAXHandler for CatalogParseHandler {
    fn set_document_locator(&mut self, locator: Arc<Locator>) {
        self.entry_file.base_uri = locator.system_id();
        self.base_uri_stack
            .push((self.entry_file.base_uri.clone(), 0));
    }

    fn start_element(
        &mut self,
        uri: Option<&str>,
        local_name: Option<&str>,
        _qname: &str,
        atts: &Attributes,
    ) {
        if self.name_stack.is_empty() {
            self.resource_failure =
                local_name != Some("catalog") || uri != Some(XML_CATALOG_NAMESPACE);
        }

        if self.resource_failure {
            return;
        }

        self.name_stack.push((
            uri.unwrap_or_default().into(),
            local_name.unwrap_or_default().into(),
        ));
        if self.ignored_depth > 0 || uri != Some(XML_CATALOG_NAMESPACE) {
            self.ignored_depth += 1;
            return;
        }

        macro_rules! check_parent {
            ( $( $parent:literal ),* ) => {
                let parent = self.name_stack.len() - 2;
                if XML_CATALOG_NAMESPACE != self.name_stack[parent].0.as_ref()
                    || ![$( $parent ),*].contains(&self.name_stack[parent].1.as_ref())
                {
                    self.ignored_depth += 1;
                    return;
                }
            };
        }

        match local_name {
            Some("catalog") => {
                if self.name_stack.len() != 1 {
                    self.ignored_depth += 1;
                    return;
                }
                if let Some(base_uri) = atts.get_value_by_qname("xml:base") {
                    let base_uri = URIString::parse(base_uri).unwrap();
                    self.push_base_uri(&base_uri, self.name_stack.len());
                }
                match atts.get_value_by_qname("prefer") {
                    Some("public") => self
                        .prefer_mode_stack
                        .push((PreferMode::Public, self.name_stack.len())),
                    Some("system") => self
                        .prefer_mode_stack
                        .push((PreferMode::System, self.name_stack.len())),
                    _ => {}
                }
            }
            Some("group") => {
                check_parent!("catalog");
                if let Some(base_uri) = atts.get_value_by_qname("xml:base") {
                    let base_uri = URIString::parse(base_uri).unwrap();
                    self.push_base_uri(&base_uri, self.name_stack.len());
                }
                match atts.get_value_by_qname("prefer") {
                    Some("public") => self
                        .prefer_mode_stack
                        .push((PreferMode::Public, self.name_stack.len())),
                    Some("system") => self
                        .prefer_mode_stack
                        .push((PreferMode::System, self.name_stack.len())),
                    _ => {}
                }
            }
            Some("delegatePublic") => {
                check_parent!("catalog", "group");
                let prefer = self.prefer();
                if prefer == PreferMode::System {
                    self.ignored_depth += 1;
                    return;
                }
                let Some(public_id) = atts
                    .get_value_by_qname("publicIdStartString")
                    .filter(|public_id| validate_public_id(public_id).is_ok())
                else {
                    self.ignored_depth += 1;
                    return;
                };
                let Some(catalog) = atts.get_value_by_qname("catalog") else {
                    self.ignored_depth += 1;
                    return;
                };
            }
            Some("delegateSystem") => {
                check_parent!("catalog", "group");
                let Some(system_id) = atts.get_value_by_qname("systemIdStartString") else {
                    self.ignored_depth += 1;
                    return;
                };
                let Some(catalog) = atts.get_value_by_qname("catalog") else {
                    self.ignored_depth += 1;
                    return;
                };
            }
            Some("delegateURI") => {
                check_parent!("catalog", "group");
                let Some(uri) = atts.get_value_by_qname("uriStartString") else {
                    self.ignored_depth += 1;
                    return;
                };
                let Some(catalog) = atts.get_value_by_qname("catalog") else {
                    self.ignored_depth += 1;
                    return;
                };
            }
            Some("public") => {
                check_parent!("catalog", "group");
                let prefer = self.prefer();
                if prefer == PreferMode::System {
                    self.ignored_depth += 1;
                    return;
                }
                let Some(public_id) = atts
                    .get_value_by_qname("publicId")
                    .filter(|public_id| validate_public_id(public_id).is_ok())
                else {
                    self.ignored_depth += 1;
                    return;
                };
            }
            Some("rewriteSystem") => {
                check_parent!("catalog", "group");
                let Some(system_id) = atts.get_value_by_qname("systemIdStartString") else {
                    self.ignored_depth += 1;
                    return;
                };
                let Some(rewrite_prefix) = atts.get_value_by_qname("rewritePrefix") else {
                    self.ignored_depth += 1;
                    return;
                };
            }
            Some("rewriteURI") => {
                check_parent!("catalog", "group");
                let Some(uri) = atts.get_value_by_qname("uriStartString") else {
                    self.ignored_depth += 1;
                    return;
                };
                let Some(rewrite_prefix) = atts.get_value_by_qname("rewritePrefix") else {
                    self.ignored_depth += 1;
                    return;
                };
            }
            Some("system") => {
                check_parent!("catalog", "group");
                let Some(system_id) = atts.get_value_by_qname("systemId") else {
                    self.ignored_depth += 1;
                    return;
                };
                let Some(uri) = atts.get_value_by_qname("uri") else {
                    self.ignored_depth += 1;
                    return;
                };
            }
            Some("systemSuffix") => {
                check_parent!("catalog", "group");
                let Some(suffix) = atts.get_value_by_qname("systemIdSuffix") else {
                    self.ignored_depth += 1;
                    return;
                };
                let Some(uri) = atts.get_value_by_qname("uri") else {
                    self.ignored_depth += 1;
                    return;
                };
            }
            Some("uri") => {
                check_parent!("catalog", "group");
                let Some(name) = atts.get_value_by_qname("name") else {
                    self.ignored_depth += 1;
                    return;
                };
                let Some(uri) = atts.get_value_by_qname("uri") else {
                    self.ignored_depth += 1;
                    return;
                };
            }
            Some("uriSuffix") => {
                check_parent!("catalog", "group");
                let Some(suffix) = atts.get_value_by_qname("uriSuffix") else {
                    self.ignored_depth += 1;
                    return;
                };
                let Some(uri) = atts.get_value_by_qname("uri") else {
                    self.ignored_depth += 1;
                    return;
                };
            }
            Some("nextCatalog") => {
                check_parent!("catalog", "group");
                let Some(catalog) = atts
                    .get_value_by_qname("catalog")
                    .and_then(|catalog| URIString::parse(catalog).ok())
                else {
                    self.ignored_depth += 1;
                    return;
                };
                let absolute = self.resolve_uri(Some(catalog));
                if self
                    .entry_file
                    .next_catalog
                    .iter()
                    .all(|uri| absolute.as_ref() != uri.as_ref())
                {
                    self.entry_file.next_catalog.push(absolute.into());
                }
            }
            _ => self.ignored_depth += 1,
        }
    }

    fn end_element(&mut self, _uri: Option<&str>, _local_name: Option<&str>, _qname: &str) {
        if self.resource_failure {
            return;
        }
        if self.ignored_depth > 0 {
            self.ignored_depth -= 1;
        }
        self.name_stack.pop();
    }
}

impl EntityResolver for CatalogParseHandler {
    fn resolve_entity(
        &mut self,
        name: &str,
        public_id: Option<&str>,
        base_uri: &URIStr,
        system_id: &URIStr,
    ) -> Result<InputSource<'static>, XMLError> {
        if let Some(resolver) = self.entity_resolver.as_mut() {
            resolver.resolve_entity(name, public_id, base_uri, system_id)
        } else {
            DefaultSAXHandler.resolve_entity(name, public_id, base_uri, system_id)
        }
    }

    fn get_external_subset(
        &mut self,
        name: &str,
        base_uri: Option<&URIStr>,
    ) -> Result<InputSource<'static>, XMLError> {
        if let Some(resolver) = self.entity_resolver.as_mut() {
            resolver.get_external_subset(name, base_uri)
        } else {
            DefaultSAXHandler.get_external_subset(name, base_uri)
        }
    }
}

impl ErrorHandler for CatalogParseHandler {
    fn fatal_error(&mut self, error: SAXParseError) {
        if let Some(handler) = self.error_handler.as_mut() {
            handler.fatal_error(error);
        }
    }

    fn error(&mut self, error: SAXParseError) {
        if let Some(handler) = self.error_handler.as_mut() {
            handler.error(error);
        }
    }

    fn warning(&mut self, error: SAXParseError) {
        if let Some(handler) = self.error_handler.as_mut() {
            handler.warning(error);
        }
    }
}

fn normalize_uri(uri: &URIStr) -> Cow<'_, str> {
    fn is_disallowed_character(c: char) -> bool {
        const MASK: u128 = (1 << 0x22)
            | (1 << 0x3C)
            | (1 << 0x3E)
            | (1 << 0x5C)
            | (1 << 0x5E)
            | (1 << 0x60)
            | (1 << 0x7B)
            | (1 << 0x7C)
            | (1 << 0x7D);
        c <= '\x20' || c >= '\x7F' || MASK & (1 << c as u32) != 0
    }

    if let Some(uri) = uri.as_unescaped_str() {
        if !uri.contains(is_disallowed_character) {
            return uri;
        }

        const LUT_BYTES: [u8; 512] = {
            let mut bytes = [0; 512];
            let mut upper = 0;
            while upper < 16 {
                let mut lower = 0;
                let u = if upper > 9 {
                    upper - 10 + b'A'
                } else {
                    upper + b'0'
                };
                while lower < 16 {
                    let l = if lower > 9 {
                        lower - 10 + b'A'
                    } else {
                        lower + b'0'
                    };
                    let b = upper * 16 + lower;
                    bytes[b as usize * 2] = u;
                    bytes[b as usize * 2 + 1] = l;
                    lower += 1;
                }
                upper += 1;
            }
            bytes
        };

        let mut ret = String::new();
        let mut buf = [0u8; 4];
        for c in uri.chars() {
            if is_disallowed_character(c) {
                let s = c.encode_utf8(&mut buf);
                for b in s.bytes() {
                    ret.push('%');
                    ret.push_str(unsafe {
                        // # Safety
                        // `LUT_BYTES` contains only ASCII hexdigit characters,
                        // so UTF-8 validation won't fail.
                        std::str::from_utf8_unchecked(
                            &LUT_BYTES[b as usize * 2..b as usize * 2 + 1],
                        )
                    });
                }
            } else {
                ret.push(c);
            }
        }
        Cow::Owned(ret)
    } else {
        Cow::Borrowed(uri.as_escaped_str())
    }
}

fn validate_public_id(public_id: &str) -> Result<(), XMLError> {
    if public_id
        .chars()
        .all(|c| XMLVersion::default().is_pubid_char(c))
    {
        Ok(())
    } else {
        Err(XMLError::CatalogInvalidPublicID)
    }
}
