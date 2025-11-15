//! Provide resource resolution APIs based on the OASIS standard
//! [XML Catalogs V1.1](https://groups.oasis-open.org/higherlogic/ws/public/download/14810/xml-catalogs.pdf/latest).

use std::{borrow::Cow, collections::HashSet, io::Read, sync::Arc};

use crate::{
    XMLVersion,
    error::XMLError,
    sax::{
        Locator,
        attributes::Attributes,
        error::SAXParseError,
        handler::{DefaultSAXHandler, EntityResolver, ErrorHandler, SAXHandler},
        parser::XMLReaderBuilder,
        source::InputSource,
    },
    uri::{URIStr, URIString},
};

const PUBLICID_URN_NAMESPACE: &str = "urn:publicid:";
pub const XML_CATALOG_NAMESPACE: &str = "urn:oasis:names:tc:entity:xmlns:xml:catalog";
pub const XML_CATALOG_PUBLICID: &str = "-//OASIS//DTD XML Catalogs V1.1//EN";

/// # Reference
/// [4.1.1 The `prefer` attribute](https://groups.oasis-open.org/higherlogic/ws/public/download/14810/xml-catalogs.pdf/latest)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]
pub enum PreferMode {
    /// Represent that `prefer="public"` is specified on `catalog` or `group`
    #[default]
    Public,
    /// Represent that `prefer="system"` is specified on `catalog` or `group`
    System,
}

/// List of catalog entry files to walk through.
#[derive(Default)]
pub struct Catalog {
    entry_files: Vec<CatalogEntryFile>,
    // 1: catalog index for entry_files
    // 2: next index for entry_list or usize::MAX if the following file does not exist
    entry_list: Vec<(usize, usize)>,
    last: usize,
}

impl Catalog {
    /// Try to retrieve the URI of the resource indicated by the external identifier,
    /// which is a pair consisting of `public_id` and `system_id`.
    ///
    /// `prefer_mode` specifies the root-level prefer mode.  \
    /// In most cases, it is recommended to use [`PreferMode::Public`] or [`PreferMode::default()`].
    ///
    /// If no matching entry is found, return [`None`].
    pub fn resolve_external_id(
        &mut self,
        public_id: Option<&str>,
        system_id: Option<&URIStr>,
        prefer_mode: PreferMode,
    ) -> Option<Arc<URIStr>> {
        if self.entry_files.is_empty() {
            return None;
        }
        if public_id.is_none() && system_id.is_none() {
            return None;
        }
        let mut seen_uris = HashSet::new();
        let public_id = public_id
            .map(normalize_public_id)
            .filter(|id| validate_public_id(id).is_ok());
        let system_id = system_id.map(normalize_uri);
        if let Some(urn) = system_id
            .as_deref()
            .filter(|uri| uri.starts_with(PUBLICID_URN_NAMESPACE))
        {
            let normalized = normalize_public_id(urn);
            if public_id
                .as_deref()
                .is_some_and(|public_id| public_id != normalized)
                || validate_public_id(&normalized).is_err()
            {
                self.do_resolve_external_id(public_id.as_deref(), None, prefer_mode, &mut seen_uris)
            } else {
                self.do_resolve_external_id(Some(&normalized), None, prefer_mode, &mut seen_uris)
            }
        } else {
            self.do_resolve_external_id(
                public_id.as_deref(),
                system_id.as_deref(),
                prefer_mode,
                &mut seen_uris,
            )
        }
    }

    fn do_resolve_external_id(
        &mut self,
        public_id: Option<&str>,
        system_id: Option<&str>,
        prefer_mode: PreferMode,
        seen_uris: &mut HashSet<Arc<URIStr>>,
    ) -> Option<Arc<URIStr>> {
        let mut now = 0;
        while now < usize::MAX {
            let (catalog, next) = self.entry_list[now];
            if !seen_uris.insert(self.entry_files[catalog].base_uri.clone()) {
                now = next;
                continue;
            }

            let mut parser = None;
            let catalog = &self.entry_files[catalog];
            if let Some(entries) = catalog.resolve_external_id(public_id, system_id, prefer_mode) {
                let mut delegate = vec![];
                let mut delegate_system = false;
                for entry in entries {
                    match entry.as_ref() {
                        CatalogEntry::System { uri, .. }
                        | CatalogEntry::Public { uri, .. }
                        | CatalogEntry::SystemSuffix { uri, .. } => {
                            return Some(uri.clone());
                        }
                        CatalogEntry::RewriteSystem { from, to } => {
                            let system_id = system_id.unwrap();
                            let stripped = system_id.strip_prefix(from.as_ref()).unwrap();
                            let uri = to.as_ref().to_owned() + stripped;
                            return Some(URIString::parse(uri).unwrap().into());
                        }
                        entry @ (CatalogEntry::DelegateSystem { catalog, .. }
                        | CatalogEntry::DelegatePublic { catalog, .. }) => {
                            delegate_system = matches!(entry, CatalogEntry::DelegateSystem { .. });
                            if seen_uris.contains(catalog.as_ref()) {
                                continue;
                            }
                            let parser = parser.get_or_insert_with(|| {
                                XMLReaderBuilder::new()
                                    .set_handler(CatalogParseHandler::new())
                                    .build()
                            });
                            parser.reset().ok();
                            if parser.parse_uri(catalog, None).is_ok()
                                && !parser.handler.resource_failure
                            {
                                delegate.push(std::mem::take(&mut parser.handler).entry_file);
                            }
                        }
                        CatalogEntry::URI { .. }
                        | CatalogEntry::RewriteURI { .. }
                        | CatalogEntry::URISuffix { .. }
                        | CatalogEntry::DelegateURI { .. } => unreachable!(),
                    }
                }

                if !delegate.is_empty() {
                    let len = delegate.len();
                    let mut delegate = Catalog {
                        entry_files: delegate,
                        entry_list: (0..len).map(|i| (i, i + 1)).collect::<_>(),
                        last: len - 1,
                    };
                    delegate.entry_list[len - 1].1 = usize::MAX;
                    return if delegate_system {
                        delegate.do_resolve_external_id(None, system_id, prefer_mode, seen_uris)
                    } else {
                        delegate.do_resolve_external_id(public_id, None, prefer_mode, seen_uris)
                    };
                }
            }

            let parser = parser.get_or_insert_with(|| {
                XMLReaderBuilder::new()
                    .set_handler(CatalogParseHandler::new())
                    .build()
            });
            let mut catalogs = vec![];
            let mut last = now;
            for uri in catalog.next_catalogs() {
                if let Some(pos) = self
                    .entry_files
                    .iter()
                    .chain(catalogs.iter())
                    .position(|file| file.base_uri.as_ref() == uri)
                {
                    self.entry_list.push((pos, self.entry_list[last].1));
                    self.entry_list[last].1 = self.entry_list.len() - 1;
                    last = self.entry_list.len() - 1;
                } else {
                    parser.reset().ok();
                    if parser.parse_uri(uri, None).is_ok() && !parser.handler.resource_failure {
                        let file = std::mem::take(&mut parser.handler).entry_file;
                        self.entry_list.push((
                            self.entry_files.len() + catalogs.len(),
                            self.entry_list[last].1,
                        ));
                        catalogs.push(file);
                        self.entry_list[last].1 = self.entry_list.len() - 1;
                        last = self.entry_list.len() - 1;
                    }
                }
            }
            self.entry_files.extend(catalogs);
            if self.entry_list[last].1 == usize::MAX {
                self.last = last;
            }

            now = next;
        }
        None
    }

    /// Try to retrieve the alternative URI of `uri`.
    ///
    /// If no matching entry is found, return [`None`].
    pub fn resolve_uri(&mut self, uri: impl AsRef<URIStr>) -> Option<Arc<URIStr>> {
        if self.entry_files.is_empty() {
            return None;
        }
        let mut seen_uris = HashSet::new();
        let uri = normalize_uri(uri.as_ref());
        if uri.starts_with(PUBLICID_URN_NAMESPACE) {
            let normalized = normalize_public_id(&uri);
            if validate_public_id(&normalized).is_err() {
                return None;
            }
            self.do_resolve_external_id(Some(&normalized), None, PreferMode::Public, &mut seen_uris)
        } else {
            self.do_resolve_uri(&uri, &mut seen_uris)
        }
    }

    fn do_resolve_uri(
        &mut self,
        uri: &str,
        seen_uris: &mut HashSet<Arc<URIStr>>,
    ) -> Option<Arc<URIStr>> {
        let mut now = 0;
        while now < usize::MAX {
            let (catalog, next) = self.entry_list[now];
            if !seen_uris.insert(self.entry_files[catalog].base_uri.clone()) {
                now = next;
                continue;
            }

            let mut parser = None;
            let catalog = &self.entry_files[catalog];
            if let Some(entries) = catalog.resolve_uri(uri) {
                let mut delegate = vec![];
                for entry in entries {
                    match entry.as_ref() {
                        CatalogEntry::URI { uri, .. } | CatalogEntry::URISuffix { uri, .. } => {
                            return Some(uri.clone());
                        }
                        CatalogEntry::RewriteURI { from, to } => {
                            let stripped = uri.strip_prefix(from.as_ref()).unwrap();
                            let uri = to.as_ref().to_owned() + stripped;
                            return Some(URIString::parse(uri).unwrap().into());
                        }
                        CatalogEntry::DelegateURI { catalog, .. } => {
                            if seen_uris.contains(catalog.as_ref()) {
                                continue;
                            }
                            let parser = parser.get_or_insert_with(|| {
                                XMLReaderBuilder::new()
                                    .set_handler(CatalogParseHandler::new())
                                    .build()
                            });
                            parser.reset().ok();
                            if parser.parse_uri(catalog, None).is_ok()
                                && !parser.handler.resource_failure
                            {
                                delegate.push(std::mem::take(&mut parser.handler).entry_file);
                            }
                        }
                        CatalogEntry::System { .. }
                        | CatalogEntry::Public { .. }
                        | CatalogEntry::SystemSuffix { .. }
                        | CatalogEntry::RewriteSystem { .. }
                        | CatalogEntry::DelegateSystem { .. }
                        | CatalogEntry::DelegatePublic { .. } => {
                            unreachable!()
                        }
                    }
                }

                if !delegate.is_empty() {
                    let len = delegate.len();
                    let mut delegate = Catalog {
                        entry_files: delegate,
                        entry_list: (0..len).map(|i| (i, i + 1)).collect::<_>(),
                        last: len - 1,
                    };
                    delegate.entry_list[len - 1].1 = usize::MAX;
                    return delegate.do_resolve_uri(uri, seen_uris);
                }
            }

            let parser = parser.get_or_insert_with(|| {
                XMLReaderBuilder::new()
                    .set_handler(CatalogParseHandler::new())
                    .build()
            });
            let mut catalogs = vec![];
            let mut last = now;
            for uri in catalog.next_catalogs() {
                if let Some(pos) = self
                    .entry_files
                    .iter()
                    .chain(catalogs.iter())
                    .position(|file| file.base_uri.as_ref() == uri)
                {
                    self.entry_list.push((pos, self.entry_list[last].1));
                    self.entry_list[last].1 = self.entry_list.len() - 1;
                    last = self.entry_list.len() - 1;
                } else {
                    parser.reset().ok();
                    if parser.parse_uri(uri, None).is_ok() && !parser.handler.resource_failure {
                        let file = std::mem::take(&mut parser.handler).entry_file;
                        self.entry_list.push((
                            self.entry_files.len() + catalogs.len(),
                            self.entry_list[last].1,
                        ));
                        catalogs.push(file);
                        self.entry_list[last].1 = self.entry_list.len() - 1;
                        last = self.entry_list.len() - 1;
                    }
                }
            }
            self.entry_files.extend(catalogs);
            if self.entry_list[last].1 == usize::MAX {
                self.last = last;
            }

            now = next;
        }
        None
    }

    /// Add the catalog entry file to the end of the list.
    ///
    /// If a catalog entry file with the same base URI as `catalog` has already been inserted,
    /// `catalog` is discarded, and the inserted file behaves as if it were newly inserted to
    /// the end of the list.
    pub fn add(&mut self, catalog: CatalogEntryFile) {
        if self.entry_files.is_empty() {
            self.entry_files.push(catalog);
            self.entry_list.push((0, usize::MAX));
            return;
        }
        if let Some(pos) = self
            .entry_files
            .iter()
            .position(|file| file.base_uri == catalog.base_uri)
        {
            self.entry_list.push((pos, usize::MAX));
        } else {
            self.entry_list.push((self.entry_files.len(), usize::MAX));
            self.entry_files.push(catalog);
        }
        self.entry_list[self.last].1 = self.entry_list.len() - 1;
        self.last = self.entry_list.len() - 1;
    }

    /// Remove all catalog entry files from this list.
    pub fn clear(&mut self) {
        self.entry_files.clear();
        self.entry_list.clear();
        self.last = usize::MAX;
    }
}

/// A single catalog entry file.
///
/// This type itself does not have methods for resolving external identifiers or alternative URIs.
/// To do that, it is necessary to generate a list using [`Catalog::default`] and add files
/// to the list.
pub struct CatalogEntryFile {
    base_uri: Arc<URIStr>,
    entries: CatalogEntryMap,
    next_catalog: Vec<Arc<URIStr>>,
}

impl CatalogEntryFile {
    fn new() -> Self {
        CatalogEntryFile {
            base_uri: URIString::parse("").unwrap().into(),
            entries: CatalogEntryMap::new(),
            next_catalog: vec![],
        }
    }

    /// Parse the catalog entry file using `uri` and `encoding`.
    ///
    /// If a custom [`EntityResolver`] or [`ErrorHandler`] is required,
    /// it can be specified using `entity_resolver` and `error_handler`.
    ///
    /// If the catalog document cannot be parsed for any reason,
    /// or if "Resource Failures" defined in the specification are detected, [`Err`] is returned.
    ///
    /// # Reference
    /// [8. Resource Failures](https://groups.oasis-open.org/higherlogic/ws/public/download/14810/xml-catalogs.pdf/latest)
    pub fn parse_uri<Resolver: EntityResolver, Reporter: ErrorHandler>(
        uri: impl AsRef<URIStr>,
        encoding: Option<&str>,
        entity_resolver: Option<Resolver>,
        error_handler: Option<Reporter>,
    ) -> Result<CatalogEntryFile, XMLError> {
        let handler = CatalogParseHandler::with_handler(entity_resolver, error_handler);
        let mut reader = XMLReaderBuilder::new().set_handler(handler).build();
        reader.parse_uri(uri, encoding)?;
        if reader.handler.resource_failure {
            return Err(XMLError::CatalogResourceFailure);
        }
        Ok(reader.handler.entry_file)
    }

    /// Parse the catalog entry file using `reader`, `encoding` and `uri`.
    ///
    /// If a custom [`EntityResolver`] or [`ErrorHandler`] is required,
    /// it can be specified using `entity_resolver` and `error_handler`.
    ///
    /// If the catalog document cannot be parsed for any reason,
    /// or if "Resource Failures" defined in the specification are detected, [`Err`] is returned.
    ///
    /// # Reference
    /// [8. Resource Failures](https://groups.oasis-open.org/higherlogic/ws/public/download/14810/xml-catalogs.pdf/latest)
    pub fn parse_reader<'a, Resolver: EntityResolver, Reporter: ErrorHandler>(
        reader: impl Read + 'a,
        encoding: Option<&str>,
        uri: impl AsRef<URIStr>,
        entity_resolver: Option<Resolver>,
        error_handler: Option<Reporter>,
    ) -> Result<CatalogEntryFile, XMLError> {
        let handler = CatalogParseHandler::with_handler(entity_resolver, error_handler);
        let mut parser = XMLReaderBuilder::new().set_handler(handler).build();
        parser.parse_reader(reader, encoding, Some(uri.as_ref()))?;
        if parser.handler.resource_failure {
            return Err(XMLError::CatalogResourceFailure);
        }
        Ok(parser.handler.entry_file)
    }

    /// Parse the catalog entry file using `catalog` and `uri`.
    ///
    /// If a custom [`EntityResolver`] or [`ErrorHandler`] is required,
    /// it can be specified using `entity_resolver` and `error_handler`.
    ///
    /// If the catalog document cannot be parsed for any reason,
    /// or if "Resource Failures" defined in the specification are detected, [`Err`] is returned.
    ///
    /// # Reference
    /// [8. Resource Failures](https://groups.oasis-open.org/higherlogic/ws/public/download/14810/xml-catalogs.pdf/latest)
    pub fn parse_str<Resolver: EntityResolver, Reporter: ErrorHandler>(
        catalog: &str,
        uri: impl AsRef<URIStr>,
        entity_resolver: Option<Resolver>,
        error_handler: Option<Reporter>,
    ) -> Result<CatalogEntryFile, XMLError> {
        let handler = CatalogParseHandler::with_handler(entity_resolver, error_handler);
        let mut parser = XMLReaderBuilder::new().set_handler(handler).build();
        parser.parse_str(catalog, Some(uri.as_ref()))?;
        if parser.handler.resource_failure {
            return Err(XMLError::CatalogResourceFailure);
        }
        Ok(parser.handler.entry_file)
    }

    /// Return an iterator of URIs for catalog files pointed to by `nextCatalog` entries.
    pub fn next_catalogs(&self) -> impl Iterator<Item = &URIStr> + '_ {
        self.next_catalog.iter().map(|uri| uri.as_ref())
    }

    fn resolve_system_id(&self, system_id: &str) -> Option<Vec<Arc<CatalogEntry>>> {
        let mut ret = self.entries.search_system_id(system_id).collect::<Vec<_>>();
        ret.sort_unstable();
        if ret.first().is_some_and(|entry| {
            matches!(
                entry.entry_type(),
                CatalogEntryType::System | CatalogEntryType::RewriteSystem
            )
        }) {
            ret.truncate(1);
            return Some(ret);
        }

        let reversed = system_id.chars().rev().collect::<String>();
        let mut rev = self
            .entries
            .search_system_suffix(&reversed)
            .collect::<Vec<_>>();
        rev.sort_unstable();
        if !rev.is_empty() {
            rev.truncate(1);
            return Some(rev);
        }
        (!ret.is_empty()).then_some(ret)
    }

    fn resolve_public_id(
        &self,
        public_id: &str,
        prefer_mode: PreferMode,
    ) -> Option<Vec<Arc<CatalogEntry>>> {
        let mut ret = self
            .entries
            .search_public_id(public_id, prefer_mode)
            .collect::<Vec<_>>();
        ret.sort_unstable();
        if ret
            .first()
            .is_some_and(|entry| matches!(entry.entry_type(), CatalogEntryType::Public))
        {
            ret.truncate(1);
            return Some(ret);
        }
        (!ret.is_empty()).then_some(ret)
    }

    fn resolve_external_id(
        &self,
        public_id: Option<&str>,
        system_id: Option<&str>,
        prefer_mode: PreferMode,
    ) -> Option<Vec<Arc<CatalogEntry>>> {
        match (public_id, system_id) {
            (Some(public_id), Some(system_id)) => self
                .resolve_system_id(system_id)
                .or_else(|| self.resolve_public_id(public_id, prefer_mode)),
            (Some(public_id), None) => self.resolve_public_id(public_id, prefer_mode),
            (None, Some(system_id)) => self.resolve_system_id(system_id),
            (None, None) => None,
        }
    }

    fn resolve_uri(&self, uri: &str) -> Option<Vec<Arc<CatalogEntry>>> {
        let mut ret = self.entries.search_uri(uri).collect::<Vec<_>>();
        ret.sort_unstable();
        if ret.first().is_some_and(|entry| {
            matches!(
                entry.entry_type(),
                CatalogEntryType::URI | CatalogEntryType::RewriteURI
            )
        }) {
            ret.truncate(1);
            return Some(ret);
        }

        let reversed = uri.chars().rev().collect::<String>();
        let mut rev = self
            .entries
            .search_uri_suffix(&reversed)
            .collect::<Vec<_>>();
        rev.sort_unstable();
        if !rev.is_empty() {
            rev.truncate(1);
            return Some(rev);
        }
        (!ret.is_empty()).then_some(ret)
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

impl CatalogParseHandler {
    fn new() -> Self {
        Self::with_handler(None, None)
    }
}

impl<Resolver: EntityResolver, Reporter: ErrorHandler> CatalogParseHandler<Resolver, Reporter> {
    fn with_handler(entity_resolver: Option<Resolver>, error_handler: Option<Reporter>) -> Self {
        Self {
            entry_file: CatalogEntryFile::new(),
            name_stack: vec![],
            base_uri_stack: vec![],
            prefer_mode_stack: vec![],
            ignored_depth: 0,
            resource_failure: false,
            entity_resolver,
            error_handler,
        }
    }

    fn prefer(&self) -> Option<PreferMode> {
        self.prefer_mode_stack.last().map(|ret| ret.0)
    }

    fn push_base_uri(&mut self, reference: &URIStr, depth: usize) {
        let base = self.base_uri_stack.last().unwrap().0.resolve(reference);
        self.base_uri_stack.push((base.into(), depth));
    }

    fn pop_stack(&mut self) {
        let depth = self.name_stack.len();
        if self.prefer_mode_stack.last().is_some_and(|v| v.1 == depth) {
            self.prefer_mode_stack.pop();
        }
        if self.base_uri_stack.last().is_some_and(|v| v.1 == depth) {
            self.base_uri_stack.pop();
        }
        self.name_stack.pop();
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

impl Default for CatalogParseHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl<Resolver: EntityResolver, Reporter: ErrorHandler> SAXHandler
    for CatalogParseHandler<Resolver, Reporter>
{
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
                if prefer == Some(PreferMode::System) {
                    self.ignored_depth += 1;
                    return;
                }
                let Some(public_id) = atts
                    .get_value_by_qname("publicIdStartString")
                    .map(|public_id| normalize_public_id(public_id))
                    .filter(|public_id| validate_public_id(public_id).is_ok())
                else {
                    self.ignored_depth += 1;
                    return;
                };
                let Some(catalog) = atts
                    .get_value_by_qname("catalog")
                    .and_then(|uri| URIString::parse(uri).ok())
                    .map(|uri| self.resolve_uri(Some(uri)).into())
                else {
                    self.ignored_depth += 1;
                    return;
                };
                self.entry_file.entries.insert(
                    &public_id,
                    CatalogEntry::DelegatePublic {
                        prefix: public_id.as_ref().into(),
                        catalog,
                        prefer,
                    },
                );
            }
            Some("delegateSystem") => {
                check_parent!("catalog", "group");
                let Some(system_id) = atts
                    .get_value_by_qname("systemIdStartString")
                    .and_then(|uri| URIString::parse(uri).ok())
                else {
                    self.ignored_depth += 1;
                    return;
                };
                let Some(catalog) = atts
                    .get_value_by_qname("catalog")
                    .and_then(|uri| URIString::parse(uri).ok())
                    .map(|uri| self.resolve_uri(Some(uri)).into())
                else {
                    self.ignored_depth += 1;
                    return;
                };
                let system_id = normalize_uri(&system_id);
                self.entry_file.entries.insert(
                    &system_id,
                    CatalogEntry::DelegateSystem {
                        prefix: system_id.as_ref().into(),
                        catalog,
                    },
                );
            }
            Some("delegateURI") => {
                check_parent!("catalog", "group");
                let Some(uri) = atts
                    .get_value_by_qname("uriStartString")
                    .and_then(|uri| URIString::parse(uri).ok())
                else {
                    self.ignored_depth += 1;
                    return;
                };
                let Some(catalog) = atts
                    .get_value_by_qname("catalog")
                    .and_then(|uri| URIString::parse(uri).ok())
                    .map(|uri| self.resolve_uri(Some(uri)).into())
                else {
                    self.ignored_depth += 1;
                    return;
                };
                let uri = normalize_uri(&uri);
                self.entry_file.entries.insert(
                    &uri,
                    CatalogEntry::DelegateURI {
                        prefix: uri.as_ref().into(),
                        catalog,
                    },
                );
            }
            Some("public") => {
                check_parent!("catalog", "group");
                let prefer = self.prefer();
                if prefer == Some(PreferMode::System) {
                    self.ignored_depth += 1;
                    return;
                }
                let Some(public_id) = atts
                    .get_value_by_qname("publicId")
                    .map(|public_id| normalize_public_id(public_id))
                    .filter(|public_id| validate_public_id(public_id).is_ok())
                else {
                    self.ignored_depth += 1;
                    return;
                };
                let Some(uri) = atts
                    .get_value_by_qname("uri")
                    .and_then(|uri| URIString::parse(uri).ok())
                    .map(|uri| self.resolve_uri(Some(uri)).into())
                else {
                    self.ignored_depth += 1;
                    return;
                };
                self.entry_file
                    .entries
                    .insert(&public_id, CatalogEntry::Public { uri, prefer });
            }
            Some("rewriteSystem") => {
                check_parent!("catalog", "group");
                let Some(system_id) = atts
                    .get_value_by_qname("systemIdStartString")
                    .and_then(|uri| URIString::parse(uri).ok())
                else {
                    self.ignored_depth += 1;
                    return;
                };
                let Some(rewrite_prefix) = atts
                    .get_value_by_qname("rewritePrefix")
                    .and_then(|uri| URIString::parse(uri).ok())
                    .map(|uri| self.resolve_uri(Some(uri)))
                else {
                    self.ignored_depth += 1;
                    return;
                };
                let system_id = normalize_uri(&system_id);
                self.entry_file.entries.insert(
                    &system_id,
                    CatalogEntry::RewriteSystem {
                        from: system_id.as_ref().into(),
                        to: rewrite_prefix
                            .as_unescaped_str()
                            .as_deref()
                            .unwrap_or(rewrite_prefix.as_escaped_str())
                            .into(),
                    },
                );
            }
            Some("rewriteURI") => {
                check_parent!("catalog", "group");
                let Some(uri) = atts
                    .get_value_by_qname("uriStartString")
                    .and_then(|uri| URIString::parse(uri).ok())
                else {
                    self.ignored_depth += 1;
                    return;
                };
                let Some(rewrite_prefix) = atts
                    .get_value_by_qname("rewritePrefix")
                    .and_then(|uri| URIString::parse(uri).ok())
                    .map(|uri| self.resolve_uri(Some(uri)))
                else {
                    self.ignored_depth += 1;
                    return;
                };
                let uri = normalize_uri(&uri);
                self.entry_file.entries.insert(
                    &uri,
                    CatalogEntry::RewriteURI {
                        from: uri.as_ref().into(),
                        to: rewrite_prefix
                            .as_unescaped_str()
                            .as_deref()
                            .unwrap_or(rewrite_prefix.as_escaped_str())
                            .into(),
                    },
                );
            }
            Some("system") => {
                check_parent!("catalog", "group");
                let Some(system_id) = atts
                    .get_value_by_qname("systemId")
                    .and_then(|uri| URIString::parse(uri).ok())
                else {
                    self.ignored_depth += 1;
                    return;
                };
                let Some(uri) = atts
                    .get_value_by_qname("uri")
                    .and_then(|uri| URIString::parse(uri).ok())
                    .map(|uri| self.resolve_uri(Some(uri)).into())
                else {
                    self.ignored_depth += 1;
                    return;
                };
                let system_id = normalize_uri(&system_id);
                self.entry_file.entries.insert(
                    &system_id,
                    CatalogEntry::System {
                        system_id: system_id.as_ref().into(),
                        uri,
                    },
                );
            }
            Some("systemSuffix") => {
                check_parent!("catalog", "group");
                let Some(suffix) = atts
                    .get_value_by_qname("systemIdSuffix")
                    .and_then(|uri| URIString::parse(uri).ok())
                else {
                    self.ignored_depth += 1;
                    return;
                };
                let Some(uri) = atts
                    .get_value_by_qname("uri")
                    .and_then(|uri| URIString::parse(uri).ok())
                    .map(|uri| self.resolve_uri(Some(uri)).into())
                else {
                    self.ignored_depth += 1;
                    return;
                };
                let suffix = normalize_uri(&suffix);
                let rev_suffix = suffix.chars().rev().collect::<String>();
                self.entry_file.entries.insert(
                    &rev_suffix,
                    CatalogEntry::SystemSuffix {
                        suffix: suffix.into(),
                        uri,
                    },
                );
            }
            Some("uri") => {
                check_parent!("catalog", "group");
                let Some(name) = atts
                    .get_value_by_qname("name")
                    .and_then(|uri| URIString::parse(uri).ok())
                else {
                    self.ignored_depth += 1;
                    return;
                };
                let Some(uri) = atts
                    .get_value_by_qname("uri")
                    .and_then(|uri| URIString::parse(uri).ok())
                    .map(|uri| self.resolve_uri(Some(uri)).into())
                else {
                    self.ignored_depth += 1;
                    return;
                };
                let name = normalize_uri(&name);
                self.entry_file.entries.insert(
                    &name,
                    CatalogEntry::URI {
                        name: name.as_ref().into(),
                        uri,
                    },
                );
            }
            Some("uriSuffix") => {
                check_parent!("catalog", "group");
                let Some(suffix) = atts
                    .get_value_by_qname("uriSuffix")
                    .and_then(|uri| URIString::parse(uri).ok())
                else {
                    self.ignored_depth += 1;
                    return;
                };
                let Some(uri) = atts
                    .get_value_by_qname("uri")
                    .and_then(|uri| URIString::parse(uri).ok())
                    .map(|uri| self.resolve_uri(Some(uri)).into())
                else {
                    self.ignored_depth += 1;
                    return;
                };
                let suffix = normalize_uri(&suffix);
                let rev_suffix = suffix.chars().rev().collect::<String>();
                self.entry_file.entries.insert(
                    &rev_suffix,
                    CatalogEntry::URISuffix {
                        suffix: suffix.into(),
                        uri,
                    },
                );
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
        self.pop_stack();
    }
}

impl<Resolver: EntityResolver, Reporter: ErrorHandler> EntityResolver
    for CatalogParseHandler<Resolver, Reporter>
{
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
        (if let Some(resolver) = self.entity_resolver.as_mut() {
            resolver.get_external_subset(name, base_uri)
        } else {
            DefaultSAXHandler.get_external_subset(name, base_uri)
        })
        .or_else(|err| {
            if name == "catalog" {
                const CDATALOG_DTD: &str = include_str!("../resources/catalog-compact.dtd");
                let mut dtd = InputSource::from_content(CDATALOG_DTD);
                dtd.set_system_id(
                    URIString::parse(
                        "http://www.oasis-open.org/committees/entity/release/1.1/catalog.dtd",
                    )
                    .unwrap(),
                );
                dtd.set_public_id(XML_CATALOG_PUBLICID);
                Ok(dtd)
            } else {
                Err(err)
            }
        })
    }
}

impl<Resolver: EntityResolver, Reporter: ErrorHandler> ErrorHandler
    for CatalogParseHandler<Resolver, Reporter>
{
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

fn normalize_public_id(public_id: &str) -> Cow<'_, str> {
    const VERSION: XMLVersion = XMLVersion::XML10;

    let mut public_id = Cow::Borrowed(public_id.trim_matches(|c| VERSION.is_whitespace(c)));
    if let Some(mut unwrapped) = public_id.strip_prefix(PUBLICID_URN_NAMESPACE) {
        let mut buf = String::new();
        loop {
            match unwrapped.as_bytes() {
                [] => break,
                [b'+', ..] => {
                    buf.push('\x20');
                    unwrapped = &unwrapped[1..];
                }
                [b':', ..] => {
                    buf.push_str("//");
                    unwrapped = &unwrapped[1..];
                }
                [b';', ..] => {
                    buf.push_str("::");
                    unwrapped = &unwrapped[1..];
                }
                [b'%', b'2', b'B', ..] => {
                    buf.push('+');
                    unwrapped = &unwrapped[3..];
                }
                [b'%', b'3', b'A', ..] => {
                    buf.push(':');
                    unwrapped = &unwrapped[3..];
                }
                [b'%', b'2', b'F', ..] => {
                    buf.push('/');
                    unwrapped = &unwrapped[3..];
                }
                [b'%', b'3', b'B', ..] => {
                    buf.push(';');
                    unwrapped = &unwrapped[3..];
                }
                [b'%', b'2', b'7', ..] => {
                    buf.push('\'');
                    unwrapped = &unwrapped[3..];
                }
                [b'%', b'3', b'F', ..] => {
                    buf.push('?');
                    unwrapped = &unwrapped[3..];
                }
                [b'%', b'2', b'3', ..] => {
                    buf.push('#');
                    unwrapped = &unwrapped[3..];
                }
                [b'%', b'2', b'5', ..] => {
                    buf.push('%');
                    unwrapped = &unwrapped[3..];
                }
                [_, ..] => buf.push(unwrapped.chars().next().unwrap()),
            }
        }

        public_id = Cow::Owned(buf);
    }
    if public_id
        .bytes()
        .any(|b| VERSION.is_whitespace(b) && b != 0x20)
        || public_id.contains("\x20\x20")
    {
        let mut buf = String::new();
        for chunk in public_id
            .split(|c| VERSION.is_whitespace(c))
            .filter(|s| !s.is_empty())
        {
            if !buf.is_empty() {
                buf.push('\x20');
            }
            buf.push_str(chunk);
        }
        Cow::Owned(buf)
    } else {
        public_id
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

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum CatalogEntryType {
    System,
    RewriteSystem,
    SystemSuffix,
    DelegateSystem,
    Public,
    DelegatePublic,
    URI,
    RewriteURI,
    URISuffix,
    DelegateURI,
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, PartialEq, Eq)]
enum CatalogEntry {
    System {
        system_id: Box<str>,
        uri: Arc<URIStr>,
    },
    RewriteSystem {
        from: Box<str>,
        to: Arc<str>,
    },
    SystemSuffix {
        suffix: Box<str>,
        uri: Arc<URIStr>,
    },
    DelegateSystem {
        prefix: Box<str>,
        catalog: Arc<URIStr>,
    },
    Public {
        uri: Arc<URIStr>,
        prefer: Option<PreferMode>,
    },
    DelegatePublic {
        prefix: Box<str>,
        catalog: Arc<URIStr>,
        prefer: Option<PreferMode>,
    },
    URI {
        name: Box<str>,
        uri: Arc<URIStr>,
    },
    RewriteURI {
        from: Box<str>,
        to: Arc<str>,
    },
    URISuffix {
        suffix: Box<str>,
        uri: Arc<URIStr>,
    },
    DelegateURI {
        prefix: Box<str>,
        catalog: Arc<URIStr>,
    },
}

impl CatalogEntry {
    const fn entry_type(&self) -> CatalogEntryType {
        match self {
            CatalogEntry::System { .. } => CatalogEntryType::System,
            CatalogEntry::RewriteSystem { .. } => CatalogEntryType::RewriteSystem,
            CatalogEntry::SystemSuffix { .. } => CatalogEntryType::SystemSuffix,
            CatalogEntry::DelegateSystem { .. } => CatalogEntryType::DelegateSystem,
            CatalogEntry::Public { .. } => CatalogEntryType::Public,
            CatalogEntry::DelegatePublic { .. } => CatalogEntryType::DelegatePublic,
            CatalogEntry::URI { .. } => CatalogEntryType::URI,
            CatalogEntry::RewriteURI { .. } => CatalogEntryType::RewriteURI,
            CatalogEntry::URISuffix { .. } => CatalogEntryType::URISuffix,
            CatalogEntry::DelegateURI { .. } => CatalogEntryType::DelegateURI,
        }
    }
}

impl PartialOrd for CatalogEntry {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for CatalogEntry {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use CatalogEntry::*;

        match (self, other) {
            (System { system_id: l, .. }, System { system_id: r, .. }) => l.cmp(r),
            (RewriteSystem { from: l, .. }, RewriteSystem { from: r, .. }) => r.cmp(l),
            (SystemSuffix { suffix: l, .. }, SystemSuffix { suffix: r, .. }) => {
                r.chars().rev().cmp(l.chars().rev())
            }
            (DelegateSystem { prefix: l, .. }, DelegateSystem { prefix: r, .. }) => r.cmp(l),
            (Public { prefer: lp, .. }, Public { prefer: rp, .. }) => lp.cmp(rp),
            (
                DelegatePublic {
                    prefix: lpre,
                    prefer: lp,
                    ..
                },
                DelegatePublic {
                    prefix: rpre,
                    prefer: rp,
                    ..
                },
            ) => match rpre.cmp(lpre) {
                std::cmp::Ordering::Equal => lp.cmp(rp),
                other => other,
            },
            (URI { name: l, .. }, URI { name: r, .. }) => l.cmp(r),
            (RewriteURI { from: l, .. }, RewriteURI { from: r, .. }) => r.cmp(l),
            (URISuffix { suffix: l, .. }, URISuffix { suffix: r, .. }) => {
                r.chars().rev().cmp(l.chars().rev())
            }
            (DelegateURI { prefix: l, .. }, DelegateURI { prefix: r, .. }) => r.cmp(l),
            _ => self.entry_type().cmp(&other.entry_type()),
        }
    }
}

struct TrieNode {
    fragment: String,
    next: Vec<(u8, usize)>,
    entries: Vec<Arc<CatalogEntry>>,
}

impl TrieNode {
    fn push_entry(&mut self, entry: CatalogEntry) {
        match entry {
            CatalogEntry::Public {
                uri,
                prefer: None,
            } => {
                if self.entries.iter().all(|entry| !matches!(entry.as_ref(), CatalogEntry::Public { .. })) {
                    self
                        .entries
                        .push(Arc::new(CatalogEntry::Public {
                            uri,
                            prefer: None,
                        }));
                }
            }
            CatalogEntry::DelegatePublic {
                prefix,
                catalog,
                prefer: None,
            } => {
                if self.entries.iter().all(|entry| !matches!(entry.as_ref(), CatalogEntry::DelegatePublic { prefix: pre, .. } if *pre == prefix)) {
                    self
                        .entries
                        .push(Arc::new(CatalogEntry::DelegatePublic {
                            prefix,
                            catalog,
                            prefer: None,
                        }));
                }
            }
            entry => {
                let entry = Arc::new(entry);
                if !self.entries.contains(&entry) {
                    self.entries.push(entry);
                }
            }
        }
    }
}

struct CatalogEntryMap {
    trie: Vec<TrieNode>,
}

impl CatalogEntryMap {
    fn new() -> Self {
        Self {
            trie: vec![TrieNode {
                fragment: "".into(),
                next: vec![],
                entries: vec![],
            }],
        }
    }

    fn insert(&mut self, mut id: &str, entry: CatalogEntry) {
        let mut node_id = 0;
        while !id.is_empty() {
            let node = &self.trie[node_id];
            let pre = id
                .bytes()
                .zip(node.fragment.bytes())
                .take_while(|(a, b)| a == b)
                .count();

            if pre == id.len() && pre == node.fragment.len() {
                self.trie[node_id].push_entry(entry);
                break;
            }

            if pre == node.fragment.len() {
                let b = id.as_bytes()[pre];
                match node.next.binary_search_by_key(&b, |n| n.0) {
                    Ok(pos) => node_id = node.next[pos].1,
                    Err(pos) => {
                        let new = TrieNode {
                            fragment: id[pre..].to_owned(),
                            next: vec![],
                            entries: vec![Arc::new(entry)],
                        };
                        let next = self.trie.len();
                        self.trie[node_id].next.insert(pos, (b, next));
                        self.trie.push(new);
                        break;
                    }
                }
            } else if pre == id.len() {
                let b = node.fragment.as_bytes()[pre];
                let mut fragment = self.trie[node_id].fragment.split_off(pre);
                std::mem::swap(&mut fragment, &mut self.trie[node_id].fragment);
                let next = self.trie.len();
                let new = TrieNode {
                    fragment,
                    next: vec![(b, next)],
                    entries: vec![Arc::new(entry)],
                };
                self.trie.push(new);
                self.trie.swap(node_id, next);
                break;
            } else {
                let b = node.fragment.as_bytes()[pre];
                let c = id.as_bytes()[pre];
                let mut fragment = self.trie[node_id].fragment.split_off(pre);
                std::mem::swap(&mut fragment, &mut self.trie[node_id].fragment);
                let next = self.trie.len();
                let new = TrieNode {
                    fragment,
                    next: if b < c {
                        vec![(b, next), (c, next + 1)]
                    } else {
                        vec![(c, next + 1), (b, next)]
                    },
                    entries: vec![],
                };
                self.trie.push(new);
                self.trie.swap(node_id, next);

                let new = TrieNode {
                    fragment: id[pre..].to_owned(),
                    next: vec![],
                    entries: vec![Arc::new(entry)],
                };
                self.trie.push(new);
                break;
            }

            id = &id[pre..];
        }
    }

    fn search_system_id<'a>(
        &'a self,
        system_id: &'a str,
    ) -> impl Iterator<Item = Arc<CatalogEntry>> + 'a {
        self.collect_entry(system_id, |id, entry| match entry {
            CatalogEntry::System { .. } if id.is_empty() => true,
            CatalogEntry::RewriteSystem { .. } | CatalogEntry::DelegateSystem { .. } => true,
            _ => false,
        })
    }

    fn search_system_suffix<'a>(
        &'a self,
        reversed_system_id: &'a str,
    ) -> impl Iterator<Item = Arc<CatalogEntry>> + 'a {
        self.collect_entry(reversed_system_id, |_, entry| {
            matches!(entry, CatalogEntry::SystemSuffix { .. })
        })
    }

    fn search_public_id<'a>(
        &'a self,
        public_id: &'a str,
        prefer_mode: PreferMode,
    ) -> impl Iterator<Item = Arc<CatalogEntry>> + 'a {
        self.collect_entry(public_id, move |id, entry| match entry {
            CatalogEntry::Public { prefer, .. }
                if id.is_empty() && prefer.unwrap_or(prefer_mode) == PreferMode::Public =>
            {
                true
            }
            CatalogEntry::DelegatePublic { prefer, .. }
                if prefer.unwrap_or(prefer_mode) == PreferMode::Public =>
            {
                true
            }
            _ => false,
        })
    }

    fn search_uri<'a>(&'a self, uri: &'a str) -> impl Iterator<Item = Arc<CatalogEntry>> + 'a {
        self.collect_entry(uri, |id, entry| match entry {
            CatalogEntry::URI { .. } if id.is_empty() => true,
            CatalogEntry::RewriteURI { .. } | CatalogEntry::DelegateURI { .. } => true,
            _ => false,
        })
    }

    fn search_uri_suffix<'a>(
        &'a self,
        reversed_system_id: &'a str,
    ) -> impl Iterator<Item = Arc<CatalogEntry>> + 'a {
        self.collect_entry(reversed_system_id, |_, entry| {
            matches!(entry, CatalogEntry::URISuffix { .. })
        })
    }

    fn collect_entry<'a>(
        &'a self,
        mut id: &'a str,
        mut predicate: impl FnMut(&str, &CatalogEntry) -> bool + 'a,
    ) -> impl Iterator<Item = Arc<CatalogEntry>> + 'a {
        let mut now = 0;
        let mut entry_index = usize::MAX;
        let mut end = false;
        std::iter::from_fn(move || {
            if end {
                return None;
            }
            loop {
                let node = &self.trie[now];
                if entry_index == usize::MAX {
                    let Some(suffix) = id.strip_prefix(&node.fragment) else {
                        end = true;
                        return None;
                    };
                    entry_index = 0;
                    id = suffix;
                }

                while entry_index < node.entries.len() {
                    let entry = node.entries[entry_index].clone();
                    entry_index += 1;

                    if predicate(id, &entry) {
                        return Some(entry);
                    }
                }

                entry_index = usize::MAX;
                if id.is_empty() {
                    end = true;
                    return None;
                }

                let next = id.as_bytes()[0];
                if let Ok(next) = node.next.binary_search_by_key(&next, |k| k.0) {
                    now = node.next[next].1;
                } else {
                    end = true;
                    return None;
                }
            }
        })
    }
}
