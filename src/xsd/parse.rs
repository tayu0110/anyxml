use std::{
    borrow::Cow,
    cell::RefCell,
    collections::{BTreeMap, HashMap, HashSet},
    io::Read,
    sync::{Arc, LazyLock},
};

use crate::{
    automata::xsregexp::{RegexpError, XSRegexp},
    error::{XMLError, XMLErrorDomain, XMLErrorLevel},
    sax::{
        AttributeType, Attributes, DOCUMENT_ENTITY_NAME, DefaultDecl, DefaultSAXHandler,
        EntityResolver, ErrorHandler, InputSource, Locator, NamespaceStack, SAXHandler, XMLReader,
        error::SAXParseError,
    },
    uri::{URIStr, URIString},
    xsd::{
        XML_SCHEMA_INSTANCE_NAMESPACE, XML_SCHEMA_NAMESPACE, XSDSchema,
        typedef::{
            Block, BlockSet, Boundary, Cardinality, Enumeration, Facets, Final, FinalSet, Length,
            Ordered, Pattern, SchemaValue, SchemaValueType, SimpleTypeDefinition, Variety,
            Whitespace, WhitespaceConstraint, XSD_ANY_SIMPLE_TYPE, get_builtin_simple_type,
        },
    },
};

const SCHEMA_SCHEMA_NAME: &str = "schema";
const SCHEMA_INCLUDE_NAME: &str = "include";
const SCHEMA_IMPORT_NAME: &str = "import";
const SCHEMA_REDEFINE_NAME: &str = "redefine";
const SCHEMA_ANNOTATION_NAME: &str = "annotation";
const SCHEMA_SIMPLETYPE_NAME: &str = "simpleType";
const SCHEMA_COMPLEXTYPE_NAME: &str = "complexType";
const SCHEMA_GROUP_NAME: &str = "group";
const SCHEMA_ATTGROUP_NAME: &str = "attributeGroup";
const SCHEMA_ELEMENT_NAME: &str = "element";
const SCHEMA_ATTRIBUTE_NAME: &str = "attribute";
const SCHEMA_NOTATION_NAME: &str = "notation";
const SCHEMA_APPINFO_NAME: &str = "appinfo";
const SCHEMA_DOCUMENTATION_NAME: &str = "documentation";
const SCHEMA_RESTRICTION_NAME: &str = "restriction";
const SCHEMA_LIST_NAME: &str = "list";
const SCHEMA_UNION_NAME: &str = "union";
const SCHEMA_MINEXCLUSIVE_NAME: &str = "minExclusive";
const SCHEMA_MININCLUSIVE_NAME: &str = "minInclusive";
const SCHEMA_MAXEXCLUSIVE_NAME: &str = "maxExclusive";
const SCHEMA_MAXINCLUSIVE_NAME: &str = "maxInclusive";
const SCHEMA_TOTALDIGITS_NAME: &str = "totalDigits";
const SCHEMA_FRACDIGITS_NAME: &str = "fractionDigits";
const SCHEMA_LENGTH_NAME: &str = "length";
const SCHEMA_MINLENGTH_NAME: &str = "minLength";
const SCHEMA_MAXLENGTH_NAME: &str = "maxLength";
const SCHEMA_ENUMERATION_NAME: &str = "enumeration";
const SCHEMA_WHITESPACE_NAME: &str = "whiteSpace";
const SCHEMA_PATTERN_NAME: &str = "pattern";
const SCHEMA_SIMPLECONT_NAME: &str = "simpleContent";
const SCHEMA_COMPLEXCONT_NAME: &str = "complexContent";
const SCHEMA_ALL_NAME: &str = "all";
const SCHEMA_CHOICE_NAME: &str = "choice";
const SCHEMA_SEQUENCE_NAME: &str = "sequence";
const SCHEMA_ANY_NAME: &str = "any";
const SCHEMA_ANYATT_NAME: &str = "anyAttribute";
const SCHEMA_EXTENSION_NAME: &str = "extension";
const SCHEMA_UNIQUE_NAME: &str = "unique";
const SCHEMA_KEY_NAME: &str = "key";
const SCHEMA_KEYREF_NAME: &str = "keyref";
const SCHEMA_SELECTOR_NAME: &str = "selector";
const SCHEMA_FIELD_NAME: &str = "field";

thread_local! {
    static META_SCHEMA_HOLDER_V1_0: RefCell<Vec<XSDSchema>> = const { RefCell::new(vec![]) };
}

#[derive(Default)]
pub struct Schema {
    merged_uris: HashSet<Arc<URIStr>>,
    pub(super) type_defs: HashMap<(Option<Arc<str>>, Arc<str>), TypeDefinition>,
    pub(super) att_decls: HashMap<(Option<Arc<str>>, Arc<str>), AttributeDeclaration>,
    pub(super) elem_decls: HashMap<(Option<Arc<str>>, Arc<str>), ElementDeclaration>,
    pub(super) attgroups: HashMap<(Option<Arc<str>>, Arc<str>), AttributeGroupDefinition>,
    pub(super) modelgroups: HashMap<(Option<Arc<str>>, Arc<str>), ModelGroupDefinition>,
    pub(super) notations: HashMap<(Option<Arc<str>>, Arc<str>), Notation>,
}

impl Schema {
    pub(super) fn extend(&mut self, other: Self) {
        for (key, value) in other.type_defs {
            self.type_defs.entry(key).or_insert(value);
        }
        for (key, value) in other.att_decls {
            self.att_decls.entry(key).or_insert(value);
        }
        for (key, value) in other.elem_decls {
            self.elem_decls.entry(key).or_insert(value);
        }
        for (key, value) in other.attgroups {
            self.attgroups.entry(key).or_insert(value);
        }
        for (key, value) in other.modelgroups {
            self.modelgroups.entry(key).or_insert(value);
        }
        for (key, value) in other.notations {
            self.notations.entry(key).or_insert(value);
        }
    }
}

impl XSDSchema {
    /// Parse XSD schema using `uri` and `encoding`.
    ///
    /// If a custom [`EntityResolver`](crate::sax::EntityResolver) or [`ErrorHandler`](crate::sax::ErrorHandler)
    /// is required, it can be specified using `handler`.
    ///
    /// If the document cannot be parsed for any reason,
    /// or the parsed document cannot be recognized as the XSD schema, return [`Err`].
    pub fn parse_uri<Handler: SAXHandler>(
        uri: impl AsRef<URIStr>,
        encoding: Option<&str>,
        handler: Option<Handler>,
    ) -> Result<Self, XMLError> {
        let mut handler: Box<dyn SAXHandler> = handler
            .map(|h| Box::new(h) as Box<dyn SAXHandler>)
            .unwrap_or_else(|| Box::new(DefaultSAXHandler));
        let mut meta = Self::parse_meta_schema();
        let validator = meta.new_validate_handler(XSDParseHandler::with_handler(&mut *handler));
        let mut parser = XMLReader::builder().set_handler(validator).build();
        parser.parse_uri(&uri, encoding)?;
        let handler = parser.handler.child;
        META_SCHEMA_HOLDER_V1_0.with_borrow_mut(|holder| holder.push(meta));
        Ok(Self {
            schema: handler.schema,
        })
    }

    /// Parse XSD schema using `reader`, `encoding` and `uri`.
    ///
    /// If a custom [`EntityResolver`](crate::sax::EntityResolver) or [`ErrorHandler`](crate::sax::ErrorHandler)
    /// is required, it can be specified using `handler`.
    ///
    /// If the document cannot be parsed for any reason,
    /// or the parsed document cannot be recognized as the XSD schema, return [`Err`].
    pub fn parse_reader<'a, Handler: SAXHandler>(
        reader: impl Read + 'a,
        encoding: Option<&str>,
        uri: Option<&URIStr>,
        handler: Option<Handler>,
    ) -> Result<Self, XMLError> {
        let mut handler: Box<dyn SAXHandler> = handler
            .map(|h| Box::new(h) as Box<dyn SAXHandler>)
            .unwrap_or_else(|| Box::new(DefaultSAXHandler));
        let mut meta = Self::parse_meta_schema();
        let validator = meta.new_validate_handler(XSDParseHandler::with_handler(&mut *handler));
        let mut parser = XMLReader::builder().set_handler(validator).build();
        parser.parse_reader(reader, encoding, uri)?;
        let handler = parser.handler.child;
        META_SCHEMA_HOLDER_V1_0.with_borrow_mut(|holder| holder.push(meta));
        Ok(Self {
            schema: handler.schema,
        })
    }

    /// Parse XSD schema using `schema` and `uri`.
    ///
    /// If a custom [`EntityResolver`](crate::sax::EntityResolver) or [`ErrorHandler`](crate::sax::ErrorHandler)
    /// is required, it can be specified using `handler`.
    ///
    /// If the document cannot be parsed for any reason,
    /// or the parsed document cannot be recognized as the XSD schema, return [`Err`].
    pub fn parse_str<Handler: SAXHandler>(
        schema: &str,
        uri: Option<&URIStr>,
        handler: Option<Handler>,
    ) -> Result<Self, XMLError> {
        let mut handler: Box<dyn SAXHandler> = handler
            .map(|h| Box::new(h) as Box<dyn SAXHandler>)
            .unwrap_or_else(|| Box::new(DefaultSAXHandler));
        let mut meta = Self::parse_meta_schema();
        let validator = meta.new_validate_handler(XSDParseHandler::with_handler(&mut *handler));
        let mut parser = XMLReader::builder().set_handler(validator).build();
        parser.parse_str(schema, uri)?;
        let handler = parser.handler.child;
        META_SCHEMA_HOLDER_V1_0.with_borrow_mut(|holder| holder.push(meta));
        Ok(Self {
            schema: handler.schema,
        })
    }

    fn parse_meta_schema() -> Self {
        if let Some(meta) = META_SCHEMA_HOLDER_V1_0.with_borrow_mut(|holder| holder.pop()) {
            return meta;
        }
        let mut handler = XSDParseHandler::with_handler(MetaSchemaParseHandler);
        handler.meta = true;
        let mut parser = XMLReader::builder().set_handler(handler).build();
        let xml = include_str!("../../resources/xsd/XMLSchema-compact.xsd");
        parser.parse_str(xml, None).unwrap();
        Self {
            schema: parser.handler.schema,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum InclusionState {
    TopLevel,
    InInclude,
    InImport,
    InRedefine,
}

#[derive(Default)]
struct RedefinedComponents {
    pub(super) type_defs: HashMap<(Option<Arc<str>>, Arc<str>), OwnedTypeDefinition>,
    pub(super) attgroup_defs: HashMap<(Option<Arc<str>>, Arc<str>), AttributeGroupDefinition>,
    pub(super) modelgroup_defs: HashMap<(Option<Arc<str>>, Arc<str>), ModelGroupDefinition>,
}

/// SAX handler for parsing XSD schema documents.
pub struct XSDParseHandler<H: SAXHandler = DefaultSAXHandler> {
    schema: Schema,
    locator: Arc<Locator>,
    // if true, this handler is in used for meta-schema parsing.
    meta: bool,

    // context
    target_namespace: Option<Arc<str>>,
    parent_target_namespace: Option<Arc<str>>,
    unnamed: usize,
    skip_depth: usize,
    inclusion_state: InclusionState,
    // context stack
    ns_stack: NamespaceStack,
    element_stack: Vec<&'static str>,

    // qualification form
    attr_form_default: QualificationForm,
    elem_form_default: QualificationForm,
    // block
    block_default: BlockSet,
    // final
    final_default: FinalSet,

    // external schemas
    includes: Vec<URIString>,
    imports: Vec<(Option<Arc<str>>, URIString)>,
    redefined_schema_location: Option<Arc<URIStr>>,
    redefines: HashMap<Arc<URIStr>, RedefinedComponents>,

    // temporary component buffer
    st_stack: Vec<SimpleTypeDefinition>,
    ct_stack: Vec<ComplexTypeDefinition>,
    ed_stack: Vec<ElementDeclaration>,
    pt_stack: Vec<Particle>,
    attribute_decl: Box<AttributeDeclaration>,
    attr_group_def: Box<AttributeGroupDefinition>,
    model_group_def: Box<ModelGroupDefinition>,
    idc: Box<IDCDefinition>,

    // handler and last error
    // By passing the handler and the last error to a separate structure,
    // there is no longer a need to acquire exclusive locks on all fields
    // solely for the purpose of reporting errors, thereby alleviating the
    // burden of ownership.
    holder: ErrorHolder<H>,
}

struct ErrorHolder<H: SAXHandler = DefaultSAXHandler> {
    last_error: Result<(), XMLError>,
    child: H,
}

impl<H: SAXHandler> ErrorHandler for ErrorHolder<H> {
    fn error(&mut self, mut error: SAXParseError) {
        if matches!(
            error.domain,
            XMLErrorDomain::XSDParser | XMLErrorDomain::XSDValid
        ) {
            error.domain = XMLErrorDomain::XSDParser;
            self.last_error = Err(error.error.clone());
        }
        self.child.error(error);
    }
    fn fatal_error(&mut self, mut error: SAXParseError) {
        self.last_error = Err(error.error.clone());
        if matches!(error.domain, XMLErrorDomain::XSDValid) {
            error.domain = XMLErrorDomain::XSDParser;
        }
        self.child.fatal_error(error);
    }
    fn warning(&mut self, mut error: SAXParseError) {
        if matches!(error.domain, XMLErrorDomain::XSDValid) {
            error.domain = XMLErrorDomain::XSDParser;
        }
        self.child.warning(error);
    }
}

impl<H: SAXHandler> XSDParseHandler<H> {
    fn with_handler(handler: H) -> Self {
        Self {
            schema: Schema::default(),
            locator: Arc::new(Locator::default()),
            meta: false,
            skip_depth: 0,
            inclusion_state: InclusionState::TopLevel,
            target_namespace: None,
            parent_target_namespace: None,
            unnamed: 0,
            ns_stack: NamespaceStack::default(),
            element_stack: vec![],
            attr_form_default: QualificationForm::Unqualified,
            elem_form_default: QualificationForm::Unqualified,
            block_default: BlockSet::default(),
            final_default: FinalSet::default(),
            includes: vec![],
            imports: vec![],
            redefined_schema_location: None,
            redefines: HashMap::new(),
            st_stack: vec![],
            ct_stack: vec![],
            ed_stack: vec![],
            pt_stack: vec![],
            attribute_decl: Box::default(),
            attr_group_def: Box::default(),
            model_group_def: Box::default(),
            idc: Box::default(),
            holder: ErrorHolder {
                last_error: Ok(()),
                child: handler,
            },
        }
    }

    /// Create dummy name. The returned name differs for each call.
    ///
    /// Since the returned name is not a syntactically valid XML name,
    /// it does not conflict with any valid name.
    fn dummy_name(&mut self) -> Arc<str> {
        self.unnamed += 1;
        Arc::from(format!("#unnamed{}", self.unnamed))
    }

    fn get_qname(&self, atts: &Attributes, name: &str) -> Option<(Option<Arc<str>>, Arc<str>)> {
        let value = atts.get_value_by_qname(name)?;
        if let Some((pre, loc)) = value.split_once(':') {
            let namespace = self.ns_stack.get(pre)?;
            Some((Some(namespace.namespace_name.clone()), loc.into()))
        } else if let Some(namespace) = self.ns_stack.get("") {
            Some((Some(namespace.namespace_name.clone()), value.into()))
        } else {
            Some((None, value.into()))
        }
    }

    fn get_element_default(&mut self, atts: &Attributes) -> Option<ValueConstraint> {
        // The following SCCs should be validated, but since neither the type nor the
        // element content model is clear at this point, they will be checked later.
        // - [cos-valid-default] clause 1
        //   if type is simple, the value must be match to this type.
        // - [cos-valid-default] clause 2.1
        //   if type is complex and its content type is empty or elemtn-only,
        //   any default value must not be set.
        // - [cos-valid-default] clause 2.2.1
        //   if type is complex and its content type is simple, the value must be match to the simple type.
        // - [cos-valid-default] clause 2.2.2
        //   if type is complex and its content type is mixed and its particle is not emptiable,
        //   any default value must not be set.
        match (
            atts.get_value_by_qname("default"),
            atts.get_value_by_qname("fixed"),
        ) {
            (Some(_), Some(_)) => {
                let msg =
                    "The attribute 'default' and 'fixed' of <element> must not both be present.";
                self.holder.error(SAXParseError {
                    error: XMLError::XSDError(XSDError::SrcElement),
                    level: XMLErrorLevel::Error,
                    domain: XMLErrorDomain::XSDParser,
                    line: self.locator.line(),
                    column: self.locator.column(),
                    system_id: self.locator.system_id(),
                    public_id: self.locator.public_id(),
                    message: Cow::Borrowed(msg),
                });
                None
            }
            (None, None) => None,
            (Some(default), None) => Some(ValueConstraint::Default(default.into())),
            (None, Some(fixed)) => Some(ValueConstraint::Fixed(fixed.into())),
        }
    }

    fn get_attribute_default(&mut self, atts: &Attributes) -> Option<ValueConstraint> {
        // The following SCCs should be validated, but since the
        // type is clear at this point, they will be checked later.
        // - [a-props-correct] clause 2
        //   the value must be match to the declarated type.
        match (
            atts.get_value_by_qname("default"),
            atts.get_value_by_qname("fixed"),
        ) {
            (Some(_), Some(_)) => {
                // [src-attribute] clause 1
                // default and fixed must not both be present.
                let msg =
                    "The attribute 'default' and 'fixed' of <attribute> must not both be present.";
                self.holder.error(SAXParseError {
                    error: XMLError::XSDError(XSDError::SrcAttribute),
                    level: XMLErrorLevel::Error,
                    domain: XMLErrorDomain::XSDParser,
                    line: self.locator.line(),
                    column: self.locator.column(),
                    system_id: self.locator.system_id(),
                    public_id: self.locator.public_id(),
                    message: Cow::Borrowed(msg),
                });
                None
            }
            (None, None) => None,
            (Some(default), None) => Some(ValueConstraint::Default(default.into())),
            (None, Some(fixed)) => Some(ValueConstraint::Fixed(fixed.into())),
        }
    }

    fn get_namespace_constraint(
        &self,
        atts: &Attributes,
        name: &str,
    ) -> Option<NamespaceConstraint> {
        let value = atts.get_value_by_qname(name)?;
        match value {
            "##any" => Some(NamespaceConstraint::Any),
            "##other" => Some(NamespaceConstraint::Not(self.target_namespace.clone())),
            _ => {
                let mut list = vec![];
                for uri in value.split_ascii_whitespace() {
                    if uri == "##targetNamespace" {
                        list.push(self.target_namespace.clone());
                    } else if uri == "##local" {
                        list.push(None);
                    } else {
                        list.push(Some(uri.into()));
                    }
                }
                // It is unclear whether duplicates should be treated as errors,
                // but for the purposes of this process, they will simply be removed
                // rather than treated as errors.
                list.sort_unstable();
                list.dedup();
                Some(NamespaceConstraint::List(list))
            }
        }
    }

    fn get_scope(&self) -> Option<Scope> {
        for &name in self.element_stack.iter().rev() {
            match name {
                SCHEMA_COMPLEXTYPE_NAME => {
                    if let Some(ct) = self.ct_stack.last() {
                        return Some(Scope::Local(ct.target_namespace.clone(), ct.name.clone()));
                    }
                }
                SCHEMA_GROUP_NAME | SCHEMA_ATTGROUP_NAME => return None,
                _ => {}
            }
        }
        Some(Scope::Global)
    }

    fn push_attribute_use(&mut self, attuse: AttributeUse) {
        if self.element_stack.last() == Some(&SCHEMA_ATTGROUP_NAME) {
            self.attr_group_def.attribute_uses.push(attuse);
        } else if let Some(ct) = self.ct_stack.last_mut() {
            ct.attribute_uses.push(attuse);
        }
    }

    /// Check if the current position is under <redefine>.
    fn in_redefine(&self) -> bool {
        // Since <redefine> appears only directly under <schema>,
        // there is no need to scan the entire stack.
        self.element_stack.len() >= 2 && self.element_stack[1] == SCHEMA_REDEFINE_NAME
    }

    fn process_include(&mut self, include: URIString, meta: Option<&mut XSDSchema>) {
        let locator = self.locator.clone();
        let include_abs = locator.system_id().resolve(&include);
        if self.schema.merged_uris.contains(include_abs.as_ref()) {
            return;
        }
        let target_namespace = self.target_namespace.clone();
        let state = self.inclusion_state;
        match self.holder.child.resolve_entity(
            DOCUMENT_ENTITY_NAME,
            None,
            &locator.system_id(),
            &include,
        ) {
            Ok(source) => {
                self.parent_target_namespace = target_namespace.clone();
                self.target_namespace = None;
                self.inclusion_state = InclusionState::InInclude;
                let last_error = std::mem::replace(&mut self.holder.last_error, Ok(()));
                let ret = if let Some(meta) = meta {
                    let validator = meta.new_validate_handler(&mut *self);
                    let mut parser = XMLReader::builder().set_handler(validator).build();
                    parser.parse_source(Box::new(source), None)
                } else {
                    let mut parser = XMLReader::builder().set_handler(&mut *self).build();
                    parser.parse_source(Box::new(source), None)
                };
                match ret {
                    Ok(()) => {
                        if let Err(err) = std::mem::replace(&mut self.holder.last_error, Ok(())) {
                            // [src-include] clause 1
                            let msg = format!(
                                "Included schema '{}' cannot be parsed because '{}'.",
                                include, err
                            );
                            self.holder.error(SAXParseError {
                                error: XMLError::XSDError(XSDError::SrcInclude),
                                level: XMLErrorLevel::Error,
                                domain: XMLErrorDomain::XSDParser,
                                line: locator.line(),
                                column: locator.column(),
                                system_id: locator.system_id(),
                                public_id: locator.public_id(),
                                message: Cow::Owned(msg),
                            });
                        }
                    }
                    Err(err) => {
                        // It is an error for it to resolve but the rest of clause 1
                        // above to fail to be satisfied. Failure to resolve may well
                        // cause less than complete ·assessment· outcomes, of course.
                        //
                        // [src-include] clause 1
                        let msg = format!(
                            "Included schema '{}' cannot be parsed because '{}'.",
                            include, err
                        );
                        self.holder.error(SAXParseError {
                            error: XMLError::XSDError(XSDError::SrcInclude),
                            level: XMLErrorLevel::Error,
                            domain: XMLErrorDomain::XSDParser,
                            line: locator.line(),
                            column: locator.column(),
                            system_id: locator.system_id(),
                            public_id: locator.public_id(),
                            message: Cow::Owned(msg),
                        });
                    }
                }
                self.holder.last_error =
                    std::mem::replace(&mut self.holder.last_error, Ok(())).and(last_error);
            }
            Err(err) => {
                // A resource resolution failure is not an error. However, since it
                // is better for the user to be able to identify a resource resolution
                // failure, report a warning to indicate that the resolution failed.
                //
                // [src-include]
                // It is not an error for the ·actual value· of the schemaLocation
                // [attribute] to fail to resolve it all, in which case no corresponding
                // inclusion is performed.
                let msg = format!(
                    "The included resource '{}' cannot be resolved because {}.",
                    include, err
                );
                self.warning(SAXParseError {
                    error: XMLError::XSDError(XSDError::SrcInclude),
                    level: XMLErrorLevel::Warning,
                    domain: XMLErrorDomain::XSDParser,
                    line: locator.line(),
                    column: locator.column(),
                    system_id: locator.system_id(),
                    public_id: locator.public_id(),
                    message: Cow::Owned(msg),
                });
            }
        }
        self.target_namespace = target_namespace;
        self.locator = locator;
        self.inclusion_state = state;
    }

    fn process_import(
        &mut self,
        import: URIString,
        namespace: Option<Arc<str>>,
        meta: Option<&mut XSDSchema>,
    ) {
        let locator = self.locator.clone();
        let import_abs = locator.system_id().resolve(&import);
        if self.schema.merged_uris.contains(import_abs.as_ref()) {
            return;
        }
        let target_namespace = self.target_namespace.clone();
        let state = self.inclusion_state;
        match self.holder.child.resolve_entity(
            DOCUMENT_ENTITY_NAME,
            None,
            &locator.system_id(),
            &import,
        ) {
            Ok(source) => {
                // Since cases where the `namespace` attribute and the target namespace are the
                // same are ignored as errors during parsing, `parent_target_namespace` is used
                // to check [src-import] clause 3.
                self.parent_target_namespace = namespace;
                self.target_namespace = None;
                self.inclusion_state = InclusionState::InImport;
                let last_error = std::mem::replace(&mut self.holder.last_error, Ok(()));
                let ret = if let Some(meta) = meta {
                    let validator = meta.new_validate_handler(&mut *self);
                    let mut parser = XMLReader::builder().set_handler(validator).build();
                    parser.parse_source(Box::new(source), None)
                } else {
                    let mut parser = XMLReader::builder().set_handler(&mut *self).build();
                    parser.parse_source(Box::new(source), None)
                };
                match ret {
                    Ok(()) => {
                        if let Err(err) = std::mem::replace(&mut self.holder.last_error, Ok(())) {
                            // [src-import] clause 2
                            let msg = format!(
                                "Imported schema '{}' cannot be parsed because '{}'.",
                                import, err
                            );
                            self.holder.error(SAXParseError {
                                error: XMLError::XSDError(XSDError::SrcImport),
                                level: XMLErrorLevel::Error,
                                domain: XMLErrorDomain::XSDParser,
                                line: locator.line(),
                                column: locator.column(),
                                system_id: locator.system_id(),
                                public_id: locator.public_id(),
                                message: Cow::Owned(msg),
                            });
                        }
                    }
                    Err(err) => {
                        // It is an error for it to resolve but the rest of clause 2 above to
                        // fail to be satisfied. Failure to find a referent may well cause less
                        // than complete ·assessment· outcomes, of course.
                        //
                        // [src-import] clause 2
                        let msg = format!(
                            "Imported schema '{}' cannot be parsed because '{}'.",
                            import, err
                        );
                        self.holder.error(SAXParseError {
                            error: XMLError::XSDError(XSDError::SrcImport),
                            level: XMLErrorLevel::Error,
                            domain: XMLErrorDomain::XSDParser,
                            line: locator.line(),
                            column: locator.column(),
                            system_id: locator.system_id(),
                            public_id: locator.public_id(),
                            message: Cow::Owned(msg),
                        });
                    }
                }
                self.holder.last_error =
                    std::mem::replace(&mut self.holder.last_error, Ok(())).and(last_error);
            }
            Err(_) => {
                // A resource resolution failure is not an error. However, since it
                // is better for the user to be able to identify a resource resolution
                // failure, report a warning to indicate that the resolution failed.
                //
                // [src-import]
                // It is not an error for the application schema reference strategy to fail.
                let msg = format!("The imported resource '{}' cannot be resolved.", import);
                self.warning(SAXParseError {
                    error: XMLError::XSDError(XSDError::SrcImport),
                    level: XMLErrorLevel::Warning,
                    domain: XMLErrorDomain::XSDParser,
                    line: locator.line(),
                    column: locator.column(),
                    system_id: locator.system_id(),
                    public_id: locator.public_id(),
                    message: Cow::Owned(msg),
                });
            }
        }
        self.target_namespace = target_namespace;
        self.locator = locator;
        self.inclusion_state = state;
    }

    fn process_redefine(
        &mut self,
        location: Arc<URIStr>,
        components: RedefinedComponents,
        meta: Option<&mut XSDSchema>,
    ) {
        let locator = self.locator.clone();
        let location_abs = locator.system_id().resolve(&location);
        if !self.schema.merged_uris.contains(location_abs.as_ref()) {
            let target_namespace = self.target_namespace.clone();
            let state = self.inclusion_state;
            match self.holder.child.resolve_entity(
                DOCUMENT_ENTITY_NAME,
                None,
                &locator.system_id(),
                &location,
            ) {
                Ok(source) => {
                    self.parent_target_namespace = target_namespace.clone();
                    self.target_namespace = None;
                    self.inclusion_state = InclusionState::InRedefine;
                    let last_error = std::mem::replace(&mut self.holder.last_error, Ok(()));
                    let ret = if let Some(meta) = meta {
                        let validator = meta.new_validate_handler(&mut *self);
                        let mut parser = XMLReader::builder().set_handler(validator).build();
                        parser.parse_source(Box::new(source), None)
                    } else {
                        let mut parser = XMLReader::builder().set_handler(&mut *self).build();
                        parser.parse_source(Box::new(source), None)
                    };
                    let ret = ret.and(std::mem::replace(&mut self.holder.last_error, Ok(())));
                    if let Err(err) = ret {
                        // [src-redefine] clause 2
                        let msg = format!(
                            "Redefined schema '{}' cannot be parsed because '{}'.",
                            location, err
                        );
                        self.holder.error(SAXParseError {
                            error: XMLError::XSDError(XSDError::SrcRedefine),
                            level: XMLErrorLevel::Error,
                            domain: XMLErrorDomain::XSDParser,
                            line: locator.line(),
                            column: locator.column(),
                            system_id: locator.system_id(),
                            public_id: locator.public_id(),
                            message: Cow::Owned(msg),
                        });
                        self.holder.last_error = Err(err);
                    }
                    self.holder.last_error =
                        std::mem::replace(&mut self.holder.last_error, Ok(())).and(last_error);
                }
                Err(err) => {
                    // If one or more components are redefined, unlike with <include>
                    // and <import>, a resource resolution failure is considered an error.
                    // If no components are redefined, a resource resolution failure is
                    // not considered an error, just as with <include> and <import>.
                    //
                    // [src-redefine] clause 1
                    // 1 If there are any element information items among the [children]
                    //   other than <annotation> then the ·actual value· of the schemaLocation
                    //   [attribute] must successfully resolve.
                    let msg = format!(
                        "The redefined resource '{}' cannot be resolved because {}.",
                        location, err
                    );
                    if components.attgroup_defs.is_empty()
                        && components.modelgroup_defs.is_empty()
                        && components.type_defs.is_empty()
                    {
                        self.warning(SAXParseError {
                            error: XMLError::XSDError(XSDError::SrcRedefine),
                            level: XMLErrorLevel::Warning,
                            domain: XMLErrorDomain::XSDParser,
                            line: locator.line(),
                            column: locator.column(),
                            system_id: locator.system_id(),
                            public_id: locator.public_id(),
                            message: Cow::Owned(msg),
                        });
                    } else {
                        self.holder.error(SAXParseError {
                            error: XMLError::XSDError(XSDError::SrcRedefine),
                            level: XMLErrorLevel::Error,
                            domain: XMLErrorDomain::XSDParser,
                            line: locator.line(),
                            column: locator.column(),
                            system_id: locator.system_id(),
                            public_id: locator.public_id(),
                            message: Cow::Owned(msg),
                        });
                    }
                }
            }
            self.target_namespace = target_namespace;
            self.locator = locator;
            self.inclusion_state = state;
        }

        // Overwrite redefined components.
        // It is unclear how to handle situations where a component being redefined contains
        // a reference to another component being redefined—or even where such references form
        // a cycle—but I will overwrite without considering that.

        for ((ns, name), def) in components.type_defs {
            if name.starts_with('#') {
                // This definition is a local definition, so just insert it as is.
                let def = match def {
                    OwnedTypeDefinition::Simple(st) => TypeDefinition::Simple(st.into()),
                    OwnedTypeDefinition::Complex(ct) => TypeDefinition::Complex(ct.into()),
                };
                self.schema.type_defs.insert((ns, name), def);
                continue;
            }
            match self.schema.type_defs.remove(&(ns.clone(), name.clone())) {
                Some(redefined) => {
                    self.unnamed += 1;
                    let redefined_name =
                        Arc::<str>::from(format!("#redefined{}/{}", self.unnamed, name));
                    let def = match def {
                        OwnedTypeDefinition::Simple(mut st) => {
                            st.base_type_definition = (ns.clone(), redefined_name.clone());
                            TypeDefinition::Simple(st.into())
                        }
                        OwnedTypeDefinition::Complex(mut ct) => {
                            ct.base_type_definition = (ns.clone(), redefined_name.clone());
                            TypeDefinition::Complex(ct.into())
                        }
                    };
                    self.schema
                        .type_defs
                        .insert((ns.clone(), redefined_name), redefined);
                    self.schema.type_defs.insert((ns, name), def);
                }
                None => {
                    // [src-expredef]
                    // In all cases there must be a top-level definition item of the
                    // appropriate name and kind in the <redefine>d schema document.
                    let msg = format!(
                        "The type definition '{}{}' is redefined, but it is not contained in redefined schema '{}'.",
                        ns.map(|ns| format!("{{{ns}}}")).unwrap_or_default(),
                        name,
                        location
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::SrcExpredef),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                }
            }
        }
        for ((ns, name), mut def) in components.modelgroup_defs {
            if name.starts_with('#') {
                // This definition is a local definition, so just insert it as is.
                self.schema.modelgroups.insert((ns, name), def);
                continue;
            }
            match self.schema.modelgroups.remove(&(ns.clone(), name.clone())) {
                Some(redefined) => {
                    self.unnamed += 1;
                    let redefined_name =
                        Arc::<str>::from(format!("#redefined{}/{}", self.unnamed, name));
                    let cnt = self.overwrite_group_reference(
                        &mut def.model_group,
                        ns.clone(),
                        name.clone(),
                        redefined_name.clone(),
                    );
                    if cnt > 1 {
                        // [src-redefine] clause 6.1.1
                        let msg = format!(
                            "<group> referring to redefined <group> must not appear more than once, but appears {} times.",
                            cnt
                        );
                        self.holder.error(SAXParseError {
                            error: XMLError::XSDError(XSDError::SrcRedefine),
                            level: XMLErrorLevel::Error,
                            domain: XMLErrorDomain::XSDParser,
                            line: self.locator.line(),
                            column: self.locator.column(),
                            system_id: self.locator.system_id(),
                            public_id: self.locator.public_id(),
                            message: Cow::Owned(msg),
                        });
                    } else if cnt == 0 {
                        // [src-redefine] clause 6.2

                        // Since [cos-particle-restrict] only defines rules for Particles,
                        // it cannot directly inspect Model Groups, so a dummy Particle is
                        // required.
                        let dummy = Particle {
                            min_occurs: 1,
                            max_occurs: 1,
                            ..Default::default()
                        };
                        if !self.is_valid_restriction_model_group(
                            &dummy,
                            &def.model_group,
                            &dummy,
                            &redefined.model_group,
                        ) {
                            let msg = format!(
                                "'{}{}' is not restricted validly by redefined model group definition.",
                                ns.as_deref()
                                    .map(|ns| format!("{{{ns}}}"))
                                    .unwrap_or_default(),
                                name
                            );
                            self.holder.error(SAXParseError {
                                error: XMLError::XSDError(XSDError::SrcRedefine),
                                level: XMLErrorLevel::Error,
                                domain: XMLErrorDomain::XSDParser,
                                line: self.locator.line(),
                                column: self.locator.column(),
                                system_id: self.locator.system_id(),
                                public_id: self.locator.public_id(),
                                message: Cow::Owned(msg),
                            });
                        }
                    }
                    self.schema
                        .modelgroups
                        .insert((ns.clone(), redefined_name), redefined);
                    self.schema.modelgroups.insert((ns, name), def);
                }
                None => {
                    // [src-expredef]
                    // In all cases there must be a top-level definition item of the
                    // appropriate name and kind in the <redefine>d schema document.
                    let msg = format!(
                        "The model group definition '{}{}' is redefined, but it is not contained in redefined schema '{}'.",
                        ns.map(|ns| format!("{{{ns}}}")).unwrap_or_default(),
                        name,
                        location
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::SrcExpredef),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                }
            }
        }
        for ((ns, name), mut def) in components.attgroup_defs {
            if name.starts_with('#') {
                // This definition is a local definition, so just insert it as is.
                self.schema.attgroups.insert((ns, name), def);
                continue;
            }
            match self.schema.attgroups.remove(&(ns.clone(), name.clone())) {
                Some(redefined) => {
                    self.unnamed += 1;
                    let redefined_name =
                        Arc::<str>::from(format!("#redefined{}/{}", self.unnamed, name));
                    let cnt = self.overwrite_attribute_group_reference(
                        &mut def,
                        ns.clone(),
                        name.clone(),
                        redefined_name.clone(),
                    );
                    if cnt > 1 {
                        // [src-redefine] clause 7.1
                        let msg = format!(
                            "<attributeGroup> referring to redefined <attributeGroup> must not appear more than once, but appears {} times.",
                            cnt
                        );
                        self.holder.error(SAXParseError {
                            error: XMLError::XSDError(XSDError::SrcRedefine),
                            level: XMLErrorLevel::Error,
                            domain: XMLErrorDomain::XSDParser,
                            line: self.locator.line(),
                            column: self.locator.column(),
                            system_id: self.locator.system_id(),
                            public_id: self.locator.public_id(),
                            message: Cow::Owned(msg),
                        });
                    } else if cnt == 0 {
                        // todo!("[src-redefine] clause 7.2")
                    }
                    self.schema
                        .attgroups
                        .insert((ns.clone(), redefined_name), redefined);
                    self.schema.attgroups.insert((ns, name), def);
                }
                None => {
                    // [src-expredef]
                    // In all cases there must be a top-level definition item of the
                    // appropriate name and kind in the <redefine>d schema document.
                    let msg = format!(
                        "The attribute group definition '{}{}' is redefined, but it is not contained in redefined schema '{}'.",
                        ns.map(|ns| format!("{{{ns}}}")).unwrap_or_default(),
                        name,
                        location
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::SrcExpredef),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                }
            }
        }
    }

    /// Rewrite references to model groups according to the semantics of <redefine>.
    ///
    /// The return value indicates the number of references that were rewritten.
    /// If this number is 2 or greater, it constitutes a violation of [src-redefine] clause 6.1.1.
    ///
    /// When rewriting references, perform the check specified in [src-redefine] clause 6.1.2.
    /// Specifically, if the minOccurs or maxOccurs of a reference being rewritten is not 1,
    /// an error is reported.
    fn overwrite_group_reference(
        &mut self,
        group: &mut ModelGroup,
        target_namespace: Option<Arc<str>>,
        from: Arc<str>,
        to: Arc<str>,
    ) -> usize {
        let mut cnt = 0;
        for particle in group.particles.iter_mut() {
            match &mut particle.term {
                Term::ModelGroupRef(ns, name) if *ns == target_namespace && *name == from => {
                    *name = to.clone();
                    cnt += 1;

                    // [src-redefine] clause 6.1.2
                    if particle.min_occurs != 1 {
                        let msg = format!(
                            "'minOccurs' of <group> referring to redefined <group> must have '1' or be absent, but has '{}'.",
                            particle.min_occurs
                        );
                        self.holder.error(SAXParseError {
                            error: XMLError::XSDError(XSDError::SrcRedefine),
                            level: XMLErrorLevel::Error,
                            domain: XMLErrorDomain::XSDParser,
                            line: self.locator.line(),
                            column: self.locator.column(),
                            system_id: self.locator.system_id(),
                            public_id: self.locator.public_id(),
                            message: Cow::Owned(msg),
                        });
                    }
                    if particle.max_occurs != 1 {
                        let msg = format!(
                            "'maxOccurs' of <group> referring to redefined <group> must have '1' or be absent, but has '{}'.",
                            particle.max_occurs
                        );
                        self.holder.error(SAXParseError {
                            error: XMLError::XSDError(XSDError::SrcRedefine),
                            level: XMLErrorLevel::Error,
                            domain: XMLErrorDomain::XSDParser,
                            line: self.locator.line(),
                            column: self.locator.column(),
                            system_id: self.locator.system_id(),
                            public_id: self.locator.public_id(),
                            message: Cow::Owned(msg),
                        });
                    }
                }
                Term::ModelGroup(model) => {
                    cnt += self.overwrite_group_reference(
                        model,
                        target_namespace.clone(),
                        from.clone(),
                        to.clone(),
                    );
                }
                _ => {}
            }
        }
        cnt
    }

    /// Rewrite references to attribute groups according to the semantics of <redefine>.
    ///
    /// The return value indicates the number of references that were rewritten.
    /// If this number is 2 or greater, it constitutes a violation of [src-redefine] clause 7.1.
    fn overwrite_attribute_group_reference(
        &mut self,
        attgroup: &mut AttributeGroupDefinition,
        target_namespace: Option<Arc<str>>,
        from: Arc<str>,
        to: Arc<str>,
    ) -> usize {
        let mut cnt = 0;
        for def in attgroup.attribute_group_refs.iter_mut() {
            if def.0 == target_namespace && def.1 == from {
                def.1 = to.clone();
                cnt += 1;
            }
        }
        cnt
    }

    /// Check if this particle is a valid restriction of another particle `base`.
    ///
    /// # Reference
    /// - [cos-particle-restrict](https://www.w3.org/TR/xmlschema-1/#cos-particle-restrict)
    fn is_valid_restriction(&self, restricted: &Particle, base: &Particle) -> bool {
        if std::ptr::eq(restricted, base) {
            // [cos-particle-restrict] clause 1
            return true;
        }

        match (&base.term, &restricted.term) {
            (Term::ElementDecl(lns, lloc), Term::ElementDecl(rns, rloc)) => {
                // "NameAnd- TypeOK" [rcase-NameAndTypeOK]

                // [rcase-NameAndTypeOK] clause 2
                if !restricted.check_range_ok(base) {
                    return false;
                }

                if lns == rns && lloc == rloc {
                    // Since each declaration distinguishes keys from those in other declarations,
                    // if the keys match, they are the same declaration, which is a valid restriction.
                    return true;
                }

                // [rcase-NameAndTypeOK] clause 1
                let Some((bdecl, rdecl)) = self
                    .schema
                    .elem_decls
                    .get(&(lns.clone(), lloc.clone()))
                    .zip(self.schema.elem_decls.get(&(rns.clone(), rloc.clone())))
                    .filter(|(b, r)| b.target_namespace == r.target_namespace && b.name == r.name)
                else {
                    return false;
                };

                // [rcase-NameAndTypeOK] clause 3
                if matches!(bdecl.scope, Some(Scope::Global))
                    && matches!(rdecl.scope, Some(Scope::Global))
                {
                    // clause 3.1
                    true
                } else {
                    // clause 3.2
                    if !bdecl.nillable && rdecl.nillable {
                        // clause 3.2.1
                        return false;
                    }
                    if let Some(ValueConstraint::Fixed(bfixed)) = bdecl.value_constraint.as_ref() {
                        // clause 3.2.2
                        if rdecl.value_constraint.is_none() {
                            return false;
                        }
                        if let Some(ValueConstraint::Fixed(rfixed)) =
                            rdecl.value_constraint.as_ref()
                            && bfixed != rfixed
                        {
                            return false;
                        }
                    }
                    if !rdecl.idc_definitions.iter().any(|idc| {
                        bdecl.idc_definitions.iter().all(|bidc| {
                            bidc.target_namespace != idc.target_namespace || bidc.name != idc.name
                        })
                    }) {
                        // clause 3.2.3
                        // This clause is very strange.
                        // There is no definition of what it means for a set of IDCs to be a
                        // subset of another set of IDCs.
                        // According to Section 3.11.1, which states that "Identity-constraint
                        // definitions are identified by their {name} and {target namespace}",
                        // it is natural to assume that the equivalence of IDCs can be determined
                        // by their names.
                        // On the other hand, IDCs can only be defined as descendants of element
                        // declarations; they cannot reference other IDCs to incorporate them as
                        // part of their own IDC, nor can multiple IDCs with the same name exist
                        // within the same schema.
                        // Therefore, it is impossible—except for the empty set—for an element
                        // declaration to have a set of IDCs that is a subset of the set of IDCs
                        // of any other element declaration.
                        // The following bug has been reported regarding the XSD specification
                        // in relation to this unnatural clause. However, since I was unable to
                        // find a response to this bug report, the true meaning of this clause
                        // remains unclear.
                        // https://www.w3.org/Bugs/Public/show_bug.cgi?id=2084
                        //
                        // Here, the implementation follows the principle that IDC equivalence
                        // is determined by name. In other words, an element declaration that
                        // is a valid restriction of an element declaration with IDCs can have
                        // no IDCs of its own.
                        return false;
                    }
                    if !bdecl
                        .disallowed_substitutions
                        .is_subset_of(&rdecl.disallowed_substitutions)
                    {
                        // clause 3.2.4
                        return false;
                    }

                    // TODO: check clause 3.2.5

                    true
                }
            }
            (Term::ElementDecl(_, _), _) => false, // Forbidden
            (Term::Wildcard(wc), Term::ElementDecl(ns, _)) => {
                // "NSCompat" [rcase-NSCompat]

                restricted.check_range_ok(base) && wc.is_match(ns.as_deref())
            }
            (Term::Wildcard(base_wc), Term::Wildcard(restricted_wc)) => {
                // "NSSubset" [rcase-NSSubset]

                // [rcase-NSSubset] clause 1
                let mut ret = restricted.check_range_ok(base);
                // [rcase-NSSubset] clause 2
                ret &= restricted_wc.is_subset_of(base_wc);
                ret
            }
            (Term::Wildcard(_), Term::ModelGroup(mg)) => {
                // "NSRecurse- CheckCardinality" [rcase-NSRecurseCheckCardinality]

                let Some((min, max)) = restricted.effective_total_range(&self.schema.modelgroups)
                else {
                    return false;
                };
                mg.particles
                    .iter()
                    .all(|p| self.is_valid_restriction(p, base))
                    && base.min_occurs <= min
                    && max <= base.max_occurs
                    && max != usize::MAX
            }
            (Term::Wildcard(_), Term::ModelGroupRef(ns, loc)) => {
                // "NSRecurse- CheckCardinality" [rcase-NSRecurseCheckCardinality]

                let Some(mg) = self.schema.modelgroups.get(&(ns.clone(), loc.clone())) else {
                    return false;
                };
                let Some((min, max)) =
                    mg.model_group
                        .effective_total_range(&self.schema.modelgroups, 1, 1)
                else {
                    return false;
                };
                mg.model_group
                    .particles
                    .iter()
                    .all(|p| self.is_valid_restriction(p, base))
                    && base.min_occurs <= min
                    && max <= base.max_occurs
                    && max != usize::MAX
            }
            (Term::ModelGroup(mg), Term::ElementDecl(_, _)) => {
                // "Recurse- AsIfGroup" [rcase-RecurseAsIfGroup]

                let compositor = mg.compositor;
                let restricted = Particle {
                    min_occurs: 1,
                    max_occurs: 1,
                    term: Term::ModelGroup(ModelGroup {
                        compositor,
                        particles: vec![restricted.clone()],
                    }),
                };
                self.is_valid_restriction(&restricted, base)
            }
            (Term::ModelGroupRef(ns, loc), Term::ElementDecl(_, _)) => {
                // "Recurse- AsIfGroup" [rcase-RecurseAsIfGroup]

                let Some(def) = self.schema.modelgroups.get(&(ns.clone(), loc.clone())) else {
                    return false;
                };
                let compositor = def.model_group.compositor;
                let restricted = Particle {
                    min_occurs: 1,
                    max_occurs: 1,
                    term: Term::ModelGroup(ModelGroup {
                        compositor,
                        particles: vec![restricted.clone()],
                    }),
                };
                self.is_valid_restriction(&restricted, base)
            }
            _ => {
                let Some((bmg, rmg)) = base
                    .model_group(&self.schema)
                    .zip(restricted.model_group(&self.schema))
                else {
                    return false;
                };
                // model group specific constraint
                //
                // all-all:             "Recurse" [rcase-Recurse]
                // choice-choice:       "RecurseLax" [rcase-RecurseLax]
                // sequence-sequence:   "Recurse" [rcase-Recurse]
                // other:               Forbidden
                self.is_valid_restriction_model_group(restricted, rmg, base, bmg)
            }
        }
    }

    /// # Reference
    /// - [cos-particle-restrict](https://www.w3.org/TR/xmlschema-1/#cos-particle-restrict)
    /// - [rcase-Recurse](https://www.w3.org/TR/xmlschema-1/#rcase-Recurse)
    fn is_valid_restriction_model_group(
        &self,
        restricted: &Particle,
        restricted_mg: &ModelGroup,
        base: &Particle,
        base_mg: &ModelGroup,
    ) -> bool {
        match (base_mg.compositor, restricted_mg.compositor) {
            (Compositor::All, Compositor::All) | (Compositor::Sequence, Compositor::Sequence) => {
                // "Recurse" [rcase-Recurse]

                // [rcase-Recurse] clause 1
                let mut ret = restricted.check_range_ok(base);
                let mut ps = base_mg.particles.iter();
                // [rcase-Recurse] clause 2
                for particle in &restricted_mg.particles {
                    loop {
                        if let Some(base_particle) = ps.next() {
                            if self.is_valid_restriction(particle, base_particle) {
                                // [rcase-Recurse] clause 2.1
                                break;
                            }
                            if !base_particle.is_emptiable(&self.schema.modelgroups) {
                                // [rcase-Recurse] clause 2.2
                                ret = false;
                            }
                        } else {
                            if ret {
                                // [rcase-Recurse] clause 2
                                ret = false;
                            }
                            break;
                        }
                    }
                }
                ret
            }
            (Compositor::Choice, Compositor::Choice) => {
                // "RecurseLax" [rcase-RecurseLax]

                // [rcase-RecurseLax] clause 1
                let mut ret = restricted.check_range_ok(base);
                let mut ps = base_mg.particles.iter();
                // [rcase-RecurseLax] clause 2
                for particle in &restricted_mg.particles {
                    loop {
                        if let Some(base_particle) = ps.next() {
                            if self.is_valid_restriction(particle, base_particle) {
                                break;
                            }
                        } else {
                            if ret {
                                // [rcase-Recurse] clause 2
                                ret = false;
                            }
                            break;
                        }
                    }
                }

                ret
            }
            (Compositor::All, Compositor::Sequence) => {
                // "Recurse- Unordered" [rcase-RecurseUnordered]

                // [rcase-RecurseUnordered] clause 1
                let mut ret = restricted.check_range_ok(base);
                // [rcase-RecurseUnordered] clause 2
                let mut ok = vec![false; restricted_mg.particles.len()];
                'b: for base in &base_mg.particles {
                    for (i, ok) in ok.iter_mut().enumerate() {
                        if *ok {
                            continue;
                        }
                        if self.is_valid_restriction(&restricted_mg.particles[i], base) {
                            *ok = true;
                            continue 'b;
                        }
                    }

                    // [rcase-RecurseUnordered] clause 2.3
                    ret &= base.is_emptiable(&self.schema.modelgroups);
                }
                ret
            }
            (Compositor::Choice, Compositor::Sequence) => {
                // "MapAndSum" [rcase-MapAndSum]
                let min_occurs = restricted
                    .min_occurs
                    .saturating_mul(restricted_mg.particles.len());
                let max_occurs = restricted
                    .max_occurs
                    .saturating_mul(restricted_mg.particles.len());
                let tmp = Particle {
                    min_occurs,
                    max_occurs,
                    term: Term::ModelGroup(ModelGroup {
                        compositor: Compositor::Choice,
                        particles: vec![],
                    }),
                };
                // [rcase-MapAndSum] clause 2
                let mut ret = tmp.check_range_ok(base);
                // [rcase-MapAndSum] clause 1
                for particle in &restricted_mg.particles {
                    ret &= base_mg
                        .particles
                        .iter()
                        .any(|base| self.is_valid_restriction(particle, base));
                }
                ret
            }
            _ => false, // Forbidden
        }
    }

    fn check_all_simple_type_derivation(&mut self) -> Result<(), XMLError> {
        let mut ret = self
            .schema
            .type_defs
            .extract_if(|_, ty| matches!(*ty, TypeDefinition::Simple(_)))
            .collect::<HashMap<_, _>>();
        self.schema.type_defs.insert(
            (
                XSD_ANY_SIMPLE_TYPE.target_namespace.clone(),
                XSD_ANY_SIMPLE_TYPE.name.clone(),
            ),
            TypeDefinition::Simple(XSD_ANY_SIMPLE_TYPE.clone()),
        );
        while let Some(key) = ret.keys().next().cloned() {
            let value = ret.remove(&key).and_then(|st| st.as_simple_type()).unwrap();
            let mut stack = vec![(key, value)];
            while let Some((key, value)) = stack.pop() {
                if let Some(builtin) =
                    get_builtin_simple_type(value.target_namespace.as_deref(), &value.name)
                {
                    self.schema
                        .type_defs
                        .insert(key, TypeDefinition::Simple(builtin));
                    continue;
                }

                match value.variety.as_ref() {
                    Some(Variety::List(ns, name)) => {
                        let ikey = (ns.clone(), name.clone());
                        if let Some(item_type) = ret.remove(&ikey).and_then(|t| t.as_simple_type())
                        {
                            stack.push((key, value));
                            stack.push((ikey, item_type));
                            continue;
                        } else if stack.iter().any(|entry| entry.0 == ikey) {
                            // [st-props-correct] clause 2
                            let msg = format!(
                                "A circluar definition is found in the simple type '{}'",
                                value.display_name()
                            );
                            self.holder.error(SAXParseError {
                                error: XMLError::XSDError(XSDError::StPropsCorrect),
                                level: XMLErrorLevel::Error,
                                domain: XMLErrorDomain::XSDParser,
                                line: self.locator.line(),
                                column: self.locator.column(),
                                system_id: self.locator.system_id(),
                                public_id: self.locator.public_id(),
                                message: Cow::Owned(msg),
                            });
                            stack.clear();
                            break;
                        }
                    }
                    Some(Variety::Union(members)) => {
                        if let Some((member, member_type)) =
                            members.iter().find_map(|m| ret.remove_entry(m))
                        {
                            stack.push((key, value));
                            stack.push((member, member_type.as_simple_type().unwrap()));
                            continue;
                        } else if members
                            .iter()
                            .any(|m| stack.iter().any(|entry| entry.0 == *m))
                        {
                            // [st-props-correct] clause 2
                            let msg = format!(
                                "A circluar definition is found in the simple type '{}'.",
                                value.display_name()
                            );
                            self.holder.error(SAXParseError {
                                error: XMLError::XSDError(XSDError::StPropsCorrect),
                                level: XMLErrorLevel::Error,
                                domain: XMLErrorDomain::XSDParser,
                                line: self.locator.line(),
                                column: self.locator.column(),
                                system_id: self.locator.system_id(),
                                public_id: self.locator.public_id(),
                                message: Cow::Owned(msg),
                            });
                            stack.clear();
                            break;
                        }
                    }
                    _ => {}
                }

                let base = value.base_type_definition.clone();
                if let Some(base_type) = ret.remove(&base).and_then(|st| st.as_simple_type()) {
                    stack.push((key, value));
                    stack.push((base, base_type));
                    continue;
                }

                if let Some(base) = self
                    .schema
                    .type_defs
                    .get(&base)
                    .and_then(|st| st.as_simple_type())
                    .or_else(|| {
                        let base_t = get_builtin_simple_type(base.0.as_deref(), &base.1)?;
                        self.schema
                            .type_defs
                            .insert(base.clone(), TypeDefinition::Simple(base_t.clone()));
                        Some(base_t)
                    })
                {
                    if let Ok(new) = self.check_simple_type_derivation(base, value.clone()) {
                        self.schema
                            .type_defs
                            .insert(key, TypeDefinition::Simple(new));
                    } else {
                        stack.clear();
                        break;
                    }
                } else if stack.iter().any(|e| e.0 == base) {
                    // [st-props-correct] clause 2
                    let msg = format!(
                        "A circluar definition is found in the simple type '{}'.",
                        value.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::StPropsCorrect),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    stack.clear();
                    break;
                } else {
                    let base_ns = base.0.map(|ns| format!("{{{ns}}}")).unwrap_or_default();
                    let msg = format!(
                        "The simple type '{}' is derived from '{}{}', but '{}{}' is not found in this schema.",
                        value.display_name(),
                        base_ns,
                        base.1,
                        base_ns,
                        base.1
                    );
                    // [st-props-correct] clause 2 ?
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::StPropsCorrect),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    stack.clear();
                    break;
                }
            }
        }
        Ok(())
    }

    /// # Reference
    /// - [Schema Component Constraint: Derivation Valid (Restriction, Simple)](https://www.w3.org/TR/xmlschema-1/#cos-st-restricts)
    fn check_simple_type_derivation(
        &mut self,
        base: Arc<SimpleTypeDefinition>,
        ty: Arc<SimpleTypeDefinition>,
    ) -> Result<Arc<SimpleTypeDefinition>, XMLError> {
        macro_rules! prohibited_derivation {
            ( $type:literal, $error:ident ) => {
                let msg = format!(
                    "'{}' cannot be derived from '{}' because '{}' is prohibited.",
                    ty.display_name(),
                    base.display_name(),
                    $type
                );
                self.holder.error(SAXParseError {
                    error: XMLError::XSDError(XSDError::$error),
                    level: XMLErrorLevel::Error,
                    domain: XMLErrorDomain::XSDParser,
                    line: self.locator.line(),
                    column: self.locator.column(),
                    system_id: self.locator.system_id(),
                    public_id: self.locator.public_id(),
                    message: Cow::Owned(msg),
                });
                return Err(XMLError::XSDError(XSDError::$error));
            };
        }

        if base.r#final.restriction() {
            // [st-props-correct] clause 3
            prohibited_derivation!("restriction", StPropsCorrect);
        }

        let mut ret = Ok(());
        let mut new = SimpleTypeDefinition::clone(&ty);
        if matches!(new.variety, None | Some(Variety::Atomic(_))) {
            new.variety = base.variety.clone();
        }

        macro_rules! unapplicable_facet {
            ( $field:ident, $facet:literal, $check_method:ident ) => {
                if !new.facets.$field.$check_method() {
                    let msg = format!(
                        "The simple type '{}' must not have the facet '{}'.",
                        ty.display_name(),
                        $facet
                    );
                    // [cos-applicable-facets]
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::CosApplicableFacets),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    ret = Err(XMLError::XSDError(XSDError::CosApplicableFacets));
                }
            };
        }

        match new.variety.as_ref() {
            Some(Variety::Atomic(name)) => {
                match name.as_ref() {
                    "string" | "hexBinary" | "base64Binary" | "anyURI" | "QName" | "NOTATION" => {
                        unapplicable_facet!(max_inclusive, "maxInclusive", is_none);
                        unapplicable_facet!(max_exclusive, "maxExclusive", is_none);
                        unapplicable_facet!(min_exclusive, "minExclusive", is_none);
                        unapplicable_facet!(min_inclusive, "minInclusive", is_none);
                        unapplicable_facet!(total_digits, "totalDigits", is_none);
                        unapplicable_facet!(fraction_digits, "fractionDigits", is_none);
                    }
                    "boolean" => {
                        unapplicable_facet!(length, "length", is_none);
                        unapplicable_facet!(min_length, "minLength", is_none);
                        unapplicable_facet!(max_length, "maxLength", is_none);
                        unapplicable_facet!(enumeration, "enumeration", is_empty);
                        unapplicable_facet!(max_inclusive, "maxInclusive", is_none);
                        unapplicable_facet!(max_exclusive, "maxExclusive", is_none);
                        unapplicable_facet!(min_exclusive, "minExclusive", is_none);
                        unapplicable_facet!(min_inclusive, "minInclusive", is_none);
                        unapplicable_facet!(total_digits, "totalDigits", is_none);
                        unapplicable_facet!(fraction_digits, "fractionDigits", is_none);
                    }
                    "float" | "double" | "duration" | "dateTime" | "time" | "date"
                    | "gYearMonth" | "gYear" | "gMonthDay" | "gDay" | "gMonth" => {
                        unapplicable_facet!(length, "length", is_none);
                        unapplicable_facet!(min_length, "minLength", is_none);
                        unapplicable_facet!(max_length, "maxLength", is_none);
                        unapplicable_facet!(total_digits, "totalDigits", is_none);
                        unapplicable_facet!(fraction_digits, "fractionDigits", is_none);
                    }
                    "decimal" => {
                        unapplicable_facet!(length, "length", is_none);
                        unapplicable_facet!(min_length, "minLength", is_none);
                        unapplicable_facet!(max_length, "maxLength", is_none);
                    }
                    _ => unreachable!(),
                }
                if !matches!(base.variety, None | Some(Variety::Atomic(_))) {
                    // [cos-st-restricts] clause 1.1
                    let msg = format!(
                        "The base type of the atomic simple type '{}' must be atomic, but '{}' is not.",
                        new.display_name(),
                        base.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::CosStRestricts),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    return Err(XMLError::XSDError(XSDError::CosStRestricts));
                }

                // [cos-st-restricts] clause 1.2
                if base.r#final.restriction() {
                    prohibited_derivation!("restriction", CosStRestricts);
                }
            }
            Some(Variety::List(ns, name)) => {
                let Some(item_type) = self
                    .schema
                    .type_defs
                    .get(&(ns.clone(), name.clone()))
                    .and_then(|t| t.as_simple_type())
                else {
                    // [cos-st-restricts] ?
                    // It is unclear what kind of error occurs when `itemType` cannot
                    // be resolved , but for now, it will be treated as cos-st-restricts.
                    let msg = format!(
                        "The list itemType '{}{}' cannot be resolved.",
                        ns.as_deref()
                            .map(|ns| format!("{{{ns}}}"))
                            .unwrap_or_default(),
                        name
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::CosStRestricts),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    return Err(XMLError::XSDError(XSDError::CosStRestricts));
                };

                if !matches!(
                    item_type.variety,
                    Some(Variety::Atomic(_) | Variety::Union(_))
                ) {
                    // [cos-st-restricts] clause 2.1
                    let msg = format!(
                        "The list itemType must be either atomic or union, but '{}' is not.",
                        item_type.display_name(),
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::CosStRestricts),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    return Err(XMLError::XSDError(XSDError::CosStRestricts));
                }

                // [cos-st-restricts] clause 2.3
                if base.is_simple_ur_type() {
                    // clause 2.3.1
                    if item_type.r#final.list() {
                        // clause 2.3.1.1
                        prohibited_derivation!("list", CosStRestricts);
                    }
                    // clause 2.3.1.2
                    unapplicable_facet!(length, "length", is_none);
                    unapplicable_facet!(min_length, "minLength", is_none);
                    unapplicable_facet!(max_length, "maxLength", is_none);
                    unapplicable_facet!(pattern, "pattern", is_empty);
                    unapplicable_facet!(enumeration, "enumeration", is_empty);
                } else {
                    // clause 2.3.2
                    let Some(Variety::List(bns, bname)) = base.variety.as_ref() else {
                        // clause 2.3.2.1
                        let msg = format!(
                            "The base type '{}' of '{}' must be list, but is not.",
                            base.display_name(),
                            new.display_name()
                        );
                        self.holder.error(SAXParseError {
                            error: XMLError::XSDError(XSDError::CosStRestricts),
                            level: XMLErrorLevel::Error,
                            domain: XMLErrorDomain::XSDParser,
                            line: self.locator.line(),
                            column: self.locator.column(),
                            system_id: self.locator.system_id(),
                            public_id: self.locator.public_id(),
                            message: Cow::Owned(msg),
                        });
                        return Err(XMLError::XSDError(XSDError::CosStRestricts));
                    };

                    // clause 2.3.2.2
                    if base.r#final.restriction() {
                        prohibited_derivation!("restriction", CosStRestricts);
                    }

                    // clause 2.3.2.3
                    let Some(base_item_type) = self
                        .schema
                        .type_defs
                        .get(&(bns.clone(), bname.clone()))
                        .and_then(|t| t.as_simple_type())
                    else {
                        // If the itemType of the base type cannot be resolved,
                        // an error should have already occurred during the base
                        // type derivation check, so no error is reported here.
                        return Err(XMLError::XSDError(XSDError::CosStRestricts));
                    };
                    self.check_cos_st_derived_ok(&item_type, &base_item_type, FinalSet::default())?;

                    // clause 2.3.2.4
                    // This is identical with [cos-applicable-facets].
                }

                unapplicable_facet!(max_inclusive, "maxInclusive", is_none);
                unapplicable_facet!(max_exclusive, "maxExclusive", is_none);
                unapplicable_facet!(min_exclusive, "minExclusive", is_none);
                unapplicable_facet!(min_inclusive, "minInclusive", is_none);
                unapplicable_facet!(total_digits, "totalDigits", is_none);
                unapplicable_facet!(fraction_digits, "fractionDigits", is_none);
            }
            Some(Variety::Union(members)) => {
                // [cos-st-restricts] clause 3.1
                // According to Part 2, Section 4.1.2.3, since a member type for which
                // {variety} is "union" is replaced with that member type, this requirement
                // is automatically satisfied.
                // As an exception, since the simple ur-type have {variety} set to "absent",
                // the simple ur-type cannot be included as a member type.
                if members.iter().any(|t| {
                    t.0 == XSD_ANY_SIMPLE_TYPE.target_namespace && t.1 == XSD_ANY_SIMPLE_TYPE.name
                }) {
                    let msg = format!(
                        "The simple ur-type cannot be contained in the memberTypes of union simple type, but the memberTypes of '{}' contains.",
                        new.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::CosStRestricts),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    return Err(XMLError::XSDError(XSDError::CosStRestricts));
                }

                let members = members
                    .iter()
                    .map(|t| {
                        self.schema
                            .type_defs
                            .get(t)
                            .ok_or(XSDError::MissingSubComponent)
                    })
                    .collect::<Result<Vec<_>, XSDError>>()?
                    .into_iter()
                    .map(|t| t.as_simple_type().ok_or(XSDError::MissingSubComponent))
                    .collect::<Result<Vec<_>, XSDError>>()?;

                // [cos-st-restricts] clause 3.3
                if base.is_simple_ur_type() {
                    // clause 3.3.1
                    if members.iter().any(|t| t.r#final.union()) {
                        // clause 3.3.1.1
                        prohibited_derivation!("union", CosStRestricts);
                    }
                    // clause 3.3.1.2
                    unapplicable_facet!(pattern, "pattern", is_empty);
                    unapplicable_facet!(enumeration, "enumeration", is_empty);
                } else {
                    // clause 3.3.2
                    let Some(Variety::Union(bmembers)) = base.variety.as_ref() else {
                        // clause 3.3.2.1
                        let msg = format!(
                            "The base type '{}' of '{}' must be union, but is not.",
                            base.display_name(),
                            new.display_name()
                        );
                        self.holder.error(SAXParseError {
                            error: XMLError::XSDError(XSDError::CosStRestricts),
                            level: XMLErrorLevel::Error,
                            domain: XMLErrorDomain::XSDParser,
                            line: self.locator.line(),
                            column: self.locator.column(),
                            system_id: self.locator.system_id(),
                            public_id: self.locator.public_id(),
                            message: Cow::Owned(msg),
                        });
                        return Err(XMLError::XSDError(XSDError::CosStRestricts));
                    };

                    // clause 3.3.2.2
                    if base.r#final.restriction() {
                        prohibited_derivation!("restriction", CosStRestricts);
                    }

                    // clause 3.3.2.3
                    // Although the intended meaning of this constraint is unclear,
                    // if the base type is not a simple ur-type, the type is derived
                    // by restrictions from a union type; since the {variety} of such
                    // a type inherits the {variety} of the base type, this constraint
                    // should generally be satisfied automatically.
                    if members.len() != bmembers.len() {
                        return Err(XMLError::XSDError(XSDError::CosStRestricts));
                    }
                    for (m, bm) in members.iter().zip(bmembers.iter()) {
                        let bm = self
                            .schema
                            .type_defs
                            .get(bm)
                            .and_then(|t| t.as_simple_type())
                            .ok_or(XSDError::MissingSubComponent)?;
                        self.check_cos_st_derived_ok(m, &bm, FinalSet::default())?;
                    }

                    // clause 3.3.2.4
                    // This is identical with [cos-applicable-facets].
                }

                unapplicable_facet!(length, "length", is_none);
                unapplicable_facet!(min_length, "minLength", is_none);
                unapplicable_facet!(max_length, "maxLength", is_none);
                unapplicable_facet!(whitespace, "whiteSpace", is_none);
                unapplicable_facet!(max_inclusive, "maxInclusive", is_none);
                unapplicable_facet!(max_exclusive, "maxExclusive", is_none);
                unapplicable_facet!(min_exclusive, "minExclusive", is_none);
                unapplicable_facet!(min_inclusive, "minInclusive", is_none);
                unapplicable_facet!(total_digits, "totalDigits", is_none);
                unapplicable_facet!(fraction_digits, "fractionDigits", is_none);
            }
            None => {}
        }

        ret?;
        self.process_st_restrict_facets(&mut new, &base)?;
        self.determine_fundamental_facets(&mut new, &base);
        Ok(new.into())
    }

    /// # Reference
    /// - [Schema Component Constraint: Type Derivation OK (Simple)](https://www.w3.org/TR/xmlschema-1/#cos-st-derived-ok)
    fn check_cos_st_derived_ok(
        &self,
        d: &SimpleTypeDefinition,
        b: &SimpleTypeDefinition,
        subset: FinalSet,
    ) -> Result<(), XMLError> {
        if d.target_namespace == b.target_namespace && d.name == b.name {
            // clause 1
            return Ok(());
        }

        // clause 2
        if subset.restriction() || b.r#final.restriction() {
            // clause 2.1
            return Err(XMLError::XSDError(XSDError::CosStDerivedOk));
        }
        let base = self
            .schema
            .type_defs
            .get(&d.base_type_definition)
            .and_then(|t| t.as_simple_type())
            .ok_or(XMLError::XSDError(XSDError::CosStDerivedOk))?;
        if b.target_namespace == base.target_namespace && b.name == base.name {
            // clause 2.2.1
            return Ok(());
        }
        if !base.is_simple_ur_type() && self.check_cos_st_derived_ok(&base, b, subset).is_ok() {
            // clause 2.2.2
            return Ok(());
        }
        if b.is_simple_ur_type()
            && matches!(d.variety, Some(Variety::List(_, _) | Variety::Union(_)))
        {
            // clause 2.2.3
            return Ok(());
        }
        if let Some(Variety::Union(members)) = b.variety.as_ref()
            && members
                .iter()
                .filter_map(|member| self.schema.type_defs.get(member))
                .filter_map(|t| t.as_simple_type())
                .any(|t| self.check_cos_st_derived_ok(d, &t, subset).is_ok())
        {
            // clause 2.2.4
            return Ok(());
        }
        Err(XMLError::XSDError(XSDError::CosStDerivedOk))
    }

    /// # Reference
    /// - [Schema Component Constraint: Simple Type Restriction (Facets)](https://www.w3.org/TR/xmlschema-1/#st-restrict-facets)
    fn process_st_restrict_facets(
        &mut self,
        new: &mut SimpleTypeDefinition,
        base: &SimpleTypeDefinition,
    ) -> Result<(), XMLError> {
        match (new.variety.as_ref(), base.variety.as_ref()) {
            (Some(Variety::Atomic(lname)), Some(Variety::Atomic(rname))) => {
                if lname != rname {
                    // [st-restrict-facets] clause 2
                    let msg = format!(
                        "Base primitive types do not match between '{}' and '{}'",
                        new.display_name(),
                        base.display_name(),
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::StRestrictFacets),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    return Err(XMLError::XSDError(XSDError::StRestrictFacets));
                }
            }
            (Some(Variety::List(_, _)), Some(Variety::List(_, _)))
            | (Some(Variety::Union(_)), Some(Variety::Union(_))) => {}
            _ => {
                // [st-restrict-facets] clause 1
                if !base.is_simple_ur_type() {
                    let msg = format!(
                        "Derivation type does not match between '{}' and '{}'",
                        new.display_name(),
                        base.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::StRestrictFacets),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    return Err(XMLError::XSDError(XSDError::StRestrictFacets));
                }
            }
        }

        let mut ret = Ok(());
        match (new.facets.length, base.facets.length) {
            (Some(nl), Some(bl)) => {
                if nl.value != bl.value {
                    // [length-valid-restriction]
                    // Due to this constraint, the value "fixed" is not considered.
                    let msg = format!(
                        "'length' of '{}' does not match that of the base type '{}'.",
                        new.display_name(),
                        base.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::LengthValidRestriction),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    ret = Err(XMLError::XSDError(XSDError::LengthValidRestriction));
                }
            }
            (None, Some(l)) => new.facets.length = Some(l),
            (Some(_), None) | (None, None) => {}
        }
        match (new.facets.min_length, base.facets.min_length) {
            (Some(nl), Some(bl)) => {
                if bl.fixed && nl.value != bl.value {
                    // Part 2, Section 4.3.2.1 The minLength Schema Component
                    let msg = format!(
                        "'minLength' of the base type '{}' is fixed, but that of '{}' has different value.",
                        base.display_name(),
                        new.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::StRestrictFacets),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    ret = Err(XMLError::XSDError(XSDError::StRestrictFacets));
                } else if nl.value < bl.value {
                    // [minLength-valid-restriction]
                    let msg = format!(
                        "'minLength' of '{}' is less than that of the base type '{}'.",
                        new.display_name(),
                        base.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::MinLengthValidRestriction),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    ret = Err(XMLError::XSDError(XSDError::MinLengthValidRestriction));
                } else if nl.value != bl.value && new.facets.length.is_some() {
                    // [length-minLength-maxLength] clause 1.2
                    // minLength    length
                    // A            None
                    // B            C       << Now (Bad)
                    // D            C       << Now (Bad)
                    let msg = format!(
                        "'minLength' cannot be applied to '{}' because it is constrained by 'length'.",
                        new.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::LengthMinLengthMaxLength),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    ret = Err(XMLError::XSDError(XSDError::LengthMinLengthMaxLength));
                } else if new
                    .facets
                    .length
                    .is_some_and(|length| nl.value > length.value)
                {
                    // [length-minLength-maxLength] clause 1.1
                    let msg = format!(
                        "The value 'minLength' of '{}' is larger than its 'length'.",
                        new.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::LengthMinLengthMaxLength),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    ret = Err(XMLError::XSDError(XSDError::LengthMinLengthMaxLength));
                }
            }
            (None, Some(l)) => new.facets.min_length = Some(l),
            (Some(_), None) => {
                if new.facets.length.is_some() {
                    // [length-minLength-maxLength] clause 1.2
                    // minLength    length
                    // None         A
                    // B            A       << Now (Bad)
                    //
                    // minLength    length
                    // None         None
                    // A            B       << Now (Bad)
                    let msg = format!(
                        "'minLength' cannot be applied to '{}' because it is constrained by 'length'.",
                        new.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::LengthMinLengthMaxLength),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    ret = Err(XMLError::XSDError(XSDError::LengthMinLengthMaxLength));
                }
            }
            (None, None) => {}
        }
        match (new.facets.max_length, base.facets.max_length) {
            (Some(nl), Some(bl)) => {
                if bl.fixed && nl.value != bl.value {
                    // Part 2, Section 4.3.3.1 The maxLength Schema Component
                    let msg = format!(
                        "'maxLength' of the base type '{}' is fixed, but that of '{}' has different value.",
                        base.display_name(),
                        new.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::StRestrictFacets),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    ret = Err(XMLError::XSDError(XSDError::StRestrictFacets));
                } else if bl.value < nl.value {
                    // [maxLength-valid-restriction]
                    let msg = format!(
                        "'maxLength' of '{}' is greater than that of the base type '{}'.",
                        new.display_name(),
                        base.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::MaxLengthValidRestriction),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    ret = Err(XMLError::XSDError(XSDError::MaxLengthValidRestriction));
                } else if nl.value != bl.value && new.facets.length.is_some() {
                    // [length-minLength-maxLength] clause 2.2
                    // maxLength    length
                    // A            None
                    // B            C       << Now (Bad)
                    // D            C       << Now (Bad)
                    let msg = format!(
                        "'maxLength' cannot be applied to '{}' because it is constrained by 'length'.",
                        new.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::LengthMinLengthMaxLength),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    ret = Err(XMLError::XSDError(XSDError::LengthMinLengthMaxLength));
                } else if new
                    .facets
                    .length
                    .is_some_and(|length| nl.value < length.value)
                {
                    // [length-minLength-maxLength] clause 2.1
                    let msg = format!(
                        "The value 'minLength' of '{}' is larger than its 'length'.",
                        new.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::LengthMinLengthMaxLength),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    ret = Err(XMLError::XSDError(XSDError::LengthMinLengthMaxLength));
                }
            }
            (None, Some(l)) => new.facets.max_length = Some(l),
            (Some(_), None) => {
                if new.facets.length.is_some() {
                    // [length-minLength-maxLength] clause 1.2
                    // maxLength    length
                    // None         A
                    // B            A       << Now (Bad)
                    //
                    // minLength    length
                    // None         None
                    // A            B       << Now (Bad)
                    let msg = format!(
                        "'maxLength' cannot be applied to '{}' because it is constrained by 'length'.",
                        new.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::LengthMinLengthMaxLength),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    ret = Err(XMLError::XSDError(XSDError::LengthMinLengthMaxLength));
                }
            }
            (None, None) => {}
        }
        match (new.facets.whitespace, base.facets.whitespace) {
            (Some(nw), Some(bw)) => {
                if bw.fixed && nw.value != bw.value {
                    // Part 2, Section 4.3.6.1 The whiteSpace Schema Component
                    let msg = format!(
                        "'whiteSpace' of the base type '{}' is fixed, but that of '{}' has different value.",
                        base.display_name(),
                        new.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::StRestrictFacets),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    ret = Err(XMLError::XSDError(XSDError::StRestrictFacets));
                } else if nw.value < bw.value {
                    // [whiteSpace-valid-restriction]
                    let msg = format!(
                        "'whiteSpace' of the base type '{}' is '{}', but that of '{}' is '{}'.",
                        base.display_name(),
                        bw.value,
                        new.display_name(),
                        nw.value
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::WhiteSpaceValidRestriction),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                }
            }
            (None, Some(bw)) => new.facets.whitespace = Some(bw),
            (Some(_), None) | (None, None) => {}
        }
        match (new.facets.total_digits, base.facets.total_digits) {
            (Some(nl), Some(bl)) => {
                if bl.fixed && nl.value != bl.value {
                    // Part 2, Section 4.3.11.1 The totalDigits Schema Component
                    let msg = format!(
                        "'totalDigits' of the base type '{}' is fixed, but that of '{}' has different value.",
                        base.display_name(),
                        new.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::StRestrictFacets),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    ret = Err(XMLError::XSDError(XSDError::StRestrictFacets));
                } else if bl.value < nl.value {
                    // [totalDigits-valid-restriction]
                    let msg = format!(
                        "'totalDigits' of '{}' is greater than that of the base type '{}'.",
                        new.display_name(),
                        base.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::TotalDigitsValidRestriction),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    ret = Err(XMLError::XSDError(XSDError::TotalDigitsValidRestriction));
                }
            }
            (None, Some(l)) => new.facets.total_digits = Some(l),
            (Some(_), None) | (None, None) => {}
        }
        match (new.facets.fraction_digits, base.facets.fraction_digits) {
            (Some(nl), Some(bl)) => {
                if bl.fixed && nl.value != bl.value {
                    // Part 2, Section 4.3.12.1 The fractionDigits Schema Component
                    let msg = format!(
                        "'fractionDigits' of the base type '{}' is fixed, but that of '{}' has different value.",
                        base.display_name(),
                        new.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::StRestrictFacets),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    ret = Err(XMLError::XSDError(XSDError::StRestrictFacets));
                } else if bl.value < nl.value {
                    // [fractionDigits-valid-restriction]
                    let msg = format!(
                        "'fractionDigits' of '{}' is greater than that of the base type '{}'.",
                        new.display_name(),
                        base.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::FractionDigitsValidRestriction),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    ret = Err(XMLError::XSDError(XSDError::FractionDigitsValidRestriction));
                }
            }
            (None, Some(l)) => new.facets.fraction_digits = Some(l),
            (Some(_), None) | (None, None) => {}
        }
        let new_name = new.display_name().into_owned();
        // Because the actual name of the base type may not match the key due to the effects
        // of the <redefine> process, we must use the `new.base_type_definition`.
        let base_name = new.base_type_definition.clone();
        for enumeration in new.facets.enumeration.iter_mut() {
            match self.schema.parse_simple_type(
                &enumeration.value.raw_value,
                base_name.0.as_deref(),
                &base_name.1,
                &enumeration.ns_context,
            ) {
                Ok(value) => enumeration.value = value,
                Err(error) => {
                    // [enumeration-valid-restriction]
                    let msg = format!(
                        "The enumerated value '{}' of '{}' is invalid value of the base type '{}' because of '{}'.",
                        enumeration.value.raw_value,
                        new_name,
                        base.display_name(),
                        error
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::EnumerationValidRestriction),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    ret = Err(XMLError::XSDError(XSDError::EnumerationValidRestriction));
                }
            }
        }

        if new.facets.max_inclusive.is_some() && new.facets.max_exclusive.is_some() {
            // [maxInclusive-maxExclusive]
            let msg = format!(
                "'{}' has both 'maxInclusive' and 'maxExclusive'.",
                new.display_name()
            );
            self.holder.error(SAXParseError {
                error: XMLError::XSDError(XSDError::MaxInclusiveMaxExclusive),
                level: XMLErrorLevel::Error,
                domain: XMLErrorDomain::XSDParser,
                line: self.locator.line(),
                column: self.locator.column(),
                system_id: self.locator.system_id(),
                public_id: self.locator.public_id(),
                message: Cow::Owned(msg),
            });
            ret = Err(XMLError::XSDError(XSDError::MaxInclusiveMaxExclusive));
        }
        if new.facets.min_inclusive.is_some() && new.facets.min_exclusive.is_some() {
            // [minInclusive-minExclusive]
            let msg = format!(
                "'{}' has both 'minInclusive' and 'minExclusive'.",
                new.display_name()
            );
            self.holder.error(SAXParseError {
                error: XMLError::XSDError(XSDError::MinInclusiveMinExclusive),
                level: XMLErrorLevel::Error,
                domain: XMLErrorDomain::XSDParser,
                line: self.locator.line(),
                column: self.locator.column(),
                system_id: self.locator.system_id(),
                public_id: self.locator.public_id(),
                message: Cow::Owned(msg),
            });
            ret = Err(XMLError::XSDError(XSDError::MinInclusiveMinExclusive));
        }

        macro_rules! parse_or_derive_boundary {
            ( $field:ident, $facet:literal ) => {
                if let Some(n) = new.facets.$field.as_mut() {
                    match self.schema.parse_simple_type(
                        &n.value.raw_value,
                        base_name.0.as_deref(),
                        &base_name.1,
                        &n.ns_context,
                    ) {
                        Ok(value) => {
                            n.value = value;
                            if let Some(b) = base.facets.$field.as_ref() && b.fixed && b.value != n.value {
                                let msg = format!(
                                    "'{}' of the base type '{}' is fixed, but that of '{}' has different value.",
                                    $facet,
                                    base.display_name(),
                                    new.display_name()
                                );
                                self.holder.error(SAXParseError {
                                    error: XMLError::XSDError(XSDError::StRestrictFacets),
                                    level: XMLErrorLevel::Error,
                                    domain: XMLErrorDomain::XSDParser,
                                    line: self.locator.line(),
                                    column: self.locator.column(),
                                    system_id: self.locator.system_id(),
                                    public_id: self.locator.public_id(),
                                    message: Cow::Owned(msg),
                                });
                                ret = Err(XMLError::XSDError(XSDError::StRestrictFacets));
                            }
                        },
                        Err(error) => {
                            ret = Err(error.clone());
                            let msg = format!(
                                "The {} boundary '{}' of '{}' is invalid.",
                                $facet, n.value.raw_value, new_name
                            );
                            self.holder.error(SAXParseError {
                                error,
                                level: XMLErrorLevel::Error,
                                domain: XMLErrorDomain::XSDParser,
                                line: self.locator.line(),
                                column: self.locator.column(),
                                system_id: self.locator.system_id(),
                                public_id: self.locator.public_id(),
                                message: Cow::Owned(msg),
                            });
                        }
                    }

                } else {
                    new.facets.$field = base.facets.$field.clone();
                }
            };
        }
        parse_or_derive_boundary!(max_inclusive, "maxInclusive");
        parse_or_derive_boundary!(max_exclusive, "maxExclusive");
        parse_or_derive_boundary!(min_exclusive, "minExclusive");
        parse_or_derive_boundary!(min_inclusive, "minInclusive");

        macro_rules! verify_facet_relation {
            ( $min:ident, $max:ident, $error:ident, $op:tt, $min_facet:literal, $max_facet:literal, $cmp:literal) => {
                if new
                    .facets
                    .$min
                    .as_ref()
                    .zip(new.facets.$max.as_ref())
                    .is_some_and(|(min, max)| min.value $op max.value)
                {
                    let msg = format!(
                        "'{}' of '{}' is {} its '{}'.",
                        $min_facet,
                        new.display_name(),
                        $cmp,
                        $max_facet,
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::$error),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    ret = Err(XMLError::XSDError(XSDError::$error));
                }
            };
        }
        // [minLength-less-than-equal-to-maxLength]
        verify_facet_relation!(
            min_length,
            max_length,
            MinLengthLessThanEqualToMaxLength,
            >,
            "minLength",
            "maxLength",
            "greater than"
        );
        // [minInclusive-less-than-equal-to-maxInclusive]
        verify_facet_relation!(
            min_inclusive,
            max_inclusive,
            MinInclusiveLessThanEqualToMaxInclusive,
            >,
            "minInclusive",
            "maxInclusive",
            "greater than"
        );
        // [minExclusive-less-than-equal-to-maxExclusive]
        verify_facet_relation!(
            min_exclusive,
            max_exclusive,
            MinExclusiveLessThanEqualToMaxExclusive,
            >,
            "minExclusive",
            "maxExclusive",
            "greater than"
        );
        // [minExclusive-less-than-maxInclusive]
        verify_facet_relation!(
            min_exclusive,
            max_inclusive,
            MinExclusiveLessThanMaxInclusive,
            >=,
            "minExclusive",
            "maxInclusive",
            "greater than or equal to"
        );
        // [minInclusive-less-than-maxExclusive]
        verify_facet_relation!(
            min_inclusive,
            max_exclusive,
            MinInclusiveLessThanMaxExclusive,
            >=,
            "minInclusive",
            "maxExclusive",
            "greater than or equal to"
        );

        macro_rules! verify_boundary_valid_restriction {
            ( $nfield:ident, $bfield:ident, $error:ident, $op:tt, $nfacet:literal, $bfacet:literal, $cmp:literal ) => {
                if new
                    .facets
                    .$nfield
                    .as_ref().zip(base.facets.$bfield.as_ref())
                    .is_some_and(|(n, b)| n.value $op b.value)
                {
                    // [maxExclusive-valid-restriction]
                    // [maxInclusive-valid-restriction]
                    // [minExclusive-valid-restriction]
                    // [minInclusive-valid-restriction]
                    let msg = format!(
                        "'{}' of '{}' is {} '{}' of the base type '{}'.",
                        $nfacet,
                        new.display_name(),
                        $cmp,
                        $bfacet,
                        base.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::$error),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    ret = Err(XMLError::XSDError(XSDError::$error));
                }
            };
        }
        // [maxInclusive-valid-restriction] clause 1
        verify_boundary_valid_restriction!(
            max_inclusive,
            max_inclusive,
            MaxInclusiveValidRestriction,
            >,
            "maxInclusive",
            "maxInclusive",
            "greather than"
        );
        // [maxInclusive-valid-restriction] clause 2
        verify_boundary_valid_restriction!(
            max_inclusive,
            max_exclusive,
            MaxInclusiveValidRestriction,
            >=,
            "maxInclusive",
            "maxExclusive",
            "greater than or equal to"
        );
        // [maxInclusive-valid-restriction] clause 3
        verify_boundary_valid_restriction!(
            max_inclusive,
            min_inclusive,
            MaxInclusiveValidRestriction,
            <,
            "maxInclusive",
            "minInclusive",
            "less than"
        );
        // [maxInclusive-valid-restriction] clause 4
        verify_boundary_valid_restriction!(
            max_inclusive,
            min_exclusive,
            MaxInclusiveValidRestriction,
            <=,
            "maxInclusive",
            "minExclusive",
            "less than or equal to"
        );
        // [maxExclusive-valid-restriction] clause 1
        verify_boundary_valid_restriction!(
            max_exclusive,
            max_exclusive,
            MaxExclusiveValidRestriction,
            >,
            "maxExclusive",
            "maxExclusive",
            "greather than"
        );
        // [maxExclusive-valid-restriction] clause 2
        verify_boundary_valid_restriction!(
            max_exclusive,
            max_inclusive,
            MaxExclusiveValidRestriction,
            >,
            "maxExclusive",
            "maxInclusive",
            "greather than"
        );
        // [maxExclusive-valid-restriction] clause 3
        verify_boundary_valid_restriction!(
            max_exclusive,
            min_inclusive,
            MaxExclusiveValidRestriction,
            <=,
            "maxExclusive",
            "minInclusive",
            "less than or equal to"
        );
        // [maxExclusive-valid-restriction] clause 4
        verify_boundary_valid_restriction!(
            max_exclusive,
            min_exclusive,
            MaxExclusiveValidRestriction,
            <=,
            "maxExclusive",
            "minExclusive",
            "less than or equal to"
        );
        // [minExclusive-valid-restriction] clause 1
        verify_boundary_valid_restriction!(
            min_exclusive,
            min_exclusive,
            MinExclusiveValidRestriction,
            <,
            "minExclusive",
            "minExclusive",
            "less than"
        );
        // [minExclusive-valid-restriction] clause 2
        verify_boundary_valid_restriction!(
            min_exclusive,
            max_inclusive,
            MinExclusiveValidRestriction,
            >,
            "minExclusive",
            "maxInclusive",
            "greather than"
        );
        // [minExclusive-valid-restriction] clause 3
        verify_boundary_valid_restriction!(
            min_exclusive,
            min_inclusive,
            MinExclusiveValidRestriction,
            <,
            "minExclusive",
            "minInclusive",
            "less than"
        );
        // [minExclusive-valid-restriction] clause 4
        verify_boundary_valid_restriction!(
            min_exclusive,
            max_exclusive,
            MinExclusiveValidRestriction,
            >=,
            "minExclusive",
            "maxExclusive",
            "greater than or equal to"
        );
        // [minInclusive-valid-restriction] clause 1
        verify_boundary_valid_restriction!(
            min_inclusive,
            min_inclusive,
            MinInclusiveValidRestriction,
            <,
            "minInclusive",
            "minInclusive",
            "less than"
        );
        // [minInclusive-valid-restriction] clause 2
        verify_boundary_valid_restriction!(
            min_inclusive,
            max_inclusive,
            MinInclusiveValidRestriction,
            >,
            "minInclusive",
            "maxInclusive",
            "greater than"
        );
        // [minInclusive-valid-restriction] clause 3
        verify_boundary_valid_restriction!(
            min_inclusive,
            min_exclusive,
            MinInclusiveValidRestriction,
            <=,
            "minInclusive",
            "minExclusive",
            "less than or equal to"
        );
        // [minInclusive-valid-restriction] clause 4
        verify_boundary_valid_restriction!(
            min_inclusive,
            max_exclusive,
            MinInclusiveValidRestriction,
            >=,
            "minInclusive",
            "maxExclusive",
            "greater than or equal to"
        );

        ret
    }

    fn determine_fundamental_facets(
        &self,
        ty: &mut SimpleTypeDefinition,
        base: &SimpleTypeDefinition,
    ) {
        match ty.variety.as_ref() {
            Some(Variety::Atomic(name)) => {
                let prim = get_builtin_simple_type(Some(XML_SCHEMA_NAMESPACE), name).unwrap();
                ty.fundamental_facets.ordered = prim.fundamental_facets.ordered;
                ty.fundamental_facets.bounded = (ty.facets.min_inclusive.is_some()
                    || ty.facets.min_exclusive.is_some())
                    && (ty.facets.max_inclusive.is_some() || ty.facets.max_exclusive.is_some());
                ty.fundamental_facets.cardinality =
                    (|facets: &Facets, base: &SimpleTypeDefinition| -> Cardinality {
                        if matches!(base.fundamental_facets.cardinality, Cardinality::Finite) {
                            return Cardinality::Finite;
                        }
                        if facets.length.is_some()
                            || facets.max_length.is_some()
                            || facets.total_digits.is_some()
                        {
                            return Cardinality::Finite;
                        }
                        if (facets.min_inclusive.is_none() && facets.min_exclusive.is_none())
                            || (facets.max_inclusive.is_none() && facets.max_exclusive.is_none())
                        {
                            return Cardinality::CountablyInfinite;
                        }
                        if facets.fraction_digits.is_some() {
                            return Cardinality::Finite;
                        }
                        if matches!(
                            prim.name.as_ref(),
                            "date" | "gYearMonth" | "gYear" | "gMonthDay" | "gDay" | "gMonth"
                        ) {
                            Cardinality::Finite
                        } else {
                            Cardinality::CountablyInfinite
                        }
                    })(&ty.facets, base);
                ty.fundamental_facets.numeric = base.fundamental_facets.numeric;
            }
            Some(Variety::List(_, _)) => {
                ty.fundamental_facets.ordered = Ordered::False;
                ty.fundamental_facets.bounded = ty.facets.length.is_some()
                    || (ty.facets.min_length.is_some() && ty.facets.max_length.is_some());
                ty.fundamental_facets.cardinality = if ty.facets.length.is_some()
                    || (ty.facets.min_length.is_some() && ty.facets.max_length.is_some())
                {
                    Cardinality::Finite
                } else {
                    Cardinality::CountablyInfinite
                };
                ty.fundamental_facets.numeric = false;
            }
            Some(Variety::Union(members)) => {
                let members = members
                    .iter()
                    .filter_map(|t| self.schema.type_defs.get(t))
                    .filter_map(|t| t.as_simple_type())
                    .collect::<Vec<_>>();
                let common_ancestor = self.schema.common_ancestor_of_simple_types(&members);

                ty.fundamental_facets.ordered = if members
                    .iter()
                    .all(|t| matches!(t.fundamental_facets.ordered, Ordered::False))
                {
                    Ordered::False
                } else if !common_ancestor.is_simple_ur_type() {
                    common_ancestor.fundamental_facets.ordered
                } else {
                    Ordered::Partial
                };
                ty.fundamental_facets.bounded =
                    members.iter().all(|t| t.fundamental_facets.bounded)
                        && !common_ancestor.is_simple_ur_type();
                ty.fundamental_facets.cardinality = if members
                    .iter()
                    .all(|t| matches!(t.fundamental_facets.cardinality, Cardinality::Finite))
                {
                    Cardinality::Finite
                } else {
                    Cardinality::CountablyInfinite
                };
                ty.fundamental_facets.numeric =
                    members.iter().all(|t| t.fundamental_facets.numeric);
            }
            None => {}
        }
    }

    /// # Reference
    /// - [3.4.6 Constraints on Complex Type Definition Schema Components](https://www.w3.org/TR/xmlschema-1/#coss-ct)
    fn check_all_complex_type_derivation(&mut self) -> Result<(), XMLError> {
        let mut ret = self
            .schema
            .type_defs
            .extract_if(|_, ty| matches!(*ty, TypeDefinition::Complex(_)))
            .filter_map(|(key, value)| value.as_complex_type().map(|v| (key, v)))
            .collect::<BTreeMap<_, _>>();
        self.schema.type_defs.insert(
            XSD_ANY_TYPE.base_type_definition.clone(),
            TypeDefinition::Complex(XSD_ANY_TYPE.clone()),
        );
        while let Some((key, value)) = ret.pop_first() {
            let mut stack = vec![(key, value)];
            while let Some((key, value)) = stack.pop() {
                let base = value.base_type_definition.clone();
                if let Some(base_type) = ret.remove(&base) {
                    stack.push((key, value));
                    stack.push((base, base_type));
                    continue;
                }

                if !self.schema.type_defs.contains_key(&base.clone())
                    && let Some(builtin) = get_builtin_simple_type(base.0.as_deref(), &base.1)
                {
                    self.schema
                        .type_defs
                        .insert(base.clone(), TypeDefinition::Simple(builtin));
                }

                if let Some(base) = self.schema.type_defs.get(&base).cloned() {
                    if matches!(base, TypeDefinition::Simple(_))
                        && !matches!(value.derivation_method, DerivationMethod::Extension)
                    {
                        // [ct-props-correct] clause 2
                        let msg = format!(
                            "The complex type '{}' cannot be derived by restriction from simple type '{}'.",
                            value.display_name(),
                            base.display_name()
                        );
                        self.holder.error(SAXParseError {
                            error: XMLError::XSDError(XSDError::CtPropsCorrect),
                            level: XMLErrorLevel::Error,
                            domain: XMLErrorDomain::XSDParser,
                            line: self.locator.line(),
                            column: self.locator.column(),
                            system_id: self.locator.system_id(),
                            public_id: self.locator.public_id(),
                            message: Cow::Owned(msg),
                        });
                        stack.clear();
                        break;
                    }
                    let mut new = ComplexTypeDefinition::clone(&value);
                    if self
                        .compute_attributes_of_complex_type(&mut new, &base)
                        .is_err()
                        || self.check_ct_props_correct_4_5(&mut new).is_err()
                    {
                        // [ct-props-correct] clause 4, 5
                        // The error is considered to have already been reported.
                        stack.clear();
                        break;
                    }
                    if self
                        .determine_content_type_of_complex_type(&mut new, &base)
                        .is_err()
                    {
                        stack.clear();
                        break;
                    }

                    let ret = match new.derivation_method {
                        DerivationMethod::Restriction => {
                            // verify [derivation-ok-restriction]
                            self.check_derivation_ok_restriction(&mut new, &base)
                        }
                        DerivationMethod::Extension => {
                            // verify [cos-ct-extends]
                            self.check_cos_ct_extends(&mut new, &base)
                        }
                    };

                    if ret.is_err() {
                        // The error is considered to have already been reported.
                        stack.clear();
                        break;
                    }

                    self.schema
                        .type_defs
                        .insert(key, TypeDefinition::Complex(new.into()));
                } else if stack.iter().any(|e| e.0 == base) {
                    // [ct-props-correct] clause 3
                    let msg = format!(
                        "A circluar definition is found in the complex type '{}'.",
                        value.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::CtPropsCorrect),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    stack.clear();
                    break;
                } else {
                    let base_ns = base.0.map(|ns| format!("{{{ns}}}")).unwrap_or_default();
                    let msg = format!(
                        "The complex type '{}' is derived from '{}{}', but '{}{}' is not found in this schema.",
                        value.display_name(),
                        base_ns,
                        base.1,
                        base_ns,
                        base.1
                    );
                    // [ct-props-correct] clause 3 ?
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::CtPropsCorrect),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    stack.clear();
                    break;
                }
            }
        }
        Ok(())
    }

    /// Expand attributeGroup, derive attributes from base type and resolve the attribute wildcard.
    fn compute_attributes_of_complex_type(
        &mut self,
        new: &mut ComplexTypeDefinition,
        base: &TypeDefinition,
    ) -> Result<(), XMLError> {
        let mut wildcards = vec![];
        while let Some(gref) = new.attribute_group_refs.pop() {
            let Some(attgroup) = self.schema.attgroups.get(&gref) else {
                // Since the verification cannot continue,
                // it will be reported as an error and terminated.
                let msg = format!(
                    "The attribute group '{}{}' is not found in schema.",
                    gref.0
                        .as_deref()
                        .map(|ns| format!("{{{ns}}}"))
                        .unwrap_or_default(),
                    gref.1
                );
                self.holder.error(SAXParseError {
                    error: XMLError::XSDError(XSDError::MissingSubComponent),
                    level: XMLErrorLevel::Error,
                    domain: XMLErrorDomain::XSDParser,
                    line: self.locator.line(),
                    column: self.locator.column(),
                    system_id: self.locator.system_id(),
                    public_id: self.locator.public_id(),
                    message: Cow::Owned(msg),
                });
                return Err(XMLError::XSDError(XSDError::MissingSubComponent));
            };
            new.attribute_uses
                .extend(attgroup.attribute_uses.iter().cloned());
            if let Some(wc) = attgroup.attribute_wildcard.as_ref() {
                wildcards.push(wc.clone());
            }
            // At this point, it is assumed that all attribute groups have been expanded.
            assert!(attgroup.attribute_group_refs.is_empty());
        }

        // {attribute uses} of complexType clause 3
        let prohibited = new
            .attribute_uses
            .extract_if(.., |au| matches!(au.required, Occurrence::Prohibited))
            .filter_map(|au| self.schema.att_decls.get(&au.attribute_declaration))
            .map(|att| (att.target_namespace.clone(), att.name.clone()))
            .collect::<HashSet<_>>();
        if let Some(ct) = base.as_complex_type() {
            match new.derivation_method {
                DerivationMethod::Extension => {
                    // In this case, clause 3.1 and 3.2 is not applied.
                    // Therefore, all attribute uses are taken over.
                    new.attribute_uses.extend_from_slice(&ct.attribute_uses);
                }
                DerivationMethod::Restriction => {
                    for attuse in &ct.attribute_uses {
                        let Some(att) = self.schema.att_decls.get(&attuse.attribute_declaration)
                        else {
                            // If the base {attribute uses} contains an unresolvable attribute use,
                            // an error should already have been reported, so no error is raised here.
                            continue;
                        };
                        if new
                            .attribute_uses
                            .iter()
                            .filter_map(|au| self.schema.att_decls.get(&au.attribute_declaration))
                            .any(|a| {
                                att.target_namespace == a.target_namespace && att.name == a.name
                            })
                        {
                            // clause 3.1
                            continue;
                        }
                        if prohibited.contains(&(att.target_namespace.clone(), att.name.clone())) {
                            // clause 3.2
                            continue;
                        }
                        new.attribute_uses.push(attuse.clone());
                    }
                }
            }
        }

        // {attribute wildcard} of complexType clause 3
        let complete_wildcard =
            attribute_complete_wildcard(&wildcards, new.attribute_wildcard.take())?;
        if matches!(new.derivation_method, DerivationMethod::Restriction) {
            // clause 3.1
            new.attribute_wildcard = complete_wildcard;
        } else {
            // clause 3.2
            let base = new.base_type_definition.clone();
            if let Some(ct) = self
                .schema
                .type_defs
                .get(&base)
                .and_then(|t| t.as_complex_type())
                && let Some(base_wildcard) = ct.attribute_wildcard.as_ref()
            {
                if let Some(complete_wildcard) = complete_wildcard {
                    // clause 3.2.2.1.2
                    new.attribute_wildcard = complete_wildcard.union(base_wildcard.clone());
                } else {
                    // clause 3.2.2.1.1
                    new.attribute_wildcard = Some(base_wildcard.clone());
                }
            } else {
                // clause 3.2.2.2
                new.attribute_wildcard = complete_wildcard;
            }
        }
        Ok(())
    }

    /// Finalize the content type that could not be resolved during component creation.
    fn determine_content_type_of_complex_type(
        &mut self,
        new: &mut ComplexTypeDefinition,
        base: &TypeDefinition,
    ) -> Result<(), XMLError> {
        match &mut new.content_type {
            ContentType::Simple(ns, name) => {
                // In the case of <restriction>, since it has already been finalized,
                // there is nothing to do.
                if matches!(new.derivation_method, DerivationMethod::Extension) {
                    assert!(ns.is_none());
                    assert!(name.is_empty());

                    // In this case, <simpleContent> alternative is selected.
                    match base {
                        TypeDefinition::Simple(st) => {
                            // {content type} of complexType clause 4
                            *ns = st.target_namespace.clone();
                            *name = st.name.clone();
                        }
                        TypeDefinition::Complex(ct) => {
                            // {content type} of complexType clause 3
                            if !matches!(ct.content_type, ContentType::Simple(_, _)) {
                                // [src-ct] clause 2.1, 2.2.1, 2.1.3
                                let msg = format!(
                                    "<simpleContent> and <extension> is chosen for '{}' and its base type is complex, but the content type of the base type '{}' is not simple.",
                                    new.display_name(),
                                    ct.display_name()
                                );
                                self.holder.error(SAXParseError {
                                    error: XMLError::XSDError(XSDError::SrcCt),
                                    level: XMLErrorLevel::Error,
                                    domain: XMLErrorDomain::XSDParser,
                                    line: self.locator.line(),
                                    column: self.locator.column(),
                                    system_id: self.locator.system_id(),
                                    public_id: self.locator.public_id(),
                                    message: Cow::Owned(msg),
                                });
                                return Err(XMLError::XSDError(XSDError::SrcCt));
                            }
                            new.content_type = ct.content_type.clone();
                        }
                    }
                }
            }
            ContentType::Empty | ContentType::ElementOnly(_) | ContentType::Mixed(_) => {}
            ContentType::Pending(pt, mixed) => {
                match new.derivation_method {
                    DerivationMethod::Extension => {
                        let Some(ct) = base.as_complex_type() else {
                            let msg = format!(
                                "The base type resolved by the 'base' attribute of <complexContent> for '{}' must be a complex type, but '{}' is not.",
                                new.display_name(),
                                base.display_name()
                            );
                            self.holder.error(SAXParseError {
                                error: XMLError::XSDError(XSDError::SrcCt),
                                level: XMLErrorLevel::Error,
                                domain: XMLErrorDomain::XSDParser,
                                line: self.locator.line(),
                                column: self.locator.column(),
                                system_id: self.locator.system_id(),
                                public_id: self.locator.public_id(),
                                message: Cow::Owned(msg),
                            });
                            return Err(XMLError::XSDError(XSDError::SrcCt));
                        };
                        // {content type} of complexType clause 3.2 (<complexContent> alternative)
                        if let Some(effective_content) = pt.take() {
                            match &ct.content_type {
                                ContentType::Empty => {
                                    // clause 3.2.2
                                    new.content_type = if *mixed {
                                        ContentType::Mixed(effective_content)
                                    } else {
                                        ContentType::ElementOnly(effective_content)
                                    }
                                }
                                ContentType::ElementOnly(bpt) | ContentType::Mixed(bpt) => {
                                    // clause 3.2.3
                                    let pt = Particle {
                                        min_occurs: 1,
                                        max_occurs: 1,
                                        term: Term::ModelGroup(ModelGroup {
                                            compositor: Compositor::Sequence,
                                            particles: vec![bpt.clone(), effective_content],
                                        }),
                                    };
                                    new.content_type = if *mixed {
                                        ContentType::Mixed(pt)
                                    } else {
                                        ContentType::ElementOnly(pt)
                                    }
                                }
                                _ => {
                                    // The case that {content type} is a simple type.
                                    // Although not explicitly stated, according to the provisions
                                    // of [cos-ct-extends] clause 1.4, a combination where the
                                    // {content type} of the base type is a simple type and the
                                    // {content type} of the type itself is anything other than a
                                    // simple type is not permitted.
                                    //
                                    // [src-ct] clause 3
                                    // [cos-ct-extends] clause 1.4
                                    let msg = format!(
                                        "The content type of the base complex type '{}' is a simple type, but that of extended complex type '{}' is not.",
                                        ct.display_name(),
                                        new.display_name()
                                    );
                                    self.holder.error(SAXParseError {
                                        error: XMLError::XSDError(XSDError::CosCtExtends),
                                        level: XMLErrorLevel::Error,
                                        domain: XMLErrorDomain::XSDParser,
                                        line: self.locator.line(),
                                        column: self.locator.column(),
                                        system_id: self.locator.system_id(),
                                        public_id: self.locator.public_id(),
                                        message: Cow::Owned(msg),
                                    });
                                    return Err(XMLError::XSDError(XSDError::CosCtExtends));
                                }
                            }
                        } else {
                            // clause 3.2.1
                            new.content_type = ct.content_type.clone();
                        }
                    }
                    DerivationMethod::Restriction => {
                        // {content type} of complexType clause 3.1
                        if let Some(effective_content) = pt.take() {
                            // clause 3.1.2
                            if *mixed {
                                new.content_type = ContentType::Mixed(effective_content);
                            } else {
                                new.content_type = ContentType::ElementOnly(effective_content);
                            }
                        } else {
                            // clause 3.1.1
                            new.content_type = ContentType::Empty;
                        }
                    }
                }
            }
        }
        Ok(())
    }

    /// Check a part of "ct-props-correct" related to {attribute uses}. (clause 4 and 5)
    ///
    /// # Reference
    /// - [ct-props-correct](https://www.w3.org/TR/xmlschema-1/#ct-props-correct)
    fn check_ct_props_correct_4_5(
        &mut self,
        new: &mut ComplexTypeDefinition,
    ) -> Result<(), XMLError> {
        assert!(new.attribute_group_refs.is_empty());
        let mut used = HashSet::new();
        let mut ret = Ok(());
        let mut idcnt = 0;
        for attuse in &new.attribute_uses {
            if !used.insert(attuse.attribute_declaration.clone()) {
                // [ct-props-correct] clause 4
                let msg = format!(
                    "The attribute '{}' for the complex type '{}' is duplicated.",
                    attuse.display_name(),
                    new.display_name()
                );
                self.holder.error(SAXParseError {
                    error: XMLError::XSDError(XSDError::CtPropsCorrect),
                    level: XMLErrorLevel::Error,
                    domain: XMLErrorDomain::XSDParser,
                    line: self.locator.line(),
                    column: self.locator.column(),
                    system_id: self.locator.system_id(),
                    public_id: self.locator.public_id(),
                    message: Cow::Owned(msg),
                });
                ret = Err(XMLError::XSDError(XSDError::CtPropsCorrect));
            } else if idcnt <= 1 {
                // [ct-props-correct] clause 5
                // It is unclear whether derivations by list or union are also prohibited.
                // While the constraint on list seems unnatural, the one on union seems natural,
                // so I think derivations using both list and union are likely prohibited.
                let Some(att) = self.schema.att_decls.get(&attuse.attribute_declaration) else {
                    let msg = format!(
                        "The attribute declaration '{}{}' is not found in schema.",
                        attuse
                            .attribute_declaration
                            .0
                            .as_deref()
                            .map(|ns| format!("{{{ns}}}"))
                            .unwrap_or_default(),
                        attuse.attribute_declaration.1
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::MissingSubComponent),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    return Err(XMLError::XSDError(XSDError::MissingSubComponent));
                };
                let Some(t) = self
                    .schema
                    .type_defs
                    .get(&att.type_definition)
                    .and_then(|t| t.as_simple_type())
                else {
                    let msg = format!(
                        "The simple type '{}{}' is not found in schema.",
                        att.type_definition
                            .0
                            .as_deref()
                            .map(|ns| format!("{{{ns}}}"))
                            .unwrap_or_default(),
                        att.type_definition.1
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::MissingSubComponent),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    return Err(XMLError::XSDError(XSDError::MissingSubComponent));
                };
                if t.is_derived_from(Some(XML_SCHEMA_NAMESPACE), "ID", &self.schema.type_defs) {
                    if idcnt == 1 {
                        let msg = format!(
                            "The complex type '{}' has multiple types which are or are derived from ID.",
                            new.display_name()
                        );
                        self.holder.error(SAXParseError {
                            error: XMLError::XSDError(XSDError::CtPropsCorrect),
                            level: XMLErrorLevel::Error,
                            domain: XMLErrorDomain::XSDParser,
                            line: self.locator.line(),
                            column: self.locator.column(),
                            system_id: self.locator.system_id(),
                            public_id: self.locator.public_id(),
                            message: Cow::Owned(msg),
                        });
                        ret = Err(XMLError::XSDError(XSDError::CtPropsCorrect));
                    }
                    idcnt += 1;
                }
            }
        }
        ret
    }

    /// # Reference
    /// - [derivation-ok-restriction](https://www.w3.org/TR/xmlschema-1/#derivation-ok-restriction)
    fn check_derivation_ok_restriction(
        &mut self,
        new: &mut ComplexTypeDefinition,
        base: &TypeDefinition,
    ) -> Result<(), XMLError> {
        let Some(base) = base.as_complex_type() else {
            // [derivation-ok-restriction] clause 1
            let msg = format!(
                "The base type '{}' of the complex type '{}' is not complex type.",
                base.display_name(),
                new.display_name()
            );
            self.holder.error(SAXParseError {
                error: XMLError::XSDError(XSDError::DerivationOkRestriction),
                level: XMLErrorLevel::Error,
                domain: XMLErrorDomain::XSDParser,
                line: self.locator.line(),
                column: self.locator.column(),
                system_id: self.locator.system_id(),
                public_id: self.locator.public_id(),
                message: Cow::Owned(msg),
            });
            return Err(XMLError::XSDError(XSDError::DerivationOkRestriction));
        };
        if base.r#final.restriction() {
            // [derivation-ok-restriction] clause 1
            let msg = format!(
                "'{}' cannot be derived from '{}' because 'restriction' is prohibited.",
                new.display_name(),
                base.display_name(),
            );
            self.holder.error(SAXParseError {
                error: XMLError::XSDError(XSDError::DerivationOkRestriction),
                level: XMLErrorLevel::Error,
                domain: XMLErrorDomain::XSDParser,
                line: self.locator.line(),
                column: self.locator.column(),
                system_id: self.locator.system_id(),
                public_id: self.locator.public_id(),
                message: Cow::Owned(msg),
            });
            return Err(XMLError::XSDError(XSDError::DerivationOkRestriction));
        }

        let mut ret = Ok(());
        for attuse in &new.attribute_uses {
            let Some(att) = self.schema.att_decls.get(&attuse.attribute_declaration) else {
                let msg = format!(
                    "The attribute declaration '{}' for the complex type '{}' is not found in schema.",
                    attuse.display_name(),
                    new.display_name()
                );
                self.holder.error(SAXParseError {
                    error: XMLError::XSDError(XSDError::MissingSubComponent),
                    level: XMLErrorLevel::Error,
                    domain: XMLErrorDomain::XSDParser,
                    line: self.locator.line(),
                    column: self.locator.column(),
                    system_id: self.locator.system_id(),
                    public_id: self.locator.public_id(),
                    message: Cow::Owned(msg),
                });
                ret = Err(XMLError::XSDError(XSDError::MissingSubComponent));
                continue;
            };
            let Some(ty) = self
                .schema
                .type_defs
                .get(&att.type_definition)
                .and_then(|t| t.as_simple_type())
            else {
                continue;
            };
            if let Some((batt, battuse)) = base.attribute_uses.iter().find_map(|a| {
                self.schema
                    .att_decls
                    .get(&a.attribute_declaration)
                    .filter(|batt| batt.target_namespace == att.target_namespace)
                    .filter(|batt| batt.name == att.name)
                    .zip(Some(a))
            }) {
                // [derivation-ok-restriction] clause 2.1

                if matches!(attuse.required, Occurrence::Optional)
                    && matches!(battuse.required, Occurrence::Required)
                {
                    // clause 2.1.1
                    let msg = format!(
                        "The attribute '{}' cannot be optional for '{}' because it is required for the base type '{}'.",
                        att.display_name(),
                        new.display_name(),
                        base.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::DerivationOkRestriction),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    ret = Err(XMLError::XSDError(XSDError::DerivationOkRestriction));
                    continue;
                }

                if let Some(bty) = self
                    .schema
                    .type_defs
                    .get(&batt.type_definition)
                    .and_then(|t| t.as_simple_type())
                    && self
                        .check_cos_st_derived_ok(&ty, &bty, FinalSet::default())
                        .is_err()
                {
                    // clause 2.1.2
                    let msg = format!(
                        "The simple type '{}' for the attribute '{}' is not validty derived from '{}' for the base type attribute '{}'.",
                        ty.display_name(),
                        att.display_name(),
                        bty.display_name(),
                        batt.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::DerivationOkRestriction),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    ret = Err(XMLError::XSDError(XSDError::DerivationOkRestriction));
                    continue;
                }

                match (
                    att.value_constraint.as_ref(),
                    batt.value_constraint.as_ref(),
                ) {
                    (Some(ValueConstraint::Default(_)), Some(ValueConstraint::Fixed(_)))
                    | (None, Some(ValueConstraint::Fixed(_))) => {
                        // clause 2.1.3.2
                        let msg = format!(
                            "The attribute '{}' has fixed default value for the base type '{}', but '{}' has not.",
                            att.display_name(),
                            base.display_name(),
                            new.display_name()
                        );
                        self.holder.error(SAXParseError {
                            error: XMLError::XSDError(XSDError::DerivationOkRestriction),
                            level: XMLErrorLevel::Error,
                            domain: XMLErrorDomain::XSDParser,
                            line: self.locator.line(),
                            column: self.locator.column(),
                            system_id: self.locator.system_id(),
                            public_id: self.locator.public_id(),
                            message: Cow::Owned(msg),
                        });
                        ret = Err(XMLError::XSDError(XSDError::DerivationOkRestriction));
                        continue;
                    }
                    (Some(ValueConstraint::Fixed(v)), Some(ValueConstraint::Fixed(bv))) => {
                        // clause 2.1.3.2
                        if v != bv {
                            let msg = format!(
                                "The fixed value of the attribute '{}' for the base type '{}' is '{}', but that for '{}' is '{}'.",
                                att.display_name(),
                                base.display_name(),
                                bv,
                                new.display_name(),
                                v,
                            );
                            self.holder.error(SAXParseError {
                                error: XMLError::XSDError(XSDError::DerivationOkRestriction),
                                level: XMLErrorLevel::Error,
                                domain: XMLErrorDomain::XSDParser,
                                line: self.locator.line(),
                                column: self.locator.column(),
                                system_id: self.locator.system_id(),
                                public_id: self.locator.public_id(),
                                message: Cow::Owned(msg),
                            });
                            ret = Err(XMLError::XSDError(XSDError::DerivationOkRestriction));
                            continue;
                        }
                    }
                    (_, Some(ValueConstraint::Default(_))) | (_, None) => {
                        // clause 2.1.3.1
                    }
                }
            } else {
                // [derivation-ok-restriction] clause 2.2
                if base
                    .attribute_wildcard
                    .as_ref()
                    .is_none_or(|wildcard| !wildcard.is_match(att.target_namespace.as_deref()))
                {
                    let msg = format!(
                        "The attribute '{}' for the complex type '{}' is not validly restricted.",
                        att.display_name(),
                        new.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::DerivationOkRestriction),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    ret = Err(XMLError::XSDError(XSDError::DerivationOkRestriction));
                }
            }
        }
        for battuse in base
            .attribute_uses
            .iter()
            .filter(|attuse| matches!(attuse.required, Occurrence::Required))
        {
            if let Some(batt) = self.schema.att_decls.get(&battuse.attribute_declaration)
                && !new.attribute_uses.iter().any(|attuse| {
                    matches!(attuse.required, Occurrence::Required)
                        && self
                            .schema
                            .att_decls
                            .get(&attuse.attribute_declaration)
                            .is_some_and(|att| {
                                att.target_namespace == batt.target_namespace
                                    && att.name == batt.name
                            })
                })
            {
                // [derivation-ok-restriction] clause 3
                let msg = format!(
                    "The attribute '{}' for the complex type '{}' is required, but is not.",
                    batt.display_name(),
                    new.display_name()
                );
                self.holder.error(SAXParseError {
                    error: XMLError::XSDError(XSDError::DerivationOkRestriction),
                    level: XMLErrorLevel::Error,
                    domain: XMLErrorDomain::XSDParser,
                    line: self.locator.line(),
                    column: self.locator.column(),
                    system_id: self.locator.system_id(),
                    public_id: self.locator.public_id(),
                    message: Cow::Owned(msg),
                });
                ret = Err(XMLError::XSDError(XSDError::DerivationOkRestriction));
            }
        }

        if let Some(wc) = new.attribute_wildcard.as_ref() {
            // [derivation-ok-restriction] clause 4
            if let Some(bwc) = base.attribute_wildcard.as_ref() {
                if !wc.is_subset_of(bwc) {
                    // clause 4.2
                    let msg = format!(
                        "The attribute wildcard of the complex type '{}' is not subset of that of the base type '{}'.",
                        new.display_name(),
                        base.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::DerivationOkRestriction),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    ret = Err(XMLError::XSDError(XSDError::DerivationOkRestriction));
                } else if !base.is_ur_type() && wc.process_contents < bwc.process_contents {
                    // clause 4.3
                    let msg = format!(
                        "The process content of the attribute wildcard of the complex type '{}' is weak than that of the base type '{}'.",
                        new.display_name(),
                        base.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::DerivationOkRestriction),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    ret = Err(XMLError::XSDError(XSDError::DerivationOkRestriction));
                }
            } else {
                // clause 4.1
                let msg = format!(
                    "The complex type '{}' has an attribute wildcard, but the base type '{}' does not.",
                    new.display_name(),
                    base.display_name()
                );
                self.holder.error(SAXParseError {
                    error: XMLError::XSDError(XSDError::DerivationOkRestriction),
                    level: XMLErrorLevel::Error,
                    domain: XMLErrorDomain::XSDParser,
                    line: self.locator.line(),
                    column: self.locator.column(),
                    system_id: self.locator.system_id(),
                    public_id: self.locator.public_id(),
                    message: Cow::Owned(msg),
                });
                ret = Err(XMLError::XSDError(XSDError::DerivationOkRestriction));
            }
        }

        // clause 5
        if base.is_ur_type() {
            // clause 5.1
            return ret;
        }

        match &new.content_type {
            ContentType::Simple(ns, name) => {
                // clause 5.2
                match &base.content_type {
                    ContentType::Simple(bns, bname) => {
                        // clause 5.2.2.1
                        let Some(ty) = self
                            .schema
                            .type_defs
                            .get(&(ns.clone(), name.clone()))
                            .and_then(|t| t.as_simple_type())
                        else {
                            let msg = format!(
                                "The simple type '{}{}' is not found in schema.",
                                ns.as_deref()
                                    .map(|ns| format!("{{{ns}}}"))
                                    .unwrap_or_default(),
                                name
                            );
                            self.holder.error(SAXParseError {
                                error: XMLError::XSDError(XSDError::DerivationOkRestriction),
                                level: XMLErrorLevel::Error,
                                domain: XMLErrorDomain::XSDParser,
                                line: self.locator.line(),
                                column: self.locator.column(),
                                system_id: self.locator.system_id(),
                                public_id: self.locator.public_id(),
                                message: Cow::Owned(msg),
                            });
                            return Err(XMLError::XSDError(XSDError::DerivationOkRestriction));
                        };
                        let Some(bty) = self
                            .schema
                            .type_defs
                            .get(&(bns.clone(), bname.clone()))
                            .and_then(|t| t.as_simple_type())
                        else {
                            let msg = format!(
                                "The simple type '{}{}' is not found in schema.",
                                bns.as_deref()
                                    .map(|ns| format!("{{{ns}}}"))
                                    .unwrap_or_default(),
                                bname
                            );
                            self.holder.error(SAXParseError {
                                error: XMLError::XSDError(XSDError::DerivationOkRestriction),
                                level: XMLErrorLevel::Error,
                                domain: XMLErrorDomain::XSDParser,
                                line: self.locator.line(),
                                column: self.locator.column(),
                                system_id: self.locator.system_id(),
                                public_id: self.locator.public_id(),
                                message: Cow::Owned(msg),
                            });
                            return Err(XMLError::XSDError(XSDError::DerivationOkRestriction));
                        };

                        if self
                            .check_cos_st_derived_ok(&ty, &bty, FinalSet::default())
                            .is_err()
                        {
                            let msg = format!(
                                "The simple content type '{}' for the complex type '{}' is not validly derived from that of the base type '{}'.",
                                ty.display_name(),
                                new.display_name(),
                                base.display_name()
                            );
                            self.holder.error(SAXParseError {
                                error: XMLError::XSDError(XSDError::DerivationOkRestriction),
                                level: XMLErrorLevel::Error,
                                domain: XMLErrorDomain::XSDParser,
                                line: self.locator.line(),
                                column: self.locator.column(),
                                system_id: self.locator.system_id(),
                                public_id: self.locator.public_id(),
                                message: Cow::Owned(msg),
                            });
                            ret = Err(XMLError::XSDError(XSDError::DerivationOkRestriction));
                        }
                    }
                    ContentType::Mixed(pt) if pt.is_emptiable(&self.schema.modelgroups) => {
                        // clause 5.2.2.2
                        // no op
                    }
                    _ => {
                        // clause 5.2.2
                        let msg = format!(
                            "The content of the complext type '{}' is a simple type, but that of the base type '{}' is neither a simple type nor a mixed emptiable particle.",
                            new.display_name(),
                            base.display_name()
                        );
                        self.holder.error(SAXParseError {
                            error: XMLError::XSDError(XSDError::DerivationOkRestriction),
                            level: XMLErrorLevel::Error,
                            domain: XMLErrorDomain::XSDParser,
                            line: self.locator.line(),
                            column: self.locator.column(),
                            system_id: self.locator.system_id(),
                            public_id: self.locator.public_id(),
                            message: Cow::Owned(msg),
                        });
                        ret = Err(XMLError::XSDError(XSDError::DerivationOkRestriction));
                    }
                }
            }
            ContentType::Empty => {
                // clause 5.3
                match &base.content_type {
                    ContentType::Empty => {
                        // clause 5.3.2.1
                    }
                    ContentType::ElementOnly(pt) | ContentType::Mixed(pt)
                        if pt.is_emptiable(&self.schema.modelgroups) =>
                    {
                        // clause 5.3.2.2
                    }
                    _ => {
                        let msg = format!(
                            "The content of the complext type '{}' is empty, but that of the base type '{}' is neither empty nor an emptiable particle.",
                            new.display_name(),
                            base.display_name()
                        );
                        self.holder.error(SAXParseError {
                            error: XMLError::XSDError(XSDError::DerivationOkRestriction),
                            level: XMLErrorLevel::Error,
                            domain: XMLErrorDomain::XSDParser,
                            line: self.locator.line(),
                            column: self.locator.column(),
                            system_id: self.locator.system_id(),
                            public_id: self.locator.public_id(),
                            message: Cow::Owned(msg),
                        });
                        ret = Err(XMLError::XSDError(XSDError::DerivationOkRestriction));
                    }
                }
            }
            ContentType::ElementOnly(pt) | ContentType::Mixed(pt) => {
                // clause 5.4
                if matches!(new.content_type, ContentType::Mixed(_))
                    && !matches!(base.content_type, ContentType::Mixed(_))
                {
                    // clause 5.4.1
                    let msg = format!(
                        "The complex type '{}' whose content type is mixed cannot be derived from '{}' whose content type is not mixed.",
                        new.display_name(),
                        base.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::DerivationOkRestriction),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    return Err(XMLError::XSDError(XSDError::DerivationOkRestriction));
                }
                let valid_restricted = match &base.content_type {
                    ContentType::ElementOnly(bpt) | ContentType::Mixed(bpt) => {
                        // clause 5.4.2
                        self.is_valid_restriction(pt, bpt)
                    }
                    ContentType::Empty => {
                        // clause 5.4.2 Note
                        todo!("check all particles are pointless")
                    }
                    _ => {
                        // It's unclear how to handle cases where the type is simple,
                        // but since there's no specific mention of it, treat it as an error.
                        false
                    }
                };
                if !valid_restricted {
                    let msg = format!(
                        "The content type of the complex type '{}' is not a valid restriction of that of the base type '{}'.",
                        new.display_name(),
                        base.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::DerivationOkRestriction),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    ret = Err(XMLError::XSDError(XSDError::DerivationOkRestriction));
                }
            }
            _ => unreachable!(),
        }
        ret
    }

    /// # Reference
    /// - [cos-ct-extends](https://www.w3.org/TR/xmlschema-1/#cos-ct-extends)
    fn check_cos_ct_extends(
        &mut self,
        new: &mut ComplexTypeDefinition,
        base: &TypeDefinition,
    ) -> Result<(), XMLError> {
        match base {
            TypeDefinition::Simple(base) => {
                // [cos-ct-extends] clause 2

                if base.r#final.extension() {
                    // [cos-ct-extends] clause 2.2
                    let msg = format!(
                        "'{}' cannot be derived from '{}' because 'extension' is prohibited.",
                        new.display_name(),
                        base.display_name(),
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::CosCtExtends),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    return Err(XMLError::XSDError(XSDError::CosCtExtends));
                }

                if let ContentType::Simple(ns, name) = &new.content_type
                    && let Some(t) = self.schema.type_defs.get(&(ns.clone(), name.clone()))
                    && let Some(st) = t.as_simple_type()
                    && st.target_namespace == base.target_namespace
                    && st.name == base.name
                {
                    // no op
                } else {
                    // [cos-ct-extends] clause 2.1
                    let msg = format!(
                        "The content simple type of the complex type '{}' is not same as that of the base type '{}'.",
                        new.display_name(),
                        base.display_name(),
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::CosCtExtends),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    return Err(XMLError::XSDError(XSDError::CosCtExtends));
                }
            }
            TypeDefinition::Complex(base) => {
                // [cos-ct-extends] clause 1

                if base.r#final.extension() {
                    // [cos-ct-extends] clause 1.1
                    let msg = format!(
                        "'{}' cannot be derived from '{}' because 'restriction' is prohibited.",
                        new.display_name(),
                        base.display_name(),
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::CosCtExtends),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    return Err(XMLError::XSDError(XSDError::CosCtExtends));
                }

                // [cos-ct-extends] clause 1.2
                for battuse in &base.attribute_uses {
                    let Some(batt) = self.schema.att_decls.get(&battuse.attribute_declaration)
                    else {
                        continue;
                    };
                    if !new
                        .attribute_uses
                        .iter()
                        .filter_map(|au| self.schema.att_decls.get(&au.attribute_declaration))
                        .any(|att| {
                            batt.target_namespace == att.target_namespace && batt.name == att.name
                        })
                    {
                        let msg = format!(
                            "The attribute uses of the complex type '{}' is not a subset of that of the base type '{}'.",
                            new.display_name(),
                            base.display_name()
                        );
                        self.holder.error(SAXParseError {
                            error: XMLError::XSDError(XSDError::CosCtExtends),
                            level: XMLErrorLevel::Error,
                            domain: XMLErrorDomain::XSDParser,
                            line: self.locator.line(),
                            column: self.locator.column(),
                            system_id: self.locator.system_id(),
                            public_id: self.locator.public_id(),
                            message: Cow::Owned(msg),
                        });
                        return Err(XMLError::XSDError(XSDError::CosCtExtends));
                    }
                }

                if let Some(bwc) = base.attribute_wildcard.as_ref() {
                    // [cos-ct-extends] clause 1.3
                    if let Some(wc) = new.attribute_wildcard.as_ref() {
                        if !bwc.is_subset_of(wc) {
                            let msg = format!(
                                "The attribute wildcard of the base complex type '{}' is not a subset of the extended complex type '{}'.",
                                base.display_name(),
                                new.display_name()
                            );
                            self.holder.error(SAXParseError {
                                error: XMLError::XSDError(XSDError::CosCtExtends),
                                level: XMLErrorLevel::Error,
                                domain: XMLErrorDomain::XSDParser,
                                line: self.locator.line(),
                                column: self.locator.column(),
                                system_id: self.locator.system_id(),
                                public_id: self.locator.public_id(),
                                message: Cow::Owned(msg),
                            });
                            return Err(XMLError::XSDError(XSDError::CosCtExtends));
                        }
                    } else {
                        let msg = format!(
                            "The base complex type '{}' has an attribute wildcard, but extended complex type '{}' does not.",
                            base.display_name(),
                            new.display_name()
                        );
                        self.holder.error(SAXParseError {
                            error: XMLError::XSDError(XSDError::CosCtExtends),
                            level: XMLErrorLevel::Error,
                            domain: XMLErrorDomain::XSDParser,
                            line: self.locator.line(),
                            column: self.locator.column(),
                            system_id: self.locator.system_id(),
                            public_id: self.locator.public_id(),
                            message: Cow::Owned(msg),
                        });
                        return Err(XMLError::XSDError(XSDError::CosCtExtends));
                    }
                }

                // [cos-ct-extends] clause 1.4
                match (&new.content_type, &base.content_type) {
                    (ContentType::Simple(ns, name), ContentType::Simple(bns, bname))
                        if ns == bns && name == bname =>
                    {
                        // [cos-ct-extends] clause 1.4.1
                    }
                    (ContentType::Empty, ContentType::Empty) => {
                        // [cos-ct-extends] clause 1.4.2
                    }
                    (ContentType::ElementOnly(_) | ContentType::Mixed(_), ContentType::Empty) => {
                        // [cos-ct-extends] clause 1.4.3.1 and 1.4.3.2.1
                    }
                    (ContentType::ElementOnly(pt), ContentType::ElementOnly(bpt))
                    | (ContentType::Mixed(pt), ContentType::Mixed(bpt))
                        if pt.is_valid_extension_of(bpt, &self.schema) =>
                    {
                        // [cos-ct-extends] clause 1.4.3.1 and 1.4.3.2.2
                    }
                    _ => {
                        let msg = format!(
                            "The content type of the complex type '{}' is not a valid extension of the base type '{}'.",
                            new.display_name(),
                            base.display_name(),
                        );
                        self.holder.error(SAXParseError {
                            error: XMLError::XSDError(XSDError::CosCtExtends),
                            level: XMLErrorLevel::Error,
                            domain: XMLErrorDomain::XSDParser,
                            line: self.locator.line(),
                            column: self.locator.column(),
                            system_id: self.locator.system_id(),
                            public_id: self.locator.public_id(),
                            message: Cow::Owned(msg),
                        });
                        return Err(XMLError::XSDError(XSDError::CosCtExtends));
                    }
                }
            }
        }
        Ok(())
    }

    /// # Reference
    /// - [cos-ct-derived-ok](https://www.w3.org/TR/xmlschema-1/#cos-ct-derived-ok)
    fn check_cos_ct_derived_ok(
        &self,
        d: &ComplexTypeDefinition,
        b: &TypeDefinition,
        subset: FinalSet,
    ) -> Result<(), XMLError> {
        if d.target_namespace == b.target_namespace() && d.name == b.name() {
            // [cos-ct-derived-ok] clause 2.1
            return Ok(());
        }

        if (matches!(d.derivation_method, DerivationMethod::Restriction) && subset.restriction())
            || (matches!(d.derivation_method, DerivationMethod::Extension) && subset.extension())
        {
            // [cos-ct-derived-ok] clause 1
            return Err(XMLError::XSDError(XSDError::CosCtDerivedOk));
        }

        if d.base_type_definition == (b.target_namespace(), b.name()) {
            // [cos-ct-derived-ok] clause 2.2
            return Ok(());
        }

        // [cos-ct-derived-ok] clause 2.3

        if d.base_type_definition == XSD_ANY_TYPE.base_type_definition {
            // [cos-ct-derived-ok] clause 2.3.1
            return Err(XMLError::XSDError(XSDError::CosCtDerivedOk));
        }

        let ds_base = self
            .schema
            .type_defs
            .get(&d.base_type_definition)
            .ok_or(XSDError::MissingSubComponent)?;
        match ds_base {
            TypeDefinition::Complex(ds_base) => {
                // [cos-ct-derived-ok] clause 2.3.2.1
                self.check_cos_ct_derived_ok(ds_base, b, subset)
            }
            TypeDefinition::Simple(ds_base) => {
                // [cos-ct-derived-ok] clause 2.3.2.2
                let st = b.as_simple_type().ok_or(XSDError::CosCtDerivedOk)?;
                self.check_cos_st_derived_ok(ds_base, &st, subset)
            }
        }
    }

    /// # Reference
    /// - [a-props-correct](https://www.w3.org/TR/xmlschema-1/#a-props-correct)
    fn check_all_attdecl(&mut self) -> Result<(), XMLError> {
        let decls = std::mem::take(&mut self.schema.att_decls);
        let mut ret = Ok(());
        for (key, mut decl) in decls {
            let t = decl.type_definition.clone();
            let Some(t) = self
                .schema
                .type_defs
                .get(&t)
                .and_then(|t| t.as_simple_type())
            else {
                let msg = format!(
                    "The simple type '{}{}' specified for the attribute declaration '{}' is not found in schema.",
                    t.0.as_deref()
                        .map(|ns| format!("{{{ns}}}"))
                        .unwrap_or_default(),
                    t.1,
                    decl.display_name()
                );
                self.holder.error(SAXParseError {
                    error: XMLError::XSDError(XSDError::MissingSubComponent),
                    level: XMLErrorLevel::Error,
                    domain: XMLErrorDomain::XSDParser,
                    line: self.locator.line(),
                    column: self.locator.column(),
                    system_id: self.locator.system_id(),
                    public_id: self.locator.public_id(),
                    message: Cow::Owned(msg),
                });
                ret = Err(XMLError::XSDError(XSDError::MissingSubComponent));
                continue;
            };

            if decl.name.as_ref() == "xmlns" {
                // [no-xmlns]
                self.holder.error(SAXParseError {
                    error: XMLError::XSDError(XSDError::NoXmlns),
                    level: XMLErrorLevel::Error,
                    domain: XMLErrorDomain::XSDParser,
                    line: self.locator.line(),
                    column: self.locator.column(),
                    system_id: self.locator.system_id(),
                    public_id: self.locator.public_id(),
                    message: Cow::Borrowed("The attribute declaration must not be named 'xmlns'."),
                });
                ret = Err(XMLError::XSDError(XSDError::NoXmlns));
                continue;
            } else if decl.target_namespace.as_deref() == Some(XML_SCHEMA_INSTANCE_NAMESPACE) {
                // [no-xsi]
                let msg = format!(
                    "The attribute declaration must not have the target namespace named '{}'.",
                    XML_SCHEMA_INSTANCE_NAMESPACE
                );
                self.holder.error(SAXParseError {
                    error: XMLError::XSDError(XSDError::NoXmlns),
                    level: XMLErrorLevel::Error,
                    domain: XMLErrorDomain::XSDParser,
                    line: self.locator.line(),
                    column: self.locator.column(),
                    system_id: self.locator.system_id(),
                    public_id: self.locator.public_id(),
                    message: Cow::Owned(msg),
                });
                ret = Err(XMLError::XSDError(XSDError::NoXmlns));
                continue;
            }

            if let Some(value) = decl.value_constraint.take() {
                if t.is_derived_from(Some(XML_SCHEMA_NAMESPACE), "ID", &self.schema.type_defs) {
                    let msg = format!(
                        "The attribute declaration '{}' must not have a default value because this is derived from 'ID'.",
                        decl.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::APropsCorrect),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    ret = Err(XMLError::XSDError(XSDError::APropsCorrect));
                    continue;
                }
                let raw_value = match value.clone() {
                    ValueConstraint::Default(value) => value,
                    ValueConstraint::Fixed(value) => value,
                };
                match self.schema.parse_simple_type(
                    &raw_value,
                    t.target_namespace.as_deref(),
                    &t.name,
                    &decl.ns_context,
                ) {
                    Ok(ret) => {
                        decl.value_constraint = match value {
                            ValueConstraint::Default(_) => {
                                Some(ValueConstraint::Default(ret.canonic_value.into()))
                            }
                            ValueConstraint::Fixed(_) => {
                                Some(ValueConstraint::Fixed(ret.canonic_value.into()))
                            }
                        }
                    }
                    Err(err) => {
                        // [a-props-correct] clause 2
                        let msg = format!(
                            "The default value '{}' of attribute declaration '{}' is not valid with respect to the simple type '{}' because of {}.",
                            raw_value,
                            decl.display_name(),
                            t.display_name(),
                            err
                        );
                        self.holder.error(SAXParseError {
                            error: XMLError::XSDError(XSDError::APropsCorrect),
                            level: XMLErrorLevel::Error,
                            domain: XMLErrorDomain::XSDParser,
                            line: self.locator.line(),
                            column: self.locator.column(),
                            system_id: self.locator.system_id(),
                            public_id: self.locator.public_id(),
                            message: Cow::Owned(msg),
                        });
                        ret = Err(XMLError::XSDError(XSDError::APropsCorrect));
                        continue;
                    }
                }
            }

            self.schema.att_decls.insert(key, decl);
        }
        ret
    }

    /// # Reference
    /// - [ag-props-correct](https://www.w3.org/TR/xmlschema-1/#ag-props-correct)
    fn check_all_attgroup(&mut self) -> Result<(), XMLError> {
        // Resolve references to other attribute groups.
        let mut attgroups = std::mem::take(&mut self.schema.attgroups);
        let mut seen = HashSet::new();
        while let Some(key) = attgroups.keys().next().cloned() {
            let attgroup = attgroups.remove(&key).unwrap();
            seen.clear();
            seen.insert(key.clone());
            self.expand_attgroup(key, attgroup, &mut attgroups, &mut seen)?;
        }

        let mut ret = Ok(());
        let attgroups = std::mem::take(&mut self.schema.attgroups);
        for attgroup in attgroups.values() {
            let mut used = HashSet::new();
            let mut idcnt = 0;
            for attuse in &attgroup.attribute_uses {
                if !used.insert(&attuse.attribute_declaration) {
                    // [ag-props-correct] clause 2
                    let msg = format!(
                        "The attribute '{}' for the attribute group '{}' is duplicated.",
                        attuse.display_name(),
                        attgroup.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::AgPropsCorrect),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    ret = Err(XMLError::XSDError(XSDError::AgPropsCorrect));
                } else if idcnt <= 1 {
                    // [ag-props-correct] clause 3
                    let Some(att) = self.schema.att_decls.get(&attuse.attribute_declaration) else {
                        let msg = format!(
                            "The attribute declaration '{}{}' is not found in schema.",
                            attuse
                                .attribute_declaration
                                .0
                                .as_deref()
                                .map(|ns| format!("{{{ns}}}"))
                                .unwrap_or_default(),
                            attuse.attribute_declaration.1
                        );
                        self.holder.error(SAXParseError {
                            error: XMLError::XSDError(XSDError::MissingSubComponent),
                            level: XMLErrorLevel::Error,
                            domain: XMLErrorDomain::XSDParser,
                            line: self.locator.line(),
                            column: self.locator.column(),
                            system_id: self.locator.system_id(),
                            public_id: self.locator.public_id(),
                            message: Cow::Owned(msg),
                        });
                        return Err(XMLError::XSDError(XSDError::MissingSubComponent));
                    };
                    let Some(t) = self
                        .schema
                        .type_defs
                        .get(&att.type_definition)
                        .and_then(|t| t.as_simple_type())
                    else {
                        let msg = format!(
                            "The simple type '{}{}' is not found in schema.",
                            att.type_definition
                                .0
                                .as_deref()
                                .map(|ns| format!("{{{ns}}}"))
                                .unwrap_or_default(),
                            att.type_definition.1
                        );
                        self.holder.error(SAXParseError {
                            error: XMLError::XSDError(XSDError::MissingSubComponent),
                            level: XMLErrorLevel::Error,
                            domain: XMLErrorDomain::XSDParser,
                            line: self.locator.line(),
                            column: self.locator.column(),
                            system_id: self.locator.system_id(),
                            public_id: self.locator.public_id(),
                            message: Cow::Owned(msg),
                        });
                        return Err(XMLError::XSDError(XSDError::MissingSubComponent));
                    };
                    if t.is_derived_from(Some(XML_SCHEMA_NAMESPACE), "ID", &self.schema.type_defs) {
                        if idcnt == 1 {
                            let msg = format!(
                                "The attribute group '{}' has multiple types which are or are derived from ID.",
                                attgroup.display_name()
                            );
                            self.holder.error(SAXParseError {
                                error: XMLError::XSDError(XSDError::AgPropsCorrect),
                                level: XMLErrorLevel::Error,
                                domain: XMLErrorDomain::XSDParser,
                                line: self.locator.line(),
                                column: self.locator.column(),
                                system_id: self.locator.system_id(),
                                public_id: self.locator.public_id(),
                                message: Cow::Owned(msg),
                            });
                            ret = Err(XMLError::XSDError(XSDError::AgPropsCorrect));
                        }
                        idcnt += 1;
                    }
                }
            }
        }

        self.schema.attgroups = attgroups;
        ret
    }

    fn expand_attgroup(
        &mut self,
        key: (Option<Arc<str>>, Arc<str>),
        mut attgroup: AttributeGroupDefinition,
        attgroups: &mut HashMap<(Option<Arc<str>>, Arc<str>), AttributeGroupDefinition>,
        seen: &mut HashSet<(Option<Arc<str>>, Arc<str>)>,
    ) -> Result<(), XMLError> {
        let mut wildcards = vec![];
        while let Some(reference) = attgroup.attribute_group_refs.pop() {
            if !seen.insert(reference.clone()) {
                // [src-attribute_group] clause 3
                let msg = format!(
                    "The attribute group definition '{}' contains a circular reference.",
                    attgroup.display_name()
                );
                self.holder.error(SAXParseError {
                    error: XMLError::XSDError(XSDError::SrcAttributeGroup),
                    level: XMLErrorLevel::Error,
                    domain: XMLErrorDomain::XSDParser,
                    line: self.locator.line(),
                    column: self.locator.column(),
                    system_id: self.locator.system_id(),
                    public_id: self.locator.public_id(),
                    message: Cow::Owned(msg),
                });
                return Err(XMLError::XSDError(XSDError::SrcAttributeGroup));
            }

            if let Some(parent) = attgroups.remove(&reference) {
                if let Some(wc) = parent.attribute_wildcard.as_ref() {
                    wildcards.push(wc.clone());
                }
                self.expand_attgroup(reference.clone(), parent, attgroups, seen)?;
            }

            if let Some(parent) = self.schema.attgroups.get(&reference) {
                if let Some(wc) = parent.attribute_wildcard.as_ref() {
                    wildcards.push(wc.clone());
                }
                attgroup
                    .attribute_uses
                    .extend_from_slice(&parent.attribute_uses);
            } else {
                let msg = format!(
                    "The attribute group declaration '{}{}' is not found in schema.",
                    reference
                        .0
                        .as_deref()
                        .map(|ns| format!("{{{ns}}}"))
                        .unwrap_or_default(),
                    reference.1,
                );
                self.holder.error(SAXParseError {
                    error: XMLError::XSDError(XSDError::MissingSubComponent),
                    level: XMLErrorLevel::Error,
                    domain: XMLErrorDomain::XSDParser,
                    line: self.locator.line(),
                    column: self.locator.column(),
                    system_id: self.locator.system_id(),
                    public_id: self.locator.public_id(),
                    message: Cow::Owned(msg),
                });
                return Err(XMLError::XSDError(XSDError::MissingSubComponent));
            }

            seen.remove(&reference);
        }

        let complete_wildcard =
            attribute_complete_wildcard(&wildcards, attgroup.attribute_wildcard.take());
        match complete_wildcard {
            Ok(attribute_wildcard) => attgroup.attribute_wildcard = attribute_wildcard,
            Err(error) => {
                let msg = format!(
                    "The attribute wildcard of the attribute group '{}' cannot be computed.",
                    attgroup.display_name()
                );
                self.holder.error(SAXParseError {
                    error: XMLError::XSDError(error),
                    level: XMLErrorLevel::Error,
                    domain: XMLErrorDomain::XSDParser,
                    line: self.locator.line(),
                    column: self.locator.column(),
                    system_id: self.locator.system_id(),
                    public_id: self.locator.public_id(),
                    message: Cow::Owned(msg),
                });
                return Err(XMLError::XSDError(error));
            }
        }
        self.schema.attgroups.insert(key, attgroup);
        Ok(())
    }

    /// # Reference
    /// - [e-props-correct](https://www.w3.org/TR/xmlschema-1/#e-props-correct)
    fn check_all_elemdecl(&mut self) -> Result<(), XMLError> {
        let mut ret = Ok(());
        // [e-props-correct] clause 2, 3
        for decl in self.schema.elem_decls.values_mut() {
            // [e-props-correct] clause 2, 5
            if let Some(mut vc) = decl.value_constraint.take() {
                let value = match &mut vc {
                    ValueConstraint::Default(value) => value,
                    ValueConstraint::Fixed(value) => value,
                };
                let Some(t) = self.schema.type_defs.get(&decl.type_definition) else {
                    let msg = format!(
                        "The type definition '{}{}' for the element declaration '{}' is not found in schema.",
                        decl.type_definition
                            .0
                            .as_deref()
                            .map(|ns| format!("{{{ns}}}"))
                            .unwrap_or_default(),
                        decl.type_definition.1,
                        decl.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::MissingSubComponent),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    ret = Err(XMLError::XSDError(XSDError::MissingSubComponent));
                    continue;
                };

                // [e-props-correct] clause 5
                if t.is_derived_from(Some(XML_SCHEMA_NAMESPACE), "ID", &self.schema.type_defs) {
                    let msg = format!(
                        "The element declaration '{}' has a default value, but its content type is derived from 'xs:ID'.",
                        decl.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::EPropsCorrect),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    ret = Err(XMLError::XSDError(XSDError::EPropsCorrect));
                    continue;
                }

                // [e-props-correct] clause 2
                // [cos-valid-default]
                match t {
                    TypeDefinition::Simple(st) => {
                        // [cos-valid-default] clause 1
                        match st.parse(value, &decl.ns_context, &self.schema.type_defs) {
                            Ok(sv) => {
                                *value = sv.canonic_value.into();
                                decl.value_constraint = Some(vc);
                            }
                            Err(error) => {
                                let msg = format!(
                                    "The default value '{}' of the element declaration '{}' is not valid as the simple type '{}' because of '{}'.",
                                    value,
                                    decl.display_name(),
                                    st.display_name(),
                                    error,
                                );
                                self.holder.error(SAXParseError {
                                    error: XMLError::XSDError(XSDError::CosValidDefault),
                                    level: XMLErrorLevel::Error,
                                    domain: XMLErrorDomain::XSDParser,
                                    line: self.locator.line(),
                                    column: self.locator.column(),
                                    system_id: self.locator.system_id(),
                                    public_id: self.locator.public_id(),
                                    message: Cow::Owned(msg),
                                });
                                ret = Err(XMLError::XSDError(XSDError::CosValidDefault));
                            }
                        }
                    }
                    TypeDefinition::Complex(ct) => {
                        // [cos-valid-default] clause 2
                        match &ct.content_type {
                            ContentType::Simple(ns, name) => {
                                let Some(st) = self
                                    .schema
                                    .type_defs
                                    .get(&(ns.clone(), name.clone()))
                                    .and_then(|t| t.as_simple_type())
                                else {
                                    ret = Err(XMLError::XSDError(XSDError::MissingSubComponent));
                                    continue;
                                };
                                match st.parse(value, &decl.ns_context, &self.schema.type_defs) {
                                    Ok(sv) => {
                                        *value = sv.canonic_value.into();
                                        decl.value_constraint = Some(vc);
                                    }
                                    Err(error) => {
                                        let msg = format!(
                                            "The default value '{}' of the element declaration '{}' is not valid as the content simple type '{}' of the complex type '{}' because of '{}'.",
                                            value,
                                            decl.display_name(),
                                            st.display_name(),
                                            ct.display_name(),
                                            error,
                                        );
                                        self.holder.error(SAXParseError {
                                            error: XMLError::XSDError(XSDError::CosValidDefault),
                                            level: XMLErrorLevel::Error,
                                            domain: XMLErrorDomain::XSDParser,
                                            line: self.locator.line(),
                                            column: self.locator.column(),
                                            system_id: self.locator.system_id(),
                                            public_id: self.locator.public_id(),
                                            message: Cow::Owned(msg),
                                        });
                                        ret = Err(XMLError::XSDError(XSDError::CosValidDefault));
                                    }
                                }
                            }
                            ContentType::Mixed(pt) if pt.is_emptiable(&self.schema.modelgroups) => {
                                // clause 2.2.2
                                decl.value_constraint = Some(vc);
                            }
                            _ => {
                                let msg = format!(
                                    "The default value '{}' of the element declaration '{}' is not valid as the content type of the complex type '{}'.",
                                    value,
                                    decl.display_name(),
                                    ct.display_name()
                                );
                                self.holder.error(SAXParseError {
                                    error: XMLError::XSDError(XSDError::CosValidDefault),
                                    level: XMLErrorLevel::Error,
                                    domain: XMLErrorDomain::XSDParser,
                                    line: self.locator.line(),
                                    column: self.locator.column(),
                                    system_id: self.locator.system_id(),
                                    public_id: self.locator.public_id(),
                                    message: Cow::Owned(msg),
                                });
                                ret = Err(XMLError::XSDError(XSDError::CosValidDefault));
                            }
                        }
                    }
                }
            }

            // [e-props-correct] clause 3
            if decl.substitution_group_affiliation.is_some()
                && !matches!(decl.scope, Some(Scope::Global))
            {
                let msg = format!(
                    "Non-global element declaration '{}' must not have 'substitutionGroup' attribute.",
                    decl.display_name()
                );
                self.holder.error(SAXParseError {
                    error: XMLError::XSDError(XSDError::EPropsCorrect),
                    level: XMLErrorLevel::Error,
                    domain: XMLErrorDomain::XSDParser,
                    line: self.locator.line(),
                    column: self.locator.column(),
                    system_id: self.locator.system_id(),
                    public_id: self.locator.public_id(),
                    message: Cow::Owned(msg),
                });
                ret = Err(XMLError::XSDError(XSDError::EPropsCorrect));
            }
        }
        ret.clone()?;

        // [e-props-correct] clause 4, 6
        // [cos-equiv-class]
        // [cos-equiv-derived-ok-rec]
        let mut decls = std::mem::take(&mut self.schema.elem_decls);
        let mut stack = vec![];
        // A set of derivation methods for the type up to the
        // HEAD of the substitution group for each declaration
        // [cos-equiv-derived-ok-rec] clause 2.3
        // > The set of all {derivation method}s involved in the derivation
        // > of D's {type definition} from C's {type definition}
        let mut derivation_methods = HashMap::new();
        // The union of {prohibited substitution}s of complex types
        // up to the HEAD of each declaration's substitution group
        // [cos-equiv-derived-ok-rec] clause 2.3
        // > C's {prohibited substitutions} (if C is complex, otherwise the empty set)
        // > and the {prohibited substitutions} (respectively the empty set) of any
        // > intermediate {type definition}s in the derivation of D's {type definition}
        // > from C's {type definition}
        let mut block_constraints = HashMap::new();
        while let Some(key) = decls.keys().next().cloned() {
            let decl = decls.remove(&key).unwrap();
            stack.push((key, decl));
            while let Some((key, decl)) = stack.pop() {
                let Some(subst) = decl.substitution_group_affiliation.clone() else {
                    // "blocking constraint" described at [cos-equiv-derived-ok-rec]
                    //      > a blocking constraint (a subset of {substitution, extension,
                    //      > restriction}, the value of a {disallowed substitutions})
                    // and [cos-equiv-class] clause 2
                    //      > HEAD's {disallowed substitutions} as the blocking constraint
                    let blocking_constraint = decl.disallowed_substitutions;
                    block_constraints.insert(
                        key.clone(),
                        blocking_constraint
                            | self
                                .schema
                                .type_defs
                                .get(&decl.type_definition)
                                .and_then(|t| t.as_complex_type())
                                .map(|ct| ct.prohibited_substitutions)
                                .unwrap_or_default(),
                    );
                    self.schema.elem_decls.insert(key.clone(), decl);
                    derivation_methods.insert(key.clone(), BlockSet::default());
                    continue;
                };
                if let Some(par) = decls.remove(&subst) {
                    stack.push((key, decl));
                    stack.push((subst, par));
                    continue;
                }
                if let Some(par) = self.schema.elem_decls.get(&subst) {
                    // [e-props-correct] clause 4
                    if self.check_e_props_correct_4(&decl, par).is_err() {
                        let msg = format!(
                            "The type definition of the element declaration '{}' is not validly derived from that of '{}'.",
                            decl.display_name(),
                            par.display_name()
                        );
                        self.holder.error(SAXParseError {
                            error: XMLError::XSDError(XSDError::EPropsCorrect),
                            level: XMLErrorLevel::Error,
                            domain: XMLErrorDomain::XSDParser,
                            line: self.locator.line(),
                            column: self.locator.column(),
                            system_id: self.locator.system_id(),
                            public_id: self.locator.public_id(),
                            message: Cow::Owned(msg),
                        });
                        ret = Err(XMLError::XSDError(XSDError::EPropsCorrect));
                    }
                    if par.disallowed_substitutions.substitution() {
                        // [cos-equiv-derived-ok-rec] clause 2.1
                        // `par` and `decl` can form a substitution group with `par` as HEAD,
                        // but substitution of `par` is prohibited.
                        let msg = format!(
                            "The element declaration '{}' is not substitutable for '{}'.",
                            decl.display_name(),
                            par.display_name()
                        );
                        self.holder.error(SAXParseError {
                            error: XMLError::XSDError(XSDError::CosEquivDerivedOkRec),
                            level: XMLErrorLevel::Error,
                            domain: XMLErrorDomain::XSDParser,
                            line: self.locator.line(),
                            column: self.locator.column(),
                            system_id: self.locator.system_id(),
                            public_id: self.locator.public_id(),
                            message: Cow::Owned(msg),
                        });
                        ret = Err(XMLError::XSDError(XSDError::CosEquivDerivedOkRec));
                    }
                    if let Some(t) = self.schema.type_defs.get(&decl.type_definition) {
                        block_constraints.insert(
                            key.clone(),
                            t.as_complex_type()
                                .map(|ct| ct.prohibited_substitutions)
                                .unwrap_or_default()
                                | block_constraints.get(&subst).copied().unwrap_or_default(),
                        );
                        let deriv = derivation_methods.get(&subst).copied().unwrap_or_default();
                        if let Some(ct) = t.as_complex_type() {
                            let new =
                                if matches!(ct.derivation_method, DerivationMethod::Restriction) {
                                    deriv | Block::Restriction
                                } else {
                                    deriv | Block::Extension
                                };
                            derivation_methods.insert(key.clone(), new);
                        } else {
                            derivation_methods.insert(key.clone(), deriv);
                        }
                    }
                    self.schema.elem_decls.insert(key, decl);
                } else if stack.iter().any(|prev| prev.0 == subst) {
                    // [e-props-correct] clause 6 (circular substitution group)
                    let msg = format!(
                        "A circular substitution group is found at '{}'.",
                        decl.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::EPropsCorrect),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    ret = Err(XMLError::XSDError(XSDError::EPropsCorrect));
                    stack.clear();
                    break;
                } else {
                    // missing subcomponent
                    let msg = format!(
                        "'{}{}' that is specified as the substition group at the element declaration '{}' is not found in schema.",
                        subst
                            .0
                            .as_deref()
                            .map(|ns| format!("{{{ns}}}"))
                            .unwrap_or_default(),
                        subst.1,
                        decl.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::EPropsCorrect),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    ret = Err(XMLError::XSDError(XSDError::EPropsCorrect));
                    self.schema.elem_decls.insert(key, decl);
                }
            }
        }

        for (key, decl) in &self.schema.elem_decls {
            if let Some(subst) = decl.substitution_group_affiliation.as_ref() {
                let derivation_methods = derivation_methods.get(key).copied().unwrap_or_default();
                // Since it states, "the {prohibited substitutions} (respectively, the empty set)
                // of any intermediate {type definitions} in the derivation of D's {type definition}
                // from C's {type definition}", the value of the substitution parent is invoked
                // so that it does not include the {prohibited substitutions} of its own {type definition}.
                let blocks = block_constraints.get(subst).copied().unwrap_or_default();
                let intersection = derivation_methods.intersection(&blocks);
                if intersection.extension() || intersection.restriction() {
                    // [cos-equiv-derived-ok-rec] clause 2.3
                    let msg = format!(
                        "The element declaration '{}' is not substitutable for HEAD of its substitution group.",
                        decl.display_name()
                    );
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::CosEquivDerivedOkRec),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    ret = Err(XMLError::XSDError(XSDError::CosEquivDerivedOkRec));
                }
            }
        }
        ret
    }

    /// # Reference
    /// - [e-props-correct](https://www.w3.org/TR/xmlschema-1/#e-props-correct)
    fn check_e_props_correct_4(
        &self,
        decl: &ElementDeclaration,
        par: &ElementDeclaration,
    ) -> Result<(), XMLError> {
        let t = self
            .schema
            .type_defs
            .get(&decl.type_definition)
            .ok_or(XSDError::MissingSubComponent)?;
        let pt = self
            .schema
            .type_defs
            .get(&par.type_definition)
            .ok_or(XSDError::MissingSubComponent)?;
        match (t, pt) {
            (TypeDefinition::Simple(_), TypeDefinition::Complex(_)) => {
                // The simple type cannot be derived from a complex type.
                Err(XMLError::XSDError(XSDError::EPropsCorrect))
            }
            (TypeDefinition::Simple(d), TypeDefinition::Simple(b)) => {
                // check by [cos-st-derived-ok]
                self.check_cos_st_derived_ok(d, b, decl.substitution_group_exclusions)
            }
            (TypeDefinition::Complex(d), _) => {
                // check by [cos-ct-derived-ok]
                self.check_cos_ct_derived_ok(d, pt, decl.substitution_group_exclusions)
            }
        }
    }
}

impl<H: SAXHandler> EntityResolver for XSDParseHandler<H> {
    fn resolve_entity(
        &mut self,
        name: &str,
        public_id: Option<&str>,
        base_uri: &URIStr,
        system_id: &URIStr,
    ) -> Result<InputSource<'static>, XMLError> {
        self.holder
            .child
            .resolve_entity(name, public_id, base_uri, system_id)
    }

    fn get_external_subset(
        &mut self,
        name: &str,
        base_uri: Option<&URIStr>,
    ) -> Result<InputSource<'static>, XMLError> {
        self.holder.child.get_external_subset(name, base_uri)
    }
}

impl<H: SAXHandler> ErrorHandler for XSDParseHandler<H> {
    fn fatal_error(&mut self, error: SAXParseError) {
        self.holder.fatal_error(error);
    }

    fn error(&mut self, error: SAXParseError) {
        self.holder.error(error);
    }

    fn warning(&mut self, error: SAXParseError) {
        self.holder.warning(error);
    }
}

impl<H: SAXHandler> SAXHandler for XSDParseHandler<H> {
    fn set_document_locator(&mut self, locator: Arc<Locator>) {
        self.locator = locator.clone();
        self.holder.child.set_document_locator(locator);
    }

    fn start_element(
        &mut self,
        namespace_name: Option<&str>,
        local_name: Option<&str>,
        qname: &str,
        atts: &Attributes,
    ) {
        self.holder
            .child
            .start_element(namespace_name, local_name, qname, atts);

        // skip non-schema elements
        //
        // 3.1.2 XML Representations of Components
        // All the element information items in the XML representation of a schema must
        // be in the XML Schema namespace, that is their [namespace name] must be
        // http://www.w3.org/2001/XMLSchema.
        if self.skip_depth > 0 || namespace_name != Some(XML_SCHEMA_NAMESPACE) {
            self.skip_depth += 1;
            return;
        }

        let local_name = local_name.unwrap();
        if self.element_stack.is_empty() && local_name != SCHEMA_SCHEMA_NAME {
            // If this is non-schema document, report error and skip all elements.
            //
            // 3.1.2 XML Representations of Components
            // [Definition:]  A document in this form (i.e. a <schema> element information
            // item) is a schema document.
            self.holder.error(SAXParseError {
                error: XMLError::XSDError(XSDError::UnallowedElement),
                level: XMLErrorLevel::Error,
                domain: XMLErrorDomain::XSDParser,
                line: self.locator.line(),
                column: self.locator.column(),
                system_id: self.locator.system_id(),
                public_id: self.locator.public_id(),
                message: Cow::Owned(format!(
                    "'{}' cannot be the root elemnt of a schema.",
                    local_name
                )),
            });
            self.skip_depth += 1;
            return;
        }

        match local_name {
            SCHEMA_SCHEMA_NAME => {
                self.attr_form_default =
                    get_qualification_form(atts, "attributeFormDefault").unwrap_or_default();
                self.elem_form_default =
                    get_qualification_form(atts, "elementFormDefault").unwrap_or_default();
                self.block_default = get_block(atts, "blockDefault").unwrap_or_default();
                self.final_default = get_final(atts, "finalDefault").unwrap_or_default();
                self.target_namespace = atts.get_value_by_qname("targetNamespace").map(From::from);
                match self.inclusion_state {
                    InclusionState::InInclude | InclusionState::InRedefine => {
                        // [src-include] clause 2
                        // [src-redefine] clause 3
                        let etype = if matches!(self.inclusion_state, InclusionState::InInclude) {
                            XSDError::SrcInclude
                        } else {
                            XSDError::SrcRedefine
                        };
                        if let Some(parent_target_namespace) = self.parent_target_namespace.clone()
                        {
                            if let Some(tn) = self.target_namespace.as_deref()
                                && tn != parent_target_namespace.as_ref()
                            {
                                let msg = format!(
                                    "Included schema's target namespace is '{}', but parent's is '{}'.",
                                    tn, parent_target_namespace
                                );
                                self.holder.error(SAXParseError {
                                    error: XMLError::XSDError(etype),
                                    level: XMLErrorLevel::Error,
                                    domain: XMLErrorDomain::XSDParser,
                                    line: self.locator.line(),
                                    column: self.locator.column(),
                                    system_id: self.locator.system_id(),
                                    public_id: self.locator.public_id(),
                                    message: Cow::Owned(msg),
                                });
                                self.skip_depth += 1;
                                return;
                            }
                            self.target_namespace = Some(parent_target_namespace);
                        } else if let Some(tn) = self.target_namespace.as_deref() {
                            let msg = format!(
                                "Included schema's target namespace is '{}', but parent's is absent.",
                                tn
                            );
                            self.holder.error(SAXParseError {
                                error: XMLError::XSDError(etype),
                                level: XMLErrorLevel::Error,
                                domain: XMLErrorDomain::XSDParser,
                                line: self.locator.line(),
                                column: self.locator.column(),
                                system_id: self.locator.system_id(),
                                public_id: self.locator.public_id(),
                                message: Cow::Owned(msg),
                            });
                            self.skip_depth += 1;
                            return;
                        }
                    }
                    InclusionState::InImport => {
                        // [src-import] clause 3
                        if self.parent_target_namespace != self.target_namespace {
                            let msg = format!(
                                "Imported schema's target namespace is {}, but parent schema's 'namespace' of <import> is {}.",
                                self.target_namespace
                                    .as_deref()
                                    .map(|ns| format!("'{ns}'"))
                                    .as_deref()
                                    .unwrap_or("absent"),
                                self.parent_target_namespace
                                    .as_deref()
                                    .map(|ns| format!("'{ns}'"))
                                    .as_deref()
                                    .unwrap_or("absent"),
                            );
                            self.holder.error(SAXParseError {
                                error: XMLError::XSDError(XSDError::SrcImport),
                                level: XMLErrorLevel::Error,
                                domain: XMLErrorDomain::XSDParser,
                                line: self.locator.line(),
                                column: self.locator.column(),
                                system_id: self.locator.system_id(),
                                public_id: self.locator.public_id(),
                                message: Cow::Owned(msg),
                            });
                            self.skip_depth += 1;
                            return;
                        }
                    }
                    InclusionState::TopLevel => {}
                }
                self.element_stack.push(SCHEMA_SCHEMA_NAME);
            }
            SCHEMA_INCLUDE_NAME => {
                let Some(value) = atts
                    .get_value_by_qname("schemaLocation")
                    .and_then(|loc| URIString::parse(loc).ok())
                else {
                    self.skip_depth += 1;
                    return;
                };
                self.includes.push(value);
                self.element_stack.push(SCHEMA_INCLUDE_NAME);
            }
            SCHEMA_REDEFINE_NAME => {
                let Some(value) = atts
                    .get_value_by_qname("schemaLocation")
                    .and_then(|loc| URIString::parse(loc).ok())
                else {
                    self.skip_depth += 1;
                    return;
                };
                self.redefined_schema_location = Some(value.into());
                self.element_stack.push(SCHEMA_REDEFINE_NAME);
            }
            SCHEMA_IMPORT_NAME => {
                let Some(schema_location) = atts
                    .get_value_by_qname("schemaLocation")
                    .and_then(|loc| URIString::parse(loc).ok())
                else {
                    // Although the absence of a `schemaLocation` is not an error, there is
                    // nothing that can be done about such `import` elements, so it is skipped.
                    self.skip_depth += 1;
                    return;
                };
                let namespace = atts.get_value_by_qname("namespace").map(Arc::<str>::from);
                if self.target_namespace.as_deref() == namespace.as_deref() {
                    // [src-import] clause 1
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::SrcImport),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Borrowed("'namespace' of <import> must not match target namespace of the schema."),
                    });
                    self.skip_depth += 1;
                    return;
                }
                self.imports.push((namespace, schema_location));
                self.element_stack.push(SCHEMA_IMPORT_NAME);
            }
            SCHEMA_SIMPLETYPE_NAME => {
                if self.element_stack.last() == Some(&SCHEMA_RESTRICTION_NAME)
                    && self.element_stack[..self.element_stack.len() - 1].last()
                        == Some(&SCHEMA_SIMPLETYPE_NAME)
                    && !self
                        .st_stack
                        .last()
                        .is_some_and(|st| st.base_type_definition.1.is_empty())
                {
                    // [src-restriction-base-or-simpleType]
                    // If the "base" attribute is present in <restriction>,
                    // <simpleType> must not exist under <restriction>.
                    let msg = "The attribute 'base' and the child <simpleType> of <restriction> must not both be present.";
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::SrcRestrictionBaseOrSimpleType),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Borrowed(msg),
                    });
                    self.skip_depth += 1;
                    return;
                } else if self.element_stack.last() == Some(&SCHEMA_LIST_NAME)
                    && self.st_stack.last().is_some_and(|st| st.variety.is_some())
                {
                    // [src-list-itemType-or-simpleType]
                    // If the `itemType` attribute is present,
                    // the child must not have a `simpleType`.
                    let msg = "The attribute 'itemType' and the child <simpleType> of <list> must not both be present.";
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::SrcListItemTypeOrSimpleType),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Borrowed(msg),
                    });
                    self.skip_depth += 1;
                    return;
                }

                // Whether "name" is required depends on whether it is a top-level
                // declaration. Since the meta-schema validator is responsible for
                // reporting errors when required attributes are missing, "name" can
                // be treated as an optional attribute here.
                let new = SimpleTypeDefinition {
                    name: atts
                        .get_value_by_qname("name")
                        .map(Arc::from)
                        .unwrap_or_else(|| self.dummy_name()),
                    target_namespace: self.target_namespace.clone(),
                    r#final: get_final(atts, "final").unwrap_or_default(),
                    ..Default::default()
                };
                self.st_stack.push(new);
                self.element_stack.push(SCHEMA_SIMPLETYPE_NAME);
            }
            SCHEMA_RESTRICTION_NAME
                if self.element_stack.last() == Some(&SCHEMA_SIMPLETYPE_NAME) =>
            {
                // 'restriction' under a simple type definition

                // Since the child may contain <simpleType>, "base" is not a required element.
                if let Some((ns, loc)) = self.get_qname(atts, "base")
                    && let Some(st) = self.st_stack.last_mut()
                {
                    st.base_type_definition = (ns, loc);
                }
                self.element_stack.push(SCHEMA_RESTRICTION_NAME);
            }
            SCHEMA_LIST_NAME => {
                // Since the child may contain <simpleType>, "itemType" is not a required element.
                let item_type = self.get_qname(atts, "itemType");
                if let Some(st) = self.st_stack.last_mut() {
                    if let Some((pre, loc)) = item_type {
                        st.variety = Some(Variety::List(pre, loc));
                    }
                    st.base_type_definition = (
                        XSD_ANY_SIMPLE_TYPE.target_namespace.clone(),
                        XSD_ANY_SIMPLE_TYPE.name.clone(),
                    );
                }
                self.element_stack.push(SCHEMA_LIST_NAME);
            }
            SCHEMA_UNION_NAME => {
                if let Some(st) = self.st_stack.last_mut() {
                    if let Some(member_types) = atts.get_value_by_qname("memberTypes") {
                        let mut buf = vec![];
                        for member in member_types.split_ascii_whitespace() {
                            if let Some((pre, loc)) = member.split_once(':') {
                                if let Some(ns) = self.ns_stack.get(pre) {
                                    buf.push((Some(ns.namespace_name.clone()), loc.into()));
                                }
                            } else if let Some(ns) = self.ns_stack.get("") {
                                buf.push((Some(ns.namespace_name.clone()), member.into()));
                            } else {
                                buf.push((None, member.into()));
                            }
                        }
                        st.variety = Some(Variety::Union(buf.into()));
                    }
                    st.base_type_definition = (
                        XSD_ANY_SIMPLE_TYPE.target_namespace.clone(),
                        XSD_ANY_SIMPLE_TYPE.name.clone(),
                    );
                }
                self.element_stack.push(SCHEMA_UNION_NAME);
            }
            SCHEMA_LENGTH_NAME => {
                let Some(value) = atts
                    .get_value_by_qname("value")
                    .and_then(|v| v.parse().ok())
                else {
                    self.skip_depth += 1;
                    return;
                };
                if let Some(st) = self.st_stack.last_mut() {
                    let fixed = get_boolean(atts, "fixed").unwrap_or_default();
                    st.facets.length = Some(Length { value, fixed });
                }
                self.element_stack.push(SCHEMA_LENGTH_NAME);
            }
            SCHEMA_MINLENGTH_NAME => {
                let Some(value) = atts
                    .get_value_by_qname("value")
                    .and_then(|v| v.parse().ok())
                else {
                    self.skip_depth += 1;
                    return;
                };
                if let Some(st) = self.st_stack.last_mut() {
                    let fixed = get_boolean(atts, "fixed").unwrap_or_default();
                    st.facets.min_length = Some(Length { value, fixed });
                }
                self.element_stack.push(SCHEMA_MINLENGTH_NAME);
            }
            SCHEMA_MAXLENGTH_NAME => {
                let Some(value) = atts
                    .get_value_by_qname("value")
                    .and_then(|v| v.parse().ok())
                else {
                    self.skip_depth += 1;
                    return;
                };
                if let Some(st) = self.st_stack.last_mut() {
                    let fixed = get_boolean(atts, "fixed").unwrap_or_default();
                    st.facets.max_length = Some(Length { value, fixed });
                }
                self.element_stack.push(SCHEMA_MAXLENGTH_NAME);
            }
            SCHEMA_PATTERN_NAME => {
                let Some(regex) = atts.get_value_by_qname("value") else {
                    self.skip_depth += 1;
                    return;
                };
                if let Err(err) = XSRegexp::compile(regex) {
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::RegexpError(err)),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(format!(
                            "'{regex}' is invalid regular expression because: {err}"
                        )),
                    });
                    self.skip_depth += 1;
                    return;
                }
                if let Some(st) = self.st_stack.last_mut() {
                    st.facets.pattern.push(Pattern {
                        value: regex.into(),
                    });
                }
                self.element_stack.push(SCHEMA_PATTERN_NAME);
            }
            SCHEMA_ENUMERATION_NAME => {
                let Some(value) = atts.get_value_by_qname("value") else {
                    self.skip_depth += 1;
                    return;
                };
                if let Some(st) = self.st_stack.last_mut() {
                    st.facets.enumeration.push(Enumeration {
                        value: SchemaValue {
                            r#type: SchemaValueType::String,
                            list: false,
                            raw_value: value.into(),
                            canonic_value: value.into(),
                        },
                        ns_context: self.ns_stack.clone(),
                    });
                }
                self.element_stack.push(SCHEMA_ENUMERATION_NAME);
            }
            SCHEMA_WHITESPACE_NAME => {
                let Some(value) = atts.get_value_by_qname("value") else {
                    self.skip_depth += 1;
                    return;
                };
                let fixed = get_boolean(atts, "fixed").unwrap_or_default();
                let value = match value {
                    "collapse" => Whitespace::Collapse,
                    "preserve" => Whitespace::Preserve,
                    "replace" => Whitespace::Replace,
                    _ => {
                        self.skip_depth += 1;
                        return;
                    }
                };
                if let Some(st) = self.st_stack.last_mut() {
                    st.facets.whitespace = Some(WhitespaceConstraint { value, fixed });
                }
                self.element_stack.push(SCHEMA_WHITESPACE_NAME);
            }
            SCHEMA_MAXINCLUSIVE_NAME => {
                let Some(value) = atts.get_value_by_qname("value") else {
                    self.skip_depth += 1;
                    return;
                };
                if let Some(st) = self.st_stack.last_mut() {
                    let fixed = get_boolean(atts, "fixed").unwrap_or_default();
                    st.facets.max_inclusive = Some(Boundary {
                        value: SchemaValue {
                            r#type: SchemaValueType::String,
                            list: false,
                            raw_value: value.into(),
                            canonic_value: value.into(),
                        },
                        fixed,
                        ns_context: self.ns_stack.clone(),
                    });
                }
                self.element_stack.push(SCHEMA_MAXINCLUSIVE_NAME);
            }
            SCHEMA_MAXEXCLUSIVE_NAME => {
                let Some(value) = atts.get_value_by_qname("value") else {
                    self.skip_depth += 1;
                    return;
                };
                if let Some(st) = self.st_stack.last_mut() {
                    let fixed = get_boolean(atts, "fixed").unwrap_or_default();
                    st.facets.max_exclusive = Some(Boundary {
                        value: SchemaValue {
                            r#type: SchemaValueType::String,
                            list: false,
                            raw_value: value.into(),
                            canonic_value: value.into(),
                        },
                        fixed,
                        ns_context: self.ns_stack.clone(),
                    });
                }
                self.element_stack.push(SCHEMA_MAXEXCLUSIVE_NAME);
            }
            SCHEMA_MINEXCLUSIVE_NAME => {
                let Some(value) = atts.get_value_by_qname("value") else {
                    self.skip_depth += 1;
                    return;
                };
                if let Some(st) = self.st_stack.last_mut() {
                    let fixed = get_boolean(atts, "fixed").unwrap_or_default();
                    st.facets.min_exclusive = Some(Boundary {
                        value: SchemaValue {
                            r#type: SchemaValueType::String,
                            list: false,
                            raw_value: value.into(),
                            canonic_value: value.into(),
                        },
                        fixed,
                        ns_context: self.ns_stack.clone(),
                    });
                }
                self.element_stack.push(SCHEMA_MINEXCLUSIVE_NAME);
            }
            SCHEMA_MININCLUSIVE_NAME => {
                let Some(value) = atts.get_value_by_qname("value") else {
                    self.skip_depth += 1;
                    return;
                };
                if let Some(st) = self.st_stack.last_mut() {
                    let fixed = get_boolean(atts, "fixed").unwrap_or_default();
                    st.facets.min_inclusive = Some(Boundary {
                        value: SchemaValue {
                            r#type: SchemaValueType::String,
                            list: false,
                            raw_value: value.into(),
                            canonic_value: value.into(),
                        },
                        fixed,
                        ns_context: self.ns_stack.clone(),
                    });
                }
                self.element_stack.push(SCHEMA_MININCLUSIVE_NAME);
            }
            SCHEMA_TOTALDIGITS_NAME => {
                let Some(value) = atts
                    .get_value_by_qname("value")
                    .and_then(|v| v.parse::<usize>().ok())
                else {
                    self.skip_depth += 1;
                    return;
                };
                if let Some(st) = self.st_stack.last_mut() {
                    let fixed = get_boolean(atts, "fixed").unwrap_or_default();
                    st.facets.total_digits = Some(Length { value, fixed });
                }
                self.element_stack.push(SCHEMA_TOTALDIGITS_NAME);
            }
            SCHEMA_FRACDIGITS_NAME => {
                let Some(value) = atts
                    .get_value_by_qname("value")
                    .and_then(|v| v.parse::<usize>().ok())
                else {
                    self.skip_depth += 1;
                    return;
                };
                if let Some(st) = self.st_stack.last_mut() {
                    let fixed = get_boolean(atts, "fixed").unwrap_or_default();
                    st.facets.fraction_digits = Some(Length { value, fixed });
                }
                self.element_stack.push(SCHEMA_FRACDIGITS_NAME);
            }
            SCHEMA_COMPLEXTYPE_NAME => {
                let r#abstract = get_boolean(atts, "abstract").unwrap_or_default();
                let block = get_block(atts, "block").unwrap_or(self.block_default);
                let r#final = get_final(atts, "final").unwrap_or(self.final_default);
                let mixed = get_boolean(atts, "mixed").unwrap_or_default();
                // Although "name" is a required attribute in the top-level declaration,
                // validation for required attributes is not performed because this is
                // covered by meta schema validation.
                let name = atts
                    .get_value_by_qname("name")
                    .map(Arc::<str>::from)
                    .unwrap_or_else(|| self.dummy_name());
                self.ct_stack.push(ComplexTypeDefinition {
                    name,
                    target_namespace: self.target_namespace.clone(),
                    r#final,
                    r#abstract,
                    content_type: if mixed {
                        // Set a value in case {content type} is not updated.
                        // component property mapping of {content type}
                        // - clause 1.2
                        // - clause 2.1.1
                        // - clause 2.1.4
                        // - clause 3.1.2.1
                        //
                        // As described in the note to [src-ct] clause 2.2, when <simpleContent>
                        // alternative is selected, the `mixed` attribute has no meaning, and
                        // {content type} is overridden by a simple type.
                        ContentType::Mixed(Particle {
                            min_occurs: 1,
                            max_occurs: 1,
                            term: Term::ModelGroup(ModelGroup {
                                compositor: Compositor::Sequence,
                                particles: vec![],
                            }),
                        })
                    } else {
                        // Set a value in case {content type} is not updated.
                        // component property mapping of {content type}
                        // - clause 1.3
                        // - clause 2.1.1
                        // - clause 2.1.5
                        // - clause 3.1.1
                        ContentType::Empty
                    },
                    prohibited_substitutions: block,
                    ..Default::default()
                });
                self.element_stack.push(SCHEMA_COMPLEXTYPE_NAME);
            }
            SCHEMA_SIMPLECONT_NAME => {
                // push dummy decl for collecting facets
                let name = self.dummy_name();
                let target_namespace = self.target_namespace.clone();
                self.st_stack.push(SimpleTypeDefinition {
                    name,
                    target_namespace,
                    ..Default::default()
                });
                self.element_stack.push(SCHEMA_SIMPLECONT_NAME);
            }
            SCHEMA_COMPLEXCONT_NAME => {
                let mixed = get_boolean(atts, "mixed");
                if let Some(ct) = self.ct_stack.last_mut() {
                    // component property mapping of {content type} (<complexType> alternative)
                    // - clause 1 (effective mixed)
                    let effective_mixed =
                        mixed.unwrap_or(matches!(ct.content_type, ContentType::Mixed(_)));
                    // - clause 2.1.1 (effective content in case no children appear)
                    let effective_content = Particle {
                        min_occurs: 1,
                        max_occurs: 1,
                        term: Term::ModelGroup(ModelGroup {
                            compositor: Compositor::Sequence,
                            particles: vec![],
                        }),
                    };
                    // Set dummy value
                    ct.content_type =
                        ContentType::Pending(Some(effective_content), effective_mixed);
                }
                self.element_stack.push(SCHEMA_COMPLEXCONT_NAME);
            }
            SCHEMA_RESTRICTION_NAME => {
                // 'restriction' under a complex type definition

                let Some((ns, local)) = self.get_qname(atts, "base") else {
                    self.skip_depth += 1;
                    return;
                };
                if let Some(ct) = self.ct_stack.last_mut() {
                    if self.element_stack.last() == Some(&SCHEMA_SIMPLECONT_NAME)
                        && let Some(st) = self.st_stack.last_mut()
                    {
                        // component property mapping of {content type} clause 1.2
                        //
                        // If there is a <simpleType> child, this will be overridden.
                        st.base_type_definition = (ns.clone(), local.clone());
                    }
                    ct.base_type_definition = (ns, local);
                    ct.derivation_method = DerivationMethod::Restriction;
                }
                self.element_stack.push(SCHEMA_RESTRICTION_NAME);
            }
            SCHEMA_EXTENSION_NAME => {
                let Some(base) = self.get_qname(atts, "base") else {
                    self.skip_depth += 1;
                    return;
                };
                if let Some(ct) = self.ct_stack.last_mut() {
                    ct.base_type_definition = base;
                    ct.derivation_method = DerivationMethod::Extension;
                    if self.element_stack.last() == Some(&SCHEMA_SIMPLECONT_NAME) {
                        // To indicate that <simpleContent> alternative has been selected,
                        // set an empty `ContentType::Simple`.
                        ct.content_type = ContentType::Simple(None, "".into());
                    }
                }
                self.element_stack.push(SCHEMA_EXTENSION_NAME);
            }
            SCHEMA_GROUP_NAME => {
                let should_named = matches!(
                    self.element_stack.last().copied(),
                    Some(SCHEMA_SCHEMA_NAME | SCHEMA_REDEFINE_NAME)
                );
                if should_named && let Some(name) = atts.get_value_by_qname("name") {
                    self.model_group_def.name = name.into();
                    self.model_group_def.target_namespace = self.target_namespace.clone();
                    // push dummy data
                    self.pt_stack.push(Particle {
                        min_occurs: 1,
                        max_occurs: 1,
                        term: Term::ModelGroup(ModelGroup::default()),
                    });
                } else if !should_named && let Some((ns, local)) = self.get_qname(atts, "ref") {
                    let min_occurs = get_occurs(atts, "minOccurs").unwrap_or(1);
                    let max_occurs = get_occurs(atts, "maxOccurs").unwrap_or(1);
                    self.pt_stack.push(Particle {
                        min_occurs,
                        max_occurs,
                        term: Term::ModelGroupRef(ns, local),
                    });
                } else {
                    self.skip_depth += 1;
                    return;
                }
                self.element_stack.push(SCHEMA_GROUP_NAME);
            }
            SCHEMA_ATTGROUP_NAME => {
                let should_named = matches!(
                    self.element_stack.last().copied(),
                    Some(SCHEMA_SCHEMA_NAME | SCHEMA_REDEFINE_NAME)
                );
                if should_named && let Some(name) = atts.get_value_by_qname("name") {
                    self.attr_group_def.name = name.into();
                    self.attr_group_def.target_namespace = self.target_namespace.clone();
                } else if !should_named && let Some((ns, local)) = self.get_qname(atts, "ref") {
                    if self.element_stack.last() == Some(&SCHEMA_ATTGROUP_NAME) {
                        self.attr_group_def.attribute_group_refs.push((ns, local));
                    } else if let Some(ct) = self.ct_stack.last_mut() {
                        ct.attribute_group_refs.push((ns, local));
                    }
                } else {
                    self.skip_depth += 1;
                    return;
                }
                self.element_stack.push(SCHEMA_ATTGROUP_NAME);
            }
            SCHEMA_ELEMENT_NAME => {
                let block = get_block(atts, "block").unwrap_or(self.block_default);
                let nillable = get_boolean(atts, "nillable").unwrap_or_default();
                let r#type = self.get_qname(atts, "type");
                let r#abstract = get_boolean(atts, "abstract").unwrap_or_default();
                let r#final = get_final(atts, "final").unwrap_or(self.final_default);
                let substitution_group = self.get_qname(atts, "substitutionGroup");
                let min_occurs = get_occurs(atts, "minOccurs").unwrap_or(1);
                let max_occurs = get_occurs(atts, "maxOccurs").unwrap_or(1);
                if self.element_stack.last() == Some(&SCHEMA_SCHEMA_NAME) {
                    // top-level element declaration.
                    // "ref", "form", "minOccurs" and "maxOccurs" are prohibited explicitly in schema of schema.
                    let Some(name) = atts.get_value_by_qname("name").map(From::from) else {
                        self.skip_depth += 1;
                        return;
                    };

                    let value_constraint = self.get_element_default(atts);
                    self.ed_stack.push(ElementDeclaration {
                        name,
                        target_namespace: self.target_namespace.clone(),
                        type_definition: r#type.unwrap_or_default(),
                        scope: Some(Scope::Global),
                        value_constraint,
                        nillable,
                        idc_definitions: vec![],
                        substitution_group_affiliation: substitution_group,
                        substitution_group_exclusions: r#final,
                        disallowed_substitutions: block,
                        r#abstract,
                        ns_context: self.ns_stack.clone(),
                    });
                } else {
                    // local element declaration.
                    // "substitutionGroup", "final" and "abstract" are prohibited explicitly in schema of schema.
                    match (atts.get_value_by_qname("name"), self.get_qname(atts, "ref")) {
                        (Some(_), Some(_)) | (None, None) => {
                            // [src-element] clause 2.1
                            let msg = "One of 'ref' or 'name' must be present, but not both for local <element>.";
                            self.holder.error(SAXParseError {
                                error: XMLError::XSDError(XSDError::SrcElement),
                                level: XMLErrorLevel::Error,
                                domain: XMLErrorDomain::XSDParser,
                                line: self.locator.line(),
                                column: self.locator.column(),
                                system_id: self.locator.system_id(),
                                public_id: self.locator.public_id(),
                                message: Cow::Borrowed(msg),
                            });
                            self.skip_depth += 1;
                            return;
                        }
                        (Some(name), None) => {
                            let form = get_qualification_form(atts, "form")
                                .unwrap_or(self.elem_form_default);
                            let value_constraint = self.get_element_default(atts);
                            self.unnamed += 1;
                            let name = Arc::<str>::from(name);
                            let target_namespace = self
                                .target_namespace
                                .clone()
                                .filter(|_| matches!(form, QualificationForm::Qualified));
                            let particle = Particle {
                                min_occurs,
                                max_occurs,
                                term: Term::ElementDecl(target_namespace.clone(), name.clone()),
                            };
                            let scope = self.get_scope();
                            self.ed_stack.push(ElementDeclaration {
                                name,
                                target_namespace,
                                type_definition: r#type.unwrap_or_default(),
                                scope,
                                value_constraint,
                                nillable,
                                idc_definitions: vec![],
                                substitution_group_affiliation: substitution_group,
                                substitution_group_exclusions: r#final,
                                disallowed_substitutions: block,
                                r#abstract,
                                ns_context: self.ns_stack.clone(),
                            });
                            if let Some(pt) = self.pt_stack.last_mut()
                                && let Term::ModelGroup(mg) = &mut pt.term
                            {
                                mg.particles.push(particle);
                            }
                        }
                        (None, Some((ns, local))) => {
                            // push dummy data
                            self.ed_stack.push(Default::default());
                            let particle = Particle {
                                min_occurs,
                                max_occurs,
                                term: Term::ElementDecl(ns, local),
                            };
                            if let Some(pt) = self.pt_stack.last_mut()
                                && let Term::ModelGroup(mg) = &mut pt.term
                            {
                                mg.particles.push(particle);
                            }
                        }
                    }
                }
                self.element_stack.push(SCHEMA_ELEMENT_NAME);
            }
            SCHEMA_ATTRIBUTE_NAME => {
                macro_rules! check_unallowed_xmlns {
                    ($name:expr) => {
                        if $name == "xmlns" {
                            // [no-xmlns]
                            self.holder.error(SAXParseError {
                                error: XMLError::XSDError(XSDError::NoXmlns),
                                level: XMLErrorLevel::Error,
                                domain: XMLErrorDomain::XSDParser,
                                line: self.locator.line(),
                                column: self.locator.column(),
                                system_id: self.locator.system_id(),
                                public_id: self.locator.public_id(),
                                message: Cow::Borrowed(
                                    "The attribute declaration 'xmlns' is not allowed.",
                                ),
                            });
                            self.skip_depth += 1;
                            return;
                        }
                    };
                }
                macro_rules! check_unallowed_xsi {
                    ($target_namespace:expr) => {
                        if $target_namespace == Some(XML_SCHEMA_INSTANCE_NAMESPACE) {
                            // [no-xsi]
                            let msg = format!(
                                "The attribute declaration whose target name space is '{}' is not allowed.",
                                XML_SCHEMA_INSTANCE_NAMESPACE
                            );
                            self.holder.error(SAXParseError {
                                error: XMLError::XSDError(XSDError::NoXsi),
                                level: XMLErrorLevel::Error,
                                domain: XMLErrorDomain::XSDParser,
                                line: self.locator.line(),
                                column: self.locator.column(),
                                system_id: self.locator.system_id(),
                                public_id: self.locator.public_id(),
                                message: Cow::Owned(msg),
                            });
                            self.skip_depth += 1;
                            return;
                        }

                    };
                }

                let value_constraint = self.get_attribute_default(atts);
                if self.element_stack.last() == Some(&SCHEMA_SCHEMA_NAME) {
                    // top-level attribute declaration
                    let Some(name) = atts.get_value_by_qname("name") else {
                        self.skip_depth += 1;
                        return;
                    };
                    check_unallowed_xmlns!(name);
                    check_unallowed_xsi!(self.target_namespace.as_deref());
                    // For the {target_namespace} of a top-level attribute declaration,
                    // the value of "form" must not be used.
                    self.attribute_decl.target_namespace = self.target_namespace.clone();
                    self.attribute_decl.name = name.into();
                    if let Some(type_definition) = self.get_qname(atts, "type") {
                        self.attribute_decl.type_definition = type_definition;
                    }
                    self.attribute_decl.scope = Some(Scope::Global);
                    self.attribute_decl.value_constraint = value_constraint;
                    self.attribute_decl.ns_context = self.ns_stack.clone();
                } else {
                    let r#use = get_occurrence(atts, "use").unwrap_or(Occurrence::Optional);
                    if matches!(value_constraint, Some(ValueConstraint::Default(_)))
                        && !matches!(r#use, Occurrence::Optional)
                    {
                        let msg = "If <attribute> has both 'default' and 'use', 'use' must have \"optional\".";
                        self.holder.error(SAXParseError {
                            error: XMLError::XSDError(XSDError::SrcElement),
                            level: XMLErrorLevel::Error,
                            domain: XMLErrorDomain::XSDParser,
                            line: self.locator.line(),
                            column: self.locator.column(),
                            system_id: self.locator.system_id(),
                            public_id: self.locator.public_id(),
                            message: Cow::Borrowed(msg),
                        });
                    }
                    if let Some((ns, local)) = self.get_qname(atts, "ref") {
                        // local attribute declaration ('ref' is present)
                        let attuse = AttributeUse {
                            required: r#use,
                            attribute_declaration: (ns, local),
                            value_constraint,
                        };
                        self.push_attribute_use(attuse);
                    } else if let Some(name) = atts.get_value_by_qname("name") {
                        // local attribute declaration ('ref' is absent)

                        check_unallowed_xmlns!(name);

                        let form =
                            get_qualification_form(atts, "form").unwrap_or(self.attr_form_default);
                        let target_namespace = self
                            .target_namespace
                            .clone()
                            .filter(|_| matches!(form, QualificationForm::Qualified));

                        check_unallowed_xsi!(target_namespace.as_deref());

                        let name = Arc::<str>::from(name);
                        self.attribute_decl.target_namespace = target_namespace.clone();
                        self.attribute_decl.name = name.clone();
                        if let Some(type_definition) = self.get_qname(atts, "type") {
                            self.attribute_decl.type_definition = type_definition;
                        }
                        self.attribute_decl.scope = self.get_scope();
                        self.attribute_decl.value_constraint = None;

                        let attuse = AttributeUse {
                            required: r#use,
                            attribute_declaration: (target_namespace, name),
                            value_constraint,
                        };
                        self.push_attribute_use(attuse);
                    } else {
                        // [src-attribute]
                        // 3 If the item's parent is not <schema>, then all of the following must be true:
                        //   3.1 One of ref or name must be present, but not both.
                        let msg = "A local attribute declaration must have one of the attribute 'name' or 'ref'.";
                        self.holder.error(SAXParseError {
                            error: XMLError::XSDError(XSDError::SrcAttribute),
                            level: XMLErrorLevel::Error,
                            domain: XMLErrorDomain::XSDParser,
                            line: self.locator.line(),
                            column: self.locator.column(),
                            system_id: self.locator.system_id(),
                            public_id: self.locator.public_id(),
                            message: Cow::Borrowed(msg),
                        });
                        self.skip_depth += 1;
                        return;
                    }
                }
                self.element_stack.push(SCHEMA_ATTRIBUTE_NAME);
            }
            SCHEMA_NOTATION_NAME => {
                let Some(name) = atts.get_value_by_qname("name").map(Arc::<str>::from) else {
                    self.skip_depth += 1;
                    return;
                };
                let public_id = atts.get_value_by_qname("public").map(From::from);
                let system_id = atts
                    .get_value_by_qname("system")
                    .and_then(|sys| URIString::parse(sys).ok())
                    .map(From::from);
                self.schema.notations.insert(
                    (self.target_namespace.clone(), name.clone()),
                    Notation {
                        name,
                        target_namespace: self.target_namespace.clone(),
                        system_id,
                        public_id,
                    },
                );
                self.element_stack.push(SCHEMA_NOTATION_NAME);
            }
            SCHEMA_ALL_NAME => {
                let min_occurs = get_occurs(atts, "minOccurs").unwrap_or(1);
                let max_occurs = get_occurs(atts, "maxOccurs").unwrap_or(1);
                let model = ModelGroup {
                    compositor: Compositor::All,
                    particles: vec![],
                };
                let particle = Particle {
                    min_occurs,
                    max_occurs,
                    term: Term::ModelGroup(model),
                };
                self.pt_stack.push(particle);
                self.element_stack.push(SCHEMA_ALL_NAME);
            }
            SCHEMA_CHOICE_NAME => {
                let min_occurs = get_occurs(atts, "minOccurs").unwrap_or(1);
                let max_occurs = get_occurs(atts, "maxOccurs").unwrap_or(1);
                let model = ModelGroup {
                    compositor: Compositor::Choice,
                    particles: vec![],
                };
                let particle = Particle {
                    min_occurs,
                    max_occurs,
                    term: Term::ModelGroup(model),
                };
                self.pt_stack.push(particle);
                self.element_stack.push(SCHEMA_CHOICE_NAME);
            }
            SCHEMA_SEQUENCE_NAME => {
                let min_occurs = get_occurs(atts, "minOccurs").unwrap_or(1);
                let max_occurs = get_occurs(atts, "maxOccurs").unwrap_or(1);
                let model = ModelGroup {
                    compositor: Compositor::Sequence,
                    particles: vec![],
                };
                let particle = Particle {
                    min_occurs,
                    max_occurs,
                    term: Term::ModelGroup(model),
                };
                self.pt_stack.push(particle);
                self.element_stack.push(SCHEMA_SEQUENCE_NAME);
            }
            SCHEMA_ANY_NAME => {
                let min_occurs = get_occurs(atts, "minOccurs").unwrap_or(1);
                let max_occurs = get_occurs(atts, "maxOccurs").unwrap_or(1);
                let process_contents =
                    get_process_contents(atts, "processContents").unwrap_or(ProcessContent::Strict);
                let ns_constraint = self
                    .get_namespace_constraint(atts, "namespace")
                    .unwrap_or(NamespaceConstraint::Any);
                let particle = Particle {
                    min_occurs,
                    max_occurs,
                    term: Term::Wildcard(Wildcard {
                        ns_constraint,
                        process_contents,
                    }),
                };
                if let Some(pt) = self.pt_stack.last_mut()
                    && let Term::ModelGroup(mg) = &mut pt.term
                {
                    mg.particles.push(particle);
                }
                self.element_stack.push(SCHEMA_ANY_NAME);
            }
            SCHEMA_ANYATT_NAME => {
                let process_contents =
                    get_process_contents(atts, "processContents").unwrap_or(ProcessContent::Strict);
                let ns_constraint = self
                    .get_namespace_constraint(atts, "namespace")
                    .unwrap_or(NamespaceConstraint::Any);
                let wildcard = Wildcard {
                    ns_constraint,
                    process_contents,
                };
                if self.element_stack.last().copied() == Some(SCHEMA_ATTGROUP_NAME) {
                    self.attr_group_def.attribute_wildcard = Some(wildcard);
                } else if let Some(ct) = self.ct_stack.last_mut() {
                    ct.attribute_wildcard = Some(wildcard);
                }
                self.element_stack.push(SCHEMA_ANYATT_NAME);
            }
            SCHEMA_UNIQUE_NAME => {
                let Some(name) = atts.get_value_by_qname("name") else {
                    self.skip_depth += 1;
                    return;
                };
                self.idc.name = name.into();
                self.idc.target_namespace = self.target_namespace.clone();
                self.idc.idc_category = IDCCategory::Unique;
                self.element_stack.push(SCHEMA_UNIQUE_NAME);
            }
            SCHEMA_KEY_NAME => {
                let Some(name) = atts.get_value_by_qname("name") else {
                    self.skip_depth += 1;
                    return;
                };
                self.idc.name = name.into();
                self.idc.target_namespace = self.target_namespace.clone();
                self.idc.idc_category = IDCCategory::Key;
                self.element_stack.push(SCHEMA_KEY_NAME);
            }
            SCHEMA_KEYREF_NAME => {
                let Some(name) = atts.get_value_by_qname("name") else {
                    self.skip_depth += 1;
                    return;
                };
                let Some((ref_ns, ref_loc)) = self.get_qname(atts, "refer") else {
                    self.skip_depth += 1;
                    return;
                };
                self.idc.name = name.into();
                self.idc.target_namespace = self.target_namespace.clone();
                self.idc.idc_category = IDCCategory::Keyref;
                self.idc.referenced_key = Some((ref_ns, ref_loc));
                self.element_stack.push(SCHEMA_KEYREF_NAME);
            }
            SCHEMA_SELECTOR_NAME => {
                let Some(xpath) = atts.get_value_by_qname("xpath") else {
                    self.skip_depth += 1;
                    return;
                };
                self.idc.selector = xpath.into();
                self.element_stack.push(SCHEMA_SELECTOR_NAME);
            }
            SCHEMA_FIELD_NAME => {
                let Some(xpath) = atts.get_value_by_qname("xpath") else {
                    self.skip_depth += 1;
                    return;
                };
                self.idc.fields.push(xpath.into());
                self.element_stack.push(SCHEMA_FIELD_NAME);
            }
            SCHEMA_ANNOTATION_NAME | SCHEMA_APPINFO_NAME | SCHEMA_DOCUMENTATION_NAME => {
                // Skip these as they are unnecessary.
                // Since schema validation is handled by the meta-schema, and the
                // annotation information is passed to the child handler, users can
                // retrieve the necessary information.
                self.skip_depth += 1;
            }
            _ => {
                // An error should be reported, but that is left
                // to the meta schema validator. Skip simply.
                self.skip_depth += 1;
            }
        }
    }

    fn end_element(&mut self, namespace_name: Option<&str>, local_name: Option<&str>, qname: &str) {
        self.holder
            .child
            .end_element(namespace_name, local_name, qname);

        if self.skip_depth > 0 {
            self.skip_depth -= 1;
            return;
        }

        assert_eq!(self.element_stack.pop(), local_name);

        let local_name = local_name.unwrap();
        match local_name {
            SCHEMA_REDEFINE_NAME => self.redefined_schema_location = None,
            SCHEMA_SIMPLETYPE_NAME => {
                if let Some(st) = self.st_stack.pop() {
                    let target_namespace = st.target_namespace.clone();
                    let name = st.name.clone();
                    if let Some(pst) = self.st_stack.last_mut() {
                        if self.element_stack.last() == Some(&SCHEMA_RESTRICTION_NAME) {
                            pst.base_type_definition = (target_namespace.clone(), name.clone());
                        } else if self.element_stack.last() == Some(&SCHEMA_LIST_NAME) {
                            pst.variety =
                                Some(Variety::List(target_namespace.clone(), name.clone()));
                        } else if self.element_stack.last() == Some(&SCHEMA_UNION_NAME) {
                            if let Some(Variety::Union(member)) = pst.variety.as_mut() {
                                *member =
                                    [member.as_ref(), &[(target_namespace.clone(), name.clone())]]
                                        .concat()
                                        .into();
                            } else {
                                pst.variety = Some(Variety::Union(
                                    vec![(target_namespace.clone(), name.clone())].into(),
                                ));
                            }
                        }
                    }
                    if self.element_stack.last() == Some(&SCHEMA_ATTRIBUTE_NAME) {
                        if !self.attribute_decl.type_definition.1.is_empty() {
                            // [src-attribute] clause 4
                            // 4 type and <simpleType> must not both be present.
                            let msg = "'type' and <simpleType> must not both be present under <attribute>.";
                            self.holder.error(SAXParseError {
                                error: XMLError::XSDError(XSDError::SrcAttribute),
                                level: XMLErrorLevel::Error,
                                domain: XMLErrorDomain::XSDParser,
                                line: self.locator.line(),
                                column: self.locator.column(),
                                system_id: self.locator.system_id(),
                                public_id: self.locator.public_id(),
                                message: Cow::Borrowed(msg),
                            });
                        } else if self.attribute_decl.name.is_empty() {
                            // [src-attribute]
                            // 3.2 If ref is present, then all of <simpleType>, form and type must be absent.
                            let msg = "If <attribute> has 'ref', then <simpleType> must be absent.";
                            self.holder.error(SAXParseError {
                                error: XMLError::XSDError(XSDError::SrcAttribute),
                                level: XMLErrorLevel::Error,
                                domain: XMLErrorDomain::XSDParser,
                                line: self.locator.line(),
                                column: self.locator.column(),
                                system_id: self.locator.system_id(),
                                public_id: self.locator.public_id(),
                                message: Cow::Borrowed(msg),
                            });
                        }
                        self.attribute_decl.type_definition =
                            (target_namespace.clone(), name.clone());
                    } else if matches!(
                        self.element_stack[..],
                        [.., SCHEMA_SIMPLECONT_NAME, SCHEMA_RESTRICTION_NAME]
                    ) {
                        // local simple type under <simpleContent>
                        if let Some(st) = self.st_stack.last_mut() {
                            // component property mapping of {content type} clause 1.1
                            // 1.1 the simple type definition corresponding to the <simpleType>
                            //     among the [children] of <restriction> if there is one;
                            st.base_type_definition = (target_namespace.clone(), name.clone());
                        }
                    } else if self.element_stack.last() == Some(&SCHEMA_REDEFINE_NAME) {
                        let target_namespace = st.target_namespace.clone();
                        let name = st.name.clone();
                        if st.base_type_definition != (target_namespace, name) {
                            // [src-redefine] clause 5
                            let msg = "The base type of redefined simple type definition must match to its own name.";
                            self.holder.error(SAXParseError {
                                error: XMLError::XSDError(XSDError::SrcRedefine),
                                level: XMLErrorLevel::Error,
                                domain: XMLErrorDomain::XSDParser,
                                line: self.locator.line(),
                                column: self.locator.column(),
                                system_id: self.locator.system_id(),
                                public_id: self.locator.public_id(),
                                message: Cow::Borrowed(msg),
                            });
                        }
                    }
                    if self.in_redefine() {
                        if let Some(loc) = self.redefined_schema_location.clone() {
                            self.redefines
                                .entry(loc)
                                .or_default()
                                .type_defs
                                .insert((target_namespace, name), OwnedTypeDefinition::Simple(st));
                        }
                    } else {
                        self.schema
                            .type_defs
                            .insert((target_namespace, name), TypeDefinition::Simple(st.into()));
                    }
                }
            }
            SCHEMA_RESTRICTION_NAME
                if self.element_stack.last() == Some(&SCHEMA_SIMPLETYPE_NAME) =>
            {
                if self
                    .st_stack
                    .last()
                    .is_some_and(|st| st.base_type_definition.1.is_empty())
                {
                    // [src-restriction-base-or-simpleType]
                    // If there is no "base" attribute, there
                    // must be a child of type <simpleType>.
                    let msg = "<restriction> must have either the attribute 'base' or the child <simpleType>.";
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::SrcRestrictionBaseOrSimpleType),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Borrowed(msg),
                    });
                }
            }
            SCHEMA_LIST_NAME => {
                if self.st_stack.last().is_some_and(|st| st.variety.is_none()) {
                    // [src-list-itemType-or-simpleType]
                    // If there is no "itemType" attribute, there
                    // must be a child of type <simpleType>.
                    let msg = "<list> must have either the attribute 'itemType' or the child <simpleType>.";
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::SrcListItemTypeOrSimpleType),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Borrowed(msg),
                    });
                }
            }
            SCHEMA_UNION_NAME => {
                if self.st_stack.last().is_some_and(|st| st.variety.is_none()) {
                    // [src-union-memberTypes-or-simpleTypes]
                    // If there is no `memberType` attribute, there
                    // must be children of type `simpleType`.
                    let msg = "<union> must have either the non-empty attribute 'memberType' or the child <simpleType>s.";
                    self.holder.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::SrcUnionMemberTypesOrSimpleTypes),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDParser,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Borrowed(msg),
                    });
                }
            }
            SCHEMA_COMPLEXTYPE_NAME => {
                if let Some(mut ct) = self.ct_stack.pop() {
                    if ct.base_type_definition.0.is_none() && ct.base_type_definition.1.is_empty() {
                        // Treat this as a case where neither <simpleContent> nor
                        // <complexContent> was selected, and consider it as if
                        // <complexContent> with the ur-type definition as its base
                        // type had been selected.
                        ct.base_type_definition.0 = XSD_ANY_TYPE.target_namespace.clone();
                        ct.base_type_definition.1 = XSD_ANY_TYPE.name.clone();
                    }
                    let target_namespace = ct.target_namespace.clone();
                    let name = ct.name.clone();
                    if self.element_stack.last() == Some(&SCHEMA_REDEFINE_NAME)
                        && (ct.base_type_definition.0 != target_namespace
                            || ct.base_type_definition.1 != name)
                    {
                        // [src-redefine] clause 5
                        let msg = "The base type of redefined complex type definition must match to its own name.";
                        self.holder.error(SAXParseError {
                            error: XMLError::XSDError(XSDError::SrcRedefine),
                            level: XMLErrorLevel::Error,
                            domain: XMLErrorDomain::XSDParser,
                            line: self.locator.line(),
                            column: self.locator.column(),
                            system_id: self.locator.system_id(),
                            public_id: self.locator.public_id(),
                            message: Cow::Borrowed(msg),
                        });
                    }
                    if self.in_redefine() {
                        if let Some(loc) = self.redefined_schema_location.clone() {
                            self.redefines
                                .entry(loc)
                                .or_default()
                                .type_defs
                                .insert((target_namespace, name), OwnedTypeDefinition::Complex(ct));
                        }
                    } else {
                        self.schema
                            .type_defs
                            .insert((target_namespace, name), TypeDefinition::Complex(ct.into()));
                    }
                }
            }
            SCHEMA_SIMPLECONT_NAME => {
                // pop dummy decl
                if let Some(st) = self.st_stack.pop()
                    && let Some(ct) = self.ct_stack.last_mut()
                {
                    let target_namespace = st.target_namespace.clone();
                    let name = st.name.clone();
                    if matches!(ct.derivation_method, DerivationMethod::Restriction) {
                        ct.content_type =
                            ContentType::Simple(target_namespace.clone(), name.clone());
                        self.schema.type_defs.insert(
                            (target_namespace, name),
                            TypeDefinition::Simple(Arc::new(st)),
                        );
                    }
                }
            }
            SCHEMA_GROUP_NAME
                if matches!(
                    self.element_stack.last().copied(),
                    Some(SCHEMA_SCHEMA_NAME | SCHEMA_REDEFINE_NAME)
                ) =>
            {
                // top-level model group definition
                // 1. pop dummy particle
                // 2. extract model group of dummy particle
                // 3. extract actual child particle
                // 4. extract model group of actual child particle
                if let Some(pt) = self.pt_stack.pop()
                    && let Term::ModelGroup(mut mg) = pt.term
                    && let Some(pt) = mg.particles.pop()
                    && let Term::ModelGroup(mg) = pt.term
                {
                    self.model_group_def.model_group = mg;
                    let target_namespace = self.model_group_def.target_namespace.clone();
                    let name = self.model_group_def.name.clone();
                    let def = std::mem::take(&mut *self.model_group_def);
                    if self.in_redefine() {
                        if let Some(loc) = self.redefined_schema_location.clone() {
                            self.redefines
                                .entry(loc)
                                .or_default()
                                .modelgroup_defs
                                .insert((target_namespace, name), def);
                        }
                    } else {
                        self.schema
                            .modelgroups
                            .insert((target_namespace, name), def);
                    }
                }

                // Since SRC checks related to <redefine> cannot be performed until after the
                // schema to be <redefine>d has been built, we will not take any action here.
            }
            SCHEMA_ATTGROUP_NAME => {
                if matches!(
                    self.element_stack.last().copied(),
                    Some(SCHEMA_SCHEMA_NAME | SCHEMA_REDEFINE_NAME)
                ) {
                    let target_namespace = self.attr_group_def.target_namespace.clone();
                    let name = self.attr_group_def.name.clone();
                    let key = (target_namespace, name);
                    if self.element_stack.last() == Some(&SCHEMA_REDEFINE_NAME) {
                        let count = self
                            .attr_group_def
                            .attribute_group_refs
                            .iter()
                            .filter(|r| r == &&key)
                            .count();
                        if count > 1 {
                            // [src-redefine] clause 7.1
                            self.holder.error(SAXParseError {
                                error: XMLError::XSDError(XSDError::SrcRedefine),
                                level: XMLErrorLevel::Error,
                                domain: XMLErrorDomain::XSDParser,
                                line: self.locator.line(),
                                column: self.locator.column(),
                                system_id: self.locator.system_id(),
                                public_id: self.locator.public_id(),
                                message: Cow::Borrowed("Redefined attribute group definition must not contain two or more self-references."),
                            });
                        }
                        // The case where `count == 0` (clause 7.2) cannot be validated
                        // until after the schema to be redefined has been constructed.
                    }
                    let def = std::mem::take(&mut *self.attr_group_def);
                    if self.in_redefine() {
                        if let Some(loc) = self.redefined_schema_location.clone() {
                            self.redefines
                                .entry(loc)
                                .or_default()
                                .attgroup_defs
                                .insert(key, def);
                        }
                    } else {
                        self.schema.attgroups.insert(key, def);
                    }
                }
            }
            SCHEMA_ELEMENT_NAME => {
                if let Some(decl) = self.ed_stack.pop()
                    && !decl.name.is_empty()
                {
                    let target_namespace = decl.target_namespace.clone();
                    let name = decl.name.clone();
                    if !matches!(decl.scope, Some(Scope::Global)) {
                        self.unnamed += 1;
                        let name = format!("#local{}/{}", self.unnamed, name).into();
                        self.schema
                            .elem_decls
                            .insert((target_namespace, name), decl);
                    } else {
                        self.schema
                            .elem_decls
                            .insert((target_namespace, name), decl);
                    }
                }
            }
            SCHEMA_ATTRIBUTE_NAME => {
                if !self.attribute_decl.name.is_empty() {
                    // 3.2.2 XML Representation of Attribute Declaration Schema Components
                    // The default when no simple type definition is referenced or provided is
                    // the ·simple ur-type definition·, which imposes no constraints at all.
                    self.attribute_decl.type_definition =
                        (Some(XML_SCHEMA_NAMESPACE.into()), "anySimpleType".into());

                    let target_namespace = self.attribute_decl.target_namespace.clone();
                    let name = self.attribute_decl.name.clone();
                    self.schema.att_decls.insert(
                        (target_namespace, name),
                        std::mem::take(&mut *self.attribute_decl),
                    );
                }
            }
            SCHEMA_GROUP_NAME | SCHEMA_ALL_NAME | SCHEMA_CHOICE_NAME | SCHEMA_SEQUENCE_NAME => {
                if let Some(pt) = self.pt_stack.pop() {
                    // model group reference
                    if let Some(bpt) = self.pt_stack.last_mut()
                        && let Term::ModelGroup(mg) = &mut bpt.term
                    {
                        // If a parent particle exists, push to its child particle.
                        mg.particles.push(pt);
                    } else {
                        // If not, this is the child of <complexType>, <restriction> or <extension>.
                        // (in other words, this this particle is used for {content type} of a complex type)
                        if let Some(ct) = self.ct_stack.last_mut() {
                            if self.element_stack.last() == Some(&SCHEMA_COMPLEXTYPE_NAME) {
                                // Neither <simpleContent> nor <complexContent> appears under <complexType>.
                                // In this case, it is assumed that ur-type is considered to be restricted.
                                //
                                // Reference: 3.4.2 XML Representation of Complex Type Definitions
                                // > The property mappings below are also used in the case where the
                                // > third alternative (neither <simpleContent> nor <complexContent>)
                                // > is chosen. This case is understood as shorthand for complex
                                // > content restricting the ·ur-type definition·, and the details
                                // > of the mappings should be modified as necessary.
                                ct.base_type_definition = XSD_ANY_TYPE.base_type_definition.clone();
                                ct.derivation_method = DerivationMethod::Restriction;
                            }

                            // component property mapping of {content type}
                            // clause 2 (effective content)
                            let effective_mixed = match &ct.content_type {
                                ContentType::Pending(_, mixed) => *mixed,
                                ContentType::Mixed(_) => true,
                                _ => false,
                            };
                            let effective_content = match &pt.term {
                                Term::ModelGroup(model) => {
                                    // <all>, <choice> or <sequence>
                                    if (matches!(
                                        model.compositor,
                                        Compositor::All | Compositor::Sequence
                                    ) || (matches!(model.compositor, Compositor::Choice)
                                        && pt.min_occurs == 0))
                                        && model.particles.is_empty()
                                    {
                                        // clause 2.1.2, 2.1.3, 2.1.4, 2.1.5
                                        effective_mixed.then_some(Particle {
                                            min_occurs: 1,
                                            max_occurs: 1,
                                            term: Term::ModelGroup(ModelGroup {
                                                compositor: Compositor::Sequence,
                                                particles: vec![],
                                            }),
                                        })
                                    } else {
                                        // clause 2.2
                                        Some(pt)
                                    }
                                }
                                Term::ModelGroupRef(_, _) => {
                                    // <group ref=...>
                                    // clause 2.2
                                    Some(pt)
                                }
                                Term::ElementDecl(_, _) | Term::Wildcard(_) => {
                                    // forbidden. simply ignore (treat as empty)
                                    None
                                }
                            };

                            // component property mapping of {content type}
                            // clause 3
                            match ct.derivation_method {
                                DerivationMethod::Restriction => {
                                    // clause 3.1
                                    ct.content_type =
                                        if let Some(effective_content) = effective_content {
                                            // clause 3.1.2
                                            if effective_mixed {
                                                ContentType::Mixed(effective_content)
                                            } else {
                                                ContentType::ElementOnly(effective_content)
                                            }
                                        } else {
                                            // clause 3.1.1
                                            ContentType::Empty
                                        };
                                }
                                DerivationMethod::Extension => {
                                    // clause 3.2
                                    // Since the base type must be clear,
                                    // this cannot be resolved at this point.
                                    ct.content_type =
                                        ContentType::Pending(effective_content, effective_mixed);
                                }
                            }
                        }
                    }
                }
            }
            SCHEMA_UNIQUE_NAME | SCHEMA_KEY_NAME | SCHEMA_KEYREF_NAME => {
                if let Some(decl) = self.ed_stack.last_mut() {
                    decl.idc_definitions.push(std::mem::take(&mut *self.idc));
                }
            }
            _ => {}
        }
    }

    fn characters(&mut self, data: &str) {
        self.holder.child.characters(data);
    }

    fn start_prefix_mapping(&mut self, prefix: Option<&str>, namespace_name: &str) {
        self.holder
            .child
            .start_prefix_mapping(prefix, namespace_name);
        self.ns_stack
            .push(prefix.unwrap_or_default(), namespace_name);
    }

    fn end_prefix_mapping(&mut self, prefix: Option<&str>) {
        self.holder.child.end_prefix_mapping(prefix);
        // Although the namespace that is actually popped may not bind the specified prefix,
        // it doesn't matter because everything works out by the time the other callbacks are
        // invoked.
        self.ns_stack.pop();
    }

    fn end_document(&mut self) {
        assert!(self.ct_stack.is_empty());
        assert!(self.st_stack.is_empty());
        assert!(self.ed_stack.is_empty());
        assert!(self.element_stack.is_empty(), "{:?}", self.element_stack);
        assert!(self.pt_stack.is_empty(), "{:?}", self.pt_stack);

        // Before processing <include>, <redefine>, or <import>, notify the
        // handler that the current schema document has been fully parsed.
        self.holder.child.end_document();

        let base_uri = self.locator.system_id();
        self.schema.merged_uris.insert(base_uri.clone());
        // Since the handler will be reused, save the lists.
        self.includes.sort_unstable();
        self.includes.dedup();
        self.imports.sort_unstable();
        self.imports.dedup();
        let includes = std::mem::take(&mut self.includes);
        let imports = std::mem::take(&mut self.imports);
        let redefines = std::mem::take(&mut self.redefines);
        if includes.is_empty() && imports.is_empty() && redefines.is_empty() {
            return;
        }
        let mut meta = (!self.meta).then(XSDSchema::parse_meta_schema);
        // process <include>
        for include in includes {
            self.process_include(include, meta.as_mut());
        }
        // process <import>
        for (namespace, import) in imports {
            self.process_import(import, namespace, meta.as_mut());
        }
        // process <redefine>
        for (location, components) in redefines {
            self.process_redefine(location, components, meta.as_mut());
        }
        if matches!(self.inclusion_state, InclusionState::TopLevel) {
            // In accordance with the module dependencies,
            // start by checking the modules with no dependencies.
            // If an error occurs during a previous module check,
            // subsequent checks are skipped because a component may be missing.
            // Reference: Part 1 Appendix E Schema Components Diagram (non-normative)
            self.holder.last_error = self
                .check_all_simple_type_derivation()
                .and_then(|_| self.check_all_attdecl())
                .and_then(|_| self.check_all_attgroup())
                .and_then(|_| self.check_all_complex_type_derivation())
                .and_then(|_| self.check_all_elemdecl())
                .and_then(|_| self.holder.last_error.clone());
        }
        if let Some(meta) = meta {
            META_SCHEMA_HOLDER_V1_0.with_borrow_mut(|holder| holder.push(meta));
        }
    }

    // unused callbacks

    fn attribute_decl(
        &mut self,
        element_name: &str,
        attribute_name: &str,
        attribute_type: &AttributeType,
        default_decl: &DefaultDecl,
    ) {
        self.holder.child.attribute_decl(
            element_name,
            attribute_name,
            attribute_type,
            default_decl,
        );
    }
    fn comment(&mut self, data: &str) {
        self.holder.child.comment(data);
    }
    fn declaration(&mut self, version: &str, encoding: Option<&str>, standalone: Option<bool>) {
        self.holder.child.declaration(version, encoding, standalone);
    }
    fn element_decl(&mut self, name: &str, contentspec: &crate::sax::ContentSpec) {
        self.holder.child.element_decl(name, contentspec);
    }
    fn start_cdata(&mut self) {
        self.holder.child.start_cdata();
    }
    fn end_cdata(&mut self) {
        self.holder.child.end_cdata();
    }
    fn start_document(&mut self) {
        self.holder.child.start_document();
    }
    fn start_dtd(&mut self, name: &str, public_id: Option<&str>, system_id: Option<&URIStr>) {
        self.holder.child.start_dtd(name, public_id, system_id);
    }
    fn end_dtd(&mut self) {
        self.holder.child.end_dtd();
    }
    fn start_entity(&mut self, name: &str) {
        self.holder.child.start_entity(name);
    }
    fn end_entity(&mut self) {
        self.holder.child.end_entity();
    }
    fn external_entity_decl(&mut self, name: &str, public_id: Option<&str>, system_id: &URIStr) {
        self.holder
            .child
            .external_entity_decl(name, public_id, system_id);
    }
    fn ignorable_whitespace(&mut self, data: &str) {
        self.holder.child.ignorable_whitespace(data);
    }
    fn internal_entity_decl(&mut self, name: &str, value: &str) {
        self.holder.child.internal_entity_decl(name, value);
    }
    fn notation_decl(&mut self, name: &str, public_id: Option<&str>, system_id: Option<&URIStr>) {
        self.holder.child.notation_decl(name, public_id, system_id);
    }
    fn processing_instruction(&mut self, target: &str, data: Option<&str>) {
        self.holder.child.processing_instruction(target, data);
    }
    fn skipped_entity(&mut self, name: &str) {
        self.holder.child.skipped_entity(name);
    }
    fn unparsed_entity_decl(
        &mut self,
        name: &str,
        public_id: Option<&str>,
        system_id: &URIStr,
        notation_name: &str,
    ) {
        self.holder
            .child
            .unparsed_entity_decl(name, public_id, system_id, notation_name);
    }
}

fn get_boolean(atts: &Attributes, name: &str) -> Option<bool> {
    atts.get_value_by_qname(name).and_then(|value| match value {
        "true" | "1" => Some(true),
        "false" | "0" => Some(false),
        _ => None,
    })
}

fn get_qualification_form(atts: &Attributes, name: &str) -> Option<QualificationForm> {
    let value = atts.get_value_by_qname(name)?;
    match value {
        "qualified" => Some(QualificationForm::Qualified),
        "unqualified" => Some(QualificationForm::Unqualified),
        _ => None,
    }
}

fn get_final(atts: &Attributes, name: &str) -> Option<FinalSet> {
    let value = atts.get_value_by_qname(name)?;
    if value == "#all" {
        return Some(Final::Extension | Final::Restriction | Final::List | Final::Union);
    }

    let mut ret = FinalSet::default();
    for fin in value.split_ascii_whitespace() {
        match fin {
            "extension" => ret |= Final::Extension,
            "restriction" => ret |= Final::Restriction,
            "list" => ret |= Final::List,
            "union" => ret |= Final::Union,
            _ => {}
        }
    }
    Some(ret)
}

fn get_block(atts: &Attributes, name: &str) -> Option<BlockSet> {
    let value = atts.get_value_by_qname(name)?;
    if value == "#all" {
        return Some(Block::Extension | Block::Restriction | Block::Substitution);
    }

    let mut ret = BlockSet::default();
    for fin in value.split_ascii_whitespace() {
        match fin {
            "extension" => ret |= Block::Extension,
            "restriction" => ret |= Block::Restriction,
            "substitution" => ret |= Block::Substitution,
            _ => {}
        }
    }
    Some(ret)
}

/// Treat "unbounded" as `usize::MAX` and always allow "unbounded".  \
/// Errors for attributes that do not allow "unbounded" are reported at meta schema level.
fn get_occurs(atts: &Attributes, name: &str) -> Option<usize> {
    let value = atts.get_value_by_qname(name)?;
    if value == "unbounded" {
        Some(usize::MAX)
    } else {
        value.parse().ok()
    }
}

fn get_occurrence(atts: &Attributes, name: &str) -> Option<Occurrence> {
    let value = atts.get_value_by_qname(name)?;
    match value {
        "optional" => Some(Occurrence::Optional),
        "prohibited" => Some(Occurrence::Prohibited),
        "required" => Some(Occurrence::Required),
        _ => None,
    }
}

fn get_process_contents(atts: &Attributes, name: &str) -> Option<ProcessContent> {
    let value = atts.get_value_by_qname(name)?;
    match value {
        "lax" => Some(ProcessContent::Lax),
        "skip" => Some(ProcessContent::Skip),
        "strict" => Some(ProcessContent::Strict),
        _ => None,
    }
}

struct MetaSchemaParseHandler;
impl ErrorHandler for MetaSchemaParseHandler {
    fn error(&mut self, error: SAXParseError) {
        DefaultSAXHandler.error(error);
    }
    fn fatal_error(&mut self, error: SAXParseError) {
        DefaultSAXHandler.fatal_error(error);
    }
    fn warning(&mut self, error: SAXParseError) {
        DefaultSAXHandler.warning(error);
    }
}
impl EntityResolver for MetaSchemaParseHandler {
    fn resolve_entity(
        &mut self,
        _name: &str,
        _public_id: Option<&str>,
        base_uri: &URIStr,
        system_id: &URIStr,
    ) -> Result<InputSource<'static>, XMLError> {
        if system_id.as_escaped_str().ends_with("xml-compact.xsd") {
            Ok(InputSource::from_content(include_str!(
                "../../resources/xsd/xml-compact.xsd"
            )))
        } else {
            let uri = base_uri.resolve(system_id);
            Err(XMLError::IOError(
                std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    format!("resource '{uri}' is not found."),
                )
                .into(),
            ))
        }
    }
}
impl SAXHandler for MetaSchemaParseHandler {}

#[derive(Debug, Clone, Copy)]
pub enum XSDError {
    InternalError,
    UnallowedElement,
    UnallowedAttribute,
    UnallowedText,
    TooLargeOccurence,
    MissingSubComponent,
    RegexpError(RegexpError),
    Unsupported,

    // Schema Representation Constraints
    /// Schema Document Location Strategy
    SchemaReference,
    /// Annotation Definition Representation OK
    SrcAnnotation,
    /// Attribute Declaration Representation OK
    SrcAttribute,
    /// Attribute Group Definition Representation OK
    SrcAttributeGroup,
    /// Complex Type Definition Representation OK
    SrcCt,
    /// Element Declaration Representation OK
    SrcElement,
    /// Individual Component Redefinition
    SrcExpredef,
    /// Identity-constraint Definition Representation OK
    SrcIdentityConstraint,
    /// Import Constraints and Semantics
    SrcImport,
    /// Inclusion Constraints and Semantics
    SrcInclude,
    /// itemType attribute or simpleType child
    SrcListItemTypeOrSimpleType,
    /// Model Group Representation OK
    SrcModelGroup,
    /// Model Group Definition Representation OK
    SrcModelGroupDefn,
    /// Multiple enumerations
    SrcMultipleEnumerations,
    /// Multiple patterns
    SrcMultiplePatterns,
    /// Notation Definition Representation OK
    SrcNotation,
    /// QName Interpretation
    SrcQname,
    /// Redefinition Constraints and Semantics
    SrcRedefine,
    /// QName resolution (Schema Document)
    SrcResolve,
    /// base attribute or simpleType child
    SrcRestrictionBaseOrSimpleType,
    /// Simple Type Definition Representation OK
    SrcSimpleType,
    /// Single Facet Value
    SrcSingleFacetValue,
    /// memberTypes attribute or simpleType children
    SrcUnionMemberTypesOrSimpleTypes,
    /// Wildcard Representation OK
    SrcWildcard,

    // Schema Component Constraints
    /// Attribute Declaration Properties Correct
    APropsCorrect,
    /// Attribute Group Definition Properties Correct
    AgPropsCorrect,
    /// Annotation Correct
    AnPropsCorrect,
    /// Attribute Use Correct
    AuPropsCorrect,
    /// Fields Value OK
    CFieldsXpaths,
    /// Identity-constraint Definition Properties Correct
    CPropsCorrect,
    /// Selector Value OK
    CSelectorXpath,
    /// All Group Limited
    CosAllLimited,
    /// applicable facets
    CosApplicableFacets,
    /// Attribute Wildcard Intersection
    CosAwIntersect,
    /// Attribute Wildcard Union
    CosAwUnion,
    /// Effective Total Range (choice)
    CosChoiceRange,
    /// Type Derivation OK (Complex)
    CosCtDerivedOk,
    /// Derivation Valid (Extension)
    CosCtExtends,
    /// Element Declarations Consistent
    CosElementConsistent,
    /// Substitution Group
    CosEquivClass,
    /// Substitution Group OK (Transitive)
    CosEquivDerivedOkRec,
    /// Particle Emptiable
    CosGroupEmptiable,
    /// list of atomic
    CosListOfAtomic,
    /// no circular unions
    CosNoCircularUnions,
    /// Unique Particle Attribution
    CosNonambig,
    /// Wildcard Subset
    CosNsSubset,
    /// Particle Valid (Extension)
    CosParticleExtend,
    /// Particle Valid (Restriction)
    CosParticleRestrict,
    /// Effective Total Range (all and sequence)
    CosSeqRange,
    /// Type Derivation OK (Simple)
    CosStDerivedOk,
    /// Derivation Valid (Restriction, Simple)
    CosStRestricts,
    /// Element Default Valid (Immediate)
    CosValidDefault,
    /// Complex Type Definition Properties Correct
    CtPropsCorrect,
    /// Derivation Valid (Restriction, Complex)
    DerivationOkRestriction,
    /// Element Declaration Properties Correct
    EPropsCorrect,
    /// enumeration facet value required for NOTATION
    EnumerationRequiredNotation,
    /// enumeration valid restriction
    EnumerationValidRestriction,
    /// fractionDigits less than or equal to totalDigits
    FractionDigitsTotalDigits,
    /// fractionDigits valid restriction
    FractionDigitsValidRestriction,
    /// length and minLength or maxLength
    LengthMinLengthMaxLength,
    /// length valid restriction
    LengthValidRestriction,
    /// maxExclusive valid restriction
    MaxExclusiveValidRestriction,
    /// maxInclusive and maxExclusive
    MaxInclusiveMaxExclusive,
    /// maxInclusive valid restriction
    MaxInclusiveValidRestriction,
    /// maxLength valid restriction
    MaxLengthValidRestriction,
    /// Model Group Correct
    MgPropsCorrect,
    /// Model Group Definition Properties Correct
    MgdPropsCorrect,
    /// minExclusive <= maxExclusive
    MinExclusiveLessThanEqualToMaxExclusive,
    /// minExclusive < maxInclusive
    MinExclusiveLessThanMaxInclusive,
    /// minExclusive valid restriction
    MinExclusiveValidRestriction,
    /// minInclusive <= maxInclusive
    MinInclusiveLessThanEqualToMaxInclusive,
    /// minInclusive < maxExclusive
    MinInclusiveLessThanMaxExclusive,
    /// minInclusive and minExclusive
    MinInclusiveMinExclusive,
    /// minInclusive valid restriction
    MinInclusiveValidRestriction,
    /// minLength <= maxLength
    MinLengthLessThanEqualToMaxLength,
    /// minLength valid restriction
    MinLengthValidRestriction,
    /// Notation Declaration Correct
    NPropsCorrect,
    /// xmlns Not Allowed
    NoXmlns,
    /// xsi: Not Allowed
    NoXsi,
    /// Particle Correct
    PPropsCorrect,
    /// Occurrence Range OK
    RangeOk,
    /// Particle Derivation OK (Sequence:Choice -- MapAndSum)
    RcaseMapAndSum,
    /// Particle Restriction OK (Elt:Elt -- NameAndTypeOK)
    RcaseNameAndTypeOK,
    /// Particle Derivation OK (Elt:Any -- NSCompat)
    RcaseNSCompat,
    /// Particle Derivation OK (All/Choice/Sequence:Any -- NSRecurseCheckCardinality)
    RcaseNSRecurseCheckCardinality,
    /// Particle Derivation OK (Any:Any -- NSSubset)
    RcaseNSSubset,
    /// Particle Derivation OK (All:All,Sequence:Sequence -- Recurse)
    RcaseRecurse,
    /// Particle Derivation OK (Elt:All/Choice/Sequence -- RecurseAsIfGroup)
    RcaseRecurseAsIfGroup,
    /// Particle Derivation OK (Choice:Choice -- RecurseLax)
    RcaseRecurseLax,
    /// Particle Derivation OK (Sequence:All -- RecurseUnordered)
    RcaseRecurseUnordered,
    /// Schema Properties Correct
    SchPropsCorrect,
    /// Simple Type Definition Properties Correct
    StPropsCorrect,
    /// Simple Type Restriction (Facets)
    StRestrictFacets,
    /// totalDigits valid restriction
    TotalDigitsValidRestriction,
    /// Wildcard Properties Correct
    WPropsCorrect,
    /// whiteSpace valid restriction
    WhiteSpaceValidRestriction,

    // Validation Rules
    /// Schema-Validity Assessment (Attribute)
    CvcAssessAttr,
    /// Schema-Validity Assessment (Element)
    CvcAssessElt,
    /// Attribute Locally Valid
    CvcAttribute,
    /// Attribute Locally Valid (Use)
    CvcAu,
    /// Element Locally Valid (Complex Type)
    CvcComplexType,
    /// Datatype Valid
    CvcDatatypeValid,
    /// Element Locally Valid (Element)
    CvcElt,
    /// enumeration valid
    CvcEnumerationValid,
    /// Facet Valid
    CvcFacetValid,
    /// fractionDigits Valid
    CvcFractionDigitsValid,
    /// Validation Root Valid (ID/IDREF)
    CvcId,
    /// Identity-constraint Satisfied
    CvcIdentityConstraint,
    /// Length Valid
    CvcLengthValid,
    /// maxExclusive Valid
    CvcMaxExclusiveValid,
    /// maxInclusive Valid
    CvcMaxInclusiveValid,
    /// maxLength Valid
    CvcMaxLengthValid,
    /// minExclusive Valid
    CvcMinExclusiveValid,
    /// minInclusive Valid
    CvcMinInclusiveValid,
    /// minLength Valid
    CvcMinLengthValid,
    /// Element Sequence Valid
    CvcModelGroup,
    /// Element Sequence Locally Valid (Particle)
    CvcParticle,
    /// pattern valid
    CvcPatternValid,
    /// QName resolution (Instance)
    CvcResolveInstance,
    /// String Valid
    CvcSimpleType,
    /// totalDigits Valid
    CvcTotalDigitsValid,
    /// Element Locally Valid (Type)
    CvcType,
    /// Item Valid (Wildcard)
    CvcWildcard,
    /// Wildcard allows Namespace Name
    CvcWildcardNamespace,
}

impl std::fmt::Display for XSDError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl std::error::Error for XSDError {}

/// Compute "complete wildcard" described on the chapter of "complexType" component
/// derived from `attributeGroup` and `anyAttribute` elements.
///
/// # Reference
/// - [3.4.2 XML Representation of Complex Type Definitions](https://www.w3.org/TR/xmlschema-1/#declare-type)
fn attribute_complete_wildcard(
    wildcards: &[Wildcard],
    local_wildcard: Option<Wildcard>,
) -> Result<Option<Wildcard>, XSDError> {
    if wildcards.is_empty() {
        // clause 2.1
        // If there are no <attributeGroup> [children] corresponding to attribute groups
        // with non-·absent· {attribute wildcard}s, then the ·local wildcard·.
        Ok(local_wildcard)
    } else if let Some(local) = local_wildcard {
        // clause 2.2.1
        // If there is an <anyAttribute> present, then a wildcard whose {process contents}
        // and {annotation} are those of the ·local wildcard·, and whose {namespace constraint}
        // is the intensional intersection of the {namespace constraint} of the ·local wildcard·
        // and of the {namespace constraint}s of all the non-·absent· {attribute wildcard}s
        // of the attribute groups corresponding to the <attributeGroup> [children], as defined
        // in Attribute Wildcard Intersection (§3.10.6).
        if let Some(wildcard) = wildcards
            .iter()
            .cloned()
            .try_fold(local, |l, r| l.intersection(r))
        {
            Ok(Some(wildcard))
        } else {
            todo!("raise error because wildcards' intersection is not expressible")
        }
    } else {
        // clause 2.2.2
        // If there is no <anyAttribute> present, then a wildcard whose properties are as follows:
        //  {process contents}
        //      The {process contents} of the first non-·absent· {attribute wildcard} of an
        //      attribute group among the attribute groups corresponding to the <attributeGroup>
        //      [children].
        //  {namespace constraint}
        //      The intensional intersection of the {namespace constraint}s of all the non-
        //      ·absent· {attribute wildcard}s of the attribute groups corresponding to the
        //      <attributeGroup> [children], as defined in Attribute Wildcard Intersection (§3.10.6).
        //  {annotation}
        //      ·absent·.
        let top = wildcards[0].clone();
        if let Some(wildcard) = wildcards
            .iter()
            .skip(1)
            .cloned()
            .try_fold(top, |l, r| l.intersection(r))
        {
            Ok(Some(wildcard))
        } else {
            todo!("raise error because wildcards' intersection is not expressible")
        }
    }
}

#[derive(Debug)]
enum Scope {
    /// top-level declaration or definition
    Global,
    /// local declaration or definition
    Local(Option<Arc<str>>, Arc<str>),
}

#[derive(Debug, Default)]
pub(super) struct AttributeDeclaration {
    name: Arc<str>,
    target_namespace: Option<Arc<str>>,
    type_definition: (Option<Arc<str>>, Arc<str>),
    scope: Option<Scope>,
    value_constraint: Option<ValueConstraint>,
    // used for parsing default value constraint.
    ns_context: NamespaceStack,
}

impl AttributeDeclaration {
    fn display_name(&self) -> Cow<'_, str> {
        if let Some(ns) = self.target_namespace.as_deref() {
            Cow::Owned(format!("{{{}}}{}", ns, self.name))
        } else {
            Cow::Borrowed(&self.name)
        }
    }
}

#[derive(Debug, Default)]
pub(super) struct ElementDeclaration {
    name: Arc<str>,
    target_namespace: Option<Arc<str>>,
    type_definition: (Option<Arc<str>>, Arc<str>),
    scope: Option<Scope>,
    value_constraint: Option<ValueConstraint>,
    nillable: bool,
    idc_definitions: Vec<IDCDefinition>,
    substitution_group_affiliation: Option<(Option<Arc<str>>, Arc<str>)>,
    substitution_group_exclusions: FinalSet,
    disallowed_substitutions: BlockSet,
    r#abstract: bool,
    // used for parsing default value constraint.
    ns_context: NamespaceStack,
}

impl ElementDeclaration {
    fn display_name(&self) -> Cow<'_, str> {
        if let Some(ns) = self.target_namespace.as_deref() {
            Cow::Owned(format!("{{{}}}{}", ns, self.name))
        } else {
            Cow::Borrowed(&self.name)
        }
    }
}

#[derive(Debug, Clone)]
pub(super) enum TypeDefinition {
    Simple(Arc<SimpleTypeDefinition>),
    Complex(Arc<ComplexTypeDefinition>),
}

impl TypeDefinition {
    pub(super) fn target_namespace(&self) -> Option<Arc<str>> {
        match self {
            TypeDefinition::Simple(def) => def.target_namespace.clone(),
            TypeDefinition::Complex(def) => def.target_namespace.clone(),
        }
    }

    pub(super) fn name(&self) -> Arc<str> {
        match self {
            TypeDefinition::Simple(def) => def.name.clone(),
            TypeDefinition::Complex(def) => def.name.clone(),
        }
    }

    pub(super) fn as_simple_type(&self) -> Option<Arc<SimpleTypeDefinition>> {
        if let TypeDefinition::Simple(st) = self {
            Some(st.clone())
        } else {
            None
        }
    }

    pub(super) fn as_complex_type(&self) -> Option<Arc<ComplexTypeDefinition>> {
        if let TypeDefinition::Complex(ct) = self {
            Some(ct.clone())
        } else {
            None
        }
    }

    pub fn display_name(&self) -> Cow<'_, str> {
        match self {
            TypeDefinition::Simple(simple) => simple.display_name(),
            TypeDefinition::Complex(complex) => complex.display_name(),
        }
    }

    fn is_derived_from(
        &self,
        target_namespace: Option<&str>,
        name: &str,
        types: &HashMap<(Option<Arc<str>>, Arc<str>), TypeDefinition>,
    ) -> bool {
        match self {
            TypeDefinition::Simple(st) => st.is_derived_from(target_namespace, name, types),
            TypeDefinition::Complex(ct) => {
                if ct.target_namespace.as_deref() == target_namespace && ct.name.as_ref() == name {
                    return true;
                }
                !ct.is_ur_type()
                    && types
                        .get(&ct.base_type_definition)
                        .is_some_and(|t| t.is_derived_from(target_namespace, name, types))
            }
        }
    }
}

#[allow(clippy::large_enum_variant)]
enum OwnedTypeDefinition {
    Simple(SimpleTypeDefinition),
    Complex(ComplexTypeDefinition),
}

#[derive(Debug, Clone, Copy, Default)]
enum DerivationMethod {
    #[default]
    Restriction,
    Extension,
}

#[derive(Debug, Clone, Default)]
enum ContentType {
    #[default]
    Empty,
    /// The reference to the simple type that constrains the content text.
    Simple(Option<Arc<str>>, Arc<str>),
    Mixed(Particle),
    ElementOnly(Particle),
    // component property mapping of {content type} clause 3.2
    // Although a complex type extension requires a base type,
    // specify this value if the base type is unknown at that point.
    // Store the effective content and effective mixed (clause 2).
    Pending(Option<Particle>, bool),
}

#[derive(Debug, Clone, Default)]
pub(super) struct ComplexTypeDefinition {
    name: Arc<str>,
    target_namespace: Option<Arc<str>>,
    base_type_definition: (Option<Arc<str>>, Arc<str>),
    derivation_method: DerivationMethod,
    r#final: FinalSet,
    r#abstract: bool,
    attribute_uses: Vec<AttributeUse>,
    attribute_group_refs: Vec<(Option<Arc<str>>, Arc<str>)>,
    attribute_wildcard: Option<Wildcard>,
    content_type: ContentType,
    prohibited_substitutions: BlockSet,
}

impl ComplexTypeDefinition {
    pub fn display_name(&self) -> Cow<'_, str> {
        if let Some(ns) = self.target_namespace.as_deref() {
            Cow::Owned(format!("{{{}}}{}", ns, self.name))
        } else {
            Cow::Borrowed(&self.name)
        }
    }

    pub fn is_ur_type(&self) -> bool {
        self.target_namespace == XSD_ANY_TYPE.target_namespace && self.name == XSD_ANY_TYPE.name
    }
}

/// [3.4.7 Built-in Complex Type Definition](https://www.w3.org/TR/xmlschema-1/#d0e9252).
static XSD_ANY_TYPE: LazyLock<Arc<ComplexTypeDefinition>> = LazyLock::new(|| {
    let target_namespace = Arc::<str>::from(XML_SCHEMA_NAMESPACE);
    let name = Arc::<str>::from("anyType");
    Arc::new(ComplexTypeDefinition {
        name: name.clone(),
        target_namespace: Some(target_namespace.clone()),
        base_type_definition: (Some(target_namespace), name),
        derivation_method: DerivationMethod::Restriction,
        r#final: FinalSet::default(),
        r#abstract: false,
        attribute_uses: vec![],
        attribute_group_refs: vec![],
        attribute_wildcard: Some(Wildcard {
            ns_constraint: NamespaceConstraint::Any,
            process_contents: ProcessContent::Lax,
        }),
        content_type: ContentType::Mixed(Particle {
            min_occurs: 1,
            max_occurs: 1,
            term: Term::ModelGroup(ModelGroup {
                compositor: Compositor::Sequence,
                particles: vec![Particle {
                    min_occurs: 0,
                    max_occurs: usize::MAX,
                    term: Term::Wildcard(Wildcard {
                        ns_constraint: NamespaceConstraint::Any,
                        process_contents: ProcessContent::Lax,
                    }),
                }],
            }),
        }),
        prohibited_substitutions: BlockSet::default(),
    })
});

#[derive(Debug, Clone)]
enum ValueConstraint {
    Default(Arc<str>),
    Fixed(Arc<str>),
}

#[derive(Debug, Clone, Copy)]
enum Occurrence {
    Optional,
    Prohibited,
    Required,
}

#[derive(Debug, Clone, Copy, Default)]
enum QualificationForm {
    #[default]
    Unqualified,
    Qualified,
}

#[derive(Debug, Clone)]
pub(super) struct AttributeUse {
    // Although the component is defined as having a Boolean value,
    // it is treated as an enumeration to accommodate the "prohibited" value.
    required: Occurrence,
    attribute_declaration: (Option<Arc<str>>, Arc<str>),
    value_constraint: Option<ValueConstraint>,
}

impl AttributeUse {
    fn display_name(&self) -> Cow<'_, str> {
        if let Some(ns) = self.attribute_declaration.0.as_deref() {
            Cow::Owned(format!("{{{}}}{}", ns, self.attribute_declaration.1))
        } else {
            Cow::Borrowed(&self.attribute_declaration.1)
        }
    }
}

#[derive(Debug, Default)]
pub(super) struct AttributeGroupDefinition {
    name: Arc<str>,
    target_namespace: Option<Arc<str>>,
    attribute_uses: Vec<AttributeUse>,
    // references to attribute group definitions
    attribute_group_refs: Vec<(Option<Arc<str>>, Arc<str>)>,
    attribute_wildcard: Option<Wildcard>,
}

impl AttributeGroupDefinition {
    fn display_name(&self) -> Cow<'_, str> {
        if let Some(ns) = self.target_namespace.as_deref() {
            Cow::Owned(format!("{{{}}}{}", ns, self.name))
        } else {
            Cow::Borrowed(&self.name)
        }
    }
}

#[derive(Default)]
pub(super) struct ModelGroupDefinition {
    name: Arc<str>,
    target_namespace: Option<Arc<str>>,
    model_group: ModelGroup,
}

#[derive(Debug, Clone, Copy, PartialEq, Default)]
enum Compositor {
    #[default]
    All,
    Choice,
    Sequence,
}

#[derive(Debug, Clone, Default)]
struct ModelGroup {
    compositor: Compositor,
    particles: Vec<Particle>,
}

impl ModelGroup {
    fn effective_total_range(
        &self,
        model_groups: &HashMap<(Option<Arc<str>>, Arc<str>), ModelGroupDefinition>,
        min_occurs: usize,
        max_occurs: usize,
    ) -> Option<(usize, usize)> {
        if matches!(self.compositor, Compositor::Choice) {
            // [cos-choice-range]
            let mut ret = None::<(usize, usize)>;
            for p in &self.particles {
                let m = match &p.term {
                    Term::ModelGroup(_) => p.effective_total_range(model_groups)?,
                    Term::ModelGroupRef(ns, loc) => {
                        let def = model_groups.get(&(ns.clone(), loc.clone()))?;
                        def.model_group.effective_total_range(model_groups, 1, 1)?
                    }
                    _ => (p.min_occurs, p.max_occurs),
                };
                let ret = ret.get_or_insert((usize::MAX, 0));
                ret.0 = ret.0.min(m.0);
                ret.1 = ret.1.max(m.1);
            }
            let (min, max) = ret?;
            Some((
                min_occurs.saturating_mul(min),
                max_occurs.saturating_mul(max),
            ))
        } else {
            // [cos-seq-range]
            let mut sum = None::<(usize, usize)>;
            for p in &self.particles {
                let m = match &p.term {
                    Term::ModelGroup(_) => p.effective_total_range(model_groups)?,
                    Term::ModelGroupRef(ns, loc) => {
                        let def = model_groups.get(&(ns.clone(), loc.clone()))?;
                        def.model_group.effective_total_range(model_groups, 1, 1)?
                    }
                    _ => (p.min_occurs, p.max_occurs),
                };
                let sum = sum.get_or_insert_default();
                sum.0 = sum.0.saturating_add(m.0);
                sum.1 = sum.1.saturating_add(m.1);
            }
            let (min, max) = sum?;
            Some((
                min_occurs.saturating_mul(min),
                max_occurs.saturating_mul(max),
            ))
        }
    }
}

#[derive(Debug, Clone)]
enum Term {
    ModelGroup(ModelGroup),
    // Only <group> can store references to model group definitions.
    ModelGroupRef(Option<Arc<str>>, Arc<str>),
    Wildcard(Wildcard),
    ElementDecl(Option<Arc<str>>, Arc<str>),
}

impl Default for Term {
    fn default() -> Self {
        Term::ModelGroup(ModelGroup::default())
    }
}

#[derive(Debug, Clone, Default)]
pub(super) struct Particle {
    min_occurs: usize,
    max_occurs: usize,
    term: Term,
}

impl Particle {
    fn model_group<'a>(&'a self, schema: &'a Schema) -> Option<&'a ModelGroup> {
        match &self.term {
            Term::ModelGroup(mg) => Some(mg),
            Term::ModelGroupRef(ns, name) => {
                let mg = schema.modelgroups.get(&(ns.clone(), name.clone()))?;
                Some(&mg.model_group)
            }
            _ => None,
        }
    }

    /// Compute the effective total range.
    ///
    /// If `term` is not a group, return [`None`].
    ///
    /// If the result is larger than [`usize::MAX`], return [`usize::MAX`].
    ///
    /// # Reference
    /// - [cos-seq-range](https://www.w3.org/TR/xmlschema-1/#cos-seq-range)
    /// - [cos-choice-range](https://www.w3.org/TR/xmlschema-1/#cos-choice-range)
    fn effective_total_range(
        &self,
        model_groups: &HashMap<(Option<Arc<str>>, Arc<str>), ModelGroupDefinition>,
    ) -> Option<(usize, usize)> {
        match &self.term {
            Term::ModelGroup(group) => {
                group.effective_total_range(model_groups, self.min_occurs, self.max_occurs)
            }
            Term::ModelGroupRef(ns, loc) => {
                let def = model_groups.get(&(ns.clone(), loc.clone()))?;
                def.model_group.effective_total_range(
                    model_groups,
                    self.min_occurs,
                    self.max_occurs,
                )
            }
            _ => None,
        }
    }

    /// Check if this particle is emptiable.
    ///
    /// # Reference
    /// - [cos-group-emptiable](https://www.w3.org/TR/xmlschema-1/#cos-group-emptiable)
    fn is_emptiable(
        &self,
        model_groups: &HashMap<(Option<Arc<str>>, Arc<str>), ModelGroupDefinition>,
    ) -> bool {
        // clause 1
        // Its {min occurs} is 0.
        if self.min_occurs == 0 {
            return true;
        }

        // clause 2
        // Its {term} is a group and the minimum part of the effective total range of that group,
        // as defined by Effective Total Range (all and sequence) (§3.8.6) (if the group is all
        // or sequence) or Effective Total Range (choice) (§3.8.6) (if it is choice), is 0.
        self.effective_total_range(model_groups)
            .is_some_and(|m| m.0 == 0)
    }

    /// # Reference
    /// - [range-ok](https://www.w3.org/TR/xmlschema-1/#range-ok)
    fn check_range_ok(&self, base: &Particle) -> bool {
        if self.min_occurs < base.min_occurs {
            // [range-ok] clause 1
            return false;
        }
        if base.max_occurs < self.max_occurs || self.max_occurs == usize::MAX {
            // [range-ok] clause 2
            return false;
        }
        true
    }

    /// # Reference
    /// - [cos-particle-extend](https://www.w3.org/TR/xmlschema-1/#cos-particle-extend)
    fn is_valid_extension_of(&self, base: &Particle, schema: &Schema) -> bool {
        if std::ptr::eq(self, base) {
            // clause 1
            return true;
        }

        // clause 2
        if self.min_occurs != 1 || self.max_occurs != 1 {
            return false;
        }

        self.model_group(schema)
            .filter(|mg| matches!(mg.compositor, Compositor::Sequence))
            .filter(|mg| !mg.particles.is_empty())
            .is_some_and(|mg| mg.particles[0].is_identical_to(base, schema))
    }

    fn is_identical_to(&self, other: &Particle, schema: &Schema) -> bool {
        if self.min_occurs != other.min_occurs || self.max_occurs != other.max_occurs {
            return false;
        }
        let (mg, omg) = match (&self.term, &other.term) {
            (Term::ElementDecl(ns, name), Term::ElementDecl(ons, oname)) => {
                return ns == ons && name == oname;
            }
            (Term::Wildcard(wc), Term::Wildcard(owc)) => return wc == owc,
            (
                Term::ModelGroup(_) | Term::ModelGroupRef(_, _),
                Term::ModelGroup(_) | Term::ModelGroupRef(_, _),
            ) => {
                if let Some(mgs) = self.model_group(schema).zip(other.model_group(schema)) {
                    mgs
                } else {
                    return false;
                }
            }
            _ => return false,
        };
        mg.compositor == omg.compositor
            && mg.particles.len() == omg.particles.len()
            && mg
                .particles
                .iter()
                .zip(&omg.particles)
                .all(|(pt, opt)| pt.is_identical_to(opt, schema))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
enum ProcessContent {
    Skip = 1,
    Lax = 2,
    Strict = 3,
}

#[derive(Debug, Clone, PartialEq)]
enum NamespaceConstraint {
    Any,
    Not(Option<Arc<str>>),
    List(Vec<Option<Arc<str>>>),
}

#[derive(Debug, Clone, PartialEq)]
pub(super) struct Wildcard {
    ns_constraint: NamespaceConstraint,
    process_contents: ProcessContent,
}

impl Wildcard {
    /// Compute "intersection" as follows [cos-aw-intersect](https://www.w3.org/TR/xmlschema-1/#cos-aw-intersect).
    ///
    /// The value of {processContent} inherits the value from `self`.
    ///
    /// If the intersection is not expressible, return [`None`].
    ///
    /// # Reference
    /// - [Schema Component Constraint: Attribute Wildcard Intersection](https://www.w3.org/TR/xmlschema-1/#cos-aw-intersect)
    fn intersection(self, other: Self) -> Option<Self> {
        use NamespaceConstraint::*;

        match (self.ns_constraint, other.ns_constraint) {
            (Any, other) | (other, Any) => Some(Self {
                // clause 1 and 2
                ns_constraint: other,
                process_contents: self.process_contents,
            }),
            (Not(l), Not(r)) => {
                if l == r {
                    // clause 1
                    Some(Self {
                        ns_constraint: Not(l),
                        process_contents: self.process_contents,
                    })
                } else if l.is_none() || r.is_none() {
                    // clause 6
                    Some(Self {
                        ns_constraint: Not(l.or(r)),
                        process_contents: self.process_contents,
                    })
                } else {
                    // clause 5
                    None
                }
            }
            (Not(l), List(r)) | (List(r), Not(l)) => {
                // clause 3
                Some(Self {
                    ns_constraint: List(r.into_iter().filter(|r| r.is_some() && *r != l).collect()),
                    process_contents: self.process_contents,
                })
            }
            (List(mut l), List(mut r)) => {
                l.sort_unstable();
                r.sort_unstable();
                if l == r {
                    // clause 1
                    Some(Self {
                        ns_constraint: List(l),
                        process_contents: self.process_contents,
                    })
                } else {
                    // clause 4
                    Some(Self {
                        ns_constraint: List(
                            l.into_iter()
                                .filter(|l| r.binary_search(l).is_ok())
                                .collect(),
                        ),
                        process_contents: self.process_contents,
                    })
                }
            }
        }
    }

    /// Compute "union" as follows [cos-aw-union](https://www.w3.org/TR/xmlschema-1/#cos-aw-union).
    ///
    /// The value of {processContent} inherits the value from `self`.
    ///
    /// If the union is not expressible, return [`None`].
    ///
    /// # Reference
    /// - [Schema Component Constraint: Attribute Wildcard Union](https://www.w3.org/TR/xmlschema-1/#cos-aw-union)
    fn union(self, other: Self) -> Option<Self> {
        if self.ns_constraint == other.ns_constraint {
            // clause 1
            return Some(self);
        }

        let ns_constraint = match (self.ns_constraint, other.ns_constraint) {
            (NamespaceConstraint::Any, _) | (_, NamespaceConstraint::Any) => {
                NamespaceConstraint::Any // clause 2
            }
            (NamespaceConstraint::List(l), NamespaceConstraint::List(r)) => {
                // clause 3
                let mut l = [l, r].concat();
                l.sort_unstable();
                l.dedup();
                NamespaceConstraint::List(l)
            }
            (NamespaceConstraint::Not(_), NamespaceConstraint::Not(_)) => {
                NamespaceConstraint::Not(None) // clause 4
            }
            (NamespaceConstraint::Not(Some(l)), NamespaceConstraint::List(r))
            | (NamespaceConstraint::List(r), NamespaceConstraint::Not(Some(l))) => {
                // clause 5
                let f0 = r.contains(&Some(l.clone()));
                let f1 = r.contains(&None);
                match (f0, f1) {
                    (true, true) => NamespaceConstraint::Any, // clause 5.1
                    (true, false) => NamespaceConstraint::Not(None), // clause 5.2
                    (false, true) => return None,             // clause 5.3
                    (false, false) => NamespaceConstraint::Not(Some(l)), // clause 5.4
                }
            }
            (NamespaceConstraint::Not(None), NamespaceConstraint::List(l))
            | (NamespaceConstraint::List(l), NamespaceConstraint::Not(None)) => {
                // clause 6
                if l.contains(&None) {
                    NamespaceConstraint::Any // clause 6.1
                } else {
                    NamespaceConstraint::Not(None) // clause 6.2
                }
            }
        };
        Some(Self {
            ns_constraint,
            process_contents: self.process_contents,
        })
    }

    /// Check if `self` is a subset of `other`.
    ///
    /// # Reference
    /// - [Schema Component Constraint: Wildcard Subset](https://www.w3.org/TR/xmlschema-1/#cos-ns-subset)
    fn is_subset_of(&self, other: &Self) -> bool {
        match (&self.ns_constraint, &other.ns_constraint) {
            (_, NamespaceConstraint::Any) => {
                // clause 1
                true
            }
            (NamespaceConstraint::Not(l), NamespaceConstraint::Not(r)) if l == r => {
                // clause 2
                true
            }
            (NamespaceConstraint::List(l), NamespaceConstraint::List(r)) => {
                // clause 3.1 and 3.2.1
                l.iter().all(|l| r.contains(l))
            }
            (NamespaceConstraint::List(l), NamespaceConstraint::Not(r)) => {
                // clause 3.1 and 3.2.2
                !l.contains(r) && !l.contains(&None)
            }
            _ => false,
        }
    }

    /// # Reference
    /// - [cvc-wildcard-namespace](https://www.w3.org/TR/xmlschema-1/#cvc-wildcard-namespace)
    fn is_match(&self, namespace_name: Option<&str>) -> bool {
        match &self.ns_constraint {
            NamespaceConstraint::Any => true, // clause 1
            NamespaceConstraint::Not(ns) => {
                // clause 2
                ns.as_deref() != namespace_name && namespace_name.is_some()
            }
            NamespaceConstraint::List(ns) => {
                // clause 3
                ns.iter()
                    .map(|ns| ns.as_deref())
                    .any(|ns| ns == namespace_name)
            }
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
enum IDCCategory {
    #[default]
    Key,
    Keyref,
    Unique,
}

#[derive(Debug, Default)]
struct IDCDefinition {
    name: Arc<str>,
    target_namespace: Option<Arc<str>>,
    idc_category: IDCCategory,
    selector: String,
    fields: Vec<String>,
    referenced_key: Option<(Option<Arc<str>>, Arc<str>)>,
}

pub(super) struct Notation {
    name: Arc<str>,
    target_namespace: Option<Arc<str>>,
    system_id: Option<Arc<URIStr>>,
    public_id: Option<Arc<str>>,
}
