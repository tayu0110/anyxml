use std::{
    borrow::Cow,
    cell::RefCell,
    collections::{HashMap, HashSet},
    io::Read,
    sync::{Arc, LazyLock},
};

use anyxml_automata::xsregexp::XSRegexp;
use anyxml_uri::uri::URIString;

use crate::{
    XML_XML_NAMESPACE, XMLVersion,
    automata::xsregexp::RegexpError,
    error::{XMLError, XMLErrorDomain, XMLErrorLevel},
    sax::{
        AttributeType, Attributes, DefaultDecl, DefaultSAXHandler, EntityResolver, ErrorHandler,
        InputSource, Locator, NamespaceStack, SAXHandler, XMLReader, error::SAXParseError,
    },
    tree::{
        Attribute, Document, Element, Node, TreeBuildHandler, convert::NodeKind, node::NodeSpec,
    },
    uri::URIStr,
    xsd::{
        XML_SCHEMA_NAMESPACE, XSDSchema,
        typedef::{
            Block, BlockSet, Boundary, Enumeration, Final, FinalSet, Length, Pattern,
            SimpleTypeDefinition, Variety, Whitespace, WhitespaceConstraint,
        },
    },
    xsdtypes::{
        SCHEMA_BUILTIN_ANYURI, SCHEMA_BUILTIN_BOOLEAN, SCHEMA_BUILTIN_NCNAME,
        SCHEMA_BUILTIN_NONNEGATIVE_INTEGER, SCHEMA_BUILTIN_QNAME, SchemaValue,
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

macro_rules! generic_error {
    ($method:ident, $handler:expr, $code:expr, $level:expr, $domain:expr, $elem:expr, $message:literal, $( $args:expr ),*) => {
        #[allow(unused)]
        use self::XSDError::*;
        $handler.$method($crate::sax::error::SAXParseError {
            error: $code.into(),
            level: $level,
            domain: $domain,
            line: 0,    // todo
            column: 0,  // todo
            system_id: $elem.owner_document().document_base_uri().as_ref().into(),
            public_id: None,
            message: ::std::borrow::Cow::Owned(format!($message, $( $args ),*)),
        })
    };
    ($method:ident, $handler:expr, $code:expr, $level:expr, $domain:expr, $elem:expr, $message:literal) => {
        use XSDError::*;
        $handler.$method($crate::sax::error::SAXParseError {
            error: $code.into(),
            level: $level,
            domain: $domain,
            line: 0,    // todo
            column: 0,  // todo
            system_id: $elem.owner_document().document_base_uri().as_ref().into(),
            public_id: None,
            message: ::std::borrow::Cow::Borrowed($message),
        })
    };
    ($method:ident, $handler:expr, $code:expr, $level:expr, $domain:expr, $elem:expr, $message:expr) => {
        use XSDError::*;
        $handler.$method($crate::sax::error::SAXParseError {
            error: $code.into(),
            level: $level,
            domain: $domain,
            line: 0,    // todo
            column: 0,  // todo
            system_id: $elem.owner_document().document_base_uri().as_ref().into(),
            public_id: None,
            message: ::std::borrow::Cow::Owned($message.into()),
        })
    };
}
macro_rules! error {
    ($handler:expr, $code:ident, $elem:expr, $message:literal, $( $args:expr ),*) => {{
        generic_error!(
            error,
            $handler,
            $code,
            $crate::error::XMLErrorLevel::Error,
            $crate::error::XMLErrorDomain::XSDParser,
            $elem,
            $message,
            $( $args ),*
        );
    }};
    ($handler:expr, $code:ident, $elem:expr, $message:literal) => {
        error!($handler, $code, $elem, $message, );
    };
    ($handler:expr, $code:ident, $elem:expr, $message:expr) => {
        generic_error!(
            error,
            $handler.handler,
            $code,
            $crate::error::XMLErrorLevel::Error,
            $crate::error::XMLErrorDomain::XSDParser,
            $elem,
            $message
        );
    };
}

macro_rules! internal_error {
    ($handler:expr, $elem:expr) => {{
        error!(
            $handler,
            InternalError,
            $elem,
            "Internal error: {}:{}",
            line!(),
            column!()
        );
        return Err(XSDError::InternalError);
    }};
}
macro_rules! unallowed_text {
    ($handler:expr, $elem:expr) => {{
        error!(
            $handler,
            UnallowedText,
            $elem,
            "Text is not allowed as children of schema element '{}'.",
            $elem.local_name()
        );
        return Err(XSDError::UnallowedText);
    }};
}
macro_rules! unallowed_element {
    ($handler:expr, $elem:expr) => {{
        error!(
            $handler,
            UnallowedElement,
            $elem,
            "'{}' is not allowed as a schema element.",
            $elem.local_name()
        );
        return Err(XSDError::UnallowedElement);
    }};
}

thread_local! {
    static META_SCHEMA_HOLDER_V1_0: RefCell<Vec<XSDSchema>> = const { RefCell::new(vec![]) };
}

#[derive(Default)]
pub struct Schema {
    type_defs: HashMap<(Option<Arc<str>>, Arc<str>), TypeDefinition>,
    att_decls: HashMap<(Option<Arc<str>>, Arc<str>), AttributeDeclaration>,
    elem_decls: HashMap<(Option<Arc<str>>, Arc<str>), ElementDeclaration>,
    attgroup_defs: HashMap<(Option<Arc<str>>, Arc<str>), AttributeGroupDefinition>,
    modelgroup_defs: HashMap<(Option<Arc<str>>, Arc<str>), ModelGroupDefinition>,
    notation_decls: HashMap<(Option<Arc<str>>, Arc<str>), Notation>,
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
        META_SCHEMA_HOLDER_V1_0.with_borrow_mut(|holder| {
            if let Some(meta) = holder.pop() {
                return meta;
            }
            let mut parser = XMLReader::builder()
                .set_handler(XSDParseHandler::with_handler(MetaSchemaParsehandler))
                .build();
            let xml = include_str!("../../resources/xsd/XMLSchema-compact.xsd");
            parser.parse_str(xml, None).unwrap();
            Self {
                schema: parser.handler.schema,
            }
        })
    }
}

/// SAX handler for parsing XSD schema documents.
pub struct XSDParseHandler<H: SAXHandler = DefaultSAXHandler> {
    child: H,
    schema: Schema,
    last_error: Result<(), XMLError>,
    locator: Arc<Locator>,
    skip_depth: usize,

    // context
    target_namespace: Option<Arc<str>>,
    unnamed: usize,
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
    imports: Vec<(Option<Box<str>>, URIString)>,

    // temporary component buffer
    simple_type: Box<SimpleTypeDefinition>,
    idc: Box<IDCDefinition>,
}

impl<H: SAXHandler> XSDParseHandler<H> {
    fn with_handler(handler: H) -> Self {
        Self {
            child: handler,
            schema: Schema::default(),
            last_error: Ok(()),
            locator: Arc::new(Locator::default()),
            skip_depth: 0,
            target_namespace: None,
            unnamed: 0,
            ns_stack: NamespaceStack::default(),
            element_stack: vec![],
            attr_form_default: QualificationForm::Unqualified,
            elem_form_default: QualificationForm::Unqualified,
            block_default: BlockSet::default(),
            final_default: FinalSet::default(),
            includes: vec![],
            imports: vec![],
            simple_type: Box::default(),
            idc: Box::default(),
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
}

impl<H: SAXHandler> EntityResolver for XSDParseHandler<H> {
    fn resolve_entity(
        &mut self,
        name: &str,
        public_id: Option<&str>,
        base_uri: &URIStr,
        system_id: &URIStr,
    ) -> Result<InputSource<'static>, XMLError> {
        self.child
            .resolve_entity(name, public_id, base_uri, system_id)
    }

    fn get_external_subset(
        &mut self,
        name: &str,
        base_uri: Option<&URIStr>,
    ) -> Result<InputSource<'static>, XMLError> {
        self.child.get_external_subset(name, base_uri)
    }
}

impl<H: SAXHandler> ErrorHandler for XSDParseHandler<H> {
    fn fatal_error(&mut self, mut error: SAXParseError) {
        self.last_error = Err(error.error.clone());
        if matches!(error.domain, XMLErrorDomain::XSDValid) {
            error.domain = XMLErrorDomain::XSDParser;
        }
        self.child.fatal_error(error);
    }

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

    fn warning(&mut self, mut error: SAXParseError) {
        if matches!(error.domain, XMLErrorDomain::XSDValid) {
            error.domain = XMLErrorDomain::XSDParser;
        }
        self.child.warning(error);
    }
}

impl<H: SAXHandler> SAXHandler for XSDParseHandler<H> {
    fn set_document_locator(&mut self, locator: Arc<Locator>) {
        self.locator = locator.clone();
        self.child.set_document_locator(locator);
    }

    fn start_element(
        &mut self,
        namespace_name: Option<&str>,
        local_name: Option<&str>,
        qname: &str,
        atts: &Attributes,
    ) {
        self.child
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
            self.error(SAXParseError {
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
                let namespace = atts.get_value_by_qname("namespace").map(Box::<str>::from);
                self.imports.push((namespace, schema_location));
                self.element_stack.push(SCHEMA_IMPORT_NAME);
            }
            SCHEMA_SIMPLETYPE_NAME => {
                if self.element_stack.last() == Some(&SCHEMA_RESTRICTION_NAME)
                    && !self.simple_type.base_type_definition.1.is_empty()
                {
                    // [src-restriction-base-or-simpleType]
                    // If the "base" attribute is present in <restriction>,
                    // <simpleType> must not exist under <restriction>.
                    let msg = "The attribute 'base' and the child <simpleType> of <restriction> must not both be present.";
                    self.error(SAXParseError {
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
                    && !self.simple_type.base_type_definition.1.is_empty()
                {
                    // [src-list-itemType-or-simpleType]
                    // If the `itemType` attribute is present,
                    // the child must not have a `simpleType`.
                    let msg = "The attribute 'itemType' and the child <simpleType> of <list> must not both be present.";
                    self.error(SAXParseError {
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
                self.simple_type.name = atts
                    .get_value_by_qname("name")
                    .map(Arc::from)
                    .unwrap_or_else(|| self.dummy_name());
                self.simple_type.target_namespace = self.target_namespace.clone();
                self.simple_type.r#final = get_final(atts, "final").unwrap_or_default();
                self.element_stack.push(SCHEMA_SIMPLETYPE_NAME);
            }
            SCHEMA_RESTRICTION_NAME
                if self.element_stack.last() == Some(&SCHEMA_SIMPLETYPE_NAME) =>
            {
                // 'restriction' under a simple type definition

                // Since the child may contain <simpleType>, "base" is not a required element.
                if let Some((ns, loc)) = self.get_qname(atts, "base") {
                    self.simple_type.base_type_definition = (ns, loc);
                }
                self.element_stack.push(SCHEMA_RESTRICTION_NAME);
            }
            SCHEMA_LIST_NAME => {
                // Since the child may contain <simpleType>, "itemType" is not a required element.
                if let Some((pre, loc)) = self.get_qname(atts, "itemType") {
                    self.simple_type.variety = Some(Variety::List(pre, loc));
                }
                self.element_stack.push(SCHEMA_LIST_NAME);
            }
            SCHEMA_UNION_NAME => {
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
                    self.simple_type.variety = Some(Variety::Union(buf.into()));
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
                let fixed = get_boolean(atts, "fixed").unwrap_or_default();
                self.simple_type.facets.length = Some(Length { value, fixed });
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
                let fixed = get_boolean(atts, "fixed").unwrap_or_default();
                self.simple_type.facets.min_length = Some(Length { value, fixed });
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
                let fixed = get_boolean(atts, "fixed").unwrap_or_default();
                self.simple_type.facets.max_length = Some(Length { value, fixed });
                self.element_stack.push(SCHEMA_MAXLENGTH_NAME);
            }
            SCHEMA_PATTERN_NAME => {
                let Some(regex) = atts.get_value_by_qname("value") else {
                    self.skip_depth += 1;
                    return;
                };
                if let Err(err) = XSRegexp::compile(regex) {
                    self.error(SAXParseError {
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
                self.simple_type.facets.pattern.push(Pattern {
                    value: regex.into(),
                });
                self.element_stack.push(SCHEMA_PATTERN_NAME);
            }
            SCHEMA_ENUMERATION_NAME => {
                let Some(value) = atts.get_value_by_qname("value") else {
                    self.skip_depth += 1;
                    return;
                };
                self.simple_type.facets.enumeration.push(Enumeration {
                    value: value.into(),
                });
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
                self.simple_type.facets.whitespace = Some(WhitespaceConstraint { value, fixed });
                self.element_stack.push(SCHEMA_WHITESPACE_NAME);
            }
            SCHEMA_MAXINCLUSIVE_NAME => {
                let Some(value) = atts.get_value_by_qname("value") else {
                    self.skip_depth += 1;
                    return;
                };
                let fixed = get_boolean(atts, "fixed").unwrap_or_default();
                self.simple_type.facets.max_inclusive = Some(Boundary {
                    value: value.into(),
                    fixed,
                });
                self.element_stack.push(SCHEMA_MAXINCLUSIVE_NAME);
            }
            SCHEMA_MAXEXCLUSIVE_NAME => {
                let Some(value) = atts.get_value_by_qname("value") else {
                    self.skip_depth += 1;
                    return;
                };
                let fixed = get_boolean(atts, "fixed").unwrap_or_default();
                self.simple_type.facets.max_exclusive = Some(Boundary {
                    value: value.into(),
                    fixed,
                });
                self.element_stack.push(SCHEMA_MAXEXCLUSIVE_NAME);
            }
            SCHEMA_MINEXCLUSIVE_NAME => {
                let Some(value) = atts.get_value_by_qname("value") else {
                    self.skip_depth += 1;
                    return;
                };
                let fixed = get_boolean(atts, "fixed").unwrap_or_default();
                self.simple_type.facets.min_exclusive = Some(Boundary {
                    value: value.into(),
                    fixed,
                });
                self.element_stack.push(SCHEMA_MINEXCLUSIVE_NAME);
            }
            SCHEMA_MININCLUSIVE_NAME => {
                let Some(value) = atts.get_value_by_qname("value") else {
                    self.skip_depth += 1;
                    return;
                };
                let fixed = get_boolean(atts, "fixed").unwrap_or_default();
                self.simple_type.facets.min_inclusive = Some(Boundary {
                    value: value.into(),
                    fixed,
                });
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
                let fixed = get_boolean(atts, "fixed").unwrap_or_default();
                self.simple_type.facets.total_digits = Some(Length { value, fixed });
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
                let fixed = get_boolean(atts, "fixed").unwrap_or_default();
                self.simple_type.facets.fraction_digits = Some(Length { value, fixed });
                self.element_stack.push(SCHEMA_FRACDIGITS_NAME);
            }
            SCHEMA_COMPLEXTYPE_NAME => {
                self.element_stack.push(SCHEMA_COMPLEXTYPE_NAME);
            }
            SCHEMA_SIMPLECONT_NAME => {
                self.element_stack.push(SCHEMA_SIMPLECONT_NAME);
            }
            SCHEMA_COMPLEXCONT_NAME => {
                self.element_stack.push(SCHEMA_COMPLEXCONT_NAME);
            }
            SCHEMA_RESTRICTION_NAME => {
                self.element_stack.push(SCHEMA_RESTRICTION_NAME);
            }
            SCHEMA_EXTENSION_NAME => {
                self.element_stack.push(SCHEMA_EXTENSION_NAME);
            }
            SCHEMA_GROUP_NAME => {
                self.element_stack.push(SCHEMA_GROUP_NAME);
            }
            SCHEMA_ATTGROUP_NAME => {
                self.element_stack.push(SCHEMA_ATTGROUP_NAME);
            }
            SCHEMA_ELEMENT_NAME => {
                self.element_stack.push(SCHEMA_ELEMENT_NAME);
            }
            SCHEMA_ATTRIBUTE_NAME => {
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
                self.schema.notation_decls.insert(
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
                self.element_stack.push(SCHEMA_ALL_NAME);
            }
            SCHEMA_CHOICE_NAME => {
                self.element_stack.push(SCHEMA_CHOICE_NAME);
            }
            SCHEMA_SEQUENCE_NAME => {
                self.element_stack.push(SCHEMA_SEQUENCE_NAME);
            }
            SCHEMA_ANY_NAME => {
                self.element_stack.push(SCHEMA_ANY_NAME);
            }
            SCHEMA_ANYATT_NAME => {
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
        self.child.end_element(namespace_name, local_name, qname);

        if self.skip_depth > 0 {
            self.skip_depth -= 1;
            return;
        }

        assert_eq!(self.element_stack.pop(), local_name);

        let local_name = local_name.unwrap();
        match local_name {
            SCHEMA_RESTRICTION_NAME
                if self.element_stack.last() == Some(&SCHEMA_SIMPLETYPE_NAME) =>
            {
                if self.simple_type.base_type_definition.1.is_empty() {
                    // [src-restriction-base-or-simpleType]
                    // If there is no "base" attribute, there
                    // must be a child of type <simpleType>.
                    let msg = "<restriction> must have either the attribute 'base' or the child <simpleType>.";
                    self.error(SAXParseError {
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
                if self.simple_type.variety.is_none() {
                    // [src-list-itemType-or-simpleType]
                    // If there is no "itemType" attribute, there
                    // must be a child of type <simpleType>.
                    let msg = "<list> must have either the attribute 'itemType' or the child <simpleType>.";
                    self.error(SAXParseError {
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
                if self.simple_type.variety.is_none() {
                    // [src-union-memberTypes-or-simpleTypes]
                    // If there is no `memberType` attribute, there
                    // must be children of type `simpleType`.
                    let msg = "<union> must have either the non-empty attribute 'memberType' or the child <simpleType>s.";
                    self.error(SAXParseError {
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
            _ => {}
        }
    }

    fn characters(&mut self, data: &str) {
        self.child.characters(data);
    }

    fn start_prefix_mapping(&mut self, prefix: Option<&str>, namespace_name: &str) {
        self.child.start_prefix_mapping(prefix, namespace_name);
        self.ns_stack
            .push(prefix.unwrap_or_default(), namespace_name);
    }

    fn end_prefix_mapping(&mut self, prefix: Option<&str>) {
        self.child.end_prefix_mapping(prefix);
        // Although the namespace that is actually popped may not bind the specified prefix,
        // it doesn't matter because everything works out by the time the other callbacks are
        // invoked.
        self.ns_stack.pop();
    }

    // unused callbacks

    fn attribute_decl(
        &mut self,
        element_name: &str,
        attribute_name: &str,
        attribute_type: &AttributeType,
        default_decl: &DefaultDecl,
    ) {
        self.child
            .attribute_decl(element_name, attribute_name, attribute_type, default_decl);
    }
    fn comment(&mut self, data: &str) {
        self.child.comment(data);
    }
    fn declaration(&mut self, version: &str, encoding: Option<&str>, standalone: Option<bool>) {
        self.child.declaration(version, encoding, standalone);
    }
    fn element_decl(&mut self, name: &str, contentspec: &crate::sax::ContentSpec) {
        self.child.element_decl(name, contentspec);
    }
    fn start_cdata(&mut self) {
        self.child.start_cdata();
    }
    fn end_cdata(&mut self) {
        self.child.end_cdata();
    }
    fn start_document(&mut self) {
        self.child.start_document();
    }
    fn end_document(&mut self) {
        self.child.end_document();
    }
    fn start_dtd(&mut self, name: &str, public_id: Option<&str>, system_id: Option<&URIStr>) {
        self.child.start_dtd(name, public_id, system_id);
    }
    fn end_dtd(&mut self) {
        self.child.end_dtd();
    }
    fn start_entity(&mut self, name: &str) {
        self.child.start_entity(name);
    }
    fn end_entity(&mut self) {
        self.child.end_entity();
    }
    fn external_entity_decl(&mut self, name: &str, public_id: Option<&str>, system_id: &URIStr) {
        self.child.external_entity_decl(name, public_id, system_id);
    }
    fn ignorable_whitespace(&mut self, data: &str) {
        self.child.ignorable_whitespace(data);
    }
    fn internal_entity_decl(&mut self, name: &str, value: &str) {
        self.child.internal_entity_decl(name, value);
    }
    fn notation_decl(&mut self, name: &str, public_id: Option<&str>, system_id: Option<&URIStr>) {
        self.child.notation_decl(name, public_id, system_id);
    }
    fn processing_instruction(&mut self, target: &str, data: Option<&str>) {
        self.child.processing_instruction(target, data);
    }
    fn skipped_entity(&mut self, name: &str) {
        self.child.skipped_entity(name);
    }
    fn unparsed_entity_decl(
        &mut self,
        name: &str,
        public_id: Option<&str>,
        system_id: &URIStr,
        notation_name: &str,
    ) {
        self.child
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

struct MetaSchemaParsehandler;
impl ErrorHandler for MetaSchemaParsehandler {
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
impl EntityResolver for MetaSchemaParsehandler {
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
impl SAXHandler for MetaSchemaParsehandler {}

fn create_handler<H: SAXHandler>(handler: H) -> TreeBuildHandler<H> {
    let mut h = TreeBuildHandler::with_handler(handler);
    h.expand_entity_reference = true;
    h.coalescing = true;
    h.ignoring_comments = true;
    h
}

fn parse_schema<H: SAXHandler + ?Sized>(
    document: Document,
    handler: &mut H,
) -> Result<Schema, XSDError> {
    let mut context = XSDParseContext::new(handler);
    context.parse(document)?;
    Ok(context.schema)
}

#[derive(Debug, Clone, Copy)]
pub enum XSDError {
    InternalError,
    UnallowedElement,
    UnallowedAttribute,
    UnallowedText,
    TooLargeOccurence,
    MissingSubComponent,
    RegexpError(RegexpError),

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
}

impl std::fmt::Display for XSDError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl std::error::Error for XSDError {}

struct XSDParseContext<'a, H: SAXHandler + ?Sized> {
    // result schema
    schema: Schema,

    // context
    target_namespace: Option<Arc<str>>,
    included_resources: HashSet<Arc<URIStr>>,
    unnamed: usize,
    simple_types: HashMap<(Option<Arc<str>>, Arc<str>), Element>,
    complex_types: HashMap<(Option<Arc<str>>, Arc<str>), Element>,
    att_groups: HashMap<(Option<Arc<str>>, Arc<str>), Element>,
    model_groups: HashMap<(Option<Arc<str>>, Arc<str>), Element>,
    elem_decls: HashMap<(Option<Arc<str>>, Arc<str>), Element>,
    attr_decls: HashMap<(Option<Arc<str>>, Arc<str>), Element>,

    // qualification form
    attr_form_default: QualificationForm,
    elem_form_default: QualificationForm,
    // block
    block_default: FinalSet,
    // final
    final_default: FinalSet,

    // handler
    handler: &'a mut H,
}

impl<'a, H: SAXHandler + ?Sized> XSDParseContext<'a, H> {
    fn new(handler: &'a mut H) -> Self {
        XSDParseContext {
            schema: Schema::default(),
            target_namespace: None,
            included_resources: HashSet::new(),
            unnamed: 0,
            simple_types: HashMap::new(),
            complex_types: HashMap::new(),
            att_groups: HashMap::new(),
            model_groups: HashMap::new(),
            elem_decls: HashMap::new(),
            attr_decls: HashMap::new(),
            attr_form_default: QualificationForm::Unqualified,
            elem_form_default: QualificationForm::Unqualified,
            block_default: FinalSet::default(),
            final_default: FinalSet::default(),
            handler,
        }
    }

    fn parse(&mut self, document: Document) -> Result<(), XSDError> {
        self.included_resources
            .insert(document.document_base_uri().as_ref().into());
        self.parse_schema(document)?;
        Ok(())
    }

    fn parse_schema(&mut self, document: Document) -> Result<(), XSDError> {
        let Some(mut schema) = document.document_element() else {
            internal_error!(self.handler, document);
        };

        if schema.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE)
            || schema.local_name().as_ref() != SCHEMA_SCHEMA_NAME
        {
            unallowed_element!(self.handler, schema);
        }

        let _id = self.extract_ncname(schema.clone(), "id", XSDError::SchPropsCorrect)?;
        let _version = schema.remove_attribute("version", None);
        let _lang = schema.remove_attribute("lang", Some(XML_XML_NAMESPACE));

        self.check_attribute(schema.clone())?;

        // include/import/redefine phase
        let mut children = schema.first_child();
        while let Some(child) = children {
            children = child.next_sibling();

            match child.downcast() {
                NodeKind::Element(elem) => {
                    if elem.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE) {
                        // skip foreign element
                        continue;
                    }
                    match elem.local_name().as_ref() {
                        SCHEMA_INCLUDE_NAME => self.parse_include(elem)?,
                        SCHEMA_IMPORT_NAME => self.parse_import(elem)?,
                        SCHEMA_REDEFINE_NAME => todo!(),
                        SCHEMA_ANNOTATION_NAME => {}
                        SCHEMA_SIMPLETYPE_NAME
                        | SCHEMA_COMPLEXTYPE_NAME
                        | SCHEMA_GROUP_NAME
                        | SCHEMA_ATTGROUP_NAME
                        | SCHEMA_ELEMENT_NAME
                        | SCHEMA_ATTRIBUTE_NAME
                        | SCHEMA_NOTATION_NAME => {
                            children = Some(child);
                            break;
                        }
                        _ => unallowed_element!(self.handler, elem),
                    }
                }
                NodeKind::Text(_) => unallowed_text!(self.handler, schema),
                _ => {
                    // PI can be ignored.
                    // Since the settings are configured so that CDATA sections, entity
                    // references, and comments do not appear, they can also be ignored.
                }
            }
        }

        // global definition/declaration phase
        while let Some(child) = children {
            children = child.next_sibling();

            match child.downcast() {
                NodeKind::Element(elem) => {
                    use XSDError::*;

                    if elem.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE) {
                        // skip foreign element
                        continue;
                    }
                    let (memo, error) = match elem.local_name().as_ref() {
                        SCHEMA_ANNOTATION_NAME => {
                            continue;
                        }
                        SCHEMA_SIMPLETYPE_NAME => (&mut self.simple_types, SrcSimpleType),
                        SCHEMA_COMPLEXTYPE_NAME => (&mut self.complex_types, SrcCt),
                        SCHEMA_GROUP_NAME => (&mut self.model_groups, SrcModelGroupDefn),
                        SCHEMA_ATTGROUP_NAME => (&mut self.att_groups, SrcAttributeGroup),
                        SCHEMA_ELEMENT_NAME => (&mut self.elem_decls, SrcElement),
                        SCHEMA_ATTRIBUTE_NAME => (&mut self.attr_decls, SrcAttribute),
                        SCHEMA_NOTATION_NAME => {
                            continue;
                        }
                        _ => unallowed_element!(self.handler, elem),
                    };

                    let Some(value) = elem.get_attribute("name", None) else {
                        todo!("raise error");
                    };
                    match SCHEMA_BUILTIN_NCNAME.parse_with_elem(&value, elem.clone()) {
                        Ok(SchemaValue::String(ncname)) => {
                            memo.insert((self.target_namespace.clone(), ncname), elem);
                        }
                        Ok(_) => internal_error!(self.handler, elem),
                        Err(err) => {
                            error!(
                                self.handler,
                                error,
                                elem,
                                "The attribute 'name' of '{}' is failed to parse as NCName because {}.",
                                elem.local_name(),
                                err
                            );
                            return Err(error);
                        }
                    }
                }
                NodeKind::Text(_) => unallowed_text!(self.handler, schema),
                _ => {
                    // PI can be ignored.
                    // Since the settings are configured so that CDATA sections, entity
                    // references, and comments do not appear, they can also be ignored.
                }
            }
        }

        // Second, resolve the attribute declarations.
        // Since they do not depend on anything other than simple type definitions,
        // they can be resolved here.
        // Because attribute group definitions and complex type definitions depend on
        // attribute declarations, they must be resolved before these.
        while let Some((key, value)) = self.attr_decls.iter().next() {
            let key = key.clone();
            let value = value.clone();

            self.attr_decls.remove(&key);
            self.parse_attribute_declaration(value, self.target_namespace.clone(), true)?;
        }
        // Third, resolve the attribute group definition.
        // Since this depends only on attribute declarations, it can be resolved here.
        // Because it is referenced from complex type definitions through attribute use
        // and model groups, it must be resolved here.
        while let Some((key, value)) = self.att_groups.iter().next() {
            let key = key.clone();
            let value = value.clone();

            self.att_groups.remove(&key);
            self.parse_named_attgroup_definition(value, key.0, &mut vec![])?;
        }

        // Finally, resolve everything that remains.
        // Element declarations, complex type definitions, and model group definitions are
        // all interdependent, so it is impossible to resolve any one of them first.
        // Therefore, the only option is to resolve the dependencies recursively.
        // As a starting point, try the complex type definitions first.
        while let Some((key, value)) = self.complex_types.iter().next() {
            let key = key.clone();
            let value = value.clone();

            self.complex_types.remove(&key);
            self.parse_complex_type_definitions(value, key.0, &mut vec![], true)?;
        }
        // Next, resolve element declarations
        while let Some((key, value)) = self.elem_decls.iter().next() {
            let key = key.clone();
            let value = value.clone();

            self.elem_decls.remove(&key);
            self.parse_top_level_element(value, key.0)?;
        }
        // Next, resolve model group definitions
        while let Some((key, value)) = self.model_groups.iter().next() {
            let key = key.clone();
            let value = value.clone();

            self.model_groups.remove(&key);
            self.parse_named_group(value, key.0)?;
        }

        Ok(())
    }

    /// Parse the `attribute` element.
    ///
    /// If this results in the generation of an Attribute component, register it in the
    /// schema. Additionally, if an Attribute Use component is generated, wrap it in an
    /// [`Ok`] and return it.
    fn parse_attribute_declaration(
        &mut self,
        mut elem: Element,
        target_namespace: Option<Arc<str>>,
        global: bool,
    ) -> Result<Option<AttributeUse>, XSDError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert_eq!(elem.local_name().as_ref(), SCHEMA_ATTRIBUTE_NAME);

        let default = elem.remove_attribute("default", None);
        let fixed = elem.remove_attribute("fixed", None);
        let form = self
            .extract_qualification_form(elem.clone(), "form", XSDError::SrcAttribute)?
            .or(global.then_some(self.attr_form_default));
        let target_namespace =
            target_namespace.filter(|_| matches!(form, Some(QualificationForm::Qualified)));
        let _id = elem.remove_attribute("id", None);
        let r#ref = elem.remove_attribute("ref", None);
        let name = match elem.remove_attribute("name", None) {
            Some(name) => match SCHEMA_BUILTIN_NCNAME.parse_with_elem(&name, elem.clone()) {
                Ok(SchemaValue::String(name)) => {
                    if !global && r#ref.is_some() {
                        // [src-attribute]
                        // 3   If the item's parent is not <schema>,
                        //     then all of the following must be true:
                        //   3.1   One of ref or name must be present, but not both.
                        error!(
                            self.handler,
                            SrcAttribute,
                            elem,
                            "A local attribute declaration must not have both the attribute 'name' and 'ref'."
                        );
                        return Err(XSDError::SrcAttribute);
                    }
                    name
                }
                Ok(_) => internal_error!(self.handler, elem),
                Err(err) => {
                    error!(
                        self.handler,
                        SrcAttribute,
                        elem,
                        "The attribute 'name' of 'attribute' is failed to parse as NCName because {}.",
                        err
                    );
                    return Err(XSDError::SrcAttribute);
                }
            },
            None => {
                if !global {
                    if r#ref.is_none() {
                        // [src-attribute]
                        // 3   If the item's parent is not <schema>,
                        //     then all of the following must be true:
                        //   3.1   One of ref or name must be present, but not both.
                        error!(
                            self.handler,
                            SrcAttribute,
                            elem,
                            "A local attribute declaration must have one of the attribute 'name' or 'ref'."
                        );
                        return Err(XSDError::SrcAttribute);
                    }
                    self.unnamed += 1;
                    format!("#unnamed{}", self.unnamed).into()
                } else {
                    todo!("raise error")
                }
            }
        };
        let r#type = elem.remove_attribute("type", None);
        let r#use = match elem.remove_attribute("use", None) {
            Some(r#use) => {
                let r#use = r#use.trim_matches(|c| XMLVersion::XML10.is_whitespace(c));
                match r#use {
                    "optional" => Occurrence::Optional,
                    "prohibited" => {
                        if default.is_some() {
                            // [src-attribute]
                            // 2 If default and use are both present,
                            // use must have the ·actual value· optional.
                            error!(
                                self.handler,
                                SrcAttribute,
                                elem,
                                "The attribute 'use' of '{}' must be 'optional' when 'default' is present.",
                                SCHEMA_ATTRIBUTE_NAME
                            );
                            return Err(XSDError::SrcAttribute);
                        }
                        Occurrence::Prohibited
                    }
                    "required" => {
                        if default.is_some() {
                            // [src-attribute]
                            // 2 If default and use are both present,
                            // use must have the ·actual value· optional.
                            error!(
                                self.handler,
                                SrcAttribute,
                                elem,
                                "The attribute 'use' of '{}' must be 'optional' when 'default' is present.",
                                SCHEMA_ATTRIBUTE_NAME
                            );
                            return Err(XSDError::SrcAttribute);
                        }
                        Occurrence::Required
                    }
                    _ => todo!("raise error"),
                }
            }
            None => Occurrence::Optional,
        };

        self.check_attribute(elem.clone())?;

        let simple_type = None;

        let type_definition = match (r#type, simple_type) {
            (Some(_), Some(_)) => {
                // [src-attribute]
                // 4 type and <simpleType> must not both be present.
                error!(
                    self.handler,
                    SrcAttribute,
                    elem,
                    "'{}' must not have both the attribute 'type' and the child element '{}'.",
                    SCHEMA_ATTRIBUTE_NAME,
                    SCHEMA_SIMPLETYPE_NAME
                );
                return Err(XSDError::SrcAttribute);
            }
            (Some(r#type), None) => {
                if !global && r#ref.is_some() {
                    // [src-attribute]
                    // 3.2 If ref is present, then all of <simpleType>, form and type must be absent.
                    error!(
                        self.handler,
                        SrcAttribute,
                        elem,
                        "The attribute 'type' of '{}' must be absent when 'ref' is present.",
                        SCHEMA_ATTRIBUTE_NAME
                    );
                    return Err(XSDError::SrcAttribute);
                }
                match SCHEMA_BUILTIN_QNAME.parse_with_elem(&r#type, elem.clone()) {
                    Ok(SchemaValue::QName(nsname, local)) => {
                        match self.schema.type_defs.get(&(nsname, local)) {
                            Some(TypeDefinition::Simple(simple)) => simple.clone(),
                            Some(_) => todo!("raise error because of non-simple-type definition"),
                            None => todo!("raise error because of missing definition"),
                        }
                    }
                    Ok(_) => internal_error!(self.handler, elem),
                    Err(err) => {
                        error!(
                            self.handler,
                            SrcAttribute,
                            elem,
                            "The attribute 'type' of 'attribute' is failed to parse as QName because {}.",
                            err
                        );
                        return Err(XSDError::SrcAttribute);
                    }
                }
            }
            (None, Some(def)) => {
                if !global && r#ref.is_some() {
                    // [src-attribute]
                    // 3.2 If ref is present, then all of <simpleType>, form and type must be absent.
                    error!(
                        self.handler,
                        SrcAttribute,
                        elem,
                        "'{}' must have no '{}' children when the attribute 'ref' is present.",
                        SCHEMA_ATTRIBUTE_NAME,
                        SCHEMA_SIMPLETYPE_NAME
                    );
                    return Err(XSDError::SrcAttribute);
                }
                def
            }
            (None, None) => {
                if global {
                    // 3.2.2 XML Representation of Attribute Declaration Schema Components
                    // {type definition}	The simple type definition corresponding to the
                    // <simpleType> element information item in the [children], if present,
                    // otherwise the simple type definition ·resolved· to by the ·actual
                    // value· of the type [attribute], if present, otherwise the ·simple
                    // ur-type definition·.
                    // (*SCHEMA_BUILTIN_ANY_SIMPLE_TYPE).clone()
                    todo!()
                } else if let Some(r#ref) = r#ref.as_deref() {
                    // 3.2.2 XML Representation of Attribute Declaration Schema Components
                    // {attribute declaration}	The (top-level) attribute declaration
                    // ·resolved· to by the ·actual value· of the ref [attribute]

                    // This branch may be reached during the parsing of a local attribute
                    // declaration.
                    // The actual value of `ref` must be resolved to a global attribute
                    // declaration, but the global attribute declaration has already been
                    // parsed before the attribute group declaration or composite type
                    // definition in which the local attribute declaration might appear.
                    // Therefore, if the actual value of `ref` cannot be resolved to any
                    // declaration at this point, it is acceptable to treat this as an error.
                    match SCHEMA_BUILTIN_QNAME.parse_with_elem(r#ref, elem.clone()) {
                        Ok(SchemaValue::QName(nsname, local)) => {
                            match self.schema.type_defs.get(&(nsname, local)) {
                                Some(TypeDefinition::Simple(simple)) => simple.clone(),
                                Some(_) => {
                                    todo!("raise error because of non-simple type definition")
                                }
                                None => todo!("raise error because of missing definition"),
                            }
                        }
                        Ok(_) => internal_error!(self.handler, elem),
                        Err(err) => {
                            error!(
                                self.handler,
                                SrcAttribute,
                                elem,
                                "The attribute 'ref' of 'attribute' is failed to parse as NCName because {}.",
                                err
                            );
                            return Err(XSDError::SrcAttribute);
                        }
                    }
                } else {
                    // 3.2.2 XML Representation of Attribute Declaration Schema Components
                    // {type definition}	The simple type definition corresponding to the
                    // <simpleType> element information item in the [children], if present,
                    // otherwise the simple type definition ·resolved· to by the ·actual
                    // value· of the type [attribute], if present, otherwise the ·simple
                    // ur-type definition·.
                    // SCHEMA_BUILTIN_ANY_SIMPLE_TYPE.clone()
                    todo!()
                }
            }
        };
        let value_constraint = match (default, fixed) {
            (Some(_), Some(_)) => {
                // [src-attribute]
                // 1 default and fixed must not both be present.
                error!(
                    self.handler,
                    SrcAttribute,
                    elem,
                    "The attribute 'default' and 'fixed' must not both be present."
                );
                return Err(XSDError::SrcAttribute);
            }
            (Some(default), None) => {
                // match type_definition.parse_with_elem(&default, elem.clone()) {
                //     Ok(value) => Some(ValueConstraint::Default(value)),
                //     Err(err) => {
                //         error!(
                //             self.handler,
                //             SrcAttribute,
                //             elem,
                //             "The attribute 'default' of '{}' is failed to parse as {} because {}.",
                //             SCHEMA_ATTRIBUTE_NAME,
                //             type_definition.name(),
                //             err
                //         );
                //         return Err(XSDError::SrcAttribute);
                //     }
                // }
                None
            }
            (None, Some(fixed)) => {
                // match type_definition.parse_with_elem(&fixed, elem.clone()) {
                //     Ok(value) => Some(ValueConstraint::Fixed(value)),
                //     Err(err) => {
                //         error!(
                //             self.handler,
                //             SrcAttribute,
                //             elem,
                //             "The attribute 'fixed' of '{}' is failed to parse as {} because {}.",
                //             SCHEMA_ATTRIBUTE_NAME,
                //             type_definition.name(),
                //             err
                //         );
                //         return Err(XSDError::SrcAttribute);
                //     }
                // }
                None
            }
            (None, None) => None,
        };

        if global || r#ref.is_none() {
            let decl = AttributeDeclaration {
                name: name.clone(),
                target_namespace: target_namespace.clone(),
                type_definition,
                scope: Some(Scope::Global),
                value_constraint,
                annotation: None,
            };
            self.schema
                .att_decls
                .insert((target_namespace.clone(), decl.name.clone()), decl);
            Ok(None)
        } else if r#ref.is_none() {
            let decl = AttributeDeclaration {
                name: name.clone(),
                target_namespace: target_namespace.clone(),
                type_definition,
                scope: None,
                value_constraint: None,
                annotation: None,
            };
            self.schema
                .att_decls
                .insert((target_namespace.clone(), decl.name.clone()), decl);
            Ok(Some(AttributeUse {
                required: r#use,
                attribute_declaration: (target_namespace, name),
                value_constraint,
            }))
        } else {
            Ok(Some(AttributeUse {
                required: r#use,
                attribute_declaration: (target_namespace, name),
                value_constraint,
            }))
        }
    }

    fn parse_top_level_element(
        &mut self,
        mut elem: Element,
        target_namespace: Option<Arc<str>>,
    ) -> Result<(), XSDError> {
        // "ref", "form", "minOccurs" and "maxOccurs" are prohibited explicitly in schema of schema.
        let r#abstract = self
            .extract_boolean(elem.clone(), "abstract", XSDError::SrcElement)?
            .unwrap_or(false);
        // let block = self
        //     .extract_final(
        //         elem.clone(),
        //         "block",
        //         Final::Restriction | Final::Extension | Final::Substitution,
        //         true,
        //     )?
        //     .unwrap_or(self.block_default);
        let block = FinalSet::default();
        let default = elem.remove_attribute("default", None);
        let r#final = self
            .extract_final(
                elem.clone(),
                "final",
                Final::Extension | Final::Restriction,
                true,
            )?
            .unwrap_or(self.final_default);
        let fixed = elem.remove_attribute("fixed", None);
        let _id = elem.remove_attribute("id", None);
        let Some(name) = self.extract_ncname(elem.clone(), "name", XSDError::SrcElement)? else {
            todo!("raise error because of missing a mandatory attribute");
        };
        let nillable = self
            .extract_boolean(elem.clone(), "nillable", XSDError::SrcElement)?
            .unwrap_or(false);
        let substitution_group = self.extract_qname(elem.clone(), "substitutionGroup")?;
        let r#type = self.extract_qname(elem.clone(), "type")?;

        self.check_attribute(elem.clone())?;

        let mut children = elem.first_child();

        let mut type_definition = None;
        while let Some(child) = children {
            children = child.next_sibling();

            match child.downcast() {
                NodeKind::Element(elem) => {
                    if elem.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE) {
                        continue;
                    }

                    match elem.local_name().as_ref() {
                        SCHEMA_COMPLEXTYPE_NAME => {
                            type_definition = Some(TypeDefinition::Complex(
                                self.parse_complex_type_definitions(
                                    elem,
                                    target_namespace.clone(),
                                    &mut vec![],
                                    false,
                                )?,
                            ));
                        }
                        _ => {
                            children = Some(child);
                        }
                    }
                    break;
                }
                NodeKind::Text(_) => unallowed_text!(self.handler, elem),
                _ => {}
            }
        }

        let idc_definitions = vec![];

        self.check_unexpected_schema_elements(children)?;

        let type_definition =
            self.resolve_element_type(elem.clone(), r#type, type_definition, false)?;
        let value_constraint =
            self.resolve_element_default(elem.clone(), default, fixed, &type_definition)?;

        let decl = ElementDeclaration {
            name: name.clone(),
            target_namespace: target_namespace.clone(),
            type_definition,
            scope: Some(Scope::Global),
            value_constraint,
            nillable,
            idc_definitions,
            substitution_group_affiliation: substitution_group,
            substitution_group_exclusions: r#final,
            disallowed_substitutions: block,
            r#abstract,
            annotation: None,
        };

        self.schema
            .elem_decls
            .insert((target_namespace, name), decl);
        Ok(())
    }

    fn parse_local_element(
        &mut self,
        mut elem: Element,
        target_namespace: Option<Arc<str>>,
        scope: Option<Scope>,
    ) -> Result<Particle, XSDError> {
        // "substitutionGroup", "final" and "abstract" are prohibited explicitly in schema of schema.
        // let block = self.extract_final(
        //     elem.clone(),
        //     "block",
        //     Final::Extension | Final::Restriction | Final::Substitution,
        //     true,
        // )?;
        let block = Some(FinalSet::default());
        let default = elem.remove_attribute("default", None);
        let fixed = elem.remove_attribute("fixed", None);
        let _id = elem.remove_attribute("id", None);
        let max_occurs = self.extract_occurs(elem.clone(), "maxOccurs", Some(1), true)?;
        let Some(min_occurs) = self.extract_occurs(elem.clone(), "minOccurs", Some(1), false)?
        else {
            internal_error!(self.handler, elem);
        };
        let name = self.extract_ncname(elem.clone(), "name", XSDError::SrcElement)?;
        let r#ref = self.extract_qname(elem.clone(), "ref")?;

        let mut children = elem.first_child();

        let mut type_definition = None;
        while let Some(child) = children {
            children = child.next_sibling();

            match child.downcast() {
                NodeKind::Element(elem) => {
                    if elem.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE) {
                        continue;
                    }

                    match elem.local_name().as_ref() {
                        SCHEMA_COMPLEXTYPE_NAME => {
                            type_definition = Some(TypeDefinition::Complex(
                                self.parse_complex_type_definitions(
                                    elem,
                                    target_namespace.clone(),
                                    &mut vec![],
                                    false,
                                )?,
                            ));
                        }
                        _ => children = Some(child),
                    }
                    break;
                }
                NodeKind::Text(_) => unallowed_text!(self.handler, elem),
                _ => {}
            }
        }

        let mut idc_definitions = vec![];

        self.check_unexpected_schema_elements(children)?;

        match (name, r#ref) {
            (Some(_), Some(_)) | (None, None) => {
                // [src-element] clause 2.1
                error!(
                    self.handler,
                    SrcElement,
                    elem,
                    "One of 'ref' or 'name' must be present, but not both for local 'element'."
                );
                Err(XSDError::SrcElement)
            }
            (Some(name), None) => {
                let form = self
                    .extract_qualification_form(elem.clone(), "form", XSDError::SrcElement)?
                    .unwrap_or(self.elem_form_default);
                let nillable = self
                    .extract_boolean(elem.clone(), "nillable", XSDError::SrcElement)?
                    .unwrap_or(false);
                let r#type = self.extract_qname(elem.clone(), "type")?;
                let type_definition =
                    self.resolve_element_type(elem.clone(), r#type, type_definition, true)?;
                let target_namespace =
                    target_namespace.filter(|_| matches!(form, QualificationForm::Qualified));

                self.check_attribute(elem.clone())?;

                let value_constraint =
                    self.resolve_element_default(elem.clone(), default, fixed, &type_definition)?;

                let decl = ElementDeclaration {
                    name,
                    target_namespace,
                    type_definition,
                    scope,
                    value_constraint,
                    nillable,
                    idc_definitions,
                    substitution_group_affiliation: None,
                    substitution_group_exclusions: FinalSet::default(),
                    disallowed_substitutions: block.unwrap_or(self.block_default),
                    r#abstract: false,
                    annotation: None,
                };

                self.unnamed += 1;
                // Change the name so that it doesn't conflict with the top-level declaration.
                let local: Arc<str> = format!("#local{}/{}", self.unnamed, decl.name).into();
                let particle = Particle {
                    min_occurs,
                    max_occurs,
                    term: Term::ElementDecl(decl.target_namespace.clone(), local.clone()),
                };
                self.schema
                    .elem_decls
                    .insert((decl.target_namespace.clone(), local), decl);
                Ok(particle)
            }
            (None, Some(r#ref)) => {
                let form = elem.remove_attribute("form", None);
                let nillable = elem.remove_attribute("nillable", None);
                let r#type = elem.remove_attribute("type", None);

                macro_rules! unallowed_schema_attribute {
                    ($name:literal, $cond:expr) => {
                        if $cond {
                            error!(
                                self.handler,
                                SrcElement,
                                elem,
                                "The attribute '{}' is not allowed on the local element declaration when 'ref' is present.",
                                $name
                            );
                        }
                    };
                }
                macro_rules! unallowed_schema_element {
                    ($cond:expr, $name:literal) => {
                        if $cond {
                            error!(
                                self.handler,
                                SrcElement,
                                elem,
                                "The element {} are not allowed on the local element declaration when 'ref' is present.",
                                $name
                            );
                        }
                    };
                }
                // [src-element] clause 2.2
                unallowed_schema_attribute!("nillable", nillable.is_some());
                unallowed_schema_attribute!("default", default.is_some());
                unallowed_schema_attribute!("fixed", fixed.is_some());
                unallowed_schema_attribute!("form", form.is_some());
                unallowed_schema_attribute!("block", block.is_some());
                unallowed_schema_attribute!("type", r#type.is_some());
                unallowed_schema_element!(
                    type_definition.is_some(),
                    "'simpleType' and 'complexType'"
                );
                unallowed_schema_element!(
                    idc_definitions.is_empty(),
                    "'key', 'keyref' and 'unique'"
                );

                self.check_attribute(elem.clone())?;

                if let Some(elem) = self.elem_decls.remove(&r#ref) {
                    self.parse_top_level_element(elem, target_namespace)?;
                }
                if !self.schema.elem_decls.contains_key(&r#ref) {
                    todo!("raise error because of missing element declaration");
                }

                Ok(Particle {
                    min_occurs,
                    max_occurs,
                    term: Term::ElementDecl(r#ref.0, r#ref.1),
                })
            }
        }
    }

    fn resolve_element_default(
        &mut self,
        elem: Element,
        default: Option<String>,
        fixed: Option<String>,
        typedef: &TypeDefinition,
    ) -> Result<Option<ValueConstraint>, XSDError> {
        if default.is_some() && fixed.is_some() {
            error!(
                self.handler,
                SrcElement,
                elem,
                "The attribute 'default' and 'fixed' of 'element' must not both be present."
            );
            Err(XSDError::SrcElement)
        } else if default.is_none() && fixed.is_none() {
            Ok(None)
        } else {
            let value = default.as_deref().or(fixed.as_deref()).unwrap();
            let value = match typedef {
                TypeDefinition::Simple(def) => {
                    // [cos-valid-default] clause 1
                    // match def.parse_with_elem(value, elem.clone()) {
                    //     Ok(value) => value,
                    //     Err(err) => todo!("raise error"),
                    // }
                }
                TypeDefinition::Complex(def) => {
                    match &def.content_type {
                        ContentType::Simple(def) => {
                            // [cos-valid-default] clause 2.2.1
                            // match def.parse_with_elem(value, elem.clone()) {
                            //     Ok(value) => value,
                            //     Err(err) => todo!("raise error"),
                            // }
                        }
                        ContentType::Mixed(particle) => {
                            if !particle.is_emptiable() {
                                // [cos-valid-default] clause 2.2.2
                                error!(
                                    self.handler,
                                    CosValidDefault,
                                    elem,
                                    "Default values must not be set for complex content that is non-empty mixed content."
                                );
                                return Err(XSDError::CosValidDefault);
                            }
                            // It is unclear what type this should be checked as,
                            // but for now, let's treat it as a string type.
                            // SchemaValue::String(value.into())
                        }
                        _ => {
                            // [cos-valid-default] clause 2.1
                            error!(
                                self.handler,
                                CosValidDefault,
                                elem,
                                "Default values must not be set for complex content that is either empty or element-only content."
                            );
                            return Err(XSDError::CosValidDefault);
                        }
                    }
                }
            };

            Ok(None)
            // Ok(if default.is_some() {
            //     Some(ValueConstraint::Default(value))
            // } else {
            //     Some(ValueConstraint::Fixed(value))
            // })
        }
    }

    fn resolve_element_type(
        &mut self,
        elem: Element,
        r#type: Option<(Option<Arc<str>>, Arc<str>)>,
        typedef: Option<TypeDefinition>,
        allow_missing: bool,
    ) -> Result<TypeDefinition, XSDError> {
        match (r#type, typedef) {
            (Some(_), Some(_)) | (None, None) => {
                // [src-element] clause 3
                error!(
                    self.handler,
                    SrcElement,
                    elem,
                    "The attribute 'type' and either a element child 'simpleType' or 'complexType' are mutually exclusive."
                );
                Err(XSDError::SrcElement)
            }
            (Some(key), None) => {
                if let Some(elem) = self.complex_types.remove(&key) {
                    self.parse_complex_type_definitions(elem, key.0.clone(), &mut vec![], true)?;
                }

                if let Some(def) = self.schema.type_defs.get(&key) {
                    Ok(def.clone())
                } else if allow_missing {
                    // Since the complex type definition may not have been
                    // resolved yet, set a temporary type definition.
                    Ok(TypeDefinition::Complex(Arc::new(ComplexTypeDefinition {
                        name: key.1,
                        target_namespace: key.0,
                        base_type_definition: None,
                        derivation_method: DerivationMethod::Restriction,
                        r#final: FinalSet::default(),
                        r#abstract: false,
                        attribute_uses: vec![],
                        attribute_wildcard: None,
                        content_type: ContentType::Empty,
                        prohibited_substitutions: FinalSet::default(),
                    })))
                } else {
                    todo!("raise error because of missing type definitions");
                }
            }
            (None, Some(type_definition)) => Ok(type_definition),
        }
    }

    /// # Reference
    /// - [src-attribute_group](https://www.w3.org/TR/xmlschema-1/#src-attribute_group)
    fn parse_named_attgroup_definition(
        &mut self,
        mut elem: Element,
        target_namespace: Option<Arc<str>>,
        loop_guard: &mut Vec<(Option<Arc<str>>, Arc<str>)>,
    ) -> Result<(), XSDError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert_eq!(elem.local_name().as_ref(), SCHEMA_ATTGROUP_NAME);

        let _id = elem.remove_attribute("id", None);
        let Some(name) = self.extract_ncname(elem.clone(), "name", XSDError::SrcAttributeGroup)?
        else {
            todo!("raise error")
        };

        loop_guard.push((target_namespace.clone(), name.clone()));

        self.check_attribute(elem.clone())?;

        let mut children = elem.first_child();
        let (attribute_uses, attribute_wildcard) =
            self.pop_attr_decls(&mut children, target_namespace.clone(), loop_guard)?;

        self.check_unexpected_schema_elements(children)?;

        let def = AttributeGroupDefinition {
            name: name.clone(),
            target_namespace: target_namespace.clone(),
            attribute_uses,
            attribute_wildcard,
            annotation: None,
        };

        loop_guard.pop();

        self.schema
            .attgroup_defs
            .insert((target_namespace, name), def);
        Ok(())
    }

    /// # Reference
    /// - [src-attribute_group](https://www.w3.org/TR/xmlschema-1/#src-attribute_group)
    fn parse_attgroup_reference(
        &mut self,
        mut elem: Element,
        loop_guard: &mut Vec<(Option<Arc<str>>, Arc<str>)>,
    ) -> Result<(Option<Arc<str>>, Arc<str>), XSDError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert_eq!(elem.local_name().as_ref(), SCHEMA_ATTGROUP_NAME);

        let _id = elem.remove_attribute("id", None);
        let Some(r#ref) = self.extract_qname(elem.clone(), "ref")? else {
            todo!("raise error")
        };

        self.check_attribute(elem.clone())?;

        if self.schema.attgroup_defs.contains_key(&r#ref) {
            return Ok(r#ref);
        }

        if loop_guard.contains(&r#ref) {
            todo!("raise error because of a reference loop")
        }

        let Some(elem) = self.att_groups.remove(&r#ref) else {
            todo!("raise error because of missing definition")
        };
        self.parse_named_attgroup_definition(elem, r#ref.0.clone(), loop_guard)?;
        Ok(r#ref)
    }

    fn parse_complex_type_definitions(
        &mut self,
        mut elem: Element,
        target_namespace: Option<Arc<str>>,
        loop_guard: &mut Vec<(Option<Arc<str>>, Arc<str>)>,
        global: bool,
    ) -> Result<Arc<ComplexTypeDefinition>, XSDError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert_eq!(elem.local_name().as_ref(), SCHEMA_COMPLEXTYPE_NAME);

        let r#abstract = self
            .extract_boolean(elem.clone(), "abstract", XSDError::SrcCt)?
            .unwrap_or(false);
        let block = self
            .extract_final(
                elem.clone(),
                "block",
                Final::Extension | Final::Restriction,
                true,
            )?
            .unwrap_or(self.block_default);
        let r#final = self
            .extract_final(
                elem.clone(),
                "final",
                Final::Extension | Final::Restriction,
                true,
            )?
            .unwrap_or(self.final_default);
        let _id = elem.remove_attribute("id", None);
        let mixed = self
            .extract_boolean(elem.clone(), "mixed", XSDError::SrcCt)?
            .unwrap_or(false);
        let name = match self.extract_ncname(elem.clone(), "name", XSDError::SrcCt)? {
            Some(name) => name,
            None => {
                if !global {
                    self.unnamed += 1;
                    // Choose a name that is not an NCName to avoid conflicts
                    // with other type definitions.
                    format!("#unnamed{}", self.unnamed).into()
                } else {
                    error!(
                        self.handler,
                        SrcCt,
                        elem,
                        "The mandatory attribute 'name' of '{}' is missing.",
                        elem.local_name()
                    );
                    return Err(XSDError::SrcCt);
                }
            }
        };
        if self
            .schema
            .type_defs
            .contains_key(&(target_namespace.clone(), name.clone()))
        {
            todo!("raise error because duplicated definition");
        }
        loop_guard.push((target_namespace.clone(), name.clone()));

        self.check_attribute(elem.clone())?;

        let mut children = elem.first_child();
        // simpleContent
        // | complexContent
        // | ((group | all | choice | sequence)?, (attribute | attributeGroup)*, anyAttribute?)
        // extract content model
        let mut def = None;
        while let Some(child) = children {
            children = child.next_sibling();

            match child.downcast() {
                NodeKind::Element(elem) => {
                    if elem.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE) {
                        continue;
                    }

                    def = match elem.local_name().as_ref() {
                        SCHEMA_SIMPLECONT_NAME => {
                            let (
                                base_type_definition,
                                derivation_method,
                                content_type,
                                attribute_uses,
                                attribute_wildcard,
                            ) = self.parse_simple_content(
                                elem,
                                target_namespace.clone(),
                                loop_guard,
                            )?;

                            Some(Arc::new(ComplexTypeDefinition {
                                name: name.clone(),
                                target_namespace: target_namespace.clone(),
                                base_type_definition: Some(base_type_definition),
                                derivation_method,
                                r#final,
                                r#abstract,
                                attribute_uses,
                                attribute_wildcard,
                                content_type,
                                prohibited_substitutions: block,
                            }))
                        }
                        SCHEMA_COMPLEXCONT_NAME => {
                            let (
                                base_type_definition,
                                derivation_method,
                                content_type,
                                attribute_uses,
                                attribute_wildcard,
                            ) = self.parse_complex_content(
                                elem,
                                target_namespace.clone(),
                                loop_guard,
                                mixed,
                            )?;

                            Some(Arc::new(ComplexTypeDefinition {
                                name: name.clone(),
                                target_namespace: target_namespace.clone(),
                                base_type_definition: Some(base_type_definition),
                                derivation_method,
                                r#final,
                                r#abstract,
                                attribute_uses,
                                attribute_wildcard,
                                content_type,
                                prohibited_substitutions: block,
                            }))
                        }
                        _ => {
                            // Reference: 3.4.2 XML Representation of Complex Type Definitions
                            // > The property mappings below are also used in the case where the
                            // > third alternative (neither <simpleContent> nor <complexContent>)
                            // > is chosen. This case is understood as shorthand for complex
                            // > content restricting the ·ur-type definition·, and the details
                            // > of the mappings should be modified as necessary.
                            children = Some(child);
                            let particle = self
                                .pop_type_def_particle(&mut children, target_namespace.clone())?;
                            let (attribute_uses, attribute_wildcard) = self.pop_attr_decls(
                                &mut children,
                                target_namespace.clone(),
                                loop_guard,
                            )?;

                            let effective_content =
                                self.effective_content(particle, elem, mixed)?;
                            let content_type = effective_content
                                .map(|c| {
                                    if mixed {
                                        ContentType::Mixed(c)
                                    } else {
                                        ContentType::ElementOnly(c)
                                    }
                                })
                                .unwrap_or(ContentType::Empty);

                            Some(Arc::new(ComplexTypeDefinition {
                                name: name.clone(),
                                target_namespace: target_namespace.clone(),
                                base_type_definition: Some(TypeDefinition::Complex(
                                    SCHEMA_BUILTIN_ANY_TYPE.clone(),
                                )),
                                derivation_method: DerivationMethod::Restriction,
                                r#final,
                                r#abstract,
                                attribute_uses,
                                attribute_wildcard,
                                content_type,
                                prohibited_substitutions: block,
                            }))
                        }
                    };
                    break;
                }
                NodeKind::Text(_) => unallowed_text!(self.handler, elem),
                _ => {}
            }
        }
        self.check_unexpected_schema_elements(children)?;

        loop_guard.pop();

        let def = if let Some(def) = def {
            def
        } else {
            // This is a model definition that omits `complexContent` and is
            // treated as having an empty model and an empty set of attributes.
            Arc::new(ComplexTypeDefinition {
                name: name.clone(),
                target_namespace: target_namespace.clone(),
                base_type_definition: Some(TypeDefinition::Complex(
                    SCHEMA_BUILTIN_ANY_TYPE.clone(),
                )),
                derivation_method: DerivationMethod::Restriction,
                r#final,
                r#abstract,
                attribute_uses: vec![],
                attribute_wildcard: None,
                content_type: ContentType::Empty,
                prohibited_substitutions: block,
            })
        };

        if global {
            self.schema.type_defs.insert(
                (target_namespace, name),
                TypeDefinition::Complex(def.clone()),
            );
        }

        Ok(def)
    }

    fn parse_simple_content(
        &mut self,
        mut elem: Element,
        target_namespace: Option<Arc<str>>,
        loop_guard: &mut Vec<(Option<Arc<str>>, Arc<str>)>,
    ) -> Result<
        (
            TypeDefinition,
            DerivationMethod,
            ContentType,
            Vec<AttributeUse>,
            Option<Wildcard>,
        ),
        XSDError,
    > {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert_eq!(elem.local_name().as_ref(), SCHEMA_SIMPLECONT_NAME);

        let _id = elem.remove_attribute("id", None);
        self.check_attribute(elem.clone())?;

        let mut children = elem.first_child();
        // extract derivation method
        let mut content = None;
        let mut attribute_uses = vec![];
        let mut attribute_wildcard = None;
        let mut derivation_method = DerivationMethod::Restriction;
        let mut base_type_definition = TypeDefinition::Complex(SCHEMA_BUILTIN_ANY_TYPE.clone());
        while let Some(child) = children {
            children = child.next_sibling();

            match child.downcast() {
                NodeKind::Element(mut elem) => {
                    if elem.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE) {
                        continue;
                    }

                    match elem.local_name().as_ref() {
                        SCHEMA_RESTRICTION_NAME => {
                            derivation_method = DerivationMethod::Restriction;
                            let mut children = elem.first_child();
                            // extract simpleType if exists
                            // This `simpleType` may be used to determine the {content type}
                            // property of a `complexType` component.
                            //
                            // ```
                            // {content type}	the appropriate case among the following:
                            //     1 If the type definition ·resolved· to by the ·actual value·
                            //       of the base [attribute] is a complex type definition whose
                            //       own {content type} is a simple type definition and the
                            //       <restriction> alternative is chosen, then starting from either
                            //         1.1 the simple type definition corresponding to the
                            //             <simpleType> among the [children] of <restriction>
                            //             if there is one;
                            //
                            //         ...<snip>...
                            //
                            //         a simple type definition which restricts the simple
                            //         type definition identified in clause 1.1 or clause 1.2
                            //         with a set of facet components corresponding to the
                            //         appropriate element information items among the
                            //         <restriction>'s [children] (i.e. those which specify
                            //         facets, if any), as defined in Simple Type Restriction
                            //         (Facets) (§3.14.6);
                            // ```
                            let simple_type = None;
                            while let Some(child) = children {
                                children = child.next_sibling();

                                match child.downcast() {
                                    NodeKind::Element(elem) => {
                                        if elem.namespace_name().as_deref()
                                            != Some(XML_SCHEMA_NAMESPACE)
                                        {
                                            continue;
                                        }
                                        break;
                                    }
                                    NodeKind::Text(_) => unallowed_text!(self.handler, elem),
                                    _ => {}
                                }
                            }

                            let _id = elem.remove_attribute("id", None);
                            let Some(key) = self.extract_qname(elem.clone(), "base")? else {
                                error!(
                                    self.handler,
                                    SrcCt,
                                    elem,
                                    "The attribute 'base' of '{}' is mandatory, but is absent.",
                                    elem.local_name()
                                );
                                return Err(XSDError::SrcCt);
                            };
                            if let Some(elem) = self.complex_types.remove(&key) {
                                self.parse_complex_type_definitions(
                                    elem,
                                    key.0.clone(),
                                    loop_guard,
                                    true,
                                )?;
                            }
                            let base = if let Some(def) = self.schema.type_defs.get(&key) {
                                base_type_definition = def.clone();
                                match def {
                                    TypeDefinition::Simple(_) => {
                                        error!(
                                            self.handler,
                                            SrcCt,
                                            elem,
                                            "The base type of '{}' must not be a simple type.",
                                            SCHEMA_SIMPLECONT_NAME
                                        );
                                        return Err(XSDError::SrcCt);
                                    }
                                    TypeDefinition::Complex(complex) => {
                                        match &complex.content_type {
                                            ContentType::Simple(simple) => {
                                                if let Some(def) = simple_type {
                                                    // component property mapping of {content type}
                                                    // clause 1.1
                                                    def
                                                } else {
                                                    // component property mapping of {content type}
                                                    // clause 1.2 and [src-ct] 2.1.1
                                                    simple.clone()
                                                }
                                            }
                                            ContentType::Mixed(mixed) => {
                                                // [src-ct] 2.1.2
                                                if !mixed.is_emptiable() {
                                                    error!(
                                                        self.handler,
                                                        SrcCt,
                                                        elem,
                                                        "The base type of '{}' must be emptiable if it is a complex type.",
                                                        SCHEMA_SIMPLECONT_NAME
                                                    );
                                                    return Err(XSDError::SrcCt);
                                                }
                                                // component property mapping of {content type}
                                                // clause 2 and [src-ct] 2.2
                                                let Some(def) = simple_type else {
                                                    error!(
                                                        self.handler,
                                                        SrcCt,
                                                        elem,
                                                        "'{}' that is a child of '{}' must have a '{}' child when the base type is a complex type.",
                                                        SCHEMA_RESTRICTION_NAME,
                                                        SCHEMA_SIMPLECONT_NAME,
                                                        SCHEMA_SIMPLETYPE_NAME
                                                    );
                                                    return Err(XSDError::SrcCt);
                                                };
                                                def
                                            }
                                            _ => {
                                                // [src-ct] 2
                                                error!(
                                                    self.handler,
                                                    SrcCt,
                                                    elem,
                                                    "The base type of '{}' whose derivation method is '{}' must be a complex type whose content type is a simple type or mixed emptiable model.",
                                                    SCHEMA_SIMPLECONT_NAME,
                                                    elem.local_name()
                                                );
                                                return Err(XSDError::SrcCt);
                                            }
                                        }
                                    }
                                }
                            } else if loop_guard.contains(&key) {
                                todo!("raise error because of type reference loop")
                            } else {
                                todo!("raise error because of undefined type")
                            };

                            // let mut builder = match base.clone().derive_by_restriction() {
                            //     Ok(builder) => builder,
                            //     Err(err) => todo!("raise error"),
                            // };

                            // content = Some(builder.build());

                            (attribute_uses, attribute_wildcard) = self.pop_attr_decls(
                                &mut children,
                                target_namespace.clone(),
                                loop_guard,
                            )?;
                            self.check_unexpected_schema_elements(children)?;
                        }
                        SCHEMA_EXTENSION_NAME => {
                            derivation_method = DerivationMethod::Extension;
                            let mut children = elem.first_child();
                            let _id = elem.remove_attribute("id", None);
                            let Some(key) = self.extract_qname(elem.clone(), "base")? else {
                                error!(
                                    self.handler,
                                    SrcCt,
                                    elem,
                                    "The attribute 'base' of '{}' is mandatory, but is absent.",
                                    elem.local_name()
                                );
                                return Err(XSDError::SrcCt);
                            };
                            if let Some(elem) = self.complex_types.remove(&key) {
                                self.parse_complex_type_definitions(
                                    elem,
                                    key.0.clone(),
                                    loop_guard,
                                    true,
                                )?;
                            }
                            let base = if let Some(def) = self.schema.type_defs.get(&key) {
                                base_type_definition = def.clone();
                                match def {
                                    TypeDefinition::Simple(simple) => {
                                        // [src-ct] 2.1.3
                                        simple.clone()
                                    }
                                    TypeDefinition::Complex(complex) => {
                                        match &complex.content_type {
                                            ContentType::Simple(simple) => {
                                                // [src-ct] 2.1.1
                                                simple.clone()
                                            }
                                            _ => {
                                                // [src-ct] 2.1
                                                error!(
                                                    self.handler,
                                                    SrcCt,
                                                    elem,
                                                    "The base type of '{}' whose derivation method is '{}' must be a complex type whose content type is a simple type.",
                                                    SCHEMA_SIMPLECONT_NAME,
                                                    elem.local_name()
                                                );
                                                return Err(XSDError::SrcCt);
                                            }
                                        }
                                    }
                                }
                            } else if loop_guard.contains(&key) {
                                todo!("raise error because of type reference loop")
                            } else {
                                todo!("raise error because of undefined type")
                            };
                            content = Some(base);

                            (attribute_uses, attribute_wildcard) = self.pop_attr_decls(
                                &mut children,
                                target_namespace.clone(),
                                loop_guard,
                            )?;
                            self.check_unexpected_schema_elements(children)?;
                        }
                        _ => unallowed_element!(self.handler, elem),
                    }
                    break;
                }
                NodeKind::Text(_) => unallowed_text!(self.handler, elem),
                _ => {}
            }
        }

        let Some(content) = content else {
            error!(
                self.handler,
                SrcCt,
                elem,
                "'{}' must have one of '{}' or '{}' as a child.",
                elem.local_name(),
                SCHEMA_RESTRICTION_NAME,
                SCHEMA_EXTENSION_NAME
            );
            return Err(XSDError::SrcCt);
        };
        Ok((
            base_type_definition,
            derivation_method,
            ContentType::Simple(content),
            attribute_uses,
            attribute_wildcard,
        ))
    }

    fn parse_complex_content(
        &mut self,
        mut elem: Element,
        target_namespace: Option<Arc<str>>,
        loop_guard: &mut Vec<(Option<Arc<str>>, Arc<str>)>,
        mixed: bool,
    ) -> Result<
        (
            TypeDefinition,
            DerivationMethod,
            ContentType,
            Vec<AttributeUse>,
            Option<Wildcard>,
        ),
        XSDError,
    > {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert_eq!(elem.local_name().as_ref(), SCHEMA_COMPLEXCONT_NAME);

        let _id = elem.remove_attribute("id", None);
        // reference: "effective mixed"
        let mixed = self
            .extract_boolean(elem.clone(), "mixed", XSDError::SrcCt)?
            .unwrap_or(mixed);

        let mut children = elem.first_child();
        let mut content_type = None;
        let mut attribute_uses = vec![];
        let mut attribute_wildcard = None;
        let mut derivation_method = DerivationMethod::Restriction;
        let mut base_type_defintion = TypeDefinition::Complex(SCHEMA_BUILTIN_ANY_TYPE.clone());
        while let Some(child) = children {
            children = child.next_sibling();

            match child.downcast() {
                NodeKind::Element(mut elem) => {
                    if elem.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE) {
                        continue;
                    }

                    if !matches!(
                        elem.local_name().as_ref(),
                        SCHEMA_RESTRICTION_NAME | SCHEMA_EXTENSION_NAME
                    ) {
                        unallowed_element!(self.handler, elem);
                    }

                    let _id = elem.remove_attribute("id", None);
                    let Some(base) = self.extract_qname(elem.clone(), "base")? else {
                        error!(
                            self.handler,
                            SrcCt,
                            elem,
                            "The attribute 'base' of '{}' is mandatory, but is absent.",
                            elem.local_name()
                        );
                        return Err(XSDError::SrcCt);
                    };
                    self.check_attribute(elem.clone())?;

                    if let Some(elem) = self.complex_types.remove(&base) {
                        self.parse_complex_type_definitions(
                            elem,
                            target_namespace.clone(),
                            loop_guard,
                            true,
                        )?;
                    }

                    let base = if let Some(def) = self.schema.type_defs.get(&base) {
                        base_type_defintion = def.clone();
                        match def {
                            TypeDefinition::Complex(base) => base.clone(),
                            TypeDefinition::Simple(_) => {
                                // [src-ct] 1
                                error!(
                                    self.handler,
                                    SrcCt,
                                    elem,
                                    "The base type of '{}' must be a complex type.",
                                    SCHEMA_COMPLEXCONT_NAME
                                );
                                return Err(XSDError::SrcCt);
                            }
                        }
                    } else {
                        error!(
                            self.handler,
                            SrcCt,
                            elem,
                            "The base type '{}{}' of '{}' cannot be resolved.",
                            base.0
                                .as_deref()
                                .map(|ns| format!("{{{ns}}}"))
                                .unwrap_or_default(),
                            base.1,
                            elem.local_name()
                        );
                        return Err(XSDError::SrcCt);
                    };

                    let mut children = elem.first_child();
                    let particle =
                        self.pop_type_def_particle(&mut children, target_namespace.clone())?;
                    (attribute_uses, attribute_wildcard) =
                        self.pop_attr_decls(&mut children, target_namespace.clone(), loop_guard)?;

                    if let Some(sub) = attribute_wildcard.as_ref()
                        && base
                            .attribute_wildcard
                            .as_ref()
                            .is_none_or(|sup| !sub.is_subset_of(sup))
                    {
                        error!(
                            self.handler,
                            CosCtExtends,
                            elem,
                            "The attribute wildcard of the extended complex type must be a subset of the one of the base complex type."
                        );
                    }

                    // calculate "effective content" described at clause 2
                    // in the property mapping of the complex type component.
                    let effective_content =
                        self.effective_content(particle, elem.clone(), mixed)?;

                    let ct = match elem.local_name().as_ref() {
                        SCHEMA_RESTRICTION_NAME => {
                            derivation_method = DerivationMethod::Restriction;
                            if base.r#final.restriction() {
                                // [derivation-ok-restriction] clause 1
                                error!(
                                    self.handler,
                                    DerivationOkRestriction,
                                    elem,
                                    "Derivation by restriction for the complex type '{}{}' is not allowed.",
                                    base.target_namespace
                                        .as_deref()
                                        .map(|n| format!("{{{n}}}"))
                                        .unwrap_or_default(),
                                    base.name
                                );
                                return Err(XSDError::DerivationOkRestriction);
                            }

                            // merge attribute uses or children and base type definition
                            for att in &base.attribute_uses {
                                if attribute_uses
                                    .iter()
                                    .any(|a| a.attribute_declaration == att.attribute_declaration)
                                {
                                    // property mapping of {attribute uses} clause 3.1
                                    continue;
                                }

                                // prohibited attributes will be removed later
                            }

                            // clause 3.1
                            effective_content
                                .map(|c| {
                                    // clause 3.1.2
                                    if mixed {
                                        ContentType::Mixed(c)
                                    } else {
                                        ContentType::ElementOnly(c)
                                    }
                                })
                                .unwrap_or(ContentType::Empty) // clause 3.1.1
                        }
                        SCHEMA_EXTENSION_NAME => {
                            derivation_method = DerivationMethod::Extension;
                            if base.r#final.extension() {
                                // [cos-ct-extends] clause 1.1
                                error!(
                                    self.handler,
                                    CosCtExtends,
                                    elem,
                                    "Derivation by extension for the complex type '{}{}' is not allowed.",
                                    base.target_namespace
                                        .as_deref()
                                        .map(|n| format!("{{{n}}}"))
                                        .unwrap_or_default(),
                                    base.name
                                );
                                return Err(XSDError::CosCtExtends);
                            }

                            // merge attribute uses or children and base type definition
                            //
                            // It is unclear how to handle cases where the attribute use of
                            // the base type overlaps with a new attribute use. However, in
                            // the case of 'restriction', the specification explicitly
                            // describes removing the attribute use of the base type.
                            // (clause 3 of {attribute uses})
                            // Therefore, since nothing is mentioned regarding the case of
                            // 'extension', it seems that simply adding the new attribute
                            // use is sufficient.
                            attribute_uses.extend(base.attribute_uses.iter().cloned());

                            // clause 3.2
                            match effective_content {
                                Some(c) => {
                                    match base.content_type.clone() {
                                        ContentType::Empty => {
                                            // clause 3.2.2
                                            if mixed {
                                                ContentType::Mixed(c)
                                            } else {
                                                ContentType::ElementOnly(c)
                                            }
                                        }
                                        ContentType::ElementOnly(p) | ContentType::Mixed(p) => {
                                            // clause 3.2.3
                                            let p = Particle {
                                                min_occurs: 1,
                                                max_occurs: Some(1),
                                                term: Term::ModelGroup(ModelGroup {
                                                    compositor: Compositor::Sequence,
                                                    particles: vec![p, c].into(),
                                                }),
                                            };
                                            if mixed {
                                                ContentType::Mixed(p)
                                            } else {
                                                ContentType::ElementOnly(p)
                                            }
                                        }
                                        _ => {
                                            // [cos-ct-extends] clause 1.4.3.2.2.1
                                            error!(
                                                self.handler,
                                                CosCtExtends,
                                                elem,
                                                "Complex types with simple content types cannot be extended using 'complexContent'."
                                            );
                                            return Err(XSDError::CosCtExtends);
                                        }
                                    }
                                }
                                None => {
                                    // clause 3.2.1
                                    base.content_type.clone()
                                }
                            }
                        }
                        _ => unreachable!(),
                    };

                    // remove prohibited attributes
                    attribute_uses.retain(|att| !matches!(att.required, Occurrence::Prohibited));
                    content_type = Some(ct);
                    break;
                }
                NodeKind::Text(_) => unallowed_text!(self.handler, elem),
                _ => {}
            }
        }

        let Some(content_type) = content_type else {
            error!(
                self.handler,
                SrcCt,
                elem,
                "'{}' must have a '{}' or '{}' child, but is not found.",
                elem.local_name(),
                SCHEMA_RESTRICTION_NAME,
                SCHEMA_EXTENSION_NAME
            );
            return Err(XSDError::SrcCt);
        };

        self.check_unexpected_schema_elements(children)?;

        Ok((
            base_type_defintion,
            derivation_method,
            content_type,
            attribute_uses,
            attribute_wildcard,
        ))
    }

    fn parse_any_attribute(
        &mut self,
        mut elem: Element,
        target_namespace: Option<Arc<str>>,
    ) -> Result<Wildcard, XSDError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert_eq!(elem.local_name().as_ref(), SCHEMA_ANYATT_NAME);

        let _id = elem.remove_attribute("id", None);
        let ns_constraint =
            self.extract_namespace_list(elem.clone(), target_namespace.clone(), "namespace")?;
        let process_contents = self.extract_process_contents(elem.clone(), "processContents")?;

        self.check_attribute(elem.clone())?;

        Ok(Wildcard {
            ns_constraint,
            process_contents,
        })
    }

    fn parse_named_group(
        &mut self,
        mut elem: Element,
        target_namespace: Option<Arc<str>>,
    ) -> Result<(), XSDError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert_eq!(elem.local_name().as_ref(), SCHEMA_GROUP_NAME);

        let _id = elem.remove_attribute("id", None);
        let Some(name) = self.extract_ncname(elem.clone(), "name", XSDError::SrcModelGroupDefn)?
        else {
            error!(
                self.handler,
                SrcModelGroupDefn,
                elem,
                "The attribute 'name' of '{}' is mandatory, but is absent.",
                SCHEMA_GROUP_NAME
            );
            return Err(XSDError::SrcModelGroupDefn);
        };
        self.check_attribute(elem.clone())?;

        let mut children = elem.first_child();
        let mut particle = None;
        while let Some(child) = children {
            children = child.next_sibling();

            match child.downcast() {
                NodeKind::Element(elem) => {
                    if elem.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE) {
                        continue;
                    }

                    if !matches!(
                        elem.local_name().as_ref(),
                        SCHEMA_ALL_NAME | SCHEMA_CHOICE_NAME | SCHEMA_SEQUENCE_NAME
                    ) {
                        unallowed_element!(self.handler, elem);
                    }

                    // 'minOccurs' and 'maxOccurs' are not allowed for simpleExplicitGroup
                    if elem.has_attribute("minOccurs", None) {
                        error!(
                            self.handler,
                            SrcModelGroupDefn,
                            elem,
                            "The attribute 'minOccurs' of '{}' that is a child of 'group' is not allowed.",
                            elem.local_name()
                        );
                        return Err(XSDError::SrcModelGroupDefn);
                    }
                    if elem.has_attribute("maxOccurs", None) {
                        error!(
                            self.handler,
                            SrcModelGroupDefn,
                            elem,
                            "The attribute 'maxOccurs' of '{}' that is a child of 'group' is not allowed.",
                            elem.local_name()
                        );
                        return Err(XSDError::SrcModelGroupDefn);
                    }

                    particle = Some(match elem.local_name().as_ref() {
                        SCHEMA_ALL_NAME => self.parse_all(elem, target_namespace.clone())?,
                        SCHEMA_CHOICE_NAME => self.parse_choice(elem, target_namespace.clone())?,
                        SCHEMA_SEQUENCE_NAME => {
                            self.parse_sequence(elem, target_namespace.clone())?
                        }
                        _ => unreachable!(),
                    });
                    break;
                }
                NodeKind::Text(_) => unallowed_text!(self.handler, elem),
                _ => {}
            }
        }
        self.check_unexpected_schema_elements(children)?;

        let Some(particle) = particle else {
            error!(
                self.handler,
                SrcModelGroupDefn,
                elem,
                "'group' that is a child of 'schema' or 'redefine' must have 'all', 'choice' or 'sequence' as a child."
            );
            return Err(XSDError::SrcModelGroupDefn);
        };
        let Term::ModelGroup(model_group) = particle.term else {
            internal_error!(self.handler, elem);
        };

        let model = ModelGroupDefinition {
            name: name.clone(),
            target_namespace: target_namespace.clone(),
            model_group,
            annotation: None,
        };
        self.schema
            .modelgroup_defs
            .insert((target_namespace, name), model);
        Ok(())
    }

    fn parse_group_ref(
        &mut self,
        mut elem: Element,
        target_namespace: Option<Arc<str>>,
    ) -> Result<Particle, XSDError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert_eq!(elem.local_name().as_ref(), SCHEMA_GROUP_NAME);

        let _id = elem.remove_attribute("id", None);
        let max_occurs = self.extract_occurs(elem.clone(), "maxOccurs", Some(1), true)?;
        let Some(min_occurs) = self.extract_occurs(elem.clone(), "minOccurs", Some(1), false)?
        else {
            internal_error!(self.handler, elem);
        };
        let Some(r#ref) = self.extract_ncname(elem.clone(), "ref", XSDError::SrcModelGroup)? else {
            error!(
                self.handler,
                SrcModelGroup,
                elem,
                "The attribute 'ref' of '{}' is mandatory, but is absent.",
                SCHEMA_GROUP_NAME
            );
            return Err(XSDError::SrcModelGroup);
        };
        self.check_attribute(elem.clone())?;

        let mut children = elem.first_child();
        self.check_unexpected_schema_elements(children)?;

        if let Some(elem) = self
            .model_groups
            .remove(&(target_namespace.clone(), r#ref.clone()))
        {
            self.parse_named_group(elem, target_namespace.clone())?;
        }

        let Some(def) = self.schema.modelgroup_defs.get(&(target_namespace, r#ref)) else {
            todo!("raise error because of missing definition")
        };

        Ok(Particle {
            min_occurs,
            max_occurs,
            term: Term::ModelGroup(def.model_group.clone()),
        })
    }

    fn parse_choice(
        &mut self,
        mut elem: Element,
        target_namespace: Option<Arc<str>>,
    ) -> Result<Particle, XSDError> {
        let _id = elem.remove_attribute("id", None);
        let max_occurs = self.extract_occurs(elem.clone(), "maxOccurs", Some(1), true)?;
        let Some(min_occurs) = self.extract_occurs(elem.clone(), "minOccurs", Some(1), false)?
        else {
            internal_error!(self.handler, elem);
        };

        self.check_attribute(elem.clone())?;

        let mut children = elem.first_child();

        let mut particles = vec![];
        while let Some(particle) =
            self.pop_nested_particle(&mut children, target_namespace.clone())?
        {
            particles.push(particle);
        }

        let model = ModelGroup {
            compositor: Compositor::Choice,
            particles: particles.into(),
        };
        Ok(Particle {
            min_occurs,
            max_occurs,
            term: Term::ModelGroup(model),
        })
    }

    fn parse_sequence(
        &mut self,
        mut elem: Element,
        target_namespace: Option<Arc<str>>,
    ) -> Result<Particle, XSDError> {
        let _id = elem.remove_attribute("id", None);
        let max_occurs = self.extract_occurs(elem.clone(), "maxOccurs", Some(1), true)?;
        let Some(min_occurs) = self.extract_occurs(elem.clone(), "minOccurs", Some(1), false)?
        else {
            internal_error!(self.handler, elem);
        };

        self.check_attribute(elem.clone())?;

        let mut children = elem.first_child();

        let mut particles = vec![];
        while let Some(particle) =
            self.pop_nested_particle(&mut children, target_namespace.clone())?
        {
            particles.push(particle);
        }

        let model = ModelGroup {
            compositor: Compositor::Sequence,
            particles: particles.into(),
        };
        Ok(Particle {
            min_occurs,
            max_occurs,
            term: Term::ModelGroup(model),
        })
    }

    fn parse_all(
        &mut self,
        mut elem: Element,
        target_namespace: Option<Arc<str>>,
    ) -> Result<Particle, XSDError> {
        let _id = elem.remove_attribute("id", None);
        let Some(max_occurs) = self.extract_occurs(elem.clone(), "maxOccurs", Some(1), false)?
        else {
            internal_error!(self.handler, elem);
        };
        let Some(min_occurs) = self.extract_occurs(elem.clone(), "minOccurs", Some(1), false)?
        else {
            internal_error!(self.handler, elem);
        };
        if max_occurs != 1 {
            error!(
                self.handler,
                SrcModelGroup,
                elem,
                "The value of attribute 'maxOccurs' of '{}' must be '1', but is '{}'.",
                SCHEMA_ALL_NAME,
                max_occurs
            );
            return Err(XSDError::SrcModelGroup);
        }
        if min_occurs > 1 {
            error!(
                self.handler,
                SrcModelGroup,
                elem,
                "The value of attribute 'minOccurs' of '{}' must be '0' or '1', but is '{}'.",
                SCHEMA_ALL_NAME,
                min_occurs
            );
            return Err(XSDError::SrcModelGroup);
        }

        self.check_attribute(elem.clone())?;

        let mut children = elem.first_child();

        let mut particles = vec![];
        while let Some(child) = children {
            children = child.next_sibling();

            match child.downcast() {
                NodeKind::Element(elem) => {
                    if elem.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE) {
                        continue;
                    }

                    if elem.local_name().as_ref() != SCHEMA_ELEMENT_NAME {
                        unallowed_element!(self.handler, elem);
                    }

                    let particle =
                        self.parse_local_element(elem, target_namespace.clone(), None)?;
                    particles.push(particle);
                }
                NodeKind::Text(_) => unallowed_text!(self.handler, elem),
                _ => {}
            }
        }

        let model = ModelGroup {
            compositor: Compositor::All,
            particles: particles.into(),
        };

        Ok(Particle {
            min_occurs,
            max_occurs: Some(max_occurs),
            term: Term::ModelGroup(model),
        })
    }

    fn parse_any(
        &mut self,
        mut elem: Element,
        target_namespace: Option<Arc<str>>,
    ) -> Result<Particle, XSDError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert_eq!(elem.local_name().as_ref(), SCHEMA_ANY_NAME);

        let _id = elem.remove_attribute("id", None);
        let max_occurs = self.extract_occurs(elem.clone(), "maxOccurs", Some(1), true)?;
        let Some(min_occurs) = self.extract_occurs(elem.clone(), "minOccurs", Some(1), false)?
        else {
            internal_error!(self.handler, elem);
        };
        let ns_constraint =
            self.extract_namespace_list(elem.clone(), target_namespace.clone(), "namespace")?;
        let process_contents = self.extract_process_contents(elem.clone(), "processContents")?;

        self.check_attribute(elem.clone())?;

        let wildcard = Wildcard {
            ns_constraint,
            process_contents,
        };
        Ok(Particle {
            min_occurs,
            max_occurs,
            term: Term::Wildcard(wildcard),
        })
    }

    fn parse_include(&mut self, mut elem: Element) -> Result<(), XSDError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert_eq!(elem.local_name().as_ref(), SCHEMA_INCLUDE_NAME);

        let _id = elem.remove_attribute("id", None);
        let Some(schema_location) =
            self.extract_any_uri(elem.clone(), "schemaLocation", XSDError::SrcInclude)?
        else {
            error!(
                self.handler,
                SrcInclude,
                elem,
                "The mandatory attribute 'schemaLocation' of 'include' is missing."
            );
            return Err(XSDError::SrcInclude);
        };

        self.check_attribute(elem.clone())?;

        if self.included_resources.contains(&schema_location) {
            // According to the note in Section 4.2.1 of XSD 1.0 Part 1,
            // including the same resource multiple times does not result in an error.
            //
            // According to clause 2 of [sch-props-correct], components with the same
            // target namespace and name pair are not allowed. To distinguish between
            // components that are duplicates due to multiple inclusions and those that
            // are not, it is necessary to prevent the same resource from being included
            // multiple times.
            return Ok(());
        }

        let mut reader = XMLReader::builder()
            .set_handler(create_handler(&mut *self.handler))
            .build();

        let included_doc = match reader.parse_uri(&schema_location, None) {
            Ok(()) if !reader.handler.fatal_error => {
                // Register it as an included resource.
                self.included_resources.insert(schema_location.clone());
                reader.handler.document
            }
            Err(XMLError::IOError(_)) => {
                // It is not an error if the resource referenced by `schemaLocation` cannot be resolved.
                // In this context, an I/O error is treated as a failure to resolve the resource.
                return Ok(());
            }
            _ => {
                // [src-include] 1.1 and 1.2
                error!(
                    self.handler,
                    SrcInclude,
                    elem,
                    "The included resource '{}' could not be parsed as XML.",
                    schema_location
                );
                return Err(XSDError::SrcInclude);
            }
        };

        let Some(mut schema) = included_doc.document_element() else {
            internal_error!(self.handler, elem);
        };

        if schema.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE)
            || schema.local_name().as_ref() != SCHEMA_SCHEMA_NAME
        {
            // [src-include] 1.1 and 1.2
            error!(
                self.handler,
                SrcInclude,
                schema,
                "The document element of the included resource '{}' must be '{{{}}}schema', but is '{}{}'.",
                schema_location,
                XML_SCHEMA_NAMESPACE,
                schema
                    .namespace_name()
                    .map(|ns| format!("{{{ns}}}"))
                    .unwrap_or_default(),
                schema.local_name()
            );
            return Err(XSDError::SrcInclude);
        }

        if let Some(target_namespace) = schema.get_attribute("targetNamespace", None) {
            if self
                .target_namespace
                .as_deref()
                .is_none_or(|ns| ns != target_namespace)
            {
                // [src-include] clause 2
                error!(
                    self.handler,
                    SrcInclude,
                    schema,
                    "The included schema's target namespace is '{}', but parent schema's one is '{}'.",
                    target_namespace,
                    self.target_namespace.as_deref().unwrap_or("(null)")
                );
                return Err(XSDError::SrcInclude);
            }
        } else if let Some(target_namespace) = self.target_namespace.as_deref() {
            schema
                .set_attribute("targetNamespace", None, Some(target_namespace))
                .ok();
        }

        // It would be easier if child elements could be retrieved and merged into the parse
        // context, but given the differences in values for `blockDefault`, `finalDefault`,
        // `xxxFormDefault`, and others, the only option is to parse them as separate schemas
        // and then merge them.
        let mut context = XSDParseContext::new(&mut self.handler);
        context.unnamed = self.unnamed;
        context.parse(included_doc)?;
        self.unnamed = context.unnamed;
        let schema = context.schema;
        // Merge schemas
        self.schema.type_defs.extend(schema.type_defs);
        self.schema.att_decls.extend(schema.att_decls);
        self.schema.elem_decls.extend(schema.elem_decls);
        self.schema.attgroup_defs.extend(schema.attgroup_defs);
        self.schema.modelgroup_defs.extend(schema.modelgroup_defs);
        self.schema.notation_decls.extend(schema.notation_decls);
        Ok(())
    }

    fn parse_import(&mut self, mut elem: Element) -> Result<(), XSDError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert_eq!(elem.local_name().as_ref(), SCHEMA_IMPORT_NAME);

        let _id = elem.remove_attribute("id", None);
        let namespace = elem.remove_attribute("namespace", None).map(|ns| {
            match SCHEMA_BUILTIN_ANYURI.parse_with_elem(&ns, elem.clone()) {
                Ok(SchemaValue::AnyURI(uri)) => Ok(uri.to_string()),
                Ok(_) => internal_error!(self.handler, elem),
                Err(err) => {
                    error!(
                        self.handler,
                        SrcImport,
                        elem,
                        "The attribute 'namespace' of '{}' is failed to parse as anyURI because {}.",
                        elem.local_name(),
                        err
                    );
                    Err(XSDError::SrcImport)
                }
            }
        }).transpose()?;
        let schema_location =
            self.extract_any_uri(elem.clone(), "schemaLocation", XSDError::SrcImport)?;

        self.check_attribute(elem.clone())?;

        let Some(schema_location) = schema_location else {
            // Unlike 'include', schemaLocation is an optional attribute, so this
            // is not an error. However, since the processor cannot do anything
            // further, it simply returns `Ok` and terminates processing.
            return Ok(());
        };
        // Manage imported URIs within the same set as included URIs.
        // Considering cases where an imported schema includes additional schemas, it
        // is better not to separate duplicate management between include and import.
        if self.included_resources.contains(&schema_location) {
            // According to the note in Section 4.2.3 of XSD 1.0 Part 1,
            // including the same resource multiple times does not result in an error.
            //
            // According to clause 2 of [sch-props-correct], components with the same
            // target namespace and name pair are not allowed. To distinguish between
            // components that are duplicates due to multiple inclusions and those that
            // are not, it is necessary to prevent the same resource from being included
            // multiple times.
            return Ok(());
        }

        let mut reader = XMLReader::builder()
            .set_handler(create_handler(&mut *self.handler))
            .build();

        let included_doc = match reader.parse_uri(&schema_location, None) {
            Ok(()) if !reader.handler.fatal_error => {
                // Register it as an imported resource.
                self.included_resources.insert(schema_location.clone());
                reader.handler.document
            }
            Err(XMLError::IOError(_)) => {
                // It is not an error if the resource referenced by `schemaLocation` cannot be resolved.
                // In this context, an I/O error is treated as a failure to resolve the resource.
                return Ok(());
            }
            _ => {
                // [src-import] 2.1 and 2.2
                error!(
                    self.handler,
                    SrcImport,
                    elem,
                    "The imported resource '{}' could not be parsed as XML.",
                    schema_location
                );
                return Err(XSDError::SrcImport);
            }
        };

        let Some(schema) = included_doc.document_element() else {
            internal_error!(self.handler, elem);
        };

        if schema.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE)
            || schema.local_name().as_ref() != SCHEMA_SCHEMA_NAME
        {
            // [src-import] 2.1 and 2.2
            error!(
                self.handler,
                SrcImport,
                schema,
                "The document element of the imported resource '{}' must be '{{{}}}schema', but is '{}{}'.",
                schema_location,
                XML_SCHEMA_NAMESPACE,
                schema
                    .namespace_name()
                    .map(|ns| format!("{{{ns}}}"))
                    .unwrap_or_default(),
                schema.local_name()
            );
            return Err(XSDError::SrcImport);
        }

        if self.target_namespace.as_deref() == namespace.as_deref() {
            // [src-import] clause 1
            error!(
                self.handler,
                SrcImport,
                elem,
                "The '{}' must not be the imported schema's namespace because it is as same as the parent schema's one.",
                namespace.as_deref().unwrap_or("(null)")
            );
            return Err(XSDError::SrcImport);
        }

        let target_namespace = self
            .get_any_uri(schema.clone(), "targetNamespace", XSDError::SrcImport)?
            .map(|ns| ns.to_string());
        if target_namespace != namespace {
            // [src-import] clause 3
            error!(
                self.handler,
                SrcImport,
                schema,
                "The imported schema's target namespace is '{}', but the attribute value of 'namespace' is '{}'.",
                target_namespace.as_deref().unwrap_or("(null)"),
                namespace.as_deref().unwrap_or("(null)")
            );
            return Err(XSDError::SrcImport);
        }

        // It would be easier if child elements could be retrieved and merged into the parse
        // context, but given the differences in values for `blockDefault`, `finalDefault`,
        // `xxxFormDefault`, and others, the only option is to parse them as separate schemas
        // and then merge them.
        let mut context = XSDParseContext::new(&mut self.handler);
        context.unnamed = self.unnamed;
        context.parse(included_doc)?;
        self.unnamed = context.unnamed;
        let schema = context.schema;
        // Merge schemas
        self.schema.type_defs.extend(schema.type_defs);
        self.schema.att_decls.extend(schema.att_decls);
        self.schema.elem_decls.extend(schema.elem_decls);
        self.schema.attgroup_defs.extend(schema.attgroup_defs);
        self.schema.modelgroup_defs.extend(schema.modelgroup_defs);
        self.schema.notation_decls.extend(schema.notation_decls);
        Ok(())
    }

    fn parse_redefine(&mut self, mut elem: Element) -> Result<(), XSDError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert_eq!(elem.local_name().as_ref(), SCHEMA_REDEFINE_NAME);

        let _id = elem.remove_attribute("id", None);
        let Some(schema_location) =
            self.extract_any_uri(elem.clone(), "schemaLocation", XSDError::SrcRedefine)?
        else {
            error!(
                self.handler,
                SrcRedefine,
                elem,
                "The mandatory attribute 'schemaLocation' of '{}' is missing.",
                elem.local_name()
            );
            return Err(XSDError::SrcRedefine);
        };

        self.check_attribute(elem.clone())?;

        let mut children = elem.first_child();
        let mut simple_types = vec![];
        let mut complex_types = vec![];
        let mut groups = vec![];
        let mut att_groups = vec![];
        while let Some(child) = children {
            children = child.next_sibling();

            match child.downcast() {
                NodeKind::Element(elem) => {
                    if elem.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE) {
                        continue;
                    }

                    match elem.local_name().as_ref() {
                        SCHEMA_ANNOTATION_NAME => {}
                        SCHEMA_SIMPLETYPE_NAME => simple_types.push(elem),
                        SCHEMA_COMPLEXTYPE_NAME => complex_types.push(elem),
                        SCHEMA_GROUP_NAME => groups.push(elem),
                        SCHEMA_ATTGROUP_NAME => att_groups.push(elem),
                        _ => unallowed_element!(self.handler, elem),
                    }
                }
                NodeKind::Text(_) => unallowed_text!(self.handler, elem),
                _ => {}
            }
        }
        self.check_unexpected_schema_elements(children)?;

        if self.included_resources.contains(&schema_location) {
            // It is unclear how to handle 'redefine' that have different sets
            // of children for the same resource, but for now, simply skip them.
            // Although this is not explicitly stated in the specification, one
            // possible strategy might be to group 'include' and 'redefine' that
            // target the same resource and treat them as a single 'redefine'.
            return Ok(());
        }

        let empty = simple_types.is_empty()
            && complex_types.is_empty()
            && groups.is_empty()
            && att_groups.is_empty();

        let mut reader = XMLReader::builder()
            .set_handler(create_handler(&mut *self.handler))
            .build();

        let included_doc = match reader.parse_uri(&schema_location, None) {
            Ok(()) if !reader.handler.fatal_error => {
                // Register it as an included resource.
                self.included_resources.insert(schema_location.clone());
                reader.handler.document
            }
            Err(XMLError::IOError(_)) if empty => {
                // Resource errors are treated the same as an 'include' only
                // when the resource has no children other than 'annotation's.
                return Ok(());
            }
            _ => {
                // [src-redefine] clause 1 and 2
                error!(
                    self.handler,
                    SrcRedefine,
                    elem,
                    "The redefined resource '{}' could not be parsed as XML.",
                    schema_location
                );
                return Err(XSDError::SrcRedefine);
            }
        };

        let Some(mut schema) = included_doc.document_element() else {
            internal_error!(self.handler, elem);
        };

        if schema.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE)
            || schema.local_name().as_ref() != SCHEMA_SCHEMA_NAME
        {
            // [src-redefine] clause 2
            error!(
                self.handler,
                SrcRedefine,
                schema,
                "The document element of the redefined resource '{}' must be '{{{}}}schema', but is '{}{}'.",
                schema_location,
                XML_SCHEMA_NAMESPACE,
                schema
                    .namespace_name()
                    .map(|ns| format!("{{{ns}}}"))
                    .unwrap_or_default(),
                schema.local_name()
            );
            return Err(XSDError::SrcRedefine);
        }

        if let Some(target_namespace) = schema.get_attribute("targetNamespace", None) {
            if self
                .target_namespace
                .as_deref()
                .is_none_or(|ns| ns != target_namespace)
            {
                // [src-redefine] clause 3
                error!(
                    self.handler,
                    SrcRedefine,
                    schema,
                    "The redefine schema's target namespace is '{}', but parent schema's one is '{}'.",
                    target_namespace,
                    self.target_namespace.as_deref().unwrap_or("(null)")
                );
                return Err(XSDError::SrcRedefine);
            }
        } else if let Some(target_namespace) = self.target_namespace.as_deref() {
            schema
                .set_attribute("targetNamespace", None, Some(target_namespace))
                .ok();
        }

        // It would be easier if child elements could be retrieved and merged into the parse
        // context, but given the differences in values for `blockDefault`, `finalDefault`,
        // `xxxFormDefault`, and others, the only option is to parse them as separate schemas
        // and then merge them.
        let mut context = XSDParseContext::new(&mut self.handler);
        context.unnamed = self.unnamed;
        context.parse(included_doc)?;
        self.unnamed = context.unnamed;
        let schema = context.schema;
        // Merge schemas
        self.schema.type_defs.extend(schema.type_defs);
        self.schema.att_decls.extend(schema.att_decls);
        self.schema.elem_decls.extend(schema.elem_decls);
        self.schema.attgroup_defs.extend(schema.attgroup_defs);
        self.schema.modelgroup_defs.extend(schema.modelgroup_defs);
        self.schema.notation_decls.extend(schema.notation_decls);
        Ok(())
    }

    fn extract_qname(
        &mut self,
        mut elem: Element,
        name: &str,
    ) -> Result<Option<(Option<Arc<str>>, Arc<str>)>, XSDError> {
        let Some(name) = elem.remove_attribute(name, None) else {
            return Ok(None);
        };

        match SCHEMA_BUILTIN_QNAME.parse_with_elem(&name, elem.clone()) {
            Ok(SchemaValue::QName(nsname, local)) => Ok(Some((nsname, local))),
            Ok(_) => internal_error!(self.handler, elem),
            Err(err) => {
                error!(
                    self.handler,
                    SrcQname,
                    elem,
                    "The attribute '{}' of '{}' is failed to parse as QName because {}.",
                    name,
                    elem.local_name(),
                    err
                );
                Err(XSDError::SrcQname)
            }
        }
    }

    fn extract_ncname(
        &mut self,
        mut elem: Element,
        name: &str,
        error: XSDError,
    ) -> Result<Option<Arc<str>>, XSDError> {
        let Some(ncname) = elem.remove_attribute(name, None) else {
            return Ok(None);
        };
        self.parse_ncname(elem, name, &ncname, error)
    }

    fn parse_ncname(
        &mut self,
        elem: Element,
        name: &str,
        value: &str,
        error: XSDError,
    ) -> Result<Option<Arc<str>>, XSDError> {
        match SCHEMA_BUILTIN_NCNAME.parse_with_elem(value, elem.clone()) {
            Ok(SchemaValue::String(ret)) => Ok(Some(ret)),
            Ok(_) => internal_error!(self.handler, elem),
            Err(err) => {
                error!(
                    self.handler,
                    error,
                    elem,
                    "The attribute '{}' of '{}' is failed to parse as NCName because {}.",
                    name,
                    elem.local_name(),
                    err
                );
                Err(error)
            }
        }
    }

    fn extract_boolean(
        &mut self,
        mut elem: Element,
        name: &str,
        error: XSDError,
    ) -> Result<Option<bool>, XSDError> {
        let Some(boolean) = elem.remove_attribute(name, None) else {
            return Ok(None);
        };

        match SCHEMA_BUILTIN_BOOLEAN.parse_with_elem(&boolean, elem.clone()) {
            Ok(SchemaValue::Boolean(ret)) => Ok(Some(ret)),
            Ok(_) => internal_error!(self.handler, elem),
            Err(err) => {
                error!(
                    self.handler,
                    error,
                    elem,
                    "The attribute '{}' of '{}' is failed to parse as boolean because {}.",
                    name,
                    elem.local_name(),
                    err
                );
                Err(error)
            }
        }
    }

    fn extract_any_uri(
        &mut self,
        mut elem: Element,
        name: &str,
        error: XSDError,
    ) -> Result<Option<Arc<URIStr>>, XSDError> {
        let Some(uri) = elem.remove_attribute(name, None) else {
            return Ok(None);
        };
        self.parse_any_uri(elem, name, &uri, error)
    }

    fn get_any_uri(
        &mut self,
        elem: Element,
        name: &str,
        error: XSDError,
    ) -> Result<Option<Arc<URIStr>>, XSDError> {
        let Some(uri) = elem.get_attribute(name, None) else {
            return Ok(None);
        };
        self.parse_any_uri(elem, name, &uri, error)
    }

    fn parse_any_uri(
        &mut self,
        elem: Element,
        name: &str,
        value: &str,
        error: XSDError,
    ) -> Result<Option<Arc<URIStr>>, XSDError> {
        match SCHEMA_BUILTIN_ANYURI.parse_with_elem(value, elem.clone()) {
            Ok(SchemaValue::AnyURI(uri)) => Ok(Some(uri)),
            Ok(_) => internal_error!(self.handler, elem),
            Err(err) => {
                error!(
                    self.handler,
                    error,
                    elem,
                    "The attribute '{}' of '{}' is failed to parse as anyURI because {}.",
                    name,
                    elem.local_name(),
                    err
                );
                Err(error)
            }
        }
    }

    fn extract_occurs(
        &mut self,
        mut elem: Element,
        name: &str,
        default: Option<usize>,
        allow_unbounded: bool,
    ) -> Result<Option<usize>, XSDError> {
        let Some(occur) = elem.remove_attribute(name, None) else {
            return Ok(default);
        };
        let occur = occur.trim_matches(|c| XMLVersion::XML10.is_whitespace(c));

        if allow_unbounded && occur == "unbounded" {
            return Ok(None);
        }

        match SCHEMA_BUILTIN_NONNEGATIVE_INTEGER.parse_with_elem(occur, elem.clone()) {
            Ok(SchemaValue::Integer(occurs)) => {
                if occurs > usize::MAX as i128 {
                    error!(
                        self.handler,
                        TooLargeOccurence,
                        elem,
                        "The value of the attribute '{}' is too large.",
                        name
                    );
                    return Err(XSDError::TooLargeOccurence);
                }
                Ok(Some(occurs as usize))
            }
            Ok(_) => internal_error!(self.handler, elem),
            Err(err) => {
                error!(
                    self.handler,
                    SrcModelGroup,
                    elem,
                    "The attribute '{}' of '{}' is failed to parse as nonNegativeInteger because {}.",
                    name,
                    SCHEMA_CHOICE_NAME,
                    err
                );
                Err(XSDError::SrcModelGroup)
            }
        }
    }

    fn extract_namespace_list(
        &mut self,
        mut elem: Element,
        target_namespace: Option<Arc<str>>,
        name: &str,
    ) -> Result<NamespaceConstraint, XSDError> {
        match elem.remove_attribute(name, None) {
            Some(namespace) => {
                let namespace = namespace.trim_matches(|c| XMLVersion::XML10.is_whitespace(c));
                if namespace == "##any" {
                    Ok(NamespaceConstraint::Any)
                } else if namespace == "##other" {
                    Ok(NamespaceConstraint::Not(target_namespace))
                } else {
                    let mut list = vec![];
                    for uri in namespace
                        .split(|c| XMLVersion::XML10.is_whitespace(c))
                        .filter(|s| !s.is_empty())
                    {
                        if uri == "##targetNamespace" {
                            list.push(target_namespace.clone());
                        } else if uri == "##local" {
                            list.push(None);
                        } else {
                            match SCHEMA_BUILTIN_ANYURI.parse_with_elem(uri, elem.clone()) {
                                Ok(SchemaValue::AnyURI(uri)) => {
                                    list.push(Some(uri.to_string().into()))
                                }
                                Ok(_) => internal_error!(self.handler, elem),
                                Err(err) => {
                                    error!(
                                        self.handler,
                                        SrcWildcard,
                                        elem,
                                        "The attribute '{}' of '{}' is failed to parse as anyURI because {}.",
                                        name,
                                        elem.local_name(),
                                        err
                                    );
                                    return Err(XSDError::SrcWildcard);
                                }
                            }
                        }
                    }
                    // It is unclear whether duplicates should be treated as errors,
                    // but for the purposes of this process, they will simply be removed
                    // rather than treated as errors.
                    list.sort_unstable();
                    list.dedup();
                    Ok(NamespaceConstraint::List(list))
                }
            }
            None => Ok(NamespaceConstraint::Any),
        }
    }

    fn extract_process_contents(
        &mut self,
        mut elem: Element,
        name: &str,
    ) -> Result<ProcessContent, XSDError> {
        match elem.remove_attribute(name, None) {
            Some(s) => {
                let s = s.trim_matches(|c| XMLVersion::XML10.is_whitespace(c));
                match s {
                    "lax" => Ok(ProcessContent::Lax),
                    "skip" => Ok(ProcessContent::Skip),
                    "strict" => Ok(ProcessContent::Strict),
                    _ => {
                        error!(
                            self.handler,
                            SrcWildcard,
                            elem,
                            "The attribute '{}' of '{}' must be 'lax', 'skip' or 'strict', but is '{}'.",
                            name,
                            elem.local_name(),
                            s
                        );
                        Err(XSDError::SrcWildcard)
                    }
                }
            }
            None => Ok(ProcessContent::Strict),
        }
    }

    fn extract_final(
        &mut self,
        mut elem: Element,
        name: &str,
        allow: FinalSet,
        allow_all: bool,
    ) -> Result<Option<FinalSet>, XSDError> {
        let Some(fin) = elem.remove_attribute(name, None) else {
            return Ok(None);
        };

        let fin = fin.trim_matches(|c| XMLVersion::XML10.is_whitespace(c));
        if allow_all && fin == "#all" {
            return Ok(Some(allow));
        }

        let mut ret = FinalSet::default();
        for fin in fin
            .split(|c| XMLVersion::XML10.is_whitespace(c))
            .filter(|s| !s.is_empty())
        {
            match fin {
                "extension" if allow.extension() => ret = ret | Final::Extension,
                "restriction" if allow.restriction() => ret = ret | Final::Restriction,
                "list" if allow.list() => ret = ret | Final::List,
                "union" if allow.union() => ret = ret | Final::Union,
                _ => {
                    todo!("raise error because of unallowed value");
                }
            }
        }
        Ok(Some(ret))
    }

    fn extract_qualification_form(
        &mut self,
        mut elem: Element,
        name: &str,
        error: XSDError,
    ) -> Result<Option<QualificationForm>, XSDError> {
        let Some(form) = elem.remove_attribute(name, None) else {
            return Ok(None);
        };

        let form = form.trim_matches(|c| XMLVersion::XML10.is_whitespace(c));
        match form {
            "qualified" => Ok(Some(QualificationForm::Qualified)),
            "unqualified" => Ok(Some(QualificationForm::Unqualified)),
            _ => {
                error!(
                    self.handler,
                    error,
                    elem,
                    "The attribute '{}' of '{}' must be 'qualified' or 'unqualified', but is '{}'.",
                    name,
                    elem.local_name(),
                    form
                );
                Err(error)
            }
        }
    }

    fn pop_attr_decls(
        &mut self,
        children: &mut Option<Node<dyn NodeSpec>>,
        target_namespace: Option<Arc<str>>,
        loop_guard: &mut Vec<(Option<Arc<str>>, Arc<str>)>,
    ) -> Result<(Vec<AttributeUse>, Option<Wildcard>), XSDError> {
        let mut attribute_uses = vec![];
        // wildcards from attributeGroup
        let mut wildcards = vec![];
        // a wildcard from anyAttribute
        let mut any_attribute = None;
        while let Some(child) = children.take() {
            *children = child.next_sibling();

            match child.downcast() {
                NodeKind::Element(elem) => {
                    if elem.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE) {
                        continue;
                    }

                    match elem.local_name().as_ref() {
                        SCHEMA_ATTRIBUTE_NAME => {
                            if any_attribute.is_some() {
                                todo!("raise error")
                            }
                            if let Some(attuse) = self.parse_attribute_declaration(
                                elem,
                                target_namespace.clone(),
                                false,
                            )? {
                                attribute_uses.push(attuse);
                            }
                        }
                        SCHEMA_ATTGROUP_NAME => {
                            if any_attribute.is_some() {
                                todo!("raise error")
                            }
                            let r#ref = self.parse_attgroup_reference(elem, loop_guard)?;
                            if let Some(def) = self.schema.attgroup_defs.get(&r#ref) {
                                attribute_uses.extend(def.attribute_uses.iter().cloned());
                                if let Some(wildcard) = def.attribute_wildcard.as_ref() {
                                    wildcards.push(wildcard.clone());
                                }
                            } else {
                                todo!("raise error because of definition registration failure")
                            }
                        }
                        SCHEMA_ANYATT_NAME => {
                            if any_attribute.is_some() {
                                todo!("raise error")
                            }
                            any_attribute =
                                Some(self.parse_any_attribute(elem, target_namespace.clone())?);
                        }
                        _ => {
                            *children = Some(child);
                            break;
                        }
                    }
                }
                NodeKind::Text(_) => unallowed_text!(
                    self.handler,
                    child.parent_node().unwrap().as_element().unwrap()
                ),
                _ => {}
            }
        }
        let attribute_wildcard = attribute_complete_wildcard(&wildcards, any_attribute)?;
        Ok((attribute_uses, attribute_wildcard))
    }

    fn pop_type_def_particle(
        &mut self,
        children: &mut Option<Node<dyn NodeSpec>>,
        target_namespace: Option<Arc<str>>,
    ) -> Result<Option<Particle>, XSDError> {
        while let Some(child) = children.take() {
            *children = child.next_sibling();

            match child.downcast() {
                NodeKind::Element(elem) => {
                    if elem.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE) {
                        continue;
                    }

                    let particle = match elem.local_name().as_ref() {
                        SCHEMA_GROUP_NAME => {
                            self.parse_group_ref(elem, target_namespace.clone())?
                        }
                        SCHEMA_ALL_NAME => self.parse_all(elem, target_namespace.clone())?,
                        SCHEMA_CHOICE_NAME => self.parse_choice(elem, target_namespace.clone())?,
                        SCHEMA_SEQUENCE_NAME => {
                            self.parse_sequence(elem, target_namespace.clone())?
                        }
                        _ => {
                            *children = Some(child);
                            break;
                        }
                    };
                    return Ok(Some(particle));
                }
                NodeKind::Text(_) => unallowed_text!(
                    self.handler,
                    child.parent_node().unwrap().as_element().unwrap()
                ),
                _ => {}
            }
        }
        Ok(None)
    }

    fn pop_nested_particle(
        &mut self,
        children: &mut Option<Node<dyn NodeSpec>>,
        target_namespace: Option<Arc<str>>,
    ) -> Result<Option<Particle>, XSDError> {
        while let Some(child) = children.take() {
            *children = child.next_sibling();

            match child.downcast() {
                NodeKind::Element(elem) => {
                    if elem.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE) {
                        continue;
                    }

                    return match elem.local_name().as_ref() {
                        SCHEMA_ELEMENT_NAME => Ok(Some(self.parse_local_element(
                            elem,
                            target_namespace,
                            None,
                        )?)),
                        SCHEMA_GROUP_NAME => {
                            Ok(Some(self.parse_group_ref(elem, target_namespace)?))
                        }
                        SCHEMA_CHOICE_NAME => Ok(Some(self.parse_choice(elem, target_namespace)?)),
                        SCHEMA_SEQUENCE_NAME => {
                            Ok(Some(self.parse_sequence(elem, target_namespace)?))
                        }
                        SCHEMA_ANY_NAME => Ok(Some(self.parse_any(elem, target_namespace)?)),
                        _ => unallowed_element!(self.handler, elem),
                    };
                }
                NodeKind::Text(_) => unallowed_text!(
                    self.handler,
                    child.parent_node().unwrap().as_element().unwrap()
                ),
                _ => {}
            }
        }
        Ok(None)
    }

    /// Raise an errors for attributes whose namespace is NULL or is the XSD namespace.
    ///
    /// Non-foreign attributes for which an error should not be raised must be removed
    /// before calling this method.
    fn check_attribute(&mut self, elem: Element) -> Result<(), XSDError> {
        let mut ret = Ok(());
        for att in elem.attributes().filter(|a| {
            a.namespace_name()
                .is_none_or(|ns| &*ns == XML_SCHEMA_NAMESPACE)
        }) {
            error!(
                self.handler,
                UnallowedAttribute,
                elem.clone(),
                "The schema attribute '{}' is not allowed for '{}'.",
                att.local_name(),
                elem.local_name()
            );
            ret = Err(XSDError::UnallowedAttribute);
        }
        ret
    }

    fn check_unexpected_schema_elements(
        &mut self,
        mut children: Option<Node<dyn NodeSpec>>,
    ) -> Result<(), XSDError> {
        while let Some(child) = children {
            children = child.next_sibling();

            match child.downcast() {
                NodeKind::Element(elem) => {
                    if elem.namespace_name().as_deref() == Some(XML_SCHEMA_NAMESPACE) {
                        unallowed_element!(self.handler, elem);
                    }
                }
                NodeKind::Text(_) => unallowed_text!(
                    self.handler,
                    child.parent_node().unwrap().as_element().unwrap()
                ),
                _ => {}
            }
        }
        Ok(())
    }

    /// Compute ["effective content"](https://www.w3.org/TR/xmlschema-1/#key-exg)
    /// base on `particle`.
    ///
    /// If the "effective content" is empty, return [`None`].
    fn effective_content(
        &mut self,
        particle: Option<Particle>,
        elem: Element,
        mixed: bool,
    ) -> Result<Option<Particle>, XSDError> {
        match particle {
            Some(particle) => {
                let Term::ModelGroup(model) = &particle.term else {
                    // There should not be a element or any child,
                    // so {term} should be a model group.
                    internal_error!(self.handler, elem);
                };
                if (matches!(model.compositor, Compositor::All | Compositor::Sequence)
                    && model.particles.is_empty())
                    || (matches!(model.compositor, Compositor::Choice)
                        && model.particles.is_empty()
                        && particle.min_occurs == 0)
                {
                    // clause 2.1.2 and 2.1.3
                    if mixed {
                        // clause 2.1.4
                        Ok(Some(Particle {
                            min_occurs: 1,
                            max_occurs: Some(1),
                            term: Term::ModelGroup(ModelGroup {
                                compositor: Compositor::Sequence,
                                particles: vec![].into(),
                            }),
                        }))
                    } else {
                        // clause 2.1.5
                        Ok(None)
                    }
                } else {
                    // clause 2.2
                    Ok(Some(particle))
                }
            }
            None => {
                // clause 2.1.1
                if mixed {
                    // clause 2.1.4
                    Ok(Some(Particle {
                        min_occurs: 1,
                        max_occurs: Some(1),
                        term: Term::ModelGroup(ModelGroup {
                            compositor: Compositor::Sequence,
                            particles: vec![].into(),
                        }),
                    }))
                } else {
                    // clause 2.1.5
                    Ok(None)
                }
            }
        }
    }
}

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

enum Scope {
    /// top-level declaration or definition
    Global,
    /// local declaration or definition
    Local(Option<Arc<str>>, Arc<str>),
}

struct AttributeDeclaration {
    name: Arc<str>,
    target_namespace: Option<Arc<str>>,
    type_definition: Arc<SimpleTypeDefinition>,
    scope: Option<Scope>,
    value_constraint: Option<ValueConstraint>,
    annotation: Option<Annotation>,
}

struct ElementDeclaration {
    name: Arc<str>,
    target_namespace: Option<Arc<str>>,
    type_definition: TypeDefinition,
    scope: Option<Scope>,
    value_constraint: Option<ValueConstraint>,
    nillable: bool,
    idc_definitions: Vec<IDCDefinition>,
    substitution_group_affiliation: Option<(Option<Arc<str>>, Arc<str>)>,
    substitution_group_exclusions: FinalSet,
    disallowed_substitutions: FinalSet,
    r#abstract: bool,
    annotation: Option<Annotation>,
}

#[derive(Clone)]
enum TypeDefinition {
    Simple(Arc<SimpleTypeDefinition>),
    Complex(Arc<ComplexTypeDefinition>),
}

#[derive(Debug, Clone, Copy)]
enum DerivationMethod {
    Restriction,
    Extension,
}

#[derive(Clone)]
enum ContentType {
    Empty,
    Simple(Arc<SimpleTypeDefinition>),
    Mixed(Particle),
    ElementOnly(Particle),
}

struct ComplexTypeDefinition {
    name: Arc<str>,
    target_namespace: Option<Arc<str>>,
    // The specification stipulates that every complex type definition must have a base type
    // definition, and that `anyType` itself is defined as a base type definition.
    // However, in this implementation, it is defined as `Option` to avoid self-reference.
    base_type_definition: Option<TypeDefinition>,
    derivation_method: DerivationMethod,
    r#final: FinalSet,
    r#abstract: bool,
    attribute_uses: Vec<AttributeUse>,
    attribute_wildcard: Option<Wildcard>,
    content_type: ContentType,
    prohibited_substitutions: FinalSet,
    // Since `Annotation` holds an `Element`, and `Element` is neither `Sync` nor `Send`,
    // attempting to add an annotation would prevent `ComplexTypeDefinition` from being
    // declared as static.
    // Since this does not affect validation, omit the annotation for now. Implement
    // improvements in the future (e.g., using Thread Local Storage?).
}

/// [3.4.7 Built-in Complex Type Definition](https://www.w3.org/TR/xmlschema-1/#d0e9252).
static SCHEMA_BUILTIN_ANY_TYPE: LazyLock<Arc<ComplexTypeDefinition>> = LazyLock::new(|| {
    Arc::new(ComplexTypeDefinition {
        name: "anyType".into(),
        target_namespace: Some(XML_SCHEMA_NAMESPACE.into()),
        base_type_definition: None,
        derivation_method: DerivationMethod::Restriction,
        r#final: FinalSet::default(),
        r#abstract: false,
        attribute_uses: vec![],
        attribute_wildcard: Some(Wildcard {
            ns_constraint: NamespaceConstraint::Any,
            process_contents: ProcessContent::Lax,
        }),
        content_type: ContentType::Mixed(Particle {
            min_occurs: 1,
            max_occurs: Some(1),
            term: Term::ModelGroup(ModelGroup {
                compositor: Compositor::Sequence,
                particles: vec![Particle {
                    min_occurs: 0,
                    max_occurs: None,
                    term: Term::Wildcard(Wildcard {
                        ns_constraint: NamespaceConstraint::Any,
                        process_contents: ProcessContent::Lax,
                    }),
                }]
                .into(),
            }),
        }),
        prohibited_substitutions: FinalSet::default(),
    })
});

#[derive(Debug, Clone)]
enum ValueConstraint {
    Default(SchemaValue),
    Fixed(SchemaValue),
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

struct AttributeGroupDefinition {
    name: Arc<str>,
    target_namespace: Option<Arc<str>>,
    attribute_uses: Vec<AttributeUse>,
    attribute_wildcard: Option<Wildcard>,
    annotation: Option<Annotation>,
}

struct ModelGroupDefinition {
    name: Arc<str>,
    target_namespace: Option<Arc<str>>,
    model_group: ModelGroup,
    annotation: Option<Annotation>,
}

#[derive(Debug, Clone, Copy)]
enum Compositor {
    All,
    Choice,
    Sequence,
}

#[derive(Debug, Clone)]
struct ModelGroup {
    compositor: Compositor,
    particles: Arc<[Particle]>,
    // annotation: Option<Annotation>,
}

#[derive(Debug, Clone)]
enum Term {
    ModelGroup(ModelGroup),
    Wildcard(Wildcard),
    ElementDecl(Option<Arc<str>>, Arc<str>),
}

#[derive(Debug, Clone)]
pub(super) struct Particle {
    min_occurs: usize,
    max_occurs: Option<usize>,
    term: Term,
}

impl Particle {
    /// Compute the minimum effective total range.
    ///
    /// If `term` is not a group, return [`None`].
    ///
    /// If the result is larger than [`usize::MAX`], return [`usize::MAX`].
    ///
    /// # Reference
    /// - [cos-seq-range](https://www.w3.org/TR/xmlschema-1/#cos-seq-range)
    /// - [cos-choice-range](https://www.w3.org/TR/xmlschema-1/#cos-choice-range)
    fn minimum_effective_total_range(&self) -> Option<usize> {
        let Term::ModelGroup(group) = &self.term else {
            return None;
        };
        if matches!(group.compositor, Compositor::Choice) {
            // [cos-choice-range]
            let min = group
                .particles
                .iter()
                .map(|p| match &p.term {
                    Term::ModelGroup(_) => p.minimum_effective_total_range().unwrap(),
                    _ => p.min_occurs,
                })
                .min()
                .unwrap_or(0);
            Some(self.min_occurs.saturating_mul(min))
        } else {
            // [cos-seq-range]
            let sum = group
                .particles
                .iter()
                .map(|p| match &p.term {
                    Term::ModelGroup(_) => p.minimum_effective_total_range().unwrap(),
                    _ => p.min_occurs,
                })
                .fold(0usize, |s, v| s.saturating_add(v));
            Some(self.min_occurs.saturating_mul(sum))
        }
    }

    /// Check if this particle is emptiable.
    ///
    /// # Reference
    /// - [cos-group-emptiable](https://www.w3.org/TR/xmlschema-1/#cos-group-emptiable)
    fn is_emptiable(&self) -> bool {
        // clause 1
        // Its {min occurs} is 0.
        if self.min_occurs == 0 {
            return true;
        }

        // clause 2
        // Its {term} is a group and the minimum part of the effective total range of that group,
        // as defined by Effective Total Range (all and sequence) (§3.8.6) (if the group is all
        // or sequence) or Effective Total Range (choice) (§3.8.6) (if it is choice), is 0.
        self.minimum_effective_total_range().is_some_and(|m| m == 0)
    }
}

#[derive(Debug, Clone, Copy)]
enum ProcessContent {
    Lax,
    Skip,
    Strict,
}

#[derive(Debug, Clone)]
enum NamespaceConstraint {
    Any,
    Not(Option<Arc<str>>),
    List(Vec<Option<Arc<str>>>),
}

#[derive(Debug, Clone)]
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
}

#[derive(Debug, Clone, Copy, Default)]
enum IDCCategory {
    #[default]
    Key,
    Keyref,
    Unique,
}

#[derive(Default)]
struct IDCDefinition {
    name: Arc<str>,
    target_namespace: Option<Arc<str>>,
    idc_category: IDCCategory,
    selector: String,
    fields: Vec<String>,
    referenced_key: Option<(Option<Arc<str>>, Arc<str>)>,
}

struct Notation {
    name: Arc<str>,
    target_namespace: Option<Arc<str>>,
    system_id: Option<Arc<URIStr>>,
    public_id: Option<Arc<str>>,
}

/// Annotation component.
///
/// # Reference
/// - [3.13.1 The Annotation Schema Component](https://www.w3.org/TR/xmlschema-1/#Annotation_details)
struct Annotation {
    appinfo: Vec<Element>,
    documentation: Vec<Element>,
    attributes: Vec<Attribute>,
}
