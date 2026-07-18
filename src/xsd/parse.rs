use std::{
    collections::HashMap,
    io::Read,
    num::NonZeroUsize,
    sync::{Arc, LazyLock},
};

use crate::{
    XML_XML_NAMESPACE, XMLVersion,
    error::XMLError,
    sax::{DefaultSAXHandler, SAXHandler, XMLReader},
    tree::{
        Attribute, Document, Element, Node, TreeBuildHandler, convert::NodeKind, node::NodeSpec,
    },
    uri::URIStr,
    xsd::{XML_SCHEMA_NAMESPACE, XSDSchema},
    xsdtypes::{
        Final, FinalSet, SCHEMA_BUILTIN_ANY_SIMPLE_TYPE, SCHEMA_BUILTIN_ANYURI,
        SCHEMA_BUILTIN_BOOLEAN, SCHEMA_BUILTIN_NCNAME, SCHEMA_BUILTIN_NONNEGATIVE_INTEGER,
        SCHEMA_BUILTIN_POSITIVE_INTEGER, SCHEMA_BUILTIN_QNAME, SCHEMA_BUILTIN_TOKEN, SchemaValue,
        SimpleTypeDefinition, SimpleTypeDefinitionBuilder, Whitespace,
    },
};

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
        use self::XSDParseError::*;
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
        use XSDParseError::*;
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
        use XSDParseError::*;
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
        return Err(XSDParseError::InternalError);
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
        return Err(XSDParseError::UnallowedText);
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
        return Err(XSDParseError::UnallowedElement);
    }};
}

#[derive(Default)]
pub struct Schema {
    type_defs: HashMap<(Option<String>, String), TypeDefinition>,
    att_decls: HashMap<(Option<String>, String), AttributeDeclaration>,
    elem_decls: HashMap<(Option<String>, String), ()>,
    attgroup_defs: HashMap<(Option<String>, String), AttributeGroupDefinition>,
    modelgroup_defs: HashMap<(Option<String>, String), ModelGroupDefinition>,
    notation_decls: HashMap<(Option<String>, String), Notation>,
    annotations: Vec<Annotation>,
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
        if let Some(handler) = handler {
            let mut parser = XMLReader::builder()
                .set_handler(create_handler(handler))
                .build();
            parser.parse_uri(uri, encoding)?;
            let _schema = parse_schema(parser.handler.document, &mut parser.handler.handler);
        } else {
            let mut parser = XMLReader::builder()
                .set_handler(create_handler(DefaultSAXHandler))
                .build();
            parser.parse_uri(uri, encoding)?;
            let _schema = parse_schema(parser.handler.document, &mut parser.handler.handler);
        };
        todo!()
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
        let document = if let Some(handler) = handler {
            let mut parser = XMLReader::builder()
                .set_handler(create_handler(handler))
                .build();
            parser.parse_reader(reader, encoding, uri)?;
            parser.handler.document
        } else {
            let mut parser = XMLReader::builder()
                .set_handler(create_handler(DefaultSAXHandler))
                .build();
            parser.parse_reader(reader, encoding, uri)?;
            parser.handler.document
        };
        todo!()
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
        let document = if let Some(handler) = handler {
            let mut parser = XMLReader::builder()
                .set_handler(create_handler(handler))
                .build();
            parser.parse_str(schema, uri)?;
            parser.handler.document
        } else {
            let mut parser = XMLReader::builder()
                .set_handler(create_handler(DefaultSAXHandler))
                .build();
            parser.parse_str(schema, uri)?;
            parser.handler.document
        };
        todo!()
    }
}

fn create_handler<H: SAXHandler>(handler: H) -> TreeBuildHandler<H> {
    let mut h = TreeBuildHandler::with_handler(handler);
    h.expand_entity_reference = true;
    h.coalescing = true;
    h.ignoring_comments = true;
    h
}

fn parse_schema<H: SAXHandler>(
    document: Document,
    handler: &mut H,
) -> Result<Schema, XSDParseError> {
    let mut context = XSDParseContext {
        schema: Schema::default(),
        target_namespace: None,
        unnamed: 0,
        simple_types: HashMap::new(),
        complex_types: HashMap::new(),
        att_groups: HashMap::new(),
        model_groups: HashMap::new(),
        elem_decls: HashMap::new(),
        attr_form_default: QualificationForm::Unqualified,
        elem_form_default: QualificationForm::Unqualified,
        handler,
    };
    context.parse_schema(document)?;
    Ok(context.schema)
}

#[derive(Debug, Clone, Copy)]
pub enum XSDParseError {
    InternalError,
    UnallowedElement,
    UnallowedAttribute,
    UnallowedText,
    TooLargeOccurence,

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

impl std::fmt::Display for XSDParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl std::error::Error for XSDParseError {}

struct XSDParseContext<'a, H: SAXHandler> {
    // result schema
    schema: Schema,

    // context
    target_namespace: Option<String>,
    unnamed: usize,
    simple_types: HashMap<(Option<String>, String), Element>,
    complex_types: HashMap<(Option<String>, String), Element>,
    att_groups: HashMap<(Option<String>, String), Element>,
    model_groups: HashMap<(Option<String>, String), Element>,
    elem_decls: HashMap<(Option<String>, String), Element>,

    // qualification form
    attr_form_default: QualificationForm,
    elem_form_default: QualificationForm,

    // handler
    handler: &'a mut H,
}

impl<H: SAXHandler> XSDParseContext<'_, H> {
    fn parse_schema(&mut self, document: Document) -> Result<(), XSDParseError> {
        let Some(mut schema) = document.document_element() else {
            internal_error!(self.handler, document);
        };
        self.attr_form_default = match schema.remove_attribute("attributeFormDefault", None) {
            Some(form) => {
                let form = form.trim_matches(|c| XMLVersion::XML10.is_whitespace(c));
                match form {
                    "qualified" => QualificationForm::Qualified,
                    "unqualified" => QualificationForm::Unqualified,
                    _ => {
                        error!(
                            self.handler,
                            SchPropsCorrect,
                            schema,
                            "The attribute 'elementFormDefault' of 'schema' must be 'qualified' or 'unqualified'."
                        );
                        return Err(XSDParseError::SchPropsCorrect);
                    }
                }
            }
            None => QualificationForm::Unqualified,
        };
        let block_default = schema.remove_attribute("blockDefault", None);
        self.elem_form_default = match schema.remove_attribute("elementFormDefault", None) {
            Some(form) => {
                let form = form.trim_matches(|c| XMLVersion::XML10.is_whitespace(c));
                match form {
                    "qualified" => QualificationForm::Qualified,
                    "unqualified" => QualificationForm::Unqualified,
                    _ => {
                        error!(
                            self.handler,
                            SchPropsCorrect,
                            schema,
                            "The attribute 'elementFormDefault' of 'schema' must be 'qualified' or 'unqualified'."
                        );
                        return Err(XSDParseError::SchPropsCorrect);
                    }
                }
            }
            None => QualificationForm::Unqualified,
        };
        let final_default = schema.remove_attribute("finalDefault", None);
        let id = schema.remove_attribute("id", None);
        let target_namespace = schema.remove_attribute("targetNamespace", None);
        let version = schema.remove_attribute("version", None);
        let _lang = schema.remove_attribute("lang", Some(XML_XML_NAMESPACE));

        if let Some(target_namespace) = target_namespace.as_deref() {
            match SCHEMA_BUILTIN_ANYURI.parse_with_elem(target_namespace, schema.clone()) {
                Ok(SchemaValue::AnyURI(uri)) => self.target_namespace = Some(uri.to_string()),
                Ok(_) => internal_error!(self.handler, schema),
                Err(err) => {
                    error!(
                        self.handler,
                        SchPropsCorrect,
                        schema,
                        "The attribute 'targetNamespace' of 'schema' is failed to parse as anyURI because {}.",
                        err
                    );
                    return Err(XSDParseError::SchPropsCorrect);
                }
            }
        } else {
            self.target_namespace = None;
        }

        self.check_attribute(schema.clone())?;

        // include/import/redefine phase
        let mut children = schema.first_child();
        while let Some(child) = children {
            children = child.next_sibling();

            match child.downcast() {
                NodeKind::Element(mut elem) => {
                    if elem.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE) {
                        // skip foreign element
                        continue;
                    }
                    match elem.local_name().as_ref() {
                        SCHEMA_INCLUDE_NAME => todo!(),
                        SCHEMA_IMPORT_NAME => todo!(),
                        SCHEMA_REDEFINE_NAME => todo!(),
                        SCHEMA_ANNOTATION_NAME => {
                            let annot = self.parse_annotation(elem)?;
                            self.schema.annotations.push(annot);
                        }
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

        let mut model_groups = vec![];
        let mut elem_decls = vec![];
        let mut att_decls = vec![];
        // global definition/declaration phase
        while let Some(child) = children {
            children = child.next_sibling();

            match child.downcast() {
                NodeKind::Element(mut elem) => {
                    if elem.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE) {
                        // skip foreign element
                        continue;
                    }
                    match elem.local_name().as_ref() {
                        SCHEMA_ANNOTATION_NAME => {
                            let annot = self.parse_annotation(elem)?;
                            self.schema.annotations.push(annot);
                        }
                        SCHEMA_SIMPLETYPE_NAME => {
                            let Some(name) = elem.get_attribute("name", None) else {
                                todo!(
                                    "raise error because of missing 'name' attr for global simple type definition"
                                )
                            };
                            match SCHEMA_BUILTIN_NCNAME.parse_with_elem(&name, elem.clone()) {
                                Ok(SchemaValue::String(ncname)) => {
                                    self.simple_types.insert(
                                        (self.target_namespace.clone(), ncname.to_string()),
                                        elem,
                                    );
                                }
                                Err(err) => {
                                    error!(
                                        self.handler,
                                        SrcSimpleType,
                                        elem,
                                        "The attribute 'name' of 'simpleType' is failed to parse as NCName because {}.",
                                        err
                                    );
                                    return Err(XSDParseError::SrcSimpleType);
                                }
                                Ok(_) => internal_error!(self.handler, elem),
                            }
                        }
                        SCHEMA_COMPLEXTYPE_NAME => {
                            let Some(name) = elem.get_attribute("name", None) else {
                                todo!(
                                    "raise error because of missing 'name' attr for global simple type definition"
                                )
                            };
                            match SCHEMA_BUILTIN_NCNAME.parse_with_elem(&name, elem.clone()) {
                                Ok(SchemaValue::String(ncname)) => {
                                    self.complex_types.insert(
                                        (self.target_namespace.clone(), ncname.to_string()),
                                        elem,
                                    );
                                }
                                Err(err) => {
                                    error!(
                                        self.handler,
                                        SrcCt,
                                        elem,
                                        "The attribute 'name' of 'complexType' is failed to parse as NCName because {}.",
                                        err
                                    );
                                    return Err(XSDParseError::SrcCt);
                                }
                                Ok(_) => internal_error!(self.handler, elem),
                            }
                        }
                        SCHEMA_GROUP_NAME => model_groups.push(elem),
                        SCHEMA_ATTGROUP_NAME => {
                            let Some(name) = elem.get_attribute("name", None) else {
                                todo!("raise error")
                            };
                            match SCHEMA_BUILTIN_NCNAME.parse_with_elem(&name, elem.clone()) {
                                Ok(SchemaValue::String(ncname)) => {
                                    self.att_groups.insert(
                                        (self.target_namespace.clone(), ncname.to_string()),
                                        elem,
                                    );
                                }
                                Ok(_) => internal_error!(self.handler, elem),
                                Err(err) => {
                                    error!(
                                        self.handler,
                                        SrcAttributeGroup,
                                        elem,
                                        "The attribute 'name' of 'attributeGroup' is failed to parse as NCName because {}.",
                                        err
                                    );
                                    return Err(XSDParseError::SrcAttributeGroup);
                                }
                            }
                        }
                        SCHEMA_ELEMENT_NAME => elem_decls.push(elem),
                        SCHEMA_ATTRIBUTE_NAME => att_decls.push(elem),
                        SCHEMA_NOTATION_NAME => {
                            let nota = self.parse_notation(elem)?;
                            self.schema
                                .notation_decls
                                .insert((nota.target_namespace.clone(), nota.name.clone()), nota);
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

        // At first, resolve all simple type definitions
        // Since simple type definitions do not depend on anything other than other simple
        // type definitions, and since almost all other declarations and definitions depend
        // on them, they should be resolved first.
        while let Some((key, value)) = self.simple_types.iter().next() {
            let key = key.clone();
            let value = value.clone();

            self.simple_types.remove(&key);
            self.parse_simple_type_definitions(value, key.0, &mut vec![], true)?;
        }
        // Second, resolve the attribute declarations.
        // Since they do not depend on anything other than simple type definitions,
        // they can be resolved here.
        // Because attribute group definitions and complex type definitions depend on
        // attribute declarations, they must be resolved before these.
        for att in att_decls {
            self.parse_attribute_declaration(att, self.target_namespace.clone(), true)?;
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
        target_namespace: Option<String>,
        global: bool,
    ) -> Result<Option<AttributeUse>, XSDParseError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert_eq!(elem.local_name().as_ref(), SCHEMA_ATTRIBUTE_NAME);

        let default = elem.remove_attribute("default", None);
        let fixed = elem.remove_attribute("fixed", None);
        let form = match elem.remove_attribute("form", None) {
            Some(form) => {
                let form = form.trim_matches(|c| XMLVersion::XML10.is_whitespace(c));
                match form {
                    "qualified" => Some(QualificationForm::Qualified),
                    "unqualified" => Some(QualificationForm::Unqualified),
                    _ => todo!("raise error"),
                }
            }
            None => None,
        };
        let target_namespace = if global
            || matches!(form, Some(QualificationForm::Qualified))
            || (form.is_none() && matches!(self.attr_form_default, QualificationForm::Qualified))
        {
            target_namespace
        } else {
            None
        };
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
                        return Err(XSDParseError::SrcAttribute);
                    }
                    name.to_string()
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
                    return Err(XSDParseError::SrcAttribute);
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
                        return Err(XSDParseError::SrcAttribute);
                    }
                    self.unnamed += 1;
                    format!("#unnamed{}", self.unnamed)
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
                            return Err(XSDParseError::SrcAttribute);
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
                            return Err(XSDParseError::SrcAttribute);
                        }
                        Occurrence::Required
                    }
                    _ => todo!("raise error"),
                }
            }
            None => Occurrence::Optional,
        };

        self.check_attribute(elem.clone())?;

        let mut simple_type = None;
        let mut children = elem.first_child();
        let annotation = self.pop_annotation(&mut children)?;
        while let Some(child) = children {
            children = child.next_sibling();

            match child.downcast() {
                NodeKind::Element(elem) => {
                    if elem.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE) {
                        continue;
                    }

                    match elem.local_name().as_ref() {
                        SCHEMA_SIMPLETYPE_NAME => {
                            if simple_type.is_some() {
                                todo!("raise error")
                            }
                            simple_type = Some(self.parse_simple_type_definitions(
                                elem,
                                target_namespace.clone(),
                                &mut vec![],
                                false,
                            )?);
                        }
                        _ => unallowed_element!(self.handler, elem),
                    }
                }
                NodeKind::Text(_) => unallowed_text!(self.handler, elem),
                _ => {}
            }
        }

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
                return Err(XSDParseError::SrcAttribute);
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
                    return Err(XSDParseError::SrcAttribute);
                }
                match SCHEMA_BUILTIN_QNAME.parse_with_elem(&r#type, elem.clone()) {
                    Ok(SchemaValue::QName(nsname, local)) => match self
                        .schema
                        .type_defs
                        .get(&(nsname.map(|ns| ns.to_string()), local.to_string()))
                    {
                        Some(TypeDefinition::Simple(simple)) => simple.clone(),
                        Some(_) => todo!("raise error because of non-simple-type definition"),
                        None => todo!("raise error because of missing definition"),
                    },
                    Ok(_) => internal_error!(self.handler, elem),
                    Err(err) => {
                        error!(
                            self.handler,
                            SrcAttribute,
                            elem,
                            "The attribute 'type' of 'attribute' is failed to parse as QName because {}.",
                            err
                        );
                        return Err(XSDParseError::SrcAttribute);
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
                    return Err(XSDParseError::SrcAttribute);
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
                    (*SCHEMA_BUILTIN_ANY_SIMPLE_TYPE).clone()
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
                            match self
                                .schema
                                .type_defs
                                .get(&(nsname.map(|ns| ns.to_string()), local.to_string()))
                            {
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
                            return Err(XSDParseError::SrcAttribute);
                        }
                    }
                } else {
                    // 3.2.2 XML Representation of Attribute Declaration Schema Components
                    // {type definition}	The simple type definition corresponding to the
                    // <simpleType> element information item in the [children], if present,
                    // otherwise the simple type definition ·resolved· to by the ·actual
                    // value· of the type [attribute], if present, otherwise the ·simple
                    // ur-type definition·.
                    (*SCHEMA_BUILTIN_ANY_SIMPLE_TYPE).clone()
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
                return Err(XSDParseError::SrcAttribute);
            }
            (Some(default), None) => {
                match type_definition.parse_with_elem(&default, elem.clone()) {
                    Ok(value) => Some(ValueConstraint::Default(value)),
                    Err(err) => {
                        error!(
                            self.handler,
                            SrcAttribute,
                            elem,
                            "The attribute 'default' of '{}' is failed to parse as {} because {}.",
                            SCHEMA_ATTRIBUTE_NAME,
                            type_definition.name(),
                            err
                        );
                        return Err(XSDParseError::SrcAttribute);
                    }
                }
            }
            (None, Some(fixed)) => match type_definition.parse_with_elem(&fixed, elem.clone()) {
                Ok(value) => Some(ValueConstraint::Fixed(value)),
                Err(err) => {
                    error!(
                        self.handler,
                        SrcAttribute,
                        elem,
                        "The attribute 'fixed' of '{}' is failed to parse as {} because {}.",
                        SCHEMA_ATTRIBUTE_NAME,
                        type_definition.name(),
                        err
                    );
                    return Err(XSDParseError::SrcAttribute);
                }
            },
            (None, None) => None,
        };

        if global || r#ref.is_none() {
            let decl = AttributeDeclaration {
                name: name.clone(),
                target_namespace: target_namespace.clone(),
                type_definition,
                scope: Some(Scope::Global),
                value_constraint,
                annotation,
            };
            self.schema
                .att_decls
                .insert((target_namespace.clone(), name.clone()), decl);
            Ok(None)
        } else if r#ref.is_none() {
            let decl = AttributeDeclaration {
                name: name.clone(),
                target_namespace: target_namespace.clone(),
                type_definition,
                scope: None,
                value_constraint: None,
                annotation,
            };
            self.schema
                .att_decls
                .insert((target_namespace.clone(), name.clone()), decl);
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

    fn parse_top_level_element(&mut self, mut elem: Element) -> Result<(), XSDParseError> {
        // "ref", "form", "minOccurs" and "maxOccurs" are prohibited explicitly in schema of schema.
        let r#abstract = match elem.remove_attribute("abstract", None) {
            Some(abst) => match SCHEMA_BUILTIN_BOOLEAN.parse_with_elem(&abst, elem.clone()) {
                Ok(SchemaValue::Boolean(abst)) => abst,
                Ok(_) => internal_error!(self.handler, elem),
                Err(err) => {
                    error!(
                        self.handler,
                        SrcElement,
                        elem,
                        "The attribute 'abstract' of 'element' is failed to parse as boolean because {}.",
                        err
                    );
                    return Err(XSDParseError::SrcElement);
                }
            },
            None => false,
        };
        let block = elem.remove_attribute("block", None);
        let default = elem.remove_attribute("default", None);
        let r#final = elem.remove_attribute("final", None);
        let fixed = elem.remove_attribute("fixed", None);
        let _id = elem.remove_attribute("id", None);
        let name = match elem.remove_attribute("name", None) {
            Some(name) => match SCHEMA_BUILTIN_NCNAME.parse_with_elem(&name, elem.clone()) {
                Ok(SchemaValue::String(ncname)) => Some(ncname.to_string()),
                Ok(_) => internal_error!(self.handler, elem),
                Err(err) => {
                    error!(
                        self.handler,
                        SrcElement,
                        elem,
                        "The attribute 'name' of 'element' is failed to parse as NCName because {}.",
                        err
                    );
                    return Err(XSDParseError::SrcElement);
                }
            },
            None => None,
        };
        let nillable = match elem.remove_attribute("nillable", None) {
            Some(nillable) => match SCHEMA_BUILTIN_BOOLEAN.parse_with_elem(&nillable, elem.clone())
            {
                Ok(SchemaValue::Boolean(nillable)) => nillable,
                Ok(_) => internal_error!(self.handler, elem),
                Err(err) => {
                    error!(
                        self.handler,
                        SrcElement,
                        elem,
                        "The attribute 'nillable' of 'element' is failed to parse as boolean because {}.",
                        err
                    );
                    return Err(XSDParseError::SrcElement);
                }
            },
            None => false,
        };
        let substitution_group = match elem.remove_attribute("substitutionGroup", None) {
            Some(subst) => match SCHEMA_BUILTIN_QNAME.parse_with_elem(&subst, elem.clone()) {
                Ok(SchemaValue::QName(nsname, local)) => {
                    Some((nsname.map(|ns| ns.to_string()), local.to_string()))
                }
                Ok(_) => internal_error!(self.handler, elem),
                Err(err) => {
                    error!(
                        self.handler,
                        SrcElement,
                        elem,
                        "The attribute 'substitutionGroup' of 'element' is failed to parse as QName because {}.",
                        err
                    );
                    return Err(XSDParseError::SrcElement);
                }
            },
            None => None,
        };
        let r#type = match elem.remove_attribute("type", None) {
            Some(r#type) => match SCHEMA_BUILTIN_QNAME.parse_with_elem(&r#type, elem.clone()) {
                Ok(SchemaValue::QName(nsname, local)) => {
                    Some((nsname.map(|ns| ns.to_string()), local.to_string()))
                }
                Ok(_) => internal_error!(self.handler, elem),
                Err(err) => {
                    error!(
                        self.handler,
                        SrcElement,
                        elem,
                        "The attribute 'type' of 'element' is failed to parse as QName because {}.",
                        err
                    );
                    return Err(XSDParseError::SrcElement);
                }
            },
            None => None,
        };

        todo!()
    }

    fn parse_local_element(
        &mut self,
        mut elem: Element,
        target_namespace: Option<String>,
        scope: Option<(Option<String>, String)>,
    ) -> Result<Particle, XSDParseError> {
        // "substitutionGroup", "final" and "abstract" are prohibited explicitly in schema of schema.
        let block = elem.remove_attribute("block", None);
        let default = elem.remove_attribute("default", None);
        let fixed = elem.remove_attribute("fixed", None);
        let form = elem.remove_attribute("form", None);
        let _id = elem.remove_attribute("id", None);
        let max_occurs = match elem.remove_attribute("maxOccurs", None) {
            Some(max_occurs) => {
                let trimmed = max_occurs.trim_matches(|c| XMLVersion::XML10.is_whitespace(c));
                if trimmed == "unbounded" {
                    None
                } else {
                    match SCHEMA_BUILTIN_NONNEGATIVE_INTEGER.parse_with_elem(trimmed, elem.clone())
                    {
                        Ok(SchemaValue::Integer(max_occurs)) => {
                            if max_occurs != 1 {
                                todo!("raise error because of attribute constraint violation")
                            }
                            Some(max_occurs as usize)
                        }
                        Ok(_) => internal_error!(self.handler, elem),
                        Err(err) => {
                            error!(
                                self.handler,
                                SrcElement,
                                elem,
                                "The attribute 'maxOccurs' of 'element' is failed to parse as nonNegativeInteger because {}.",
                                err
                            );
                            return Err(XSDParseError::SrcElement);
                        }
                    }
                }
            }
            None => Some(1),
        };
        let min_occurs = match elem.remove_attribute("minOccurs", None) {
            Some(min_occurs) => match SCHEMA_BUILTIN_NONNEGATIVE_INTEGER
                .parse_with_elem(&min_occurs, elem.clone())
            {
                Ok(SchemaValue::Integer(min_occurs)) => {
                    if min_occurs > usize::MAX as i128 {
                        todo!("raise error because of too large minOccurs")
                    }
                    min_occurs as usize
                }
                Ok(_) => internal_error!(self.handler, elem),
                Err(err) => {
                    error!(
                        self.handler,
                        SrcElement,
                        elem,
                        "The attribute 'minOccurs' of 'element' is failed to parse as nonNegativeInteger because {}.",
                        err
                    );
                    return Err(XSDParseError::SrcElement);
                }
            },
            None => 1,
        };
        let name = match elem.remove_attribute("name", None) {
            Some(name) => match SCHEMA_BUILTIN_NCNAME.parse_with_elem(&name, elem.clone()) {
                Ok(SchemaValue::String(ncname)) => Some(ncname.to_string()),
                Ok(_) => internal_error!(self.handler, elem),
                Err(err) => {
                    error!(
                        self.handler,
                        SrcElement,
                        elem,
                        "The attribute 'name' of 'element' is failed to parse as NCName because {}.",
                        err
                    );
                    return Err(XSDParseError::SrcElement);
                }
            },
            None => None,
        };
        let nillable = elem.remove_attribute("nillable", None);
        let r#ref = match elem.remove_attribute("ref", None) {
            Some(r#ref) => match SCHEMA_BUILTIN_QNAME.parse_with_elem(&r#ref, elem.clone()) {
                Ok(SchemaValue::QName(nsname, local)) => {
                    Some((nsname.map(|ns| ns.to_string()), local.to_string()))
                }
                Ok(_) => internal_error!(self.handler, elem),
                Err(err) => {
                    error!(
                        self.handler,
                        SrcElement,
                        elem,
                        "The attribute 'ref' of 'element' is failed to parse as QName because {}.",
                        err
                    );
                    return Err(XSDParseError::SrcElement);
                }
            },
            None => None,
        };
        let r#type = elem.remove_attribute("type", None);

        self.check_attribute(elem.clone())?;

        let mut children = elem.first_child();
        let _annot = self.pop_annotation(&mut children)?;

        let mut type_definition = None;
        while let Some(child) = children {
            children = child.next_sibling();

            match child.downcast() {
                NodeKind::Element(elem) => {
                    if elem.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE) {
                        continue;
                    }

                    match elem.local_name().as_ref() {
                        SCHEMA_SIMPLETYPE_NAME => {
                            type_definition =
                                Some(TypeDefinition::Simple(self.parse_simple_type_definitions(
                                    elem,
                                    target_namespace.clone(),
                                    &mut vec![],
                                    false,
                                )?));
                        }
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
        while let Some(idc) = self.pop_idc(&mut children, self.target_namespace.clone())? {
            idc_definitions.push(idc);
        }

        self.check_unexpected_schema_elements(children)?;

        match (name, r#ref) {
            (Some(_), Some(_)) | (None, None) => {
                todo!("raise error because [src-element] clause 2.1")
            }
            (Some(name), None) => {
                let form = match form {
                    Some(form) => {
                        let form = form.trim_matches(|c| XMLVersion::XML10.is_whitespace(c));
                        match form {
                            "qualified" => QualificationForm::Qualified,
                            "unqualified" => QualificationForm::Unqualified,
                            _ => todo!("raise error"),
                        }
                    }
                    None => self.elem_form_default,
                };
                let nillable = match nillable {
                    Some(nillable) => {
                        match SCHEMA_BUILTIN_BOOLEAN.parse_with_elem(&nillable, elem.clone()) {
                            Ok(SchemaValue::Boolean(nillable)) => nillable,
                            Ok(_) => internal_error!(self.handler, elem),
                            Err(err) => {
                                error!(
                                    self.handler,
                                    SrcElement,
                                    elem,
                                    "The attribute 'nillable' of 'element' is failed to parse as boolean because {}.",
                                    err
                                );
                                return Err(XSDParseError::SrcElement);
                            }
                        }
                    }
                    None => false,
                };
                let type_definition = match (r#type, type_definition) {
                    (Some(_), Some(_)) | (None, None) => {
                        todo!("raise error because of [src-element] clause 3")
                    }
                    (Some(r#type), None) => {
                        let key = match SCHEMA_BUILTIN_QNAME.parse_with_elem(&r#type, elem.clone())
                        {
                            Ok(SchemaValue::QName(nsname, local)) => {
                                (nsname.map(|ns| ns.to_string()), local.to_string())
                            }
                            Ok(_) => internal_error!(self.handler, elem),
                            Err(err) => {
                                error!(
                                    self.handler,
                                    SrcElement,
                                    elem,
                                    "The attribute 'type' of 'element' is failed to parse as QName because {}.",
                                    err
                                );
                                return Err(XSDParseError::SrcElement);
                            }
                        };

                        if let Some(elem) = self.complex_types.remove(&key) {
                            self.parse_complex_type_definitions(
                                elem,
                                key.0.clone(),
                                &mut vec![],
                                true,
                            )?;
                        }

                        if let Some(def) = self.schema.type_defs.get(&key) {
                            def.clone()
                        } else {
                            todo!("raise error because of missing type declaration")
                        }
                    }
                    (None, Some(type_definition)) => type_definition,
                };
                let target_namespace =
                    target_namespace.filter(|_| matches!(form, QualificationForm::Qualified));

                todo!()
            }
            (None, Some(r#ref)) => {
                if type_definition.is_some()
                    || !idc_definitions.is_empty()
                    || nillable.is_some()
                    || default.is_some()
                    || fixed.is_some()
                    || form.is_some()
                    || block.is_some()
                    || r#type.is_some()
                {
                    todo!("raise error because of [src-element] clause 2.2")
                }

                if let Some(elem) = self.elem_decls.remove(&r#ref) {
                    self.parse_top_level_element(elem)?;
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

    /// # Reference
    /// - [src-attribute_group](https://www.w3.org/TR/xmlschema-1/#src-attribute_group)
    fn parse_named_attgroup_definition(
        &mut self,
        mut elem: Element,
        target_namespace: Option<String>,
        loop_guard: &mut Vec<(Option<String>, String)>,
    ) -> Result<(), XSDParseError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert_eq!(elem.local_name().as_ref(), SCHEMA_ATTGROUP_NAME);

        let _id = elem.remove_attribute("id", None);
        let name = match elem.remove_attribute("name", None) {
            Some(name) => match SCHEMA_BUILTIN_NCNAME.parse_with_elem(&name, elem.clone()) {
                Ok(SchemaValue::String(ncname)) => ncname.to_string(),
                Ok(_) => internal_error!(self.handler, elem),
                Err(err) => {
                    error!(
                        self.handler,
                        SrcAttributeGroup,
                        elem,
                        "The attribute 'name' of '{}' is failed to parse as NCName because {}.",
                        SCHEMA_ATTGROUP_NAME,
                        err
                    );
                    return Err(XSDParseError::SrcAttributeGroup);
                }
            },
            None => todo!("raise error"),
        };

        loop_guard.push((target_namespace.clone(), name.clone()));

        self.check_attribute(elem.clone())?;

        let mut children = elem.first_child();
        let annotation = self.pop_annotation(&mut children)?;
        let (attribute_uses, attribute_wildcard) =
            self.pop_attr_decls(&mut children, target_namespace.clone(), loop_guard)?;

        self.check_unexpected_schema_elements(children)?;

        let def = AttributeGroupDefinition {
            name: name.clone(),
            target_namespace: target_namespace.clone(),
            attribute_uses,
            attribute_wildcard,
            annotation,
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
        loop_guard: &mut Vec<(Option<String>, String)>,
    ) -> Result<(Option<String>, String), XSDParseError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert_eq!(elem.local_name().as_ref(), SCHEMA_ATTGROUP_NAME);

        let _id = elem.remove_attribute("id", None);
        let r#ref = match elem.remove_attribute("ref", None) {
            Some(r#ref) => match SCHEMA_BUILTIN_QNAME.parse_with_elem(&r#ref, elem.clone()) {
                Ok(SchemaValue::QName(nsname, local)) => {
                    (nsname.map(|ns| ns.to_string()), local.to_string())
                }
                Ok(_) => internal_error!(self.handler, elem),
                Err(err) => {
                    error!(
                        self.handler,
                        SrcAttributeGroup,
                        elem,
                        "The attribute 'ref' of '{}' is failed to parse as QName because {}.",
                        SCHEMA_ATTGROUP_NAME,
                        err
                    );
                    return Err(XSDParseError::SrcAttributeGroup);
                }
            },
            None => todo!("raise error"),
        };

        self.check_attribute(elem.clone())?;

        let _annot = self.parse_single_annotation_child(elem.clone())?;

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

    fn parse_simple_type_definitions(
        &mut self,
        mut elem: Element,
        target_namespace: Option<String>,
        loop_guard: &mut Vec<(Option<String>, String)>,
        global: bool,
    ) -> Result<Arc<SimpleTypeDefinition>, XSDParseError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert_eq!(elem.local_name().as_ref(), SCHEMA_SIMPLETYPE_NAME);

        let r#final = elem.remove_attribute("final", None);
        let _id = elem.remove_attribute("id", None);
        let name = match elem.remove_attribute("name", None) {
            Some(name) => match SCHEMA_BUILTIN_NCNAME.parse_with_elem(&name, elem.clone()) {
                Ok(SchemaValue::String(ncname)) => ncname.to_string(),
                Ok(_) => internal_error!(self.handler, elem),
                Err(err) => {
                    error!(
                        self.handler,
                        SrcSimpleType,
                        elem,
                        "The attribute 'name' of '{}' is failed to parse as QName because {}.",
                        SCHEMA_SIMPLETYPE_NAME,
                        err
                    );
                    return Err(XSDParseError::SrcSimpleType);
                }
            },
            None => {
                if !global {
                    self.unnamed += 1;
                    // Choose a name that is not an NCName to avoid conflicts
                    // with other type definitions.
                    format!("#unnamed{}", self.unnamed)
                } else {
                    todo!("raise error because of missing mandatory attribute");
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
        // extract annotation
        let _annot = self.pop_annotation(&mut children)?;
        // extract derivation method
        let mut ret = None;
        while let Some(child) = children {
            children = child.next_sibling();

            match child.downcast() {
                NodeKind::Element(mut elem) => {
                    if elem.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE) {
                        continue;
                    }

                    match elem.local_name().as_ref() {
                        SCHEMA_RESTRICTION_NAME => {
                            if ret.is_some() {
                                todo!("raise error")
                            }
                            let mut children = elem.first_child();
                            // extract annotation
                            let _annot = self.pop_annotation(&mut children)?;
                            // extract simpleType if exists
                            let mut simple_type = None;
                            while let Some(child) = children {
                                children = child.next_sibling();

                                match child.downcast() {
                                    NodeKind::Element(elem) => {
                                        if elem.namespace_name().as_deref()
                                            != Some(XML_SCHEMA_NAMESPACE)
                                        {
                                            continue;
                                        }

                                        match elem.local_name().as_ref() {
                                            SCHEMA_SIMPLETYPE_NAME => simple_type = Some(elem),
                                            _ => children = Some(child),
                                        }
                                        break;
                                    }
                                    NodeKind::Text(_) => unallowed_text!(self.handler, elem),
                                    _ => {}
                                }
                            }

                            let _id = elem.remove_attribute("id", None);
                            let base = match elem.remove_attribute("base", None) {
                                Some(base) => {
                                    if simple_type.is_some() {
                                        // [src-restriction-base-or-simpleType]
                                        // If the `base` attribute is present,
                                        // the child must not have a `simpleType`.
                                        todo!("raise error");
                                    }
                                    match SCHEMA_BUILTIN_QNAME.parse_with_elem(&base, elem.clone())
                                    {
                                        Ok(SchemaValue::QName(nsname, local)) => {
                                            let key =
                                                (nsname.map(|n| n.to_string()), local.to_string());
                                            if let Some(elem) = self.simple_types.remove(&key) {
                                                self.parse_simple_type_definitions(
                                                    elem,
                                                    key.0.clone(),
                                                    loop_guard,
                                                    true,
                                                )?;
                                            }
                                            if let Some(def) = self.schema.type_defs.get(&key) {
                                                match def {
                                                    TypeDefinition::Simple(simple) => {
                                                        simple.clone()
                                                    }
                                                    TypeDefinition::Complex(_) => {
                                                        todo!("raise error")
                                                    }
                                                }
                                            } else if loop_guard.contains(&key) {
                                                todo!("raise error because of type reference loop")
                                            } else {
                                                todo!("raise error because of undefined type")
                                            }
                                        }
                                        Ok(_) => internal_error!(self.handler, elem),
                                        Err(err) => {
                                            error!(
                                                self.handler,
                                                SrcSimpleType,
                                                elem,
                                                "The attribute 'base' of '{}' is failed to parse as QName because {}.",
                                                SCHEMA_RESTRICTION_NAME,
                                                err
                                            );
                                            return Err(XSDParseError::SrcSimpleType);
                                        }
                                    }
                                }
                                None => {
                                    let Some(simple_type) = simple_type else {
                                        // [src-restriction-base-or-simpleType]
                                        // If there is no `base` attribute, there
                                        // must be a child of type `simpleType`.
                                        todo!("raise error");
                                    };

                                    self.parse_simple_type_definitions(
                                        simple_type,
                                        target_namespace.clone(),
                                        loop_guard,
                                        false,
                                    )?
                                }
                            };

                            let mut builder = match base.clone().derive_by_restriction() {
                                Ok(builder) => builder,
                                Err(err) => todo!("raise error"),
                            };

                            // extract facets
                            let builder = loop {
                                match self.pop_facet(&mut children, &base, builder)? {
                                    Ok(b) => builder = b,
                                    Err(b) => break b,
                                }
                            };
                            ret = Some(builder);
                        }
                        SCHEMA_LIST_NAME => {
                            if ret.is_some() {
                                todo!("raise error")
                            }
                            let mut children = elem.first_child();
                            // extract annotation
                            let _annot = self.pop_annotation(&mut children)?;
                            // extract simpleType if exists
                            let mut simple_type = None;
                            while let Some(child) = children {
                                children = child.next_sibling();

                                match child.downcast() {
                                    NodeKind::Element(elem) => {
                                        if elem.namespace_name().as_deref()
                                            != Some(XML_SCHEMA_NAMESPACE)
                                        {
                                            continue;
                                        }

                                        match elem.local_name().as_ref() {
                                            SCHEMA_SIMPLETYPE_NAME => simple_type = Some(elem),
                                            _ => children = Some(child),
                                        }
                                        break;
                                    }
                                    NodeKind::Text(_) => unallowed_text!(self.handler, elem),
                                    _ => {}
                                }
                            }

                            let _id = elem.remove_attribute("id", None);
                            let item_type = match elem.remove_attribute("itemType", None) {
                                Some(item_type) => {
                                    if simple_type.is_some() {
                                        // [src-list-itemType-or-simpleType]
                                        // If the `itemType` attribute is present,
                                        // the child must not have a `simpleType`.
                                        todo!("raise error");
                                    }
                                    match SCHEMA_BUILTIN_QNAME
                                        .parse_with_elem(&item_type, elem.clone())
                                    {
                                        Ok(SchemaValue::QName(nsname, local)) => {
                                            let key =
                                                (nsname.map(|n| n.to_string()), local.to_string());
                                            if let Some(elem) = self.simple_types.remove(&key) {
                                                self.parse_simple_type_definitions(
                                                    elem,
                                                    key.0.clone(),
                                                    loop_guard,
                                                    true,
                                                )?;
                                            }
                                            if let Some(def) = self.schema.type_defs.get(&key) {
                                                match def {
                                                    TypeDefinition::Simple(simple) => {
                                                        simple.clone()
                                                    }
                                                    TypeDefinition::Complex(_) => {
                                                        todo!("raise error")
                                                    }
                                                }
                                            } else if loop_guard.contains(&key) {
                                                todo!("raise error because of type reference loop")
                                            } else {
                                                todo!("raise error because of undefined type")
                                            }
                                        }
                                        Ok(_) => internal_error!(self.handler, elem),
                                        Err(err) => {
                                            error!(
                                                self.handler,
                                                SrcSimpleType,
                                                elem,
                                                "The attribute 'itemType' of '{}' is failed to parse as QName because {}.",
                                                SCHEMA_LIST_NAME,
                                                err
                                            );
                                            return Err(XSDParseError::SrcSimpleType);
                                        }
                                    }
                                }
                                None => {
                                    let Some(simple_type) = simple_type else {
                                        // [src-list-itemType-or-simpleType]
                                        // If there is no `itemType` attribute, there
                                        // must be a child of type `simpleType`.
                                        todo!("raise error");
                                    };

                                    self.parse_simple_type_definitions(
                                        simple_type,
                                        target_namespace.clone(),
                                        loop_guard,
                                        false,
                                    )?
                                }
                            };

                            // Check to see if there are any unexpected children
                            self.check_unexpected_schema_elements(children)?;

                            let builder = match item_type.clone().derive_by_list() {
                                Ok(builder) => builder,
                                Err(err) => todo!("raise error"),
                            };
                            ret = Some(builder);
                        }
                        SCHEMA_UNION_NAME => {
                            if ret.is_some() {
                                todo!("raise error")
                            }
                            let mut children = elem.first_child();
                            // extract annotation
                            let _annot = self.pop_annotation(&mut children)?;
                            // extract simpleTypes if exist
                            let mut members = vec![];
                            while let Some(child) = children {
                                children = child.next_sibling();

                                match child.downcast() {
                                    NodeKind::Element(elem) => {
                                        if elem.namespace_name().as_deref()
                                            != Some(XML_SCHEMA_NAMESPACE)
                                        {
                                            continue;
                                        }

                                        match elem.local_name().as_ref() {
                                            SCHEMA_SIMPLETYPE_NAME => {
                                                let def = self.parse_simple_type_definitions(
                                                    elem,
                                                    target_namespace.clone(),
                                                    loop_guard,
                                                    false,
                                                )?;
                                                members.push(def);
                                            }
                                            _ => unallowed_element!(self.handler, elem),
                                        }
                                    }
                                    NodeKind::Text(_) => unallowed_text!(self.handler, elem),
                                    _ => {}
                                }
                            }

                            let _id = elem.remove_attribute("id", None);
                            if let Some(member_types) = elem.remove_attribute("memberTypes", None) {
                                for member_type in member_types
                                    .split(|c| XMLVersion::XML10.is_whitespace(c))
                                    .filter(|s| !s.is_empty())
                                {
                                    match SCHEMA_BUILTIN_QNAME
                                        .parse_with_elem(member_type, elem.clone())
                                    {
                                        Ok(SchemaValue::QName(nsname, local)) => {
                                            let key =
                                                (nsname.map(|n| n.to_string()), local.to_string());
                                            if let Some(elem) = self.simple_types.remove(&key) {
                                                self.parse_simple_type_definitions(
                                                    elem,
                                                    key.0.clone(),
                                                    loop_guard,
                                                    true,
                                                )?;
                                            }
                                            let def = if let Some(def) =
                                                self.schema.type_defs.get(&key)
                                            {
                                                match def {
                                                    TypeDefinition::Simple(simple) => {
                                                        simple.clone()
                                                    }
                                                    TypeDefinition::Complex(_) => {
                                                        todo!("raise error")
                                                    }
                                                }
                                            } else if loop_guard.contains(&key) {
                                                todo!("raise error because of type reference loop")
                                            } else {
                                                todo!("raise error because of undefined type")
                                            };
                                            members.push(def);
                                        }
                                        Ok(_) => internal_error!(self.handler, elem),
                                        Err(err) => {
                                            error!(
                                                self.handler,
                                                SrcSimpleType,
                                                elem,
                                                "The attribute 'memberTypes' of '{}' is failed to parse as QName because {}.",
                                                SCHEMA_UNION_NAME,
                                                err
                                            );
                                            return Err(XSDParseError::SrcSimpleType);
                                        }
                                    }
                                }
                            } else if members.is_empty() {
                                // [src-union-memberTypes-or-simpleTypes]
                                // If there is no `memberType` attribute, there
                                // must be children of type `simpleType`.
                                todo!("raise error")
                            }

                            let Ok(builder) = SimpleTypeDefinition::derive_by_union(members) else {
                                todo!("raise error")
                            };
                            ret = Some(builder);
                        }
                        _ => unallowed_element!(self.handler, elem),
                    }
                }
                NodeKind::Text(_) => unallowed_text!(self.handler, elem),
                _ => {}
            }
        }

        loop_guard.pop();

        if let Some(mut builder) = ret {
            if let Some(r#final) = r#final {
                let f = r#final.trim_matches(|c| XMLVersion::XML10.is_whitespace(c));
                let mut set = FinalSet::default();
                if f == "#all" {
                    set = Final::List | Final::Union | Final::Restriction;
                } else {
                    // Logically, I'd think duplicates wouldn't be allowed, but
                    // since I can't find any mention in the specs saying they're
                    // not allowed, I'll put off implementing that for now.
                    for f in f
                        .split(|c| XMLVersion::XML10.is_whitespace(c))
                        .filter(|s| !s.is_empty())
                    {
                        match f {
                            "list" => set = set | Final::List,
                            "union" => set = set | Final::Union,
                            "restriction" => set = set | Final::Restriction,
                            _ => todo!("raise error"),
                        }
                    }
                }
                builder = builder.r#final(set);
            }
            let def = builder
                .name(
                    name.as_str().into(),
                    target_namespace.as_deref().map(|s| s.into()),
                )
                .build();
            if global {
                self.schema.type_defs.insert(
                    (target_namespace, name),
                    TypeDefinition::Simple(def.clone()),
                );
            }
            Ok(def)
        } else {
            todo!("raise error")
        }
    }

    fn parse_complex_type_definitions(
        &mut self,
        mut elem: Element,
        target_namespace: Option<String>,
        loop_guard: &mut Vec<(Option<String>, String)>,
        global: bool,
    ) -> Result<Arc<ComplexTypeDefinition>, XSDParseError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert_eq!(elem.local_name().as_ref(), SCHEMA_COMPLEXTYPE_NAME);

        let r#abstract = match elem.remove_attribute("abstract", None) {
            Some(abst) => match SCHEMA_BUILTIN_BOOLEAN.parse_with_elem(&abst, elem.clone()) {
                Ok(SchemaValue::Boolean(abst)) => abst,
                Ok(_) => internal_error!(self.handler, elem),
                Err(err) => {
                    error!(
                        self.handler,
                        SrcCt,
                        elem,
                        "The attribute 'abstract' of '{}' is failed to parse as boolean because {}.",
                        SCHEMA_COMPLEXTYPE_NAME,
                        err
                    );
                    return Err(XSDParseError::SrcCt);
                }
            },
            None => false,
        };
        let block = elem.remove_attribute("block", None);
        let r#final = elem.remove_attribute("final", None);
        let _id = elem.remove_attribute("id", None);
        let mixed = match elem.remove_attribute("mixed", None) {
            Some(mixed) => match SCHEMA_BUILTIN_BOOLEAN.parse_with_elem(&mixed, elem.clone()) {
                Ok(SchemaValue::Boolean(mixed)) => mixed,
                Ok(_) => internal_error!(self.handler, elem),
                Err(err) => {
                    error!(
                        self.handler,
                        SrcCt,
                        elem,
                        "The attribute 'mixed' of '{}' is failed to parse as boolean because {}.",
                        SCHEMA_COMPLEXTYPE_NAME,
                        err
                    );
                    return Err(XSDParseError::SrcCt);
                }
            },
            None => false,
        };
        let name = match elem.remove_attribute("name", None) {
            Some(name) => match SCHEMA_BUILTIN_NCNAME.parse_with_elem(&name, elem.clone()) {
                Ok(SchemaValue::String(ncname)) => ncname.to_string(),
                Ok(_) => internal_error!(self.handler, elem),
                Err(err) => {
                    error!(
                        self.handler,
                        SrcCt,
                        elem,
                        "The attribute 'name' of '{}' is failed to parse as boolean because {}.",
                        SCHEMA_COMPLEXTYPE_NAME,
                        err
                    );
                    return Err(XSDParseError::SrcCt);
                }
            },
            None => {
                if !global {
                    self.unnamed += 1;
                    // Choose a name that is not an NCName to avoid conflicts
                    // with other type definitions.
                    format!("#unnamed{}", self.unnamed)
                } else {
                    error!(
                        self.handler,
                        SrcCt,
                        elem,
                        "The mandatory attribute 'name' of '{}' is missing.",
                        elem.local_name()
                    );
                    return Err(XSDParseError::SrcCt);
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
        // extract annotation
        let _annot = self.pop_annotation(&mut children)?;
        // simpleContent
        // | complexContent
        // | ((group | all | choice | sequence)?, (attribute | attributeGroup)*, anyAttribute?)
        // extract content model
        while let Some(child) = children {
            children = child.next_sibling();

            match child.downcast() {
                NodeKind::Element(mut elem) => {
                    if elem.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE) {
                        continue;
                    }

                    match elem.local_name().as_ref() {
                        SCHEMA_SIMPLECONT_NAME => {
                            let (content_type, attribute_uses, attribute_wildcard) = self
                                .parse_simple_content(elem, target_namespace.clone(), loop_guard)?;
                        }
                        SCHEMA_COMPLEXCONT_NAME => {
                            let (content_type, attribute_uses, attribute_wildcard) = self
                                .parse_complex_content(
                                    elem,
                                    target_namespace.clone(),
                                    loop_guard,
                                    mixed,
                                )?;
                        }
                        _ => {
                            children = Some(child);
                            let particle = self
                                .pop_type_def_particle(&mut children, target_namespace.clone())?;
                            let (attribute_uses, attribute_wildcard) = self.pop_attr_decls(
                                &mut children,
                                target_namespace.clone(),
                                loop_guard,
                            )?;
                        }
                    }
                    break;
                }
                NodeKind::Text(_) => unallowed_text!(self.handler, elem),
                _ => {}
            }
        }
        self.check_unexpected_schema_elements(children)?;

        loop_guard.pop();
        todo!()
    }

    fn parse_simple_content(
        &mut self,
        mut elem: Element,
        target_namespace: Option<String>,
        loop_guard: &mut Vec<(Option<String>, String)>,
    ) -> Result<(ContentType, Vec<AttributeUse>, Option<Wildcard>), XSDParseError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert_eq!(elem.local_name().as_ref(), SCHEMA_SIMPLECONT_NAME);

        let _id = elem.remove_attribute("id", None);
        self.check_attribute(elem.clone())?;

        let mut children = elem.first_child();
        // extract annotation
        let _annot = self.pop_annotation(&mut children)?;
        // extract derivation method
        let mut content = None;
        let mut attribute_uses = vec![];
        let mut attribute_wildcard = None;
        while let Some(child) = children {
            children = child.next_sibling();

            match child.downcast() {
                NodeKind::Element(mut elem) => {
                    if elem.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE) {
                        continue;
                    }

                    match elem.local_name().as_ref() {
                        SCHEMA_RESTRICTION_NAME => {
                            let mut children = elem.first_child();
                            // extract annotation
                            let _annot = self.pop_annotation(&mut children)?;
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
                            let mut simple_type = None;
                            while let Some(child) = children {
                                children = child.next_sibling();

                                match child.downcast() {
                                    NodeKind::Element(elem) => {
                                        if elem.namespace_name().as_deref()
                                            != Some(XML_SCHEMA_NAMESPACE)
                                        {
                                            continue;
                                        }

                                        match elem.local_name().as_ref() {
                                            SCHEMA_SIMPLETYPE_NAME => simple_type = Some(elem),
                                            _ => children = Some(child),
                                        }
                                        break;
                                    }
                                    NodeKind::Text(_) => unallowed_text!(self.handler, elem),
                                    _ => {}
                                }
                            }

                            let _id = elem.remove_attribute("id", None);
                            let base = match elem.remove_attribute("base", None) {
                                Some(base) => {
                                    match SCHEMA_BUILTIN_QNAME.parse_with_elem(&base, elem.clone())
                                    {
                                        Ok(SchemaValue::QName(nsname, local)) => {
                                            let key =
                                                (nsname.map(|n| n.to_string()), local.to_string());
                                            if let Some(elem) = self.complex_types.remove(&key) {
                                                self.parse_complex_type_definitions(
                                                    elem,
                                                    key.0.clone(),
                                                    loop_guard,
                                                    true,
                                                )?;
                                            }
                                            if let Some(def) = self.schema.type_defs.get(&key) {
                                                match def {
                                                    TypeDefinition::Simple(_) => {
                                                        todo!("raise error because of [src-ct] 2.1")
                                                    }
                                                    TypeDefinition::Complex(complex) => {
                                                        match &complex.content_type {
                                                            ContentType::Simple(simple) => {
                                                                // [src-ct] 2.1.1
                                                                simple.clone()
                                                            }
                                                            ContentType::Mixed(_) => {
                                                                todo!("[src-ct] 2.1.2")
                                                            }
                                                            _ => todo!(
                                                                "raise error because of src-ct 2.1"
                                                            ),
                                                        }
                                                    }
                                                }
                                            } else if loop_guard.contains(&key) {
                                                todo!("raise error because of type reference loop")
                                            } else {
                                                todo!("raise error because of undefined type")
                                            }
                                        }
                                        Ok(_) => internal_error!(self.handler, elem),
                                        Err(err) => todo!("raise error"),
                                    }
                                }
                                None => todo!("raise error"),
                            };

                            let mut builder = match base.clone().derive_by_restriction() {
                                Ok(builder) => builder,
                                Err(err) => todo!("raise error"),
                            };

                            // extract facets
                            let builder = loop {
                                match self.pop_facet(&mut children, &base, builder)? {
                                    Ok(b) => builder = b,
                                    Err(b) => break b,
                                }
                            };
                            content = Some(builder.build());

                            (attribute_uses, attribute_wildcard) = self.pop_attr_decls(
                                &mut children,
                                target_namespace.clone(),
                                loop_guard,
                            )?;
                            self.check_unexpected_schema_elements(children)?;
                        }
                        SCHEMA_EXTENSION_NAME => {
                            let mut children = elem.first_child();
                            // extract annotation
                            let _annot = self.pop_annotation(&mut children)?;
                            let _id = elem.remove_attribute("id", None);
                            let base = match elem.remove_attribute("base", None) {
                                Some(base) => {
                                    match SCHEMA_BUILTIN_QNAME.parse_with_elem(&base, elem.clone())
                                    {
                                        Ok(SchemaValue::QName(nsname, local)) => {
                                            let key =
                                                (nsname.map(|n| n.to_string()), local.to_string());
                                            if let Some(elem) = self.complex_types.remove(&key) {
                                                self.parse_complex_type_definitions(
                                                    elem,
                                                    key.0.clone(),
                                                    loop_guard,
                                                    true,
                                                )?;
                                            }
                                            if let Some(def) = self.schema.type_defs.get(&key) {
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
                                                                todo!(
                                                                    "raise error because of [src-ct] 2.1"
                                                                )
                                                            }
                                                        }
                                                    }
                                                }
                                            } else if loop_guard.contains(&key) {
                                                todo!("raise error because of type reference loop")
                                            } else {
                                                todo!("raise error because of undefined type")
                                            }
                                        }
                                        Ok(_) => internal_error!(self.handler, elem),
                                        Err(err) => {
                                            error!(
                                                self.handler,
                                                SrcCt,
                                                elem,
                                                "The attribute 'base' of '{}' is failed to parse as QName because {}.",
                                                SCHEMA_SIMPLECONT_NAME,
                                                err
                                            );
                                            return Err(XSDParseError::SrcCt);
                                        }
                                    }
                                }
                                None => todo!("raise error"),
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
                }
                NodeKind::Text(_) => unallowed_text!(self.handler, elem),
                _ => {}
            }
        }

        let Some(content) = content else {
            todo!("raise error")
        };
        Ok((
            ContentType::Simple(content),
            attribute_uses,
            attribute_wildcard,
        ))
    }

    fn parse_complex_content(
        &mut self,
        mut elem: Element,
        target_namespace: Option<String>,
        loop_guard: &mut Vec<(Option<String>, String)>,
        mixed: bool,
    ) -> Result<(ContentType, Vec<AttributeUse>, Option<Wildcard>), XSDParseError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert_eq!(elem.local_name().as_ref(), SCHEMA_COMPLEXCONT_NAME);

        let _id = elem.remove_attribute("id", None);
        let mixed = match elem.remove_attribute("mixed", None) {
            Some(mixed) => match SCHEMA_BUILTIN_BOOLEAN.parse_with_elem(&mixed, elem.clone()) {
                Ok(SchemaValue::Boolean(mixed)) => mixed,
                Ok(_) => internal_error!(self.handler, elem),
                Err(err) => {
                    error!(
                        self.handler,
                        SrcCt,
                        elem,
                        "The attribute 'mixed' of '{}' is failed to parse as boolean because {}.",
                        SCHEMA_COMPLEXCONT_NAME,
                        err
                    );
                    return Err(XSDParseError::SrcCt);
                }
            },
            None => mixed,
        };

        let mut children = elem.first_child();
        // extract annotation
        let _annot = self.pop_annotation(&mut children)?;
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
                    let Some(base) = elem.remove_attribute("base", None) else {
                        todo!("raise error because of missing mandatory attribute");
                    };

                    let mut children = elem.first_child();
                    let _annot = self.pop_annotation(&mut children)?;
                    let model =
                        self.pop_type_def_particle(&mut children, target_namespace.clone())?;
                    let (attribute_uses, attribute_wildcard) =
                        self.pop_attr_decls(&mut children, target_namespace.clone(), loop_guard)?;
                }
                NodeKind::Text(_) => unallowed_text!(self.handler, elem),
                _ => {}
            }
        }

        todo!()
    }

    fn parse_any_attribute(
        &mut self,
        mut elem: Element,
        target_namespace: Option<String>,
    ) -> Result<Wildcard, XSDParseError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert_eq!(elem.local_name().as_ref(), SCHEMA_ANYATT_NAME);

        let _id = elem.remove_attribute("id", None);
        let namespace = match elem.remove_attribute("namespace", None) {
            Some(namespace) => {
                let namespace = namespace.trim_matches(|c| XMLVersion::XML10.is_whitespace(c));
                if namespace == "##any" {
                    NamespaceConstraint::Any
                } else if namespace == "##other" {
                    NamespaceConstraint::Not(target_namespace)
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
                                Ok(SchemaValue::AnyURI(uri)) => list.push(Some(uri.to_string())),
                                Ok(_) => internal_error!(self.handler, elem),
                                Err(err) => {
                                    error!(
                                        self.handler,
                                        SrcCt,
                                        elem,
                                        "The attribute 'namespace' of '{}' is failed to parse as anyURI because {}.",
                                        SCHEMA_ANYATT_NAME,
                                        err
                                    );
                                    return Err(XSDParseError::SrcCt);
                                }
                            }
                        }
                    }
                    NamespaceConstraint::List(list)
                }
            }
            None => NamespaceConstraint::Any,
        };
        let process_contents = match elem.remove_attribute("processContents", None) {
            Some(s) => {
                let s = s.trim_matches(|c| XMLVersion::XML10.is_whitespace(c));
                match s {
                    "lax" => ProcessContent::Lax,
                    "skip" => ProcessContent::Skip,
                    "strict" => ProcessContent::Strict,
                    _ => todo!("raise error"),
                }
            }
            None => ProcessContent::Strict,
        };

        self.check_attribute(elem.clone())?;

        let _annot = self.parse_single_annotation_child(elem)?;

        Ok(Wildcard {
            ns_constraint: namespace,
            process_contents,
        })
    }

    fn parse_named_group(
        &mut self,
        mut elem: Element,
        target_namespace: Option<String>,
    ) -> Result<(), XSDParseError> {
        let _id = elem.remove_attribute("id", None);
        let Some(name) = elem.remove_attribute("name", None) else {
            error!(
                self.handler,
                SrcModelGroupDefn,
                elem,
                "The attribute 'name' of '{}' is mandatory, but is absent.",
                SCHEMA_GROUP_NAME
            );
            return Err(XSDParseError::SrcModelGroupDefn);
        };
        todo!()
    }

    fn parse_group_ref(
        &mut self,
        mut elem: Element,
        target_namespace: Option<String>,
    ) -> Result<Particle, XSDParseError> {
        let _id = elem.remove_attribute("id", None);
        let max_occurs = self.parse_occurs(elem.clone(), "maxOccurs", Some(1), true)?;
        let Some(min_occurs) = self.parse_occurs(elem.clone(), "minOccurs", Some(1), false)? else {
            internal_error!(self.handler, elem);
        };
        let Some(r#ref) = elem.remove_attribute("ref", None) else {
            error!(
                self.handler,
                SrcModelGroup,
                elem,
                "The attribute 'ref' of '{}' is mandatory, but is absent.",
                SCHEMA_GROUP_NAME
            );
            return Err(XSDParseError::SrcModelGroup);
        };
        self.check_attribute(elem.clone())?;

        let mut children = elem.first_child();
        let _annot = self.pop_annotation(&mut children)?;
        self.check_unexpected_schema_elements(children)?;

        todo!()
    }

    fn parse_choice(
        &mut self,
        mut elem: Element,
        target_namespace: Option<String>,
    ) -> Result<Particle, XSDParseError> {
        let _id = elem.remove_attribute("id", None);
        let max_occurs = self.parse_occurs(elem.clone(), "maxOccurs", Some(1), true)?;
        let Some(min_occurs) = self.parse_occurs(elem.clone(), "minOccurs", Some(1), false)? else {
            internal_error!(self.handler, elem);
        };

        self.check_attribute(elem.clone())?;

        let mut children = elem.first_child();
        let _annot = self.pop_annotation(&mut children)?;

        let mut particles = vec![];
        while let Some(particle) =
            self.pop_nested_particle(&mut children, target_namespace.clone())?
        {
            particles.push(particle);
        }

        let model = ModelGroup {
            compositor: Compositor::Choice,
            particles,
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
        target_namespace: Option<String>,
    ) -> Result<Particle, XSDParseError> {
        let _id = elem.remove_attribute("id", None);
        let max_occurs = self.parse_occurs(elem.clone(), "maxOccurs", Some(1), true)?;
        let Some(min_occurs) = self.parse_occurs(elem.clone(), "minOccurs", Some(1), false)? else {
            internal_error!(self.handler, elem);
        };

        self.check_attribute(elem.clone())?;

        let mut children = elem.first_child();
        let _annot = self.pop_annotation(&mut children)?;

        let mut particles = vec![];
        while let Some(particle) =
            self.pop_nested_particle(&mut children, target_namespace.clone())?
        {
            particles.push(particle);
        }

        let model = ModelGroup {
            compositor: Compositor::Choice,
            particles,
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
        target_namespace: Option<String>,
    ) -> Result<Particle, XSDParseError> {
        let _id = elem.remove_attribute("id", None);
        let Some(max_occurs) = self.parse_occurs(elem.clone(), "maxOccurs", Some(1), false)? else {
            internal_error!(self.handler, elem);
        };
        let Some(min_occurs) = self.parse_occurs(elem.clone(), "minOccurs", Some(1), false)? else {
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
            return Err(XSDParseError::SrcModelGroup);
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
            return Err(XSDParseError::SrcModelGroup);
        }

        self.check_attribute(elem.clone())?;

        let mut children = elem.first_child();
        let _annot = self.pop_annotation(&mut children)?;

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
            particles,
        };

        Ok(Particle {
            min_occurs,
            max_occurs: Some(max_occurs),
            term: Term::ModelGroup(model),
        })
    }

    fn parse_boundary(
        &mut self,
        mut elem: Element,
        base: &SimpleTypeDefinition,
    ) -> Result<(bool, SchemaValue), XSDParseError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert!(matches!(
            elem.local_name().as_ref(),
            SCHEMA_MINEXCLUSIVE_NAME
                | SCHEMA_MININCLUSIVE_NAME
                | SCHEMA_MAXEXCLUSIVE_NAME
                | SCHEMA_MAXINCLUSIVE_NAME
        ));

        let fixed = elem.remove_attribute("fixed", None);
        let _id = elem.remove_attribute("id", None);
        let Some(value) = elem.remove_attribute("value", None) else {
            todo!("raise error because of missing a mandatory attribute");
        };
        self.check_attribute(elem.clone())?;

        let _annot = self.parse_single_annotation_child(elem.clone())?;

        let fixed = match fixed {
            Some(fixed) => match SCHEMA_BUILTIN_BOOLEAN.parse_with_elem(&fixed, elem.clone()) {
                Ok(SchemaValue::Boolean(fixed)) => fixed,
                Ok(_) => internal_error!(self.handler, elem),
                Err(err) => {
                    error!(
                        self.handler,
                        SrcSimpleType,
                        elem,
                        "The attribute 'fixed' of '{}' is failed to parse as boolean because {}.",
                        elem.local_name(),
                        err
                    );
                    return Err(XSDParseError::SrcSimpleType);
                }
            },
            None => false,
        };
        let Ok(value) = base.parse_with_elem(&value, elem.clone()) else {
            todo!("raise error")
        };
        Ok((fixed, value))
    }

    fn parse_total_digits(
        &mut self,
        mut elem: Element,
    ) -> Result<(bool, NonZeroUsize), XSDParseError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert!(elem.local_name().as_ref() == SCHEMA_TOTALDIGITS_NAME);

        let fixed = elem.remove_attribute("fixed", None);
        let _id = elem.remove_attribute("id", None);
        let Some(value) = elem.remove_attribute("value", None) else {
            todo!("raise error because of missing a mandatory attribute");
        };
        self.check_attribute(elem.clone())?;

        let _annot = self.parse_single_annotation_child(elem.clone())?;

        let fixed = match fixed {
            Some(fixed) => match SCHEMA_BUILTIN_BOOLEAN.parse_with_elem(&fixed, elem.clone()) {
                Ok(SchemaValue::Boolean(fixed)) => fixed,
                Ok(_) => internal_error!(self.handler, elem),
                Err(err) => {
                    error!(
                        self.handler,
                        SrcSimpleType,
                        elem,
                        "The attribute 'fixed' of '{}' is failed to parse as boolean because {}.",
                        elem.local_name(),
                        err
                    );
                    return Err(XSDParseError::SrcSimpleType);
                }
            },
            None => false,
        };
        let Ok(SchemaValue::Integer(value)) =
            SCHEMA_BUILTIN_POSITIVE_INTEGER.parse_with_elem(&value, elem.clone())
        else {
            todo!("raise error")
        };
        let Ok(value) = usize::try_from(value) else {
            todo!("raise error because of too big positive integer")
        };
        let Some(value) = NonZeroUsize::new(value) else {
            internal_error!(self.handler, elem);
        };
        Ok((fixed, value))
    }

    fn parse_fraction_digits(&mut self, mut elem: Element) -> Result<(bool, usize), XSDParseError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert!(elem.local_name().as_ref() == SCHEMA_FRACDIGITS_NAME);

        let fixed = elem.remove_attribute("fixed", None);
        let _id = elem.remove_attribute("id", None);
        let Some(value) = elem.remove_attribute("value", None) else {
            todo!("raise error because of missing a mandatory attribute");
        };
        self.check_attribute(elem.clone())?;

        let _annot = self.parse_single_annotation_child(elem.clone())?;

        let fixed = match fixed {
            Some(fixed) => match SCHEMA_BUILTIN_BOOLEAN.parse_with_elem(&fixed, elem.clone()) {
                Ok(SchemaValue::Boolean(fixed)) => fixed,
                Ok(_) => internal_error!(self.handler, elem),
                Err(err) => {
                    error!(
                        self.handler,
                        SrcSimpleType,
                        elem,
                        "The attribute 'fixed' of '{}' is failed to parse as boolean because {}.",
                        elem.local_name().as_ref(),
                        err
                    );
                    return Err(XSDParseError::SrcSimpleType);
                }
            },
            None => false,
        };
        let Ok(SchemaValue::Integer(value)) =
            SCHEMA_BUILTIN_NONNEGATIVE_INTEGER.parse_with_elem(&value, elem.clone())
        else {
            todo!("raise error")
        };
        let Ok(value) = usize::try_from(value) else {
            todo!("raise error because of too big positive integer")
        };
        Ok((fixed, value))
    }

    fn parse_length(&mut self, mut elem: Element) -> Result<(bool, usize), XSDParseError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert!(matches!(
            elem.local_name().as_ref(),
            SCHEMA_LENGTH_NAME | SCHEMA_MINLENGTH_NAME | SCHEMA_MAXLENGTH_NAME
        ));

        let fixed = elem.remove_attribute("fixed", None);
        let _id = elem.remove_attribute("id", None);
        let Some(value) = elem.remove_attribute("value", None) else {
            todo!("raise error because of missing a mandatory attribute");
        };
        self.check_attribute(elem.clone())?;

        let _annot = self.parse_single_annotation_child(elem.clone())?;

        let fixed = match fixed {
            Some(fixed) => match SCHEMA_BUILTIN_BOOLEAN.parse_with_elem(&fixed, elem.clone()) {
                Ok(SchemaValue::Boolean(fixed)) => fixed,
                Ok(_) => internal_error!(self.handler, elem),
                Err(err) => {
                    error!(
                        self.handler,
                        SrcSimpleType,
                        elem,
                        "The attribute 'fixed' of '{}' is failed to parse as boolean because {}.",
                        elem.local_name().as_ref(),
                        err
                    );
                    return Err(XSDParseError::SrcSimpleType);
                }
            },
            None => false,
        };
        let Ok(SchemaValue::Integer(value)) =
            SCHEMA_BUILTIN_NONNEGATIVE_INTEGER.parse_with_elem(&value, elem.clone())
        else {
            todo!("raise error")
        };
        let Ok(value) = usize::try_from(value) else {
            todo!("raise error because of too big positive integer")
        };
        Ok((fixed, value))
    }

    fn parse_enumeration(
        &mut self,
        mut elem: Element,
        base: &SimpleTypeDefinition,
    ) -> Result<SchemaValue, XSDParseError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert!(elem.local_name().as_ref() == SCHEMA_ENUMERATION_NAME);

        let _id = elem.remove_attribute("id", None);
        let Some(value) = elem.remove_attribute("value", None) else {
            todo!("raise error because of missing a mandatory attribute");
        };
        self.check_attribute(elem.clone())?;

        let _annot = self.parse_single_annotation_child(elem.clone())?;

        let Ok(value) = base.parse_with_elem(&value, elem.clone()) else {
            todo!("raise error")
        };
        Ok(value)
    }

    fn parse_whitespace(&mut self, mut elem: Element) -> Result<(bool, Whitespace), XSDParseError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert!(elem.local_name().as_ref() == SCHEMA_WHITESPACE_NAME);

        let fixed = elem.remove_attribute("fixed", None);
        let _id = elem.remove_attribute("id", None);
        let Some(value) = elem.remove_attribute("value", None) else {
            todo!("raise error because of missing a mandatory attribute");
        };
        self.check_attribute(elem.clone())?;

        let _annot = self.parse_single_annotation_child(elem.clone())?;

        let fixed = match fixed {
            Some(fixed) => match SCHEMA_BUILTIN_BOOLEAN.parse_with_elem(&fixed, elem.clone()) {
                Ok(SchemaValue::Boolean(fixed)) => fixed,
                Ok(_) => internal_error!(self.handler, elem),
                Err(err) => {
                    error!(
                        self.handler,
                        SrcSimpleType,
                        elem,
                        "The attribute 'fixed' of '{}' is failed to parse as boolean because {}.",
                        elem.local_name().as_ref(),
                        err
                    );
                    return Err(XSDParseError::SrcSimpleType);
                }
            },
            None => false,
        };
        match value.as_str() {
            "collapse" => Ok((fixed, Whitespace::Collapse)),
            "preserve" => Ok((fixed, Whitespace::Preserve)),
            "replace" => Ok((fixed, Whitespace::Replace)),
            _ => todo!("raise error"),
        }
    }

    fn parse_pattern(&mut self, mut elem: Element) -> Result<String, XSDParseError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert!(elem.local_name().as_ref() == SCHEMA_PATTERN_NAME);

        let _id = elem.remove_attribute("id", None);
        let Some(value) = elem.remove_attribute("value", None) else {
            todo!("raise error because of missing a mandatory attribute");
        };
        self.check_attribute(elem.clone())?;

        let _annot = self.parse_single_annotation_child(elem.clone())?;
        Ok(value)
    }

    fn parse_include(&mut self, mut elem: Element) -> Result<(), XSDParseError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert_eq!(elem.local_name().as_ref(), SCHEMA_INCLUDE_NAME);

        let _id = elem.remove_attribute("id", None);
        let schema_location = match elem.remove_attribute("schemaLocation", None) {
            Some(loc) => match SCHEMA_BUILTIN_ANYURI.parse_with_elem(&loc, elem.clone()) {
                Ok(SchemaValue::AnyURI(uri)) => uri,
                Ok(_) => internal_error!(self.handler, elem),
                Err(err) => {
                    error!(
                        self.handler,
                        SrcInclude,
                        elem,
                        "The attribute 'schemaLocation' of '{}' is failed to parse as anyURI because {}.",
                        elem.local_name().as_ref(),
                        err
                    );
                    return Err(XSDParseError::SrcInclude);
                }
            },
            None => {
                error!(
                    self.handler,
                    SrcInclude,
                    elem,
                    "The mandatory attribute 'schemaLocation' of 'include' is missing."
                );
                return Err(XSDParseError::SrcInclude);
            }
        };

        self.check_attribute(elem.clone())?;

        let _annot = self.parse_single_annotation_child(elem)?;
        todo!()
    }

    fn parse_import(&mut self, mut elem: Element) -> Result<(), XSDParseError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert_eq!(elem.local_name().as_ref(), SCHEMA_IMPORT_NAME);

        let _id = elem.remove_attribute("id", None);
        let schema_location = match elem.remove_attribute("schemaLocation", None) {
            Some(loc) => match SCHEMA_BUILTIN_ANYURI.parse_with_elem(&loc, elem.clone()) {
                Ok(SchemaValue::AnyURI(uri)) => uri,
                Ok(_) => internal_error!(self.handler, elem),
                Err(err) => {
                    error!(
                        self.handler,
                        SrcImport,
                        elem,
                        "The attribute 'schemaLocation' of '{}' is failed to parse as anyURI because {}.",
                        elem.local_name().as_ref(),
                        err
                    );
                    return Err(XSDParseError::SrcImport);
                }
            },
            None => {
                // The absence of the `schemaLocation` attribute is not an error.
                // This simply means that no hint is provided regarding the location
                // of the schema to be imported; since there is nothing the processor
                // can do in this case, it simply ignores it and terminates processing.
                return Ok(());
            }
        };

        self.check_attribute(elem.clone())?;

        let _annot = self.parse_single_annotation_child(elem)?;
        todo!()
    }

    fn parse_redefine(&mut self, mut elem: Element) -> Result<(), XSDParseError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert_eq!(elem.local_name().as_ref(), SCHEMA_REDEFINE_NAME);

        let _id = elem.remove_attribute("id", None);
        let schema_location = match elem.remove_attribute("schemaLocation", None) {
            Some(loc) => match SCHEMA_BUILTIN_ANYURI.parse_with_elem(&loc, elem.clone()) {
                Ok(SchemaValue::AnyURI(uri)) => uri,
                Ok(_) => internal_error!(self.handler, elem),
                Err(err) => {
                    error!(
                        self.handler,
                        SrcRedefine,
                        elem,
                        "The attribute 'schemaLocation' of '{}' is failed to parse as anyURI because {}.",
                        elem.local_name().as_ref(),
                        err
                    );
                    return Err(XSDParseError::SrcRedefine);
                }
            },
            None => {
                error!(
                    self.handler,
                    SrcImport,
                    elem,
                    "The mandatory attribute 'schemaLocation' of 'redefine' is missing."
                );
                return Err(XSDParseError::SrcImport);
            }
        };

        self.check_attribute(elem.clone())?;

        let mut children = elem.first_child();
        while let Some(child) = children {
            children = child.next_sibling();

            match child.downcast() {
                NodeKind::Element(elem) => {
                    if elem.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE) {
                        continue;
                    }

                    match elem.local_name().as_ref() {
                        SCHEMA_ANNOTATION_NAME => {
                            let _annot = self.parse_annotation(elem)?;
                        }
                        SCHEMA_SIMPLETYPE_NAME => todo!(),
                        SCHEMA_COMPLEXTYPE_NAME => todo!(),
                        SCHEMA_GROUP_NAME => todo!(),
                        SCHEMA_ATTGROUP_NAME => todo!(),
                        _ => todo!("raise error"),
                    }
                }
                NodeKind::Text(_) => unallowed_text!(self.handler, elem),
                _ => {}
            }
        }
        todo!()
    }

    fn parse_notation(&mut self, mut elem: Element) -> Result<Notation, XSDParseError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert_eq!(elem.local_name().as_ref(), SCHEMA_NOTATION_NAME);

        let _id = elem.remove_attribute("id", None);
        let Some(name) = elem.remove_attribute("name", None) else {
            todo!("raise error because of mandatory attribute");
        };
        let public = match elem.remove_attribute("public", None) {
            Some(public) => match SCHEMA_BUILTIN_TOKEN.parse_with_elem(&public, elem.clone()) {
                Ok(SchemaValue::String(token)) => Some(token),
                Ok(_) => internal_error!(self.handler, elem),
                Err(err) => {
                    error!(
                        self.handler,
                        SrcNotation,
                        elem,
                        "The attribute 'public' of '{}' is failed to parse as token because {}.",
                        elem.local_name().as_ref(),
                        err
                    );
                    return Err(XSDParseError::SrcNotation);
                }
            },
            None => todo!("raise error"),
        };
        let system = match elem.remove_attribute("system", None) {
            Some(system) => match SCHEMA_BUILTIN_ANYURI.parse_with_elem(&system, elem.clone()) {
                Ok(SchemaValue::AnyURI(uri)) => Some(uri),
                Ok(_) => internal_error!(self.handler, elem),
                Err(err) => {
                    error!(
                        self.handler,
                        SrcNotation,
                        elem,
                        "The attribute 'system' of '{}' is failed to parse as anyURI because {}.",
                        elem.local_name().as_ref(),
                        err
                    );
                    return Err(XSDParseError::SrcNotation);
                }
            },
            None => todo!("raise error"),
        };
        let annot = self.parse_single_annotation_child(elem.clone())?;

        self.check_attribute(elem.clone())?;

        Ok(Notation {
            name,
            target_namespace: self.target_namespace.clone(),
            system_id: system,
            public_id: public,
            annotation: annot,
        })
    }

    fn parse_annotation(&mut self, mut elem: Element) -> Result<Annotation, XSDParseError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert_eq!(elem.local_name().as_ref(), SCHEMA_ANNOTATION_NAME);

        let _id = elem.remove_attribute("id", None);
        self.check_attribute(elem.clone())?;
        elem.set_attribute("id", None, _id.as_deref()).ok();

        let mut appinfo = vec![];
        let mut documentation = vec![];
        let mut children = elem.first_child();
        while let Some(child) = children {
            children = child.next_sibling();

            match child.downcast() {
                NodeKind::Element(elem) => {
                    if elem.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE) {
                        continue;
                    }

                    match elem.local_name().as_ref() {
                        SCHEMA_APPINFO_NAME => {
                            appinfo.push(self.parse_appinfo(elem)?);
                        }
                        SCHEMA_DOCUMENTATION_NAME => {
                            documentation.push(self.parse_documentation(elem)?);
                        }
                        _ => unallowed_element!(self.handler, elem),
                    }
                }
                NodeKind::Text(_) => unallowed_text!(self.handler, elem),
                _ => {}
            }
        }

        Ok(Annotation {
            appinfo,
            documentation,
            attributes: elem.attributes().collect(),
        })
    }

    fn parse_appinfo(&mut self, mut elem: Element) -> Result<Element, XSDParseError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert_eq!(elem.local_name().as_ref(), SCHEMA_APPINFO_NAME);

        let source = elem.remove_attribute("source", None);
        self.check_attribute(elem.clone())?;
        elem.set_attribute("source", None, source.as_deref()).ok();

        Ok(elem)
    }

    fn parse_documentation(&mut self, mut elem: Element) -> Result<Element, XSDParseError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert_eq!(elem.local_name().as_ref(), SCHEMA_APPINFO_NAME);

        let source = elem.remove_attribute("source", None);
        self.check_attribute(elem.clone())?;
        elem.set_attribute("source", None, source.as_deref()).ok();
        Ok(elem)
    }

    /// For sequences that are children of `parent`, treat them as if they should contain
    /// up to one annotation.
    ///
    /// The occurrence of any schema elements or text other than annotations is an error.
    fn parse_single_annotation_child(
        &mut self,
        parent: Element,
    ) -> Result<Option<Annotation>, XSDParseError> {
        let mut children = parent.first_child();
        let ret = self.pop_annotation(&mut children)?;
        self.check_unexpected_schema_elements(children)?;
        Ok(ret)
    }

    fn parse_occurs(
        &mut self,
        mut elem: Element,
        name: &str,
        default: Option<usize>,
        allow_unbounded: bool,
    ) -> Result<Option<usize>, XSDParseError> {
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
                    return Err(XSDParseError::TooLargeOccurence);
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
                Err(XSDParseError::SrcModelGroup)
            }
        }
    }

    fn pop_facet(
        &mut self,
        children: &mut Option<Node<dyn NodeSpec>>,
        base: &SimpleTypeDefinition,
        builder: SimpleTypeDefinitionBuilder,
    ) -> Result<Result<SimpleTypeDefinitionBuilder, SimpleTypeDefinitionBuilder>, XSDParseError>
    {
        while let Some(child) = children.take() {
            *children = child.next_sibling();

            match child.downcast() {
                NodeKind::Element(elem) => {
                    if elem.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE) {
                        continue;
                    }

                    let ret = match elem.local_name().as_ref() {
                        SCHEMA_MINEXCLUSIVE_NAME => {
                            let (_fixed, value) = self.parse_boundary(elem.clone(), base)?;
                            builder.min_exclusive(value)
                        }
                        SCHEMA_MININCLUSIVE_NAME => {
                            let (_fixed, value) = self.parse_boundary(elem.clone(), base)?;
                            builder.min_inclusive(value)
                        }
                        SCHEMA_MAXEXCLUSIVE_NAME => {
                            let (_fixed, value) = self.parse_boundary(elem.clone(), base)?;
                            builder.max_exclusive(value)
                        }
                        SCHEMA_MAXINCLUSIVE_NAME => {
                            let (_fixed, value) = self.parse_boundary(elem.clone(), base)?;
                            builder.max_inclusive(value)
                        }
                        SCHEMA_TOTALDIGITS_NAME => {
                            let (_fixed, value) = self.parse_total_digits(elem.clone())?;
                            builder.total_digits(value)
                        }
                        SCHEMA_FRACDIGITS_NAME => {
                            let (_fixed, value) = self.parse_fraction_digits(elem.clone())?;
                            builder.fraction_digits(value)
                        }
                        SCHEMA_LENGTH_NAME => {
                            let (_fixed, value) = self.parse_length(elem.clone())?;
                            builder.length(value)
                        }
                        SCHEMA_MINLENGTH_NAME => {
                            let (_fixed, value) = self.parse_length(elem.clone())?;
                            builder.min_length(value)
                        }
                        SCHEMA_MAXLENGTH_NAME => {
                            let (_fixed, value) = self.parse_length(elem.clone())?;
                            builder.max_length(value)
                        }
                        SCHEMA_ENUMERATION_NAME => {
                            let enumeration = self.parse_enumeration(elem.clone(), base)?;
                            builder.enumeration(enumeration)
                        }
                        SCHEMA_WHITESPACE_NAME => {
                            let (_fixed, ws) = self.parse_whitespace(elem.clone())?;
                            builder.whitespace(ws)
                        }
                        SCHEMA_PATTERN_NAME => {
                            let value = self.parse_pattern(elem.clone())?;
                            builder.pattern(value)
                        }
                        _ => {
                            *children = Some(child);
                            unallowed_element!(self.handler, elem);
                        }
                    };

                    match ret {
                        Ok(builder) => return Ok(Ok(builder)),
                        Err(err) => {
                            error!(
                                self.handler,
                                SrcSimpleType,
                                elem,
                                "The simple type restriction by '{}' is invalid because {}",
                                elem.local_name(),
                                err
                            );
                            return Err(XSDParseError::SrcSimpleType);
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
        Ok(Err(builder))
    }

    fn pop_annotation(
        &mut self,
        children: &mut Option<Node<dyn NodeSpec>>,
    ) -> Result<Option<Annotation>, XSDParseError> {
        while let Some(child) = children.take() {
            *children = child.next_sibling();

            match child.downcast() {
                NodeKind::Element(elem) => {
                    if elem.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE) {
                        continue;
                    }

                    match elem.local_name().as_ref() {
                        SCHEMA_ANNOTATION_NAME => {
                            let annot = self.parse_annotation(elem)?;
                            return Ok(Some(annot));
                        }
                        _ => {
                            *children = Some(child);
                            return Ok(None);
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
        Ok(None)
    }

    fn pop_attr_decls(
        &mut self,
        children: &mut Option<Node<dyn NodeSpec>>,
        target_namespace: Option<String>,
        loop_guard: &mut Vec<(Option<String>, String)>,
    ) -> Result<(Vec<AttributeUse>, Option<Wildcard>), XSDParseError> {
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
        target_namespace: Option<String>,
    ) -> Result<Option<Particle>, XSDParseError> {
        while let Some(child) = children.take() {
            *children = child.next_sibling();

            match child.downcast() {
                NodeKind::Element(mut elem) => {
                    if elem.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE) {
                        continue;
                    }

                    match elem.local_name().as_ref() {
                        SCHEMA_GROUP_NAME => {
                            let _id = elem.remove_attribute("id", None);
                            let r#ref = elem.remove_attribute("ref", None);
                            let max_occurs = elem.remove_attribute("maxOccurs", None);
                            let min_occurs = elem.remove_attribute("minOccurs", None);

                            self.check_attribute(elem.clone())?;

                            let _annot = self.parse_single_annotation_child(elem)?;
                        }
                        SCHEMA_ALL_NAME => {
                            self.parse_all(elem, target_namespace.clone())?;
                        }
                        SCHEMA_CHOICE_NAME => {
                            self.parse_choice(elem, target_namespace.clone())?;
                        }
                        SCHEMA_SEQUENCE_NAME => {
                            self.parse_sequence(elem, target_namespace.clone())?;
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
        Ok(None)
    }

    fn pop_nested_particle(
        &mut self,
        children: &mut Option<Node<dyn NodeSpec>>,
        target_namespace: Option<String>,
    ) -> Result<Option<Particle>, XSDParseError> {
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
                        SCHEMA_GROUP_NAME => todo!(),
                        SCHEMA_CHOICE_NAME => Ok(Some(self.parse_choice(elem, target_namespace)?)),
                        SCHEMA_SEQUENCE_NAME => {
                            Ok(Some(self.parse_sequence(elem, target_namespace)?))
                        }
                        SCHEMA_ANY_NAME => todo!(),
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

    fn pop_idc(
        &mut self,
        children: &mut Option<Node<dyn NodeSpec>>,
        target_namespace: Option<String>,
    ) -> Result<Option<IDCDefinition>, XSDParseError> {
        while let Some(child) = children.take() {
            *children = child.next_sibling();

            match child.downcast() {
                NodeKind::Element(mut elem) => {
                    if elem.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE) {
                        continue;
                    }

                    let idc_category = match elem.local_name().as_ref() {
                        SCHEMA_UNIQUE_NAME => IDCCategory::Unique,
                        SCHEMA_KEY_NAME => IDCCategory::Key,
                        SCHEMA_KEYREF_NAME => IDCCategory::Keyref,
                        _ => {
                            *children = Some(child);
                            return Ok(None);
                        }
                    };

                    let _id = elem.remove_attribute("id", None);
                    let Some(name) = elem.remove_attribute("name", None) else {
                        todo!("raise error because of missing a mandatory attribute")
                    };
                    let referenced_key = if matches!(idc_category, IDCCategory::Keyref) {
                        if let Some(refer) = elem.remove_attribute("refer", None) {
                            match SCHEMA_BUILTIN_QNAME.parse_with_elem(&refer, elem.clone()) {
                                Ok(SchemaValue::QName(nsname, local)) => {
                                    Some((nsname.map(|n| n.to_string()), local.to_string()))
                                }
                                Ok(_) => internal_error!(self.handler, elem),
                                Err(err) => {
                                    error!(
                                        self.handler,
                                        SrcIdentityConstraint,
                                        elem,
                                        "The attribute 'refer' of '{}' is failed to parse as ID because {}.",
                                        elem.local_name().as_ref(),
                                        err
                                    );
                                    return Err(XSDParseError::SrcIdentityConstraint);
                                }
                            }
                        } else {
                            error!(
                                self.handler,
                                SrcIdentityConstraint,
                                elem.clone(),
                                "The mandatory attribute 'refer' of '{}' is missing.",
                                SCHEMA_KEYREF_NAME
                            );
                            return Err(XSDParseError::SrcIdentityConstraint);
                        }
                    } else {
                        None
                    };

                    self.check_attribute(elem.clone())?;

                    let mut children = elem.first_child();
                    let mut annotation = vec![];
                    if let Some(annot) = self.pop_annotation(&mut children)? {
                        annotation.push(annot);
                    }
                    let Some((selector, annot)) = self.pop_selector(&mut children)? else {
                        error!(
                            self.handler,
                            SrcIdentityConstraint,
                            elem.clone(),
                            "The mandatory element child 'selector' of '{}' is missing.",
                            elem.local_name()
                        );
                        return Err(XSDParseError::SrcIdentityConstraint);
                    };
                    if let Some(annot) = annot {
                        annotation.push(annot);
                    }
                    let mut fields = vec![];
                    while let Some((field, annot)) = self.pop_field(&mut children)? {
                        fields.push(field);
                        if let Some(annot) = annot {
                            annotation.push(annot);
                        }
                    }
                    self.check_unexpected_schema_elements(children)?;

                    return Ok(Some(IDCDefinition {
                        name,
                        target_namespace,
                        idc_category,
                        selector,
                        fields,
                        referenced_key,
                        annotation,
                    }));
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

    fn pop_selector(
        &mut self,
        children: &mut Option<Node<dyn NodeSpec>>,
    ) -> Result<Option<(String, Option<Annotation>)>, XSDParseError> {
        while let Some(child) = children.take() {
            *children = child.next_sibling();

            match child.downcast() {
                NodeKind::Element(mut elem) => {
                    if elem.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE) {
                        continue;
                    }

                    if elem.local_name().as_ref() != SCHEMA_SELECTOR_NAME {
                        *children = Some(child);
                        return Ok(None);
                    }

                    let annot = self.parse_single_annotation_child(elem.clone())?;
                    let _id = elem.remove_attribute("id", None);
                    let Some(xpath) = elem.remove_attribute("xpath", None) else {
                        error!(
                            self.handler,
                            SrcIdentityConstraint,
                            elem.clone(),
                            "The mandatory attribute 'xpath' of '{}' is missing.",
                            elem.local_name()
                        );
                        return Err(XSDParseError::SrcIdentityConstraint);
                    };
                    return Ok(Some((xpath, annot)));
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

    fn pop_field(
        &mut self,
        children: &mut Option<Node<dyn NodeSpec>>,
    ) -> Result<Option<(String, Option<Annotation>)>, XSDParseError> {
        while let Some(child) = children.take() {
            *children = child.next_sibling();

            match child.downcast() {
                NodeKind::Element(mut elem) => {
                    if elem.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE) {
                        continue;
                    }

                    if elem.local_name().as_ref() != SCHEMA_FIELD_NAME {
                        *children = Some(child);
                        return Ok(None);
                    }

                    let annot = self.parse_single_annotation_child(elem.clone())?;
                    let _id = elem.remove_attribute("id", None);
                    let Some(xpath) = elem.remove_attribute("xpath", None) else {
                        error!(
                            self.handler,
                            SrcIdentityConstraint,
                            elem.clone(),
                            "The mandatory attribute 'xpath' of '{}' is missing.",
                            elem.local_name()
                        );
                        return Err(XSDParseError::SrcIdentityConstraint);
                    };
                    return Ok(Some((xpath, annot)));
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
    fn check_attribute(&mut self, elem: Element) -> Result<(), XSDParseError> {
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
            ret = Err(XSDParseError::UnallowedAttribute);
        }
        ret
    }

    fn check_unexpected_schema_elements(
        &mut self,
        mut children: Option<Node<dyn NodeSpec>>,
    ) -> Result<(), XSDParseError> {
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
}

/// Compute "complete wildcard" described on the chapter of "complexType" component
/// derived from `attributeGroup` and `anyAttribute` elements.
///
/// # Reference
/// - [3.4.2 XML Representation of Complex Type Definitions](https://www.w3.org/TR/xmlschema-1/#declare-type)
fn attribute_complete_wildcard(
    wildcards: &[Wildcard],
    local_wildcard: Option<Wildcard>,
) -> Result<Option<Wildcard>, XSDParseError> {
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
    Local(Option<String>, String),
}

struct AttributeDeclaration {
    name: String,
    target_namespace: Option<String>,
    type_definition: Arc<SimpleTypeDefinition>,
    scope: Option<Scope>,
    value_constraint: Option<ValueConstraint>,
    annotation: Option<Annotation>,
}

struct ElementDeclaration {
    name: String,
    target_namespace: Option<String>,
    type_definition: TypeDefinition,
    scope: Option<(Option<String>, String)>,
    value_constraint: Option<ValueConstraint>,
    nillable: bool,
    idc_definitions: Vec<IDCDefinition>,
    substitution_group_affiliation: (),
    substitution_group_exclusions: (),
    disallowed_substitutions: (),
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
pub static SCHEMA_BUILTIN_ANY_TYPE: LazyLock<Arc<ComplexTypeDefinition>> = LazyLock::new(|| {
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
                }],
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

#[derive(Debug, Clone, Copy)]
enum QualificationForm {
    Qualified,
    Unqualified,
}

#[derive(Debug, Clone)]
struct AttributeUse {
    // Although the component is defined as having a Boolean value,
    // it is treated as an enumeration to accommodate the "prohibited" value.
    required: Occurrence,
    attribute_declaration: (Option<String>, String),
    value_constraint: Option<ValueConstraint>,
}

struct AttributeGroupDefinition {
    name: String,
    target_namespace: Option<String>,
    attribute_uses: Vec<AttributeUse>,
    attribute_wildcard: Option<Wildcard>,
    annotation: Option<Annotation>,
}

struct ModelGroupDefinition {
    name: String,
    target_namespace: Option<String>,
    model_group: ModelGroup,
    annotation: Option<Annotation>,
}

#[derive(Debug, Clone, Copy)]
enum Compositor {
    All,
    Choice,
    Sequence,
}

struct ModelGroup {
    compositor: Compositor,
    particles: Vec<Particle>,
    // annotation: Option<Annotation>,
}

enum Term {
    ModelGroup(ModelGroup),
    Wildcard(Wildcard),
    ElementDecl(Option<String>, String),
}

struct Particle {
    min_occurs: usize,
    max_occurs: Option<usize>,
    term: Term,
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
    Not(Option<String>),
    List(Vec<Option<String>>),
}

#[derive(Debug, Clone)]
struct Wildcard {
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
}

#[derive(Debug, Clone, Copy)]
enum IDCCategory {
    Key,
    Keyref,
    Unique,
}

struct IDCDefinition {
    name: String,
    target_namespace: Option<String>,
    idc_category: IDCCategory,
    selector: String,
    fields: Vec<String>,
    referenced_key: Option<(Option<String>, String)>,
    annotation: Vec<Annotation>,
}

struct Notation {
    name: String,
    target_namespace: Option<String>,
    system_id: Option<Arc<URIStr>>,
    public_id: Option<Arc<str>>,
    annotation: Option<Annotation>,
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
