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
    tree::{Attribute, Document, Element, TreeBuildHandler, convert::NodeKind},
    uri::URIStr,
    xsd::{XML_SCHEMA_NAMESPACE, XSDSchema},
    xsdtypes::{
        Final, FinalSet, SCHEMA_BUILTIN_ANY_SIMPLE_TYPE, SCHEMA_BUILTIN_ANYURI,
        SCHEMA_BUILTIN_BOOLEAN, SCHEMA_BUILTIN_ID, SCHEMA_BUILTIN_NCNAME,
        SCHEMA_BUILTIN_NONNEGATIVE_INTEGER, SCHEMA_BUILTIN_POSITIVE_INTEGER, SCHEMA_BUILTIN_QNAME,
        SCHEMA_BUILTIN_TOKEN, SchemaValue, SimpleTypeDefinition, Whitespace,
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
const SCHEMA_ANYATT_NAME: &str = "anyAttribute";
const SCHEMA_EXTENSION_NAME: &str = "extension";

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
        let document = if let Some(handler) = handler {
            let mut parser = XMLReader::builder()
                .set_handler(create_handler(handler))
                .build();
            parser.parse_uri(uri, encoding)?;
            parser.handler.document
        } else {
            let mut parser = XMLReader::builder()
                .set_handler(create_handler(DefaultSAXHandler))
                .build();
            parser.parse_uri(uri, encoding)?;
            parser.handler.document
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
        attr_form_default: QualificationForm::Unqualified,
        elem_form_default: QualificationForm::Unqualified,
        handler,
    };
    context.parse_schema(document)?;
    Ok(context.schema)
}

enum XSDParseError {
    InternalError,
}

struct XSDParseContext<'a, H: SAXHandler> {
    // result schema
    schema: Schema,

    // context
    target_namespace: Option<String>,
    unnamed: usize,

    // qualification form
    attr_form_default: QualificationForm,
    elem_form_default: QualificationForm,

    // handler
    handler: &'a mut H,
}

impl<H: SAXHandler> XSDParseContext<'_, H> {
    fn parse_schema(&mut self, document: Document) -> Result<(), XSDParseError> {
        let Some(mut schema) = document.document_element() else {
            todo!("raise error")
        };
        self.attr_form_default = match schema.remove_attribute("attributeFormDefault", None) {
            Some(form) => {
                let form = form.trim_matches(|c| XMLVersion::XML10.is_whitespace(c));
                match form {
                    "qualified" => QualificationForm::Qualified,
                    "unqualified" => QualificationForm::Unqualified,
                    _ => todo!("raise error"),
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
                    _ => todo!("raise error"),
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
                Ok(_) => todo!("internal error"),
                Err(err) => todo!("raise error because of invalid targetNamespace"),
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
                        _ => {
                            elem.detach().ok();
                            todo!("raise error because of schema namespace element")
                        }
                    }
                }
                NodeKind::Text(mut text) => {
                    text.detach().ok();
                    todo!("raise error because the text cannot be displayed")
                }
                _ => {
                    // PI can be ignored.
                    // Since the settings are configured so that CDATA sections, entity
                    // references, and comments do not appear, they can also be ignored.
                }
            }
        }

        let mut simple_types = HashMap::new();
        let mut complex_types = HashMap::new();
        let mut model_groups = vec![];
        let mut att_groups = HashMap::new();
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
                                    simple_types.insert(
                                        (self.target_namespace.clone(), ncname.to_string()),
                                        elem,
                                    );
                                }
                                Err(err) => todo!("raise error"),
                                Ok(_) => todo!("internal error"),
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
                                    complex_types.insert(
                                        (self.target_namespace.clone(), ncname.to_string()),
                                        elem,
                                    );
                                }
                                Err(err) => todo!("raise error"),
                                Ok(_) => todo!("internal error"),
                            }
                        }
                        SCHEMA_GROUP_NAME => model_groups.push(elem),
                        SCHEMA_ATTGROUP_NAME => {
                            let Some(name) = elem.get_attribute("name", None) else {
                                todo!("raise error")
                            };
                            match SCHEMA_BUILTIN_NCNAME.parse_with_elem(&name, elem.clone()) {
                                Ok(SchemaValue::String(ncname)) => {
                                    att_groups.insert(
                                        (self.target_namespace.clone(), ncname.to_string()),
                                        elem,
                                    );
                                }
                                Ok(_) => todo!("internal error"),
                                Err(err) => todo!("raise error"),
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
                        _ => {
                            elem.detach().ok();
                            todo!("raise error because of schema namespace element")
                        }
                    }
                }
                NodeKind::Text(mut text) => {
                    text.detach().ok();
                    todo!("raise error because the text cannot be displayed")
                }
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
        while let Some((key, value)) = simple_types.iter().next() {
            let key = key.clone();
            let value = value.clone();

            simple_types.remove(&key);
            self.parse_simple_type_definitions(value, key.0, &mut vec![], &mut simple_types, true)?;
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
        while let Some((key, value)) = att_groups.iter().next() {
            let key = key.clone();
            let value = value.clone();

            simple_types.remove(&key);
            self.parse_named_attgroup_definition(value, key.0, &mut att_groups, &mut vec![])?;
        }

        // Finally, resolve everything that remains.
        // Element declarations, complex type definitions, and model group definitions are
        // all interdependent, so it is impossible to resolve any one of them first.
        // Therefore, the only option is to resolve the dependencies recursively.
        // As a starting point, try the complex type definitions first.
        while let Some((key, value)) = complex_types.iter().next() {
            let key = key.clone();
            let value = value.clone();

            complex_types.remove(&key);
            self.parse_complex_type_definitions(
                value,
                key.0,
                &mut vec![],
                &mut complex_types,
                true,
            )?;
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
        let target_namespace = if global {
            target_namespace
        } else {
            if matches!(form, Some(QualificationForm::Qualified))
                || (form.is_none()
                    && matches!(self.attr_form_default, QualificationForm::Qualified))
            {
                target_namespace
            } else {
                None
            }
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
                        todo!("raise error because of [src-attribute]")
                    }
                    name.to_string()
                }
                Ok(_) => todo!("internal error"),
                Err(err) => todo!("raise error"),
            },
            None => {
                if !global {
                    if r#ref.is_none() {
                        // [src-attribute]
                        // 3   If the item's parent is not <schema>,
                        //     then all of the following must be true:
                        //   3.1   One of ref or name must be present, but not both.
                        todo!("raise error because of [src-attribute]")
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
                            todo!("raise error because of [src-attribute]")
                        }
                        Occurrence::Prohibited
                    }
                    "required" => {
                        if default.is_some() {
                            // [src-attribute]
                            // 2 If default and use are both present,
                            // use must have the ·actual value· optional.
                            todo!("raise error because of [src-attribute]")
                        }
                        Occurrence::Required
                    }
                    _ => todo!("raise error"),
                }
            }
            None => Occurrence::Optional,
        };

        self.check_attribute(elem.clone())?;

        let mut annotation = None;
        let mut simple_type = None;
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
                            if annotation.is_some() || simple_type.is_some() {
                                todo!("raise error")
                            }
                            annotation = Some(self.parse_annotation(elem)?);
                        }
                        SCHEMA_SIMPLETYPE_NAME => {
                            if simple_type.is_some() {
                                todo!("raise error")
                            }
                            simple_type = Some(self.parse_simple_type_definitions(
                                elem,
                                target_namespace.clone(),
                                &mut vec![],
                                &mut HashMap::new(),
                                false,
                            )?);
                        }
                        _ => todo!("raise error"),
                    }
                }
                NodeKind::Text(text) => todo!("raise error"),
                _ => {}
            }
        }

        let type_definition = match (r#type, simple_type) {
            (Some(_), Some(_)) => {
                // [src-attribute]
                // 4 type and <simpleType> must not both be present.
                todo!("raise error because of [src-attribute]")
            }
            (Some(r#type), None) => {
                if !global && r#ref.is_some() {
                    // [src-attribute]
                    // 3.2 If ref is present, then all of <simpleType>, form and type must be absent.
                    todo!("raise error because of [src-attribute]")
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
                    Ok(_) => todo!("internal error"),
                    Err(err) => todo!("raise error"),
                }
            }
            (None, Some(def)) => {
                if !global && r#ref.is_some() {
                    // [src-attribute]
                    // 3.2 If ref is present, then all of <simpleType>, form and type must be absent.
                    todo!("raise error because of [src-attribute]")
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
                        Ok(_) => todo!("internal error"),
                        Err(err) => todo!("raise error"),
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
                todo!("raise error because of [src-attribute]")
            }
            (Some(default), None) => {
                match type_definition.parse_with_elem(&default, elem.clone()) {
                    Ok(value) => Some(ValueConstraint::Default(value)),
                    Err(err) => todo!("raise error"),
                }
            }
            (None, Some(fixed)) => match type_definition.parse_with_elem(&fixed, elem.clone()) {
                Ok(value) => Some(ValueConstraint::Fixed(value)),
                Err(err) => todo!("raise error"),
            },
            (None, None) => None,
        };

        if global || r#ref.is_none() {
            let decl = AttributeDeclaration {
                name: name.clone(),
                target_namespace: target_namespace.clone(),
                type_definition,
                scope: None,
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

    /// # Reference
    /// - [src-attribute_group](https://www.w3.org/TR/xmlschema-1/#src-attribute_group)
    fn parse_named_attgroup_definition(
        &mut self,
        mut elem: Element,
        target_namespace: Option<String>,
        memo: &mut HashMap<(Option<String>, String), Element>,
        loop_guard: &mut Vec<(Option<String>, String)>,
    ) -> Result<(), XSDParseError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert_eq!(elem.local_name().as_ref(), SCHEMA_ATTGROUP_NAME);

        let _id = elem.remove_attribute("id", None);
        let name = match elem.remove_attribute("name", None) {
            Some(name) => match SCHEMA_BUILTIN_NCNAME.parse_with_elem(&name, elem.clone()) {
                Ok(SchemaValue::String(ncname)) => ncname.to_string(),
                Ok(_) => todo!("internal error"),
                Err(err) => todo!("raise error"),
            },
            None => todo!("raise error"),
        };

        loop_guard.push((target_namespace.clone(), name.clone()));

        self.check_attribute(elem.clone())?;

        let mut children = elem.first_child();
        let mut annotation = None;
        let mut attribute_uses = vec![];
        // wildcards from attributeGroup
        let mut wildcards = vec![];
        // a wildcard from anyAttribute
        let mut any_attribute = None;
        while let Some(child) = children {
            children = child.next_sibling();

            match child.downcast() {
                NodeKind::Element(elem) => {
                    if elem.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE) {
                        continue;
                    }

                    match elem.local_name().as_ref() {
                        SCHEMA_ANNOTATION_NAME => {
                            if annotation.is_some()
                                || !attribute_uses.is_empty()
                                || any_attribute.is_some()
                            {
                                todo!("raise error")
                            }
                            annotation = Some(self.parse_annotation(elem)?)
                        }
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
                            let r#ref = self.parse_attgroup_reference(elem, memo, loop_guard)?;
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
                        _ => todo!("raise error"),
                    }
                }
                NodeKind::Text(text) => todo!("raise error"),
                _ => {}
            }
        }

        let attribute_wildcard = attribute_complete_wildcard(&wildcards, any_attribute)?;
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
        memo: &mut HashMap<(Option<String>, String), Element>,
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
                Ok(_) => todo!("internal error"),
                Err(err) => todo!("raise error"),
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

        let Some(elem) = memo.remove(&r#ref) else {
            todo!("raise error because of missing definition")
        };
        self.parse_named_attgroup_definition(elem, r#ref.0.clone(), memo, loop_guard)?;
        Ok(r#ref)
    }

    fn parse_simple_type_definitions(
        &mut self,
        mut elem: Element,
        target_namespace: Option<String>,
        loop_guard: &mut Vec<(Option<String>, String)>,
        memo: &mut HashMap<(Option<String>, String), Element>,
        global: bool,
    ) -> Result<Arc<SimpleTypeDefinition>, XSDParseError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert_eq!(elem.local_name().as_ref(), SCHEMA_SIMPLETYPE_NAME);

        let r#final = elem.remove_attribute("final", None);
        let _id = elem.remove_attribute("id", None);
        let name = match elem.remove_attribute("name", None) {
            Some(name) => match SCHEMA_BUILTIN_NCNAME.parse_with_elem(&name, elem.clone()) {
                Ok(SchemaValue::String(ncname)) => ncname.to_string(),
                Ok(_) => todo!("internal error"),
                Err(err) => todo!("raise error"),
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
        while let Some(child) = children {
            children = child.next_sibling();

            match child.downcast() {
                NodeKind::Element(elem) => {
                    if elem.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE) {
                        continue;
                    }

                    if elem.local_name().as_ref() == SCHEMA_ANNOTATION_NAME {
                        self.parse_annotation(elem)?;
                    } else {
                        children = Some(child);
                    }
                    break;
                }
                NodeKind::Text(text) => todo!("raise error"),
                _ => {}
            }
        }
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
                                            SCHEMA_ANNOTATION_NAME => {
                                                self.parse_annotation(elem)?;
                                            }
                                            _ => children = Some(child),
                                        }
                                        break;
                                    }
                                    NodeKind::Text(text) => todo!("raise error"),
                                    _ => {}
                                }
                            }
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
                                    NodeKind::Text(text) => todo!("raise error"),
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
                                            if let Some(elem) = memo.remove(&key) {
                                                self.parse_simple_type_definitions(
                                                    elem,
                                                    key.0.clone(),
                                                    loop_guard,
                                                    memo,
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
                                        Ok(_) => todo!("raise error"),
                                        Err(err) => todo!("raise error"),
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
                                        memo,
                                        false,
                                    )?
                                }
                            };

                            let mut builder = match base.clone().derive_by_restriction() {
                                Ok(builder) => builder,
                                Err(err) => todo!("raise error"),
                            };

                            // extract facets
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
                                            SCHEMA_MINEXCLUSIVE_NAME => {
                                                let (_fixed, value) =
                                                    self.parse_boundary(elem, &base)?;
                                                let Ok(b) = builder.min_exclusive(value) else {
                                                    todo!("raise error")
                                                };
                                                builder = b;
                                            }
                                            SCHEMA_MININCLUSIVE_NAME => {
                                                let (_fixed, value) =
                                                    self.parse_boundary(elem, &base)?;
                                                let Ok(b) = builder.min_inclusive(value) else {
                                                    todo!("raise error")
                                                };
                                                builder = b;
                                            }
                                            SCHEMA_MAXEXCLUSIVE_NAME => {
                                                let (_fixed, value) =
                                                    self.parse_boundary(elem, &base)?;
                                                let Ok(b) = builder.max_exclusive(value) else {
                                                    todo!("raise error")
                                                };
                                                builder = b;
                                            }
                                            SCHEMA_MAXINCLUSIVE_NAME => {
                                                let (_fixed, value) =
                                                    self.parse_boundary(elem, &base)?;
                                                let Ok(b) = builder.max_inclusive(value) else {
                                                    todo!("raise error")
                                                };
                                                builder = b;
                                            }
                                            SCHEMA_TOTALDIGITS_NAME => {
                                                let (_fixed, value) =
                                                    self.parse_total_digits(elem)?;
                                                let Ok(b) = builder.total_digits(value) else {
                                                    todo!("raise error")
                                                };
                                                builder = b;
                                            }
                                            SCHEMA_FRACDIGITS_NAME => {
                                                let (_fixed, value) =
                                                    self.parse_fraction_digits(elem)?;
                                                let Ok(b) = builder.fraction_digits(value) else {
                                                    todo!("raise error")
                                                };
                                                builder = b;
                                            }
                                            SCHEMA_LENGTH_NAME => {
                                                let (_fixed, value) = self.parse_length(elem)?;
                                                let Ok(b) = builder.length(value) else {
                                                    todo!("raise error")
                                                };
                                                builder = b;
                                            }
                                            SCHEMA_MINLENGTH_NAME => {
                                                let (_fixed, value) = self.parse_length(elem)?;
                                                let Ok(b) = builder.min_length(value) else {
                                                    todo!("raise error")
                                                };
                                                builder = b;
                                            }
                                            SCHEMA_MAXLENGTH_NAME => {
                                                let (_fixed, value) = self.parse_length(elem)?;
                                                let Ok(b) = builder.max_length(value) else {
                                                    todo!("raise error")
                                                };
                                                builder = b;
                                            }
                                            SCHEMA_ENUMERATION_NAME => {
                                                let enumeration =
                                                    self.parse_enumeration(elem, &base)?;
                                                let Ok(b) = builder.enumeration(enumeration) else {
                                                    todo!("raise error")
                                                };
                                                builder = b;
                                            }
                                            SCHEMA_WHITESPACE_NAME => {
                                                let (_fixed, ws) = self.parse_whitespace(elem)?;
                                                let Ok(b) = builder.whitespace(ws) else {
                                                    todo!("raise error")
                                                };
                                                builder = b;
                                            }
                                            SCHEMA_PATTERN_NAME => {
                                                let value = self.parse_pattern(elem)?;
                                                let Ok(b) = builder.pattern(value) else {
                                                    todo!("raise error")
                                                };
                                                builder = b;
                                            }
                                            _ => todo!("raise error"),
                                        }
                                    }
                                    NodeKind::Text(text) => todo!("raise error"),
                                    _ => {}
                                }
                            }

                            ret = Some(builder);
                        }
                        SCHEMA_LIST_NAME => {
                            if ret.is_some() {
                                todo!("raise error")
                            }
                            let mut children = elem.first_child();
                            // extract annotation
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
                                            SCHEMA_ANNOTATION_NAME => {
                                                self.parse_annotation(elem)?;
                                            }
                                            _ => children = Some(child),
                                        }
                                        break;
                                    }
                                    NodeKind::Text(text) => todo!("raise error"),
                                    _ => {}
                                }
                            }
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
                                    NodeKind::Text(text) => todo!("raise error"),
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
                                            if let Some(elem) = memo.remove(&key) {
                                                self.parse_simple_type_definitions(
                                                    elem,
                                                    key.0.clone(),
                                                    loop_guard,
                                                    memo,
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
                                        Ok(_) => todo!("raise error"),
                                        Err(err) => todo!("raise error"),
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
                                        memo,
                                        false,
                                    )?
                                }
                            };

                            // Check to see if there are any unexpected children
                            while let Some(child) = children {
                                children = child.next_sibling();

                                match child.downcast() {
                                    NodeKind::Element(elem) => {
                                        if elem.namespace_name().as_deref()
                                            == Some(XML_SCHEMA_NAMESPACE)
                                        {
                                            todo!("raise error")
                                        }
                                    }
                                    NodeKind::Text(text) => todo!("raise error"),
                                    _ => {}
                                }
                            }

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
                                            SCHEMA_ANNOTATION_NAME => {
                                                self.parse_annotation(elem)?;
                                            }
                                            _ => children = Some(child),
                                        }
                                        break;
                                    }
                                    NodeKind::Text(text) => todo!("raise error"),
                                    _ => {}
                                }
                            }
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
                                                    memo,
                                                    false,
                                                )?;
                                                members.push(def);
                                            }
                                            _ => todo!("raise error"),
                                        }
                                    }
                                    NodeKind::Text(text) => todo!("raise error"),
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
                                            if let Some(elem) = memo.remove(&key) {
                                                self.parse_simple_type_definitions(
                                                    elem,
                                                    key.0.clone(),
                                                    loop_guard,
                                                    memo,
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
                                        Ok(_) => todo!("internal error"),
                                        Err(_) => todo!("raise error"),
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
                        _ => todo!("raise error"),
                    }
                }
                NodeKind::Text(text) => todo!("raise error"),
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
        memo: &mut HashMap<(Option<String>, String), Element>,
        global: bool,
    ) -> Result<Arc<SimpleTypeDefinition>, XSDParseError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert_eq!(elem.local_name().as_ref(), SCHEMA_COMPLEXTYPE_NAME);

        let r#abstract = match elem.remove_attribute("abstract", None) {
            Some(abst) => match SCHEMA_BUILTIN_BOOLEAN.parse_with_elem(&abst, elem.clone()) {
                Ok(SchemaValue::Boolean(abst)) => abst,
                Ok(_) => todo!("internal error"),
                Err(err) => todo!("raise erorr"),
            },
            None => false,
        };
        let block = elem.remove_attribute("block", None);
        let r#final = elem.remove_attribute("final", None);
        let _id = elem.remove_attribute("id", None);
        let mixed = match elem.remove_attribute("mixed", None) {
            Some(mixed) => match SCHEMA_BUILTIN_BOOLEAN.parse_with_elem(&mixed, elem.clone()) {
                Ok(SchemaValue::Boolean(mixed)) => mixed,
                Ok(_) => todo!("internal error"),
                Err(err) => todo!("raise erorr"),
            },
            None => false,
        };
        let name = match elem.remove_attribute("name", None) {
            Some(name) => match SCHEMA_BUILTIN_NCNAME.parse_with_elem(&name, elem.clone()) {
                Ok(SchemaValue::String(ncname)) => ncname.to_string(),
                Ok(_) => todo!("internal error"),
                Err(err) => todo!("raise error"),
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
        while let Some(child) = children {
            children = child.next_sibling();

            match child.downcast() {
                NodeKind::Element(elem) => {
                    if elem.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE) {
                        continue;
                    }

                    if elem.local_name().as_ref() == SCHEMA_ANNOTATION_NAME {
                        self.parse_annotation(elem)?;
                    } else {
                        children = Some(child);
                    }
                    break;
                }
                NodeKind::Text(text) => todo!("raise error"),
                _ => {}
            }
        }
        // simpleContent
        // | complexContent
        // | ((group | all | choice | sequence)?, (attribute | attributeGroup)*, anyAttribute?)
        // extract content model
        let mut ret = None;
        while let Some(child) = children {
            children = child.next_sibling();

            match child.downcast() {
                NodeKind::Element(mut elem) => {
                    if elem.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE) {
                        continue;
                    }

                    match elem.local_name().as_ref() {
                        SCHEMA_SIMPLECONT_NAME => {
                            self.parse_simple_content(
                                elem,
                                target_namespace.clone(),
                                loop_guard,
                                memo,
                            )?;
                        }
                        SCHEMA_COMPLEXCONT_NAME => {
                            self.parse_complex_content(
                                elem,
                                target_namespace.clone(),
                                loop_guard,
                                memo,
                                mixed,
                            )?;
                        }
                        _ => todo!("raise error"),
                    }
                }
                NodeKind::Text(text) => todo!("raise error"),
                _ => {}
            }
        }

        loop_guard.pop();
        todo!()
    }

    fn parse_simple_content(
        &mut self,
        mut elem: Element,
        target_namespace: Option<String>,
        loop_guard: &mut Vec<(Option<String>, String)>,
        memo: &mut HashMap<(Option<String>, String), Element>,
    ) -> Result<(), XSDParseError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert_eq!(elem.local_name().as_ref(), SCHEMA_SIMPLECONT_NAME);

        let _id = elem.remove_attribute("id", None);
        self.check_attribute(elem.clone())?;

        let mut children = elem.first_child();
        // extract annotation
        while let Some(child) = children {
            children = child.next_sibling();

            match child.downcast() {
                NodeKind::Element(elem) => {
                    if elem.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE) {
                        continue;
                    }

                    if elem.local_name().as_ref() == SCHEMA_ANNOTATION_NAME {
                        self.parse_annotation(elem)?;
                    } else {
                        children = Some(child);
                    }
                    break;
                }
                NodeKind::Text(text) => todo!("raise error"),
                _ => {}
            }
        }
        // extract derivation method
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
                                            SCHEMA_ANNOTATION_NAME => {
                                                self.parse_annotation(elem)?;
                                            }
                                            _ => children = Some(child),
                                        }
                                        break;
                                    }
                                    NodeKind::Text(text) => todo!("raise error"),
                                    _ => {}
                                }
                            }
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
                                    NodeKind::Text(text) => todo!("raise error"),
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
                                            if let Some(elem) = memo.remove(&key) {
                                                self.parse_complex_type_definitions(
                                                    elem,
                                                    key.0.clone(),
                                                    loop_guard,
                                                    memo,
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
                                                            ContentType::Mixed => {
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
                                        Ok(_) => todo!("internal error"),
                                        Err(err) => todo!("raise error"),
                                    }
                                }
                                None => todo!("raise error"),
                            };

                            // extract facets
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
                                            SCHEMA_MINEXCLUSIVE_NAME => {
                                                let (_fixed, value) =
                                                    self.parse_boundary(elem, &base)?;
                                                let Ok(b) = builder.min_exclusive(value) else {
                                                    todo!("raise error")
                                                };
                                                builder = b;
                                            }
                                            SCHEMA_MININCLUSIVE_NAME => {
                                                let (_fixed, value) =
                                                    self.parse_boundary(elem, &base)?;
                                                let Ok(b) = builder.min_inclusive(value) else {
                                                    todo!("raise error")
                                                };
                                                builder = b;
                                            }
                                            SCHEMA_MAXEXCLUSIVE_NAME => {
                                                let (_fixed, value) =
                                                    self.parse_boundary(elem, &base)?;
                                                let Ok(b) = builder.max_exclusive(value) else {
                                                    todo!("raise error")
                                                };
                                                builder = b;
                                            }
                                            SCHEMA_MAXINCLUSIVE_NAME => {
                                                let (_fixed, value) =
                                                    self.parse_boundary(elem, &base)?;
                                                let Ok(b) = builder.max_inclusive(value) else {
                                                    todo!("raise error")
                                                };
                                                builder = b;
                                            }
                                            SCHEMA_TOTALDIGITS_NAME => {
                                                let (_fixed, value) =
                                                    self.parse_total_digits(elem)?;
                                                let Ok(b) = builder.total_digits(value) else {
                                                    todo!("raise error")
                                                };
                                                builder = b;
                                            }
                                            SCHEMA_FRACDIGITS_NAME => {
                                                let (_fixed, value) =
                                                    self.parse_fraction_digits(elem)?;
                                                let Ok(b) = builder.fraction_digits(value) else {
                                                    todo!("raise error")
                                                };
                                                builder = b;
                                            }
                                            SCHEMA_LENGTH_NAME => {
                                                let (_fixed, value) = self.parse_length(elem)?;
                                                let Ok(b) = builder.length(value) else {
                                                    todo!("raise error")
                                                };
                                                builder = b;
                                            }
                                            SCHEMA_MINLENGTH_NAME => {
                                                let (_fixed, value) = self.parse_length(elem)?;
                                                let Ok(b) = builder.min_length(value) else {
                                                    todo!("raise error")
                                                };
                                                builder = b;
                                            }
                                            SCHEMA_MAXLENGTH_NAME => {
                                                let (_fixed, value) = self.parse_length(elem)?;
                                                let Ok(b) = builder.max_length(value) else {
                                                    todo!("raise error")
                                                };
                                                builder = b;
                                            }
                                            SCHEMA_ENUMERATION_NAME => {
                                                let enumeration =
                                                    self.parse_enumeration(elem, &base)?;
                                                let Ok(b) = builder.enumeration(enumeration) else {
                                                    todo!("raise error")
                                                };
                                                builder = b;
                                            }
                                            SCHEMA_WHITESPACE_NAME => {
                                                let (_fixed, ws) = self.parse_whitespace(elem)?;
                                                let Ok(b) = builder.whitespace(ws) else {
                                                    todo!("raise error")
                                                };
                                                builder = b;
                                            }
                                            SCHEMA_PATTERN_NAME => {
                                                let value = self.parse_pattern(elem)?;
                                                let Ok(b) = builder.pattern(value) else {
                                                    todo!("raise error")
                                                };
                                                builder = b;
                                            }
                                            _ => {
                                                children = Some(child);
                                                break;
                                            }
                                        }
                                    }
                                    NodeKind::Text(text) => todo!("raise error"),
                                    _ => {}
                                }
                            }

                            // extract attributes, attributeGroups or anyAttribute
                            let mut attribute_wildcard = None;
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
                                            SCHEMA_ATTRIBUTE_NAME => todo!(),
                                            SCHEMA_ATTGROUP_NAME => todo!(),
                                            SCHEMA_ANYATT_NAME => {
                                                attribute_wildcard =
                                                    Some(self.parse_any_attribute(
                                                        elem,
                                                        target_namespace.clone(),
                                                    )?);
                                                break;
                                            }
                                            _ => todo!("raise error"),
                                        }
                                    }
                                    NodeKind::Text(text) => todo!("raise error"),
                                    _ => {}
                                }
                            }
                        }
                        SCHEMA_EXTENSION_NAME => {
                            let mut children = elem.first_child();
                            // extract annotation
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
                                            SCHEMA_ANNOTATION_NAME => {
                                                self.parse_annotation(elem)?;
                                            }
                                            _ => children = Some(child),
                                        }
                                        break;
                                    }
                                    NodeKind::Text(text) => todo!("raise error"),
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
                                            if let Some(elem) = memo.remove(&key) {
                                                self.parse_complex_type_definitions(
                                                    elem,
                                                    key.0.clone(),
                                                    loop_guard,
                                                    memo,
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
                                        Ok(_) => todo!("internal error"),
                                        Err(err) => todo!("raise error"),
                                    }
                                }
                                None => todo!("raise error"),
                            };

                            // extract attributes or attribute Groups or anyAttribute
                            while let Some(child) = children {
                                children = child.next_sibling();

                                match child.downcast() {
                                    NodeKind::Element(mut elem) => {
                                        if elem.namespace_name().as_deref()
                                            != Some(XML_SCHEMA_NAMESPACE)
                                        {
                                            continue;
                                        }

                                        match elem.local_name().as_ref() {
                                            SCHEMA_ATTRIBUTE_NAME => todo!(),
                                            SCHEMA_ATTGROUP_NAME => todo!(),
                                            SCHEMA_ANYATT_NAME => todo!(),
                                            _ => todo!("raise error"),
                                        }
                                    }
                                    NodeKind::Text(text) => todo!("raise error"),
                                    _ => {}
                                }
                            }

                            // report unexpected schema element
                            while let Some(child) = children {
                                children = child.next_sibling();
                                match child.downcast() {
                                    NodeKind::Element(elem) => {
                                        if elem.namespace_name().as_deref()
                                            == Some(XML_SCHEMA_NAMESPACE)
                                        {
                                            todo!("raise error")
                                        }
                                    }
                                    NodeKind::Text(text) => todo!("raise error"),
                                    _ => {}
                                }
                            }

                            ret = Some(builder);
                        }
                        _ => todo!("raise error"),
                    }
                }
                NodeKind::Text(text) => todo!("raise error"),
                _ => {}
            }
        }
        todo!()
    }

    fn parse_complex_content(
        &mut self,
        mut elem: Element,
        target_namespace: Option<String>,
        loop_guard: &mut Vec<(Option<String>, String)>,
        memo: &mut HashMap<(Option<String>, String), Element>,
        mixed: bool,
    ) -> Result<(), XSDParseError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert_eq!(elem.local_name().as_ref(), SCHEMA_COMPLEXCONT_NAME);

        let _id = elem.remove_attribute("id", None);
        let mixed = match elem.remove_attribute("mixed", None) {
            Some(mixed) => match SCHEMA_BUILTIN_BOOLEAN.parse_with_elem(&mixed, elem.clone()) {
                Ok(SchemaValue::Boolean(mixed)) => mixed,
                Ok(_) => todo!("internal error"),
                Err(err) => todo!("raise erorr"),
            },
            None => mixed,
        };

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
                    NamespaceConstraint::Other
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
                                Ok(_) => todo!("internal error"),
                                Err(err) => todo!("raise error"),
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
                Ok(_) => todo!("internal error"),
                Err(err) => todo!("raise error"),
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
                Ok(_) => todo!("internal error"),
                Err(err) => todo!("raise error"),
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
            todo!("internal error")
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
                Ok(_) => todo!("internal error"),
                Err(err) => todo!("raise error"),
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
                Ok(_) => todo!("internal error"),
                Err(err) => todo!("raise error"),
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
                Ok(_) => todo!("internal error"),
                Err(err) => todo!("raise error"),
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
                Ok(_) => todo!("internal error"),
                Err(err) => todo!("raise error because of invalid anyURI"),
            },
            None => todo!("raise error because of missing a mandatory attribute"),
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
                Ok(_) => todo!("internal error"),
                Err(err) => todo!("raise error because of invalid anyURI"),
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
                Ok(_) => todo!("internal error"),
                Err(err) => todo!("raise error because of invalid anyURI"),
            },
            None => todo!("raise error because of missing a mandatory attribute"),
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
                NodeKind::Text(text) => todo!("raise error"),
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
                Ok(_) => todo!("internal error"),
                Err(err) => todo!("raise error"),
            },
            None => todo!("raise error"),
        };
        let system = match elem.remove_attribute("system", None) {
            Some(system) => match SCHEMA_BUILTIN_ANYURI.parse_with_elem(&system, elem.clone()) {
                Ok(SchemaValue::AnyURI(uri)) => Some(uri),
                Ok(_) => todo!("internal error"),
                Err(err) => todo!("raise error"),
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
                        _ => {
                            todo!("raise error")
                        }
                    }
                }
                NodeKind::Text(text) => todo!("raise error"),
                _ => {}
            }
        }

        let atts = elem
            .attributes()
            .filter(|a| {
                a.namespace()
                    .is_none_or(|ns| ns.namespace_name().as_ref() == XML_SCHEMA_NAMESPACE)
            })
            .collect::<Vec<_>>();
        for mut att in atts {
            if att.namespace_name().is_none() && att.local_name().as_ref() == "id" {
                let value = att.value();
                match SCHEMA_BUILTIN_ID.parse_with_elem(&value, elem.clone()) {
                    Ok(SchemaValue::String(id)) => {
                        att.set_value(&id).ok();
                    }
                    Ok(_) => todo!("internal error"),
                    Err(err) => {
                        todo!("raise error");
                    }
                }
                continue;
            }
            elem.remove_attribute_node(att).ok();
            todo!("raise error because of null or schema namespace.")
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

        let atts = elem
            .attributes()
            .filter(|a| {
                a.namespace()
                    .is_none_or(|ns| ns.namespace_name().as_ref() == XML_SCHEMA_NAMESPACE)
            })
            .collect::<Vec<_>>();
        for mut att in atts {
            if att.namespace_name().is_none() && att.local_name().as_ref() == "source" {
                let value = att.value();
                match SCHEMA_BUILTIN_ANYURI.parse_with_elem(&value, elem.clone()) {
                    Ok(SchemaValue::AnyURI(uri)) => {
                        att.set_value(&uri.to_string()).ok();
                    }
                    Ok(_) => todo!("internal error"),
                    Err(err) => {
                        todo!("raise error");
                    }
                }
                continue;
            }
            elem.remove_attribute_node(att).ok();
            todo!("raise error because of null or schema namespace.")
        }
        Ok(elem)
    }

    fn parse_documentation(&mut self, mut elem: Element) -> Result<Element, XSDParseError> {
        assert_eq!(elem.namespace_name().as_deref(), Some(XML_SCHEMA_NAMESPACE));
        assert_eq!(elem.local_name().as_ref(), SCHEMA_APPINFO_NAME);

        let atts = elem
            .attributes()
            .filter(|a| {
                a.namespace()
                    .is_none_or(|ns| ns.namespace_name().as_ref() == XML_SCHEMA_NAMESPACE)
            })
            .collect::<Vec<_>>();
        for mut att in atts {
            if att.namespace_name().is_none() && att.local_name().as_ref() == "source" {
                let value = att.value();
                match SCHEMA_BUILTIN_ANYURI.parse_with_elem(&value, elem.clone()) {
                    Ok(SchemaValue::AnyURI(uri)) => {
                        att.set_value(&uri.to_string()).ok();
                    }
                    Ok(_) => todo!("internal error"),
                    Err(err) => {
                        todo!("raise error");
                    }
                }
                continue;
            }
            elem.remove_attribute_node(att).ok();
            todo!("raise error because of null or schema namespace.")
        }
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
        let mut ret = None;

        let mut annot_count = 0;
        let mut children = parent.first_child();
        while let Some(child) = children {
            children = child.next_sibling();

            match child.downcast() {
                NodeKind::Element(elem) => {
                    if elem.namespace_name().as_deref() != Some(XML_SCHEMA_NAMESPACE) {
                        continue;
                    }

                    match elem.local_name().as_ref() {
                        SCHEMA_ANNOTATION_NAME => {
                            if annot_count > 0 {
                                todo!("raise error")
                            }
                            annot_count += 1;
                            ret = Some(self.parse_annotation(elem)?);
                        }
                        _ => {
                            todo!("raise error")
                        }
                    }
                }
                NodeKind::Text(text) => todo!("raise error"),
                _ => {}
            }
        }
        Ok(ret)
    }

    /// Raise an errors for attributes whose namespace is NULL or is the XSD namespace.
    ///
    /// Non-foreign attributes for which an error should not be raised must be removed
    /// before calling this method.
    fn check_attribute(&mut self, elem: Element) -> Result<(), XSDParseError> {
        for att in elem.attributes().filter(|a| {
            a.namespace_name()
                .is_none_or(|ns| &*ns == XML_SCHEMA_NAMESPACE)
        }) {
            todo!("raise error because of null or non-schema namespace attribute")
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
            .fold(Some(local), |l, r| l.and_then(|l| l.intersection(r)))
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
            .fold(Some(top), |l, r| l.and_then(|l| l.intersection(r)))
        {
            Ok(Some(wildcard))
        } else {
            todo!("raise error because wildcards' intersection is not expressible")
        }
    }
}

struct AttributeDeclaration {
    name: String,
    target_namespace: Option<String>,
    type_definition: Arc<SimpleTypeDefinition>,
    scope: Option<(Option<String>, String)>,
    value_constraint: Option<ValueConstraint>,
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
    Mixed,
    ElementOnly,
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
    attribute_uses: (),
    attribute_wildcard: Option<Wildcard>,
    content_type: ContentType,
    prohibited_substitutions: (),
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
        attribute_uses: (),
        attribute_wildcard: Some(Wildcard {
            ns_constraint: NamespaceConstraint::Any,
            process_contents: ProcessContent::Lax,
        }),
        content_type: (),
        prohibited_substitutions: (),
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
    model_group: (),
    annotation: Option<Annotation>,
}

struct ModelGroup {
    compositor: (),
    particles: (),
    annotation: Option<Annotation>,
}

struct Particle {
    min_occurs: usize,
    max_occurs: Option<usize>,
    term: (),
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

struct IDCDefinition {
    name: String,
    target_namespace: Option<String>,
    idc_category: (),
    selector: String,
    fields: Vec<String>,
    referenced_key: (),
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
