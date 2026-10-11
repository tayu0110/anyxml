use std::{borrow::Cow, sync::Arc};

use crate::{
    XMLVersion,
    error::{XMLError, XMLErrorDomain, XMLErrorLevel},
    sax::{
        AttributeType, Attributes, ContentSpec, DefaultDecl, EntityResolver, ErrorHandler,
        InputSource, Locator, NamespaceStack, SAXHandler, error::SAXParseError,
    },
    uri::{URIStr, URIString},
    xsd::{Schema, XML_SCHEMA_INSTANCE_NAMESPACE, XSDError, XSDSchema},
};

pub struct ValidateHandler<'a, H: SAXHandler + ?Sized> {
    schema: &'a mut Schema,
    locator: Arc<Locator>,

    // context
    depth: usize,
    skip_depth: usize,
    ns_stack: NamespaceStack,

    // flags
    /// If true, this validator will recognize and use schema location hints
    /// (e.g. `xsi:schemaLocation` and `xsi:noNamespaceSchemaLocation`).
    pub use_schema_location_hint: bool,

    // child handler
    pub child: H,
}

impl<'a, H: SAXHandler> ValidateHandler<'a, H> {
    pub(super) fn new(schema: &'a mut Schema, child: H) -> Self {
        ValidateHandler {
            schema,
            locator: Arc::new(Locator::default()),
            depth: 0,
            skip_depth: 0,
            ns_stack: NamespaceStack::default(),
            use_schema_location_hint: false,
            child,
        }
    }
}

impl<'a, H: SAXHandler + ?Sized> ValidateHandler<'a, H> {
    fn parse_qname(&self, value: &str) -> Result<(Option<Arc<str>>, Arc<str>), XSDError> {
        if !XMLVersion::XML10.validate_qname(value) {
            return Err(XSDError::CvcSimpleType);
        }
        if let Some((pre, loc)) = value.split_once(':') {
            let namespace = self.ns_stack.get(pre).ok_or(XSDError::CvcSimpleType)?;
            Ok((Some(namespace.namespace_name.clone()), loc.into()))
        } else if let Some(namespace) = self.ns_stack.get("") {
            Ok((Some(namespace.namespace_name.clone()), value.into()))
        } else {
            Ok((None, value.into()))
        }
    }

    fn handle_schema_location_hint(&mut self, atts: &Attributes) {
        if let Some(hint) =
            atts.get_value_by_expanded_name(Some(XML_SCHEMA_INSTANCE_NAMESPACE), "schemaLocation")
        {
            let hint = hint
                .split(|c| XMLVersion::XML10.is_whitespace(c))
                .filter(|s| !s.is_empty())
                .map(URIString::parse)
                .collect::<Result<Vec<_>, _>>();
            match hint {
                Ok(hint) if hint.len() % 2 == 0 => {
                    for chunk in hint.chunks_exact(2) {
                        let hint = self.locator.system_id().resolve(&chunk[1]);
                        match XSDSchema::parse_uri(&hint, None, Some(&mut self.child)) {
                            Ok(schema) => self.schema.extend(schema.schema),
                            Err(error) => {
                                let msg = format!(
                                    "The schema '{}' provided 'schemaLocation' cannot be parsed because '{}'.",
                                    hint, error
                                );
                                self.error(SAXParseError {
                                    error,
                                    level: XMLErrorLevel::Error,
                                    domain: XMLErrorDomain::XSDValid,
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
                Ok(hint) => {
                    let msg = format!(
                        "'schemaLocation' must be a sequence of anyURIs of even length, but the length is {}.",
                        hint.len()
                    );
                    self.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::CvcAttribute),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDValid,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                }
                Err(_) => {
                    let msg = "'schemaLocation' must be a sequence of anyURIs, but non-anyURI values are contained.";
                    self.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::CvcAttribute),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDValid,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Borrowed(msg),
                    });
                }
            }
        }
        if let Some(hint) = atts.get_value_by_expanded_name(
            Some(XML_SCHEMA_INSTANCE_NAMESPACE),
            "noNamespaceSchemaLocation",
        ) {
            let hint = hint.trim_matches(|c| XMLVersion::XML10.is_whitespace(c));
            match URIString::parse(hint) {
                Ok(hint) => {
                    let hint = self.locator.system_id().resolve(&hint);
                    match XSDSchema::parse_uri(&hint, None, Some(&mut self.child)) {
                        Ok(schema) => self.schema.extend(schema.schema),
                        Err(error) => {
                            let msg = format!(
                                "The schema '{}' provided 'noNamespaceSchemaLocation' cannot be parsed because '{}'.",
                                hint, error
                            );
                            self.error(SAXParseError {
                                error,
                                level: XMLErrorLevel::Error,
                                domain: XMLErrorDomain::XSDValid,
                                line: self.locator.line(),
                                column: self.locator.column(),
                                system_id: self.locator.system_id(),
                                public_id: self.locator.public_id(),
                                message: Cow::Owned(msg),
                            });
                        }
                    }
                }
                Err(err) => {
                    let msg = format!(
                        "'noNamespaceSchemaLocation' msut be anyURI but '{}' is not because '{}'.",
                        hint, err
                    );
                    self.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::CvcAttribute),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDValid,
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
}

impl<H: SAXHandler + ?Sized> EntityResolver for ValidateHandler<'_, H> {
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

impl<H: SAXHandler + ?Sized> ErrorHandler for ValidateHandler<'_, H> {
    fn fatal_error(&mut self, error: SAXParseError) {
        self.child.fatal_error(error);
    }
    fn error(&mut self, error: SAXParseError) {
        self.child.error(error);
    }
    fn warning(&mut self, error: SAXParseError) {
        self.child.warning(error);
    }
}

impl<H: SAXHandler + ?Sized> SAXHandler for ValidateHandler<'_, H> {
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
        self.depth += 1;
        if self.skip_depth > 0 {
            self.child
                .start_element(namespace_name, local_name, qname, atts);
            self.skip_depth += 1;
            return;
        }
        if local_name.is_none() {
            self.error(SAXParseError {
                error: XMLError::XSDError(XSDError::Unsupported),
                level: XMLErrorLevel::Error,
                domain: XMLErrorDomain::XSDValid,
                line: self.locator.line(),
                column: self.locator.column(),
                system_id: self.locator.system_id(),
                public_id: self.locator.public_id(),
                message: Cow::Borrowed(
                    "Namespace feature must be enabled for XSD validator of this crate.",
                ),
            });
            self.child
                .start_element(namespace_name, local_name, qname, atts);
            self.skip_depth += 1;
            return;
        }

        let local_name = local_name.unwrap();

        // resolve schema location hint
        if self.use_schema_location_hint {
            self.handle_schema_location_hint(atts);
        }

        if self.depth == 1 {
            // root element
            let mut root_type = None;

            if let Some(xsi_type) =
                atts.get_value_by_expanded_name(Some(XML_SCHEMA_INSTANCE_NAMESPACE), "type")
            {
                // 'xsi:type' handling.
                let xsi_type = xsi_type.trim_matches(|c| XMLVersion::XML10.is_whitespace(c));
                match self.parse_qname(xsi_type) {
                    Ok((ns, loc)) => {
                        root_type = Some((ns, loc));
                    }
                    Err(_) => {
                        // [cvc-elt] clause 4.1
                        let msg = format!(
                            "'xsi:type' must be QName, but '{}' cannot be parsed as QName at this context.",
                            xsi_type
                        );
                        self.error(SAXParseError {
                            error: XMLError::XSDError(XSDError::CvcElt),
                            level: XMLErrorLevel::Error,
                            domain: XMLErrorDomain::XSDValid,
                            line: self.locator.line(),
                            column: self.locator.column(),
                            system_id: self.locator.system_id(),
                            public_id: self.locator.public_id(),
                            message: Cow::Owned(msg),
                        });
                    }
                }
            }

            if root_type.is_none() {
                let Some(decl) = self
                    .schema
                    .elem_decls
                    .get(&(namespace_name.map(From::from), local_name.into()))
                else {
                    // [cvc-elt] clause 1
                    let msg = format!(
                        "No element declarations for '{}{}' are found.",
                        namespace_name
                            .map(|ns| format!("{{{ns}}}"))
                            .unwrap_or_default(),
                        local_name
                    );
                    self.error(SAXParseError {
                        error: XMLError::XSDError(XSDError::CvcElt),
                        level: XMLErrorLevel::Error,
                        domain: XMLErrorDomain::XSDValid,
                        line: self.locator.line(),
                        column: self.locator.column(),
                        system_id: self.locator.system_id(),
                        public_id: self.locator.public_id(),
                        message: Cow::Owned(msg),
                    });
                    self.skip_depth += 1;
                    self.child
                        .start_element(namespace_name, Some(local_name), qname, atts);
                    return;
                };
            }
        }

        self.child
            .start_element(namespace_name, Some(local_name), qname, atts);
    }

    fn end_element(&mut self, namespace_name: Option<&str>, local_name: Option<&str>, qname: &str) {
        self.depth -= 1;
        if self.skip_depth > 0 {
            self.child.end_element(namespace_name, local_name, qname);
            self.skip_depth -= 1;
            return;
        }
        self.child.end_element(namespace_name, local_name, qname);
    }

    fn characters(&mut self, data: &str) {
        self.child.characters(data);
    }

    fn start_prefix_mapping(&mut self, prefix: Option<&str>, namespace_name: &str) {
        self.ns_stack
            .push(prefix.unwrap_or_default(), namespace_name);
        self.child.start_prefix_mapping(prefix, namespace_name);
    }

    fn end_prefix_mapping(&mut self, prefix: Option<&str>) {
        self.ns_stack.pop();
        self.child.end_prefix_mapping(prefix);
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
    fn element_decl(&mut self, name: &str, contentspec: &ContentSpec) {
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
