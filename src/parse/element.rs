use std::sync::Arc;

use anyxml_uri::uri::URIString;

use crate::{
    ParserSpec, XML_NS_NAMESPACE, XML_XML_NAMESPACE, XMLVersion,
    error::XMLError,
    sax::{
        AttributeType, DefaultDecl, EntityDecl,
        attributes::Attribute,
        error::{fatal_error, ns_error, validity_error},
        handler::SAXHandler,
        parser::{ParserOption, XMLReader},
        source::InputSource,
    },
};

impl<'a, Spec: ParserSpec<Reader = InputSource<'a>>, H: SAXHandler> XMLReader<Spec, H> {
    /// ```text
    /// [39] element ::= EmptyElemTag | STag content ETag       [WFC: Element Type Match]
    ///                                                         [VC:  Element Valid]
    /// [40] STag ::= '<' Name (S Attribute)* S? '>'            [WFC: Unique Att Spec]
    /// [42] ETag ::= '</' Name S? '>'
    /// [44] EmptyElemTag ::= '<' Name (S Attribute)* S? '/>'   [WFC: Unique Att Spec]
    /// ```
    pub(crate) fn parse_element(&mut self) -> Result<(), XMLError> {
        let old_ns_stack_depth = self.namespaces.len();
        let mut name = String::new();
        let mut prefix_length = 0;
        let empty = self.parse_start_or_empty_tag(&mut name, &mut prefix_length)?;

        if !empty {
            self.parse_content()?;
            self.grow()?;

            // parse end tag

            if !self.source.content_bytes().starts_with(b"</") {
                fatal_error!(
                    self,
                    ParserInvalidEndTag,
                    "'</' is not found at the head of the end tag."
                );
                return Err(XMLError::ParserInvalidEndTag);
            }
            // skip '</'
            self.source.advance(2)?;
            self.locator.update_column(|c| c + 2);

            let mut end_tag_name = String::new();
            if self.config.is_enable(ParserOption::Namespaces) {
                self.parse_qname(&mut end_tag_name)?;
            } else {
                self.parse_name(&mut end_tag_name)?;
            }

            if name != end_tag_name {
                let name = if name.chars().count() > 15 {
                    format!("{}...", name.chars().take(12).collect::<String>())
                } else {
                    name
                };
                let end_tag_name = if end_tag_name.chars().count() > 15 {
                    format!("{}...", end_tag_name.chars().take(12).collect::<String>())
                } else {
                    end_tag_name
                };
                fatal_error!(
                    self,
                    ParserMismatchElementType,
                    "The start tag ('{}') and end tag ('{}') names do not match.",
                    name,
                    end_tag_name
                );
                return Err(XMLError::ParserMismatchElementType);
            }

            self.skip_whitespaces()?;
            self.grow()?;

            if !self.source.content_bytes().starts_with(b">") {
                fatal_error!(
                    self,
                    ParserInvalidEndTag,
                    "The end tag does not end with '>'."
                );
                return Err(XMLError::ParserInvalidEndTag);
            }
            // skip '>'
            self.source.advance(1)?;
            self.locator.update_column(|c| c + 1);
        }

        if !self.fatal_error_occurred {
            if self.config.is_enable(ParserOption::Namespaces) {
                if prefix_length > 0 {
                    if let Some(&pos) = self.prefix_map.get(&name[..prefix_length]) {
                        self.handler.end_element(
                            Some(&self.namespaces[pos].0),
                            Some(&name[prefix_length + 1..]),
                            &name,
                        );
                    } else {
                        ns_error!(
                            self,
                            ParserUndefinedNamespace,
                            "The prefix '{}' is not bind to any namespaces.",
                            &name[..prefix_length]
                        );
                    }
                } else {
                    // default namespace
                    if let Some(&pos) = self.prefix_map.get("") {
                        self.handler
                            .end_element(Some(&self.namespaces[pos].0), Some(&name), &name);
                    } else {
                        self.handler.end_element(None, Some(&name), &name);
                    }
                }
            } else {
                self.handler.end_element(None, None, &name);
            }
        }

        // resume namespace stack
        while self.namespaces.len() > old_ns_stack_depth {
            let (pre, _, old_position) = self.namespaces.pop().unwrap();
            if !self.fatal_error_occurred {
                self.handler
                    .end_prefix_mapping((!pre.is_empty()).then_some(pre.as_ref()));
            }

            if old_position < usize::MAX {
                *self.prefix_map.get_mut(&pre).unwrap() = old_position;
            }
        }

        if self.config.is_enable(ParserOption::Validation)
            && let Some(Some((context_name, mut validator))) = self.validation_stack.pop()
            && !self.fatal_error_occurred
        {
            assert_eq!(context_name.as_ref(), name);
            if validator.is_external_element_content()
                && validator.whitespace()
                && self.standalone == Some(true)
            {
                // [VC: Standalone Document Declaration]
                validity_error!(
                    self,
                    ParserInvalidStandaloneDocument,
                    "standalone='yes', but the element '{}' containing whitespace is declared to have element content in the external markup.",
                    name
                );
            }
            if !validator.finish() {
                validity_error!(
                    self,
                    ParserMismatchElementContentModel,
                    "The content of element '{}' does not match to its content model.",
                    name
                );
            }
        }

        Ok(())
    }

    /// Return `true` if the tag is empty tag, otherwise `false`.
    fn parse_start_or_empty_tag(
        &mut self,
        name: &mut String,
        prefix_length: &mut usize,
    ) -> Result<bool, XMLError> {
        self.grow()?;

        if !self.source.content_bytes().starts_with(b"<") {
            fatal_error!(
                self,
                ParserInvalidStartOrEmptyTag,
                "StartTag or EmptyTag must start with '<'."
            );
            return Err(XMLError::ParserInvalidStartOrEmptyTag);
        }
        // skip '<'
        self.source.advance(1)?;
        self.locator.update_column(|c| c + 1);

        if self.config.is_enable(ParserOption::Namespaces) {
            *prefix_length = self.parse_qname(name)?;
        } else {
            self.parse_name(name)?;
        }

        match self.validation_stack.last_mut() {
            Some(Some((_, validator))) => {
                if self.config.is_enable(ParserOption::Validation) {
                    validator.push_name(name);
                }
            }
            Some(None) => {
                // The parent element is not declared.
            }
            None => {
                // This is root element or the validation option is disabled.
                // If the validation option is enabled, Check if element name is equal to dtd name.
                // [VC: Root Element Type]
                if self.config.is_enable(ParserOption::Validation) && *name != self.dtd_name {
                    validity_error!(
                        self,
                        ParserMismatchElementType,
                        "The document type declaration name does not match the document element type."
                    );
                }
            }
        }

        if let Some(contentspec) = self.elementdecls.get_mut(name) {
            let validator = contentspec.new_validator();
            self.validation_stack
                .push(Some((name.as_str().into(), validator)));
        } else if self.config.is_enable(ParserOption::Validation) {
            // [VC: Element Valid]
            validity_error!(
                self,
                ParserUndeclaredElement,
                "The element type '{}' is undeclared.",
                name
            );
            self.validation_stack.push(None);
        }

        let mut s = self.skip_whitespaces()?;
        self.grow()?;
        if self.source.content_bytes().is_empty() {
            return Err(XMLError::ParserUnexpectedEOF);
        }

        let mut atts = vec![];
        let mut att_name = String::new();
        let mut att_value = String::new();
        let xml_ns_namespace: Arc<str> = XML_NS_NAMESPACE.into();
        while !matches!(self.source.content_bytes()[0], b'/' | b'>') {
            if s == 0 {
                fatal_error!(
                    self,
                    ParserInvalidStartOrEmptyTag,
                    "Whitespaces are required before attribute names."
                );
            }

            att_name.clear();
            let mut prefix_length = 0;
            if self.config.is_enable(ParserOption::Namespaces) {
                prefix_length = self.parse_qname(&mut att_name)?;
            } else {
                self.parse_name(&mut att_name)?;
            }

            self.skip_whitespaces()?;
            self.grow()?;
            if !self.source.content_bytes().starts_with(b"=") {
                fatal_error!(
                    self,
                    ParserInvalidAttribute,
                    "'=' is not found after an attribute name in start or empty tag."
                );
                return Err(XMLError::ParserInvalidAttribute);
            }
            // skip '='
            self.source.advance(1)?;
            self.locator.update_column(|c| c + 1);

            self.skip_whitespaces()?;

            att_value.clear();
            self.parse_att_value(&mut att_value)?;
            let (declared, modified) = {
                let before_normalize = att_value.len();
                let declared = self.normalize_att_value(name, &att_name, &mut att_value, None);
                (declared, before_normalize != att_value.len())
            };

            if self.config.is_enable(ParserOption::Namespaces) {
                let mut uri = None;
                if (prefix_length == 5 && &att_name[..prefix_length] == "xmlns")
                    || att_name == "xmlns"
                {
                    // This is a namespace declaration. Register the namespace.

                    // TODO:
                    // Warn when the namespace name is not a URI or is a relative URI.
                    // According to the namespace specification, the check is optional,
                    // so it conforms to the specification as is, but it may cause confusion
                    // when utilizing other specifications.
                    let prefix = if att_name == "xmlns" {
                        if att_value == XML_NS_NAMESPACE || att_value == XML_XML_NAMESPACE {
                            ns_error!(
                                self,
                                ParserUnacceptableNamespaceName,
                                "Namespace '{}' cannot be declared as default namespace.",
                                att_value
                            );
                        }
                        ""
                    } else {
                        if att_value.is_empty()
                            && matches!(self.version, XMLVersion::XML10 | XMLVersion::Unknown)
                        {
                            ns_error!(
                                self,
                                ParserUnacceptableNamespaceName,
                                "Empty namespace name is not allowed in Namespace in XML 1.0."
                            );
                        } else if att_value == XML_NS_NAMESPACE {
                            ns_error!(
                                self,
                                ParserUnacceptableNamespaceName,
                                "The namespace '{}' cannot be declared explicitly.",
                                XML_NS_NAMESPACE
                            );
                        } else if &att_name[prefix_length + 1..] != "xml"
                            && att_value == XML_XML_NAMESPACE
                        {
                            ns_error!(
                                self,
                                ParserUnacceptableNamespaceName,
                                "The namespace '{}' cannot bind prefixes other than 'xml'.",
                                att_value
                            );
                        } else if &att_name[prefix_length + 1..] == "xml"
                            && att_value != XML_XML_NAMESPACE
                        {
                            ns_error!(
                                self,
                                ParserUnacceptableNamespaceName,
                                "The namespace '{}' cannot bind the prefix 'xml'.",
                                &att_name[prefix_length + 1..]
                            );
                        } else if &att_name[prefix_length + 1..] == "xmlns" {
                            ns_error!(
                                self,
                                ParserUnacceptableNamespaceName,
                                "Any namespaces cannot bind 'xmlns' explicitly."
                            );
                        }
                        &att_name[prefix_length + 1..]
                    };
                    match URIString::parse(&att_value) {
                        Ok(uri) if !uri.is_absolute() => {
                            ns_error!(
                                self,
                                ParserNamespaceNameNotAbsoluteURI,
                                "The namespace name '{}' is not a valid absolute URI.",
                                att_value
                            );
                        }
                        Ok(_) => {}
                        Err(_) => {
                            ns_error!(
                                self,
                                ParserNamespaceNameNotURI,
                                "The namespace name '{}' is not a valid URI.",
                                att_value
                            );
                        }
                    }
                    let pos = self.namespaces.len();
                    if let Some((pre, old)) = self.prefix_map.get_key_value(prefix) {
                        self.namespaces
                            .push((pre.clone(), att_value.as_str().into(), *old));
                        *self.prefix_map.get_mut(prefix).unwrap() = pos;
                    } else {
                        let prefix: Arc<str> = prefix.into();
                        self.namespaces.push((
                            prefix.clone(),
                            att_value.as_str().into(),
                            usize::MAX,
                        ));
                        self.prefix_map.insert(prefix, pos);
                    }
                    uri = Some(xml_ns_namespace.clone());
                }
                // The namespace name may be overwritten by declarations that appear later,
                // so set it to `None` at this point.
                // Check after reading all attributes of this tag.
                let mut att = Attribute {
                    uri,
                    local_name: if prefix_length > 0 {
                        Some(att_name[prefix_length + 1..].into())
                    } else {
                        Some(att_name.as_str().into())
                    },
                    qname: att_name.as_str().into(),
                    value: att_value.as_str().into(),
                    flag: 0,
                };
                att.set_specified();
                if att.uri.is_some() {
                    att.set_nsdecl();
                }
                if declared {
                    att.set_declared();
                }
                if modified {
                    att.set_declaration_dependent_normalization();
                }
                atts.push(att);
            } else {
                let mut att = Attribute {
                    uri: None,
                    local_name: None,
                    qname: att_name.as_str().into(),
                    value: att_value.as_str().into(),
                    flag: 0,
                };
                att.set_specified();
                if declared {
                    att.set_declared();
                }
                if modified {
                    att.set_declaration_dependent_normalization();
                }
                atts.push(att);
            }

            s = self.skip_whitespaces()?;
            if self.source.content_bytes().is_empty() {
                self.grow()?;
                if self.source.content_bytes().is_empty() {
                    return Err(XMLError::ParserUnexpectedEOF);
                }
            }
        }

        // resolve namespaces for attribtues
        if self.config.is_enable(ParserOption::Namespaces) {
            for att in &mut atts {
                if att.is_nsdecl() {
                    continue;
                }
                let len = att.local_name.as_deref().unwrap().len();
                if len == att.qname.len() {
                    // According to the namespace specification, attribute names without prefixes
                    // do not belong to the default namespace, but rather belong to no namespace.
                    // Therefore, we need to do nothing.
                    continue;
                }

                let prefix = &att.qname[..att.qname.len() - len - 1];
                if let Some(&pos) = self.prefix_map.get(prefix) {
                    att.uri = Some(self.namespaces[pos].1.clone());
                } else {
                    // It is unclear what to do when the corresponding namespace cannot be found,
                    // but for now, we will do nothing except for report an error.
                    ns_error!(
                        self,
                        ParserUndefinedNamespace,
                        "The namespace name for the prefix '{}' has not been declared.",
                        prefix
                    );
                }
            }
        }
        // check attribute constraints
        if self.config.is_enable(ParserOption::Namespaces) {
            for (i, att) in atts.iter().enumerate() {
                for prev in atts.iter().take(i) {
                    if att.local_name == prev.local_name && att.uri == prev.uri {
                        // [NSC: Attributes Unique]
                        fatal_error!(
                            self,
                            ParserDuplicateAttributes,
                            "The attribute '{{{}}}{}' is duplicated",
                            att.uri.as_deref().unwrap_or("(null)"),
                            att.local_name.as_deref().unwrap()
                        );
                        break;
                    }
                }
            }
        } else {
            for (i, att) in atts.iter().enumerate() {
                for prev in atts.iter().take(i) {
                    if att.qname == prev.qname {
                        // [WFC: Unique Att Spec]
                        fatal_error!(
                            self,
                            ParserDuplicateAttributes,
                            "The attribute '{}' is duplicated.",
                            att.qname
                        );
                        break;
                    }
                }
            }
        }
        if self.config.is_enable(ParserOption::Validation) {
            let mut notation_attribute = false;
            for att in &atts {
                let Some((atttype, default_decl, is_external_markup)) =
                    self.attlistdecls.get(name, &att.qname)
                else {
                    // [VC: Attribute Value Type]
                    validity_error!(
                        self,
                        ParserUndeclaredAttribute,
                        "The attribute '{}' is not declared in DTD.",
                        att.qname
                    );
                    continue;
                };
                match atttype {
                    AttributeType::CDATA => {
                        // no constraints
                    }
                    AttributeType::ID => {
                        if self.config.is_enable(ParserOption::Namespaces)
                            && let Err(err) = self.validate_ncname(&att.value)
                        {
                            // [VC: ID]
                            validity_error!(self, err, "ID attribute must match to NCName.");
                        } else if !self.config.is_enable(ParserOption::Namespaces)
                            && let Err(err) = self.validate_name(&att.value)
                        {
                            // [VC: ID]
                            validity_error!(self, err, "ID attribute must match to Name.");
                        } else {
                            if !self.specified_ids.insert(att.value.clone()) {
                                // [VC: ID]
                                validity_error!(
                                    self,
                                    ParserDuplicateIDAttribute,
                                    "ID '{}' is specified multiple times in the document.",
                                    att.value
                                );
                            }
                            self.unresolved_ids.remove(&att.value);
                        }
                    }
                    AttributeType::IDREF => {
                        if self.config.is_enable(ParserOption::Namespaces)
                            && let Err(err) = self.validate_ncname(&att.value)
                        {
                            // [VC: IDREF]
                            validity_error!(self, err, "IDREF attribute must match to NCName.");
                        } else if !self.config.is_enable(ParserOption::Namespaces)
                            && let Err(err) = self.validate_name(&att.value)
                        {
                            // [VC: IDREF]
                            validity_error!(self, err, "IDREF attribute must match to Name.");
                        } else if !self.specified_ids.contains(&att.value) {
                            self.unresolved_ids.insert(att.value.clone());
                        }
                    }
                    AttributeType::IDREFS => {
                        if self.config.is_enable(ParserOption::Namespaces)
                            && let Err(err) =
                                self.validate_names(&att.value, |name| self.validate_ncname(name))
                        {
                            // [VC: IDREF]
                            validity_error!(self, err, "IDREFS attribute must match to Names.");
                        } else if !self.config.is_enable(ParserOption::Namespaces)
                            && let Err(err) =
                                self.validate_names(&att.value, |name| self.validate_name(name))
                        {
                            // [VC: IDREF]
                            validity_error!(self, err, "IDREFS attribute must match to Names.");
                        } else {
                            for idref in att.value.split('\x20') {
                                if !self.specified_ids.contains(idref) {
                                    self.unresolved_ids.insert(idref.into());
                                }
                            }
                        }
                    }
                    AttributeType::ENTITY => {
                        if self.config.is_enable(ParserOption::Namespaces)
                            && let Err(err) = self.validate_ncname(&att.value)
                        {
                            // [VC: Entity Name]
                            validity_error!(self, err, "ENTITY attribute must match to NCName.");
                        } else if !self.config.is_enable(ParserOption::Namespaces)
                            && let Err(err) = self.validate_name(&att.value)
                        {
                            // [VC: Entity Name]
                            validity_error!(self, err, "ENTITY attribute must match to Name.");
                        } else if let Some(entity) = self.entities.get(&att.value) {
                            if !matches!(entity, EntityDecl::ExternalGeneralUnparsedEntity { .. }) {
                                // [VC: Entity Name]
                                validity_error!(
                                    self,
                                    ParserUndeclaredEntityReference,
                                    "The entity '{}' referred by the attribute '{}' is not an unparsed entity.",
                                    att.value,
                                    att.qname
                                );
                            }
                        } else {
                            // [VC: Entity Name]
                            validity_error!(
                                self,
                                ParserUndeclaredEntityReference,
                                "ENTITY attribute value '{}' cannot refer to any entities.",
                                att.value
                            );
                        }
                    }
                    AttributeType::ENTITIES => {
                        if self.config.is_enable(ParserOption::Namespaces)
                            && let Err(err) =
                                self.validate_names(&att.value, |name| self.validate_ncname(name))
                        {
                            // [VC: Entity Name]
                            validity_error!(
                                self,
                                err,
                                "'{}' of ENTITY attribute '{}' doex not match to Names.",
                                att.value,
                                att.qname
                            );
                        } else if !self.config.is_enable(ParserOption::Namespaces)
                            && let Err(err) =
                                self.validate_names(&att.value, |name| self.validate_name(name))
                        {
                            // [VC: Entity Name]
                            validity_error!(
                                self,
                                err,
                                "'{}' of ENTITY attribute '{}' doex not match to Names.",
                                att.value,
                                att.qname
                            );
                        } else {
                            for entity in att.value.split('\x20') {
                                if let Some(decl) = self.entities.get(entity) {
                                    if !matches!(
                                        decl,
                                        EntityDecl::ExternalGeneralUnparsedEntity { .. }
                                    ) {
                                        // [VC: Entity Name]
                                        validity_error!(
                                            self,
                                            ParserUndeclaredEntityReference,
                                            "The entity '{}' referred by the attribute '{}' is not an unparsed entity.",
                                            entity,
                                            att.qname
                                        );
                                    }
                                } else {
                                    // [VC: Entity Name]
                                    validity_error!(
                                        self,
                                        ParserUndeclaredEntityReference,
                                        "ENTITY attribute value '{}' cannot refer to any entities.",
                                        entity
                                    );
                                }
                            }
                        }
                    }
                    AttributeType::NMTOKEN => {
                        if let Err(err) = self.validate_nmtoken(&att.value) {
                            // [VC: Name Token]
                            validity_error!(
                                self,
                                err,
                                "'{}' of NMTOKEN attribute '{}' does not match to Nmtoken.",
                                att.value,
                                att.qname
                            );
                        }
                    }
                    AttributeType::NMTOKENS => {
                        if let Err(err) = self.validate_nmtokens(&att.value) {
                            // [VC: Name Token]
                            validity_error!(
                                self,
                                err,
                                "'{}' of NMTOKENS attribute '{}' does not match to Nmtokens.",
                                att.value,
                                att.qname
                            );
                        }
                    }
                    AttributeType::NOTATION(set) => {
                        if !set.contains(&att.value) {
                            // [VC: Notation Attributes]
                            validity_error!(
                                self,
                                ParserUnacceptableNotationAttribute,
                                "'{}' is not allowed as a value for attribute '{}'.",
                                att.value,
                                att.qname
                            );
                        }
                        if notation_attribute {
                            // [VC: One Notation Per Element Type]
                            validity_error!(
                                self,
                                ParserMultipleNotationAttributePerElement,
                                "Attribute `{}` appeared as a multiple-occurrence notation attribute in the element '{}'.",
                                att.qname,
                                name
                            );
                        }
                        notation_attribute = true;
                    }
                    AttributeType::Enumeration(set) => {
                        if !set.contains(&att.value) {
                            // [VC: Validity constraint: Enumeration]
                            validity_error!(
                                self,
                                ParserUnacceptableEnumerationAttribute,
                                "'{}' is not allowed as a value for attribute '{}'.",
                                att.value,
                                att.qname
                            );
                        }
                    }
                }

                // Since valid documents do not have default values for ID attributes,
                // ID attributes are excluded from validation of #FIXED default attribute values.
                if !matches!(atttype, AttributeType::ID)
                    && let DefaultDecl::FIXED(def) = default_decl
                    && &att.value != def
                {
                    // [VC: Fixed Attribute Default]
                    validity_error!(
                        self,
                        ParserMismatchFixedDefaultAttributeValue,
                        "The attribute '{}' of the element '{}' is fixed as '{}', but specified '{}'.",
                        att.qname,
                        name,
                        def,
                        att.value
                    );
                }

                if !matches!(atttype, AttributeType::CDATA)
                    && *is_external_markup
                    && self.standalone == Some(true)
                    && att.has_declaration_dependent_normalization()
                {
                    // [VC: Standalone Document Declaration]
                    validity_error!(
                        self,
                        ParserInvalidStandaloneDocument,
                        "standalone='yes', but an attribute declaration affecting attribute value normalization was found."
                    );
                }
            }
        }
        if let Some(decls) = self.attlistdecls.attlist(name) {
            for (attr, (_, default_decl, is_external_markup)) in decls {
                match default_decl {
                    DefaultDecl::REQUIRED => {
                        if self.config.is_enable(ParserOption::Validation)
                            && atts.iter().all(|att| att.qname.as_ref() != attr)
                        {
                            validity_error!(
                                self,
                                ParserRequiredAttributeNotFound,
                                "#REQUIRED attribute '{}' of the element '{}' is not specified.",
                                attr,
                                name
                            );
                        }
                    }
                    DefaultDecl::None(def) | DefaultDecl::FIXED(def) => {
                        if atts.iter().all(|att| att.qname.as_ref() != attr) {
                            if self.config.is_enable(ParserOption::Validation)
                                && *is_external_markup
                                && self.standalone == Some(true)
                            {
                                // [VC: Standalone Document Declaration]
                                validity_error!(
                                    self,
                                    ParserInvalidStandaloneDocument,
                                    "standalone='yes', but an unspecified attribute '{}' of the element '{}' is declared to have a default value in the external markup.",
                                    attr,
                                    name
                                );
                            }
                            let mut att = Attribute {
                                uri: None,
                                local_name: None,
                                qname: attr.into(),
                                value: def.clone(),
                                flag: 0,
                            };
                            att.set_declared();

                            // TODO: Namespace handling
                        }
                    }
                    _ => {}
                }
            }
        }

        self.grow()?;
        if !self.source.content_bytes().starts_with(b">")
            && !self.source.content_bytes().starts_with(b"/>")
        {
            fatal_error!(
                self,
                ParserInvalidStartOrEmptyTag,
                "Start or Empty tag does not end with '>' or '/>'."
            );
            return Err(XMLError::ParserInvalidStartOrEmptyTag);
        }

        if !self.fatal_error_occurred {
            for att in atts.iter().filter(|att| att.is_nsdecl()) {
                let len = att.local_name.as_deref().unwrap().len();
                if len == att.qname.len() {
                    self.handler.start_prefix_mapping(None, &att.value);
                } else {
                    self.handler.start_prefix_mapping(
                        Some(&att.qname[..att.qname.len() - len - 1]),
                        &att.value,
                    );
                }
            }
            if self.config.is_enable(ParserOption::Namespaces) {
                if *prefix_length > 0 {
                    if let Some(&pos) = self.prefix_map.get(&name[..*prefix_length]) {
                        self.handler.start_element(
                            Some(&self.namespaces[pos].0),
                            Some(&name[*prefix_length + 1..]),
                            name,
                            &atts,
                        );
                    } else {
                        ns_error!(
                            self,
                            ParserUndefinedNamespace,
                            "The prefix '{}' is not bind to any namespaces.",
                            &name[..*prefix_length]
                        );
                    }
                } else {
                    // default namespace
                    if let Some(&pos) = self.prefix_map.get("") {
                        self.handler.start_element(
                            Some(&self.namespaces[pos].0),
                            Some(name),
                            name,
                            &atts,
                        );
                    } else {
                        self.handler.start_element(None, Some(name), name, &atts);
                    }
                }
            } else {
                self.handler.start_element(None, None, name, &atts);
            }
        }

        self.grow()?;
        if self.source.content_bytes().starts_with(b"/>") {
            // This is an empty tag.

            // skip '/>'
            self.source.advance(2)?;
            self.locator.update_column(|c| c + 2);
            Ok(true)
        } else {
            // This is a start tag.

            // skip '>'
            self.source.advance(1)?;
            self.locator.update_column(|c| c + 1);
            Ok(false)
        }
    }
}
