use std::{
    mem::take,
    sync::{Arc, LazyLock},
};

use anyxml_uri::uri::URIString;

use crate::{
    ParserSpec, XML_NS_NAMESPACE, XML_XML_NAMESPACE, XMLVersion,
    error::XMLError,
    sax::{
        AttributeType, DefaultDecl, EntityDecl,
        attributes::{Attribute, Attributes},
        error::{error, fatal_error, ns_error, validity_error},
        handler::SAXHandler,
        parser::{ParserOption, ParserState, XMLReader},
        source::InputSource,
    },
};

static ARC_XML_NS_NAMESPACE: LazyLock<Arc<str>> = LazyLock::new(|| XML_NS_NAMESPACE.into());

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
            self.state = ParserState::InContent;
            self.parse_content()?;

            // parse end tag
            let end_tag_name = self.parse_end_tag()?;
            self.check_element_type_match(&name, &end_tag_name)?;
        }

        self.report_end_element(&name, prefix_length);
        self.resume_namespace_stack(old_ns_stack_depth);
        self.finish_content_model_validation(&name);
        Ok(())
    }

    /// Return `true` if the tag is empty tag, otherwise `false`.
    pub(crate) fn parse_start_or_empty_tag(
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

        let mut atts = Attributes::new();
        let mut att_name = String::new();
        let mut att_value = String::new();
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

            self.add_attribute(
                &mut atts,
                &att_name,
                &att_value,
                prefix_length,
                true,
                declared,
                modified,
            );

            s = self.skip_whitespaces()?;
            if self.source.content_bytes().is_empty() {
                self.grow()?;
                if self.source.content_bytes().is_empty() {
                    return Err(XMLError::ParserUnexpectedEOF);
                }
            }
        }

        let attlistdecls = take(&mut self.attlistdecls);
        if let Some(decls) = attlistdecls.attlist(name) {
            for (attr, (_, default_decl, is_external_markup)) in decls {
                match default_decl {
                    DefaultDecl::REQUIRED => {
                        if self.config.is_enable(ParserOption::Validation)
                            && !atts.contains_qname(attr)
                        {
                            // [VC: Required Attribute]
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
                        if !atts.contains_qname(attr) {
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
                            let prefix_length = attr.find(':').unwrap_or(0);
                            self.add_attribute(
                                &mut atts,
                                attr,
                                def,
                                prefix_length,
                                false,
                                true,
                                false,
                            );
                        }
                    }
                    _ => {}
                }
            }
        }
        self.attlistdecls = attlistdecls;

        // resolve namespaces for attribtues
        if self.config.is_enable(ParserOption::Namespaces) {
            for i in 0..atts.len() {
                let att = &atts[i];
                if att.is_nsdecl() {
                    continue;
                }
                atts.set_namespace(i, |prefix| {
                    if let Some(namespace) = self.namespaces.get(prefix) {
                        Some(namespace.namespace_name)
                    } else {
                        // It is unclear what to do when the corresponding namespace cannot be found,
                        // but for now, we will do nothing except for report an error.
                        ns_error!(
                            self,
                            ParserUndefinedNamespace,
                            "The namespace name for the prefix '{}' has not been declared.",
                            prefix
                        );
                        None
                    }
                });
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
                    if let Some(namespace) = self.namespaces.get(&name[..*prefix_length]) {
                        self.handler.start_element(
                            Some(&namespace.namespace_name),
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
                    if let Some(namespace) = self.namespaces.get("") {
                        self.handler.start_element(
                            Some(&namespace.namespace_name),
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

    /// Return the name of parsed end tag.
    pub(crate) fn parse_end_tag(&mut self) -> Result<String, XMLError> {
        self.grow()?;
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
        Ok(end_tag_name)
    }

    pub(crate) fn check_element_type_match(
        &mut self,
        start: &str,
        end: &str,
    ) -> Result<(), XMLError> {
        if start != end {
            let start = if start.chars().count() > 15 {
                &format!("{}...", start.chars().take(12).collect::<String>())
            } else {
                start
            };
            let end = if end.chars().count() > 15 {
                &format!("{}...", end.chars().take(12).collect::<String>())
            } else {
                end
            };
            // [WFC: Element Type Match]
            fatal_error!(
                self,
                ParserMismatchElementType,
                "The start tag ('{}') and end tag ('{}') names do not match.",
                start,
                end
            );
            return Err(XMLError::ParserMismatchElementType);
        }
        Ok(())
    }

    pub(crate) fn report_end_element(&mut self, name: &str, prefix_length: usize) {
        if !self.fatal_error_occurred {
            if self.config.is_enable(ParserOption::Namespaces) {
                if prefix_length > 0 {
                    if let Some(namespace) = self.namespaces.get(&name[..prefix_length]) {
                        self.handler.end_element(
                            Some(&namespace.namespace_name),
                            Some(&name[prefix_length + 1..]),
                            name,
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
                    if let Some(namespace) = self.namespaces.get("") {
                        self.handler
                            .end_element(Some(&namespace.namespace_name), Some(name), name);
                    } else {
                        self.handler.end_element(None, Some(name), name);
                    }
                }
            } else {
                self.handler.end_element(None, None, name);
            }
        }
    }

    pub(crate) fn resume_namespace_stack(&mut self, old_depth: usize) {
        while self.namespaces.len() > old_depth
            && let Some(namespace) = self.namespaces.pop()
        {
            if namespace.prefix.is_empty() {
                self.handler.end_prefix_mapping(None);
            } else {
                self.handler.end_prefix_mapping(Some(&namespace.prefix));
            }
        }
    }

    pub(crate) fn finish_content_model_validation(&mut self, name: &str) {
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
    }

    fn add_attribute(
        &mut self,
        atts: &mut Attributes,
        att_name: &str,
        att_value: &str,
        prefix_length: usize,
        specified: bool,
        declared: bool,
        modified: bool,
    ) {
        let mut att = if self.config.is_enable(ParserOption::Namespaces) {
            let mut uri = None;
            if (prefix_length == 5 && &att_name[..prefix_length] == "xmlns") || att_name == "xmlns"
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
                match URIString::parse(att_value) {
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
                self.namespaces.push(prefix, att_value);
                uri = Some(ARC_XML_NS_NAMESPACE.clone());
            }
            // The namespace name may be overwritten by declarations that appear later,
            // so set it to `None` at this point.
            // Check after reading all attributes of this tag.
            let mut att = Attribute {
                uri,
                local_name: if prefix_length > 0 {
                    Some(att_name[prefix_length + 1..].into())
                } else {
                    Some(att_name.into())
                },
                qname: att_name.into(),
                value: att_value.into(),
                flag: 0,
            };
            if att.uri.is_some() {
                att.set_nsdecl();
            }
            att
        } else {
            Attribute {
                uri: None,
                local_name: None,
                qname: att_name.into(),
                value: att_value.into(),
                flag: 0,
            }
        };
        if specified {
            att.set_specified();
        }
        if declared {
            att.set_declared();
        }
        if modified {
            att.set_declaration_dependent_normalization();
        }
        if att.qname.as_ref() == "xml:space"
            && !matches!(att.value.as_ref(), "default" | "preserve")
        {
            error!(
                self,
                ParserUnacceptableXMLSpaceAttribute,
                "The value of 'xml:space' attribute is 'default' or 'preserve', but '{}' is specified.",
                att.value
            );
        }
        if let Err((att, _)) = atts.push(att) {
            // check attribute constraints
            if self.config.is_enable(ParserOption::Namespaces) {
                // [NSC: Attributes Unique]
                fatal_error!(
                    self,
                    ParserDuplicateAttributes,
                    "The attribute '{{{}}}{}' is duplicated",
                    att.uri.as_deref().unwrap_or("(null)"),
                    att.local_name.as_deref().unwrap()
                );
            } else {
                // [WFC: Unique Att Spec]
                fatal_error!(
                    self,
                    ParserDuplicateAttributes,
                    "The attribute '{}' is duplicated.",
                    att.qname
                );
            }
        }
    }
}
