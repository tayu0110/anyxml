use std::collections::HashSet;

use crate::{
    ParserSpec,
    error::XMLError,
    sax::{
        AttributeType, DefaultDecl,
        error::{error, fatal_error, validity_error, warning},
        handler::SAXHandler,
        parser::{ParserOption, XMLReader},
        source::InputSource,
    },
};

impl<'a, Spec: ParserSpec<Reader = InputSource<'a>>, H: SAXHandler> XMLReader<Spec, H> {
    /// ```text
    /// [52] AttlistDecl ::= '<!ATTLIST' S Name AttDef* S? '>'
    /// ```
    pub(crate) fn parse_attlist_decl(&mut self) -> Result<(), XMLError> {
        self.grow()?;

        if !self.source.content_bytes().starts_with(b"<!ATTLIST") {
            fatal_error!(
                self,
                ParserInvalidAttlistDecl,
                "Attribute list declration must start with '<!ATTLIST'."
            );
            return Err(XMLError::ParserInvalidAttlistDecl);
        }
        // skip '<!ATTLIST'
        self.source.advance(9)?;
        self.locator.update_column(|c| c + 9);

        let base_source_id = self.source.source_id();
        let is_external_markup = self.is_external_markup();
        if self.skip_whitespaces_with_handle_peref(true)? == 0 {
            fatal_error!(
                self,
                ParserInvalidAttlistDecl,
                "Whitespaces are required after '<!ATTLIST' in attribute list declaration."
            );
        }

        let mut name = String::new();
        if self.config.is_enable(ParserOption::Namespaces) {
            self.parse_qname(&mut name)?;
        } else {
            self.parse_name(&mut name)?;
        }

        let mut s = self.skip_whitespaces_with_handle_peref(true)?;
        self.grow()?;
        let mut att_name = String::new();
        while !self.source.content_bytes().starts_with(b">") {
            if s == 0 {
                fatal_error!(
                    self,
                    ParserInvalidAttlistDecl,
                    "Whitespaces are required before Name in AttDef."
                );
            }
            att_name.clear();
            let (atttype, default_decl) = self.parse_att_def(false, &mut att_name)?;
            if !self.fatal_error_occurred {
                self.handler
                    .attribute_decl(&name, &att_name, &atttype, &default_decl);
            }
            if !self.attlistdecls.insert(
                name.as_str(),
                att_name.as_str(),
                atttype,
                default_decl,
                is_external_markup,
            ) {
                warning!(
                    self,
                    ParserDuplicateAttlistDecl,
                    "An attribute list declaration for the attribute '{}' the element '{}' is duplicated.",
                    att_name,
                    name
                );
            } else if self.config.is_enable(ParserOption::Validation) {
                let (atttype, default_decl, _) = self.attlistdecls.get(&name, &att_name).unwrap();
                // check validity constraints
                match atttype {
                    AttributeType::ID => {
                        if !matches!(default_decl, DefaultDecl::REQUIRED | DefaultDecl::IMPLIED) {
                            // [VC: ID Attribute Default]
                            validity_error!(
                                self,
                                ParserInvalidIDAttributeValue,
                                "ID attribute default must be '#REQUIRED' or '#IMPLIED'."
                            );
                        }

                        match self.idattr_decls.entry(name.as_str().into()) {
                            std::collections::hash_map::Entry::Occupied(_) => {
                                // [VC: One ID per Element Type]
                                validity_error!(
                                    self,
                                    ParserMultipleIDAttributePerElement,
                                    "ID attribute declarations appear multiple times on element '{}'.",
                                    name
                                );
                            }
                            std::collections::hash_map::Entry::Vacant(entry) => {
                                entry.insert(att_name.as_str().into());
                            }
                        }
                    }
                    AttributeType::IDREF => match &default_decl {
                        DefaultDecl::FIXED(def) | DefaultDecl::None(def) => {
                            if self.config.is_enable(ParserOption::Namespaces)
                                && self.validate_ncname(def).is_err()
                            {
                                // [VC: IDREF]
                                validity_error!(
                                    self,
                                    ParserInvalidIDREFAttributeValue,
                                    "IDREF attribute default must match to NCName."
                                );
                            } else if !self.config.is_enable(ParserOption::Namespaces)
                                && self.validate_name(def).is_err()
                            {
                                // [VC: IDREF]
                                validity_error!(
                                    self,
                                    ParserInvalidIDREFAttributeValue,
                                    "IDREF attribute default must match to Name."
                                );
                            }
                        }
                        _ => {}
                    },
                    AttributeType::IDREFS => match &default_decl {
                        DefaultDecl::FIXED(def) | DefaultDecl::None(def) => {
                            if (self.config.is_enable(ParserOption::Namespaces)
                                && self
                                    .validate_names(def, |name| self.validate_ncname(name))
                                    .is_err())
                                || (!self.config.is_enable(ParserOption::Namespaces)
                                    && self
                                        .validate_names(def, |name| self.validate_name(name))
                                        .is_err())
                            {
                                // [VC: IDREF]
                                validity_error!(
                                    self,
                                    ParserInvalidIDREFAttributeValue,
                                    "IDREFS attribute default must match to Names."
                                );
                            }
                        }
                        _ => {}
                    },
                    AttributeType::ENTITY | AttributeType::ENTITIES => {
                        // [VC: Entity Name]
                        // Since we need to verify whether all referenced entities are declared
                        // after all entities have been parsed, we skip this step.
                    }
                    AttributeType::NMTOKEN => match default_decl {
                        DefaultDecl::FIXED(def) | DefaultDecl::None(def) => {
                            if let Err(err) = self.validate_nmtoken(def) {
                                // [VC: Name Token]
                                validity_error!(
                                    self,
                                    err,
                                    "NMTOKEN attribute default must match to Nmtoken."
                                );
                            }
                        }
                        _ => {}
                    },
                    AttributeType::NMTOKENS => {
                        match default_decl {
                            DefaultDecl::FIXED(def) | DefaultDecl::None(def) => {
                                if let Err(err) = self.validate_nmtokens(def) {
                                    // [VC: Name Token]
                                    validity_error!(
                                        self,
                                        err,
                                        "NMTOKENS attribute default must match to Nmtokens."
                                    );
                                }
                            }
                            _ => {}
                        }
                    }
                    _ => {}
                }
            }
            s = self.skip_whitespaces_with_handle_peref(true)?;
            if self.source.content_bytes().is_empty() {
                self.grow()?;
                if self.source.content_bytes().is_empty() {
                    break;
                }
            }
        }

        if self.source.source_id() != base_source_id {
            // [VC: Proper Declaration/PE Nesting]
            validity_error!(
                self,
                ParserEntityIncorrectNesting,
                "A parameter entity in an attribute declaration is nested incorrectly."
            );
        }

        if !self.source.content_bytes().starts_with(b">") {
            return Err(XMLError::ParserUnexpectedEOF);
        }
        // skip '>'
        self.source.advance(1)?;
        self.locator.update_column(|c| c + 1);

        Ok(())
    }

    /// ```text
    /// [53] AttDef ::= S Name S AttType S DefaultDecl
    /// ```
    pub(crate) fn parse_att_def(
        &mut self,
        need_trim_whitespace: bool,
        att_name: &mut String,
    ) -> Result<(AttributeType, DefaultDecl), XMLError> {
        if need_trim_whitespace && self.skip_whitespaces_with_handle_peref(true)? == 0 {
            fatal_error!(
                self,
                ParserInvalidAttlistDecl,
                "Whitespaces are required before Name in AttDef."
            );
        }

        if self.config.is_enable(ParserOption::Namespaces) {
            self.parse_qname(att_name)?;
        } else {
            self.parse_name(att_name)?;
        }

        if self.skip_whitespaces_with_handle_peref(true)? == 0 {
            fatal_error!(
                self,
                ParserInvalidAttlistDecl,
                "Whitespaces are required before AttType in AttDef."
            );
        }

        // parse AttType
        self.grow()?;
        let mut atttype = match self.source.content_bytes() {
            [b'(', ..] => {
                // Enumeration
                // [59] Enumeration ::= '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'

                // skip '('
                self.source.advance(1)?;
                self.locator.update_column(|c| c + 1);

                let enum_source_id = self.source.source_id();

                self.skip_whitespaces_with_handle_peref(true)?;
                let mut buffer = String::new();
                self.parse_nmtoken(&mut buffer)?;
                let mut ret = HashSet::new();
                self.skip_whitespaces_with_handle_peref(true)?;
                self.grow()?;
                while self.source.content_bytes().starts_with(b"|") {
                    self.source.advance(1)?;
                    self.locator.update_column(|c| c + 1);
                    self.skip_whitespaces_with_handle_peref(true)?;
                    if !ret.insert(buffer.as_str().into())
                        && self.config.is_enable(ParserOption::Validation)
                    {
                        // [VC: No Duplicate Tokens]
                        validity_error!(
                            self,
                            ParserDuplicateTokensInAttlistDecl,
                            "'{}' is duplicate in an attlist declaration for the attribute '{}'.",
                            buffer,
                            att_name
                        );
                    }
                    buffer.clear();
                    self.parse_nmtoken(&mut buffer)?;
                    self.skip_whitespaces_with_handle_peref(true)?;
                    if self.source.content_bytes().is_empty() {
                        self.grow()?;
                    }
                }
                if !ret.insert(buffer.as_str().into())
                    && self.config.is_enable(ParserOption::Validation)
                {
                    // [VC: No Duplicate Tokens]
                    validity_error!(
                        self,
                        ParserDuplicateTokensInAttlistDecl,
                        "'{}' is duplicate in an attlist declaration for the attribute '{}'.",
                        buffer,
                        att_name
                    );
                }

                if self.source.source_id() != enum_source_id {
                    fatal_error!(
                        self,
                        ParserEntityIncorrectNesting,
                        "A parameter entity in an AttDef is nested incorrectly."
                    );
                    return Err(XMLError::ParserEntityIncorrectNesting);
                }

                if !self.source.content_bytes().starts_with(b")") {
                    fatal_error!(
                        self,
                        ParserInvalidAttlistDecl,
                        "Enumerated Attribute Type declaration does not close with ')'."
                    );
                    return Err(XMLError::ParserInvalidAttlistDecl);
                }
                // skip ')'
                self.source.advance(1)?;
                self.locator.update_column(|c| c + 1);

                AttributeType::Enumeration(ret)
            }
            [b'C', b'D', b'A', b'T', b'A', ..] => {
                // StringType
                // [55] StringType ::= 'CDATA'
                self.source.advance(5)?;
                self.locator.update_column(|c| c + 5);
                AttributeType::CDATA
            }
            [b'N', b'O', b'T', b'A', b'T', b'I', b'O', b'N', ..] => {
                // NotationType
                // [58] NotationType ::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'

                // skip 'NOTATION'
                self.source.advance(8)?;
                self.locator.update_column(|c| c + 8);

                if self.skip_whitespaces_with_handle_peref(true)? == 0 {
                    fatal_error!(
                        self,
                        ParserInvalidAttlistDecl,
                        "Whitespaces are required after 'NOTATION' in Notation Attribute Type declaration"
                    );
                }

                self.grow()?;
                if !self.source.content_bytes().starts_with(b"(") {
                    fatal_error!(
                        self,
                        ParserInvalidAttlistDecl,
                        "'(' is required after 'NOTATION' in Notation Attribute Type declaration."
                    );
                    return Err(XMLError::ParserInvalidAttlistDecl);
                }
                // skip '('
                self.source.advance(1)?;
                self.locator.update_column(|c| c + 1);

                let enum_source_id = self.source.source_id();

                self.skip_whitespaces_with_handle_peref(true)?;
                let mut buffer = String::new();
                if self.config.is_enable(ParserOption::Namespaces) {
                    self.parse_ncname(&mut buffer)?;
                } else {
                    self.parse_name(&mut buffer)?;
                }
                self.skip_whitespaces_with_handle_peref(true)?;
                self.grow()?;
                let mut ret = HashSet::new();
                while self.source.content_bytes().starts_with(b"|") {
                    // skip '|'
                    self.source.advance(1)?;
                    self.locator.update_column(|c| c + 1);
                    self.skip_whitespaces_with_handle_peref(true)?;
                    if !ret.insert(buffer.as_str().into())
                        && self.config.is_enable(ParserOption::Validation)
                    {
                        // [VC: No Duplicate Tokens]
                        validity_error!(
                            self,
                            ParserDuplicateTokensInAttlistDecl,
                            "'{}' is duplicate in an attlist declaration for the attribute '{}'.",
                            buffer,
                            att_name
                        );
                    }
                    buffer.clear();
                    if self.config.is_enable(ParserOption::Namespaces) {
                        self.parse_ncname(&mut buffer)?;
                    } else {
                        self.parse_name(&mut buffer)?;
                    }
                    self.skip_whitespaces_with_handle_peref(true)?;
                    if self.source.content_bytes().is_empty() {
                        self.grow()?;
                    }
                }
                if !ret.insert(buffer.as_str().into())
                    && self.config.is_enable(ParserOption::Validation)
                {
                    // [VC: No Duplicate Tokens]
                    validity_error!(
                        self,
                        ParserDuplicateTokensInAttlistDecl,
                        "'{}' is duplicate in an attlist declaration for the attribute '{}'.",
                        buffer,
                        att_name
                    );
                }

                if self.source.source_id() != enum_source_id {
                    fatal_error!(
                        self,
                        ParserEntityIncorrectNesting,
                        "A parameter entity in an AttDef is nested incorrectly."
                    );
                    return Err(XMLError::ParserEntityIncorrectNesting);
                }

                if !self.source.content_bytes().starts_with(b")") {
                    fatal_error!(
                        self,
                        ParserInvalidAttlistDecl,
                        "Enumerated Attribute Type declaration does not close with ')'."
                    );
                    return Err(XMLError::ParserInvalidAttlistDecl);
                }
                // skip ')'
                self.source.advance(1)?;
                self.locator.update_column(|c| c + 1);

                AttributeType::NOTATION(ret)
            }
            // TokenizedType
            [b'I', b'D', b'R', b'E', b'F', b'S', ..] => {
                self.source.advance(6)?;
                self.locator.update_column(|c| c + 6);
                AttributeType::IDREFS
            }
            [b'I', b'D', b'R', b'E', b'F', ..] => {
                self.source.advance(5)?;
                self.locator.update_column(|c| c + 5);
                AttributeType::IDREF
            }
            [b'I', b'D', ..] => {
                self.source.advance(2)?;
                self.locator.update_column(|c| c + 2);
                AttributeType::ID
            }
            [b'E', b'N', b'T', b'I', b'T', b'I', b'E', b'S', ..] => {
                self.source.advance(8)?;
                self.locator.update_column(|c| c + 8);
                AttributeType::ENTITIES
            }
            [b'E', b'N', b'T', b'I', b'T', b'Y', ..] => {
                self.source.advance(6)?;
                self.locator.update_column(|c| c + 6);
                AttributeType::ENTITY
            }
            [b'N', b'M', b'T', b'O', b'K', b'E', b'N', b'S', ..] => {
                self.source.advance(8)?;
                self.locator.update_column(|c| c + 8);
                AttributeType::NMTOKENS
            }
            [b'N', b'M', b'T', b'O', b'K', b'E', b'N', ..] => {
                self.source.advance(7)?;
                self.locator.update_column(|c| c + 7);
                AttributeType::NMTOKEN
            }
            _ => {
                fatal_error!(
                    self,
                    ParserInvalidAttlistDecl,
                    "AttType cannot be recognized."
                );
                return Err(XMLError::ParserInvalidAttlistDecl);
            }
        };

        if self.skip_whitespaces_with_handle_peref(true)? == 0 {
            fatal_error!(
                self,
                ParserInvalidAttlistDecl,
                "Whitespaces are required between AttType and DefaultDecl in Attribute list declaration."
            );
        }

        // parse DefaultDecl
        self.grow()?;
        let default_decl = match self.source.content_bytes() {
            [b'#', b'R', b'E', b'Q', b'U', b'I', b'R', b'E', b'D', ..] => {
                // skip '#REQUIRED'
                self.source.advance(9)?;
                self.locator.update_column(|c| c + 9);
                DefaultDecl::REQUIRED
            }
            [b'#', b'I', b'M', b'P', b'L', b'I', b'E', b'D', ..] => {
                // skip '#IMPLIED'
                self.source.advance(8)?;
                self.locator.update_column(|c| c + 8);
                DefaultDecl::IMPLIED
            }
            [b'#', b'F', b'I', b'X', b'E', b'D', ..] => {
                // skip '#FIXED'
                self.source.advance(6)?;
                self.locator.update_column(|c| c + 6);

                if self.skip_whitespaces_with_handle_peref(true)? == 0 {
                    fatal_error!(
                        self,
                        ParserInvalidAttlistDecl,
                        "Whitespaces are required after '#FIXED' in DefaultDecl for attribute list declaration."
                    );
                }

                let mut buffer = String::new();
                self.parse_att_value(&mut buffer)?;
                self.normalize_att_value(
                    "",
                    "",
                    &mut buffer,
                    Some(matches!(atttype, AttributeType::CDATA)),
                );
                DefaultDecl::FIXED(buffer.into_boxed_str())
            }
            _ => {
                let mut buffer = String::new();
                self.parse_att_value(&mut buffer)?;
                self.normalize_att_value(
                    "",
                    "",
                    &mut buffer,
                    Some(matches!(atttype, AttributeType::CDATA)),
                );
                DefaultDecl::None(buffer.into_boxed_str())
            }
        };

        if att_name == "xml:id" && atttype != AttributeType::ID {
            error!(
                self,
                ParserMismatchXMLIDAttributeType, "The attribute type of 'xml:id' must be ID."
            );
            // overwrite attribute type
            atttype = AttributeType::ID;

            // The validation of DefaultDecl is delegated to the same method
            // used for other attribute declarations.
        }

        if let DefaultDecl::FIXED(def) | DefaultDecl::None(def) = &default_decl {
            match &atttype {
                AttributeType::IDREF | AttributeType::ENTITY => {
                    if (self.config.is_enable(ParserOption::Namespaces)
                        && self.validate_ncname(def).is_err())
                        || (!self.config.is_enable(ParserOption::Namespaces)
                            && self.validate_name(def).is_err())
                    {
                        // [VC: Attribute Default Value Syntactically Correct]
                        validity_error!(
                            self,
                            ParserSyntaxticallyIncorrectAttributeDefault,
                            "'{}' is syntaxtically incorrect as {} type attribute value.",
                            def,
                            if matches!(atttype, AttributeType::IDREF) {
                                "IDREF"
                            } else {
                                "ENTITY"
                            }
                        );
                    }
                }
                AttributeType::IDREFS | AttributeType::ENTITIES => {
                    if (self.config.is_enable(ParserOption::Namespaces)
                        && self
                            .validate_names(def, |name| self.validate_ncname(name))
                            .is_err())
                        || (!self.config.is_enable(ParserOption::Namespaces)
                            && self
                                .validate_names(def, |name| self.validate_name(name))
                                .is_err())
                    {
                        // [VC: Attribute Default Value Syntactically Correct]
                        validity_error!(
                            self,
                            ParserSyntaxticallyIncorrectAttributeDefault,
                            "'{}' is syntaxtically incorrect as {} type attribute value.",
                            def,
                            if matches!(atttype, AttributeType::IDREFS) {
                                "IDREFS"
                            } else {
                                "ENTITIES"
                            }
                        );
                    }
                }
                AttributeType::NMTOKEN => {
                    if self.validate_nmtoken(def).is_err() {
                        // [VC: Attribute Default Value Syntactically Correct]
                        validity_error!(
                            self,
                            ParserSyntaxticallyIncorrectAttributeDefault,
                            "'{}' is syntaxtically incorrect as NMTOKEN type attribute value.",
                            def
                        );
                    }
                }
                AttributeType::NMTOKENS => {
                    if self.validate_nmtokens(def).is_err() {
                        // [VC: Attribute Default Value Syntactically Correct]
                        validity_error!(
                            self,
                            ParserSyntaxticallyIncorrectAttributeDefault,
                            "'{}' is syntaxtically incorrect as NMTOKENS type attribute value.",
                            def
                        );
                    }
                }
                AttributeType::Enumeration(set) | AttributeType::NOTATION(set) => {
                    if !set.contains(def.as_ref()) {
                        // [VC: Attribute Default Value Syntactically Correct]
                        validity_error!(
                            self,
                            ParserSyntaxticallyIncorrectAttributeDefault,
                            "'{}' is syntaxtically incorrect as {} type attribute value.",
                            def,
                            if matches!(atttype, AttributeType::Enumeration(_)) {
                                "Enumeration"
                            } else {
                                "NOTATION"
                            }
                        );
                    }
                }
                _ => {}
            }
        }

        Ok((atttype, default_decl))
    }
}
