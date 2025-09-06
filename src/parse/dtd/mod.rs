mod attlist_decl;
mod element_decl;
mod entity_decl;
mod ext_subset;
mod notation_decl;

use std::mem::take;

use anyxml_uri::uri::URIString;

use crate::{
    ParserSpec,
    error::XMLError,
    sax::{
        AttributeType, EntityDecl,
        contentspec::ContentSpec,
        error::{error, fatal_error, validity_error},
        handler::SAXHandler,
        parser::{ParserOption, ParserState, XMLReader},
        source::InputSource,
    },
};

impl<'a, Spec: ParserSpec<Reader = InputSource<'a>>, H: SAXHandler> XMLReader<Spec, H> {
    /// ```text
    /// [28] doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'  
    ///                                                             [VC:  Root Element Type]
    ///                                                             [WFC: External Subset]
    /// ```
    pub(crate) fn parse_doctypedecl(&mut self) -> Result<(), XMLError> {
        self.grow()?;
        if !self.source.content_bytes().starts_with(b"<!DOCTYPE") {
            fatal_error!(
                self,
                ParserInvalidDoctypeDecl,
                "The document type declaration must start with '<!DOCTYPE'."
            );
            return Err(XMLError::ParserInvalidDoctypeDecl);
        }
        // skip '<!DOCTYPE'
        self.source.advance(9)?;
        self.locator.update_column(|c| c + 9);

        if self.skip_whitespaces()? == 0 {
            fatal_error!(
                self,
                ParserInvalidDoctypeDecl,
                "Whitespaces are required after '<!DOCTYPE'."
            );
        }

        let mut name = take(&mut self.dtd_name);
        if self.config.is_enable(ParserOption::Namespaces) {
            self.parse_qname(&mut name)?;
        } else {
            self.parse_name(&mut name)?;
        }
        self.dtd_name = name;

        let s = self.skip_whitespaces()?;
        self.grow()?;
        if self.source.is_empty() {
            return Err(XMLError::ParserUnexpectedEOF);
        }

        // If the following character is neither ‘[’ nor ‘>’, then there is an ExternalID.
        let mut system_id = None::<URIString>;
        let mut public_id = None;
        let mut external_subset = None;
        if !matches!(self.source.content_bytes()[0], b'[' | b'>') {
            if s == 0 {
                fatal_error!(
                    self,
                    ParserInvalidDoctypeDecl,
                    "Whitespaces are required between Name and ExternalID."
                );
            }
            let mut buf = String::new();
            self.parse_external_id(&mut buf, &mut public_id)?;
            system_id = Some(URIString::parse(buf)?);
            self.skip_whitespaces()?;
            if self.config.is_enable(ParserOption::ExternalGeneralEntities)
                || self.config.is_enable(ParserOption::Validation)
            {
                external_subset = Some(self.handler.resolve_entity(
                    "[dtd]",
                    public_id.as_deref(),
                    &self.base_uri,
                    system_id.as_deref().unwrap(),
                )?);
            }
        } else if (self.config.is_enable(ParserOption::ExternalGeneralEntities)
            || self.config.is_enable(ParserOption::Validation))
            && let Ok(ext) = self
                .handler
                .get_external_subset(&self.dtd_name, Some(&self.base_uri))
        {
            system_id = ext.system_id().map(ToOwned::to_owned);
            public_id = ext.public_id().map(str::to_owned);
            external_subset = Some(ext);
        }
        if !self.fatal_error_occurred {
            self.handler
                .start_dtd(&self.dtd_name, public_id.as_deref(), system_id.as_deref());
        }

        self.grow()?;
        // try to detect Internal Subset
        if self.source.content_bytes().starts_with(b"[") {
            // skip '['
            self.source.advance(1)?;
            self.locator.update_column(|c| c + 1);

            self.has_internal_subset = true;

            // parse internal subset
            self.parse_int_subset()?;

            self.grow()?;
            if !self.source.content_bytes().starts_with(b"]") {
                fatal_error!(
                    self,
                    ParserInvalidDoctypeDecl,
                    "']' for the end of internal DTD subset is not found."
                );
                return Err(XMLError::ParserInvalidDoctypeDecl);
            }
            // skip ']'
            self.source.advance(1)?;
            self.locator.update_column(|c| c + 1);

            self.skip_whitespaces()?;
        }

        self.grow()?;
        if !self.source.content_bytes().starts_with(b">") {
            fatal_error!(
                self,
                ParserInvalidDoctypeDecl,
                "Document type declaration does not close with '>'."
            );
            return Err(XMLError::ParserInvalidDoctypeDecl);
        }
        // skip '>'
        self.source.advance(1)?;
        self.locator.update_column(|c| c + 1);

        if let Some(external_subset) = external_subset {
            self.has_external_subset = true;
            self.push_source(
                Box::new(external_subset),
                self.base_uri.clone(),
                Some("[dtd]".into()),
                system_id
                    .as_deref()
                    .map(From::from)
                    .unwrap_or_else(|| URIString::parse("").unwrap().into()),
                public_id.as_deref().map(From::from),
            )?;

            if !self.fatal_error_occurred {
                self.handler.start_entity("[dtd]");
            }

            self.parse_ext_subset()?;
            self.grow()?;

            if !self.source.is_empty() {
                fatal_error!(
                    self,
                    ParserEntityIncorrectNesting,
                    "The external DTD subset finish incorrectly."
                );
            }

            self.pop_source()?;

            if !self.fatal_error_occurred {
                self.handler.end_entity();
            }
        } else if system_id.is_some()
            && !self.config.is_enable(ParserOption::Validation)
            && !self.config.is_enable(ParserOption::ExternalGeneralEntities)
            && !self.fatal_error_occurred
        {
            self.handler.skipped_entity("[dtd]");
        }

        if !self.fatal_error_occurred {
            self.handler.end_dtd();
        }

        if self.config.is_enable(ParserOption::Validation) {
            for (name, decl) in self.entities.iter() {
                match decl {
                    EntityDecl::ExternalGeneralUnparsedEntity { notation_name, .. } => {
                        if !self.notations.contains_key(notation_name) {
                            // [VC: Notation Declared]
                            validity_error!(
                                self,
                                ParserUndeclaredNotation,
                                "The notation '{}' is undeclared.",
                                notation_name.as_ref()
                            );
                        }
                    }
                    EntityDecl::InternalGeneralEntity {
                        replacement_text, ..
                    }
                    | EntityDecl::InternalParameterEntity {
                        replacement_text, ..
                    } => {
                        let mut text = replacement_text.as_ref();
                        while let Some((_, rem)) = text.split_once('&') {
                            text = rem;
                            if let Some((entity, rem)) = text.split_once(';') {
                                text = rem;
                                if matches!(
                                    self.entities.get(entity),
                                    Some(EntityDecl::ExternalGeneralUnparsedEntity { .. })
                                ) {
                                    // 4.4.9 Error
                                    error!(
                                        self,
                                        ParserInvalidEntityReference,
                                        "The unparsed entity '{}' appears in EntityValue of the entity '{}'.",
                                        entity,
                                        name
                                    );
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }

            for (elem_name, attr_name, (atttype, _, _)) in self.attlistdecls.iter_all() {
                if let AttributeType::NOTATION(notations) = atttype {
                    if let Some(ContentSpec::EMPTY) = self.elementdecls.get(elem_name) {
                        // [VC: No Notation on Empty Element]
                        validity_error!(
                            self,
                            ParserNotationAttlistDeclOnEmptyElement,
                            "Notation Type attribute '{}' is declared on the empty type element '{}'.",
                            attr_name,
                            elem_name
                        );
                    }

                    for notation in notations {
                        if !self.notations.contains_key(notation) {
                            // [VC: Notation Attributes]
                            validity_error!(
                                self,
                                ParserUndeclaredNotation,
                                "The notation '{}' referenced in the notation type list for attribute '{}' of the element '{}' is not declared.",
                                notation,
                                attr_name,
                                elem_name
                            );
                        }
                    }
                }
            }
        }

        Ok(())
    }

    /// ```text
    /// [28a] DeclSep    ::= PEReference | S                [WFC: PE Between Declarations]
    /// [28b] intSubset  ::= (markupdecl | DeclSep)*
    /// [29]  markupdecl ::= elementdecl | AttlistDecl | EntityDecl | NotationDecl | PI | Comment
    ///                                                     [VC:  Proper Declaration/PE Nesting]
    ///                                                     [WFC: PEs in Internal Subset]
    /// ```
    fn parse_int_subset(&mut self) -> Result<(), XMLError> {
        self.state = ParserState::InInternalSubset;
        self.skip_whitespaces()?;

        let source_id = self.source.source_id();
        loop {
            self.grow()?;
            match self.source.content_bytes() {
                [b'%', ..] => {
                    self.parse_pe_reference()?;
                }
                [b'<', b'?', ..] => self.parse_pi()?,
                [b'<', b'!', b'-', b'-', ..] => self.parse_comment()?,
                [b'<', b'!', b'E', b'L', ..] => self.parse_element_decl()?,
                [b'<', b'!', b'E', b'N', ..] => self.parse_entity_decl()?,
                [b'<', b'!', b'A', ..] => self.parse_attlist_decl()?,
                [b'<', b'!', b'N', ..] => self.parse_notation_decl()?,
                _ => {
                    if self.source.source_id() != source_id {
                        self.pop_source()?;
                        if !self.fatal_error_occurred {
                            self.handler.end_entity();
                        }
                    } else {
                        break Ok(());
                    }
                }
            }

            self.skip_whitespaces()?;
        }
    }

    /// ```text
    /// [75] ExternalID ::= 'SYSTEM' S SystemLiteral
    ///                     | 'PUBLIC' S PubidLiteral S SystemLiteral
    /// [76] NDataDecl  ::= S 'NDATA' S Name        [VC: Notation Declared]
    /// ```
    fn parse_external_id(
        &mut self,
        system_id: &mut String,
        public_id: &mut Option<String>,
    ) -> Result<(), XMLError> {
        self.grow()?;
        match self.source.content_bytes() {
            [b'S', b'Y', b'S', b'T', b'E', b'M', ..] => {
                // skip 'SYSTEM'
                self.source.advance(6)?;
                self.locator.update_column(|c| c + 6);
                let s = if self.state == ParserState::InExternalSubset {
                    self.skip_whitespaces_with_handle_peref(true)?
                } else {
                    self.skip_whitespaces()?
                };
                if s == 0 {
                    fatal_error!(
                        self,
                        ParserInvalidExternalID,
                        "Whitespaces are required after 'SYSTEM' in ExternalID."
                    );
                }
                *public_id = None;
                self.parse_system_literal(system_id)?;
            }
            [b'P', b'U', b'B', b'L', b'I', b'C', ..] => {
                // skip 'PUBLIC'
                self.source.advance(6)?;
                self.locator.update_column(|c| c + 6);
                let s = if self.state == ParserState::InExternalSubset {
                    self.skip_whitespaces_with_handle_peref(true)?
                } else {
                    self.skip_whitespaces()?
                };
                if s == 0 {
                    fatal_error!(
                        self,
                        ParserInvalidExternalID,
                        "Whitespaces are required after 'PUBLIC' in ExternalID."
                    );
                }
                self.parse_pubid_literal(public_id.get_or_insert_default())?;
                let s = if self.state == ParserState::InExternalSubset {
                    self.skip_whitespaces_with_handle_peref(true)?
                } else {
                    self.skip_whitespaces()?
                };
                if s == 0 {
                    fatal_error!(
                        self,
                        ParserInvalidExternalID,
                        "Whitespaces are required after PubidLiteral in ExternalID."
                    );
                }
                self.parse_system_literal(system_id)?;
            }
            _ => {
                fatal_error!(
                    self,
                    ParserInvalidExternalID,
                    "ExternalID must start with 'SYSTEM' or 'PUBLIC'."
                );
                return Err(XMLError::ParserInvalidExternalID);
            }
        }

        // According to the definition in Section 4.2.2 of the specification,
        // if a system identifier is a URI reference with a fragment, it is an error.
        //
        // There is no specification for handling invalid URI references,
        // but here it is treated as an error.
        match URIString::parse(system_id.as_str()) {
            Ok(uri) if uri.fragment().is_some() => {
                error!(
                    self,
                    ParserSystemLiteralWithFragment,
                    "The system ID '{}' has a fragment, but it is not allowed.",
                    system_id
                );
            }
            Ok(_) => {}
            Err(err) => {
                let err = XMLError::from(err);
                error!(
                    self,
                    err, "The system ID '{}' cannot be recognized as a URI.", system_id
                );
            }
        }
        Ok(())
    }
}
