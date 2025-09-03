pub mod literals;
pub mod tokens;

use std::{collections::HashSet, mem::take, str::from_utf8_unchecked, sync::Arc};

use anyxml_uri::uri::URIString;

use crate::{
    CHARDATA_CHUNK_LENGTH, DefaultParserSpec, ENCODING_NAME_LIMIT_LENGTH, ParserSpec,
    XML_NS_NAMESPACE, XML_VERSION_NUM_LIMIT_LENGTH, XML_XML_NAMESPACE, XMLVersion,
    error::XMLError,
    sax::{
        Attribute, AttributeType, DefaultDecl, EntityDecl, Notation,
        contentspec::{ContentSpec, ElementContent, ElementContentStateID},
        error::{error, fatal_error, ns_error, validity_error, warning},
        parser::{ParserOption, ParserState, XMLReader},
        source::InputSource,
    },
};

impl XMLReader<DefaultParserSpec<'_>> {
    /// ```text
    /// [1] document ::= prolog element Misc*
    /// ```
    pub(crate) fn parse_document(&mut self) -> Result<(), XMLError> {
        self.content_handler
            .set_document_locator(self.locator.clone());
        self.content_handler.start_document();
        self.state = ParserState::Parsing;
        self.parse_prolog()?;
        // At this point, the encoding should have been changed if necessary,
        // so set it to compact mode.
        self.source.set_compact_mode();
        self.parse_element()?;
        self.parse_misc()?;
        self.grow()?;
        if !self.source.is_empty() {
            fatal_error!(
                self,
                ParserUnexpectedDocumentContent,
                "Unnecessary document content remains. (Elements, character data, etc.)"
            );
            return Err(XMLError::ParserUnexpectedDocumentContent);
        }

        if !self.fatal_error_occurred
            && self.config.is_enable(ParserOption::Validation)
            && !self.unresolved_ids.is_empty()
        {
            // [VC: IDREF]
            for idref in self.unresolved_ids.drain() {
                validity_error!(
                    self,
                    ParserUnresolvableIDReference,
                    "IDREF '{}' has no referenced ID.",
                    idref
                );
            }
        }
        self.content_handler().end_document();
        Ok(())
    }

    /// ```text
    /// [22] prolog ::= XMLDecl? Misc* (doctypedecl Misc*)?
    /// ```
    pub(crate) fn parse_prolog(&mut self) -> Result<(), XMLError> {
        self.state = ParserState::InXMLDeclaration;
        self.grow()?;
        if self.source.content_bytes().starts_with(b"<?xml") {
            self.parse_xml_decl()?;
        }
        self.state = ParserState::Parsing;
        self.parse_misc()?;
        self.grow()?;
        if self.source.content_bytes().starts_with(b"<!DOCTYPE") {
            self.parse_doctypedecl()?;
            self.state = ParserState::Parsing;
            self.parse_misc()?;
        }
        Ok(())
    }

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
                external_subset = Some(self.entity_resolver.resolve_entity(
                    "[dtd]",
                    public_id.as_deref(),
                    &self.base_uri,
                    system_id.as_deref().unwrap(),
                )?);
            }
        } else if (self.config.is_enable(ParserOption::ExternalGeneralEntities)
            || self.config.is_enable(ParserOption::Validation))
            && let Ok(ext) = self
                .entity_resolver
                .get_external_subset(&self.dtd_name, Some(&self.base_uri))
        {
            system_id = ext.system_id().map(ToOwned::to_owned);
            public_id = ext.public_id().map(str::to_owned);
            external_subset = Some(ext);
        }
        if !self.fatal_error_occurred {
            self.lexical_handler.start_dtd(
                &self.dtd_name,
                public_id.as_deref(),
                system_id.as_deref(),
            );
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
                self.lexical_handler.start_entity("[dtd]");
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
                self.lexical_handler.end_entity();
            }
        } else if system_id.is_some()
            && !self.config.is_enable(ParserOption::Validation)
            && !self.config.is_enable(ParserOption::ExternalGeneralEntities)
            && !self.fatal_error_occurred
        {
            self.content_handler.skipped_entity("[dtd]");
        }

        if !self.fatal_error_occurred {
            self.lexical_handler.end_dtd();
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
    /// [30] extSubset ::= TextDecl? extSubsetDecl
    /// ```
    pub(crate) fn parse_ext_subset(&mut self) -> Result<(), XMLError> {
        let old_state = self.state;
        self.state = ParserState::InTextDeclaration;
        self.grow()?;
        if self.source.content_bytes().starts_with(b"<?xml") {
            self.parse_text_decl()?;
        }
        self.source.set_compact_mode();
        self.state = ParserState::InExternalSubset;
        self.parse_ext_subset_decl()?;

        self.state = old_state;
        Ok(())
    }

    /// ```text
    /// [31] extSubsetDecl ::= ( markupdecl | conditionalSect | DeclSep)*
    /// ```
    pub(crate) fn parse_ext_subset_decl(&mut self) -> Result<(), XMLError> {
        self.grow()?;
        self.skip_whitespaces()?;

        loop {
            self.grow()?;
            match self.source.content_bytes() {
                [b'%', ..] => {
                    let entity_push = self.parse_pe_reference()?;
                    let source_id = self.source.source_id();
                    self.parse_ext_subset_decl()?;
                    if self.source.source_id() != source_id {
                        fatal_error!(
                            self,
                            ParserEntityIncorrectNesting,
                            "A parameter entity in extSubsetDecl is nested incorrectly."
                        );
                        return Err(XMLError::ParserEntityIncorrectNesting);
                    }
                    if entity_push {
                        self.pop_source()?;
                    }
                    if !self.fatal_error_occurred {
                        self.lexical_handler.end_entity();
                    }
                }
                [b'<', b'?', ..] => self.parse_pi()?,
                [b'<', b'!', b'-', b'-', ..] => self.parse_comment()?,
                [b'<', b'!', b'[', ..] => self.parse_conditional_sect()?,
                [b'<', b'!', b'E', b'L', ..] => self.parse_element_decl()?,
                [b'<', b'!', b'E', b'N', ..] => self.parse_entity_decl()?,
                [b'<', b'!', b'A', ..] => self.parse_attlist_decl()?,
                [b'<', b'!', b'N', ..] => self.parse_notation_decl()?,
                _ => break Ok(()),
            }

            self.skip_whitespaces()?;
        }
    }

    /// If an entity is pushed, return `true`, otherwise return `false`.
    ///
    /// # Note
    /// This method adds InputSource to the context depending on whether the configuration
    /// or reference resolution succeeds. If InputSource is added to the context,
    /// [`LexicalHandler::start_entity`](crate::sax::handler::LexicalHandler::start_entity)
    /// is called; otherwise,
    /// [`ContentHandler::skipped_entity`](crate::sax::handler::ContentHandler::skipped_entity)
    /// is called.  \
    /// If an InputSource is added, the caller should remove the InputSource that has reached
    /// EOF and call
    /// [`LexicalHandler::end_entity`](crate::sax::handler::LexicalHandler::end_entity).
    ///
    /// ```text
    /// [69] PEReference ::= '%' Name ';'   [VC:  Entity Declared]
    ///                                     [WFC: No Recursion]
    ///                                     [WFC: In DTD]
    /// ```
    pub(crate) fn parse_pe_reference(&mut self) -> Result<bool, XMLError> {
        // skip '%'
        self.source.advance(1)?;
        self.locator.update_column(|c| c + 1);

        let mut name = "%".to_owned();
        if self.config.is_enable(ParserOption::Namespaces) {
            self.parse_ncname(&mut name)?;
        } else {
            self.parse_name(&mut name)?;
        }

        self.grow()?;
        if !self.source.content_bytes().starts_with(b";") {
            fatal_error!(
                self,
                ParserInvalidEntityReference,
                "A parameter reference does not end with ';'."
            );
            return Err(XMLError::ParserInvalidEntityReference);
        }
        // skip ';'
        self.source.advance(1)?;
        self.locator.update_column(|c| c + 1);

        if self.entity_recursion_check(name.as_str()) {
            // [WFC: No Recursion]
            fatal_error!(
                self,
                ParserEntityRecursion,
                "The parameter entity '{}' appears recursively.",
                name
            );
            return Err(XMLError::ParserEntityRecursion);
        }

        let mut entity_push = false;
        if self.config.is_enable(ParserOption::Validation)
            || self
                .config
                .is_enable(ParserOption::ExternalParameterEntities)
        {
            if let Some(decl) = self.entities.get(&name) {
                match decl {
                    EntityDecl::InternalParameterEntity {
                        base_uri,
                        replacement_text,
                    } => {
                        if self.config.is_enable(ParserOption::Validation) {
                            let source = InputSource::from_content(replacement_text.as_ref());
                            let name: Arc<str> = name.into();
                            self.push_source(
                                Box::new(source),
                                base_uri.clone(),
                                Some(name.clone()),
                                URIString::parse(format!("?internal-parameter-entity.{name}"))?
                                    .into(),
                                None,
                            )?;
                            entity_push = true;

                            if !self.fatal_error_occurred {
                                self.lexical_handler.start_entity(&name);
                            }
                        } else if !self.fatal_error_occurred {
                            self.content_handler.skipped_entity(&name);
                        }
                    }
                    EntityDecl::ExternalParameterEntity {
                        base_uri,
                        system_id,
                        public_id,
                    } => {
                        match self.entity_resolver.resolve_entity(
                            &name,
                            public_id.as_deref(),
                            base_uri.as_ref(),
                            system_id.as_ref(),
                        ) {
                            Ok(source) => {
                                let source = source;
                                let name: Arc<str> = name.into();
                                self.push_source(
                                    Box::new(source),
                                    base_uri.clone(),
                                    Some(name.clone()),
                                    system_id.as_ref().into(),
                                    public_id.as_deref().map(Arc::from),
                                )?;
                                entity_push = true;

                                if !self.fatal_error_occurred {
                                    self.lexical_handler.start_entity(&name);
                                }
                            }
                            Err(err) => {
                                error!(
                                    self,
                                    err,
                                    "The external general entity '{}' cannot be resolved.",
                                    name
                                );
                                if !self.fatal_error_occurred {
                                    self.content_handler.skipped_entity(&name);
                                }
                            }
                        }
                    }
                    _ => unreachable!(),
                }
            } else {
                // [VC: Entity Declared]
                validity_error!(
                    self,
                    ParserEntityNotFound,
                    "The parameter entity '{}' is not declared.",
                    name
                );
                self.content_handler.skipped_entity(&name);
            }
        } else if !self.fatal_error_occurred {
            self.content_handler.skipped_entity(&name);
        }
        Ok(entity_push)
    }

    /// Skip white space while handling parameter entity references.
    ///
    /// If `in_decl` is `true`, whitespace is considered to appear within the markup declaration.
    /// Parameter entity references within markup declarations in the internal subset are WFC
    /// violations, so a fatal error is reported.
    pub(crate) fn skip_whitespaces_with_handle_peref(
        &mut self,
        in_decl: bool,
    ) -> Result<usize, XMLError> {
        let mut s = self.skip_whitespaces()?;
        loop {
            self.grow()?;
            if self.source.content_bytes().starts_with(b"%") {
                let rem = unsafe {
                    // # Safety
                    // `self.source.content_bytes` is a valid UTF-8 byte sequence,
                    // and '%' is a single byte UTF-8 character.
                    // Therefore, `self.source.content_bytes[1..]` is also a valid
                    // UTF-8 byte sequence.
                    from_utf8_unchecked(&self.source.content_bytes()[1..])
                };
                // A single ‘%’ is definitely not a parameter entity, so processing ends.
                if rem.is_empty() || rem.starts_with(|c| self.is_whitespace(c)) {
                    break Ok(s);
                }
                if in_decl && self.state == ParserState::InInternalSubset {
                    fatal_error!(
                        self,
                        ParserInvalidEntityReference,
                        "A parameter entity appears in the markup declaration in an internal subset."
                    );
                    return Err(XMLError::ParserInvalidEntityReference);
                } else {
                    self.parse_pe_reference()?;
                    s += self.skip_whitespaces()?;
                    self.grow()?;
                }
            } else if self.source.is_empty() {
                if self.pop_source().is_err() {
                    // Since there's no popping source, exit the loop.
                    break Ok(s);
                }
                if !self.fatal_error_occurred {
                    self.lexical_handler.end_entity();
                }
                s += 1 + self.skip_whitespaces()?;
                self.grow()?;
            } else {
                break Ok(s + self.skip_whitespaces()?);
            }
        }
    }

    /// ```text
    /// [61] conditionalSect    ::= includeSect | ignoreSect
    /// [62] includeSect        ::= '<![' S? 'INCLUDE' S? '[' extSubsetDecl ']]>'
    ///                                         [VC: Proper Conditional Section/PE Nesting]
    /// [63] ignoreSect         ::= '<![' S? 'IGNORE' S? '[' ignoreSectContents* ']]>'
    ///                                         [VC: Proper Conditional Section/PE Nesting]
    /// [64] ignoreSectContents ::= Ignore ('<![' ignoreSectContents ']]>' Ignore)*
    /// [65] Ignore             ::= Char* - (Char* ('<![' | ']]>') Char*)
    /// ```
    pub(crate) fn parse_conditional_sect(&mut self) -> Result<(), XMLError> {
        self.grow()?;
        if !self.source.content_bytes().starts_with(b"<![") {
            fatal_error!(
                self,
                ParserInvalidConditionalSect,
                "A conditional section in DTD does not start with '<!['."
            );
            return Err(XMLError::ParserInvalidConditionalSect);
        }
        // skip '<!['
        self.source.advance(3)?;
        self.locator.update_column(|c| c + 3);

        let base_source_id = self.source.source_id();
        self.skip_whitespaces_with_handle_peref(true)?;
        self.grow()?;

        match self.source.content_bytes() {
            [b'I', b'N', b'C', b'L', b'U', b'D', b'E', ..] => {
                // skip 'INCLUDE'
                self.source.advance(7)?;
                self.locator.update_column(|c| c + 7);

                self.skip_whitespaces_with_handle_peref(true)?;
                self.grow()?;
                if self.source.source_id() != base_source_id {
                    // [VC: Proper Conditional Section/PE Nesting]
                    validity_error!(
                        self,
                        ParserEntityIncorrectNesting,
                        "A parameter entity in the conditional section is nested incorrectly."
                    );
                }

                if !self.source.content_bytes().starts_with(b"[") {
                    fatal_error!(
                        self,
                        ParserInvalidConditionalSect,
                        "'[' is not found after 'INCLUDE' in a conditional section."
                    );
                    return Err(XMLError::ParserInvalidConditionalSect);
                }
                // skip '['
                self.source.advance(1)?;
                self.locator.update_column(|c| c + 1);

                self.parse_ext_subset_decl()?;

                if !self.source.content_bytes().starts_with(b"]]>") {
                    fatal_error!(
                        self,
                        ParserInvalidConditionalSect,
                        "The conditional section does not end with ']]>'."
                    );
                    return Err(XMLError::ParserInvalidConditionalSect);
                }
                // skip ']]>'
                self.source.advance(3)?;
                self.locator.update_column(|c| c + 3);
            }
            [b'I', b'G', b'N', b'O', b'R', b'E', ..] => {
                // skip 'IGNORE'
                self.source.advance(6)?;
                self.locator.update_column(|c| c + 6);

                self.skip_whitespaces_with_handle_peref(true)?;
                self.grow()?;
                if self.source.source_id() != base_source_id {
                    // [VC: Proper Conditional Section/PE Nesting]
                    validity_error!(
                        self,
                        ParserEntityIncorrectNesting,
                        "A parameter entity in the conditional section is nested incorrectly."
                    );
                }

                if !self.source.content_bytes().starts_with(b"[") {
                    fatal_error!(
                        self,
                        ParserInvalidConditionalSect,
                        "'[' is not found after 'IGNORE' in a conditional section."
                    );
                    return Err(XMLError::ParserInvalidConditionalSect);
                }
                // skip '['
                self.source.advance(1)?;
                self.locator.update_column(|c| c + 1);

                let mut depth = 1;
                while depth > 0 {
                    self.grow()?;
                    if self.source.content_bytes().starts_with(b"<![") {
                        depth += 1;
                        self.source.advance(3)?;
                        self.locator.update_column(|c| c + 3);
                    } else if self.source.content_bytes().starts_with(b"]]>") {
                        depth -= 1;
                        self.source.advance(3)?;
                        self.locator.update_column(|c| c + 3);
                    } else {
                        match self.source.next_char()? {
                            Some('\r') => {
                                if self.source.peek_char()? != Some('\n') {
                                    self.locator.update_line(|l| l + 1);
                                    self.locator.set_column(1);
                                }
                            }
                            Some('\n') => {
                                self.locator.update_line(|l| l + 1);
                                self.locator.set_column(1);
                            }
                            Some(c) if self.is_char(c) => {
                                self.locator.update_column(|c| c + 1);
                            }
                            Some(c) => {
                                fatal_error!(
                                    self,
                                    ParserInvalidCharacter,
                                    "The character '0x{:X}' is not allowed in the XML document.",
                                    c as u32
                                );
                                self.locator.update_column(|c| c + 1);
                            }
                            None => return Err(XMLError::ParserUnexpectedEOF),
                        }
                    }
                }
            }
            _ => {
                fatal_error!(
                    self,
                    ParserInvalidConditionalSect,
                    "A conditional section does not have neither 'INCLUDE' nor 'IGNORE' parameter."
                );
                return Err(XMLError::ParserInvalidConditionalSect);
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
    pub(crate) fn parse_int_subset(&mut self) -> Result<(), XMLError> {
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
                            self.lexical_handler.end_entity();
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
    /// [45] elementdecl ::= '<!ELEMENT' S Name S contentspec S? '>'
    ///                                                 [VC: Unique Element Type Declaration]
    /// [46] contentspec ::= 'EMPTY' | 'ANY' | Mixed | children
    /// ```
    pub(crate) fn parse_element_decl(&mut self) -> Result<(), XMLError> {
        self.grow()?;

        if !self.source.content_bytes().starts_with(b"<!ELEMENT") {
            fatal_error!(
                self,
                ParserInvalidElementDecl,
                "Element declaration must start with '<!ELEMENT'."
            );
            return Err(XMLError::ParserInvalidElementDecl);
        }
        // skip '<!ELEMENT'
        self.source.advance(9)?;
        self.locator.update_column(|c| c + 9);

        let base_source_id = self.source.source_id();
        let is_external_markup = self.is_external_markup();
        if self.skip_whitespaces_with_handle_peref(true)? == 0 {
            fatal_error!(
                self,
                ParserInvalidElementDecl,
                "Whitespaces are required after '<!ELEMENT' in element declaration."
            );
        }

        let mut name = String::new();
        if self.config.is_enable(ParserOption::Namespaces) {
            self.parse_qname(&mut name)?;
        } else {
            self.parse_name(&mut name)?;
        }

        if self.skip_whitespaces_with_handle_peref(true)? == 0 {
            fatal_error!(
                self,
                ParserInvalidElementDecl,
                "Whitespaces are required after Name in element declaration."
            );
        }
        self.grow()?;

        // parse contentspec
        let contentspec = match self.source.content_bytes() {
            [b'E', b'M', b'P', b'T', b'Y', ..] => {
                // skip 'EMPTY'
                self.source.advance(5)?;
                self.locator.update_column(|c| c + 5);
                ContentSpec::EMPTY
            }
            [b'A', b'N', b'Y', ..] => {
                // skip 'ANY'
                self.source.advance(3)?;
                self.locator.update_column(|c| c + 3);
                ContentSpec::ANY
            }
            _ => {
                if !self.source.content_bytes().starts_with(b"(") {
                    fatal_error!(
                        self,
                        ParserInvalidElementDecl,
                        "Element or Mixed content must start with '('."
                    );
                    return Err(XMLError::ParserInvalidElementDecl);
                }
                // skip '('
                self.source.advance(1)?;
                self.locator.update_column(|c| c + 1);
                let model_source_id = self.source.source_id();

                self.skip_whitespaces_with_handle_peref(true)?;
                self.grow()?;

                if self.source.content_bytes().starts_with(b"#PCDATA") {
                    // Mixed Content
                    // [51] Mixed ::= '(' S? '#PCDATA' (S? '|' S? Name)* S? ')*'
                    //              | '(' S? '#PCDATA' S? ')'

                    // skip '#PCDATA'
                    self.source.advance(7)?;
                    self.locator.update_column(|c| c + 7);

                    self.skip_whitespaces_with_handle_peref(true)?;
                    self.grow()?;
                    let mut ret = HashSet::new();
                    if self.source.content_bytes().starts_with(b"|") {
                        // skip '|'
                        self.source.advance(1)?;
                        self.locator.update_column(|c| c + 1);

                        self.skip_whitespaces_with_handle_peref(true)?;
                        let mut buffer = String::new();
                        if self.config.is_enable(ParserOption::Namespaces) {
                            self.parse_qname(&mut buffer)?;
                        } else {
                            self.parse_name(&mut buffer)?;
                        }
                        self.skip_whitespaces_with_handle_peref(true)?;
                        while self.source.content_bytes().starts_with(b"|") {
                            // skip '|'
                            self.source.advance(1)?;
                            self.locator.update_column(|c| c + 1);

                            if !ret.insert(buffer.as_str().into())
                                && self.config.is_enable(ParserOption::Validation)
                            {
                                // [VC: No Duplicate Types]
                                validity_error!(
                                    self,
                                    ParserDuplicateMixedContent,
                                    "'{}' is duplicated as a mixed content of element '{}'.",
                                    buffer,
                                    name
                                );
                            }
                            buffer.clear();
                            self.skip_whitespaces_with_handle_peref(true)?;
                            if self.config.is_enable(ParserOption::Namespaces) {
                                self.parse_qname(&mut buffer)?;
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
                            // [VC: No Duplicate Types]
                            validity_error!(
                                self,
                                ParserDuplicateMixedContent,
                                "'{}' is duplicated as a mixed content of element '{}'.",
                                buffer,
                                name
                            );
                        }
                    }
                    if self.source.source_id() != model_source_id {
                        // [VC: Proper Group/PE Nesting]
                        validity_error!(
                            self,
                            ParserEntityIncorrectNesting,
                            "A parameter entity in an element declaration is nested incorrectly."
                        );
                    }
                    if self.source.content_bytes().starts_with(b")*") {
                        // skip ')*'
                        self.source.advance(2)?;
                        self.locator.update_column(|c| c + 2);
                    } else if self.source.content_bytes().starts_with(b")") {
                        if !ret.is_empty() {
                            fatal_error!(
                                self,
                                ParserInvalidElementDecl,
                                "Mixed Content with elements does not end with ')*'."
                            );
                        }
                        // skip ')'
                        self.source.advance(1)?;
                        self.locator.update_column(|c| c + 1);
                    } else {
                        fatal_error!(
                            self,
                            ParserInvalidElementDecl,
                            "Mixed Content is not wrapped by parentheses correctly."
                        );
                        return Err(XMLError::ParserInvalidElementDecl);
                    }

                    ContentSpec::Mixed(Arc::new(ret))
                } else {
                    // Element Content
                    // [47] children ::= (choice | seq) ('?' | '*' | '+')?
                    // [48] cp       ::= (Name | choice | seq) ('?' | '*' | '+')?
                    // [49] choice   ::= '(' S? cp ( S? '|' S? cp )+ S? ')'	[VC: Proper Group/PE Nesting]
                    // [50] seq      ::= '(' S? cp ( S? ',' S? cp )* S? ')' [VC: Proper Group/PE Nesting]
                    let mut content = ElementContent::new(is_external_markup);
                    let mut buffer = String::new();
                    self.parse_children(&mut buffer, &mut content)?;
                    if content.compile().unwrap() {
                        error!(
                            self,
                            ParserAmbiguousElementContentModel,
                            "The element content of '{}' is ambiguous.",
                            name
                        );
                    }
                    ContentSpec::Children(content)
                }
            }
        };

        self.skip_whitespaces_with_handle_peref(true)?;
        if self.source.source_id() != base_source_id {
            // [VC: Proper Declaration/PE Nesting]
            validity_error!(
                self,
                ParserEntityIncorrectNesting,
                "A parameter entity in an element declaration is nested incorrectly."
            );
        }

        self.grow()?;
        if !self.source.content_bytes().starts_with(b">") {
            fatal_error!(
                self,
                ParserInvalidElementDecl,
                "Element declaration does not end with '>'."
            );
            return Err(XMLError::ParserInvalidElementDecl);
        }
        // skip '>'
        self.source.advance(1)?;
        self.locator.update_column(|c| c + 1);

        if !self.fatal_error_occurred {
            self.decl_handler.element_decl(&name, &contentspec);
        }
        // [VC: Unique Element Type Declaration]
        if self.elementdecls.insert(name, contentspec).is_err()
            && self.config.is_enable(ParserOption::Validation)
        {
            validity_error!(
                self,
                ParserDuplicateElementDecl,
                "An element declaration is duplicated."
            );
        }

        Ok(())
    }

    /// # Note
    /// Due to the implementation of `parse_element_decl`, the leading ‘(’ and subsequent
    /// whitespaces are considered to have already been consumed.  \
    /// This is a compromise due to the constraint that it is necessary to consume the
    /// leading ‘(’ and subsequent whitespace to determine whether ‘#PCDATA’ follows, in
    /// order to distinguish between mixed content and element content.
    ///
    /// ```text
    /// [47] children ::= (choice | seq) ('?' | '*' | '+')?
    /// [48] cp       ::= (Name | choice | seq) ('?' | '*' | '+')?
    /// [49] choice   ::= '(' S? cp ( S? '|' S? cp )+ S? ')' [VC: Proper Group/PE Nesting]
    /// [50] seq      ::= '(' S? cp ( S? ',' S? cp )* S? ')' [VC: Proper Group/PE Nesting]
    /// ```
    pub(crate) fn parse_children(
        &mut self,
        buffer: &mut String,
        model: &mut ElementContent,
    ) -> Result<(), XMLError> {
        let base_source_id = self.source.source_id();
        let mut id = self.parse_cp(buffer, model)?;
        self.skip_whitespaces_with_handle_peref(true)?;

        self.grow()?;
        match self.source.content_bytes() {
            [b'|', ..] => {
                while self.source.content_bytes().starts_with(b"|") {
                    // skip '|'
                    self.source.advance(1)?;
                    self.locator.update_column(|c| c + 1);

                    self.skip_whitespaces_with_handle_peref(true)?;
                    let id2 = self.parse_cp(buffer, model)?;
                    id = model.create_alternation(id, id2);
                    self.skip_whitespaces_with_handle_peref(true)?;
                }
            }
            [b',', ..] => {
                while self.source.content_bytes().starts_with(b",") {
                    // skip ','
                    self.source.advance(1)?;
                    self.locator.update_column(|c| c + 1);

                    self.skip_whitespaces_with_handle_peref(true)?;
                    let id2 = self.parse_cp(buffer, model)?;
                    id = model.create_catenation(id, id2);
                    self.skip_whitespaces_with_handle_peref(true)?;
                }
            }
            [b')', ..] => {}
            [_, ..] => {
                fatal_error!(
                    self,
                    ParserInvalidElementDecl,
                    "Unexpected character is occurred in an element declaration."
                );
                return Err(XMLError::ParserInvalidElementDecl);
            }
            _ => return Err(XMLError::ParserUnexpectedEOF),
        }

        if self.source.source_id() != base_source_id {
            // [VC: Proper Group/PE Nesting]
            validity_error!(
                self,
                ParserEntityIncorrectNesting,
                "A parameter entity in an element declaration is nested incorrectly."
            );
        }

        if !self.source.content_bytes().starts_with(b")") {
            fatal_error!(
                self,
                ParserInvalidElementDecl,
                "A choice or seq in contentspec in an element declaration does not end with '('."
            );
            return Err(XMLError::ParserInvalidElementDecl);
        }
        // skip ')'
        self.source.advance(1)?;
        self.locator.update_column(|c| c + 1);
        buffer.push(')');

        self.grow()?;
        if let [c @ (b'?' | b'*' | b'+'), ..] = self.source.content_bytes() {
            match c {
                b'?' => model.create_zero_or_one(id),
                b'*' => model.create_zero_or_more(id),
                b'+' => model.create_one_or_more(id),
                _ => unreachable!(),
            };
            self.source.advance(1)?;
            self.locator.update_column(|c| c + 1);
        }

        Ok(())
    }

    /// ```text
    /// [48] cp       ::= (Name | choice | seq) ('?' | '*' | '+')?
    /// ```
    pub(crate) fn parse_cp(
        &mut self,
        buffer: &mut String,
        model: &mut ElementContent,
    ) -> Result<ElementContentStateID, XMLError> {
        self.grow()?;

        let base_source_id = self.source.source_id();
        let mut id = if self.source.content_bytes().starts_with(b"(") {
            self.parse_choice_or_seq(buffer, model)?
        } else {
            // parse Name
            buffer.clear();
            if self.config.is_enable(ParserOption::Namespaces) {
                self.parse_qname(buffer)?;
            } else {
                self.parse_name(buffer)?;
            }
            model.create_name(buffer.as_str())
        };

        self.grow()?;
        if let [c @ (b'?' | b'*' | b'+'), ..] = self.source.content_bytes() {
            id = match c {
                b'?' => model.create_zero_or_one(id),
                b'*' => model.create_zero_or_more(id),
                b'+' => model.create_one_or_more(id),
                _ => unreachable!(),
            };
            self.source.advance(1)?;
            self.locator.update_column(|c| c + 1);
        }

        if self.source.source_id() != base_source_id {
            // [VC: Proper Group/PE Nesting]
            validity_error!(
                self,
                ParserEntityIncorrectNesting,
                "A parameter entity in an element declaration is nested incorrectly."
            );
        }

        Ok(id)
    }

    /// ```text
    /// [49] choice   ::= '(' S? cp ( S? '|' S? cp )+ S? ')' [VC: Proper Group/PE Nesting]
    /// [50] seq      ::= '(' S? cp ( S? ',' S? cp )* S? ')' [VC: Proper Group/PE Nesting]
    /// ```
    pub(crate) fn parse_choice_or_seq(
        &mut self,
        buffer: &mut String,
        model: &mut ElementContent,
    ) -> Result<ElementContentStateID, XMLError> {
        self.grow()?;
        if !self.source.content_bytes().starts_with(b"(") {
            fatal_error!(
                self,
                ParserInvalidElementDecl,
                "A choice or seq in contentspec in an element declaration does not start with '('."
            );
            return Err(XMLError::ParserInvalidElementDecl);
        }
        // skip '('
        self.source.advance(1)?;
        self.locator.update_column(|c| c + 1);

        let base_source_id = self.source.source_id();
        self.skip_whitespaces_with_handle_peref(true)?;

        let mut id = self.parse_cp(buffer, model)?;

        self.skip_whitespaces_with_handle_peref(true)?;

        self.grow()?;
        match self.source.content_bytes() {
            [b'|', ..] => {
                while self.source.content_bytes().starts_with(b"|") {
                    // skip '|'
                    self.source.advance(1)?;
                    self.locator.update_column(|c| c + 1);

                    self.skip_whitespaces_with_handle_peref(true)?;
                    let id2 = self.parse_cp(buffer, model)?;
                    id = model.create_alternation(id, id2);
                    self.skip_whitespaces_with_handle_peref(true)?;
                }
            }
            [b',', ..] => {
                while self.source.content_bytes().starts_with(b",") {
                    // skip ','
                    self.source.advance(1)?;
                    self.locator.update_column(|c| c + 1);

                    self.skip_whitespaces_with_handle_peref(true)?;
                    let id2 = self.parse_cp(buffer, model)?;
                    id = model.create_catenation(id, id2);
                    self.skip_whitespaces_with_handle_peref(true)?;
                }
            }
            [b')', ..] => {}
            [_, ..] => {
                fatal_error!(
                    self,
                    ParserInvalidElementDecl,
                    "Unexpected character is occurred in an element declaration."
                );
                return Err(XMLError::ParserInvalidElementDecl);
            }
            _ => return Err(XMLError::ParserUnexpectedEOF),
        }

        if self.source.source_id() != base_source_id {
            // [VC: Proper Group/PE Nesting]
            validity_error!(
                self,
                ParserEntityIncorrectNesting,
                "A parameter entity in an element declaration is nested incorrectly."
            );
        }

        if !self.source.content_bytes().starts_with(b")") {
            fatal_error!(
                self,
                ParserInvalidElementDecl,
                "A choice or seq in contentspec in an element declaration does not end with '('."
            );
            return Err(XMLError::ParserInvalidElementDecl);
        }
        // skip ')'
        self.source.advance(1)?;
        self.locator.update_column(|c| c + 1);

        Ok(id)
    }

    /// ```text
    /// [70] EntityDecl ::= GEDecl | PEDecl
    /// [71] GEDecl     ::= '<!ENTITY' S Name S EntityDef S? '>'
    /// [72] PEDecl     ::= '<!ENTITY' S '%' S Name S PEDef S? '>'
    /// [73] EntityDef  ::= EntityValue | (ExternalID NDataDecl?)
    /// [74] PEDef      ::= EntityValue | ExternalID
    /// ```
    pub(crate) fn parse_entity_decl(&mut self) -> Result<(), XMLError> {
        self.grow()?;

        if !self.source.content_bytes().starts_with(b"<!ENTITY") {
            fatal_error!(
                self,
                ParserInvalidEntityDecl,
                "Entity declaration must start with '<!ENTITY'."
            );
            return Err(XMLError::ParserInvalidEntityDecl);
        }
        // skip '<!ENTITY'
        self.source.advance(8)?;
        self.locator.update_column(|c| c + 8);

        // The base URI of the entity is determined by the position of the first occurrence
        // of '<', so it must be saved at this point before the parameter entity is resolved.
        let base_uri = self.base_uri.clone();
        let base_source_id = self.source.source_id();
        let is_external_markup = self.is_external_markup();

        let mut s = self.skip_whitespaces_with_handle_peref(true)?;
        self.grow()?;
        let mut pe = false;
        if self.source.content_bytes().starts_with(b"%") {
            if s == 0 {
                fatal_error!(
                    self,
                    ParserInvalidEntityDecl,
                    "Whitespaces are required before '%' in a parameter entity declaration."
                );
            }

            pe = true;
            // skip '%'
            self.source.advance(1)?;
            self.locator.update_column(|c| c + 1);

            s = self.skip_whitespaces_with_handle_peref(true)?;
        }

        if s == 0 {
            fatal_error!(
                self,
                ParserInvalidEntityDecl,
                "Whitespaces are required before Name in an entity declaration."
            );
        }

        let mut name = if pe { "%".to_owned() } else { String::new() };
        if self.config.is_enable(ParserOption::Namespaces) {
            self.parse_ncname(&mut name)?;
        } else {
            self.parse_name(&mut name)?;
        }

        if self.skip_whitespaces_with_handle_peref(true)? == 0 {
            fatal_error!(
                self,
                ParserInvalidEntityDecl,
                "Whitespaces are required after Name in entity declaration."
            );
        }

        self.grow()?;
        let decl = match self.source.content_bytes() {
            [b'"' | b'\'', ..] => {
                let mut buffer = String::new();
                self.parse_entity_value(&mut buffer)?;
                self.skip_whitespaces_with_handle_peref(true)?;
                if pe {
                    EntityDecl::InternalParameterEntity {
                        base_uri,
                        replacement_text: buffer.into_boxed_str(),
                    }
                } else {
                    EntityDecl::InternalGeneralEntity {
                        base_uri,
                        replacement_text: buffer.into_boxed_str(),
                        in_external_markup: is_external_markup,
                    }
                }
            }
            [b'S', b'Y', b'S', b'T', b'E', b'M', ..] | [b'P', b'U', b'B', b'L', b'I', b'C', ..] => {
                let mut system_id = String::new();
                let mut public_id = None;
                self.parse_external_id(&mut system_id, &mut public_id)?;

                let s = self.skip_whitespaces_with_handle_peref(true)?;

                // If this is a general entity declaration, NDataDecl may follow.
                // [73] EntityDef ::= EntityValue | (ExternalID NDataDecl?)
                // If this is a parameter entity declaration, '>' must follow.
                // [74] PEDef     ::= EntityValue | ExternalID
                let mut ndata = None::<String>;
                if !pe && !self.source.content_bytes().starts_with(b">") {
                    if s == 0 {
                        fatal_error!(
                            self,
                            ParserInvalidEntityDecl,
                            "Whitespaces are required between ExternalID and NDataDecl."
                        );
                    }

                    if !self.source.content_bytes().starts_with(b"NDATA") {
                        fatal_error!(
                            self,
                            ParserInvalidEntityDecl,
                            "NDataDecl must start with 'NDATA'."
                        );
                        return Err(XMLError::ParserInvalidEntityDecl);
                    }
                    // skip 'NDATA'
                    self.source.advance(5)?;
                    self.locator.update_column(|c| c + 5);

                    if self.skip_whitespaces_with_handle_peref(true)? == 0 {
                        fatal_error!(
                            self,
                            ParserInvalidEntityDecl,
                            "Whitespaces are required after 'NDATA' in entity declaration."
                        );
                    }

                    let ndata = ndata.get_or_insert_default();
                    if self.config.is_enable(ParserOption::Namespaces) {
                        self.parse_ncname(ndata)?;
                    } else {
                        self.parse_name(ndata)?;
                    }

                    // Since notation declarations may appear after this declaration,
                    // the [VC: Notation Declared] check is performed after reading the entire DTD.

                    self.skip_whitespaces_with_handle_peref(true)?;
                }

                let system_id = URIString::parse(system_id)?;
                if !pe && !self.fatal_error_occurred {
                    if let Some(ndata) = ndata.as_deref() {
                        self.dtd_handler.unparsed_entity_decl(
                            &name,
                            public_id.as_deref(),
                            &system_id,
                            ndata,
                        );
                    } else {
                        self.decl_handler.external_entity_decl(
                            &name,
                            public_id.as_deref(),
                            &system_id,
                        );
                    }
                }

                if pe {
                    EntityDecl::ExternalParameterEntity {
                        base_uri,
                        system_id: system_id.into(),
                        public_id: public_id.map(From::from),
                    }
                } else if let Some(notation) = ndata {
                    EntityDecl::ExternalGeneralUnparsedEntity {
                        base_uri,
                        system_id: system_id.into(),
                        public_id: public_id.map(From::from),
                        notation_name: notation.into(),
                    }
                } else {
                    EntityDecl::ExternalGeneralParsedEntity {
                        base_uri,
                        system_id: system_id.into(),
                        public_id: public_id.map(From::from),
                        in_external_markup: is_external_markup,
                    }
                }
            }
            [_, ..] => {
                fatal_error!(
                    self,
                    ParserInvalidEntityDecl,
                    "Neither EntityValue nor ExternalID are found in entity declaration."
                );
                return Err(XMLError::ParserInvalidEntityDecl);
            }
            [] => return Err(XMLError::ParserUnexpectedEOF),
        };

        if self.source.source_id() != base_source_id {
            // [VC: Proper Declaration/PE Nesting]
            validity_error!(
                self,
                ParserEntityIncorrectNesting,
                "A parameter entity in an element declaration is nested incorrectly."
            );
        }
        if !self.source.content_bytes().starts_with(b">") {
            fatal_error!(
                self,
                ParserInvalidEntityDecl,
                "Entity declaration does not end with '>'."
            );
            return Err(XMLError::ParserInvalidEntityDecl);
        }
        // skip '>'
        self.source.advance(1)?;
        self.locator.update_column(|c| c + 1);

        // Duplicate declarations are not errors.
        //
        // Reference: 4.2 Entity Declarations
        // > If the same entity is declared more than once, the first declaration
        // > encountered is binding; at user option, an XML processor MAY issue a
        // > warning if entities are declared multiple times.
        self.entities.insert(name, decl).ok();
        self.has_parameter_entity |= pe;

        Ok(())
    }

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
                self.decl_handler
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
        let atttype = match self.source.content_bytes() {
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

    /// ```text
    /// [82] NotationDecl ::= '<!NOTATION' S Name S (ExternalID | PublicID) S? '>'
    ///                                             [VC: Unique Notation Name]
    /// [83] PublicID     ::= 'PUBLIC' S PubidLiteral
    /// ```
    pub(crate) fn parse_notation_decl(&mut self) -> Result<(), XMLError> {
        self.grow()?;

        if !self.source.content_bytes().starts_with(b"<!NOTATION") {
            fatal_error!(
                self,
                ParserInvalidNotationDecl,
                "Notation declration must start with '<!NOTATION'."
            );
            return Err(XMLError::ParserInvalidNotationDecl);
        }
        // skip '<!NOTATION'
        self.source.advance(10)?;
        self.locator.update_column(|c| c + 10);

        let base_source_id = self.source.source_id();

        if self.skip_whitespaces_with_handle_peref(true)? == 0 {
            fatal_error!(
                self,
                ParserInvalidNotationDecl,
                "Whitespaces are required after '<!NOTATION' in Notation declaration."
            );
        }

        let mut name = String::new();
        if self.config.is_enable(ParserOption::Namespaces) {
            self.parse_ncname(&mut name)?;
        } else {
            self.parse_name(&mut name)?;
        }

        if self.skip_whitespaces_with_handle_peref(true)? == 0 {
            fatal_error!(
                self,
                ParserInvalidNotationDecl,
                "Whitespaces are required after Name in Notation declaration."
            );
        }

        self.grow()?;
        let mut system_id = None::<String>;
        let mut public_id = None::<String>;
        match self.source.content_bytes() {
            [b'S', b'Y', b'S', b'T', b'E', b'M', ..] => {
                // If it starts with “SYSTEM,” it is surely an ExternalID.
                let system_id = system_id.get_or_insert_default();
                self.parse_external_id(system_id, &mut None)?;
                if !self.fatal_error_occurred {
                    let system_id = URIString::parse(system_id)?;
                    self.dtd_handler
                        .notation_decl(&name, None, Some(&system_id));
                }
            }
            [b'P', b'U', b'B', b'L', b'I', b'C', ..] => {
                // If it starts with “PUBLIC,” it is impossible to distinguish between ExternalID
                // and PublicID, so methods such as `parse_external_id` should not be used.
                //
                // [75] ExternalID ::= 'SYSTEM' S SystemLiteral
                //                      | 'PUBLIC' S PubidLiteral S SystemLiteral
                // [83] PublicID   ::= 'PUBLIC' S PubidLiteral

                // skip 'PUBLIC'
                self.source.advance(6)?;
                self.locator.update_column(|c| c + 6);

                if self.skip_whitespaces_with_handle_peref(true)? == 0 {
                    fatal_error!(
                        self,
                        ParserInvalidPubidLiteral,
                        "Whitespaces are required after 'PUBLIC' in Notation declaration."
                    );
                }
                let public_id = public_id.get_or_insert_default();
                self.parse_pubid_literal(public_id)?;

                let s = self.skip_whitespaces_with_handle_peref(true)?;
                self.grow()?;
                let mut system_id = None::<String>;
                // If '>' appears, notation declaration finished.
                if !self.source.content_bytes().starts_with(b">") {
                    // If notation declaration has not finished,
                    // whitespaces are required because SystemLiteral follows.
                    if s == 0 {
                        fatal_error!(
                            self,
                            ParserInvalidPubidLiteral,
                            "Whitespaces are required before SystemLiteral in Notation declaration."
                        );
                    }

                    self.parse_system_literal(system_id.get_or_insert_default())?;
                    self.skip_whitespaces_with_handle_peref(true)?;
                }

                if !self.fatal_error_occurred {
                    let system_id = system_id.map(URIString::parse).transpose()?;
                    self.dtd_handler
                        .notation_decl(&name, Some(public_id), system_id.as_deref());
                }
            }
            _ => {
                fatal_error!(
                    self,
                    ParserInvalidNotationDecl,
                    "Notation declaration must have either ExternalID or PublicID."
                );
                return Err(XMLError::ParserInvalidNotationDecl);
            }
        }

        self.skip_whitespaces_with_handle_peref(true)?;
        if self.source.source_id() != base_source_id {
            // [VC: Proper Declaration/PE Nesting]
            validity_error!(
                self,
                ParserEntityIncorrectNesting,
                "A parameter entity in a notation declaration is nested incorrectly."
            );
        }
        if !self.source.content_bytes().starts_with(b">") {
            fatal_error!(
                self,
                ParserInvalidNotationDecl,
                "A notation declaration does not end with '>'."
            );
            return Err(XMLError::ParserInvalidNotationDecl);
        }
        // skip '>'
        self.source.advance(1)?;
        self.locator.update_column(|c| c + 1);

        match self.notations.entry(name.as_str().into()) {
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(Notation {
                    name: name.into(),
                    system_id: system_id.map(From::from),
                    public_id: public_id.map(From::from),
                });
            }
            std::collections::hash_map::Entry::Occupied(_) => {
                if self.config.is_enable(ParserOption::Validation) {
                    // [VC: Unique Notation Name]
                    validity_error!(
                        self,
                        ParserDuplicateNotationDecl,
                        "The notation '{}' is duplicated.",
                        name
                    );
                }
            }
        }

        Ok(())
    }

    /// ```text
    /// [75] ExternalID ::= 'SYSTEM' S SystemLiteral
    ///                     | 'PUBLIC' S PubidLiteral S SystemLiteral
    /// [76] NDataDecl  ::= S 'NDATA' S Name        [VC: Notation Declared]
    /// ```
    pub(crate) fn parse_external_id(
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

    /// ```text
    /// [23] XMLDecl ::= '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
    /// ```
    pub(crate) fn parse_xml_decl(&mut self) -> Result<(), XMLError> {
        self.state = ParserState::InXMLDeclaration;
        self.grow()?;
        if !self.source.content_bytes().starts_with(b"<?xml") {
            fatal_error!(
                self,
                ParserInvalidXMLDecl,
                "XML declaration must start with '<?xml'."
            );
            return Err(XMLError::ParserInvalidXMLDecl);
        }
        // skip '<?xml'
        self.source.advance(5)?;
        self.locator.update_column(|c| c + 5);

        // parse VersionInfo
        let (version, version_str) = self.parse_version_info(true)?;

        // parse EncodingDecl if exists
        let mut s = self.skip_whitespaces()?;
        self.grow()?;
        let mut encoding = None;
        if self.source.content_bytes().starts_with(b"encoding") {
            if s == 0 {
                fatal_error!(
                    self,
                    ParserInvalidXMLDecl,
                    "Whitespaces are required before 'encoding'."
                );
            }
            encoding = Some(self.parse_encoding_decl(false)?);
            s = self.skip_whitespaces()?;
            self.grow()?;
        }

        // parse SDDecl if exists
        let mut standalone = None;
        if self.source.content_bytes().starts_with(b"standalone") {
            if s == 0 {
                fatal_error!(
                    self,
                    ParserInvalidXMLDecl,
                    "Whitespaces are required before 'standalone'."
                );
            }
            standalone = Some(self.parse_sddecl(false)?);
            self.skip_whitespaces()?;
            self.grow()?;
        }

        if !self.source.content_bytes().starts_with(b"?>") {
            fatal_error!(
                self,
                ParserInvalidXMLDecl,
                "XMLDecl is not closed with '?>'."
            );
            return Err(XMLError::ParserInvalidXMLDecl);
        }
        // skip '?>'
        self.source.advance(2)?;
        self.locator.update_column(|c| c + 2);

        // If an encoding is provided from an external source, it is used as a priority.
        // If not, `self.encoding` is `None`, so `self.encoding` is initialized with the value
        // obtained from the XML declaration, and the decoder for `self.source` is switched.
        if let Some(encoding) = encoding.as_deref()
            && self.encoding.is_none()
            && let Err(err) = self.source.switch_encoding(encoding)
        {
            fatal_error!(
                self,
                ParserUnsupportedEncoding,
                "The declared encoding '{}' is not supported.",
                encoding
            );
            return Err(err);
        }

        if !self.fatal_error_occurred {
            self.content_handler
                .declaration(&version_str, encoding.as_deref(), standalone);
        }
        self.version = version;
        self.standalone = standalone;
        if self.encoding.is_none() {
            self.encoding = encoding;
        }
        Ok(())
    }

    /// ```text
    /// [24] VersionInfo ::= S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
    /// ```
    pub(crate) fn parse_version_info(
        &mut self,
        need_trim_whitespace: bool,
    ) -> Result<(XMLVersion, String), XMLError> {
        if need_trim_whitespace && self.skip_whitespaces()? == 0 {
            fatal_error!(
                self,
                ParserInvalidEncodingDecl,
                "Whitespaces are required before 'version' for VersionDecl."
            );
        }

        if !self.source.content_bytes().starts_with(b"version") {
            fatal_error!(
                self,
                ParserInvalidXMLVersion,
                "VersionInfo must start with 'version'."
            );
            return Err(XMLError::ParserInvalidXMLVersion);
        }
        // skip 'version'
        self.source.advance(7)?;
        self.locator.update_column(|c| c + 7);

        self.skip_whitespaces()?;
        if !self.source.content_bytes().starts_with(b"=") {
            fatal_error!(
                self,
                ParserInvalidXMLDecl,
                "'=' is not found after 'version' in VersionInfo."
            );
            return Err(XMLError::ParserInvalidXMLDecl);
        }
        // skip '='
        self.source.advance(1)?;
        self.locator.update_column(|c| c + 1);
        self.skip_whitespaces()?;

        let quote = match self.source.next_char()? {
            Some(c @ ('"' | '\'')) => c,
            _ => {
                fatal_error!(
                    self,
                    ParserInvalidXMLDecl,
                    "The quotation marks in the version number are incorrect."
                );
                return Err(XMLError::ParserInvalidXMLDecl);
            }
        };
        self.locator.update_column(|c| c + 1);

        self.grow()?;
        let content = self.source.content_bytes();
        let limit = content.len().min(XML_VERSION_NUM_LIMIT_LENGTH);
        let mut major = 0;
        while major < limit && content[major].is_ascii_digit() {
            major += 1;
        }
        if major > 1 || content[0] != b'1' {
            fatal_error!(
                self,
                ParserInvalidXMLVersion,
                "XML major version number must be '1'."
            );
        } else if content[major] != b'.' {
            fatal_error!(
                self,
                ParserInvalidXMLVersion,
                "Invalid XML version number is found."
            );
            return Err(XMLError::ParserInvalidXMLVersion);
        }
        let mut minor = major + 1;
        while minor < limit && content[minor].is_ascii_digit() {
            minor += 1;
        }
        if minor == limit {
            fatal_error!(
                self,
                ParserTooLongXMLVersionNumber,
                "Too long XML version number is found."
            );
            return Err(XMLError::ParserTooLongXMLVersionNumber);
        }
        let version = if major + 1 < minor {
            match content[..minor] {
                [b'1', b'.', b'0'] => XMLVersion::XML10,
                _ => XMLVersion::Unknown,
            }
        } else {
            fatal_error!(
                self,
                ParserInvalidXMLVersion,
                "XML minor version number is not found."
            );
            return Err(XMLError::ParserInvalidXMLVersion);
        };
        if version == XMLVersion::Unknown {
            warning!(
                self,
                ParserUnsupportedXMLVersion,
                "Unsupported XML version number is found. Fallback to XML 1.0."
            );
        }
        if content[minor] != quote as u8 {
            fatal_error!(
                self,
                ParserInvalidXMLDecl,
                "The quotation marks in the version number are incorrect."
            );
            return Err(XMLError::ParserInvalidXMLDecl);
        }
        let version_str = unsafe {
            // # Safety
            // `content[..minor]` contains only ASCII digit or '.',
            // so this operation is safe.
            String::from_utf8_unchecked(content[..minor].to_owned())
        };
        self.source.advance(minor + 1)?;
        self.locator.update_column(|c| c + minor + 1);

        Ok((version, version_str))
    }

    /// ```text
    /// [80] EncodingDecl ::= S 'encoding' Eq ('"' EncName '"' | "'" EncName "'" )
    /// ```
    pub(crate) fn parse_encoding_decl(
        &mut self,
        need_trim_whitespace: bool,
    ) -> Result<String, XMLError> {
        if need_trim_whitespace && self.skip_whitespaces()? == 0 {
            fatal_error!(
                self,
                ParserInvalidEncodingDecl,
                "Whitespaces are required before 'encoding' for EncodingDecl."
            );
        }

        if !self.source.content_bytes().starts_with(b"encoding") {
            fatal_error!(
                self,
                ParserInvalidEncodingDecl,
                "'encoding' is not found for EncodingDecl."
            );
            return Err(XMLError::ParserInvalidEncodingDecl);
        }
        // skip 'encoding'
        self.source.advance(8)?;
        self.locator.update_column(|c| c + 8);

        self.skip_whitespaces()?;
        if self.source.next_char()? != Some('=') {
            fatal_error!(
                self,
                ParserInvalidEncodingDecl,
                "'=' is not found after 'encoding' in EncodingDecl."
            );
            return Err(XMLError::ParserInvalidEncodingDecl);
        }
        self.locator.update_column(|c| c + 1);
        self.skip_whitespaces()?;

        let quote = match self.source.next_char()? {
            Some(c @ ('"' | '\'')) => c,
            _ => {
                fatal_error!(
                    self,
                    ParserInvalidEncodingDecl,
                    "The quotation marks in the encoding name are incorrect."
                );
                return Err(XMLError::ParserInvalidEncodingDecl);
            }
        };
        self.locator.update_column(|c| c + 1);

        let encoding = self.parse_enc_name()?;
        self.grow()?;

        match self.source.next_char()? {
            Some(c) if c == quote => {
                self.locator.update_column(|c| c + 1);
            }
            Some(_) => {
                fatal_error!(
                    self,
                    ParserInvalidEncodingDecl,
                    "The quotation marks in the encoding name are incorrect."
                );
                return Err(XMLError::ParserInvalidEncodingDecl);
            }
            _ => {
                // If we call `grow` just before and `source` is empty,
                // `source` has probably reached EOF.
                return if self.source.is_empty() {
                    Err(XMLError::ParserUnexpectedEOF)
                } else {
                    fatal_error!(
                        self,
                        ParserInvalidEncodingDecl,
                        "The quotation marks in the encoding name are incorrect."
                    );
                    Err(XMLError::ParserInvalidEncodingDecl)
                };
            }
        }

        Ok(encoding)
    }

    /// ```text
    /// [32] SDDecl ::= S 'standalone' Eq (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no') '"'))
    ///                                                 [VC: Standalone Document Declaration]
    /// ```
    pub(crate) fn parse_sddecl(&mut self, need_trim_whitespace: bool) -> Result<bool, XMLError> {
        if need_trim_whitespace && self.skip_whitespaces()? == 0 {
            fatal_error!(
                self,
                ParserInvalidSDDecl,
                "Whitespaces are required before 'standalone' for SDDecl."
            );
        }

        self.grow()?;
        let content = self.source.content_bytes();
        if !content.starts_with(b"standalone") {
            fatal_error!(
                self,
                ParserInvalidSDDecl,
                "'standalone' is not found for SDDecl."
            );
            return Err(XMLError::ParserInvalidSDDecl);
        }
        // skip 'standalone'
        self.source.advance(10)?;
        self.locator.update_column(|c| c + 10);

        self.skip_whitespaces()?;
        self.grow()?;
        if !self.source.content_bytes().starts_with(b"=") {
            fatal_error!(self, ParserInvalidSDDecl, "'=' is not found for SDDecl.");
            return Err(XMLError::ParserInvalidSDDecl);
        }
        // skip '='
        self.source.advance(1)?;
        self.locator.update_column(|c| c + 10);

        let quote = match self.source.next_char()? {
            Some(c @ ('"' | '\'')) => c,
            _ => {
                fatal_error!(
                    self,
                    ParserInvalidSDDecl,
                    "The quotation marks in the standalone declaration are incorrect."
                );
                return Err(XMLError::ParserInvalidSDDecl);
            }
        };
        self.locator.update_column(|c| c + 1);

        let ret = match self.source.content_bytes() {
            [b'y', b'e', b's', ..] => {
                self.source.advance(3)?;
                self.locator.update_column(|c| c + 3);
                true
            }
            [b'n', b'o', ..] => {
                self.source.advance(2)?;
                self.locator.update_column(|c| c + 2);
                false
            }
            _ => {
                fatal_error!(
                    self,
                    ParserInvalidSDDecl,
                    "The value of SDDecl must be either 'yes' or 'no'."
                );
                return Err(XMLError::ParserInvalidXMLDecl);
            }
        };

        if self.source.next_char()? != Some(quote) {
            fatal_error!(
                self,
                ParserInvalidSDDecl,
                "The quotation marks in the standalone declaration are incorrect."
            );
            return Err(XMLError::ParserInvalidSDDecl);
        }
        self.locator.update_column(|c| c + 1);

        Ok(ret)
    }

    /// ```text
    /// [81] EncName ::= [A-Za-z] ([A-Za-z0-9._] | '-')*
    ///                                     /* Encoding name contains only Latin characters */
    /// ```
    pub(crate) fn parse_enc_name(&mut self) -> Result<String, XMLError> {
        self.grow()?;

        let content = self.source.content_bytes();
        if content.is_empty() {
            // If we call `grow` just before and `source` is empty,
            // `source` has probably reached EOF.
            return if self.source.is_empty() {
                Err(XMLError::ParserUnexpectedEOF)
            } else {
                fatal_error!(
                    self,
                    ParserInvalidEncodingName,
                    "Data that may not be accepted as an encoding name has been detected."
                );
                Err(XMLError::ParserInvalidEncodingName)
            };
        }

        if !content[0].is_ascii_alphabetic() {
            fatal_error!(
                self,
                ParserInvalidEncodingName,
                "The first character of an encoding name must be ASCII alphabetic."
            );
        }

        let limit = ENCODING_NAME_LIMIT_LENGTH.min(content.len());
        let mut cur = 0;
        while cur < limit
            && (content[cur].is_ascii_alphanumeric() || matches!(content[cur], b'.' | b'_' | b'-'))
        {
            cur += 1;
        }

        if cur == ENCODING_NAME_LIMIT_LENGTH {
            fatal_error!(
                self,
                PraserTooLongEncodingName,
                "Too long encoding name is found."
            );
            return Err(XMLError::PraserTooLongEncodingName);
        } else if cur == content.len() {
            return if self.source.is_empty() {
                Err(XMLError::ParserUnexpectedEOF)
            } else {
                fatal_error!(
                    self,
                    ParserInvalidEncodingName,
                    "Data that may not be accepted as an encoding name has been detected."
                );
                Err(XMLError::ParserInvalidEncodingName)
            };
        }

        let ret = unsafe {
            // # Safety
            // `content[..cur]` contains only ASCII characters,
            // so this operation is safe.
            String::from_utf8_unchecked(content[..cur].to_owned())
        };
        self.source.advance(cur)?;
        self.locator.update_column(|c| c + cur);
        Ok(ret)
    }

    /// ```text
    /// [27] Misc ::= Comment | PI | S
    /// ```
    pub(crate) fn parse_misc(&mut self) -> Result<(), XMLError> {
        self.skip_whitespaces()?;
        self.grow()?;

        loop {
            match self.source.content_bytes() {
                [b'<', b'!', b'-', b'-', ..] => self.parse_comment()?,
                [b'<', b'?', ..] => self.parse_pi()?,
                _ => break Ok(()),
            }
            self.skip_whitespaces()?;
            self.grow()?;
        }
    }

    /// ```text
    /// [15] Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
    /// ```
    pub(crate) fn parse_comment(&mut self) -> Result<(), XMLError> {
        self.grow()?;
        if !self.source.content_bytes().starts_with(b"<!--") {
            fatal_error!(
                self,
                ParserInvalidComment,
                "Comment does not start with '<!--'."
            );
            return Err(XMLError::ParserInvalidComment);
        }
        // skip '<!--'
        self.source.advance(4)?;
        self.locator.update_column(|c| c + 4);

        if let Some(Some((_, validator))) = self.validation_stack.last_mut() {
            // [VC: Element Valid]
            // Markup matching the `Misc` category is allowed as element content.
            validator.push_misc();
        }

        self.grow()?;
        let mut buffer = String::new();
        while !self.source.content_bytes().starts_with(b"-->") {
            let next = self.source.next_char()?;
            match next {
                Some('-') => {
                    if self.source.peek_char()? == Some('-') {
                        fatal_error!(
                            self,
                            ParserInvalidComment,
                            "Comment must not contain '--' except for delimiters."
                        );
                    }
                    buffer.push('-');
                }
                Some('\r') => {
                    // If the next character is not a line feed, normalize it to a line feed.
                    // If so, treat it as a single line feed together with the next line feed
                    // and do nothing.
                    if self.source.peek_char()? != Some('\n') {
                        self.locator.update_line(|l| l + 1);
                        self.locator.set_column(1);
                        buffer.push('\n');
                    }
                }
                Some('\n') => {
                    self.locator.update_line(|l| l + 1);
                    self.locator.set_column(1);
                    buffer.push('\n');
                }
                Some(c) if self.is_char(c) => {
                    self.locator.update_column(|c| c + 1);
                    buffer.push(c)
                }
                Some(c) => {
                    self.locator.update_column(|c| c + 1);
                    fatal_error!(
                        self,
                        ParserInvalidCharacter,
                        "A character '0x{:X}' is not allowed in XML documents.",
                        c as u32
                    );
                }
                None => {
                    return Err(XMLError::ParserUnexpectedEOF);
                }
            }

            if buffer.len() >= CHARDATA_CHUNK_LENGTH {
                if !self.fatal_error_occurred {
                    self.lexical_handler.comment(&buffer);
                }
                buffer.clear();
            }
            if self.source.content_bytes().len() < 3 {
                self.grow()?;
            }
        }

        if !buffer.is_empty() && !self.fatal_error_occurred {
            self.lexical_handler.comment(&buffer);
        }

        if !self.source.content_bytes().starts_with(b"-->") {
            fatal_error!(
                self,
                ParserInvalidComment,
                "Comment does not end with '-->'."
            );
            return Err(XMLError::ParserInvalidComment);
        }
        // skip '-->'
        self.source.advance(3)?;
        self.locator.update_column(|c| c + 3);

        Ok(())
    }

    /// ```text
    /// [16] PI       ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
    /// [17] PITarget ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
    /// ```
    pub(crate) fn parse_pi(&mut self) -> Result<(), XMLError> {
        self.grow()?;
        if !self.source.content_bytes().starts_with(b"<?") {
            fatal_error!(
                self,
                ParserInvalidProcessingInstruction,
                "PI does not start with '<?'."
            );
            return Err(XMLError::ParserInvalidProcessingInstruction);
        }
        // skip '<?'
        self.source.advance(2)?;
        self.locator.update_column(|c| c + 2);

        if let Some(Some((_, validator))) = self.validation_stack.last_mut() {
            // [VC: Element Valid]
            // Markup matching the `Misc` category is allowed as element content.
            validator.push_misc();
        }

        let mut target = String::new();
        if self.config.is_enable(ParserOption::Namespaces) {
            self.parse_ncname(&mut target)?;
        } else {
            self.parse_name(&mut target)?;
        }

        if target.eq_ignore_ascii_case("xml") {
            fatal_error!(
                self,
                ParserUnacceptablePITarget,
                "PI target '{}' is not allowed.",
                target
            );
        }

        let s = self.skip_whitespaces()?;
        self.grow()?;
        if self.source.content_bytes().starts_with(b"?>") {
            // skip '?>'
            self.source.advance(2)?;
            self.locator.update_column(|c| c + 2);

            if !self.fatal_error_occurred {
                self.content_handler.processing_instruction(&target, None);
            }

            return Ok(());
        }

        if s == 0 {
            fatal_error!(
                self,
                ParserInvalidProcessingInstruction,
                "Whitespaces are required between PI target and data."
            );
        }

        let mut data = String::new();
        self.grow()?;
        while !self.source.content_bytes().starts_with(b"?>") {
            match self.source.next_char()? {
                Some('\r') => {
                    if self.source.peek_char()? != Some('\n') {
                        self.locator.update_line(|l| l + 1);
                        self.locator.set_column(1);
                        data.push('\n');
                    }
                }
                Some('\n') => {
                    self.locator.update_line(|l| l + 1);
                    self.locator.set_column(1);
                    data.push('\n');
                }
                Some(c) if self.is_char(c) => {
                    self.locator.update_column(|c| c + 1);
                    data.push(c);
                }
                Some(c) => {
                    self.locator.update_column(|c| c + 1);
                    data.push(c);
                    fatal_error!(
                        self,
                        ParserInvalidCharacter,
                        "A character '0x{:X}' is not allowed in XML document.",
                        c as u32
                    );
                }
                None => return Err(XMLError::ParserUnexpectedEOF),
            }
            if self.source.content_bytes().len() < 2 {
                self.grow()?;
            }
        }

        if !self.source.content_bytes().starts_with(b"?>") {
            fatal_error!(
                self,
                ParserInvalidProcessingInstruction,
                "PI does not close with '?>'."
            );
            return Err(XMLError::ParserInvalidProcessingInstruction);
        }
        // skip '?>'
        self.source.advance(2)?;
        self.locator.update_column(|c| c + 2);

        if !self.fatal_error_occurred {
            self.content_handler
                .processing_instruction(&target, Some(&data));
        }

        Ok(())
    }

    /// ```text
    /// [39] element ::= EmptyElemTag | STag content ETag       [WFC: Element Type Match]
    ///                                                         [VC:  Element Valid]
    /// [40] STag ::= '<' Name (S Attribute)* S? '>'            [WFC: Unique Att Spec]
    /// [42] ETag ::= '</' Name S? '>'
    /// [44] EmptyElemTag ::= '<' Name (S Attribute)* S? '/>'   [WFC: Unique Att Spec]
    /// ```
    pub(crate) fn parse_element(&mut self) -> Result<(), XMLError> {
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

        let mut name = String::new();
        let mut prefix_length = 0;
        if self.config.is_enable(ParserOption::Namespaces) {
            prefix_length = self.parse_qname(&mut name)?;
        } else {
            self.parse_name(&mut name)?;
        }

        match self.validation_stack.last_mut() {
            Some(Some((_, validator))) => {
                if self.config.is_enable(ParserOption::Validation) {
                    validator.push_name(&name);
                }
            }
            Some(None) => {
                // The parent element is not declared.
            }
            None => {
                // This is root element or the validation option is disabled.
                // If the validation option is enabled, Check if element name is equal to dtd name.
                // [VC: Root Element Type]
                if self.config.is_enable(ParserOption::Validation) && name != self.dtd_name {
                    validity_error!(
                        self,
                        ParserMismatchElementType,
                        "The document type declaration name does not match the document element type."
                    );
                }
            }
        }

        if let Some(contentspec) = self.elementdecls.get_mut(&name) {
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

        let old_ns_stack_depth = self.namespaces.len();
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
                let declared = self.normalize_att_value(&name, &att_name, &mut att_value, None);
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
                    self.attlistdecls.get(&name, &att.qname)
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
        if let Some(decls) = self.attlistdecls.attlist(&name) {
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
                    self.content_handler.start_prefix_mapping(None, &att.value);
                } else {
                    self.content_handler.start_prefix_mapping(
                        Some(&att.qname[..att.qname.len() - len - 1]),
                        &att.value,
                    );
                }
            }
            if self.config.is_enable(ParserOption::Namespaces) {
                if prefix_length > 0 {
                    if let Some(&pos) = self.prefix_map.get(&name[..prefix_length]) {
                        self.content_handler.start_element(
                            Some(&self.namespaces[pos].0),
                            Some(&name[prefix_length + 1..]),
                            &name,
                            &atts,
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
                        self.content_handler.start_element(
                            Some(&self.namespaces[pos].0),
                            Some(&name),
                            &name,
                            &atts,
                        );
                    } else {
                        self.content_handler
                            .start_element(None, Some(&name), &name, &atts);
                    }
                }
            } else {
                self.content_handler.start_element(None, None, &name, &atts);
            }
        }

        if self.source.content_bytes().starts_with(b"/>") {
            // This is an empty tag.

            // skip '/>'
            self.source.advance(2)?;
            self.locator.update_column(|c| c + 2);
        } else {
            // This is a start tag.

            // skip '>'
            self.source.advance(1)?;
            self.locator.update_column(|c| c + 1);

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
                        self.content_handler.end_element(
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
                        self.content_handler.end_element(
                            Some(&self.namespaces[pos].0),
                            Some(&name),
                            &name,
                        );
                    } else {
                        self.content_handler.end_element(None, Some(&name), &name);
                    }
                }
            } else {
                self.content_handler.end_element(None, None, &name);
            }
        }

        // resume namespace stack
        while self.namespaces.len() > old_ns_stack_depth {
            let (pre, _, old_position) = self.namespaces.pop().unwrap();
            if !self.fatal_error_occurred {
                self.content_handler
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

    /// ```text
    /// [43] content ::= CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
    /// ```
    pub(crate) fn parse_content(&mut self) -> Result<(), XMLError> {
        loop {
            self.grow()?;
            if self.source.content_bytes().is_empty() {
                break Ok(());
            }

            match self.source.content_bytes() {
                [b'<', b'?', ..] => self.parse_pi()?,
                [b'<', b'!', b'-', b'-', ..] => self.parse_comment()?,
                [b'<', b'!', b'[', b'C', b'D', b'A', b'T', b'A', b'[', ..] => {
                    self.parse_cdsect()?
                }
                [b'<', b'/', ..] => break Ok(()),
                [b'<', ..] => self.parse_element()?,
                [b'&', b'#', ..] => {
                    // Character references are treated as part of the character data.
                    self.parse_char_data()?
                }
                [b'&', ..] => self.parse_entity_ref_in_content()?,
                _ => self.parse_char_data()?,
            }

            if let Some(Some((_, validator))) = self.validation_stack.last_mut() {
                // If we reach this point, we should have read at least one markup or character data.
                // Since Misc does not affect the validation of elements other than those of type EMPTY,
                // it is safe to assume that Misc appears here.
                validator.push_misc();
            }
        }
    }

    /// # Note
    /// This method parses and expands assuming that entity references appear in the content.  \
    /// Entity references appearing in attribute values are outside the scope of this method.
    ///
    /// ```text
    /// [68] EntityRef ::= '&' Name ';'     [WFC: Entity Declared]
    ///                                     [VC:  Entity Declared]
    ///                                     [WFC: Parsed Entity]
    ///                                     [WFC: No Recursion]
    /// ```
    pub(crate) fn parse_entity_ref_in_content(&mut self) -> Result<(), XMLError> {
        self.grow()?;

        if !self.source.content_bytes().starts_with(b"&") {
            fatal_error!(
                self,
                ParserInvalidEntityReference,
                "The entity reference does not start with '&'."
            );
            return Err(XMLError::ParserInvalidEntityReference);
        }
        // skip '&'
        self.source.advance(1)?;
        self.locator.update_column(|c| c + 1);

        let mut name = String::new();
        if self.config.is_enable(ParserOption::Namespaces) {
            self.parse_ncname(&mut name)?;
        } else {
            self.parse_name(&mut name)?;
        }

        self.grow()?;
        if !self.source.content_bytes().starts_with(b";") {
            fatal_error!(
                self,
                ParserInvalidEntityReference,
                "The entity reference does not end with ';'."
            );
            return Err(XMLError::ParserInvalidEntityReference);
        }
        // skip ';'
        self.source.advance(1)?;
        self.locator.update_column(|c| c + 1);

        if let Some(decl) = self.entities.get(name.as_str()) {
            if self.entity_recursion_check(name.as_str()) {
                // [WFC: No Recursion]
                fatal_error!(
                    self,
                    ParserEntityRecursion,
                    "The entity '{}' appears recursively.",
                    name
                );
                return Err(XMLError::ParserEntityRecursion);
            }
            match decl {
                EntityDecl::InternalGeneralEntity {
                    base_uri,
                    replacement_text,
                    in_external_markup,
                } => {
                    if *in_external_markup && self.standalone == Some(true) {
                        // [WFC: Entity Declared]
                        fatal_error!(
                            self,
                            ParserUndeclaredEntityReference,
                            "standalone='yes', but it does not reference any entities declared in the internal DTD."
                        );
                    } else {
                        let source = InputSource::from_content(replacement_text.as_ref());
                        let name: Arc<str> = name.into();
                        self.push_source(
                            Box::new(source),
                            base_uri.clone(),
                            Some(name.clone()),
                            URIString::parse(format!("?internal-entity.{name}"))?.into(),
                            None,
                        )?;

                        if !self.fatal_error_occurred {
                            self.lexical_handler.start_entity(&name);
                        }

                        self.parse_content()?;
                        self.grow()?;

                        if !self.source.is_empty() {
                            fatal_error!(
                                self,
                                ParserEntityIncorrectNesting,
                                "The entity '{}' is nested incorrectly.",
                                name
                            );
                        }

                        self.pop_source()?;
                        if !self.fatal_error_occurred {
                            self.lexical_handler.end_entity();
                        }
                    }
                }
                EntityDecl::ExternalGeneralParsedEntity {
                    base_uri,
                    system_id,
                    public_id,
                    in_external_markup,
                } => {
                    if *in_external_markup && self.standalone == Some(true) {
                        // [WFC: Entity Declared]
                        fatal_error!(
                            self,
                            ParserUndeclaredEntityReference,
                            "standalone='yes', but it does not reference any entities declared in the internal DTD."
                        );
                    } else if self.config.is_enable(ParserOption::ExternalGeneralEntities)
                        || self.config.is_enable(ParserOption::Validation)
                    {
                        match self.entity_resolver.resolve_entity(
                            &name,
                            public_id.as_deref(),
                            base_uri.as_ref(),
                            system_id.as_ref(),
                        ) {
                            Ok(source) => {
                                let source = source;
                                let name: Arc<str> = name.into();
                                self.push_source(
                                    Box::new(source),
                                    base_uri.clone(),
                                    Some(name.clone()),
                                    system_id.as_ref().into(),
                                    public_id.as_deref().map(Arc::from),
                                )?;

                                if !self.fatal_error_occurred {
                                    self.lexical_handler.start_entity(&name);
                                }

                                self.parse_ext_parsed_ent()?;
                                self.grow()?;

                                if !self.source.is_empty() {
                                    fatal_error!(
                                        self,
                                        ParserEntityIncorrectNesting,
                                        "The entity '{}' is nested incorrectly.",
                                        name
                                    );
                                }

                                self.pop_source()?;
                                if !self.fatal_error_occurred {
                                    self.lexical_handler.end_entity();
                                }
                            }
                            Err(err) => {
                                error!(
                                    self,
                                    err,
                                    "The external general entity '{}' cannot be resolved.",
                                    name
                                );
                            }
                        }
                    } else if !self.fatal_error_occurred {
                        self.content_handler.skipped_entity(&name);
                    }
                }
                EntityDecl::ExternalGeneralUnparsedEntity { .. } => {
                    // [WFC: Parsed Entity]
                    fatal_error!(
                        self,
                        ParserInvalidEntityReference,
                        "The unparsed entity '{}' cannot be referred.",
                        name
                    );
                }
                EntityDecl::InternalParameterEntity { .. }
                | EntityDecl::ExternalParameterEntity { .. } => {
                    // The fact that we have reached this point suggests that the general
                    // entity has been mistakenly registered as a parameter entity somewhere.
                    unreachable!("Internal error: Reference name: {name}");
                }
            }
        } else {
            if self.standalone == Some(true)
                || (!self.has_internal_subset && !self.has_external_subset)
                || (!self.has_external_subset && !self.has_parameter_entity)
            {
                // [WFC: Entity Declared]
                fatal_error!(
                    self,
                    ParserEntityNotFound,
                    "The entity '{}' is not declared.",
                    name
                );
            } else if self.config.is_enable(ParserOption::Validation) {
                // [VC: Entity Declared]
                validity_error!(
                    self,
                    ParserEntityNotFound,
                    "The entity '{}' is not declared.",
                    name
                );
            }

            if !self.fatal_error_occurred {
                self.content_handler.skipped_entity(&name);
            }
        }
        Ok(())
    }

    /// # Note
    /// It is assumed that the `InputSource` corresponding to the External Parsed Entity
    /// to be parsed has been set by `push_source`.  \
    /// In addition, `start_entity` and `end_entity` are not reported. This is the duty
    /// of the caller.
    ///
    /// ```text
    /// [78] extParsedEnt ::= TextDecl? content
    /// ```
    pub(crate) fn parse_ext_parsed_ent(&mut self) -> Result<(), XMLError> {
        self.state = ParserState::InTextDeclaration;
        self.grow()?;
        // Save version and encoding because they may be overwritten by a text declaration.
        let version = self.version;
        let encoding = self.encoding.clone();
        if self.source.content_bytes().starts_with(b"<?xml") {
            self.parse_text_decl()?;
        }

        self.state = ParserState::Parsing;
        self.source.set_compact_mode();
        self.parse_content()?;
        self.grow()?;
        if !self.source.is_empty() {
            fatal_error!(
                self,
                ParserUnexpectedDocumentContent,
                "Unnecessary external parsed content remains. (Elements, character data, etc.)"
            );
            return Err(XMLError::ParserUnexpectedDocumentContent);
        }
        // Restore version and encoding.
        self.encoding = encoding;
        self.version = version;

        Ok(())
    }

    /// ```text
    /// [77] TextDecl ::= '<?xml' VersionInfo? EncodingDecl S? '?>'
    /// ```
    pub(crate) fn parse_text_decl(&mut self) -> Result<(), XMLError> {
        self.grow()?;
        if !self.source.content_bytes().starts_with(b"<?xml") {
            fatal_error!(
                self,
                ParserInvalidTextDecl,
                "The text declaration does not start '<?xml'."
            );
            return Err(XMLError::ParserInvalidTextDecl);
        }
        // skip '<?xml'
        self.source.advance(5)?;
        self.locator.update_column(|c| c + 5);

        // parse VersionInfo if exists
        let mut s = self.skip_whitespaces()?;
        if self.source.content_bytes().starts_with(b"version") {
            if s == 0 {
                fatal_error!(
                    self,
                    ParserInvalidTextDecl,
                    "Whitespaces are required before 'encoding'."
                );
            }
            let (version, _) = self.parse_version_info(false)?;
            self.version = version;
            s = self.skip_whitespaces()?;
        }

        // parse EncodingDecl
        self.grow()?;
        if s == 0 {
            fatal_error!(
                self,
                ParserInvalidXMLDecl,
                "Whitespaces are required before 'encoding'."
            );
        }
        let encoding = self.parse_encoding_decl(false)?;
        if self.source.switch_encoding(&encoding).is_err() {
            fatal_error!(
                self,
                ParserUnsupportedEncoding,
                "Switching encoding to '{}' is failed.",
                encoding
            );
            return Err(XMLError::ParserUnsupportedEncoding);
        }
        self.encoding = Some(encoding);
        self.skip_whitespaces()?;
        self.grow()?;

        if !self.source.content_bytes().starts_with(b"?>") {
            fatal_error!(
                self,
                ParserInvalidTextDecl,
                "The text declaration does not end with '?>'."
            );
            return Err(XMLError::ParserInvalidTextDecl);
        }
        // skip '?>'
        self.source.advance(2)?;
        self.locator.update_column(|c| c + 2);

        Ok(())
    }

    /// ```text
    /// [14] CharData ::= [^<&]* - ([^<&]* ']]>' [^<&]*)
    /// ```
    pub(crate) fn parse_char_data(&mut self) -> Result<(), XMLError> {
        self.grow()?;
        if self.source.content_bytes().is_empty() {
            return Ok(());
        }

        let mut buffer = String::new();
        let mut non_whitespace = 0usize;
        'outer: loop {
            while !self.source.content_bytes().is_empty()
                && !matches!(self.source.content_bytes()[0], b'<' | b'&')
            {
                match self.source.next_char()? {
                    Some('\r') => {
                        if self.source.peek_char()? != Some('\n') {
                            self.locator.update_line(|l| l + 1);
                            self.locator.set_column(1);
                            buffer.push('\n');
                        }
                    }
                    Some('\n') => {
                        self.locator.update_line(|l| l + 1);
                        self.locator.set_column(1);
                        buffer.push('\n');
                    }
                    Some(']') => {
                        if self.source.content_bytes().starts_with(b"]>") {
                            fatal_error!(
                                self,
                                ParserUnacceptablePatternInCharData,
                                "']]>' is not allowed in a character data."
                            );
                        }
                        self.locator.update_column(|c| c + 1);
                        buffer.push(']');
                        non_whitespace += 1;
                    }
                    Some(c) if self.is_char(c) => {
                        self.locator.update_column(|c| c + 1);
                        buffer.push(c);
                        if !self.is_whitespace(c) {
                            non_whitespace += c.len_utf8();
                        }
                    }
                    Some(c) => {
                        fatal_error!(
                            self,
                            ParserInvalidCharacter,
                            "The characeter '0x{:X}' is not allowed in the XML document.",
                            c as u32
                        );
                        self.locator.update_column(|c| c + 1);
                        buffer.push(c);
                        non_whitespace += c.len_utf8();
                    }
                    _ => unreachable!(),
                }

                if buffer.len() >= CHARDATA_CHUNK_LENGTH {
                    if !self.fatal_error_occurred {
                        if let Some(Some((_, validator))) = self.validation_stack.last_mut() {
                            if non_whitespace != buffer.len() {
                                validator.push_whitespaces();
                            }
                            if non_whitespace == 0 && validator.is_element_content() {
                                self.content_handler.ignorable_whitespace(&buffer);
                            } else {
                                validator.push_pcdata();
                                self.content_handler.characters(&buffer);
                            }
                        } else {
                            self.content_handler.characters(&buffer);
                        }
                    }
                    buffer.clear();
                    non_whitespace = 0;
                }

                // Since it is necessary to check whether ']]>' is included,
                // maintain at least 3 bytes as much as possible.
                if self.source.content_bytes().len() < 3 {
                    self.grow()?;
                    if self.source.content_bytes().is_empty() {
                        break 'outer;
                    }
                }
            }

            self.grow()?;
            if self.source.content_bytes().starts_with(b"&#") {
                // Resolve character references here and include them in the character data.
                let c = self.parse_char_ref()?;
                buffer.push(c);
                non_whitespace += c.len_utf8();

                if buffer.len() >= CHARDATA_CHUNK_LENGTH {
                    if !self.fatal_error_occurred {
                        if let Some(Some((_, validator))) = self.validation_stack.last_mut() {
                            if non_whitespace != buffer.len() {
                                validator.push_whitespaces();
                            }
                            if non_whitespace == 0 && validator.is_element_content() {
                                self.content_handler.ignorable_whitespace(&buffer);
                            } else {
                                validator.push_pcdata();
                                self.content_handler.characters(&buffer);
                            }
                        } else {
                            self.content_handler.characters(&buffer);
                        }
                    }
                    buffer.clear();
                    non_whitespace = 0;
                }
            } else {
                // Do not process references other than character references or markup here,
                // and exit the loop.
                break;
            }
        }

        if !buffer.is_empty() && !self.fatal_error_occurred {
            if let Some(Some((_, validator))) = self.validation_stack.last_mut() {
                if non_whitespace != buffer.len() {
                    validator.push_whitespaces();
                }
                if non_whitespace == 0 && validator.is_element_content() {
                    self.content_handler.ignorable_whitespace(&buffer);
                } else {
                    validator.push_pcdata();
                    self.content_handler.characters(&buffer);
                }
            } else {
                self.content_handler.characters(&buffer);
            }
        }

        Ok(())
    }

    /// ```text
    /// [18] CDSect  ::= CDStart CData CDEnd
    /// [19] CDStart ::= '<![CDATA['
    /// [20] CData   ::= (Char* - (Char* ']]>' Char*))
    /// [21] CDEnd   ::= ']]>'
    /// ```
    pub(crate) fn parse_cdsect(&mut self) -> Result<(), XMLError> {
        self.grow()?;

        if !self.source.content_bytes().starts_with(b"<![CDATA[") {
            fatal_error!(
                self,
                ParserInvalidCDSect,
                "CDSect must start with '<![CDATA['."
            );
            return Err(XMLError::ParserInvalidCDSect);
        }
        // skip '<![CDATA['
        self.source.advance(9)?;
        self.locator.update_column(|c| c + 9);

        if !self.fatal_error_occurred {
            self.lexical_handler.start_cdata();
        }

        if let Some(Some((_, validator))) = self.validation_stack.last_mut() {
            // [VC: Element Valid]
            // CDATA sections are treated as character data that is not allowed
            // within element content, regardless of their contents.
            validator.push_pcdata();
        }

        self.grow()?;
        let mut buffer = String::new();
        while !self.source.content_bytes().starts_with(b"]]>") {
            match self.source.next_char()? {
                Some('\r') => {
                    if self.source.peek_char()? != Some('\n') {
                        self.locator.update_line(|l| l + 1);
                        self.locator.set_column(1);
                        buffer.push('\n');
                    }
                }
                Some('\n') => {
                    self.locator.update_line(|l| l + 1);
                    self.locator.set_column(1);
                    buffer.push('\n');
                }
                Some(c) if self.is_char(c) => {
                    self.locator.update_column(|c| c + 1);
                    buffer.push(c);
                }
                Some(c) => {
                    fatal_error!(
                        self,
                        ParserInvalidCharacter,
                        "The character '0x{:X}' is not allowed in the XML document.",
                        c as u32
                    );
                    self.locator.update_column(|c| c + 1);
                    buffer.push(c);
                }
                None => break,
            }

            if buffer.len() >= CHARDATA_CHUNK_LENGTH {
                if !self.fatal_error_occurred {
                    self.content_handler.characters(&buffer);
                }
                buffer.clear();
            }

            if self.source.content_bytes().len() < 3 {
                self.grow()?;
            }
        }

        if !buffer.is_empty() && !self.fatal_error_occurred {
            self.content_handler.characters(&buffer);
        }

        if !self.source.content_bytes().starts_with(b"]]>") {
            fatal_error!(self, ParserInvalidCDSect, "CDSect does not end with ']]>'.");
            return Err(XMLError::ParserInvalidCDSect);
        }
        // skip ']]>'
        self.source.advance(3)?;
        self.locator.update_column(|c| c + 3);

        if !self.fatal_error_occurred {
            self.lexical_handler.end_cdata();
        }

        Ok(())
    }
}

impl<'a, Spec: ParserSpec<Reader = InputSource<'a>>> XMLReader<Spec> {
    /// ```text
    /// [66] CharRef ::= '&#' [0-9]+ ';' | '&#x' [0-9a-fA-F]+ ';' [WFC: Legal Character]
    /// ```
    pub(crate) fn parse_char_ref(&mut self) -> Result<char, XMLError> {
        self.source.grow()?;

        let (code, overflowed, hex, len) = match self.source.content_bytes() {
            [b'&', b'#', b'x', ..] => {
                // skip '&#x'
                self.source.advance(3)?;
                self.locator.update_column(|c| c + 3);

                self.source.grow()?;
                let content = self.source.content_bytes();
                let mut cur = 0;
                let mut code = 0u32;
                let mut overflowed = false;
                while cur < content.len() && content[cur].is_ascii_hexdigit() {
                    let (new, f) = code.overflowing_mul(16);
                    let (new, g) = if content[cur].is_ascii_digit() {
                        new.overflowing_add((content[cur] - b'0') as u32)
                    } else if content[cur].is_ascii_uppercase() {
                        new.overflowing_add((content[cur] - b'A' + 10) as u32)
                    } else {
                        new.overflowing_add((content[cur] - b'a' + 10) as u32)
                    };
                    code = new;
                    cur += 1;
                    overflowed |= f | g;
                }
                (code, overflowed, true, cur)
            }
            [b'&', b'#', ..] => {
                // skip '&#'
                self.source.advance(2)?;
                self.locator.update_column(|c| c + 2);

                self.source.grow()?;
                let content = self.source.content_bytes();
                let mut cur = 0;
                let mut code = 0u32;
                let mut overflowed = false;
                while cur < content.len() && content[cur].is_ascii_digit() {
                    let (new, f) = code.overflowing_mul(10);
                    let (new, g) = new.overflowing_add((content[cur] - b'0') as u32);
                    code = new;
                    cur += 1;
                    overflowed |= f | g;
                }
                (code, overflowed, false, cur)
            }
            _ => {
                fatal_error!(
                    self,
                    ParserInvalidCharacterReference,
                    "A character reference must start with '&#' or '&#x'."
                );
                return Err(XMLError::ParserInvalidCharacterReference);
            }
        };

        // skip the read characters
        self.source.advance(len)?;
        self.locator.update_column(|c| c + len);

        self.source.grow()?;
        let content = self.source.content_bytes();
        if content.is_empty() {
            Err(XMLError::ParserUnexpectedEOF)
        } else if (hex && content[0].is_ascii_hexdigit())
            || (!hex && content[0].is_ascii_digit())
            || overflowed
        {
            fatal_error!(
                self,
                ParserInvalidCharacterReference,
                "The code point specified by the character reference is too large."
            );
            Err(XMLError::ParserInvalidCharacterReference)
        } else if content[0] != b';' {
            fatal_error!(
                self,
                ParserInvalidCharacterReference,
                "The character reference does not end with ';'"
            );
            Err(XMLError::ParserInvalidCharacterReference)
        } else if len == 0 {
            fatal_error!(
                self,
                ParserInvalidCharacterReference,
                "'&#{};' is not a correct character reference.",
                if hex { "x" } else { "" }
            );
            Err(XMLError::ParserInvalidCharacterReference)
        } else if let Some(c) = char::from_u32(code).filter(|c| self.is_char(*c)) {
            // skip ';'
            self.source.advance(1)?;
            self.locator.update_column(|c| c + 1);

            Ok(c)
        } else {
            fatal_error!(
                self,
                ParserInvalidCharacter,
                "The code point '0x{:X}' does not indicate a character that is allowed in a XML document.",
                code
            );
            Err(XMLError::ParserInvalidCharacter)
        }
    }

    /// Returns `true` if normalized according to the declaration,
    /// and `false` if no declaration is found.
    ///
    /// If `is_cdata` is specified as `Some(true)` or `Some(false)`,
    /// determines whether the attribute value is of type CDATA based on the specified boolean value.  \
    /// In this case, `elem_name` and `attr_name` are not used.
    ///
    /// Since normalization that does not depend on attribute list declarations is
    /// performed along with attribute value parsing, this function only performs
    /// normalization  that depends on attribute list declarations.
    pub(crate) fn normalize_att_value(
        &self,
        elem_name: &str,
        attr_name: &str,
        att_value: &mut String,
        is_cdata: Option<bool>,
    ) -> bool {
        let is_cdata = if let Some(is_cdata) = is_cdata {
            is_cdata
        } else if let Some((att_type, _, _)) = self.attlistdecls.get(elem_name, attr_name) {
            matches!(att_type, AttributeType::CDATA)
        } else {
            return false;
        };

        // CDATA attribute values do not require space character normalization.
        if !is_cdata {
            unsafe {
                // # Safety
                // As long as the algorithm works correctly, only space characters
                // are normalized, so there are no violations of UTF-8 constraints.
                let bytes = att_value.as_bytes_mut();
                let mut filled = 0;
                let mut before_space = true;
                for i in 0..bytes.len() {
                    if bytes[i] != 0x20 {
                        bytes[filled] = bytes[i];
                        filled += 1;
                        before_space = false;
                    } else {
                        if !before_space {
                            bytes[filled] = 0x20;
                            filled += 1;
                        }
                        before_space = true;
                    }
                }
                // trim the tail of 0x20
                while filled > 0 && bytes[filled - 1] == 0x20 {
                    filled -= 1;
                }
                // To avoid violating UTF-8 constraints, fill with all NULL characters.
                bytes[filled..].fill(0);
                att_value.truncate(filled);
            }
        }
        true
    }
}
