mod cdsect;
mod comment;
mod dtd;
mod element;
pub mod literals;
mod pi;
pub mod tokens;
mod xmldecl;

use std::{str::from_utf8_unchecked, sync::Arc};

use anyxml_uri::uri::URIString;

use crate::{
    CHARDATA_CHUNK_LENGTH, ParserSpec,
    error::XMLError,
    sax::{
        AttributeType, EntityDecl,
        error::{error, fatal_error, validity_error},
        handler::SAXHandler,
        parser::{ParserOption, ParserState, XMLReader},
        source::InputSource,
    },
};

impl<'a, Spec: ParserSpec<Reader = InputSource<'a>>, H: SAXHandler> XMLReader<Spec, H> {
    /// ```text
    /// [1] document ::= prolog element Misc*
    /// ```
    pub(crate) fn parse_document(&mut self) -> Result<(), XMLError> {
        self.handler.set_document_locator(self.locator.clone());
        self.handler.start_document();
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
        self.handler.end_document();
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
                                self.handler.start_entity(&name);
                            }
                        } else if !self.fatal_error_occurred {
                            self.handler.skipped_entity(&name);
                        }
                    }
                    EntityDecl::ExternalParameterEntity {
                        base_uri,
                        system_id,
                        public_id,
                    } => {
                        match self.handler.resolve_entity(
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
                                    self.handler.start_entity(&name);
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
                                    self.handler.skipped_entity(&name);
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
                self.handler.skipped_entity(&name);
            }
        } else if !self.fatal_error_occurred {
            self.handler.skipped_entity(&name);
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
                    self.handler.end_entity();
                }
                s += 1 + self.skip_whitespaces()?;
                self.grow()?;
            } else {
                break Ok(s + self.skip_whitespaces()?);
            }
        }
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
                            self.handler.start_entity(&name);
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
                            self.handler.end_entity();
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
                        match self.handler.resolve_entity(
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
                                    self.handler.start_entity(&name);
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
                                    self.handler.end_entity();
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
                        self.handler.skipped_entity(&name);
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
                self.handler.skipped_entity(&name);
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
                                self.handler.ignorable_whitespace(&buffer);
                            } else {
                                validator.push_pcdata();
                                self.handler.characters(&buffer);
                            }
                        } else {
                            self.handler.characters(&buffer);
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
                                self.handler.ignorable_whitespace(&buffer);
                            } else {
                                validator.push_pcdata();
                                self.handler.characters(&buffer);
                            }
                        } else {
                            self.handler.characters(&buffer);
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
                    self.handler.ignorable_whitespace(&buffer);
                } else {
                    validator.push_pcdata();
                    self.handler.characters(&buffer);
                }
            } else {
                self.handler.characters(&buffer);
            }
        }

        Ok(())
    }

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
