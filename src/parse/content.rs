use std::sync::Arc;

use crate::{
    ParserSpec,
    error::XMLError,
    sax::{
        EntityDecl,
        error::{error, fatal_error, validity_error},
        handler::SAXHandler,
        parser::{ParserOption, ParserState, XMLReader},
        source::InputSource,
    },
    uri::URIString,
};

impl<'a, Spec: ParserSpec<Reader = InputSource<'a>>, H: SAXHandler> XMLReader<Spec, H> {
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
    fn parse_entity_ref_in_content(&mut self) -> Result<(), XMLError> {
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
    fn parse_ext_parsed_ent(&mut self) -> Result<(), XMLError> {
        self.state = ParserState::InTextDeclaration;
        self.grow()?;
        // Save version and encoding because they may be overwritten by a text declaration.
        let version = self.version;
        let encoding = self.encoding.clone();
        if self.source.content_bytes().starts_with(b"<?xml") {
            self.parse_text_decl()?;
        }

        self.state = ParserState::InContent;
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
}
