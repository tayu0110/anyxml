use std::{path::PathBuf, sync::Arc};

use crate::{
    ParserSpec,
    error::XMLError,
    sax::{
        EntityDecl,
        error::{error, fatal_error},
        parser::{ParserOption, ParserState, XMLReader},
        source::InputSource,
    },
};

impl<'a, Spec: ParserSpec<Reader = InputSource<'a>>> XMLReader<Spec> {
    fn check_literal_start(&mut self) -> Result<char, XMLError> {
        match self.source.next_char()? {
            Some(c @ ('"' | '\'')) => {
                self.locator.update_column(|c| c + 1);
                Ok(c)
            }
            Some(c) => {
                fatal_error!(
                    self.error_handler,
                    ParserIncorrectLiteralQuotation,
                    self.locator,
                    "A character '0x{:X}' is not correct quotation mark for a literal.",
                    c as u32
                );
                self.state = ParserState::FatalErrorOccurred;
                self.locator.update_column(|c| c + 1);
                Err(XMLError::ParserIncorrectLiteralQuotation)
            }
            None => {
                fatal_error!(
                    self.error_handler,
                    ParserUnexpectedEOF,
                    self.locator,
                    "Unexpected EOF."
                );
                self.state = ParserState::FatalErrorOccurred;
                Err(XMLError::ParserUnexpectedEOF)
            }
        }
    }

    fn check_literal_end(&mut self, quote: char) -> Result<(), XMLError> {
        match self.source.next_char()? {
            Some(c) if c == quote => {
                self.locator.update_column(|c| c + 1);
                Ok(())
            }
            Some(_) => {
                fatal_error!(
                    self.error_handler,
                    ParserIncorrectLiteralQuotation,
                    self.locator,
                    "The literal does not close with the correct quotation mark."
                );
                self.state = ParserState::FatalErrorOccurred;
                self.locator.update_column(|c| c + 1);
                Err(XMLError::ParserIncorrectLiteralQuotation)
            }
            None => {
                fatal_error!(
                    self.error_handler,
                    ParserUnexpectedEOF,
                    self.locator,
                    "Unexpected EOF."
                );
                self.state = ParserState::FatalErrorOccurred;
                Err(XMLError::ParserUnexpectedEOF)
            }
        }
    }

    /// ```text
    /// [11] SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'")
    /// ```
    pub(crate) fn parse_system_literal(&mut self, buffer: &mut String) -> Result<(), XMLError> {
        let quote = self.check_literal_start()?;

        // Since BNF does not explicitly use Char, we do not perform a check using `self.is_char`.
        // (However, since all control characters except for a few are accepted as Char,
        // this should not be a problem in most cases.)
        while let Some(c) = self.source.next_char_if(|c| c != quote)? {
            match c {
                '\r' => {
                    if self.source.peek_char()? != Some('\n') {
                        self.locator.update_line(|l| l + 1);
                        self.locator.set_column(1);
                        buffer.push('\n');
                    }
                }
                '\n' => {
                    self.locator.update_line(|l| l + 1);
                    self.locator.set_column(1);
                }
                c => {
                    self.locator.update_column(|c| c + 1);
                    buffer.push(c);
                }
            }
        }

        self.check_literal_end(quote)
    }

    /// ```text
    /// [12] PubidLiteral ::= '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
    /// ```
    pub(crate) fn parse_pubid_literal(&mut self, buffer: &mut String) -> Result<(), XMLError> {
        let quote = self.check_literal_start()?;

        while let Some(c) = self
            .source
            .next_char_if(|c| self.version.is_pubid_char(c) && c != quote)?
        {
            buffer.push(c);
            self.locator.update_column(|c| c + 1);
        }

        self.check_literal_end(quote)
    }

    /// ```text
    /// [10] AttValue ::= '"' ([^<&"] | Reference)* '"' | "'" ([^<&'] | Reference)* "'"
    /// ```
    pub(crate) fn parse_att_value(&mut self, buffer: &mut String) -> Result<(), XMLError> {
        let quote = self.check_literal_start()?;

        self.source.grow()?;
        let orig_entity_stack = self.entity_name_stack.len();
        self.parse_att_value_internal(buffer, quote, orig_entity_stack)?;
        // `Ok` is returned only when entity references are nested correctly.
        // Therefore, it is not necessary to check whether the stack state is correct.
        assert_eq!(orig_entity_stack, self.entity_name_stack.len());

        self.check_literal_end(quote)
    }

    fn parse_att_value_internal(
        &mut self,
        buffer: &mut String,
        quote: char,
        orig_entity_stack: usize,
    ) -> Result<(), XMLError> {
        self.source.grow()?;
        'outer: while !self.source.content_bytes().is_empty() {
            while !matches!(self.source.content_bytes()[0], b'<' | b'&')
                && self.source.content_bytes()[0] != quote as u8
            {
                match self.source.next_char()? {
                    Some(c) => buffer.push(c),
                    None => unreachable!(),
                }
                self.locator.update_column(|c| c + 1);

                if self.source.content_bytes().is_empty() {
                    self.source.grow()?;
                    if self.source.content_bytes().is_empty() {
                        break 'outer;
                    }
                }
            }

            match self.source.content_bytes()[0] {
                b'<' => {
                    fatal_error!(
                        self.error_handler,
                        ParserInvalidAttValue,
                        self.locator,
                        "AttValue must not contain '<'."
                    );
                    buffer.push('<');
                    self.source.advance(1)?;
                    self.locator.update_column(|c| c + 1);
                    self.state = ParserState::FatalErrorOccurred;
                }
                b'&' => {
                    self.source.grow()?;
                    if self.source.content_bytes().starts_with(b"&#") {
                        // character reference
                        buffer.push(self.parse_char_ref()?);
                    } else {
                        // general refenrence
                        self.parse_entity_ref_in_att_value(buffer, quote, orig_entity_stack)?;
                    }
                }
                _ => break,
            }
            self.source.grow()?;
        }

        if self.source.content_bytes().is_empty() {
            if self.entity_name_stack.len() == orig_entity_stack {
                fatal_error!(
                    self.error_handler,
                    ParserUnexpectedEOF,
                    self.locator,
                    "Unexpected EOF."
                );
            }
            self.state = ParserState::FatalErrorOccurred;
        }
        Ok(())
    }

    fn parse_entity_ref_in_att_value(
        &mut self,
        buffer: &mut String,
        quote: char,
        orig_entity_stack: usize,
    ) -> Result<(), XMLError> {
        self.source.grow()?;
        if !self.source.content_bytes().starts_with(b"&") {
            fatal_error!(
                self.error_handler,
                ParserInvalidEntityReference,
                self.locator,
                "An entity reference does not start with '&'."
            );
            self.state = ParserState::FatalErrorOccurred;
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

        self.source.grow()?;
        if !self.source.content_bytes().starts_with(b";") {
            fatal_error!(
                self.error_handler,
                ParserInvalidEntityReference,
                self.locator,
                "The entity reference does not end with ';'."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidEntityReference);
        }
        // skip ';'
        self.source.advance(1)?;
        self.locator.update_column(|c| c + 1);

        if let Some(decl) = self.entities.get(name.as_str()) {
            if self
                .entity_name_stack
                .iter()
                .any(|ent| ent.as_deref() == Some(name.as_str()))
            {
                // [WFC: No Recursion]
                fatal_error!(
                    self.error_handler,
                    ParserEntityRecursion,
                    self.locator,
                    "The entity '{}' appears recursively.",
                    name
                );
                self.state = ParserState::FatalErrorOccurred;
                return Err(XMLError::ParserEntityRecursion);
            }
            match decl {
                EntityDecl::InternalGeneralEntity {
                    base_uri,
                    replacement_text,
                } => {
                    let source = InputSource::from_content(replacement_text.as_ref());
                    let name: Arc<str> = name.into();
                    self.push_source(
                        Box::new(source),
                        base_uri.clone(),
                        Some(name.clone()),
                        PathBuf::from(format!("#internal-entity.{name}")).into(),
                        None,
                    )?;

                    if self.state != ParserState::FatalErrorOccurred {
                        self.lexical_handler.start_entity(&name);
                    }

                    self.parse_att_value_internal(buffer, quote, orig_entity_stack)?;
                    self.source.grow()?;

                    if !self.source.is_empty() {
                        fatal_error!(
                            self.error_handler,
                            ParserEntityIncorrectNesting,
                            self.locator,
                            "The entity '{}' is nested incorrectly.",
                            name
                        );
                        self.state = ParserState::FatalErrorOccurred;
                        return Err(XMLError::ParserEntityIncorrectNesting);
                    }

                    self.pop_source()?;
                    if self.state != ParserState::FatalErrorOccurred {
                        self.lexical_handler.end_entity();
                    }
                }
                EntityDecl::ExternalGeneralParsedEntity { .. } => {
                    // 4.4 XML Processor Treatment of Entities and References
                    // [Reference in Attribute Value]
                    // 4.4.4 Forbidden
                    fatal_error!(
                        self.error_handler,
                        ParserInvalidEntityReference,
                        self.locator,
                        "An external entity reference is not allowed in AttValue"
                    );
                    self.state = ParserState::FatalErrorOccurred;
                }
                EntityDecl::ExternalGeneralUnparsedEntity { .. } => {
                    // [WFC: Parsed Entity]
                    fatal_error!(
                        self.error_handler,
                        ParserInvalidEntityReference,
                        self.locator,
                        "The unparsed entity '{}' cannot be referred.",
                        name
                    );
                    self.state = ParserState::FatalErrorOccurred;
                }
                EntityDecl::InternalParameterEntity { .. }
                | EntityDecl::ExternalParameterEntity { .. } => {
                    // The fact that we have reached this point suggests that the general
                    // entity has been mistakenly registered as a parameter entity somewhere.
                    unreachable!("Internal error: Reference name: {name}");
                }
            }
        } else {
            if self.config.is_enable(ParserOption::Validation) {
                // is it correct ???
                // I don't really understand the meaning of [WFC: Entity Declared],
                // so if I'm wrong, please let me know...
                if !self.has_external_subset || self.standalone == Some(true) {
                    // [WFC: Entity Declared]
                    fatal_error!(
                        self.error_handler,
                        ParserEntityNotFound,
                        self.locator,
                        "The entity '{}' is not declared.",
                        name
                    );
                    self.state = ParserState::FatalErrorOccurred;
                } else {
                    // [VC: Entity Declared]
                    error!(
                        self.error_handler,
                        ParserEntityNotFound,
                        self.locator,
                        "The entity '{}' is not declared.",
                        name
                    );
                }
            } else {
                // [WFC: Entity Declared]
                if self.standalone == Some(true) {
                    fatal_error!(
                        self.error_handler,
                        ParserEntityNotFound,
                        self.locator,
                        "The entity '{}' is not declared.",
                        name
                    );
                    self.state = ParserState::FatalErrorOccurred;
                }
            }

            if self.state != ParserState::FatalErrorOccurred {
                self.content_handler.skipped_entity(&name);
            }
        }
        Ok(())
    }
}
