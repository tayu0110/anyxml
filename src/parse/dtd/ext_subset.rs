use crate::{
    ParserSpec,
    error::XMLError,
    sax::{
        error::{fatal_error, validity_error},
        handler::SAXHandler,
        parser::{ParserState, XMLReader},
        source::InputSource,
    },
};

impl<'a, Spec: ParserSpec<Reader = InputSource<'a>>, H: SAXHandler> XMLReader<Spec, H> {
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
    fn parse_ext_subset_decl(&mut self) -> Result<(), XMLError> {
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
                        self.handler.end_entity();
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

            self.skip_whitespaces_with_handle_peref(false)?;
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
    fn parse_conditional_sect(&mut self) -> Result<(), XMLError> {
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
}
