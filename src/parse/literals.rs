use crate::{
    ParserSpec,
    error::XMLError,
    sax::{
        error::fatal_error,
        parser::{ParserState, XMLReader},
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
        if !self.source.content_bytes().is_empty() {
            'outer: loop {
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
                            fatal_error!(
                                self.error_handler,
                                ParserUnexpectedEOF,
                                self.locator,
                                "Unexpected EOF."
                            );
                            self.state = ParserState::FatalErrorOccurred;
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
                            todo!("General Reference");
                        }
                    }
                    _ => break 'outer,
                }
            }
        }

        self.check_literal_end(quote)
    }
}
