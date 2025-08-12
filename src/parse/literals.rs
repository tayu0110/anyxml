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
    /// ```text
    /// [11] SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'")
    /// ```
    pub(crate) fn parse_system_literal(&mut self, buffer: &mut String) -> Result<(), XMLError> {
        let quote = match self.source.next_char()? {
            Some(c @ ('"' | '\'')) => c,
            Some(c) => {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserInvalidSystemLiteral,
                    self.locator,
                    "A character '0x{:X}' is not correct quotation mark for SystemLiteral.",
                    c as u32
                );
                self.state = ParserState::FatalErrorOccurred;
                return Err(XMLError::ParserInvalidSystemLiteral);
            }
            None => {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserUnexpectedEOF,
                    self.locator,
                    "Unexpected EOF."
                );
                self.state = ParserState::FatalErrorOccurred;
                return Err(XMLError::ParserUnexpectedEOF);
            }
        };
        self.locator.update_column(|c| c + 1);

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

        match self.source.next_char()? {
            Some(c) if c == quote => {
                self.locator.update_column(|c| c + 1);
                Ok(())
            }
            Some(_) => {
                self.locator.update_column(|c| c + 1);
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserInvalidPubidLiteral,
                    self.locator,
                    "SystemLiteral does not close with the correct quotation mark."
                );
                self.state = ParserState::FatalErrorOccurred;
                Err(XMLError::ParserInvalidPubidLiteral)
            }
            None => {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserUnexpectedEOF,
                    self.locator,
                    "Unexpected EOF."
                );
                self.state = ParserState::FatalErrorOccurred;
                Err(XMLError::ParserUnexpectedEOF)
            }
        }
    }

    /// ```text
    /// [12] PubidLiteral ::= '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
    /// ```
    pub(crate) fn parse_pubid_literal(&mut self, buffer: &mut String) -> Result<(), XMLError> {
        let quote = match self.source.next_char()? {
            Some(c @ ('"' | '\'')) => c,
            Some(c) => {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserInvalidPubidLiteral,
                    self.locator,
                    "A character '0x{:X}' is not correct quotation mark for PubidLiteral.",
                    c as u32
                );
                self.state = ParserState::FatalErrorOccurred;
                return Err(XMLError::ParserInvalidPubidLiteral);
            }
            None => {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserUnexpectedEOF,
                    self.locator,
                    "Unexpected EOF."
                );
                self.state = ParserState::FatalErrorOccurred;
                return Err(XMLError::ParserUnexpectedEOF);
            }
        };
        self.locator.update_column(|c| c + 1);

        while let Some(c) = self
            .source
            .next_char_if(|c| self.version.is_pubid_char(c) && c != quote)?
        {
            buffer.push(c);
            self.locator.update_column(|c| c + 1);
        }

        match self.source.next_char()? {
            Some(c) if c == quote => {
                self.locator.update_column(|c| c + 1);
                Ok(())
            }
            Some(_) => {
                self.locator.update_column(|c| c + 1);
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserInvalidPubidLiteral,
                    self.locator,
                    "PubidLiteral does not close with the correct quotation mark."
                );
                self.state = ParserState::FatalErrorOccurred;
                Err(XMLError::ParserInvalidPubidLiteral)
            }
            None => {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserUnexpectedEOF,
                    self.locator,
                    "Unexpected EOF."
                );
                self.state = ParserState::FatalErrorOccurred;
                Err(XMLError::ParserUnexpectedEOF)
            }
        }
    }

    pub(crate) fn parse_att_value(&mut self, buffer: &mut String) -> Result<(), XMLError> {
        todo!()
    }
}
