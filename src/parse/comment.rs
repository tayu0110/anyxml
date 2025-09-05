use crate::{
    CHARDATA_CHUNK_LENGTH, ParserSpec,
    error::XMLError,
    sax::{error::fatal_error, handler::SAXHandler, parser::XMLReader, source::InputSource},
};

impl<'a, Spec: ParserSpec<Reader = InputSource<'a>>, H: SAXHandler> XMLReader<Spec, H> {
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
                    self.handler.comment(&buffer);
                }
                buffer.clear();
            }
            if self.source.content_bytes().len() < 3 {
                self.grow()?;
            }
        }

        if !buffer.is_empty() && !self.fatal_error_occurred {
            self.handler.comment(&buffer);
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
}
