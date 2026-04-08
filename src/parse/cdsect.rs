use crate::{
    CHARDATA_CHUNK_LENGTH,
    error::XMLError,
    sax::{InputSource, ParserSpec, SAXHandler, XMLReader, error::fatal_error},
};

impl<'a, Spec: ParserSpec<Reader = InputSource<'a>>, H: SAXHandler> XMLReader<Spec, H> {
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
        self.source.advance(9);
        self.locator.update_column(|c| c + 9);

        if !self.fatal_error_occurred {
            self.handler.start_cdata();
        }

        if let Some(Some((_, validator))) = self.validation_stack.last_mut() {
            // [VC: Element Valid]
            // CDATA sections are treated as character data that is not allowed
            // within element content, regardless of their contents.
            validator.push_pcdata();
        }

        self.grow()?;
        self.text_buffer.clear();
        while !self.source.content_bytes().starts_with(b"]]>") {
            match self.source.next_char()? {
                Some('\r') => {
                    if self.source.peek_char()? != Some('\n') {
                        self.locator.update_line(|l| l + 1);
                        self.locator.set_column(1);
                        self.text_buffer.push('\n');
                    }
                }
                Some('\n') => {
                    self.locator.update_line(|l| l + 1);
                    self.locator.set_column(1);
                    self.text_buffer.push('\n');
                }
                Some(c) if self.is_char(c) => {
                    self.locator.update_column(|c| c + 1);
                    self.text_buffer.push(c);
                }
                Some(c) => {
                    fatal_error!(
                        self,
                        ParserInvalidCharacter,
                        "The character '0x{:X}' is not allowed in the XML document.",
                        c as u32
                    );
                    self.locator.update_column(|c| c + 1);
                    self.text_buffer.push(c);
                }
                None => break,
            }

            if self.text_buffer.len() >= CHARDATA_CHUNK_LENGTH {
                if !self.fatal_error_occurred {
                    self.handler.characters(&self.text_buffer);
                }
                self.text_buffer.clear();
            }

            if self.source.content_bytes().len() < 3 {
                self.grow()?;
            }
        }

        if !self.text_buffer.is_empty() && !self.fatal_error_occurred {
            self.handler.characters(&self.text_buffer);
        }

        if !self.source.content_bytes().starts_with(b"]]>") {
            fatal_error!(self, ParserInvalidCDSect, "CDSect does not end with ']]>'.");
            return Err(XMLError::ParserInvalidCDSect);
        }
        // skip ']]>'
        self.source.advance(3);
        self.locator.update_column(|c| c + 3);

        if !self.fatal_error_occurred {
            self.handler.end_cdata();
        }

        Ok(())
    }
}
