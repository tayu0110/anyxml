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
    pub fn is_char(&self, c: char) -> bool {
        self.version.is_char(c)
    }

    pub fn is_name_start_char(&self, c: char) -> bool {
        self.version.is_name_start_char(c)
    }

    pub fn is_name_char(&self, c: char) -> bool {
        self.version.is_name_char(c)
    }

    pub fn is_pubid_char(&self, c: char) -> bool {
        self.version.is_pubid_char(c)
    }

    pub fn is_whitespace(&self, c: char) -> bool {
        self.version.is_whitespace(c)
    }

    pub fn skip_whitespaces(&mut self) -> Result<usize, XMLError> {
        let mut skipped = 0;
        while let Some(w) = self.source.peek_char()? {
            if !self.is_whitespace(w) {
                break;
            }
            self.source.next_char()?;

            match w {
                '\x20' | '\t' => self.locator.update_line(|c| c + 1),
                '\n' => {
                    self.locator.set_column(1);
                    self.locator.update_line(|l| l + 1);
                }
                '\r' => {
                    if self.source.peek_char()?.is_some_and(|c| c == '\n') {
                        self.source.next_char()?;
                    }
                    self.locator.set_column(1);
                    self.locator.update_line(|l| l + 1);
                }
                _ => unimplemented!(),
            }
            skipped += 1;
        }

        Ok(skipped)
    }

    pub fn parse_nmtoken(&mut self, buffer: &mut String) -> Result<(), XMLError> {
        let orig = buffer.len();
        while let Some(c) = self.source.next_char_if(|c| self.version.is_name_char(c))? {
            buffer.push(c);
            self.locator.update_column(|c| c + 1);
        }

        if buffer.len() == orig {
            fatal_error!(
                self.error_handler,
                ParserEmptyName,
                self.locator,
                "Nmtoken is empty."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserEmptyName);
        }
        Ok(())
    }

    pub fn parse_name(&mut self, buffer: &mut String) -> Result<(), XMLError> {
        let Some(c) = self
            .source
            .next_char_if(|c| self.version.is_name_start_char(c))?
        else {
            fatal_error!(
                self.error_handler,
                ParserEmptyName,
                self.locator,
                "Name is empty."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserEmptyName);
        };
        buffer.push(c);
        self.locator.update_column(|c| c + 1);

        while let Some(c) = self.source.next_char_if(|c| self.version.is_name_char(c))? {
            buffer.push(c);
            self.locator.update_column(|c| c + 1);
        }

        Ok(())
    }

    /// Even if NCName is empty, no error will be reported.
    fn parse_ncname_allow_empty(&mut self, buffer: &mut String) -> Result<(), XMLError> {
        let Some(c) = self
            .source
            .next_char_if(|c| self.version.is_name_start_char(c) && c != ':')?
        else {
            return Ok(());
        };
        buffer.push(c);
        self.locator.update_column(|c| c + 1);

        while let Some(c) = self
            .source
            .next_char_if(|c| self.version.is_name_char(c) && c != ':')?
        {
            buffer.push(c);
            self.locator.update_column(|c| c + 1);
        }

        Ok(())
    }

    pub fn parse_ncname(&mut self, buffer: &mut String) -> Result<(), XMLError> {
        let orig = buffer.len();
        self.parse_ncname_allow_empty(buffer)?;
        if buffer.len() == orig {
            fatal_error!(
                self.error_handler,
                ParserEmptyName,
                self.locator,
                "Name is empty."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserEmptyName);
        }
        Ok(())
    }

    /// Return the length of prefix if some errors occurred.
    pub fn parse_qname(&mut self, buffer: &mut String) -> Result<usize, XMLError> {
        let orig = buffer.len();
        self.parse_ncname_allow_empty(buffer)?;

        if self.source.next_char_if(|c| c == ':')?.is_none() {
            return if buffer.len() == orig {
                fatal_error!(
                    self.error_handler,
                    ParserEmptyQName,
                    self.locator,
                    "QName is empty."
                );
                Err(XMLError::ParserEmptyQName)
            } else {
                Ok(0)
            };
        };
        if buffer.len() == orig {
            fatal_error!(
                self.error_handler,
                ParserEmptyQNamePrefix,
                self.locator,
                "':' is found in QName, but its prefix is empty."
            );
            self.state = ParserState::FatalErrorOccurred;
        }
        let prefix = buffer.len() - orig;
        buffer.push(':');
        self.locator.update_column(|c| c + 1);
        self.parse_ncname_allow_empty(buffer)?;

        if buffer.len() == orig + prefix + 1 {
            fatal_error!(
                self.error_handler,
                ParserEmptyQNameLocalPart,
                self.locator,
                "':' is found in QName, but its local part is empty."
            );
            self.state = ParserState::FatalErrorOccurred;
            Err(XMLError::ParserEmptyQNameLocalPart)
        } else if prefix == 0 {
            Err(XMLError::ParserEmptyQNamePrefix)
        } else {
            Ok(prefix)
        }
    }
}
