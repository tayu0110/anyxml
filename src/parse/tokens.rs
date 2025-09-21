use crate::{
    ParserSpec,
    error::XMLError,
    sax::{error::fatal_error, handler::SAXHandler, parser::XMLReader, source::InputSource},
};

impl<'a, Spec: ParserSpec<Reader = InputSource<'a>>, H: SAXHandler> XMLReader<Spec, H> {
    pub(crate) fn is_char(&self, c: char) -> bool {
        self.version.is_char(c)
    }

    pub(crate) fn is_name_start_char(&self, c: char) -> bool {
        self.version.is_name_start_char(c)
    }

    pub(crate) fn is_name_char(&self, c: char) -> bool {
        self.version.is_name_char(c)
    }

    pub(crate) fn is_whitespace(&self, c: char) -> bool {
        self.version.is_whitespace(c)
    }

    pub(crate) fn skip_whitespaces(&mut self) -> Result<usize, XMLError> {
        let mut skipped = 0;
        while let Some(w) = self.source.peek_char()? {
            if !self.is_whitespace(w) {
                break;
            }
            self.source.next_char()?;

            match w {
                '\x20' | '\t' => self.locator.update_column(|c| c + 1),
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

    pub(crate) fn parse_nmtoken(&mut self, buffer: &mut String) -> Result<(), XMLError> {
        let orig = buffer.len();
        while let Some(c) = self.source.next_char_if(|c| self.version.is_name_char(c))? {
            buffer.push(c);
            self.locator.update_column(|c| c + 1);
        }

        if buffer.len() == orig {
            fatal_error!(self, ParserEmptyName, "Nmtoken is empty.");
            return Err(XMLError::ParserEmptyName);
        }
        Ok(())
    }

    pub(crate) fn parse_name(&mut self, buffer: &mut String) -> Result<(), XMLError> {
        let Some(c) = self
            .source
            .next_char_if(|c| self.version.is_name_start_char(c))?
        else {
            fatal_error!(self, ParserEmptyName, "Name is empty.");
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

    pub(crate) fn parse_ncname(&mut self, buffer: &mut String) -> Result<(), XMLError> {
        let orig = buffer.len();
        self.parse_ncname_allow_empty(buffer)?;
        if buffer.len() == orig {
            fatal_error!(self, ParserEmptyName, "Name is empty.");
            return Err(XMLError::ParserEmptyName);
        }
        Ok(())
    }

    /// Return the length of prefix if some errors occurred.
    pub(crate) fn parse_qname(&mut self, buffer: &mut String) -> Result<usize, XMLError> {
        let orig = buffer.len();
        self.parse_ncname_allow_empty(buffer)?;

        if self.source.next_char_if(|c| c == ':')?.is_none() {
            return if buffer.len() == orig {
                fatal_error!(self, ParserEmptyQName, "QName is empty.");
                Err(XMLError::ParserEmptyQName)
            } else {
                Ok(0)
            };
        };
        if buffer.len() == orig {
            fatal_error!(
                self,
                ParserEmptyQNamePrefix,
                "':' is found in QName, but its prefix is empty."
            );
        }
        let prefix = buffer.len() - orig;
        buffer.push(':');
        self.locator.update_column(|c| c + 1);
        self.parse_ncname_allow_empty(buffer)?;

        if buffer.len() == orig + prefix + 1 {
            fatal_error!(
                self,
                ParserEmptyQNameLocalPart,
                "':' is found in QName, but its local part is empty."
            );
            Err(XMLError::ParserEmptyQNameLocalPart)
        } else if prefix == 0 {
            Err(XMLError::ParserEmptyQNamePrefix)
        } else {
            Ok(prefix)
        }
    }

    pub fn validate_nmtoken(&self, nmtoken: &str) -> Result<(), XMLError> {
        if nmtoken.is_empty() {
            return Err(XMLError::ParserEmptyNmtoken);
        }
        nmtoken
            .chars()
            .all(|c| self.is_name_char(c))
            .then_some(())
            .ok_or(XMLError::ParserInvalidNameChar)
    }

    pub fn validate_name(&self, name: &str) -> Result<(), XMLError> {
        if name.is_empty() {
            return Err(XMLError::ParserEmptyName);
        }

        name.strip_prefix(|c| self.is_name_start_char(c))
            .ok_or(XMLError::ParserInvalidNameStartChar)?
            .chars()
            .all(|c| self.is_name_char(c))
            .then_some(())
            .ok_or(XMLError::ParserInvalidNameChar)
    }

    pub fn validate_ncname(&self, name: &str) -> Result<(), XMLError> {
        if name.is_empty() {
            return Err(XMLError::ParserEmptyNCName);
        }

        name.strip_prefix(|c| c != ':' && self.is_name_start_char(c))
            .ok_or(XMLError::ParserInvalidNCNameStartChar)?
            .chars()
            .all(|c| c != ':' && self.is_name_char(c))
            .then_some(())
            .ok_or(XMLError::ParserInvalidNCNameChar)
    }

    pub fn validate_qname(&self, mut name: &str) -> Result<(), XMLError> {
        if name.is_empty() {
            return Err(XMLError::ParserEmptyQName);
        }

        if name.starts_with(':') {
            return Err(XMLError::ParserEmptyQNamePrefix);
        }

        name = name
            .strip_prefix(|c| self.is_name_start_char(c))
            .ok_or(XMLError::ParserInvalidNCNameStartChar)?;
        name = name.trim_start_matches(|c| c != ':' && self.is_name_char(c));

        if name.is_empty() {
            // This is an UnprefixedName
            return Ok(());
        }
        name = name
            .strip_prefix(|c| c == ':')
            .ok_or(XMLError::ParserInvalidQNameSeparator)?;
        if name.is_empty() {
            return Err(XMLError::ParserEmptyQNameLocalPart);
        }
        self.validate_ncname(name)
    }

    pub fn validate_nmtokens(&self, nmtokens: &str) -> Result<(), XMLError> {
        if nmtokens.is_empty() {
            return Err(XMLError::ParserEmptyNmtokens);
        }
        self.validate_names(nmtokens, |nmtoken| self.validate_nmtoken(nmtoken))
    }

    pub fn validate_names(
        &self,
        names: &str,
        name_validation: impl Fn(&str) -> Result<(), XMLError>,
    ) -> Result<(), XMLError> {
        if names.is_empty() {
            return Err(XMLError::ParserEmptyNames);
        }
        for name in names.split('\x20') {
            name_validation(name)?;
        }
        Ok(())
    }
}
