use crate::{
    CHARDATA_CHUNK_LENGTH, ParserSpec,
    error::XMLError,
    sax::{error::fatal_error, handler::SAXHandler, parser::XMLReader, source::InputSource},
};

impl<'a, Spec: ParserSpec<Reader = InputSource<'a>>, H: SAXHandler> XMLReader<Spec, H> {
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
}
