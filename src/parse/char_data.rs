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
        self.source.grow()?;
        if self.source.content_bytes().is_empty() {
            return Ok(());
        }

        self.text_buffer.clear();
        let mut non_whitespace = 0usize;
        'outer: loop {
            while let content = self.source.content_bytes()
                && !content.is_empty()
                && !matches!(content[0], b'<' | b'&')
            {
                if content[0] == b'\r' {
                    self.source.advance(1);
                    if self.source.content_bytes().first() != Some(&b'\n') {
                        self.locator.update_line(|l| l + 1);
                        self.locator.set_column(1);
                        self.text_buffer.push('\n');
                    }
                } else if content[0] == b'\n' {
                    self.source.advance(1);
                    self.locator.update_line(|l| l + 1);
                    self.locator.set_column(1);
                    self.text_buffer.push('\n');
                } else if content[0] == b']' {
                    if content.starts_with(b"]]>") {
                        fatal_error!(
                            self,
                            ParserUnacceptablePatternInCharData,
                            "']]>' is not allowed in a character data."
                        );
                    }
                    self.source.advance(1);
                    self.locator.update_column(|c| c + 1);
                    self.text_buffer.push(']');
                    non_whitespace += 1;
                } else if matches!(content[0], b'\x20' | b'\t') {
                    self.text_buffer.push(content[0] as char);
                    self.source.advance(1);
                    self.locator.update_column(|c| c + 1);
                } else if let at = content
                    .iter()
                    .position(|&b| !matches!(b, 0x21..0x80) || b == b'<' || b == b'&' || b == b']')
                    .unwrap_or(content.len())
                    && at > 0
                {
                    let at = at.min(CHARDATA_CHUNK_LENGTH.saturating_sub(self.text_buffer.len()));
                    non_whitespace += at;
                    self.text_buffer.push_str(unsafe {
                        // # Safety
                        // `content[..at]` contains only ASCII graphic characters
                        // other than '<', '&' and ']', so UTF-8 validation won't fail.
                        std::str::from_utf8_unchecked(&content[..at])
                    });
                    self.source.advance(at);
                    self.locator.update_column(|c| c + at);
                } else {
                    match self.source.next_char()? {
                        Some(c) if self.is_char(c) => {
                            self.locator.update_column(|c| c + 1);
                            self.text_buffer.push(c);
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
                            self.text_buffer.push(c);
                            non_whitespace += c.len_utf8();
                        }
                        _ => unreachable!(),
                    }
                }

                if self.text_buffer.len() >= CHARDATA_CHUNK_LENGTH {
                    if !self.fatal_error_occurred {
                        if let Some(Some((_, validator))) = self.validation_stack.last_mut() {
                            if non_whitespace != self.text_buffer.len() {
                                validator.push_whitespaces();
                            }
                            if non_whitespace == 0 && validator.is_element_content() {
                                self.handler.ignorable_whitespace(&self.text_buffer);
                            } else {
                                validator.push_pcdata();
                                self.handler.characters(&self.text_buffer);
                            }
                        } else {
                            self.handler.characters(&self.text_buffer);
                        }
                    }
                    self.text_buffer.clear();
                    non_whitespace = 0;
                }

                // Since it is necessary to check whether ']]>' is included,
                // maintain at least 3 bytes as much as possible.
                if self.source.content_bytes().len() < 3 {
                    self.source.grow()?;
                    if self.source.content_bytes().is_empty() {
                        break 'outer;
                    }
                }
            }

            self.source.grow()?;
            if self.source.content_bytes().starts_with(b"&#") {
                // Resolve character references here and include them in the character data.
                let c = self.parse_char_ref()?;
                self.text_buffer.push(c);
                non_whitespace += c.len_utf8();

                if self.text_buffer.len() >= CHARDATA_CHUNK_LENGTH {
                    if !self.fatal_error_occurred {
                        if let Some(Some((_, validator))) = self.validation_stack.last_mut() {
                            if non_whitespace != self.text_buffer.len() {
                                validator.push_whitespaces();
                            }
                            if non_whitespace == 0 && validator.is_element_content() {
                                self.handler.ignorable_whitespace(&self.text_buffer);
                            } else {
                                validator.push_pcdata();
                                self.handler.characters(&self.text_buffer);
                            }
                        } else {
                            self.handler.characters(&self.text_buffer);
                        }
                    }
                    self.text_buffer.clear();
                    non_whitespace = 0;
                }
            } else {
                // Do not process references other than character references or markup here,
                // and exit the loop.
                break;
            }
        }

        if !self.text_buffer.is_empty() && !self.fatal_error_occurred {
            if let Some(Some((_, validator))) = self.validation_stack.last_mut() {
                if non_whitespace != self.text_buffer.len() {
                    validator.push_whitespaces();
                }
                if non_whitespace == 0 && validator.is_element_content() {
                    self.handler.ignorable_whitespace(&self.text_buffer);
                } else {
                    validator.push_pcdata();
                    self.handler.characters(&self.text_buffer);
                }
            } else {
                self.handler.characters(&self.text_buffer);
            }
        }

        Ok(())
    }
}
