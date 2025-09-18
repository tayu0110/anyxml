use std::{ops::ControlFlow, sync::Arc};

use anyxml_uri::uri::URIString;

use crate::{
    ProgressiveParserSpec,
    error::XMLError,
    sax::{
        EntityDecl,
        error::{error, fatal_error, validity_error},
        handler::SAXHandler,
        parser::{ParserOption, ParserState, XMLReader},
        source::InputSource,
    },
};

impl<H: SAXHandler> XMLReader<ProgressiveParserSpec, H> {
    /// Returns results based on the following conditions:
    /// - If a single event is successfully parsed, return `Ok(true)`.
    /// - If insufficient data is available to parse a single event, return `Ok(false)`.
    /// - If an unrecoverable error occurs during event parsing, return `Err`.
    pub(crate) fn parse_event_once(&mut self, finish: bool) -> Result<bool, XMLError> {
        loop {
            match self.state {
                ParserState::BeforeStart => {
                    if self.source.total_read() < 4 {
                        break if finish {
                            Err(XMLError::ParserUnexpectedEOF)
                        } else {
                            Ok(false)
                        };
                    }
                    if !self.fatal_error_occurred {
                        self.handler.set_document_locator(self.locator.clone());
                        self.handler.start_document();
                    }
                    self.state = ParserState::InXMLDeclaration;
                    break Ok(true);
                }
                ParserState::InXMLDeclaration => {
                    let content = self.source.content_bytes();
                    // 6 = "<?xml".len() + <the length of a whitespace character>
                    if content.len() < 6 {
                        // If all text has already been read, the XML declaration does not follow,
                        // so we can proceed to the next state.
                        if finish {
                            self.state = ParserState::InMiscAfterXMLDeclaration;
                            continue;
                        }
                        break Ok(false);
                    }
                    // To distinguish it from processing instructions appearing in `prolog`,
                    // check up to `<?xml`.
                    if !content.starts_with(b"<?xml") {
                        self.state = ParserState::InMiscAfterXMLDeclaration;
                        continue;
                    }

                    // Additionally, to distinguish it from reserved processing instructions
                    // starting with `xml` (such as `xml-stylesheet`),
                    // ensure that they are followed by whitespace.
                    if !self.source.content_str()[5..].starts_with(|c| self.is_whitespace(c)) {
                        self.state = ParserState::InMiscAfterXMLDeclaration;
                        continue;
                    }

                    self.specific_context.seen = self.specific_context.seen.max(5);
                    // Since the only '?' characters allowed in an XML declaration are '<?' or '?>',
                    // search for '?' in the closing '?>'.
                    break if let Some(pos) = self.source.content_bytes()
                        [self.specific_context.seen..]
                        .iter()
                        .position(|&b| b == b'?')
                    {
                        self.specific_context.seen += pos;
                        if self.specific_context.seen + 1 < self.source.content_bytes().len() {
                            if self.source.content_bytes()[self.specific_context.seen + 1] == b'>' {
                                self.parse_xml_decl()?;
                                self.source.set_compact_mode();
                                self.specific_context.seen = 0;
                                self.state = ParserState::InMiscAfterXMLDeclaration;
                                Ok(true)
                            } else {
                                self.specific_context.seen += 1;
                                Ok(false)
                            }
                        } else {
                            Ok(false)
                        }
                    } else {
                        self.specific_context.seen = self.source.content_bytes().len();
                        Ok(false)
                    };
                }
                ParserState::InMiscAfterXMLDeclaration => {
                    match self.try_parse_misc_once(ParserState::InInternalSubset, finish)? {
                        ControlFlow::Break(ret) => break Ok(ret),
                        ControlFlow::Continue(_) => continue,
                    }
                }
                ParserState::InInternalSubset => {
                    if self.source.content_bytes().len() < 9 {
                        break if finish {
                            self.state = ParserState::DocumentElement;
                            continue;
                        } else {
                            Ok(false)
                        };
                    }

                    if self.specific_context.seen == 0
                        && !self.source.content_bytes().starts_with(b"<!DOCTYPE")
                    {
                        self.state = ParserState::DocumentElement;
                        continue;
                    }

                    self.specific_context.seen = self.specific_context.seen.max(9);
                    while self.specific_context.seen < self.source.content_bytes().len() {
                        if self.specific_context.quote != 0 {
                            if let Some(pos) = self.source.content_bytes()
                                [self.specific_context.seen..]
                                .iter()
                                .position(|&b| b == self.specific_context.quote)
                            {
                                self.specific_context.seen += pos + 1;
                                self.specific_context.quote = 0;
                            } else {
                                self.specific_context.seen = self.source.content_bytes().len();
                                return Ok(false);
                            }
                        } else if self.specific_context.in_markup {
                            if let Some(pos) = self.source.content_bytes()
                                [self.specific_context.seen..]
                                .iter()
                                .position(|&b| matches!(b, b'\'' | b'"' | b'>'))
                            {
                                self.specific_context.seen += pos;
                                match self.source.content_bytes()[self.specific_context.seen] {
                                    b @ (b'\'' | b'"') => {
                                        self.specific_context.quote = b;
                                        self.specific_context.seen += 1;
                                    }
                                    b'>' => {
                                        self.specific_context.in_markup = false;
                                        self.specific_context.seen += 1;
                                    }
                                    _ => unreachable!(),
                                }
                            } else {
                                self.specific_context.seen = self.source.content_bytes().len();
                                return Ok(false);
                            }
                        } else if let Some(pos) = self.source.content_bytes()
                            [self.specific_context.seen..]
                            .iter()
                            .position(|&b| matches!(b, b'\'' | b'"' | b']' | b'>' | b'<'))
                        {
                            self.specific_context.seen += pos;
                            match self.source.content_bytes()[self.specific_context.seen] {
                                b @ (b'\'' | b'"') => {
                                    self.specific_context.quote = b;
                                    self.specific_context.seen += 1;
                                }
                                b']' => {
                                    if self.specific_context.seen + 1
                                        < self.source.content_bytes().len()
                                    {
                                        if self.source.content_bytes()
                                            [self.specific_context.seen + 1]
                                            == b'>'
                                        {
                                            // end doctypedecl
                                            self.parse_doctypedecl()?;
                                            self.specific_context.seen = 0;
                                            self.specific_context.quote = 0;
                                            self.specific_context.in_markup = false;
                                            self.state = ParserState::InMiscAfterDOCTYPEDeclaration;
                                            return Ok(true);
                                        }
                                        self.specific_context.seen += 1;
                                    } else {
                                        return Ok(false);
                                    }
                                }
                                b'>' => {
                                    // end doctypedecl
                                    self.parse_doctypedecl()?;
                                    self.specific_context.seen = 0;
                                    self.specific_context.quote = 0;
                                    self.specific_context.in_markup = false;
                                    self.state = ParserState::InMiscAfterDOCTYPEDeclaration;
                                    return Ok(true);
                                }
                                b'<' => {
                                    self.specific_context.in_markup = true;
                                    self.specific_context.seen += 1;
                                }
                                _ => unreachable!(),
                            }
                        } else {
                            self.specific_context.seen = self.source.content_bytes().len();
                            return Ok(false);
                        }
                    }

                    break if finish {
                        Err(XMLError::ParserUnexpectedEOF)
                    } else {
                        Ok(false)
                    };
                }
                ParserState::InMiscAfterDOCTYPEDeclaration => {
                    match self.try_parse_misc_once(ParserState::DocumentElement, finish)? {
                        ControlFlow::Break(ret) => break Ok(ret),
                        ControlFlow::Continue(_) => continue,
                    }
                }
                ParserState::DocumentElement => {
                    break self.try_parse_start_or_empty_tag_once(finish);
                }
                ParserState::InContent => {
                    // If we are currently in an entity other than a document entity,
                    // `self.source` is not in progressive mode, so data should be appended.
                    // Otherwise, nothing happens.
                    if self.entity_name().is_some() {
                        self.grow()?;
                        let event = match self.source.content_bytes() {
                            [b'<', b'?', ..] => {
                                self.parse_pi()?;
                                true
                            }
                            [b'<', b'!', b'-', b'-', ..] => {
                                self.parse_comment()?;
                                true
                            }
                            [b'<', b'!', b'[', b'C', b'D', b'A', b'T', b'A', b'[', ..] => {
                                self.parse_cdsect()?;
                                true
                            }
                            [b'<', b'/', ..] => false,
                            [b'<', ..] => {
                                let old_ns_stack_depth = self.namespaces.len();
                                let mut name = String::new();
                                let mut prefix_length = 0;
                                let is_empty_tag =
                                    self.parse_start_or_empty_tag(&mut name, &mut prefix_length)?;

                                if is_empty_tag {
                                    self.report_end_element(&name, prefix_length);
                                    self.resume_namespace_stack(old_ns_stack_depth);
                                    self.finish_content_model_validation(&name);
                                    if self.specific_context.element_stack.is_empty() {
                                        self.state = ParserState::InMiscAfterDocumentElement;
                                    }
                                } else {
                                    self.specific_context.element_stack.push((
                                        name,
                                        prefix_length,
                                        old_ns_stack_depth,
                                    ));
                                    self.state = ParserState::InContent;
                                }
                                true
                            }
                            [b'&', b'#', ..] => {
                                // Character references are treated as part of the character data.
                                self.parse_char_data()?;
                                true
                            }
                            [b'&', ..] => false,
                            [_b, ..] => {
                                self.parse_char_data()?;
                                true
                            }
                            [] => false,
                        };

                        if event {
                            break Ok(true);
                        }
                    }

                    if self.source.content_bytes().is_empty() {
                        break if let Some(name) = self.entity_name() {
                            self.pop_source()?;
                            let (old_element_stack_depth, old_version, old_encoding) =
                                self.specific_context.entity_stack.pop().unwrap();
                            if self.specific_context.element_stack.len() != old_element_stack_depth
                            {
                                fatal_error!(
                                    self,
                                    ParserEntityIncorrectNesting,
                                    "The entity '{}' is nested incorrectly.",
                                    name
                                );
                                return Err(XMLError::ParserEntityIncorrectNesting);
                            }
                            self.version = old_version;
                            self.encoding = old_encoding;
                            if !self.fatal_error_occurred {
                                self.handler.end_entity();
                            }
                            Ok(true)
                        } else if finish {
                            Err(XMLError::ParserUnexpectedEOF)
                        } else {
                            Ok(false)
                        };
                    }

                    match self.source.content_bytes() {
                        [b'<', ..] => {
                            if self.source.content_bytes().len() < 2 {
                                break if finish {
                                    Err(XMLError::ParserUnexpectedEOF)
                                } else {
                                    Ok(false)
                                };
                            }

                            match self.source.content_bytes()[1] {
                                b'?' => {
                                    self.specific_context.seen = self.specific_context.seen.max(2);
                                    while let Some(pos) = self.source.content_bytes()
                                        [self.specific_context.seen..]
                                        .iter()
                                        .position(|&b| b == b'?')
                                    {
                                        self.specific_context.seen += pos;
                                        if self.specific_context.seen + 1
                                            < self.source.content_bytes().len()
                                        {
                                            if self.source.content_bytes()
                                                [self.specific_context.seen + 1]
                                                == b'>'
                                            {
                                                self.parse_pi()?;
                                                self.specific_context.seen = 0;
                                                return Ok(true);
                                            } else {
                                                self.specific_context.seen += 1;
                                            }
                                        } else {
                                            break;
                                        }
                                    }
                                }
                                b'/' => {
                                    if self.entity_name().is_none()
                                        && !self.source.content_bytes().contains(&b'>')
                                    {
                                        return if finish {
                                            Err(XMLError::ParserUnexpectedEOF)
                                        } else {
                                            Ok(false)
                                        };
                                    }

                                    let end = self.parse_end_tag()?;
                                    let (start, prefix_length, old_ns_stack_depth) =
                                        self.specific_context.element_stack.pop().unwrap();
                                    self.check_element_type_match(&start, &end)?;
                                    self.report_end_element(&start, prefix_length);
                                    self.resume_namespace_stack(old_ns_stack_depth);
                                    self.finish_content_model_validation(&start);
                                    if self.specific_context.element_stack.is_empty() {
                                        self.state = ParserState::InMiscAfterDocumentElement;
                                    }
                                    return Ok(true);
                                }
                                b'!' => {
                                    if self.source.content_bytes().len() < 4 {
                                        return if finish {
                                            Err(XMLError::ParserUnexpectedEOF)
                                        } else {
                                            Ok(false)
                                        };
                                    }

                                    if self.source.content_bytes()[2] == b'-' {
                                        // comment
                                        self.specific_context.seen =
                                            self.specific_context.seen.max(4);
                                        while let Some(pos) = self.source.content_bytes()
                                            [self.specific_context.seen..]
                                            .iter()
                                            .position(|&b| b == b'-')
                                        {
                                            self.specific_context.seen += pos;
                                            if self.specific_context.seen + 2
                                                < self.source.content_bytes().len()
                                            {
                                                if self.source.content_bytes()
                                                    [self.specific_context.seen + 1]
                                                    == b'-'
                                                    && self.source.content_bytes()
                                                        [self.specific_context.seen + 2]
                                                        == b'>'
                                                {
                                                    self.parse_comment()?;
                                                    self.specific_context.seen = 0;
                                                    return Ok(true);
                                                } else {
                                                    self.specific_context.seen += 1;
                                                }
                                            } else {
                                                break;
                                            }
                                        }
                                    } else {
                                        // CDSect
                                        if self.source.content_bytes().len() < 9 {
                                            return if finish {
                                                Err(XMLError::ParserUnexpectedEOF)
                                            } else {
                                                Ok(false)
                                            };
                                        }

                                        self.specific_context.seen =
                                            self.specific_context.seen.max(9);
                                        while let Some(pos) = self.source.content_bytes()
                                            [self.specific_context.seen..]
                                            .iter()
                                            .position(|&b| b == b']')
                                        {
                                            self.specific_context.seen += pos;
                                            if self.specific_context.seen + 2
                                                < self.source.content_bytes().len()
                                            {
                                                if self.source.content_bytes()
                                                    [self.specific_context.seen + 1]
                                                    == b']'
                                                    && self.source.content_bytes()
                                                        [self.specific_context.seen + 2]
                                                        == b'>'
                                                {
                                                    self.parse_cdsect()?;
                                                    self.specific_context.seen = 0;
                                                    return Ok(true);
                                                } else {
                                                    self.specific_context.seen += 1;
                                                }
                                            } else {
                                                break;
                                            }
                                        }
                                    }
                                }
                                _ => break self.try_parse_start_or_empty_tag_once(finish),
                            }
                            return Ok(false);
                        }
                        [b'&', ..] => {
                            if self.source.content_bytes().len() < 2 {
                                break if finish {
                                    Err(XMLError::ParserUnexpectedEOF)
                                } else {
                                    Ok(false)
                                };
                            }

                            if self.source.content_bytes()[1] == b'#' {
                                // character reference

                                // If the current position is not within a document entity,
                                // parsing can begin.
                                if self.entity_name().is_some() {
                                    self.parse_char_data()?;
                                    self.specific_context.seen = 0;
                                    return Ok(true);
                                }

                                // skip the first of `&`
                                self.specific_context.seen = self.specific_context.seen.max(1);

                                // treat as CharData
                                while let Some(pos) = self.source.content_bytes()
                                    [self.specific_context.seen..]
                                    .iter()
                                    .position(|&b| matches!(b, b'<' | b'&'))
                                {
                                    self.specific_context.seen += pos;
                                    if self.source.content_bytes()[self.specific_context.seen]
                                        == b'<'
                                    {
                                        self.parse_char_data()?;
                                        self.specific_context.seen = 0;
                                        return Ok(true);
                                    } else if self.specific_context.seen + 1
                                        < self.source.content_bytes().len()
                                        && self.source.content_bytes()
                                            [self.specific_context.seen + 1]
                                            == b'#'
                                    {
                                        self.specific_context.seen += 1;
                                    } else {
                                        self.parse_char_data()?;
                                        self.specific_context.seen = 0;
                                        return Ok(true);
                                    }
                                }
                                self.specific_context.seen = self.source.content_bytes().len();
                                return Ok(false);
                            } else {
                                // general entity reference
                                match self.try_parse_and_push_general_entity(finish)? {
                                    ControlFlow::Break(ret) => break Ok(ret),
                                    ControlFlow::Continue(_) => continue,
                                }
                            }
                        }
                        _ => {
                            while let Some(pos) = self.source.content_bytes()
                                [self.specific_context.seen..]
                                .iter()
                                .position(|&b| matches!(b, b'<' | b'&'))
                            {
                                self.specific_context.seen += pos;
                                if self.source.content_bytes()[self.specific_context.seen] == b'<' {
                                    self.parse_char_data()?;
                                    self.specific_context.seen = 0;
                                    return Ok(true);
                                } else if self.specific_context.seen + 1
                                    < self.source.content_bytes().len()
                                    && self.source.content_bytes()[self.specific_context.seen + 1]
                                        == b'#'
                                {
                                    self.specific_context.seen += 1;
                                } else {
                                    self.parse_char_data()?;
                                    self.specific_context.seen = 0;
                                    return Ok(true);
                                }
                            }
                            self.specific_context.seen = self.source.content_bytes().len();
                            return Ok(false);
                        }
                    }
                }
                ParserState::InMiscAfterDocumentElement => {
                    match self.try_parse_misc_once(ParserState::Finished, finish)? {
                        ControlFlow::Break(ret) => break Ok(ret),
                        ControlFlow::Continue(_) => continue,
                    }
                }
                ParserState::InTextDeclaration => {
                    self.grow()?;
                    if !self.source.content_bytes().starts_with(b"<?xml") {
                        self.source.set_compact_mode();
                        self.state = ParserState::InContent;
                        continue;
                    }

                    self.parse_text_decl()?;
                    self.state = ParserState::InContent;
                    break Ok(true);
                }
                ParserState::Finished => {
                    break if !self.source.content_bytes().is_empty() {
                        fatal_error!(
                            self,
                            ParserUnexpectedDocumentContent,
                            "Unnecessary document content remains. (Elements, character data, etc.)"
                        );
                        Err(XMLError::ParserUnexpectedDocumentContent)
                    } else {
                        Ok(false)
                    };
                }
                state @ ParserState::InExternalSubset => {
                    // unused state
                    unreachable!("reached unused state: {state:?}")
                }
            }
        }
    }

    fn try_parse_misc_once(
        &mut self,
        next_state: ParserState,
        finish: bool,
    ) -> Result<ControlFlow<bool>, XMLError> {
        self.skip_whitespaces()?;
        if self.source.content_bytes().len() < 2 {
            return if finish && next_state == ParserState::Finished {
                // If the next state is `Finished`, hand off everything to the next state,
                // including error handling.
                self.state = next_state;
                if !self.fatal_error_occurred && self.source.content_bytes().is_empty() {
                    self.handler.end_document();
                }
                Ok(ControlFlow::Break(true))
            } else if finish {
                // Even the shortest markup requires 4 characters,
                // so if all data has already been inserted at this point,
                // it is no longer well-formed.
                Err(XMLError::ParserUnexpectedEOF)
            } else {
                Ok(ControlFlow::Break(false))
            };
        }

        match self.source.content_bytes() {
            [b'<', b'?', ..] => {
                self.specific_context.seen = self.specific_context.seen.max(2);
                while let Some(pos) = self.source.content_bytes()[self.specific_context.seen..]
                    .iter()
                    .position(|&b| b == b'?')
                {
                    self.specific_context.seen += pos;
                    if self.specific_context.seen + 1 == self.source.content_bytes().len() {
                        return if finish {
                            Err(XMLError::ParserUnexpectedEOF)
                        } else {
                            Ok(ControlFlow::Break(false))
                        };
                    }

                    if self.source.content_bytes()[self.specific_context.seen + 1] == b'>' {
                        self.parse_pi()?;
                        self.specific_context.seen = 0;
                        return Ok(ControlFlow::Break(true));
                    }
                    self.specific_context.seen += 1;
                }
                // We only reach this point if '?' is not found.
                self.specific_context.seen = self.source.content_bytes().len();
                Ok(ControlFlow::Break(false))
            }
            [b'<', b'!', ..] => {
                if self.source.content_bytes().len() < 7 {
                    return if finish {
                        Err(XMLError::ParserUnexpectedEOF)
                    } else {
                        Ok(ControlFlow::Break(false))
                    };
                }

                if self.source.content_bytes()[2] != b'-' || self.source.content_bytes()[3] != b'-'
                {
                    self.state = next_state;
                    return Ok(ControlFlow::Continue(()));
                }

                self.specific_context.seen = self.specific_context.seen.max(4);
                while let Some(pos) = self.source.content_bytes()[self.specific_context.seen..]
                    .iter()
                    .position(|&b| b == b'-')
                {
                    self.specific_context.seen += pos;
                    if self.specific_context.seen + 2 >= self.source.content_bytes().len() {
                        return if finish {
                            Err(XMLError::ParserUnexpectedEOF)
                        } else {
                            Ok(ControlFlow::Break(false))
                        };
                    }

                    if self.source.content_bytes()[self.specific_context.seen + 1] != b'-' {
                        self.specific_context.seen += 2;
                        continue;
                    }

                    if self.source.content_bytes()[self.specific_context.seen + 2] == b'>' {
                        self.parse_comment()?;
                        self.specific_context.seen = 0;
                        return Ok(ControlFlow::Break(true));
                    } else if self.source.content_bytes()[self.specific_context.seen + 2] == b'-' {
                        self.specific_context.seen += 1;
                    } else {
                        self.specific_context.seen += 3;
                    }
                }
                self.specific_context.seen = self.source.content_bytes().len();
                Ok(ControlFlow::Break(false))
            }
            _ => {
                self.state = next_state;
                Ok(ControlFlow::Continue(()))
            }
        }
    }

    fn try_parse_start_or_empty_tag_once(&mut self, finish: bool) -> Result<bool, XMLError> {
        if self.source.content_bytes().len() < 4 {
            return if finish {
                Err(XMLError::ParserUnexpectedEOF)
            } else {
                Ok(false)
            };
        }

        if !self.source.content_bytes().starts_with(b"<") {
            return Err(XMLError::ParserInvalidStartOrEmptyTag);
        }

        self.specific_context.seen = self.specific_context.seen.max(1);
        while self.specific_context.seen < self.source.content_bytes().len() {
            if self.specific_context.quote != 0 {
                // in attribute value

                if let Some(pos) = self.source.content_bytes()[self.specific_context.seen..]
                    .iter()
                    .position(|&b| b == self.specific_context.quote)
                {
                    self.specific_context.seen += pos + 1;
                    self.specific_context.quote = 0;
                } else {
                    self.specific_context.seen = self.source.content_bytes().len();
                    return if finish {
                        Err(XMLError::ParserUnexpectedEOF)
                    } else {
                        Ok(false)
                    };
                }
            } else {
                // out of attribute value

                if let Some(pos) = self.source.content_bytes()[self.specific_context.seen..]
                    .iter()
                    .position(|&b| matches!(b, b'\'' | b'"' | b'>'))
                {
                    self.specific_context.seen += pos;
                    match self.source.content_bytes()[self.specific_context.seen] {
                        b @ (b'\'' | b'"') => {
                            self.specific_context.quote = b;
                            self.specific_context.seen += 1;
                        }
                        b'>' => {
                            self.specific_context.seen = 0;

                            let old_ns_stack_depth = self.namespaces.len();
                            let mut name = String::new();
                            let mut prefix_length = 0;
                            let is_empty_tag =
                                self.parse_start_or_empty_tag(&mut name, &mut prefix_length)?;

                            if is_empty_tag {
                                self.report_end_element(&name, prefix_length);
                                self.resume_namespace_stack(old_ns_stack_depth);
                                self.finish_content_model_validation(&name);
                                if self.specific_context.element_stack.is_empty() {
                                    self.state = ParserState::InMiscAfterDocumentElement;
                                }
                            } else {
                                self.specific_context.element_stack.push((
                                    name,
                                    prefix_length,
                                    old_ns_stack_depth,
                                ));
                                self.state = ParserState::InContent;
                            }
                            return Ok(true);
                        }
                        _ => unreachable!(),
                    }
                } else {
                    self.specific_context.seen = self.source.content_bytes().len();
                    return if finish {
                        Err(XMLError::ParserUnexpectedEOF)
                    } else {
                        Ok(false)
                    };
                }
            }
        }

        if finish {
            Err(XMLError::ParserUnexpectedEOF)
        } else {
            Ok(false)
        }
    }

    /// If successfully parsed the entity reference, returns the next `ParserState`;  \
    /// if insufficient data was available for parsing, returns `None`;  \
    /// if an unrecoverable error occurred, returns `Err(XMLError)`.
    fn try_parse_and_push_general_entity(
        &mut self,
        finish: bool,
    ) -> Result<ControlFlow<bool>, XMLError> {
        let in_entity = self.entity_name().is_some();

        self.grow()?;
        if !self.source.content_bytes().starts_with(b"&") {
            return Err(XMLError::InternalError);
        }
        if !in_entity {
            self.specific_context.seen = self.specific_context.seen.max(1);
            if !self.source.content_bytes()[self.specific_context.seen..].contains(&b';') {
                self.specific_context.seen = self.source.content_bytes().len();
                return if finish {
                    Err(XMLError::ParserUnexpectedEOF)
                } else {
                    Ok(ControlFlow::Break(false))
                };
            }
            self.specific_context.seen = 0;
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

        self.grow()?;
        if !self.source.content_bytes().starts_with(b";") {
            fatal_error!(
                self,
                ParserInvalidEntityReference,
                "The entity reference does not end with ';'."
            );
            return Err(XMLError::ParserInvalidEntityReference);
        }

        // skip ';'
        self.source.advance(1)?;
        self.locator.update_column(|c| c + 1);

        if let Some(decl) = self.entities.get(name.as_str()) {
            if self.entity_recursion_check(name.as_str()) {
                // [WFC: No Recursion]
                fatal_error!(
                    self,
                    ParserEntityRecursion,
                    "The entity '{}' appears recursively.",
                    name
                );
                return Err(XMLError::ParserEntityRecursion);
            }
            match decl {
                EntityDecl::InternalGeneralEntity {
                    base_uri,
                    replacement_text,
                    in_external_markup,
                } => {
                    if *in_external_markup && self.standalone == Some(true) {
                        // [WFC: Entity Declared]
                        fatal_error!(
                            self,
                            ParserUndeclaredEntityReference,
                            "standalone='yes', but it does not reference any entities declared in the internal DTD."
                        );
                    } else {
                        let source = InputSource::from_content(replacement_text.as_ref());
                        let name: Arc<str> = name.into();
                        self.push_source(
                            Box::new(source),
                            base_uri.clone(),
                            Some(name.clone()),
                            URIString::parse(format!("?internal-entity.{name}"))?.into(),
                            None,
                        )?;

                        if !self.fatal_error_occurred {
                            self.handler.start_entity(&name);
                        }

                        self.specific_context.entity_stack.push((
                            self.specific_context.element_stack.len(),
                            self.version,
                            self.encoding.clone(),
                        ));
                        self.state = ParserState::InContent;
                        return Ok(ControlFlow::Break(true));
                    }
                }
                EntityDecl::ExternalGeneralParsedEntity {
                    base_uri,
                    system_id,
                    public_id,
                    in_external_markup,
                } => {
                    if *in_external_markup && self.standalone == Some(true) {
                        // [WFC: Entity Declared]
                        fatal_error!(
                            self,
                            ParserUndeclaredEntityReference,
                            "standalone='yes', but it does not reference any entities declared in the internal DTD."
                        );
                    } else if self.config.is_enable(ParserOption::ExternalGeneralEntities)
                        || self.config.is_enable(ParserOption::Validation)
                    {
                        match self.handler.resolve_entity(
                            &name,
                            public_id.as_deref(),
                            base_uri.as_ref(),
                            system_id.as_ref(),
                        ) {
                            Ok(source) => {
                                let source = source;
                                let name: Arc<str> = name.into();
                                self.push_source(
                                    Box::new(source),
                                    base_uri.clone(),
                                    Some(name.clone()),
                                    system_id.as_ref().into(),
                                    public_id.as_deref().map(Arc::from),
                                )?;

                                if !self.fatal_error_occurred {
                                    self.handler.start_entity(&name);
                                }

                                self.specific_context.entity_stack.push((
                                    self.specific_context.element_stack.len(),
                                    self.version,
                                    self.encoding.clone(),
                                ));
                                self.state = ParserState::InTextDeclaration;
                                return Ok(ControlFlow::Break(true));
                            }
                            Err(err) => {
                                error!(
                                    self,
                                    err,
                                    "The external general entity '{}' cannot be resolved.",
                                    name
                                );
                            }
                        }
                    } else if !self.fatal_error_occurred {
                        self.handler.skipped_entity(&name);
                        self.state = ParserState::InContent;
                        return Ok(ControlFlow::Break(true));
                    }
                }
                EntityDecl::ExternalGeneralUnparsedEntity { .. } => {
                    // [WFC: Parsed Entity]
                    fatal_error!(
                        self,
                        ParserInvalidEntityReference,
                        "The unparsed entity '{}' cannot be referred.",
                        name
                    );
                }
                EntityDecl::InternalParameterEntity { .. }
                | EntityDecl::ExternalParameterEntity { .. } => {
                    // The fact that we have reached this point suggests that the general
                    // entity has been mistakenly registered as a parameter entity somewhere.
                    unreachable!("Internal error: Reference name: {name}");
                }
            }
        } else {
            if self.standalone == Some(true)
                || (!self.has_internal_subset && !self.has_external_subset)
                || (!self.has_external_subset && !self.has_parameter_entity)
            {
                // [WFC: Entity Declared]
                fatal_error!(
                    self,
                    ParserEntityNotFound,
                    "The entity '{}' is not declared.",
                    name
                );
            } else if self.config.is_enable(ParserOption::Validation) {
                // [VC: Entity Declared]
                validity_error!(
                    self,
                    ParserEntityNotFound,
                    "The entity '{}' is not declared.",
                    name
                );
            }

            if !self.fatal_error_occurred {
                self.handler.skipped_entity(&name);
            }
            return Ok(ControlFlow::Break(true));
        }
        Ok(ControlFlow::Continue(()))
    }
}
