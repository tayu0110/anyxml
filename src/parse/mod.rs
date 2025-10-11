mod cdsect;
mod char_data;
mod comment;
mod content;
mod dtd;
mod element;
pub mod literals;
mod pi;
mod progressive;
pub mod tokens;
mod xmldecl;

use std::{str::from_utf8_unchecked, sync::Arc};

use crate::{
    ParserSpec,
    error::XMLError,
    sax::{
        AttributeType, EntityDecl,
        error::{error, fatal_error, validity_error},
        handler::SAXHandler,
        parser::{ParserOption, ParserState, XMLReader},
        source::InputSource,
    },
    uri::URIString,
};

impl<'a, Spec: ParserSpec<Reader = InputSource<'a>>, H: SAXHandler> XMLReader<Spec, H> {
    /// ```text
    /// [1] document ::= prolog element Misc*
    /// ```
    pub(crate) fn parse_document(&mut self) -> Result<(), XMLError> {
        self.handler.set_document_locator(self.locator.clone());
        self.handler.start_document();
        self.parse_prolog()?;
        // At this point, the encoding should have been changed if necessary,
        // so set it to compact mode.
        self.source.set_compact_mode();
        self.state = ParserState::DocumentElement;
        self.parse_element()?;
        self.state = ParserState::InMiscAfterDocumentElement;
        self.parse_misc()?;
        self.grow()?;
        if !self.source.is_empty() {
            fatal_error!(
                self,
                ParserUnexpectedDocumentContent,
                "Unnecessary document content remains. (Elements, character data, etc.)"
            );
            return Err(XMLError::ParserUnexpectedDocumentContent);
        }

        self.state = ParserState::Finished;
        if !self.fatal_error_occurred
            && self.config.is_enable(ParserOption::Validation)
            && !self.unresolved_ids.is_empty()
        {
            // [VC: IDREF]
            for idref in self.unresolved_ids.drain() {
                validity_error!(
                    self,
                    ParserUnresolvableIDReference,
                    "IDREF '{}' has no referenced ID.",
                    idref
                );
            }
        }
        self.handler.end_document();
        Ok(())
    }

    /// ```text
    /// [22] prolog ::= XMLDecl? Misc* (doctypedecl Misc*)?
    /// ```
    pub(crate) fn parse_prolog(&mut self) -> Result<(), XMLError> {
        self.state = ParserState::InXMLDeclaration;
        self.grow()?;
        if self.source.content_bytes().starts_with(b"<?xml") {
            self.parse_xml_decl()?;
        }
        self.state = ParserState::InMiscAfterXMLDeclaration;
        self.parse_misc()?;
        self.grow()?;
        if self.source.content_bytes().starts_with(b"<!DOCTYPE") {
            self.state = ParserState::InInternalSubset;
            self.parse_doctypedecl()?;
            self.state = ParserState::InMiscAfterDOCTYPEDeclaration;
            self.parse_misc()?;
        }
        Ok(())
    }

    /// If an entity is pushed, return `true`, otherwise return `false`.
    ///
    /// # Note
    /// This method adds InputSource to the context depending on whether the configuration
    /// or reference resolution succeeds. If InputSource is added to the context,
    /// [`LexicalHandler::start_entity`](crate::sax::handler::LexicalHandler::start_entity)
    /// is called; otherwise,
    /// [`ContentHandler::skipped_entity`](crate::sax::handler::ContentHandler::skipped_entity)
    /// is called.  \
    /// If an InputSource is added, the caller should remove the InputSource that has reached
    /// EOF and call
    /// [`LexicalHandler::end_entity`](crate::sax::handler::LexicalHandler::end_entity).
    ///
    /// ```text
    /// [69] PEReference ::= '%' Name ';'   [VC:  Entity Declared]
    ///                                     [WFC: No Recursion]
    ///                                     [WFC: In DTD]
    /// ```
    pub(crate) fn parse_pe_reference(&mut self, in_decl: bool) -> Result<bool, XMLError> {
        // skip '%'
        self.source.advance(1)?;
        self.locator.update_column(|c| c + 1);

        let mut name = "%".to_owned();
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
                "A parameter reference does not end with ';'."
            );
            return Err(XMLError::ParserInvalidEntityReference);
        }
        // skip ';'
        self.source.advance(1)?;
        self.locator.update_column(|c| c + 1);

        if self.entity_recursion_check(name.as_str()) {
            // [WFC: No Recursion]
            fatal_error!(
                self,
                ParserEntityRecursion,
                "The parameter entity '{}' appears recursively.",
                name
            );
            return Err(XMLError::ParserEntityRecursion);
        }

        let mut entity_push = false;
        if self.config.is_enable(ParserOption::Validation)
            || self
                .config
                .is_enable(ParserOption::ExternalParameterEntities)
        {
            if let Some(decl) = self.entities.get(&name) {
                match decl {
                    EntityDecl::InternalParameterEntity {
                        base_uri,
                        replacement_text,
                    } => {
                        if self.config.is_enable(ParserOption::Validation) {
                            let source = InputSource::from_content(replacement_text.as_ref());
                            let name: Arc<str> = name.into();
                            self.push_source(
                                Box::new(source),
                                base_uri.clone(),
                                Some(name.clone()),
                                URIString::parse(format!("?internal-parameter-entity.{name}"))?
                                    .into(),
                                None,
                            )?;
                            entity_push = true;

                            if !in_decl && !self.fatal_error_occurred {
                                self.handler.start_entity(&name);
                            }
                        } else if !in_decl && !self.fatal_error_occurred {
                            self.handler.skipped_entity(&name);
                        }
                    }
                    EntityDecl::ExternalParameterEntity {
                        base_uri,
                        system_id,
                        public_id,
                    } => {
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
                                entity_push = true;

                                if !in_decl && !self.fatal_error_occurred {
                                    self.handler.start_entity(&name);
                                }
                            }
                            Err(err) => {
                                error!(
                                    self,
                                    err,
                                    "The external general entity '{}' cannot be resolved.",
                                    name
                                );
                                if !in_decl && !self.fatal_error_occurred {
                                    self.handler.skipped_entity(&name);
                                }
                            }
                        }
                    }
                    _ => unreachable!(),
                }
            } else {
                // [VC: Entity Declared]
                validity_error!(
                    self,
                    ParserEntityNotFound,
                    "The parameter entity '{}' is not declared.",
                    name
                );
                if !in_decl {
                    self.handler.skipped_entity(&name);
                }
            }
        } else if !in_decl && !self.fatal_error_occurred {
            self.handler.skipped_entity(&name);
        }
        Ok(entity_push)
    }

    /// Skip white space while handling parameter entity references.
    ///
    /// If `in_decl` is `true`, whitespace is considered to appear within the markup declaration.
    /// Parameter entity references within markup declarations in the internal subset are WFC
    /// violations, so a fatal error is reported.
    pub(crate) fn skip_whitespaces_with_handle_peref(
        &mut self,
        in_decl: bool,
    ) -> Result<usize, XMLError> {
        let mut s = self.skip_whitespaces()?;
        loop {
            self.grow()?;
            if self.source.content_bytes().starts_with(b"%") {
                let rem = unsafe {
                    // # Safety
                    // `self.source.content_bytes` is a valid UTF-8 byte sequence,
                    // and '%' is a single byte UTF-8 character.
                    // Therefore, `self.source.content_bytes[1..]` is also a valid
                    // UTF-8 byte sequence.
                    from_utf8_unchecked(&self.source.content_bytes()[1..])
                };
                // A single ‘%’ is definitely not a parameter entity, so processing ends.
                if rem.is_empty() || rem.starts_with(|c| self.is_whitespace(c)) {
                    break Ok(s);
                }
                if in_decl && self.state == ParserState::InInternalSubset {
                    fatal_error!(
                        self,
                        ParserInvalidEntityReference,
                        "A parameter entity appears in the markup declaration in an internal subset."
                    );
                    return Err(XMLError::ParserInvalidEntityReference);
                } else {
                    self.parse_pe_reference(in_decl)?;
                    s += self.skip_whitespaces()?;
                    self.grow()?;
                }
            } else if self.source.is_empty() {
                if self
                    .entity_name()
                    .is_none_or(|name| name.as_ref() == "[dtd]")
                {
                    // Since there's no popping source, exit the loop.
                    break Ok(s);
                }
                self.pop_source()?;
                if !in_decl && !self.fatal_error_occurred {
                    self.handler.end_entity();
                }
                s += 1 + self.skip_whitespaces()?;
                self.grow()?;
            } else {
                break Ok(s + self.skip_whitespaces()?);
            }
        }
    }

    /// ```text
    /// [27] Misc ::= Comment | PI | S
    /// ```
    pub(crate) fn parse_misc(&mut self) -> Result<(), XMLError> {
        self.skip_whitespaces()?;
        self.grow()?;

        loop {
            match self.source.content_bytes() {
                [b'<', b'!', b'-', b'-', ..] => self.parse_comment()?,
                [b'<', b'?', ..] => self.parse_pi()?,
                _ => break Ok(()),
            }
            self.skip_whitespaces()?;
            self.grow()?;
        }
    }

    /// ```text
    /// [66] CharRef ::= '&#' [0-9]+ ';' | '&#x' [0-9a-fA-F]+ ';' [WFC: Legal Character]
    /// ```
    pub(crate) fn parse_char_ref(&mut self) -> Result<char, XMLError> {
        self.source.grow()?;

        let (code, overflowed, hex, len) = match self.source.content_bytes() {
            [b'&', b'#', b'x', ..] => {
                // skip '&#x'
                self.source.advance(3)?;
                self.locator.update_column(|c| c + 3);

                self.source.grow()?;
                let content = self.source.content_bytes();
                let mut cur = 0;
                let mut code = 0u32;
                let mut overflowed = false;
                while cur < content.len() && content[cur].is_ascii_hexdigit() {
                    let (new, f) = code.overflowing_mul(16);
                    let (new, g) = if content[cur].is_ascii_digit() {
                        new.overflowing_add((content[cur] - b'0') as u32)
                    } else if content[cur].is_ascii_uppercase() {
                        new.overflowing_add((content[cur] - b'A' + 10) as u32)
                    } else {
                        new.overflowing_add((content[cur] - b'a' + 10) as u32)
                    };
                    code = new;
                    cur += 1;
                    overflowed |= f | g;
                }
                (code, overflowed, true, cur)
            }
            [b'&', b'#', ..] => {
                // skip '&#'
                self.source.advance(2)?;
                self.locator.update_column(|c| c + 2);

                self.source.grow()?;
                let content = self.source.content_bytes();
                let mut cur = 0;
                let mut code = 0u32;
                let mut overflowed = false;
                while cur < content.len() && content[cur].is_ascii_digit() {
                    let (new, f) = code.overflowing_mul(10);
                    let (new, g) = new.overflowing_add((content[cur] - b'0') as u32);
                    code = new;
                    cur += 1;
                    overflowed |= f | g;
                }
                (code, overflowed, false, cur)
            }
            _ => {
                fatal_error!(
                    self,
                    ParserInvalidCharacterReference,
                    "A character reference must start with '&#' or '&#x'."
                );
                return Err(XMLError::ParserInvalidCharacterReference);
            }
        };

        // skip the read characters
        self.source.advance(len)?;
        self.locator.update_column(|c| c + len);

        self.source.grow()?;
        let content = self.source.content_bytes();
        if content.is_empty() {
            Err(XMLError::ParserUnexpectedEOF)
        } else if (hex && content[0].is_ascii_hexdigit())
            || (!hex && content[0].is_ascii_digit())
            || overflowed
        {
            fatal_error!(
                self,
                ParserInvalidCharacterReference,
                "The code point specified by the character reference is too large."
            );
            Err(XMLError::ParserInvalidCharacterReference)
        } else if content[0] != b';' {
            fatal_error!(
                self,
                ParserInvalidCharacterReference,
                "The character reference does not end with ';'"
            );
            Err(XMLError::ParserInvalidCharacterReference)
        } else if len == 0 {
            fatal_error!(
                self,
                ParserInvalidCharacterReference,
                "'&#{};' is not a correct character reference.",
                if hex { "x" } else { "" }
            );
            Err(XMLError::ParserInvalidCharacterReference)
        } else if let Some(c) = char::from_u32(code).filter(|c| self.is_char(*c)) {
            // skip ';'
            self.source.advance(1)?;
            self.locator.update_column(|c| c + 1);

            Ok(c)
        } else {
            fatal_error!(
                self,
                ParserInvalidCharacter,
                "The code point '0x{:X}' does not indicate a character that is allowed in a XML document.",
                code
            );
            Err(XMLError::ParserInvalidCharacter)
        }
    }

    /// Returns `true` if normalized according to the declaration,
    /// and `false` if no declaration is found.
    ///
    /// If `is_cdata` is specified as `Some(true)` or `Some(false)`,
    /// determines whether the attribute value is of type CDATA based on the specified boolean value.  \
    /// In this case, `elem_name` and `attr_name` are not used.
    ///
    /// Since normalization that does not depend on attribute list declarations is
    /// performed along with attribute value parsing, this function only performs
    /// normalization  that depends on attribute list declarations.
    pub(crate) fn normalize_att_value(
        &self,
        elem_name: &str,
        attr_name: &str,
        att_value: &mut String,
        is_cdata: Option<bool>,
    ) -> bool {
        let is_cdata = if let Some(is_cdata) = is_cdata {
            is_cdata
        } else if let Some((att_type, _, _)) = self.attlistdecls.get(elem_name, attr_name) {
            matches!(att_type, AttributeType::CDATA)
        } else {
            return false;
        };

        // CDATA attribute values do not require space character normalization.
        if !is_cdata {
            unsafe {
                // # Safety
                // As long as the algorithm works correctly, only space characters
                // are normalized, so there are no violations of UTF-8 constraints.
                let bytes = att_value.as_bytes_mut();
                let mut filled = 0;
                let mut before_space = true;
                for i in 0..bytes.len() {
                    if bytes[i] != 0x20 {
                        bytes[filled] = bytes[i];
                        filled += 1;
                        before_space = false;
                    } else {
                        if !before_space {
                            bytes[filled] = 0x20;
                            filled += 1;
                        }
                        before_space = true;
                    }
                }
                // trim the tail of 0x20
                while filled > 0 && bytes[filled - 1] == 0x20 {
                    filled -= 1;
                }
                // To avoid violating UTF-8 constraints, fill with all NULL characters.
                bytes[filled..].fill(0);
                att_value.truncate(filled);
            }
        }
        true
    }
}
