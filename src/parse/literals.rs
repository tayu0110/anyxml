use std::sync::Arc;

use anyxml_uri::uri::URIString;

use crate::{
    ParserSpec,
    error::XMLError,
    sax::{
        EntityDecl,
        error::{error, fatal_error},
        parser::{ParserOption, XMLReader},
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
                    self,
                    ParserIncorrectLiteralQuotation,
                    "A character '0x{:X}' is not correct quotation mark for a literal.",
                    c as u32
                );
                self.locator.update_column(|c| c + 1);
                Err(XMLError::ParserIncorrectLiteralQuotation)
            }
            None => {
                fatal_error!(self, ParserUnexpectedEOF, "Unexpected EOF.");
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
                    self,
                    ParserIncorrectLiteralQuotation,
                    "The literal does not close with the correct quotation mark."
                );
                self.locator.update_column(|c| c + 1);
                Err(XMLError::ParserIncorrectLiteralQuotation)
            }
            None => {
                fatal_error!(self, ParserUnexpectedEOF, "Unexpected EOF.");
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

    /// # Note
    /// If the parsing is successful, the stored attribute values are normalized.  \
    /// However, normalization dependent on attribute value types is not performed.
    /// This is because when this method is used to parse attribute list declarations,
    /// the type of attribute value is unknown.
    ///
    /// ```text
    /// [10] AttValue ::= '"' ([^<&"] | Reference)* '"' | "'" ([^<&'] | Reference)* "'"
    /// ```
    pub(crate) fn parse_att_value(&mut self, buffer: &mut String) -> Result<(), XMLError> {
        let quote = self.check_literal_start()?;

        self.source.grow()?;
        let orig_entity_stack = self.entity_name_stack.len();
        self.parse_att_value_internal(buffer, quote, orig_entity_stack)?;
        // `Ok` is returned only when entity references are nested correctly.
        // Therefore, it is not necessary to check whether the stack state is correct.
        assert_eq!(orig_entity_stack, self.entity_name_stack.len());

        self.check_literal_end(quote)
    }

    fn parse_att_value_internal(
        &mut self,
        buffer: &mut String,
        quote: char,
        orig_entity_stack: usize,
    ) -> Result<(), XMLError> {
        self.source.grow()?;
        'outer: while !self.source.content_bytes().is_empty() {
            while !matches!(self.source.content_bytes()[0], b'<' | b'&')
                && self.source.content_bytes()[0] != quote as u8
            {
                match self.source.next_char()? {
                    Some('\r') => {
                        // End-of-Line normalization is performed before entity references
                        // are recognized, so there is no need to consider the possibility
                        // of references with line feeds at the beginning appearing
                        // immediately after.
                        //
                        // Reference: 3.3.3 Attribute-Value Normalization
                        if self.source.peek_char()? != Some('\n') {
                            self.locator.update_line(|l| l + 1);
                            self.locator.set_column(1);
                            buffer.push('\x20');
                        }
                    }
                    Some('\n') => {
                        self.locator.update_line(|l| l + 1);
                        self.locator.set_column(1);
                        buffer.push('\x20');
                    }
                    Some(c) if self.is_whitespace(c) => {
                        self.locator.update_column(|c| c + 1);
                        buffer.push('\x20');
                    }
                    Some(c) if self.is_char(c) => {
                        self.locator.update_column(|c| c + 1);
                        buffer.push(c);
                    }
                    Some(c) => {
                        fatal_error!(
                            self,
                            ParserInvalidCharacter,
                            "The character '0x{:X}' is not allowed in the XML document.",
                            c as u32
                        );
                        self.locator.update_column(|c| c + 1);
                    }
                    None => unreachable!(),
                }

                if self.source.content_bytes().is_empty() {
                    self.source.grow()?;
                    if self.source.content_bytes().is_empty() {
                        break 'outer;
                    }
                }
            }

            match self.source.content_bytes()[0] {
                b'<' => {
                    fatal_error!(
                        self,
                        ParserInvalidAttValue,
                        "AttValue must not contain '<'."
                    );
                    buffer.push('<');
                    self.source.advance(1)?;
                    self.locator.update_column(|c| c + 1);
                }
                b'&' => {
                    self.source.grow()?;
                    if self.source.content_bytes().starts_with(b"&#") {
                        // character reference
                        // Even if it is a whitespace character, it will not be normalized.
                        // Reference: 3.3.3 Attribute-Value Normalization
                        buffer.push(self.parse_char_ref()?);
                    } else {
                        // general refenrence
                        self.parse_entity_ref_in_att_value(buffer, quote, orig_entity_stack)?;
                    }
                }
                c if c == quote as u8 && self.entity_name_stack.len() != orig_entity_stack => {
                    // Within the included entity, quotes must also be treated as part of the data.
                    // Reference: 4.4.5 Included in Literal
                    buffer.push(quote);
                    self.source.advance(quote.len_utf8())?;
                    self.locator.update_column(|c| c + quote.len_utf8());
                }
                _ => break,
            }
            self.source.grow()?;
        }

        if self.source.content_bytes().is_empty()
            && self.entity_name_stack.len() == orig_entity_stack
        {
            fatal_error!(self, ParserUnexpectedEOF, "Unexpected EOF.");
        }
        Ok(())
    }

    fn parse_entity_ref_in_att_value(
        &mut self,
        buffer: &mut String,
        quote: char,
        orig_entity_stack: usize,
    ) -> Result<(), XMLError> {
        self.source.grow()?;
        if !self.source.content_bytes().starts_with(b"&") {
            fatal_error!(
                self,
                ParserInvalidEntityReference,
                "An entity reference does not start with '&'."
            );
            return Err(XMLError::ParserInvalidEntityReference);
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

        self.source.grow()?;
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
            if self
                .entity_name_stack
                .iter()
                .any(|ent| ent.as_deref() == Some(name.as_str()))
            {
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
                } => {
                    let source = InputSource::from_content(replacement_text.as_ref());
                    let name: Arc<str> = name.into();
                    self.push_source(
                        Box::new(source),
                        base_uri.clone(),
                        Some(name.clone()),
                        URIString::parse(format!("#internal-entity.{name}"))?.into(),
                        None,
                    )?;

                    if !self.fatal_error_occurred {
                        self.lexical_handler.start_entity(&name);
                    }

                    self.parse_att_value_internal(buffer, quote, orig_entity_stack)?;
                    self.source.grow()?;

                    if !self.source.is_empty() {
                        fatal_error!(
                            self,
                            ParserEntityIncorrectNesting,
                            "The entity '{}' is nested incorrectly.",
                            name
                        );
                        return Err(XMLError::ParserEntityIncorrectNesting);
                    }

                    self.pop_source()?;
                    if !self.fatal_error_occurred {
                        self.lexical_handler.end_entity();
                    }
                }
                EntityDecl::ExternalGeneralParsedEntity { .. } => {
                    // 4.4 XML Processor Treatment of Entities and References
                    // [Reference in Attribute Value]
                    // 4.4.4 Forbidden
                    fatal_error!(
                        self,
                        ParserInvalidEntityReference,
                        "An external entity reference is not allowed in AttValue"
                    );
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
            if self.config.is_enable(ParserOption::Validation) {
                // is it correct ???
                // I don't really understand the meaning of [WFC: Entity Declared],
                // so if I'm wrong, please let me know...
                if !self.has_external_subset || self.standalone == Some(true) {
                    // [WFC: Entity Declared]
                    fatal_error!(
                        self,
                        ParserEntityNotFound,
                        "The entity '{}' is not declared.",
                        name
                    );
                } else {
                    // [VC: Entity Declared]
                    error!(
                        self,
                        ParserEntityNotFound, "The entity '{}' is not declared.", name
                    );
                }
            } else {
                // [WFC: Entity Declared]
                if self.standalone == Some(true) {
                    fatal_error!(
                        self,
                        ParserEntityNotFound,
                        "The entity '{}' is not declared.",
                        name
                    );
                }
            }

            if !self.fatal_error_occurred {
                self.content_handler.skipped_entity(&name);
            }
        }
        Ok(())
    }

    pub(crate) fn parse_entity_value(&mut self, buffer: &mut String) -> Result<(), XMLError> {
        let quote = self.check_literal_start()?;
        self.parse_entity_value_internal(buffer, quote, false)?;
        self.check_literal_end(quote)
    }

    pub(crate) fn parse_entity_value_internal(
        &mut self,
        buffer: &mut String,
        quote: char,
        in_entity: bool,
    ) -> Result<(), XMLError> {
        loop {
            self.source.grow()?;
            match self.source.content_bytes() {
                [b'%', ..] => {
                    // skip '%'
                    self.source.advance(1)?;
                    self.locator.update_column(|c| c + 1);

                    let mut name = "%".to_owned();
                    if self.config.is_enable(ParserOption::Namespaces) {
                        self.parse_ncname(&mut name)?;
                    } else {
                        self.parse_name(&mut name)?;
                    }
                    self.source.grow()?;
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
                        if self
                            .entity_name_stack
                            .iter()
                            .any(|ent| ent.as_deref() == Some(name.as_str()))
                        {
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
                            EntityDecl::InternalGeneralEntity { .. }
                            | EntityDecl::ExternalGeneralParsedEntity { .. }
                            | EntityDecl::ExternalGeneralUnparsedEntity { .. } => {
                                // The fact that we have reached this point suggests that the general
                                // entity has been mistakenly registered as a parameter entity somewhere.
                                unreachable!("Internal error: Reference name: {name}");
                            }
                            EntityDecl::InternalParameterEntity {
                                base_uri,
                                replacement_text,
                            } => {
                                let source = InputSource::from_content(replacement_text);
                                let name: Arc<str> = name.into();
                                self.push_source(
                                    Box::new(source),
                                    base_uri.clone(),
                                    Some(name.clone()),
                                    URIString::parse(format!("#internal-entity.{name}"))?.into(),
                                    None,
                                )?;

                                self.parse_entity_value_internal(buffer, quote, true)?;
                                self.source.grow()?;

                                if !self.source.is_empty() {
                                    fatal_error!(
                                        self,
                                        ParserEntityIncorrectNesting,
                                        "The entity '{}' is nested incorrectly.",
                                        name
                                    );
                                    return Err(XMLError::ParserEntityIncorrectNesting);
                                }
                                self.pop_source()?;
                            }
                            EntityDecl::ExternalParameterEntity {
                                base_uri,
                                system_id,
                                public_id,
                            } => {
                                if self.config.is_enable(ParserOption::Validation)
                                    || self
                                        .config
                                        .is_enable(ParserOption::ExternalParameterEntities)
                                {
                                    match self.entity_resolver.resolve_entity(
                                        &name,
                                        public_id.as_deref(),
                                        base_uri,
                                        system_id,
                                    ) {
                                        Ok(source) => {
                                            let name: Arc<str> = name.into();
                                            self.push_source(
                                                Box::new(source),
                                                base_uri.clone(),
                                                Some(name.clone()),
                                                URIString::parse(format!(
                                                    "#internal-entity.{name}"
                                                ))?
                                                .into(),
                                                None,
                                            )?;

                                            self.parse_entity_value_internal(buffer, quote, true)?;
                                            self.source.grow()?;

                                            if !self.source.is_empty() {
                                                fatal_error!(
                                                    self,
                                                    ParserEntityIncorrectNesting,
                                                    "The entity '{}' is nested incorrectly.",
                                                    name
                                                );
                                                return Err(XMLError::ParserEntityIncorrectNesting);
                                            }
                                            self.pop_source()?;
                                        }
                                        Err(err) => {
                                            error!(
                                                self,
                                                err, "The entity '{}' cannot be resolved.", name
                                            );
                                            if !self.fatal_error_occurred {
                                                self.content_handler.skipped_entity(&name);
                                            }
                                        }
                                    }
                                } else if !self.fatal_error_occurred {
                                    self.content_handler.skipped_entity(&name);
                                }
                            }
                        }
                    } else {
                        if self.config.is_enable(ParserOption::Validation) {
                            // is it correct ???
                            // I don't really understand the meaning of [WFC: Entity Declared],
                            // so if I'm wrong, please let me know...
                            if !self.has_external_subset || self.standalone == Some(true) {
                                // [WFC: Entity Declared]
                                fatal_error!(
                                    self,
                                    ParserEntityNotFound,
                                    "The entity '{}' is not declared.",
                                    name
                                );
                            } else {
                                // [VC: Entity Declared]
                                error!(
                                    self,
                                    ParserEntityNotFound, "The entity '{}' is not declared.", name
                                );
                            }
                        } else {
                            // [WFC: Entity Declared]
                            if self.standalone == Some(true) {
                                fatal_error!(
                                    self,
                                    ParserEntityNotFound,
                                    "The entity '{}' is not declared.",
                                    name
                                );
                            }
                        }
                        if !self.fatal_error_occurred {
                            self.content_handler.skipped_entity(&name);
                        }
                    }
                }
                [b'&', b'#', ..] => buffer.push(self.parse_char_ref()?),
                [b'&', ..] => {
                    // skip '&'
                    self.source.advance(1)?;
                    self.locator.update_column(|c| c + 1);

                    let mut name = String::new();
                    if self.config.is_enable(ParserOption::Namespaces) {
                        self.parse_ncname(&mut name)?;
                    } else {
                        self.parse_name(&mut name)?;
                    }
                    self.source.grow()?;
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
                        // Since general entities are not expanded here,
                        // recursion checking is not necessary.

                        match decl {
                            EntityDecl::InternalGeneralEntity { .. }
                            | EntityDecl::ExternalGeneralParsedEntity { .. } => {
                                // 4.4.7 Bypassed
                                buffer.push('&');
                                buffer.push_str(&name);
                                buffer.push(';');
                            }
                            EntityDecl::ExternalGeneralUnparsedEntity { .. } => {
                                // 4.4.9 Error
                                error!(
                                    self,
                                    ParserInvalidEntityReference,
                                    "The unparsed entity '{}' must not appear in EntityValue.",
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
                        // In the case of an Unparsed Entity, it should be an error,
                        // but since no declaration is found at this point, it is
                        // processed as a Parsed Entity for now.
                        // If it is an Unparsed Entity, an error should occur when
                        // this entity is expanded, so there should be no problem.
                        buffer.push('&');
                        buffer.push_str(&name);
                        buffer.push(';');
                    }
                }
                [c, ..] if *c == quote as u8 => {
                    if in_entity {
                        // Within the included entity, quotes must also be treated as part of the data.
                        // Reference: 4.4.5 Included in Literal
                        self.source.advance(quote.len_utf8())?;
                        self.locator.update_column(|c| c + quote.len_utf8());
                        buffer.push(quote);
                    } else {
                        break Ok(());
                    }
                }
                [_, ..] => match self.source.next_char()? {
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
                    Some(c) if self.is_char(c) => {
                        buffer.push(c);
                        self.locator.update_column(|c| c + 1);
                    }
                    Some(c) => {
                        fatal_error!(
                            self,
                            ParserInvalidCharacter,
                            "The character '0x{:X}' is not allowed in the XML document.",
                            c as u32
                        );
                        buffer.push(c);
                        self.locator.update_column(|c| c + 1);
                    }
                    _ => unreachable!(),
                },
                [] => {
                    fatal_error!(self, ParserUnexpectedEOF, "Unexpected EOF.");
                    break Err(XMLError::ParserUnexpectedEOF);
                }
            }
        }
    }
}
