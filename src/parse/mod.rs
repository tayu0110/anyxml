pub mod literals;
pub mod tokens;

use crate::{
    AttributeType, CHARDATA_CHUNK_LENGTH, ContentSpec, DefaultDecl, DefaultParserSpec,
    ENCODING_NAME_LIMIT_LENGTH, XML_VERSION_NUM_LIMIT_LENGTH, XMLVersion,
    error::XMLError,
    sax::{
        error::{fatal_error, warning},
        parser::{ParserOption, ParserState, XMLReader},
    },
};

impl XMLReader<DefaultParserSpec<'_>> {
    pub(crate) fn parse_document(&mut self) -> Result<(), XMLError> {
        self.content_handler
            .set_document_locator(self.locator.clone());
        self.content_handler.start_document();
        self.state = ParserState::Parsing;
        self.parse_prolog()?;
        self.parse_element()?;
        self.parse_misc()?;
        self.content_handler().end_document();
        todo!()
    }

    pub(crate) fn parse_prolog(&mut self) -> Result<(), XMLError> {
        if self.source.content_bytes().starts_with(b"<?xml") {
            self.parse_xmldecl()?;
            self.state = ParserState::Parsing;
        }
        self.parse_misc()?;
        self.grow()?;
        if self.source.content_bytes().starts_with(b"<!DOCTYPE") {
            self.parse_doctypedecl()?;
            self.state = ParserState::Parsing;
            self.parse_misc()?;
        }
        Ok(())
    }

    pub(crate) fn parse_doctypedecl(&mut self) -> Result<(), XMLError> {
        self.grow()?;
        if !self.source.content_bytes().starts_with(b"<!DOCTYPE") {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidDoctypeDecl,
                self.locator,
                "The document type declaration must start with '<!DOCTYPE'."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidDoctypeDecl);
        }
        // skip '<!DOCTYPE'
        self.source.advance(9)?;
        self.locator.update_column(|c| c + 9);

        if self.skip_whitespaces()? == 0 {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidDoctypeDecl,
                self.locator,
                "Whitespaces are required after '<!DOCTYPE'."
            );
            self.state = ParserState::FatalErrorOccurred;
        }

        let mut name = String::new();
        if self.config.is_enable(ParserOption::Namespaces) {
            self.parse_qname(&mut name)?;
        } else {
            self.parse_name(&mut name)?;
        }

        let s = self.skip_whitespaces()?;
        self.grow()?;
        if self.source.is_empty() {
            fatal_error!(
                self.error_handler,
                XMLError::ParserUnexpectedEOF,
                self.locator,
                "Unexpected EOF"
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserUnexpectedEOF);
        }

        // If the following character is neither ‘[’ nor ‘>’, then there is an ExternalID.
        let mut system_id = None;
        let mut public_id = None;
        if !matches!(self.source.content_bytes()[0], b'[' | b'>') {
            if s == 0 {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserInvalidDoctypeDecl,
                    self.locator,
                    "Whitespaces are required between Name and ExternalID."
                );
                self.state = ParserState::FatalErrorOccurred;
            }
            self.parse_external_id(system_id.get_or_insert_default(), &mut public_id)?;
            self.skip_whitespaces()?;
        }
        if self.state != ParserState::FatalErrorOccurred {
            self.lexical_handler
                .start_dtd(&name, public_id.as_deref(), system_id.as_deref());
        }

        self.grow()?;
        // try to detect Internal Subset
        if self.source.content_bytes().starts_with(b"[") {
            // skip '['
            self.source.advance(1)?;
            self.locator.update_column(|c| c + 1);

            // parse internal subset
            self.parse_int_subset()?;

            self.grow()?;
            if !self.source.content_bytes().starts_with(b"]") {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserInvalidDoctypeDecl,
                    self.locator,
                    "']' for the end of internal DTD subset is not found."
                );
                self.state = ParserState::FatalErrorOccurred;
                return Err(XMLError::ParserInvalidDoctypeDecl);
            }
            // skip ']'
            self.source.advance(1)?;
            self.locator.update_column(|c| c + 1);

            self.skip_whitespaces()?;
        }

        self.grow()?;
        if !self.source.content_bytes().starts_with(b">") {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidDoctypeDecl,
                self.locator,
                "Document type declaration does not close with '>'."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidDoctypeDecl);
        }
        // skip '>'
        self.source.advance(1)?;
        self.locator.update_column(|c| c + 1);

        todo!("parse external subset");

        if self.state != ParserState::FatalErrorOccurred {
            self.lexical_handler.end_dtd();
        }

        Ok(())
    }

    pub(crate) fn parse_int_subset(&mut self) -> Result<(), XMLError> {
        self.state = ParserState::InInternalSubset;
        self.skip_whitespaces()?;

        loop {
            self.grow()?;
            match self.source.content_bytes() {
                [b'%', ..] => todo!("PEReference"),
                [b'<', b'?', ..] => self.parse_pi()?,
                [b'<', b'!', b'-', b'-', ..] => self.parse_comment()?,
                [b'<', b'!', b'E', b'L', ..] => self.parse_element_decl()?,
                [b'<', b'!', b'E', b'N', ..] => self.parse_entity_decl()?,
                [b'<', b'!', b'A', ..] => self.parse_attlist_decl()?,
                [b'<', b'!', b'N', ..] => self.parse_notation_decl()?,
                _ => break Ok(()),
            }

            self.skip_whitespaces()?;
        }
    }

    pub(crate) fn parse_element_decl(&mut self) -> Result<(), XMLError> {
        self.grow()?;

        if !self.source.content_bytes().starts_with(b"<!ELEMENT") {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidElementDecl,
                self.locator,
                "Element declaration must start with '<!ELEMENT'."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidElementDecl);
        }
        // skip '<!ELEMENT'
        self.source.advance(9)?;
        self.locator.update_column(|c| c + 9);

        if self.skip_whitespaces()? == 0 {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidElementDecl,
                self.locator,
                "Whitespaces are required after '<!ELEMENT' in element declaration."
            );
            self.state = ParserState::FatalErrorOccurred;
        }

        let mut name = String::new();
        if self.config.is_enable(ParserOption::Namespaces) {
            self.parse_qname(&mut name)?;
        } else {
            self.parse_name(&mut name)?;
        }

        if self.skip_whitespaces()? == 0 {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidElementDecl,
                self.locator,
                "Whitespaces are required after Name in element declaration."
            );
            self.state = ParserState::FatalErrorOccurred;
        }

        // parse contentspec
        self.grow()?;
        let contentspec = match self.source.content_bytes() {
            [b'E', b'M', b'P', b'T', b'Y', ..] => ContentSpec::EMPTY,
            [b'A', b'N', b'Y', ..] => ContentSpec::ANY,
            _ => {
                if !self.source.content_bytes().starts_with(b"(") {
                    fatal_error!(
                        self.error_handler,
                        XMLError::ParserInvalidElementDecl,
                        self.locator,
                        "Element or Mixed content must start with '('."
                    );
                    self.state = ParserState::FatalErrorOccurred;
                    return Err(XMLError::ParserInvalidElementDecl);
                }
                // skip '('
                self.source.advance(1)?;
                self.locator.update_column(|c| c + 1);

                self.skip_whitespaces()?;

                self.grow()?;
                if self.source.content_bytes().starts_with(b"#PCDATA") {
                    // Mixed Content
                    // [51] Mixed ::= '(' S? '#PCDATA' (S? '|' S? Name)* S? ')*'
                    //              | '(' S? '#PCDATA' S? ')'
                    self.skip_whitespaces()?;
                    self.grow()?;
                    let mut ret = vec![];
                    if self.source.content_bytes().starts_with(b"|") {
                        // skip '|'
                        self.source.advance(1)?;
                        self.locator.update_column(|c| c + 1);
                        self.skip_whitespaces()?;

                        let mut buffer = String::new();
                        if self.config.is_enable(ParserOption::Namespaces) {
                            self.parse_qname(&mut buffer)?;
                        } else {
                            self.parse_name(&mut buffer)?;
                        }
                        self.skip_whitespaces()?;
                        while self.source.content_bytes().starts_with(b"|") {
                            // skip '|'
                            self.source.advance(1)?;
                            self.locator.update_column(|c| c + 1);
                            self.skip_whitespaces()?;

                            ret.push(buffer.as_str().into());
                            buffer.clear();
                            if self.config.is_enable(ParserOption::Namespaces) {
                                self.parse_qname(&mut buffer)?;
                            } else {
                                self.parse_name(&mut buffer)?;
                            }
                            self.skip_whitespaces()?;
                            if self.source.content_bytes().is_empty() {
                                self.grow()?;
                            }
                        }
                        ret.push(buffer.into_boxed_str());
                    }
                    if !self.source.content_bytes().starts_with(b")") {
                        fatal_error!(
                            self.error_handler,
                            XMLError::ParserInvalidElementDecl,
                            self.locator,
                            "Mixed Content is not wrapped by parentheses correctly."
                        );
                        self.state = ParserState::FatalErrorOccurred;
                        return Err(XMLError::ParserInvalidElementDecl);
                    }
                    // skip ')'
                    self.source.advance(1)?;
                    self.locator.update_column(|c| c + 1);
                    ContentSpec::Mixed(ret)
                } else {
                    // Element Content
                    // [47] children ::= (choice | seq) ('?' | '*' | '+')?
                    // [48] cp       ::= (Name | choice | seq) ('?' | '*' | '+')?
                    // [49] choice   ::= '(' S? cp ( S? '|' S? cp )+ S? ')'	[VC: Proper Group/PE Nesting]
                    // [50] seq      ::= '(' S? cp ( S? ',' S? cp )* S? ')' [VC: Proper Group/PE Nesting]
                    todo!("Element Content")
                }
            }
        };

        self.skip_whitespaces()?;

        self.grow()?;
        if !self.source.content_bytes().starts_with(b">") {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidElementDecl,
                self.locator,
                "Element declaration does not end with '>'."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidElementDecl);
        }
        // skip '>'
        self.source.advance(1)?;
        self.locator.update_column(|c| c + 1);

        if self.state != ParserState::FatalErrorOccurred {
            self.decl_handler.element_decl(&name, contentspec);
        }

        Ok(())
    }

    pub(crate) fn parse_entity_decl(&mut self) -> Result<(), XMLError> {
        self.grow()?;

        if !self.source.content_bytes().starts_with(b"<!ENTITY") {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidEntityDecl,
                self.locator,
                "Entity declaration must start with '<!ENTITY'."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidEntityDecl);
        }
        // skip '<!ENTITY'
        self.source.advance(8)?;
        self.locator.update_column(|c| c + 8);

        if self.skip_whitespaces()? == 0 {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidEntityDecl,
                self.locator,
                "Whitespaces are required after '<!ENTITY' in entity declaration."
            );
            self.state = ParserState::FatalErrorOccurred;
        }

        self.grow()?;
        let mut pe = false;
        if self.source.next_char_if(|c| c == '%')?.is_some() {
            pe = true;
            self.locator.update_column(|c| c + 1);
            if self.skip_whitespaces()? == 0 {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserInvalidEntityDecl,
                    self.locator,
                    "Whitespaces are required after '%' in entity declaration."
                );
                self.state = ParserState::FatalErrorOccurred;
            }
        }

        let mut name = String::new();
        if self.config.is_enable(ParserOption::Namespaces) {
            self.parse_ncname(&mut name)?;
        } else {
            self.parse_name(&mut name)?;
        }

        if self.skip_whitespaces()? == 0 {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidEntityDecl,
                self.locator,
                "Whitespaces are required after Name in entity declaration."
            );
            self.state = ParserState::FatalErrorOccurred;
        }

        self.grow()?;
        match self.source.content_bytes() {
            [b'"' | b'\'', ..] => {
                todo!("EntityValue")
            }
            [b'S', b'Y', b'S', b'T', b'E', b'M', ..] | [b'P', b'U', b'B', b'L', b'I', b'C', ..] => {
                let mut system_id = String::new();
                let mut public_id = None;
                self.parse_external_id(&mut system_id, &mut public_id)?;

                let s = self.skip_whitespaces()?;
                self.grow()?;

                // If this is a general entity declaration, NDataDecl may follow.
                // [73] EntityDef ::= EntityValue | (ExternalID NDataDecl?)
                // If this is a parameter entity declaration, '>' must follow.
                // [74] PEDef     ::= EntityValue | ExternalID
                let mut ndata = None;
                if !pe && !self.source.content_bytes().starts_with(b">") {
                    if s == 0 {
                        fatal_error!(
                            self.error_handler,
                            XMLError::ParserInvalidEntityDecl,
                            self.locator,
                            "Whitespaces are required between ExternalID and NDataDecl."
                        );
                        self.state = ParserState::FatalErrorOccurred;
                    }

                    if !self.source.content_bytes().starts_with(b"NDATA") {
                        fatal_error!(
                            self.error_handler,
                            XMLError::ParserInvalidEntityDecl,
                            self.locator,
                            "NDataDecl must start with 'NDATA'."
                        );
                        self.state = ParserState::FatalErrorOccurred;
                        return Err(XMLError::ParserInvalidEntityDecl);
                    }
                    // skip 'NDATA'
                    self.source.advance(5)?;
                    self.locator.update_column(|c| c + 5);

                    if self.skip_whitespaces()? == 0 {
                        fatal_error!(
                            self.error_handler,
                            XMLError::ParserInvalidEntityDecl,
                            self.locator,
                            "Whitespaces are required after 'NDATA' in entity declaration."
                        );
                        self.state = ParserState::FatalErrorOccurred;
                    }

                    if self.config.is_enable(ParserOption::Namespaces) {
                        self.parse_ncname(ndata.get_or_insert_default())?;
                    } else {
                        self.parse_name(ndata.get_or_insert_default())?;
                    }
                    self.skip_whitespaces()?;
                }
                if !self.source.content_bytes().starts_with(b">") {
                    fatal_error!(
                        self.error_handler,
                        XMLError::ParserInvalidEntityDecl,
                        self.locator,
                        "Entity declaration does not end with '>'."
                    );
                    self.state = ParserState::FatalErrorOccurred;
                    return Err(XMLError::ParserInvalidEntityDecl);
                }
                // skip '>'
                self.source.advance(1)?;
                self.locator.update_column(|c| c + 1);

                if !pe && self.state != ParserState::FatalErrorOccurred {
                    if let Some(ndata) = ndata {
                        self.dtd_handler.unparsed_entity_decl(
                            &name,
                            public_id.as_deref(),
                            &system_id,
                            &ndata,
                        );
                    } else {
                        self.decl_handler.external_entity_decl(
                            &name,
                            public_id.as_deref(),
                            &system_id,
                        );
                    }
                }
            }
            _ => {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserInvalidEntityDecl,
                    self.locator,
                    "Neither EntityValue nor ExternalID are found in entity declaration."
                );
                self.state = ParserState::FatalErrorOccurred;
                return Err(XMLError::ParserInvalidEntityDecl);
            }
        }

        Ok(())
    }

    pub(crate) fn parse_attlist_decl(&mut self) -> Result<(), XMLError> {
        self.grow()?;

        if !self.source.content_bytes().starts_with(b"<!ATTLIST") {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidAttlistDecl,
                self.locator,
                "Attribute list declration must start with '<!ATTLIST'."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidAttlistDecl);
        }
        // skip '<!ATTLIST'
        self.source.advance(9)?;
        self.locator.update_column(|c| c + 9);

        if self.skip_whitespaces()? == 0 {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidAttlistDecl,
                self.locator,
                "Whitespaces are required after '<!ATTLIST' in attribute list declaration."
            );
            self.state = ParserState::FatalErrorOccurred;
        }

        let mut name = String::new();
        if self.config.is_enable(ParserOption::Namespaces) {
            self.parse_qname(&mut name)?;
        } else {
            self.parse_name(&mut name)?;
        }

        let mut s = self.skip_whitespaces()?;
        self.grow()?;
        let mut att_name = String::new();
        while !self.source.content_bytes().starts_with(b">") {
            if s == 0 {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserInvalidAttlistDecl,
                    self.locator,
                    "Whitespaces are required before Name in AttDef."
                );
                self.state = ParserState::FatalErrorOccurred;
            }
            att_name.clear();
            let (atttype, default_decl) = self.parse_att_def(false, &mut att_name)?;
            if self.state != ParserState::FatalErrorOccurred {
                self.decl_handler
                    .attribute_decl(&name, &att_name, atttype, default_decl);
            }
            s = self.skip_whitespaces()?;
            if self.source.content_bytes().is_empty() {
                self.grow()?;
                if self.source.content_bytes().is_empty() {
                    break;
                }
            }
        }

        if !self.source.content_bytes().starts_with(b">") {
            fatal_error!(
                self.error_handler,
                XMLError::ParserUnexpectedEOF,
                self.locator,
                "Unexpected EOF."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserUnexpectedEOF);
        }
        // skip '>'
        self.source.advance(1)?;
        self.locator.update_column(|c| c + 1);

        Ok(())
    }

    pub(crate) fn parse_att_def(
        &mut self,
        need_trim_whitespace: bool,
        att_name: &mut String,
    ) -> Result<(AttributeType, DefaultDecl), XMLError> {
        if need_trim_whitespace && self.skip_whitespaces()? == 0 {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidAttlistDecl,
                self.locator,
                "Whitespaces are required before Name in AttDef."
            );
            self.state = ParserState::FatalErrorOccurred;
        }

        if self.config.is_enable(ParserOption::Namespaces) {
            self.parse_qname(att_name)?;
        } else {
            self.parse_name(att_name)?;
        }

        if self.skip_whitespaces()? == 0 {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidAttlistDecl,
                self.locator,
                "Whitespaces are required before AttType in AttDef."
            );
            self.state = ParserState::FatalErrorOccurred;
        }

        // parse AttType
        self.grow()?;
        let atttype = match self.source.content_bytes() {
            [b'(', ..] => {
                // Enumeration
                // [59] Enumeration ::= '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'

                // skip '('
                self.source.advance(1)?;
                self.locator.update_column(|c| c + 1);

                self.skip_whitespaces()?;
                let mut buffer = String::new();
                self.parse_nmtoken(&mut buffer)?;
                let mut ret = vec![];
                self.skip_whitespaces()?;
                self.grow()?;
                while self.source.content_bytes().starts_with(b"|") {
                    self.source.advance(1)?;
                    self.locator.update_column(|c| c + 1);
                    self.skip_whitespaces()?;
                    ret.push(buffer.as_str().into());
                    buffer.clear();
                    self.parse_nmtoken(&mut buffer)?;
                    self.skip_whitespaces()?;
                    if self.source.content_bytes().is_empty() {
                        self.grow()?;
                    }
                }
                ret.push(buffer.into_boxed_str());

                if !self.source.content_bytes().starts_with(b")") {
                    fatal_error!(
                        self.error_handler,
                        XMLError::ParserInvalidAttlistDecl,
                        self.locator,
                        "Enumerated Attribute Type declaration does not close with ')'."
                    );
                    self.state = ParserState::FatalErrorOccurred;
                    return Err(XMLError::ParserInvalidAttlistDecl);
                }
                // skip ')'
                self.source.advance(1)?;
                self.locator.update_column(|c| c + 1);

                AttributeType::Enumeration(ret)
            }
            [b'C', b'D', b'A', b'T', b'A', ..] => {
                // StringType
                // [55] StringType ::= 'CDATA'
                self.source.advance(5)?;
                self.locator.update_column(|c| c + 5);
                AttributeType::CDATA
            }
            [b'N', b'O', b'T', b'A', b'T', b'I', b'O', b'N', ..] => {
                // NotationType
                // [58] NotationType ::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'

                // skip 'NOTATION'
                self.source.advance(8)?;
                self.locator.update_column(|c| c + 8);

                if self.skip_whitespaces()? == 0 {
                    fatal_error!(
                        self.error_handler,
                        XMLError::ParserInvalidAttlistDecl,
                        self.locator,
                        "Whitespaces are required after 'NOTATION' in Notation Attribute Type declaration"
                    );
                    self.state = ParserState::FatalErrorOccurred;
                }

                self.grow()?;
                if !self.source.content_bytes().starts_with(b"(") {
                    fatal_error!(
                        self.error_handler,
                        XMLError::ParserInvalidAttlistDecl,
                        self.locator,
                        "'(' is required after 'NOTATION' in Notation Attribute Type declaration."
                    );
                    self.state = ParserState::FatalErrorOccurred;
                    return Err(XMLError::ParserInvalidAttlistDecl);
                }
                // skip '('
                self.source.advance(1)?;
                self.locator.update_column(|c| c + 1);

                self.skip_whitespaces()?;
                let mut buffer = String::new();
                if self.config.is_enable(ParserOption::Namespaces) {
                    self.parse_ncname(&mut buffer)?;
                } else {
                    self.parse_name(&mut buffer)?;
                }
                self.skip_whitespaces()?;
                self.grow()?;
                let mut ret = vec![];
                while self.source.content_bytes().starts_with(b"|") {
                    // skip '|'
                    self.source.advance(1)?;
                    self.locator.update_column(|c| c + 1);
                    self.skip_whitespaces()?;
                    ret.push(buffer.as_str().into());
                    if self.config.is_enable(ParserOption::Namespaces) {
                        self.parse_ncname(&mut buffer)?;
                    } else {
                        self.parse_name(&mut buffer)?;
                    }
                    self.skip_whitespaces()?;
                    if self.source.content_bytes().is_empty() {
                        self.grow()?;
                    }
                }
                ret.push(buffer.into_boxed_str());

                if !self.source.content_bytes().starts_with(b")") {
                    fatal_error!(
                        self.error_handler,
                        XMLError::ParserInvalidAttlistDecl,
                        self.locator,
                        "Enumerated Attribute Type declaration does not close with ')'."
                    );
                    self.state = ParserState::FatalErrorOccurred;
                    return Err(XMLError::ParserInvalidAttlistDecl);
                }
                // skip ')'
                self.source.advance(1)?;
                self.locator.update_column(|c| c + 1);

                AttributeType::NOTATION(ret)
            }
            // TokenizedType
            [b'I', b'D', b'R', b'E', b'F', b'S', ..] => {
                self.source.advance(6)?;
                self.locator.update_column(|c| c + 6);
                AttributeType::IDREFS
            }
            [b'I', b'D', b'R', b'E', b'F', ..] => {
                self.source.advance(5)?;
                self.locator.update_column(|c| c + 5);
                AttributeType::IDREF
            }
            [b'I', b'D', ..] => {
                self.source.advance(2)?;
                self.locator.update_column(|c| c + 2);
                AttributeType::ID
            }
            [b'E', b'N', b'T', b'I', b'T', b'I', b'E', b'S', ..] => {
                self.source.advance(8)?;
                self.locator.update_column(|c| c + 8);
                AttributeType::ENTITIES
            }
            [b'E', b'N', b'T', b'I', b'T', b'Y', ..] => {
                self.source.advance(6)?;
                self.locator.update_column(|c| c + 6);
                AttributeType::ENTITY
            }
            [b'N', b'M', b'T', b'O', b'K', b'E', b'N', b'S', ..] => {
                self.source.advance(8)?;
                self.locator.update_column(|c| c + 8);
                AttributeType::NMTOKENS
            }
            [b'N', b'M', b'T', b'O', b'K', b'E', b'N', ..] => {
                self.source.advance(7)?;
                self.locator.update_column(|c| c + 7);
                AttributeType::NMTOKEN
            }
            _ => {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserInvalidAttlistDecl,
                    self.locator,
                    "AttType cannot be recognized."
                );
                self.state = ParserState::FatalErrorOccurred;
                return Err(XMLError::ParserInvalidAttlistDecl);
            }
        };

        if self.skip_whitespaces()? == 0 {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidAttlistDecl,
                self.locator,
                "Whitespaces are required between AttType and DefaultDecl in Attribute list declaration."
            );
            self.state = ParserState::FatalErrorOccurred;
        }

        // parse DefaultDecl
        self.grow()?;
        let default_decl = match self.source.content_bytes() {
            [b'#', b'R', b'E', b'Q', b'U', b'I', b'R', b'E', b'D', ..] => {
                // skip '#REQUIRED'
                self.source.advance(9)?;
                self.locator.update_column(|c| c + 9);
                DefaultDecl::REQUIRED
            }
            [b'#', b'I', b'M', b'P', b'L', b'I', b'E', b'D', ..] => {
                // skip '#IMPLIED'
                self.source.advance(8)?;
                self.locator.update_column(|c| c + 8);
                DefaultDecl::IMPLIED
            }
            [b'#', b'F', b'I', b'X', b'E', b'D', ..] => {
                // skip '#FIXED'
                self.source.advance(6)?;
                self.locator.update_column(|c| c + 6);

                if self.skip_whitespaces()? == 0 {
                    fatal_error!(
                        self.error_handler,
                        XMLError::ParserInvalidAttlistDecl,
                        self.locator,
                        "Whitespaces are required after '#FIXED' in DefaultDecl for attribute list declaration."
                    );
                    self.state = ParserState::FatalErrorOccurred;
                }

                let mut buffer = String::new();
                self.parse_att_value(&mut buffer)?;
                DefaultDecl::FIXED(buffer.into_boxed_str())
            }
            _ => {
                let mut buffer = String::new();
                self.parse_att_value(&mut buffer)?;
                DefaultDecl::None(buffer.into_boxed_str())
            }
        };

        Ok((atttype, default_decl))
    }

    pub(crate) fn parse_notation_decl(&mut self) -> Result<(), XMLError> {
        self.grow()?;

        if !self.source.content_bytes().starts_with(b"<!NOTATION") {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidNotationDecl,
                self.locator,
                "Notation declration must start with '<!NOTATION'."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidNotationDecl);
        }
        // skip '<!NOTATION'
        self.source.advance(10)?;
        self.locator.update_column(|c| c + 10);

        if self.skip_whitespaces()? == 0 {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidNotationDecl,
                self.locator,
                "Whitespaces are required after '<!NOTATION' in Notation declaration."
            );
            self.state = ParserState::FatalErrorOccurred;
        }

        let mut name = String::new();
        if self.config.is_enable(ParserOption::Namespaces) {
            self.parse_ncname(&mut name)?;
        } else {
            self.parse_name(&mut name)?;
        }

        if self.skip_whitespaces()? == 0 {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidNotationDecl,
                self.locator,
                "Whitespaces are required after Name in Notation declaration."
            );
            self.state = ParserState::FatalErrorOccurred;
        }

        self.grow()?;
        match self.source.content_bytes() {
            [b'S', b'Y', b'S', b'T', b'E', b'M', ..] => {
                // If it starts with “SYSTEM,” it is surely an ExternalID.
                let mut system_id = String::new();
                self.parse_external_id(&mut system_id, &mut None)?;
                if self.state != ParserState::FatalErrorOccurred {
                    self.dtd_handler
                        .notation_decl(&name, None, Some(&system_id));
                }
            }
            [b'P', b'U', b'B', b'L', b'I', b'C', ..] => {
                // If it starts with “PUBLIC,” it is impossible to distinguish between ExternalID
                // and PublicID, so methods such as `parse_external_id` should not be used.
                //
                // [75] ExternalID ::= 'SYSTEM' S SystemLiteral
                //                      | 'PUBLIC' S PubidLiteral S SystemLiteral
                // [83] PublicID   ::= 'PUBLIC' S PubidLiteral

                // skip 'PUBLIC'
                self.source.advance(6)?;
                self.locator.update_column(|c| c + 6);

                if self.skip_whitespaces()? == 0 {
                    fatal_error!(
                        self.error_handler,
                        XMLError::ParserInvalidPubidLiteral,
                        self.locator,
                        "Whitespaces are required after 'PUBLIC' in Notation declaration."
                    );
                    self.state = ParserState::FatalErrorOccurred;
                }
                let mut public_id = String::new();
                self.parse_pubid_literal(&mut public_id)?;

                let s = self.skip_whitespaces()?;
                self.grow()?;
                // If '>' appears, notation declaration finished.
                if self.source.content_bytes().starts_with(b">") {
                    // skip '>'
                    self.source.advance(1)?;
                    self.locator.update_column(|c| c + 1);

                    if self.state != ParserState::FatalErrorOccurred {
                        self.dtd_handler
                            .notation_decl(&name, Some(&public_id), None);
                    }
                    return Ok(());
                }

                // If notation declaration has not finished,
                // whitespaces are required because SystemLiteral follows.
                if s == 0 {
                    fatal_error!(
                        self.error_handler,
                        XMLError::ParserInvalidPubidLiteral,
                        self.locator,
                        "Whitespaces are required before SystemLiteral in Notation declaration."
                    );
                    self.state = ParserState::FatalErrorOccurred;
                }

                let mut system_id = String::new();
                self.parse_system_literal(&mut system_id)?;
                self.skip_whitespaces()?;

                if self.state != ParserState::FatalErrorOccurred {
                    self.dtd_handler
                        .notation_decl(&name, Some(&public_id), Some(&system_id));
                }
            }
            _ => {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserInvalidNotationDecl,
                    self.locator,
                    "Notation declaration must have either ExternalID or PublicID."
                );
                self.state = ParserState::FatalErrorOccurred;
                return Err(XMLError::ParserInvalidNotationDecl);
            }
        }
        Ok(())
    }

    pub(crate) fn parse_external_id(
        &mut self,
        system_id: &mut String,
        public_id: &mut Option<String>,
    ) -> Result<(), XMLError> {
        self.grow()?;
        match self.source.content_bytes() {
            [b'S', b'Y', b'S', b'T', b'E', b'M', ..] => {
                // skip 'SYSTEM'
                self.source.advance(6)?;
                self.locator.update_column(|c| c + 6);
                if self.skip_whitespaces()? == 0 {
                    fatal_error!(
                        self.error_handler,
                        XMLError::ParserInvalidExternalID,
                        self.locator,
                        "Whitespaces are required after 'SYSTEM' in ExternalID."
                    );
                    self.state = ParserState::FatalErrorOccurred;
                }
                *public_id = None;
                self.parse_system_literal(system_id)?;
            }
            [b'P', b'U', b'B', b'L', b'I', b'C', ..] => {
                // skip 'PUBLIC'
                self.source.advance(6)?;
                self.locator.update_column(|c| c + 6);
                if self.skip_whitespaces()? == 0 {
                    fatal_error!(
                        self.error_handler,
                        XMLError::ParserInvalidExternalID,
                        self.locator,
                        "Whitespaces are required after 'PUBLIC' in ExternalID."
                    );
                    self.state = ParserState::FatalErrorOccurred;
                }
                self.parse_pubid_literal(public_id.get_or_insert_default())?;
                if self.skip_whitespaces()? == 0 {
                    fatal_error!(
                        self.error_handler,
                        XMLError::ParserInvalidExternalID,
                        self.locator,
                        "Whitespaces are required after PubidLiteral in ExternalID."
                    );
                    self.state = ParserState::FatalErrorOccurred;
                }
                self.parse_system_literal(system_id)?;
            }
            _ => {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserInvalidExternalID,
                    self.locator,
                    "ExternalID must start with 'SYSTEM' or 'PUBLIC'."
                );
                self.state = ParserState::FatalErrorOccurred;
                return Err(XMLError::ParserInvalidExternalID);
            }
        }
        Ok(())
    }

    pub(crate) fn parse_xmldecl(&mut self) -> Result<(), XMLError> {
        self.state = ParserState::InXMLDeclaration;
        self.grow()?;
        if !self.source.content_bytes().starts_with(b"<?xml") {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidXMLDecl,
                self.locator,
                "XML declaration must start with '<?xml'."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidXMLDecl);
        }
        // skip '<?xml'
        self.source.advance(5)?;
        self.locator.update_column(|c| c + 5);

        // parse VersionInfo
        let s = self.skip_whitespaces()?;
        if s == 0 {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidXMLDecl,
                self.locator,
                "Whitespaces are required between '<?xml' and 'version'."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidXMLDecl);
        }

        if !self.source.content_bytes().starts_with(b"version") {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidXMLDecl,
                self.locator,
                "VersionInfo is not found in XMLDecl."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidXMLDecl);
        }
        // skip 'version'
        self.source.advance(7)?;
        self.locator.update_column(|c| c + 7);

        self.skip_whitespaces()?;
        if !self.source.content_bytes().starts_with(b"=") {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidXMLDecl,
                self.locator,
                "'=' is not found after 'version' in XMLDecl."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidXMLDecl);
        }
        // skip '='
        self.source.advance(1)?;
        self.locator.update_column(|c| c + 1);
        self.skip_whitespaces()?;

        let quote = match self.source.next_char()? {
            Some(c @ ('"' | '\'')) => c,
            _ => {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserInvalidXMLDecl,
                    self.locator,
                    "The quotation marks in the version number are incorrect."
                );
                self.state = ParserState::FatalErrorOccurred;
                return Err(XMLError::ParserInvalidXMLDecl);
            }
        };
        self.locator.update_column(|c| c + 1);

        self.grow()?;
        let content = self.source.content_bytes();
        let limit = content.len().min(XML_VERSION_NUM_LIMIT_LENGTH);
        let mut major = 0;
        while major < limit && content[major].is_ascii_digit() {
            major += 1;
        }
        if major > 1 || content[0] != b'1' {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidXMLVersion,
                self.locator,
                "XML major version number must be '1'."
            );
            self.state = ParserState::FatalErrorOccurred;
        } else if content[major] != b'.' {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidXMLVersion,
                self.locator,
                "Invalid XML version number is found."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidXMLVersion);
        }
        let mut minor = major + 1;
        while minor < limit && content[minor].is_ascii_digit() {
            minor += 1;
        }
        if minor == limit {
            fatal_error!(
                self.error_handler,
                XMLError::ParserTooLongXMLVersionNumber,
                self.locator,
                "Too long XML version number is found."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserTooLongXMLVersionNumber);
        }
        let version = if major + 1 < minor {
            match content[..minor] {
                [b'1', b'.', b'0'] => XMLVersion::XML10,
                _ => XMLVersion::Unknown,
            }
        } else {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidXMLVersion,
                self.locator,
                "XML minor version number is not found."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidXMLVersion);
        };
        if version == XMLVersion::Unknown {
            warning!(
                self.error_handler,
                XMLError::ParserUnsupportedXMLVersion,
                self.locator,
                "Unsupported XML version number is found. Fallback to XML 1.0."
            );
        }
        if content[minor] != quote as u8 {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidXMLDecl,
                self.locator,
                "The quotation marks in the version number are incorrect."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidXMLDecl);
        }
        let version_str = unsafe {
            // # Safety
            // `content[..minor]` contains only ASCII digit or '.',
            // so this operation is safe.
            String::from_utf8_unchecked(content[..minor].to_owned())
        };
        self.source.advance(minor + 1)?;
        self.locator.update_column(|c| c + minor + 1);

        // parse EncodingDecl
        let mut s = self.skip_whitespaces()?;
        self.grow()?;
        let mut encoding = None;
        if self.source.content_bytes().starts_with(b"encoding") {
            if s == 0 {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserInvalidXMLDecl,
                    self.locator,
                    "Whitespaces are required before 'encoding'."
                );
                self.state = ParserState::FatalErrorOccurred;
            }
            encoding = Some(self.parse_encoding_decl(false)?);
            s = self.skip_whitespaces()?;
            self.grow()?;
        }

        // parse SDDecl
        let mut standalone = None;
        if self.source.content_bytes().starts_with(b"standalone") {
            if s == 0 {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserInvalidXMLDecl,
                    self.locator,
                    "Whitespaces are required before 'standalone'."
                );
                self.state = ParserState::FatalErrorOccurred;
            }
            standalone = Some(self.parse_sddecl(false)?);
            self.skip_whitespaces()?;
            self.grow()?;
        }

        if !self.source.content_bytes().starts_with(b"?>") {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidXMLDecl,
                self.locator,
                "XMLDecl is not closed with '?>'."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidXMLDecl);
        }
        // skip '?>'
        self.source.advance(2)?;
        self.locator.update_column(|c| c + 2);

        // If an encoding is provided from an external source, it is used as a priority.
        // If not, `self.encoding` is `None`, so `self.encoding` is initialized with the value
        // obtained from the XML declaration, and the decoder for `self.source` is switched.
        if let Some(encoding) = encoding.as_deref()
            && self.encoding.is_none()
            && self.source.switch_encoding(encoding).is_err()
        {
            fatal_error!(
                self.error_handler,
                XMLError::ParserUnsupportedEncoding,
                self.locator,
                "The declared encoding '{}' is not supported.",
                encoding
            );
            self.state = ParserState::FatalErrorOccurred;
            // We continue decoding the data using the encoding inferred from the BOM or
            // byte sequence at the beginning of the document entity, and attempt parsing
            // and error detection.
        }

        if self.state != ParserState::FatalErrorOccurred {
            self.content_handler
                .declaration(&version_str, encoding.as_deref(), standalone);
        }
        self.version = version;
        self.standalone = standalone;
        if self.encoding.is_none() {
            self.encoding = encoding;
        }
        Ok(())
    }

    pub(crate) fn parse_encoding_decl(
        &mut self,
        need_trim_whitespace: bool,
    ) -> Result<String, XMLError> {
        if need_trim_whitespace && self.skip_whitespaces()? == 0 {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidEncodingDecl,
                self.locator,
                "Whitespaces are required before 'encoding' for EncodingDecl."
            );
            self.state = ParserState::FatalErrorOccurred;
        }

        if !self.source.content_bytes().starts_with(b"encoding") {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidEncodingDecl,
                self.locator,
                "'encoding' is not found for EncodingDecl."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidEncodingDecl);
        }
        // skip 'encoding'
        self.source.advance(8)?;
        self.locator.update_column(|c| c + 8);

        self.skip_whitespaces()?;
        if self.source.next_char()? != Some('=') {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidEncodingDecl,
                self.locator,
                "'=' is not found after 'encoding' in EncodingDecl."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidEncodingDecl);
        }
        self.locator.update_column(|c| c + 1);
        self.skip_whitespaces()?;

        let quote = match self.source.next_char()? {
            Some(c @ ('"' | '\'')) => c,
            _ => {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserInvalidEncodingDecl,
                    self.locator,
                    "The quotation marks in the encoding name are incorrect."
                );
                self.state = ParserState::FatalErrorOccurred;
                return Err(XMLError::ParserInvalidEncodingDecl);
            }
        };
        self.locator.update_column(|c| c + 1);

        let encoding = self.parse_enc_name()?;
        self.grow()?;

        match self.source.next_char()? {
            Some(c) if c == quote => {
                self.locator.update_column(|c| c + 1);
            }
            Some(_) => {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserInvalidEncodingDecl,
                    self.locator,
                    "The quotation marks in the encoding name are incorrect."
                );
                self.state = ParserState::FatalErrorOccurred;
                return Err(XMLError::ParserInvalidEncodingDecl);
            }
            _ => {
                self.state = ParserState::FatalErrorOccurred;
                // If we call `grow` just before and `source` is empty,
                // `source` has probably reached EOF.
                return if self.source.is_empty() {
                    fatal_error!(
                        self.error_handler,
                        XMLError::ParserUnexpectedEOF,
                        self.locator,
                        "Unexpected EOF."
                    );
                    Err(XMLError::ParserUnexpectedEOF)
                } else {
                    fatal_error!(
                        self.error_handler,
                        XMLError::ParserInvalidEncodingDecl,
                        self.locator,
                        "The quotation marks in the encoding name are incorrect."
                    );
                    Err(XMLError::ParserInvalidEncodingDecl)
                };
            }
        }

        Ok(encoding)
    }

    pub(crate) fn parse_sddecl(&mut self, need_trim_whitespace: bool) -> Result<bool, XMLError> {
        if need_trim_whitespace && self.skip_whitespaces()? == 0 {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidSDDecl,
                self.locator,
                "Whitespaces are required before 'standalone' for SDDecl."
            );
            self.state = ParserState::FatalErrorOccurred;
        }

        self.grow()?;
        let content = self.source.content_bytes();
        if !content.starts_with(b"standalone") {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidSDDecl,
                self.locator,
                "'standalone' is not found for SDDecl."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidSDDecl);
        }
        // skip 'standalone'
        self.source.advance(10)?;
        self.locator.update_column(|c| c + 10);

        self.skip_whitespaces()?;
        self.grow()?;
        if !self.source.content_bytes().starts_with(b"=") {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidSDDecl,
                self.locator,
                "'=' is not found for SDDecl."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidSDDecl);
        }
        // skip '='
        self.source.advance(1)?;
        self.locator.update_column(|c| c + 10);

        let quote = match self.source.next_char()? {
            Some(c @ ('"' | '\'')) => c,
            _ => {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserInvalidSDDecl,
                    self.locator,
                    "The quotation marks in the standalone declaration are incorrect."
                );
                self.state = ParserState::FatalErrorOccurred;
                return Err(XMLError::ParserInvalidSDDecl);
            }
        };
        self.locator.update_column(|c| c + 1);

        let ret = match self.source.content_bytes() {
            [b'y', b'e', b's', ..] => {
                self.source.advance(3)?;
                self.locator.update_column(|c| c + 3);
                true
            }
            [b'n', b'o', ..] => {
                self.source.advance(2)?;
                self.locator.update_column(|c| c + 2);
                false
            }
            _ => {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserInvalidSDDecl,
                    self.locator,
                    "The value of SDDecl must be either 'yes' or 'no'."
                );
                self.state = ParserState::FatalErrorOccurred;
                return Err(XMLError::ParserInvalidXMLDecl);
            }
        };

        if self.source.next_char()? != Some(quote) {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidSDDecl,
                self.locator,
                "The quotation marks in the standalone declaration are incorrect."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidSDDecl);
        }
        self.locator.update_column(|c| c + 1);

        Ok(ret)
    }

    pub(crate) fn parse_enc_name(&mut self) -> Result<String, XMLError> {
        self.grow()?;

        let content = self.source.content_bytes();
        if content.is_empty() {
            self.state = ParserState::FatalErrorOccurred;
            // If we call `grow` just before and `source` is empty,
            // `source` has probably reached EOF.
            return if self.source.is_empty() {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserUnexpectedEOF,
                    self.locator,
                    "Unexpected EOF."
                );
                Err(XMLError::ParserUnexpectedEOF)
            } else {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserInvalidEncodingName,
                    self.locator,
                    "Data that may not be accepted as an encoding name has been detected."
                );
                Err(XMLError::ParserInvalidEncodingName)
            };
        }

        if !content[0].is_ascii_alphabetic() {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidEncodingName,
                self.locator,
                "The first character of an encoding name must be ASCII alphabetic."
            );
            self.state = ParserState::FatalErrorOccurred;
        }

        let limit = ENCODING_NAME_LIMIT_LENGTH.min(content.len());
        let mut cur = 0;
        while cur < limit
            && (content[cur].is_ascii_alphanumeric() || matches!(content[cur], b'.' | b'_' | b'-'))
        {
            cur += 1;
        }

        if cur == ENCODING_NAME_LIMIT_LENGTH {
            fatal_error!(
                self.error_handler,
                XMLError::PraserTooLongEncodingName,
                self.locator,
                "Too long encoding name is found."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::PraserTooLongEncodingName);
        } else if cur == content.len() {
            self.state = ParserState::FatalErrorOccurred;
            return if self.source.is_empty() {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserUnexpectedEOF,
                    self.locator,
                    "Unexpected EOF."
                );
                Err(XMLError::ParserUnexpectedEOF)
            } else {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserInvalidEncodingName,
                    self.locator,
                    "Data that may not be accepted as an encoding name has been detected."
                );
                Err(XMLError::ParserInvalidEncodingName)
            };
        }

        let ret = unsafe {
            // # Safety
            // `content[..cur]` contains only ASCII characters,
            // so this operation is safe.
            String::from_utf8_unchecked(content[..cur].to_owned())
        };
        self.source.advance(cur)?;
        self.locator.update_column(|c| c + cur);
        Ok(ret)
    }

    pub(crate) fn parse_element(&mut self) -> Result<(), XMLError> {
        todo!()
    }

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

    pub(crate) fn parse_comment(&mut self) -> Result<(), XMLError> {
        self.grow()?;
        if !self.source.content_bytes().starts_with(b"<!--") {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidComment,
                self.locator,
                "Comment does not start with '<!--'."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidComment);
        }
        // skip '<!--'
        self.source.advance(4)?;
        self.locator.update_column(|c| c + 4);

        self.grow()?;
        let mut buffer = String::new();
        while !self.source.content_bytes().starts_with(b"-->") {
            let next = self.source.next_char()?;
            match next {
                Some('-') => {
                    if self.source.peek_char()? == Some('-') {
                        fatal_error!(
                            self.error_handler,
                            XMLError::ParserInvalidComment,
                            self.locator,
                            "Comment must not contain '--' except for delimiters."
                        );
                        self.state = ParserState::FatalErrorOccurred;
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
                        self.error_handler,
                        XMLError::ParserInvalidCharacter,
                        self.locator,
                        "A character '0x{:X}' is not allowed in XML documents.",
                        c as u32
                    );
                    self.state = ParserState::FatalErrorOccurred;
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
            }

            if buffer.len() >= CHARDATA_CHUNK_LENGTH {
                if self.state != ParserState::FatalErrorOccurred {
                    self.lexical_handler.comment(&buffer);
                }
                buffer.clear();
            }
            if self.source.content_bytes().len() < 3 {
                self.grow()?;
            }
        }

        if !buffer.is_empty() && self.state != ParserState::FatalErrorOccurred {
            self.lexical_handler.comment(&buffer);
        }

        if !self.source.content_bytes().starts_with(b"-->") {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidComment,
                self.locator,
                "Comment does not end with '-->'."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidComment);
        }
        // skip '-->'
        self.source.advance(3)?;
        self.locator.update_column(|c| c + 3);

        Ok(())
    }

    pub(crate) fn parse_pi(&mut self) -> Result<(), XMLError> {
        self.grow()?;
        if !self.source.content_bytes().starts_with(b"<?") {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidProcessingInstruction,
                self.locator,
                "PI does not start with '<?'."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidProcessingInstruction);
        }
        // skip '<?'
        self.source.advance(2)?;
        self.locator.update_column(|c| c + 2);

        let mut target = String::new();
        if self.config.is_enable(ParserOption::Namespaces) {
            self.parse_ncname(&mut target)?;
        } else {
            self.parse_name(&mut target)?;
        }

        if target.eq_ignore_ascii_case("xml") {
            fatal_error!(
                self.error_handler,
                XMLError::ParserUnacceptablePITarget,
                self.locator,
                "PI target '{}' is not allowed.",
                target
            );
            self.state = ParserState::FatalErrorOccurred;
        }

        let s = self.skip_whitespaces()?;
        self.grow()?;
        if self.source.content_bytes().starts_with(b"?>") {
            // skip '?>'
            self.source.advance(2)?;
            self.locator.update_column(|c| c + 2);

            if self.state != ParserState::FatalErrorOccurred {
                self.content_handler.processing_instruction(&target, None);
            }

            return Ok(());
        }

        if s != 0 {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidProcessingInstruction,
                self.locator,
                "Whitespaces are required between PI target and data."
            );
            self.state = ParserState::FatalErrorOccurred;
        }

        let mut data = String::new();
        self.grow()?;
        while !self.source.content_bytes().starts_with(b"?>") {
            match self.source.next_char()? {
                Some('\r') => {
                    if self.source.peek_char()? != Some('\n') {
                        self.locator.update_line(|l| l + 1);
                        self.locator.set_column(1);
                        data.push('\n');
                    }
                }
                Some('\n') => {
                    self.locator.update_line(|l| l + 1);
                    self.locator.set_column(1);
                    data.push('\n');
                }
                Some(c) if self.is_char(c) => {
                    self.locator.update_column(|c| c + 1);
                    data.push(c);
                }
                Some(c) => {
                    self.locator.update_column(|c| c + 1);
                    data.push(c);
                    fatal_error!(
                        self.error_handler,
                        XMLError::ParserInvalidCharacter,
                        self.locator,
                        "A character '0x{:X}' is not allowed in XML document.",
                        c as u32
                    );
                    self.state = ParserState::FatalErrorOccurred;
                }
                None => {
                    fatal_error!(
                        self.error_handler,
                        XMLError::ParserUnexpectedEOF,
                        self.locator,
                        "Unexpected EOF."
                    );
                    self.state = ParserState::FatalErrorOccurred;
                }
            }
            if self.source.content_bytes().len() < 2 {
                self.grow()?;
            }
        }

        if !self.source.content_bytes().starts_with(b"?>") {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidProcessingInstruction,
                self.locator,
                "PI does not close with '?>'."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidProcessingInstruction);
        }

        if self.state != ParserState::FatalErrorOccurred {
            self.content_handler
                .processing_instruction(&target, Some(&data));
        }

        Ok(())
    }
}
