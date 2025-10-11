use crate::{
    ParserSpec,
    error::XMLError,
    sax::{
        Notation,
        error::{fatal_error, validity_error},
        handler::SAXHandler,
        parser::{ParserOption, XMLReader},
        source::InputSource,
    },
    uri::URIString,
};

impl<'a, Spec: ParserSpec<Reader = InputSource<'a>>, H: SAXHandler> XMLReader<Spec, H> {
    /// ```text
    /// [82] NotationDecl ::= '<!NOTATION' S Name S (ExternalID | PublicID) S? '>'
    ///                                             [VC: Unique Notation Name]
    /// [83] PublicID     ::= 'PUBLIC' S PubidLiteral
    /// ```
    pub(crate) fn parse_notation_decl(&mut self) -> Result<(), XMLError> {
        self.grow()?;

        if !self.source.content_bytes().starts_with(b"<!NOTATION") {
            fatal_error!(
                self,
                ParserInvalidNotationDecl,
                "Notation declration must start with '<!NOTATION'."
            );
            return Err(XMLError::ParserInvalidNotationDecl);
        }
        // skip '<!NOTATION'
        self.source.advance(10)?;
        self.locator.update_column(|c| c + 10);

        let base_source_id = self.source.source_id();

        if self.skip_whitespaces_with_handle_peref(true)? == 0 {
            fatal_error!(
                self,
                ParserInvalidNotationDecl,
                "Whitespaces are required after '<!NOTATION' in Notation declaration."
            );
        }

        let mut name = String::new();
        if self.config.is_enable(ParserOption::Namespaces) {
            self.parse_ncname(&mut name)?;
        } else {
            self.parse_name(&mut name)?;
        }

        if self.skip_whitespaces_with_handle_peref(true)? == 0 {
            fatal_error!(
                self,
                ParserInvalidNotationDecl,
                "Whitespaces are required after Name in Notation declaration."
            );
        }

        self.grow()?;
        let mut system_id = None::<String>;
        let mut public_id = None::<String>;
        match self.source.content_bytes() {
            [b'S', b'Y', b'S', b'T', b'E', b'M', ..] => {
                // If it starts with “SYSTEM,” it is surely an ExternalID.
                let system_id = system_id.get_or_insert_default();
                self.parse_external_id(system_id, &mut None)?;
                if !self.fatal_error_occurred {
                    let system_id = URIString::parse(system_id)?;
                    self.handler.notation_decl(&name, None, Some(&system_id));
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

                if self.skip_whitespaces_with_handle_peref(true)? == 0 {
                    fatal_error!(
                        self,
                        ParserInvalidPubidLiteral,
                        "Whitespaces are required after 'PUBLIC' in Notation declaration."
                    );
                }
                let public_id = public_id.get_or_insert_default();
                self.parse_pubid_literal(public_id)?;

                let s = self.skip_whitespaces_with_handle_peref(true)?;
                self.grow()?;
                let mut system_id = None::<String>;
                // If '>' appears, notation declaration finished.
                if !self.source.content_bytes().starts_with(b">") {
                    // If notation declaration has not finished,
                    // whitespaces are required because SystemLiteral follows.
                    if s == 0 {
                        fatal_error!(
                            self,
                            ParserInvalidPubidLiteral,
                            "Whitespaces are required before SystemLiteral in Notation declaration."
                        );
                    }

                    self.parse_system_literal(system_id.get_or_insert_default())?;
                    self.skip_whitespaces_with_handle_peref(true)?;
                }

                if !self.fatal_error_occurred {
                    let system_id = system_id.map(URIString::parse).transpose()?;
                    self.handler
                        .notation_decl(&name, Some(public_id), system_id.as_deref());
                }
            }
            _ => {
                fatal_error!(
                    self,
                    ParserInvalidNotationDecl,
                    "Notation declaration must have either ExternalID or PublicID."
                );
                return Err(XMLError::ParserInvalidNotationDecl);
            }
        }

        self.skip_whitespaces_with_handle_peref(true)?;
        if self.source.source_id() != base_source_id {
            // [VC: Proper Declaration/PE Nesting]
            validity_error!(
                self,
                ParserEntityIncorrectNesting,
                "A parameter entity in a notation declaration is nested incorrectly."
            );
        }
        if !self.source.content_bytes().starts_with(b">") {
            fatal_error!(
                self,
                ParserInvalidNotationDecl,
                "A notation declaration does not end with '>'."
            );
            return Err(XMLError::ParserInvalidNotationDecl);
        }
        // skip '>'
        self.source.advance(1)?;
        self.locator.update_column(|c| c + 1);

        match self.notations.entry(name.as_str().into()) {
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(Notation {
                    name: name.into(),
                    system_id: system_id.map(From::from),
                    public_id: public_id.map(From::from),
                });
            }
            std::collections::hash_map::Entry::Occupied(_) => {
                if self.config.is_enable(ParserOption::Validation) {
                    // [VC: Unique Notation Name]
                    validity_error!(
                        self,
                        ParserDuplicateNotationDecl,
                        "The notation '{}' is duplicated.",
                        name
                    );
                }
            }
        }

        Ok(())
    }
}
