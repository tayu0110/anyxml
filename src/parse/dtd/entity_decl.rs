use anyxml_uri::uri::URIString;

use crate::{
    ParserSpec,
    error::XMLError,
    sax::{
        EntityDecl,
        error::{fatal_error, validity_error},
        handler::SAXHandler,
        parser::{ParserOption, XMLReader},
        source::InputSource,
    },
};

impl<'a, Spec: ParserSpec<Reader = InputSource<'a>>, H: SAXHandler> XMLReader<Spec, H> {
    /// ```text
    /// [70] EntityDecl ::= GEDecl | PEDecl
    /// [71] GEDecl     ::= '<!ENTITY' S Name S EntityDef S? '>'
    /// [72] PEDecl     ::= '<!ENTITY' S '%' S Name S PEDef S? '>'
    /// [73] EntityDef  ::= EntityValue | (ExternalID NDataDecl?)
    /// [74] PEDef      ::= EntityValue | ExternalID
    /// ```
    pub(crate) fn parse_entity_decl(&mut self) -> Result<(), XMLError> {
        self.grow()?;

        if !self.source.content_bytes().starts_with(b"<!ENTITY") {
            fatal_error!(
                self,
                ParserInvalidEntityDecl,
                "Entity declaration must start with '<!ENTITY'."
            );
            return Err(XMLError::ParserInvalidEntityDecl);
        }
        // skip '<!ENTITY'
        self.source.advance(8)?;
        self.locator.update_column(|c| c + 8);

        // The base URI of the entity is determined by the position of the first occurrence
        // of '<', so it must be saved at this point before the parameter entity is resolved.
        let base_uri = self.base_uri.clone();
        let base_source_id = self.source.source_id();
        let is_external_markup = self.is_external_markup();

        let mut s = self.skip_whitespaces_with_handle_peref(true)?;
        self.grow()?;
        let mut pe = false;
        if self.source.content_bytes().starts_with(b"%") {
            if s == 0 {
                fatal_error!(
                    self,
                    ParserInvalidEntityDecl,
                    "Whitespaces are required before '%' in a parameter entity declaration."
                );
            }

            pe = true;
            // skip '%'
            self.source.advance(1)?;
            self.locator.update_column(|c| c + 1);

            s = self.skip_whitespaces_with_handle_peref(true)?;
        }

        if s == 0 {
            fatal_error!(
                self,
                ParserInvalidEntityDecl,
                "Whitespaces are required before Name in an entity declaration."
            );
        }

        let mut name = if pe { "%".to_owned() } else { String::new() };
        if self.config.is_enable(ParserOption::Namespaces) {
            self.parse_ncname(&mut name)?;
        } else {
            self.parse_name(&mut name)?;
        }

        if self.skip_whitespaces_with_handle_peref(true)? == 0 {
            fatal_error!(
                self,
                ParserInvalidEntityDecl,
                "Whitespaces are required after Name in entity declaration."
            );
        }

        self.grow()?;
        let decl = match self.source.content_bytes() {
            [b'"' | b'\'', ..] => {
                let mut buffer = String::new();
                self.parse_entity_value(&mut buffer)?;
                self.skip_whitespaces_with_handle_peref(true)?;
                if pe {
                    EntityDecl::InternalParameterEntity {
                        base_uri,
                        replacement_text: buffer.into_boxed_str(),
                    }
                } else {
                    EntityDecl::InternalGeneralEntity {
                        base_uri,
                        replacement_text: buffer.into_boxed_str(),
                        in_external_markup: is_external_markup,
                    }
                }
            }
            [b'S', b'Y', b'S', b'T', b'E', b'M', ..] | [b'P', b'U', b'B', b'L', b'I', b'C', ..] => {
                let mut system_id = String::new();
                let mut public_id = None;
                self.parse_external_id(&mut system_id, &mut public_id)?;

                let s = self.skip_whitespaces_with_handle_peref(true)?;

                // If this is a general entity declaration, NDataDecl may follow.
                // [73] EntityDef ::= EntityValue | (ExternalID NDataDecl?)
                // If this is a parameter entity declaration, '>' must follow.
                // [74] PEDef     ::= EntityValue | ExternalID
                let mut ndata = None::<String>;
                if !pe && !self.source.content_bytes().starts_with(b">") {
                    if s == 0 {
                        fatal_error!(
                            self,
                            ParserInvalidEntityDecl,
                            "Whitespaces are required between ExternalID and NDataDecl."
                        );
                    }

                    if !self.source.content_bytes().starts_with(b"NDATA") {
                        fatal_error!(
                            self,
                            ParserInvalidEntityDecl,
                            "NDataDecl must start with 'NDATA'."
                        );
                        return Err(XMLError::ParserInvalidEntityDecl);
                    }
                    // skip 'NDATA'
                    self.source.advance(5)?;
                    self.locator.update_column(|c| c + 5);

                    if self.skip_whitespaces_with_handle_peref(true)? == 0 {
                        fatal_error!(
                            self,
                            ParserInvalidEntityDecl,
                            "Whitespaces are required after 'NDATA' in entity declaration."
                        );
                    }

                    let ndata = ndata.get_or_insert_default();
                    if self.config.is_enable(ParserOption::Namespaces) {
                        self.parse_ncname(ndata)?;
                    } else {
                        self.parse_name(ndata)?;
                    }

                    // Since notation declarations may appear after this declaration,
                    // the [VC: Notation Declared] check is performed after reading the entire DTD.

                    self.skip_whitespaces_with_handle_peref(true)?;
                }

                let system_id = URIString::parse(system_id)?;
                if !pe && !self.fatal_error_occurred {
                    if let Some(ndata) = ndata.as_deref() {
                        self.handler.unparsed_entity_decl(
                            &name,
                            public_id.as_deref(),
                            &system_id,
                            ndata,
                        );
                    } else {
                        self.handler
                            .external_entity_decl(&name, public_id.as_deref(), &system_id);
                    }
                }

                if pe {
                    EntityDecl::ExternalParameterEntity {
                        base_uri,
                        system_id: system_id.into(),
                        public_id: public_id.map(From::from),
                    }
                } else if let Some(notation) = ndata {
                    EntityDecl::ExternalGeneralUnparsedEntity {
                        base_uri,
                        system_id: system_id.into(),
                        public_id: public_id.map(From::from),
                        notation_name: notation.into(),
                    }
                } else {
                    EntityDecl::ExternalGeneralParsedEntity {
                        base_uri,
                        system_id: system_id.into(),
                        public_id: public_id.map(From::from),
                        in_external_markup: is_external_markup,
                    }
                }
            }
            [_, ..] => {
                fatal_error!(
                    self,
                    ParserInvalidEntityDecl,
                    "Neither EntityValue nor ExternalID are found in entity declaration."
                );
                return Err(XMLError::ParserInvalidEntityDecl);
            }
            [] => return Err(XMLError::ParserUnexpectedEOF),
        };

        if self.source.source_id() != base_source_id {
            // [VC: Proper Declaration/PE Nesting]
            validity_error!(
                self,
                ParserEntityIncorrectNesting,
                "A parameter entity in an element declaration is nested incorrectly."
            );
        }
        if !self.source.content_bytes().starts_with(b">") {
            fatal_error!(
                self,
                ParserInvalidEntityDecl,
                "Entity declaration does not end with '>'."
            );
            return Err(XMLError::ParserInvalidEntityDecl);
        }
        // skip '>'
        self.source.advance(1)?;
        self.locator.update_column(|c| c + 1);

        // Duplicate declarations are not errors.
        //
        // Reference: 4.2 Entity Declarations
        // > If the same entity is declared more than once, the first declaration
        // > encountered is binding; at user option, an XML processor MAY issue a
        // > warning if entities are declared multiple times.
        self.entities.insert(name, decl).ok();
        self.has_parameter_entity |= pe;

        Ok(())
    }
}
