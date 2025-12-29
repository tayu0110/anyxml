use std::{collections::HashSet, sync::Arc};

use crate::{
    ParserSpec,
    error::XMLError,
    sax::{
        contentspec::{ContentSpec, ElementContent, ElementContentStateID},
        error::{error, fatal_error, validity_error},
        handler::SAXHandler,
        parser::{ParserOption, XMLReader},
        source::InputSource,
    },
};

impl<'a, Spec: ParserSpec<Reader = InputSource<'a>>, H: SAXHandler> XMLReader<Spec, H> {
    /// ```text
    /// [45] elementdecl ::= '<!ELEMENT' S Name S contentspec S? '>'
    ///                                                 [VC: Unique Element Type Declaration]
    /// [46] contentspec ::= 'EMPTY' | 'ANY' | Mixed | children
    /// ```
    pub(crate) fn parse_element_decl(&mut self) -> Result<(), XMLError> {
        self.grow()?;

        if !self.source.content_bytes().starts_with(b"<!ELEMENT") {
            fatal_error!(
                self,
                ParserInvalidElementDecl,
                "Element declaration must start with '<!ELEMENT'."
            );
            return Err(XMLError::ParserInvalidElementDecl);
        }
        // skip '<!ELEMENT'
        self.source.advance(9)?;
        self.locator.update_column(|c| c + 9);

        let base_source_id = self.source.source_id();
        let is_pe_base_source = self.entity_name().is_some_and(|name| name.starts_with('%'));
        let is_external_markup = self.is_external_markup();
        if self.skip_whitespaces_with_handle_peref(true)? == 0 {
            fatal_error!(
                self,
                ParserInvalidElementDecl,
                "Whitespaces are required after '<!ELEMENT' in element declaration."
            );
        }

        let mut name = String::new();
        if self.config.is_enable(ParserOption::Namespaces) {
            self.parse_qname(&mut name)?;
        } else {
            self.parse_name(&mut name)?;
        }

        if self.skip_whitespaces_with_handle_peref(true)? == 0 {
            fatal_error!(
                self,
                ParserInvalidElementDecl,
                "Whitespaces are required after Name in element declaration."
            );
        }
        self.grow()?;

        // parse contentspec
        let contentspec = match self.source.content_bytes() {
            [b'E', b'M', b'P', b'T', b'Y', ..] => {
                // skip 'EMPTY'
                self.source.advance(5)?;
                self.locator.update_column(|c| c + 5);
                ContentSpec::EMPTY
            }
            [b'A', b'N', b'Y', ..] => {
                // skip 'ANY'
                self.source.advance(3)?;
                self.locator.update_column(|c| c + 3);
                ContentSpec::ANY
            }
            _ => {
                if !self.source.content_bytes().starts_with(b"(") {
                    fatal_error!(
                        self,
                        ParserInvalidElementDecl,
                        "Element or Mixed content must start with '('."
                    );
                    return Err(XMLError::ParserInvalidElementDecl);
                }
                // skip '('
                self.source.advance(1)?;
                self.locator.update_column(|c| c + 1);
                let model_source_id = self.source.source_id();

                self.skip_whitespaces_with_handle_peref(true)?;
                self.grow()?;

                if self.source.content_bytes().starts_with(b"#PCDATA") {
                    // Mixed Content
                    // [51] Mixed ::= '(' S? '#PCDATA' (S? '|' S? Name)* S? ')*'
                    //              | '(' S? '#PCDATA' S? ')'

                    // skip '#PCDATA'
                    self.source.advance(7)?;
                    self.locator.update_column(|c| c + 7);

                    self.skip_whitespaces_with_handle_peref(true)?;
                    self.grow()?;
                    let mut ret = HashSet::new();
                    if self.source.content_bytes().starts_with(b"|") {
                        // skip '|'
                        self.source.advance(1)?;
                        self.locator.update_column(|c| c + 1);

                        self.skip_whitespaces_with_handle_peref(true)?;
                        let mut buffer = String::new();
                        if self.config.is_enable(ParserOption::Namespaces) {
                            self.parse_qname(&mut buffer)?;
                        } else {
                            self.parse_name(&mut buffer)?;
                        }
                        self.skip_whitespaces_with_handle_peref(true)?;
                        while self.source.content_bytes().starts_with(b"|") {
                            // skip '|'
                            self.source.advance(1)?;
                            self.locator.update_column(|c| c + 1);

                            if !ret.insert(buffer.as_str().into())
                                && self.config.is_enable(ParserOption::Validation)
                            {
                                // [VC: No Duplicate Types]
                                validity_error!(
                                    self,
                                    ParserDuplicateMixedContent,
                                    "'{}' is duplicated as a mixed content of element '{}'.",
                                    buffer,
                                    name
                                );
                            }
                            buffer.clear();
                            self.skip_whitespaces_with_handle_peref(true)?;
                            if self.config.is_enable(ParserOption::Namespaces) {
                                self.parse_qname(&mut buffer)?;
                            } else {
                                self.parse_name(&mut buffer)?;
                            }
                            self.skip_whitespaces_with_handle_peref(true)?;
                            if self.source.content_bytes().is_empty() {
                                self.grow()?;
                            }
                        }
                        if !ret.insert(buffer.as_str().into())
                            && self.config.is_enable(ParserOption::Validation)
                        {
                            // [VC: No Duplicate Types]
                            validity_error!(
                                self,
                                ParserDuplicateMixedContent,
                                "'{}' is duplicated as a mixed content of element '{}'.",
                                buffer,
                                name
                            );
                        }
                    }
                    if self.source.source_id() != model_source_id {
                        // [VC: Proper Group/PE Nesting]
                        validity_error!(
                            self,
                            ParserEntityIncorrectNesting,
                            "A parameter entity in an element declaration is nested incorrectly."
                        );
                    }
                    if self.source.content_bytes().starts_with(b")*") {
                        // skip ')*'
                        self.source.advance(2)?;
                        self.locator.update_column(|c| c + 2);
                    } else if self.source.content_bytes().starts_with(b")") {
                        if !ret.is_empty() {
                            fatal_error!(
                                self,
                                ParserInvalidElementDecl,
                                "Mixed Content with elements does not end with ')*'."
                            );
                        }
                        // skip ')'
                        self.source.advance(1)?;
                        self.locator.update_column(|c| c + 1);
                    } else {
                        fatal_error!(
                            self,
                            ParserInvalidElementDecl,
                            "Mixed Content is not wrapped by parentheses correctly."
                        );
                        return Err(XMLError::ParserInvalidElementDecl);
                    }

                    ContentSpec::Mixed(Arc::new(ret))
                } else {
                    // Element Content
                    // [47] children ::= (choice | seq) ('?' | '*' | '+')?
                    // [48] cp       ::= (Name | choice | seq) ('?' | '*' | '+')?
                    // [49] choice   ::= '(' S? cp ( S? '|' S? cp )+ S? ')'	[VC: Proper Group/PE Nesting]
                    // [50] seq      ::= '(' S? cp ( S? ',' S? cp )* S? ')' [VC: Proper Group/PE Nesting]
                    let mut content = ElementContent::new(is_external_markup);
                    let mut buffer = String::new();
                    self.parse_children(&mut buffer, &mut content, model_source_id)?;
                    if content.compile().unwrap() {
                        error!(
                            self,
                            ParserAmbiguousElementContentModel,
                            "The element content of '{}' is ambiguous.",
                            name
                        );
                    }
                    ContentSpec::Children(content)
                }
            }
        };

        self.skip_whitespaces_with_handle_peref(true)?;
        if self.source.source_id() != base_source_id {
            if is_pe_base_source {
                // [WFC: PE Between Declarations]
                fatal_error!(
                    self,
                    ParserEntityIncorrectNesting,
                    "The replacement text of a PE reference in a DeclSep must be extSubsetDecl."
                );
            } else {
                // [VC: Proper Declaration/PE Nesting]
                validity_error!(
                    self,
                    ParserEntityIncorrectNesting,
                    "A parameter entity in an element declaration is nested incorrectly."
                );
            }
        }

        self.grow()?;
        if !self.source.content_bytes().starts_with(b">") {
            fatal_error!(
                self,
                ParserInvalidElementDecl,
                "Element declaration does not end with '>'."
            );
            return Err(XMLError::ParserInvalidElementDecl);
        }
        // skip '>'
        self.source.advance(1)?;
        self.locator.update_column(|c| c + 1);

        if !self.fatal_error_occurred {
            self.handler.element_decl(&name, &contentspec);
        }
        // [VC: Unique Element Type Declaration]
        if self.elementdecls.insert(name, contentspec).is_err()
            && self.config.is_enable(ParserOption::Validation)
        {
            validity_error!(
                self,
                ParserDuplicateElementDecl,
                "An element declaration is duplicated."
            );
        }

        Ok(())
    }

    /// # Note
    /// Due to the implementation of `parse_element_decl`, the leading ‘(’ and subsequent
    /// whitespaces are considered to have already been consumed.  \
    /// This is a compromise due to the constraint that it is necessary to consume the
    /// leading ‘(’ and subsequent whitespace to determine whether ‘#PCDATA’ follows, in
    /// order to distinguish between mixed content and element content.
    ///
    /// ```text
    /// [47] children ::= (choice | seq) ('?' | '*' | '+')?
    /// [48] cp       ::= (Name | choice | seq) ('?' | '*' | '+')?
    /// [49] choice   ::= '(' S? cp ( S? '|' S? cp )+ S? ')' [VC: Proper Group/PE Nesting]
    /// [50] seq      ::= '(' S? cp ( S? ',' S? cp )* S? ')' [VC: Proper Group/PE Nesting]
    /// ```
    fn parse_children(
        &mut self,
        buffer: &mut String,
        model: &mut ElementContent,
        base_source_id: usize,
    ) -> Result<(), XMLError> {
        let mut id = self.parse_cp(buffer, model)?;
        self.skip_whitespaces_with_handle_peref(true)?;

        self.grow()?;
        match self.source.content_bytes() {
            [b'|', ..] => {
                while self.source.content_bytes().starts_with(b"|") {
                    // skip '|'
                    self.source.advance(1)?;
                    self.locator.update_column(|c| c + 1);

                    self.skip_whitespaces_with_handle_peref(true)?;
                    let id2 = self.parse_cp(buffer, model)?;
                    id = model.create_alternation(id, id2);
                    self.skip_whitespaces_with_handle_peref(true)?;
                }
            }
            [b',', ..] => {
                while self.source.content_bytes().starts_with(b",") {
                    // skip ','
                    self.source.advance(1)?;
                    self.locator.update_column(|c| c + 1);

                    self.skip_whitespaces_with_handle_peref(true)?;
                    let id2 = self.parse_cp(buffer, model)?;
                    id = model.create_catenation(id, id2);
                    self.skip_whitespaces_with_handle_peref(true)?;
                }
            }
            [b')', ..] => {}
            [_, ..] => {
                fatal_error!(
                    self,
                    ParserInvalidElementDecl,
                    "Unexpected character is occurred in an element declaration."
                );
                return Err(XMLError::ParserInvalidElementDecl);
            }
            _ => return Err(XMLError::ParserUnexpectedEOF),
        }

        if self.source.source_id() != base_source_id {
            // [VC: Proper Group/PE Nesting]
            validity_error!(
                self,
                ParserEntityIncorrectNesting,
                "A parameter entity in an element declaration is nested incorrectly."
            );
        }

        if !self.source.content_bytes().starts_with(b")") {
            fatal_error!(
                self,
                ParserInvalidElementDecl,
                "A choice or seq in contentspec in an element declaration does not end with '('."
            );
            return Err(XMLError::ParserInvalidElementDecl);
        }
        // skip ')'
        self.source.advance(1)?;
        self.locator.update_column(|c| c + 1);
        buffer.push(')');

        self.grow()?;
        if let [c @ (b'?' | b'*' | b'+'), ..] = self.source.content_bytes() {
            match c {
                b'?' => model.create_zero_or_one(id),
                b'*' => model.create_zero_or_more(id),
                b'+' => model.create_one_or_more(id),
                _ => unreachable!(),
            };
            self.source.advance(1)?;
            self.locator.update_column(|c| c + 1);
        }

        Ok(())
    }

    /// ```text
    /// [48] cp       ::= (Name | choice | seq) ('?' | '*' | '+')?
    /// ```
    fn parse_cp(
        &mut self,
        buffer: &mut String,
        model: &mut ElementContent,
    ) -> Result<ElementContentStateID, XMLError> {
        self.grow()?;

        let base_source_id = self.source.source_id();
        let mut id = if self.source.content_bytes().starts_with(b"(") {
            self.parse_choice_or_seq(buffer, model)?
        } else {
            // parse Name
            buffer.clear();
            if self.config.is_enable(ParserOption::Namespaces) {
                self.parse_qname(buffer)?;
            } else {
                self.parse_name(buffer)?;
            }
            model.create_name(buffer.as_str())
        };

        self.grow()?;
        if let [c @ (b'?' | b'*' | b'+'), ..] = self.source.content_bytes() {
            id = match c {
                b'?' => model.create_zero_or_one(id),
                b'*' => model.create_zero_or_more(id),
                b'+' => model.create_one_or_more(id),
                _ => unreachable!(),
            };
            self.source.advance(1)?;
            self.locator.update_column(|c| c + 1);
        }

        if self.source.source_id() != base_source_id {
            // [VC: Proper Group/PE Nesting]
            validity_error!(
                self,
                ParserEntityIncorrectNesting,
                "A parameter entity in an element declaration is nested incorrectly."
            );
        }

        Ok(id)
    }

    /// ```text
    /// [49] choice   ::= '(' S? cp ( S? '|' S? cp )+ S? ')' [VC: Proper Group/PE Nesting]
    /// [50] seq      ::= '(' S? cp ( S? ',' S? cp )* S? ')' [VC: Proper Group/PE Nesting]
    /// ```
    fn parse_choice_or_seq(
        &mut self,
        buffer: &mut String,
        model: &mut ElementContent,
    ) -> Result<ElementContentStateID, XMLError> {
        self.grow()?;
        if !self.source.content_bytes().starts_with(b"(") {
            fatal_error!(
                self,
                ParserInvalidElementDecl,
                "A choice or seq in contentspec in an element declaration does not start with '('."
            );
            return Err(XMLError::ParserInvalidElementDecl);
        }
        // skip '('
        self.source.advance(1)?;
        self.locator.update_column(|c| c + 1);

        let base_source_id = self.source.source_id();
        self.skip_whitespaces_with_handle_peref(true)?;

        let mut id = self.parse_cp(buffer, model)?;

        self.skip_whitespaces_with_handle_peref(true)?;

        self.grow()?;
        match self.source.content_bytes() {
            [b'|', ..] => {
                while self.source.content_bytes().starts_with(b"|") {
                    // skip '|'
                    self.source.advance(1)?;
                    self.locator.update_column(|c| c + 1);

                    self.skip_whitespaces_with_handle_peref(true)?;
                    let id2 = self.parse_cp(buffer, model)?;
                    id = model.create_alternation(id, id2);
                    self.skip_whitespaces_with_handle_peref(true)?;
                }
            }
            [b',', ..] => {
                while self.source.content_bytes().starts_with(b",") {
                    // skip ','
                    self.source.advance(1)?;
                    self.locator.update_column(|c| c + 1);

                    self.skip_whitespaces_with_handle_peref(true)?;
                    let id2 = self.parse_cp(buffer, model)?;
                    id = model.create_catenation(id, id2);
                    self.skip_whitespaces_with_handle_peref(true)?;
                }
            }
            [b')', ..] => {}
            [_, ..] => {
                fatal_error!(
                    self,
                    ParserInvalidElementDecl,
                    "Unexpected character is occurred in an element declaration."
                );
                return Err(XMLError::ParserInvalidElementDecl);
            }
            _ => return Err(XMLError::ParserUnexpectedEOF),
        }

        if self.source.source_id() != base_source_id {
            // [VC: Proper Group/PE Nesting]
            validity_error!(
                self,
                ParserEntityIncorrectNesting,
                "A parameter entity in an element declaration is nested incorrectly."
            );
        }

        if !self.source.content_bytes().starts_with(b")") {
            fatal_error!(
                self,
                ParserInvalidElementDecl,
                "A choice or seq in contentspec in an element declaration does not end with '('."
            );
            return Err(XMLError::ParserInvalidElementDecl);
        }
        // skip ')'
        self.source.advance(1)?;
        self.locator.update_column(|c| c + 1);

        Ok(id)
    }
}
