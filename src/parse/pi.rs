use anyxml_uri::uri::URIString;

use crate::{
    ParserSpec,
    catalog::CatalogEntryFile,
    error::XMLError,
    sax::{
        error::{error, fatal_error},
        handler::{DefaultSAXHandler, SAXHandler},
        parser::{ParserOption, ParserState, XMLReader},
        source::InputSource,
    },
};

pub(crate) const CATALOG_PI_TARGET_NAME: &str = "oasis-xml-catalog";

impl<'a, Spec: ParserSpec<Reader = InputSource<'a>>, H: SAXHandler> XMLReader<Spec, H> {
    /// ```text
    /// [16] PI       ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
    /// [17] PITarget ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
    /// ```
    pub(crate) fn parse_pi(&mut self) -> Result<(), XMLError> {
        self.grow()?;
        if !self.source.content_bytes().starts_with(b"<?") {
            fatal_error!(
                self,
                ParserInvalidProcessingInstruction,
                "PI does not start with '<?'."
            );
            return Err(XMLError::ParserInvalidProcessingInstruction);
        }
        // skip '<?'
        self.source.advance(2)?;
        self.locator.update_column(|c| c + 2);

        if let Some(Some((_, validator))) = self.validation_stack.last_mut() {
            // [VC: Element Valid]
            // Markup matching the `Misc` category is allowed as element content.
            validator.push_misc();
        }

        let mut target = String::new();
        if self.config.is_enable(ParserOption::Namespaces) {
            self.parse_ncname(&mut target)?;
        } else {
            self.parse_name(&mut target)?;
        }

        if target.eq_ignore_ascii_case("xml") {
            fatal_error!(
                self,
                ParserUnacceptablePITarget,
                "PI target '{}' is not allowed.",
                target
            );
        }

        let s = self.skip_whitespaces()?;
        self.grow()?;
        if self.source.content_bytes().starts_with(b"?>") {
            if self.config.is_enable(ParserOption::CatalogPIAware)
                && target == CATALOG_PI_TARGET_NAME
            {
                error!(
                    self,
                    ParserInvalidCatalogPIAttribute,
                    "The PI '{}' must have a single pseudo-attribute 'catalog'.",
                    CATALOG_PI_TARGET_NAME
                );
            }

            // skip '?>'
            self.source.advance(2)?;
            self.locator.update_column(|c| c + 2);

            if !self.fatal_error_occurred {
                self.handler.processing_instruction(&target, None);
            }

            return Ok(());
        }

        if s == 0 {
            fatal_error!(
                self,
                ParserInvalidProcessingInstruction,
                "Whitespaces are required between PI target and data."
            );
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
                        self,
                        ParserInvalidCharacter,
                        "A character '0x{:X}' is not allowed in XML document.",
                        c as u32
                    );
                }
                None => return Err(XMLError::ParserUnexpectedEOF),
            }
            if self.source.content_bytes().len() < 2 {
                self.grow()?;
            }
        }

        if self.config.is_enable(ParserOption::CatalogPIAware) && target == CATALOG_PI_TARGET_NAME {
            if self.state != ParserState::InMiscAfterXMLDeclaration {
                error!(
                    self,
                    ParserUnacceptableCatalogPIPosition,
                    "The PI '{}' must only appear between XML declaration and Documnet Type declaration.",
                    CATALOG_PI_TARGET_NAME
                );
            } else {
                self.parse_and_add_catalog_pi_attribute(&data);
            }
        }

        if !self.source.content_bytes().starts_with(b"?>") {
            fatal_error!(
                self,
                ParserInvalidProcessingInstruction,
                "PI does not close with '?>'."
            );
            return Err(XMLError::ParserInvalidProcessingInstruction);
        }
        // skip '?>'
        self.source.advance(2)?;
        self.locator.update_column(|c| c + 2);

        if !self.fatal_error_occurred {
            self.handler.processing_instruction(&target, Some(&data));
        }

        Ok(())
    }

    fn parse_and_add_catalog_pi_attribute(&mut self, data: &str) {
        if let Some(literal) = data
            .trim_matches(|c| self.is_whitespace(c))
            .strip_prefix("catalog")
            .map(|data| data.trim_start_matches(|c| self.is_whitespace(c)))
            .and_then(|data| data.strip_prefix('='))
            .map(|data| data.trim_start_matches(|c| self.is_whitespace(c)))
            .filter(|data| !data.is_empty())
        {
            let quote = literal.chars().next().unwrap();
            if matches!(quote, '"' | '\'')
                && let Some(catalog) = literal[1..]
                    .strip_suffix(quote)
                    .and_then(|value| URIString::parse(value).ok())
                    .map(|catalog| self.base_uri.resolve(&catalog))
                    .and_then(|catalog| {
                        CatalogEntryFile::parse_uri(
                            catalog,
                            None,
                            None::<DefaultSAXHandler>,
                            None::<DefaultSAXHandler>,
                        )
                        .ok()
                    })
            {
                self.pi_catalog.add(catalog);
            } else {
                error!(
                    self,
                    ParserInvalidCatalogPIAttribute,
                    "The value '{}' of 'catalog' pseudo-attribute for the PI '{}' cannot be parsed.",
                    literal,
                    CATALOG_PI_TARGET_NAME
                );
            }
        } else {
            error!(
                self,
                ParserInvalidCatalogPIAttribute,
                "The PI '{}' must have a single pseudo-attribute 'catalog', but '{}' is specified.",
                CATALOG_PI_TARGET_NAME,
                data
            );
        }
    }
}
