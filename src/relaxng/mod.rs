//! APIs for parsing RELAX NG schema document and validate XML document using parsed schema.
//!
//! The current implementation supports only RELAX NG schemas using XML syntax and only
//! supports tree validation. Parsing schemas using Compact Syntax and streaming validation
//! that can be inserted into SAX handlers are not supported at this time.
//!
//! # Reference
//! - [ISO/IEC 19757-2:2008 Information technology — Document Schema Definition Language (DSDL)Part 2: Regular-grammar-based validation — RELAX NG](https://www.iso.org/standard/52348.html)

mod datatype_library;
mod grammar;
mod parse;
mod validate;

use std::io::Read;

use crate::{
    error::XMLError,
    relaxng::{
        grammar::{Grammar, RelaxNGGrammar, ValidateHandler},
        parse::RelaxNGParseHandler,
    },
    sax::{handler::SAXHandler, parser::XMLReaderBuilder},
    uri::URIStr,
};

pub const XML_RELAX_NG_NAMESPACE: &str = "http://relaxng.org/ns/structure/1.0";

pub struct RelaxNGSchema {
    _grammar: RelaxNGGrammar,
    grammar: Grammar,
}

impl RelaxNGSchema {
    /// Parse RELAX NG schema using `uri` and `encoding`.
    ///
    /// If a custom [`EntityResolver`](crate::sax::handler::EntityResolver) or [`ErrorHandler`]
    /// is required, it can be specified using `handler`.
    ///
    /// If the document cannot be parsed for any reason,
    /// or the parsed document cannot be recognized as the RELAX NG schema, return [`Err`].
    pub fn parse_uri<Handler: SAXHandler>(
        uri: impl AsRef<URIStr>,
        encoding: Option<&str>,
        handler: Option<Handler>,
    ) -> Result<Self, XMLError> {
        if let Some(handler) = handler {
            let mut parser = XMLReaderBuilder::new()
                .set_handler(RelaxNGParseHandler::with_handler(handler))
                .build();
            parser.parse_uri(uri, encoding)?;
            let grammar = parser.handler.simplification().map_err(|err| err.error)?;
            let _grammar = parser.handler.build_grammar()?;
            Ok(Self { _grammar, grammar })
        } else {
            let mut parser = XMLReaderBuilder::new()
                .set_handler(RelaxNGParseHandler::default())
                .build();
            parser.parse_uri(uri, encoding)?;
            let grammar = parser.handler.simplification().map_err(|err| err.error)?;
            let _grammar = parser.handler.build_grammar()?;
            Ok(Self { _grammar, grammar })
        }
    }

    /// Parse RELAX NG schema using `reader`, `encoding` and `uri`.
    ///
    /// If a custom [`EntityResolver`](crate::sax::handler::EntityResolver) or [`ErrorHandler`]
    /// is required, it can be specified using `handler`.
    ///
    /// If the document cannot be parsed for any reason,
    /// or the parsed document cannot be recognized as the RELAX NG schema, return [`Err`].
    pub fn parse_reader<'a, Handler: SAXHandler>(
        reader: impl Read + 'a,
        encoding: Option<&str>,
        uri: impl AsRef<URIStr>,
        handler: Option<Handler>,
    ) -> Result<Self, XMLError> {
        if let Some(handler) = handler {
            let mut parser = XMLReaderBuilder::new()
                .set_handler(RelaxNGParseHandler::with_handler(handler))
                .build();
            parser.parse_reader(reader, encoding, Some(uri.as_ref()))?;
            let grammar = parser.handler.simplification().map_err(|err| err.error)?;
            let _grammar = parser.handler.build_grammar()?;
            Ok(Self { _grammar, grammar })
        } else {
            let mut parser = XMLReaderBuilder::new()
                .set_handler(RelaxNGParseHandler::default())
                .build();
            parser.parse_reader(reader, encoding, Some(uri.as_ref()))?;
            let grammar = parser.handler.simplification().map_err(|err| err.error)?;
            let _grammar = parser.handler.build_grammar()?;
            Ok(Self { _grammar, grammar })
        }
    }

    /// Parse RELAX NG schema using `schema` and `uri`.
    ///
    /// If a custom [`EntityResolver`](crate::sax::handler::EntityResolver) or [`ErrorHandler`]
    /// is required, it can be specified using `handler`.
    ///
    /// If the document cannot be parsed for any reason,
    /// or the parsed document cannot be recognized as the RELAX NG schema, return [`Err`].
    pub fn parse_str<Handler: SAXHandler>(
        schema: &str,
        uri: impl AsRef<URIStr>,
        handler: Option<Handler>,
    ) -> Result<Self, XMLError> {
        if let Some(handler) = handler {
            let mut parser = XMLReaderBuilder::new()
                .set_handler(RelaxNGParseHandler::with_handler(handler))
                .build();
            parser.parse_str(schema, Some(uri.as_ref()))?;
            let grammar = parser.handler.simplification().map_err(|err| err.error)?;
            let _grammar = parser.handler.build_grammar()?;
            Ok(Self { _grammar, grammar })
        } else {
            let mut parser = XMLReaderBuilder::new()
                .set_handler(RelaxNGParseHandler::default())
                .build();
            parser.parse_str(schema, Some(uri.as_ref()))?;
            let grammar = parser.handler.simplification().map_err(|err| err.error)?;
            let _grammar = parser.handler.build_grammar()?;
            Ok(Self { _grammar, grammar })
        }
    }

    pub fn new_validate_handler<H: SAXHandler>(&mut self, handler: H) -> ValidateHandler<'_, H> {
        self.grammar.new_validate_handler(handler)
    }
}
