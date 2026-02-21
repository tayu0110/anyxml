//! APIs for parsing RELAX NG schema document and validate XML document using parsed schema.
//!
//! The current implementation supports only RELAX NG schemas using XML syntax.  \
//! Parsing schemas using Compact Syntax is not supported at this time.
//!
//! # Streaming validation
//! This crate supports SAX handler-based validators.  \
//! By generating handlers from parsed schemas and configuring them as custom SAX handlers
//! in the parser, it enables document parsing and validation at the same time.
//!
//! ## Example
//! ```rust
//! use anyxml::{
//!     relaxng::RelaxNGSchema,
//!     sax::{
//!         handler::DefaultSAXHandler,
//!         parser::XMLReaderBuilder,
//!     },
//! };
//!
//! // example of RELAX NG Tutorial 1. Getting started
//! // https://relaxng.org/tutorial-20011203.html
//! const SCHEMA: &str = r#"<element name="addressBook" xmlns="http://relaxng.org/ns/structure/1.0">
//!     <zeroOrMore>
//!         <element name="card">
//!             <element name="name"><text/></element>
//!             <element name="email"><text/></element>
//!         </element>
//!     </zeroOrMore>
//! </element>
//! "#;
//! const DOCUMENT: &str = r#"<addressBook>
//!     <card>
//!         <name>John Smith</name>
//!         <email>js@example.com</email>
//!     </card>
//!     <card>
//!         <name>Fred Bloggs</name>
//!         <email>fb@example.net</email>
//!     </card>
//! </addressBook>"#;
//!
//! let mut schema = RelaxNGSchema::parse_str(SCHEMA, None, Some(DefaultSAXHandler)).unwrap();
//! let validator = schema.new_validate_handler(DefaultSAXHandler);
//! let mut reader = XMLReaderBuilder::new().set_handler(validator).build();
//! reader.parse_str(DOCUMENT, None).unwrap();
//! // `last_error` holds the last validation error.
//! assert!(reader.handler.last_error.is_ok());
//! ```
//!
//! # Document tree validation
//! It is also possible to validate an already constructed document tree.
//!
//! Compared to streaming validation, a limitation is that elements in the document tree do
//! not have positional information within the document's string representation, so less
//! information is available from errors.  \
//! On the other hand, there is the advantage of being able to validate subtrees of the
//! document tree. This is useful when validating an XML document embedded within another
//! document.
//!
//! ## Example
//! ```rust
//! use anyxml::{
//!     relaxng::RelaxNGSchema,
//!     sax::{
//!         handler::DefaultSAXHandler,
//!         parser::XMLReaderBuilder,
//!     },
//!     tree::TreeBuildHandler,
//! };
//!
//! // a part of example of RELAX NG Tutorial 1. Getting started
//! // https://relaxng.org/tutorial-20011203.html
//! const SCHEMA: &str = r#"<element name="card" xmlns="http://relaxng.org/ns/structure/1.0">
//!     <element name="name"><text/></element>
//!     <element name="email"><text/></element>
//! </element>
//! "#;
//! const DOCUMENT: &str = r#"<addressBook>
//!     <card>
//!         <name>John Smith</name>
//!         <email>js@example.com</email>
//!     </card>
//!     <card>
//!         <name>Fred Bloggs</name>
//!         <email>fb@example.net</email>
//!     </card>
//! </addressBook>"#;
//!
//! let mut schema = RelaxNGSchema::parse_str(SCHEMA, None, Some(DefaultSAXHandler)).unwrap();
//! let mut reader = XMLReaderBuilder::new().set_handler(TreeBuildHandler::default()).build();
//! reader.parse_str(DOCUMENT, None).unwrap();
//! let document = reader.handler.document;
//! let element = document.document_element().unwrap();
//! // document subtree validation
//! let card = element.first_element_child().unwrap();
//! assert!(schema.validate(&card, DefaultSAXHandler).is_ok());
//! ```
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
    relaxng::{grammar::Grammar, parse::RelaxNGParseHandler, validate::ValidateHandler},
    sax::{handler::SAXHandler, parser::XMLReaderBuilder},
    uri::URIStr,
};

pub const XML_RELAX_NG_NAMESPACE: &str = "http://relaxng.org/ns/structure/1.0";

pub struct RelaxNGSchema {
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
            Ok(Self { grammar })
        } else {
            let mut parser = XMLReaderBuilder::new()
                .set_handler(RelaxNGParseHandler::default())
                .build();
            parser.parse_uri(uri, encoding)?;
            let grammar = parser.handler.simplification().map_err(|err| err.error)?;
            Ok(Self { grammar })
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
        uri: Option<&URIStr>,
        handler: Option<Handler>,
    ) -> Result<Self, XMLError> {
        if let Some(handler) = handler {
            let mut parser = XMLReaderBuilder::new()
                .set_handler(RelaxNGParseHandler::with_handler(handler))
                .build();
            parser.parse_reader(reader, encoding, uri)?;
            let grammar = parser.handler.simplification().map_err(|err| err.error)?;
            Ok(Self { grammar })
        } else {
            let mut parser = XMLReaderBuilder::new()
                .set_handler(RelaxNGParseHandler::default())
                .build();
            parser.parse_reader(reader, encoding, uri)?;
            let grammar = parser.handler.simplification().map_err(|err| err.error)?;
            Ok(Self { grammar })
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
        uri: Option<&URIStr>,
        handler: Option<Handler>,
    ) -> Result<Self, XMLError> {
        if let Some(handler) = handler {
            let mut parser = XMLReaderBuilder::new()
                .set_handler(RelaxNGParseHandler::with_handler(handler))
                .build();
            parser.parse_str(schema, uri)?;
            let grammar = parser.handler.simplification().map_err(|err| err.error)?;
            Ok(Self { grammar })
        } else {
            let mut parser = XMLReaderBuilder::new()
                .set_handler(RelaxNGParseHandler::default())
                .build();
            parser.parse_str(schema, uri)?;
            let grammar = parser.handler.simplification().map_err(|err| err.error)?;
            Ok(Self { grammar })
        }
    }

    /// Generate SAX handler for RELAX NG schema validation.
    ///
    ///
    pub fn new_validate_handler<H: SAXHandler>(&mut self, handler: H) -> ValidateHandler<'_, H> {
        self.grammar.new_validate_handler(handler)
    }
}
