//! APIs for parsing RELAX NG schema document and validate XML document using parsed schema.
//!
//! The current implementation supports both Full Syntax and Compact Syntax.
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
//!     sax::{DefaultSAXHandler, XMLReader},
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
//! let mut reader = XMLReader::builder().set_handler(validator).build();
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
//!     sax::{DefaultSAXHandler, XMLReader},
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
//! let mut reader = XMLReader::builder().set_handler(TreeBuildHandler::default()).build();
//! reader.parse_str(DOCUMENT, None).unwrap();
//! let document = reader.handler.document;
//! let element = document.document_element().unwrap();
//! // document subtree validation
//! let card = element.first_element_child().unwrap();
//! assert!(schema.validate(&card, DefaultSAXHandler).is_ok());
//! ```
//!
//! # Compact Syntax support
//! This crate supports schemas in Compact Syntax as well as Full Syntax (XML Syntax).  \
//! To parse a Compact Syntax schema, use the methods of [`RelaxNGSchema`], just as you
//! would with Full Syntax.
//!
//! Due to technical limitations, the schema is constructed by reading all resources
//! into memory and building the RELAX NG document tree, rather than parsing them in
//! a streaming manner.  \
//! The constructed document tree can be received as an event via a handler.  \
//! While conversion from Compact Syntax to Full Syntax is not explicitly supported, it is
//! possible to construct canonicalized XML or an XML document tree by forwarding events
//! received from the handler to the [`CanonicalizeHandler`](crate::c14n::CanonicalizeHandler)
//! or [`TreeBuildHandler`](crate::tree::TreeBuildHandler).
//!
//! ## Limitation
//! Schemas called via `include` or `externalRef` can be either Full Syntax or Compact Syntax.  \
//! However, the current implementation identifies resources solely based on whether the URI
//! ends with `.rnc`. Therefore, Compact Syntax schemas whose URIs do not end with `.rnc` will
//! not be parsed correctly. The same applies to Full Syntax schemas whose URIs end with `.rnc`.  \
//! As a workaround, it is possible to handle this by converting the resource using a custom
//! [`EntityResolver`](crate::sax::EntityResolver) beforehand.
//! In the future, it is planned to provide reliable resource type identification by allowing
//! the media type to be set on [`InputSource`](crate::sax::InputSource).
//!
//! ## Example
//! ```rust
//! use anyxml::{
//!     relaxng::RelaxNGSchema,
//!     sax::{DefaultSAXHandler, XMLReader},
//! };
//!
//! // conversion of the example of RELAX NG Tutorial 1. Getting started
//! // https://relaxng.org/tutorial-20011203.html
//! const SCHEMA: &str = r#"element addressBook {
//!     element card {
//!         element name  { text },
//!         element email { text }
//!     }*
//! }"#;
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
//! let mut schema = RelaxNGSchema::parse_compact_str(SCHEMA, None, Some(DefaultSAXHandler)).unwrap();
//! let validator = schema.new_validate_handler(DefaultSAXHandler);
//! let mut reader = XMLReader::builder().set_handler(validator).build();
//! reader.parse_str(DOCUMENT, None).unwrap();
//! // `last_error` holds the last validation error.
//! assert!(reader.handler.last_error.is_ok());
//! ```
//!
//! # XSD types support
//! While the RELAX NG specification itself defines only two built-in data types
//! —`string` and `token`—this is often insufficient in practical use.
//!
//! This crate provides additional support for data types defined in
//! [XML Schema Part 2: Datatypes Second Edition](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/),
//! in accordance with the [Guidelines for using W3C XML Schema Datatypes with RELAX NG](https://relaxng.org/xsd-20010907.html).
//!
//! To enable support for XSD types, schema authors must specify `"http://www.w3.org/2001/XMLSchema-datatypes"`
//! as the data type library, as indicated in the guidelines above.
//!
//! # Reference
//! - [ISO/IEC 19757-2:2008 Information technology — Document Schema Definition Language (DSDL)Part 2: Regular-grammar-based validation — RELAX NG](https://www.iso.org/standard/52348.html)
//! - [RELAX NG Compact Syntax Committee Specification 21 November 2002](https://relaxng.org/compact-20021121.html)
//! - [Guidelines for using W3C XML Schema Datatypes with RELAX NG](https://relaxng.org/xsd-20010907.html)

mod compact;
mod datatype_library;
mod grammar;
mod parse;
mod validate;

use std::io::Read;

use crate::{
    error::XMLError,
    relaxng::{grammar::Grammar, parse::RelaxNGParseHandler},
    sax::{SAXHandler, XMLReader},
    uri::URIStr,
};

pub use compact::RncParseError;
pub use validate::ValidateHandler;

/// RELAX NG namespace
pub const XML_RELAX_NG_NAMESPACE: &str = "http://relaxng.org/ns/structure/1.0";
/// RELAX NG annotation namespace
pub const XML_RELAX_NG_ANNOTATION_NAMESPACE: &str =
    "http://relaxng.org/ns/compatibility/annotations/1.0";

/// Parsed RELAX NG schema.
pub struct RelaxNGSchema {
    grammar: Grammar,
}

impl RelaxNGSchema {
    /// Parse RELAX NG schema using `uri` and `encoding`.
    ///
    /// If a custom [`EntityResolver`](crate::sax::EntityResolver) or [`ErrorHandler`](crate::sax::ErrorHandler)
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
            let mut parser = XMLReader::builder()
                .set_handler(RelaxNGParseHandler::with_handler(handler))
                .build();
            parser.parse_uri(uri, encoding)?;
            let grammar = parser.handler.simplification().map_err(|err| err.error)?;
            Ok(Self { grammar })
        } else {
            let mut parser = XMLReader::builder()
                .set_handler(RelaxNGParseHandler::default())
                .build();
            parser.parse_uri(uri, encoding)?;
            let grammar = parser.handler.simplification().map_err(|err| err.error)?;
            Ok(Self { grammar })
        }
    }

    /// Parse RELAX NG schema using `reader`, `encoding` and `uri`.
    ///
    /// If a custom [`EntityResolver`](crate::sax::EntityResolver) or [`ErrorHandler`](crate::sax::ErrorHandler)
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
            let mut parser = XMLReader::builder()
                .set_handler(RelaxNGParseHandler::with_handler(handler))
                .build();
            parser.parse_reader(reader, encoding, uri)?;
            let grammar = parser.handler.simplification().map_err(|err| err.error)?;
            Ok(Self { grammar })
        } else {
            let mut parser = XMLReader::builder()
                .set_handler(RelaxNGParseHandler::default())
                .build();
            parser.parse_reader(reader, encoding, uri)?;
            let grammar = parser.handler.simplification().map_err(|err| err.error)?;
            Ok(Self { grammar })
        }
    }

    /// Parse RELAX NG schema using `schema` and `uri`.
    ///
    /// If a custom [`EntityResolver`](crate::sax::EntityResolver) or [`ErrorHandler`](crate::sax::ErrorHandler)
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
            let mut parser = XMLReader::builder()
                .set_handler(RelaxNGParseHandler::with_handler(handler))
                .build();
            parser.parse_str(schema, uri)?;
            let grammar = parser.handler.simplification().map_err(|err| err.error)?;
            Ok(Self { grammar })
        } else {
            let mut parser = XMLReader::builder()
                .set_handler(RelaxNGParseHandler::default())
                .build();
            parser.parse_str(schema, uri)?;
            let grammar = parser.handler.simplification().map_err(|err| err.error)?;
            Ok(Self { grammar })
        }
    }

    /// Generate SAX handler for RELAX NG schema validation.
    pub fn new_validate_handler<H: SAXHandler>(&mut self, handler: H) -> ValidateHandler<'_, H> {
        self.grammar.new_validate_handler(handler)
    }
}
