//! APIs for parsing XML Schema 1.0 document and validate XML document using parsed schema.
//!
//! # Limitations
//!
//! ## Handling missing subcomponents
//! This implementation of the crate does not support lazy loading of components, as
//! suggested in Chapters 4 and 5 of XSD Part 1. In other words, it requires that the
//! schema document (and any documents it includes or imports) represent a complete schema.  \
//! According to Chapters 4 and 5 of XSD Part 1, it is suggested that schemas can be
//! constructed by lazily loading components. If such a mechanism is allowed, one must
//! consider the possibility that a schema document may not contain the information items
//! representing components referenced by QNames within that document. Consequently, an
//! implementation would be required to tolerate the absence of components that are
//! considered mandatory under the schema specification.  \
//! However, allowing mandatory components to be missing due to the lazy-loading mechanism
//! makes it considerably more difficult to check schema component constraints.  \
//! To avoid such a complex implementation, this crate requires that all QName references
//! be correctly resolved during schema construction.
//!
//! ## Regular expression syntax
//! The regular expressions allowed by this crate’s XSD implementation conform to XSD Part 2,
//! Appendix F. Therefore, strictly speaking, this is not a limitation.  \
//! Compared to widely used regular expression engines such as PCRE, this regular expression
//! syntax is simple and has limited expressiveness.  \
//! For example, since regular expressions in XSD are used to check patterns across the entire
//! string, anchors such as '^' and '$' are meaningless, and in fact, such syntax is not supported.  \
//! Furthermore, backreferences, lookahead and lookbehind, internal setting options, and
//! certain escape sequences are also not supported. Since backreferences are not supported,
//! capturing is meaningless, and such syntax is not supported either.  \
//! Please refer to the specification for detailed features.
//!
//! # Reference
//! - [XML Schema Part 0: Primer Second Edition W3C Recommendation 28 October 2004](https://www.w3.org/TR/xmlschema-0/)
//! - [XML Schema Part 1: Structures Second Edition W3C Recommendation 28 October 2004](https://www.w3.org/TR/xmlschema-1/)
//! - [XML Schema Part 2: Datatypes Second Edition W3C Recommendation 28 October 2004](https://www.w3.org/TR/xmlschema-2/)
mod parse;
mod typedef;
mod validate;

pub use parse::{Schema, XSDError};

use crate::{sax::SAXHandler, xsd::validate::ValidateHandler};

pub const XML_SCHEMA_NAMESPACE: &str = "http://www.w3.org/2001/XMLSchema";
pub const XML_SCHEMA_INSTANCE_NAMESPACE: &str = "http://www.w3.org/2001/XMLSchema-instance";

#[derive(Default)]
pub struct XSDSchema {
    schema: Schema,
}

impl XSDSchema {
    pub fn new_validate_handler<H: SAXHandler>(&mut self, child: H) -> ValidateHandler<'_, H> {
        ValidateHandler::new(&mut self.schema, child)
    }

    pub fn extend(&mut self, other: Self) {
        self.schema.extend(other.schema);
    }
}
