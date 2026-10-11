//! APIs for parsing XML Schema 1.0 document and validate XML document using parsed schema.
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
