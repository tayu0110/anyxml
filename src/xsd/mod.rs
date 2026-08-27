mod parse;
mod typedef;

pub use parse::{Schema, XSDParseError};

pub const XML_SCHEMA_NAMESPACE: &str = "http://www.w3.org/2001/XMLSchema";
pub const XML_SCHEMA_INSTANCE_NAMESPACE: &str = "http://www.w3.org/2001/XMLSchema-instance";

pub struct XSDSchema {}

impl XSDSchema {}
