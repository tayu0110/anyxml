//! XML Schema (XSD) 1.1 validation.
//!
//! This module implements validation of XML documents against XSD schemas.
//! It covers:
//!
//! - **Part 1 (Structures)**: Complex types, simple types, elements, attributes,
//!   sequences, choices, all groups, minOccurs/maxOccurs, mixed content.
//! - **Part 2 (Datatypes)**: Built-in primitive types (string, boolean, decimal,
//!   float, double, integer, date, dateTime, etc.) and facet-based restrictions
//!   (minLength, maxLength, pattern, enumeration, minInclusive, maxInclusive, etc.).
//!
//! # Usage
//!
//! ```
//! use uppsala::{parse, xsd::XsdValidator};
//!
//! let schema_xml = r#"
//! <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
//!   <xs:element name="root" type="xs:string"/>
//! </xs:schema>
//! "#;
//!
//! let doc_xml = "<root>hello</root>";
//!
//! let schema = parse(schema_xml).unwrap();
//! let doc = parse(doc_xml).unwrap();
//! let validator = XsdValidator::from_schema(&schema).unwrap();
//! let errors = validator.validate(&doc);
//! assert!(errors.is_empty());
//! ```

mod builder;
mod builtins;
mod composition;
mod datetime;
mod decimal;
mod error;
mod facet_resolution;
mod identity;
mod parser;
#[cfg(test)]
mod tests;
pub(crate) mod types;
mod validation;
mod wildcard;

pub use error::XSDValidationError;
/// The primary public type: construct via `XsdValidator::from_schema()` and call
/// `validator.validate(&doc)` to obtain a list of validation errors.
pub use types::XsdValidator;

/// The XML Schema namespace URI (`xs:` / `xsd:`).
pub const XS_NAMESPACE: &str = "http://www.w3.org/2001/XMLSchema";

/// The XML Schema Instance namespace URI (`xsi:`).
pub const XSI_NAMESPACE: &str = "http://www.w3.org/2001/XMLSchema-instance";
