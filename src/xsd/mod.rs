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

use std::collections::HashMap;

pub use error::XSDValidationError;

use crate::xsd::types::{AttributeDecl, AttributeGroupDef, ElementDecl, ModelGroupDef, TypeDef};

/// The XML Schema namespace URI (`xs:` / `xsd:`).
pub const XML_SCHEMA_NAMESPACE: &str = "http://www.w3.org/2001/XMLSchema";

/// The XML Schema Instance namespace URI (`xsi:`).
pub const XML_SCHEMA_INSTANCE_NAMESPACE: &str = "http://www.w3.org/2001/XMLSchema-instance";

/// Type alias for substitution group maps: head element key -> vec of member element keys.
type SubstitutionGroupMap = HashMap<(Option<String>, String), Vec<(Option<String>, String)>>;

pub struct XSDSchema {
    /// Top-level element declarations: (namespace_name, local_name) -> ElementDecl
    pub(super) elements: HashMap<(Option<String>, String), ElementDecl>,
    /// Named type definitions: (namespace_name, local_name) -> TypeDef
    pub(super) types: HashMap<(Option<String>, String), TypeDef>,
    /// Global attribute declarations: (namespace_name, local_name) -> AttributeDecl
    pub(super) global_attributes: HashMap<(Option<String>, String), AttributeDecl>,
    /// Attribute group definitions: (namespace_name, local_name) -> AttributeGroupDef
    pub(super) attribute_groups: HashMap<(Option<String>, String), AttributeGroupDef>,
    /// Model group definitions: (namespace_name, local_name) -> ModelGroupDef
    pub(super) model_groups: HashMap<(Option<String>, String), ModelGroupDef>,
    /// Target namespace of the schema.
    pub(super) target_namespace: Option<String>,
    /// Schema-level blockDefault for extension.
    pub(super) block_default_extension: bool,
    /// Schema-level blockDefault for restriction.
    pub(super) block_default_restriction: bool,
    /// Whether to enforce length/minLength/maxLength facets on QName and NOTATION types.
    /// NIST tests expect these to be ignored (Bug #4009), MS tests expect enforcement.
    /// Default: true (enforce).
    pub(super) enforce_qname_length_facets: bool,
    /// Substitution group membership: head_key -> vec of member keys (transitive).
    /// Each key is (namespace, local_name).
    pub(super) substitution_groups: SubstitutionGroupMap,
}
