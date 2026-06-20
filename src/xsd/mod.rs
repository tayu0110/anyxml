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

use crate::{
    error::XMLError,
    tree::Document,
    uri::URIStr,
    xsd::{
        composition::{CompositionState, process_schema_composition},
        facet_resolution::{
            resolve_content_model_list_item_facets, resolve_inline_list_item_facets,
        },
        parser::{
            parse_attribute_group_def, parse_complex_type, parse_element_decl,
            parse_model_group_def, parse_simple_type, resolve_type_name,
        },
        types::{
            AttributeDecl, AttributeGroupDef, BuiltInType, ElementDecl, ModelGroupDef, TypeDef,
            TypeRef,
        },
    },
};

pub const XML_SCHEMA_NAMESPACE: &str = "http://www.w3.org/2001/XMLSchema";
pub const XML_SCHEMA_INSTANCE_NAMESPACE: &str = "http://www.w3.org/2001/XMLSchema-instance";

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

impl XSDSchema {
    /// Set whether length/minLength/maxLength facets on QName and NOTATION types
    /// are enforced. Default is `true` (enforce). Set to `false` to ignore them,
    /// which matches the NIST test suite interpretation of W3C Bug #4009.
    pub fn set_enforce_qname_length_facets(&mut self, enforce: bool) {
        self.enforce_qname_length_facets = enforce;
    }

    /// Build a validator from a parsed XSD schema document, with a base path
    /// for resolving `schemaLocation` attributes in `xs:include` and `xs:redefine`.
    ///
    /// # Passes
    ///
    /// 1. **Pass 0** — schema composition: `xs:include` / `xs:redefine` / `xs:import`
    /// 2. **Pass 0.5** — global attribute declarations (needed before attributeGroup parsing)
    /// 3. **Pass 1** — attribute-group and model-group definitions
    /// 4. **Pass 2** — top-level elements, complex types, simple types, remaining attributes
    /// 5. **Substitution-group construction** — direct + transitive membership map
    /// 6. **List-type resolution** — three sub-passes to propagate `is_list`, `item_type`,
    ///    and `item_facets` through the type graph and into inline declarations
    pub fn from_schema(schema_doc: &Document) -> Result<Self, XMLError> {
        // Public entry creates fresh composition state (visited paths +
        // depth counter) and delegates to the internal variant. The
        // state is what lets `xs:include` / `xs:import` chains detect
        // cycles and enforce a nesting cap.
        let base_uri = schema_doc.document_base_uri();
        let mut state = CompositionState::new(&base_uri);
        Self::from_schema_with_composition_state(schema_doc, &base_uri, &mut state)
    }

    /// Internal entry used by `from_schema_with_base_path` and by
    /// recursive composition. Threads the `CompositionState` so visited
    /// paths and depth survive across nested `xs:include` / `xs:import`
    /// calls.
    fn from_schema_with_composition_state(
        schema_doc: &Document,
        base_uri: &URIStr,
        state: &mut CompositionState,
    ) -> Result<Self, XMLError> {
        let mut validator = XSDSchema {
            elements: HashMap::new(),
            types: HashMap::new(),
            global_attributes: HashMap::new(),
            attribute_groups: HashMap::new(),
            model_groups: HashMap::new(),
            target_namespace: None,
            block_default_extension: false,
            block_default_restriction: false,
            enforce_qname_length_facets: true,
            substitution_groups: HashMap::new(),
        };

        let schema_elem = schema_doc.document_element().ok_or_else(|| {
            XMLError::XSDError(Box::new(XSDValidationError {
                message: "Schema document has no root element".into(),
                line: None,
                column: None,
            }))
        })?;

        // Get target namespace and elementFormDefault
        validator.target_namespace = schema_elem.get_attribute("targetNamespace", None);
        let element_form_qualified = schema_elem
            .get_attribute("elementFormDefault", None)
            .as_deref()
            == Some("qualified");
        // Parse blockDefault
        if let Some(block_default) = schema_elem.get_attribute("blockDefault", None) {
            for token in block_default.split_whitespace() {
                match token {
                    "extension" => validator.block_default_extension = true,
                    "restriction" => validator.block_default_restriction = true,
                    "#all" => {
                        validator.block_default_extension = true;
                        validator.block_default_restriction = true;
                    }
                    _ => {}
                }
            }
        }

        // Determine the effective namespace for local element declarations:
        // If elementFormDefault="qualified", local elements inherit the target namespace.
        let local_elem_ns = if element_form_qualified {
            validator.target_namespace.clone()
        } else {
            None
        };

        // Pass 0: Process xs:include and xs:redefine to merge external schema declarations
        process_schema_composition(schema_elem.clone(), &mut validator, base_uri, state)?;

        // Pass 0.5: Parse global attribute declarations first, since attributeGroup
        // definitions may reference them via <attribute ref="..."/>.
        let mut children = schema_elem.first_element_child();
        while let Some(elem) = children {
            children = elem.next_element_sibling();
            let is_xs = elem.namespace_name().as_deref() == Some(XML_SCHEMA_NAMESPACE);
            if !is_xs {
                continue;
            }
            if elem.local_name().as_ref() == "attribute"
                && let Some(name) = elem.get_attribute("name", None)
            {
                let type_ref = if let Some(type_attr) = elem.get_attribute("type", None) {
                    resolve_type_name(type_attr.as_str(), &validator.target_namespace)
                } else {
                    // Check for inline simpleType child
                    let mut inline_type = None;
                    let mut gchildren = elem.first_element_child();
                    while let Some(ge) = gchildren {
                        gchildren = ge.next_element_sibling();
                        if ge.local_name().as_ref() == "simpleType"
                            && let Ok(td) = parse_simple_type(ge)
                        {
                            inline_type = Some(TypeRef::Inline(Box::new(td)));
                        }
                    }
                    inline_type.unwrap_or(TypeRef::BuiltIn(BuiltInType::String))
                };
                let required = elem.get_attribute("use", None).as_deref() == Some("required");
                let default = elem.get_attribute("default", None);
                let decl = AttributeDecl {
                    name: name.to_string(),
                    type_ref,
                    required,
                    default,
                    prohibited: false,
                };
                let key = (validator.target_namespace.clone(), name.to_string());
                validator.global_attributes.insert(key, decl);
            }
        }

        // Pass 1: Parse attribute group and model group definitions
        // (both needed by complexType parsing in Pass 2)
        let mut children = schema_elem.first_element_child();
        while let Some(elem) = children {
            children = elem.next_element_sibling();

            let is_xs = elem.namespace_name().as_deref() == Some(XML_SCHEMA_NAMESPACE);
            if !is_xs {
                continue;
            }
            if elem.local_name().as_ref() == "attributeGroup"
                && let Some(name) = elem.get_attribute("name", None)
            {
                let ag_def = parse_attribute_group_def(
                    elem.clone(),
                    &validator.target_namespace,
                    &validator.global_attributes,
                    &validator.attribute_groups,
                )?;
                let key = (validator.target_namespace.clone(), name.to_string());
                validator.attribute_groups.insert(key, ag_def);
            }
            if elem.local_name().as_ref() == "group"
                && let Some(name) = elem.get_attribute("name", None)
            {
                let mg_def = parse_model_group_def(
                    elem,
                    &local_elem_ns,
                    &validator.target_namespace,
                    &validator.attribute_groups,
                    &validator.model_groups,
                    validator.block_default_extension,
                    validator.block_default_restriction,
                )?;
                let key = (validator.target_namespace.clone(), name.to_string());
                validator.model_groups.insert(key, mg_def);
            }
        }

        // Pass 2: Process all other top-level children
        let mut children = schema_elem.first_element_child();
        while let Some(elem) = children {
            children = elem.next_element_sibling();

            let local = elem.local_name();
            let is_xs = elem.namespace_name().as_deref() == Some(XML_SCHEMA_NAMESPACE)
                || elem.prefix().as_deref() == Some("xs")
                || elem.prefix().as_deref() == Some("xsd");

            if !is_xs {
                continue;
            }

            match local.as_ref() {
                "element" => {
                    let decl = parse_element_decl(
                        elem,
                        &validator.target_namespace,
                        &local_elem_ns,
                        &validator.target_namespace,
                        &validator.attribute_groups,
                        &validator.model_groups,
                        validator.block_default_extension,
                        validator.block_default_restriction,
                    )?;
                    let key = (validator.target_namespace.clone(), decl.name.clone());
                    validator.elements.insert(key, decl);
                }
                "complexType" => {
                    let type_def = parse_complex_type(
                        elem,
                        &local_elem_ns,
                        &validator.target_namespace,
                        &validator.target_namespace,
                        &validator.attribute_groups,
                        &validator.model_groups,
                        validator.block_default_extension,
                        validator.block_default_restriction,
                    )?;
                    if let TypeDef::Complex(ref ct) = type_def
                        && let Some(name) = &ct.name
                    {
                        let key = (validator.target_namespace.clone(), name.clone());
                        validator.types.insert(key, type_def);
                    }
                }
                "simpleType" => {
                    let type_def = parse_simple_type(elem)?;
                    if let TypeDef::Simple(ref st) = type_def
                        && let Some(name) = &st.name
                    {
                        let key = (validator.target_namespace.clone(), name.clone());
                        validator.types.insert(key, type_def);
                    }
                }
                "attribute" => {
                    // Parse global attribute declarations
                    if let Some(name) = elem.get_attribute("name", None) {
                        let type_ref = if let Some(type_attr) = elem.get_attribute("type", None) {
                            resolve_type_name(type_attr.as_str(), &validator.target_namespace)
                        } else {
                            // Check for inline simpleType child
                            let mut inline_type = None;
                            let mut gchildren = elem.first_element_child();
                            while let Some(ge) = gchildren {
                                gchildren = ge.next_element_sibling();
                                if ge.local_name().as_ref() == "simpleType"
                                    && let Ok(td) = parse_simple_type(ge)
                                {
                                    inline_type = Some(TypeRef::Inline(Box::new(td)));
                                }
                            }
                            inline_type.unwrap_or(TypeRef::BuiltIn(BuiltInType::String))
                        };
                        let required =
                            elem.get_attribute("use", None).as_deref() == Some("required");
                        let default = elem.get_attribute("default", None);
                        let decl = AttributeDecl {
                            name: name.to_string(),
                            type_ref,
                            required,
                            default,
                            prohibited: false,
                        };
                        let key = (validator.target_namespace.clone(), name.to_string());
                        validator.global_attributes.insert(key, decl);
                    }
                }
                _ => {
                    // Ignore other top-level declarations for now
                }
            }
        }

        // Build substitution group map from element declarations.
        // First, collect direct memberships: member -> head.
        let mut direct_head: HashMap<(Option<String>, String), (Option<String>, String)> =
            HashMap::new();
        for (key, decl) in &validator.elements {
            if let Some(ref sg_head) = decl.substitution_group {
                direct_head.insert(key.clone(), sg_head.clone());
            }
        }
        // Build transitive map: for each element that is a substitution group head,
        // collect all (direct and transitive) members.
        // An element E is a member of head H if:
        //   - E.substitutionGroup == H (direct), or
        //   - E.substitutionGroup == M where M is a member of H (transitive)
        for member_key in direct_head.keys() {
            // Walk up the chain from member to find all heads
            let mut current = member_key.clone();
            let mut chain = vec![member_key.clone()];
            while let Some(head) = direct_head.get(&current) {
                // Add member_key as a member of head
                validator
                    .substitution_groups
                    .entry(head.clone())
                    .or_default()
                    .push(member_key.clone());
                current = head.clone();
                // Prevent infinite loops
                if chain.contains(&current) {
                    break;
                }
                chain.push(current.clone());
            }
        }
        // Deduplicate members
        for members in validator.substitution_groups.values_mut() {
            members.sort_by(|a, b| a.1.cmp(&b.1).then_with(|| a.0.cmp(&b.0)));
            members.dedup();
        }

        // Resolution pass: propagate list type info from base types to derived types.
        // Types that restrict a list type inherit is_list and item_type.
        let type_keys = validator.types.keys().cloned().collect::<Vec<_>>();
        for key in &type_keys {
            let base_local = {
                if let Some(TypeDef::Simple(st)) = validator.types.get(key) {
                    st._base_type_local.clone()
                } else {
                    None
                }
            };
            if let Some(base_name) = base_local {
                // Look up the base type in the same namespace
                let base_key = (key.0.clone(), base_name);
                let (is_list, item_type) = {
                    if let Some(TypeDef::Simple(base_st)) = validator.types.get(&base_key) {
                        (base_st.is_list, base_st.item_type.clone())
                    } else {
                        (false, None)
                    }
                };
                if is_list && let Some(TypeDef::Simple(st)) = validator.types.get_mut(key) {
                    st.is_list = true;
                    if st.item_type.is_none() {
                        st.item_type = item_type;
                    }
                }
            }
        }

        // Resolution pass 2: resolve item type facets for list types whose item type
        // is a user-defined simple type (not a built-in).
        let type_keys2 = validator.types.keys().cloned().collect::<Vec<_>>();
        for key in &type_keys2 {
            let item_local = {
                if let Some(TypeDef::Simple(st)) = validator.types.get(key) {
                    st._item_type_local.clone()
                } else {
                    None
                }
            };
            if let Some(item_name) = item_local {
                // Look up the item type in the same namespace
                let item_key = (key.0.clone(), item_name);
                let resolved = {
                    if let Some(TypeDef::Simple(item_st)) = validator.types.get(&item_key) {
                        Some((item_st.base.clone(), item_st.facets.clone()))
                    } else {
                        None
                    }
                };
                if let Some((item_base, item_facets)) = resolved
                    && let Some(TypeDef::Simple(st)) = validator.types.get_mut(key)
                {
                    st.item_type = Some(item_base);
                    st.item_facets = item_facets;
                }
            }
        }

        // Resolution pass 3: resolve item type facets for inline list types embedded in
        // element declarations (both global elements and particles inside complex types).
        // Collect resolved item types from the types map first.
        let resolved_items = validator
            .types
            .iter()
            .filter_map(|(k, td)| {
                if let TypeDef::Simple(st) = td {
                    Some((k.clone(), (st.base.clone(), st.facets.clone())))
                } else {
                    None
                }
            })
            .collect::<HashMap<_, _>>();

        // Resolve inline list types in global element declarations
        for elem_decl in validator.elements.values_mut() {
            resolve_inline_list_item_facets(
                &mut elem_decl.type_ref,
                &resolved_items,
                &validator.target_namespace,
            );
        }

        // Resolve inline list types in complex type content models
        let type_keys3 = validator.types.keys().cloned().collect::<Vec<_>>();
        for key in type_keys3 {
            if let Some(TypeDef::Complex(ct)) = validator.types.get_mut(&key) {
                resolve_content_model_list_item_facets(
                    &mut ct.content,
                    &resolved_items,
                    &validator.target_namespace,
                );
            }
        }

        Ok(validator)
    }
}
