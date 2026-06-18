//! Identity constraint evaluation for XSD validation.
//!
//! Implements xs:key, xs:unique, and xs:keyref identity constraints using
//! a restricted XPath subset for selector and field evaluation. Supports
//! `.//` descendant selectors, composite (multi-field) keys, QName
//! namespace-aware value comparison, and decimal normalization.

use std::collections::HashMap;

use crate::tree::Element;

use super::{
    error::XSDValidationError,
    types::{IdentityConstraint, IdentityConstraintKind, XSDValidator},
};

impl XSDValidator {
    /// Evaluate identity constraints declared on an element.
    /// `context_node` is the element that declares the constraints (the scope).
    pub(super) fn evaluate_identity_constraints(
        &self,
        context_node: Element,
        constraints: &[IdentityConstraint],
        errors: &mut Vec<XSDValidationError>,
    ) {
        // Collect key/unique constraint values by constraint name, so keyrefs can look them up.
        let mut key_tables: HashMap<String, Vec<Vec<String>>> = HashMap::new();

        // First pass: evaluate key and unique constraints
        for constraint in constraints {
            if constraint.kind == IdentityConstraintKind::KeyRef {
                continue; // Process keyrefs in second pass
            }

            let selected = idc_select_nodes(context_node.clone(), &constraint.selector);

            let mut tuples: Vec<Vec<String>> = vec![];

            for sel_node in &selected {
                let mut field_values = vec![];
                let mut field_source_nodes = vec![];
                let mut all_present = true;
                let mut multiplicity_error = false;

                for field_xpath in &constraint.fields {
                    let (value, match_count, source_node) =
                        idc_evaluate_field(sel_node.clone(), field_xpath);
                    // For xs:key (and xs:unique), if a field selects more than one node,
                    // that's an error per XSD spec §3.11.4
                    if match_count > 1 && constraint.kind == IdentityConstraintKind::Key {
                        let elem_name = sel_node.local_name();
                        errors.push(XSDValidationError {
                            message: format!(
                                "Key '{}': field '{}' selects {} nodes for element '{}' (must select at most one)",
                                constraint.name, field_xpath, match_count, elem_name
                            ),
                            line: Some(0),
                            column: Some(0),
                        });
                        multiplicity_error = true;
                        break;
                    }
                    if value.is_none() {
                        all_present = false;
                    }
                    field_values.push(value);
                    field_source_nodes.push(source_node);
                }

                if multiplicity_error {
                    continue;
                }

                if constraint.kind == IdentityConstraintKind::Key {
                    // For xs:key, every field must be present
                    if !all_present {
                        let elem_name = sel_node.local_name();
                        errors.push(XSDValidationError {
                            message: format!(
                                "Key '{}': field value missing for element '{}'",
                                constraint.name, elem_name
                            ),
                            line: Some(0),
                            column: Some(0),
                        });
                        continue;
                    }
                }

                // For xs:unique, skip rows where any field is absent
                if !all_present {
                    continue;
                }

                // Build tuple, normalizing QName values using namespace context
                let tuple: Vec<String> = field_values
                    .into_iter()
                    .enumerate()
                    .map(|(i, v)| {
                        let val = v.unwrap();
                        // Try to normalize as QName if value contains a prefix
                        if let Some(source) = field_source_nodes[i].clone() {
                            idc_normalize_qname(source, &val)
                        } else {
                            val
                        }
                    })
                    .collect();

                // Check for duplicate
                let is_dup = tuples.iter().any(|existing| {
                    existing.len() == tuple.len()
                        && existing
                            .iter()
                            .zip(tuple.iter())
                            .all(|(a, b)| idc_values_equal(a, b))
                });

                if is_dup {
                    let kind_str = match constraint.kind {
                        IdentityConstraintKind::Key => "Key",
                        IdentityConstraintKind::Unique => "Unique",
                        _ => "Constraint",
                    };
                    errors.push(XSDValidationError {
                        message: format!(
                            "{} '{}': duplicate value {:?}",
                            kind_str, constraint.name, tuple
                        ),
                        line: Some(0),
                        column: Some(0),
                    });
                } else {
                    tuples.push(tuple);
                }
            }

            key_tables.insert(constraint.name.clone(), tuples);
        }

        // Second pass: evaluate keyref constraints
        for constraint in constraints {
            if constraint.kind != IdentityConstraintKind::KeyRef {
                continue;
            }

            let refer_name = match &constraint.refer {
                Some(name) => name,
                None => continue,
            };

            let referred_tuples = key_tables.get(refer_name);
            if referred_tuples.is_none() {
                continue;
            }
            let referred_tuples = referred_tuples.unwrap();
            let selected = idc_select_nodes(context_node.clone(), &constraint.selector);

            for sel_node in &selected {
                let mut field_values = vec![];
                let mut field_source_nodes = vec![];
                let mut all_present = true;

                for field_xpath in &constraint.fields {
                    let (value, _match_count, source_node) =
                        idc_evaluate_field(sel_node.clone(), field_xpath);
                    if value.is_none() {
                        all_present = false;
                    }
                    field_values.push(value);
                    field_source_nodes.push(source_node);
                }

                // KeyRef rows with missing fields are skipped
                if !all_present {
                    continue;
                }

                // Build tuple, normalizing QName values using namespace context
                let tuple: Vec<String> = field_values
                    .into_iter()
                    .enumerate()
                    .map(|(i, v)| {
                        let val = v.unwrap();
                        if let Some(source) = field_source_nodes[i].clone() {
                            idc_normalize_qname(source, &val)
                        } else {
                            val
                        }
                    })
                    .collect();

                // Check if tuple exists in the referred key table
                let found = referred_tuples.iter().any(|key_tuple| {
                    key_tuple.len() == tuple.len()
                        && key_tuple
                            .iter()
                            .zip(tuple.iter())
                            .all(|(a, b)| idc_values_equal(a, b))
                });

                if !found {
                    errors.push(XSDValidationError {
                        message: format!(
                            "KeyRef '{}': no matching key value {:?} in referred constraint '{}'",
                            constraint.name, tuple, refer_name
                        ),
                        line: Some(0),
                        column: Some(0),
                    });
                }
            }
        }
    }
}

/// Evaluate a restricted XPath selector expression, returning selected nodes.
///
///   selector ::= path ('|' path)*
///   path     ::= ('.//') ? step ('/' step)*
///   step     ::= '.' | nametest
///   nametest ::= qname | '*'
fn idc_select_nodes(context: Element, selector: &str) -> Vec<Element> {
    let mut results: Vec<Element> = vec![];

    // Split on '|' for union
    for path_str in selector.split('|') {
        let path = path_str.trim();
        if path.is_empty() {
            continue;
        }

        let (descendant, steps) = idc_parse_path(path);

        if descendant {
            // .// prefix: select from all descendants
            let mut descendants = Vec::new();
            idc_collect_descendants(context.clone(), &mut descendants);
            for desc in descendants {
                if idc_match_steps(context.clone(), desc.clone(), &steps, 0)
                    && results.iter().all(|r| !r.is_same_node(&desc))
                {
                    results.push(desc);
                }
            }
        } else {
            // No .// prefix: select from direct path starting at context
            let mut candidates = vec![context.clone()];
            for (i, step) in steps.iter().enumerate() {
                let mut next_candidates = Vec::new();
                for cand in &candidates {
                    let mut children = cand.first_child();
                    while let Some(child) = children {
                        if let Some(child) = child.as_element()
                            && idc_step_matches(child.clone(), step)
                        {
                            if i == steps.len() - 1 {
                                if results.iter().all(|r| !r.is_same_node(&child)) {
                                    results.push(child);
                                }
                            } else {
                                next_candidates.push(child);
                            }
                        }
                        children = child.next_sibling();
                    }
                }
                candidates = next_candidates;
                if i == steps.len() - 1 {
                    break;
                }
            }
        }
    }

    results
}

/// Parse a selector path into (is_descendant, steps).
/// A step is either "*" (wildcard), "." (self), or a potentially namespace-prefixed name.
fn idc_parse_path(path: &str) -> (bool, Vec<String>) {
    let mut s = path.trim();
    let descendant = if s.starts_with(".//") {
        s = &s[3..];
        true
    } else if s.starts_with("./") {
        s = &s[2..];
        false
    } else {
        false
    };

    let steps: Vec<String> = s.split('/').map(|st| st.trim().to_string()).collect();
    (descendant, steps)
}

/// Check if a node matches a single step (name test or wildcard).
fn idc_step_matches(node: Element, step: &str) -> bool {
    if step == "*" {
        // Wildcard matches any element
        return true;
    }
    if step == "." {
        return true; // Self
    }

    // Check name match. The step might have a namespace prefix.
    if let Some(colon) = step.find(':') {
        let _prefix = &step[..colon];
        let local = &step[colon + 1..];
        // For namespace-qualified steps, compare local name and check that
        // the element has a namespace matching the prefix's namespace.
        // In XSD identity constraints, the prefix is resolved from the schema document's
        // namespace bindings, which typically match the instance document's target namespace.
        node.local_name().as_ref() == local
    } else {
        // Unprefixed: match local name, typically for elements in a namespace
        // (the schema's target namespace)
        node.local_name().as_ref() == step
    }
}

/// Collect all descendant element nodes.
fn idc_collect_descendants(node: Element, result: &mut Vec<Element>) {
    let mut children = node.first_child();
    while let Some(child) = children {
        if let Some(elem) = child.as_element() {
            result.push(elem.clone());
            idc_collect_descendants(elem, result);
        }
        children = child.next_sibling();
    }
}

/// Check if a descendant node matches the step path from the context.
/// For `.//` selectors, the steps represent a suffix path that can match at any depth.
/// For example, `.//v:vehicle` (1 step) matches any descendant element named `vehicle`,
/// and `.//v:state/v:vehicle` (2 steps) matches a `vehicle` whose parent is a `state`.
/// The key insight: the LAST N nodes in the path from context to target must match
/// the N steps, allowing arbitrary depth for the `.//` descendant axis.
fn idc_match_steps(context: Element, target: Element, steps: &[String], _step_idx: usize) -> bool {
    if steps.is_empty() {
        return false;
    }

    // Build the path from context to target by walking up from target
    let mut path_to_target = Vec::new();
    let mut current = target;
    while !current.is_same_node(&context) {
        path_to_target.push(current.clone());
        match current.parent_node() {
            Some(parent) => current = parent.as_element().unwrap(),
            None => return false, // target is not a descendant of context
        }
    }
    path_to_target.reverse(); // Now: [first child, ..., target]

    // The path must be at least as long as the steps (descendant can be deeper)
    if path_to_target.len() < steps.len() {
        return false;
    }

    // Match the LAST N nodes in the path against the N steps.
    // This allows `.//v:vehicle` to match at any depth, and
    // `.//v:state/v:vehicle` to match a vehicle whose immediate parent is a state.
    let offset = path_to_target.len() - steps.len();
    for (i, step) in steps.iter().enumerate() {
        if !idc_step_matches(path_to_target[offset + i].clone(), step) {
            return false;
        }
    }

    true
}

/// Evaluate a field XPath on a selected node, returning the field value (if present),
/// the count of matching nodes (for multiplicity checking), and the NodeId of the
/// node where the value was extracted from (for namespace resolution on QName types).
///
/// Field syntax:
///   '.' -> text content of the element
///   '@attr' -> attribute value
///   'child' or 'prefix:child' -> text content of the first matching child element
///   'child1/child2/...' -> nested child path, text of the leaf
///
/// Returns (value, match_count, source_node) where match_count is the number of nodes
/// that matched the field path. For xs:key, match_count > 1 is an error.
fn idc_evaluate_field(node: Element, field: &str) -> (Option<String>, usize, Option<Element>) {
    let field = field.trim();

    if field == "." {
        // Text content of the element itself — always exactly 1 match (the element itself)
        let text = node.text_content();
        let trimmed = text.trim();
        if trimmed.is_empty() {
            return (None, 1, Some(node));
        }
        return (Some(trimmed.to_string()), 1, Some(node));
    }

    if let Some(attr_name) = field.strip_prefix('@') {
        // Attribute
        // Handle pipe-separated (union) attribute fields like "@id|@id|..."
        // Take just the first one since they're all the same
        let attr_name = if let Some(pipe) = attr_name.find('|') {
            &attr_name[..pipe]
        } else {
            attr_name
        };
        let mut count = 0;
        let mut value = None;
        for attr in node.attributes() {
            if attr.local_name().as_ref() == attr_name {
                count += 1;
                if value.is_none() {
                    value = Some(attr.value());
                }
            }
        }
        if count > 0 {
            return (value, count, Some(node));
        }
        return (None, 0, Some(node));
    }

    // Child path: split on '/' and navigate
    let parts: Vec<&str> = field.split('/').collect();
    let mut current_nodes = vec![node];

    for part in &parts {
        let part = part.trim();
        let mut next_nodes = vec![];

        for cn in &current_nodes {
            let mut children = cn.first_child();
            while let Some(child) = children {
                if let Some(child) = child.as_element()
                    && idc_step_matches(child.clone(), part)
                {
                    next_nodes.push(child.clone());
                }
                children = child.next_sibling();
            }
        }

        current_nodes = next_nodes;
        if current_nodes.is_empty() {
            return (None, 0, None);
        }
    }

    let match_count = current_nodes.len();

    // Return text content of the first matching node
    if let Some(result_node) = current_nodes.first() {
        let text = result_node.text_content();
        let trimmed = text.trim();
        if trimmed.is_empty() {
            // For key constraint: check if element exists even if empty
            // An empty element is still a "present" field value (empty string)
            return (Some(String::new()), match_count, Some(result_node.clone()));
        }
        (
            Some(trimmed.to_string()),
            match_count,
            Some(result_node.clone()),
        )
    } else {
        (None, 0, None)
    }
}

/// Normalize a value that might be a QName by resolving its namespace prefix.
/// If the value contains a `:` that looks like a namespace prefix, resolve the
/// prefix to a namespace URI using the in-scope namespace declarations on the
/// source element (walking up the DOM tree). Returns `{namespace_uri}local_name`
/// for prefixed QNames, or the original value if no prefix or prefix can't be resolved.
fn idc_normalize_qname(source_node: Element, value: &str) -> String {
    let value = value.trim();
    if let Some(colon) = value.find(':') {
        let prefix = &value[..colon];
        let local = &value[colon + 1..];

        // Don't treat things with empty prefix or local as QNames
        if prefix.is_empty() || local.is_empty() {
            return value.to_string();
        }

        // Resolve the namespace prefix by walking up the DOM tree
        if let Some(ns_uri) = source_node.search_namespace_by_prefix(Some(prefix)) {
            return format!("{{{}}}{}", ns_uri.namespace_name(), local);
        }
    }
    value.to_string()
}

/// Compare two identity constraint field values for equality.
/// This needs to be type-aware for xs:decimal and xs:QName, but for the
/// restricted XPath subset used in identity constraints, we primarily compare strings.
/// We also normalize decimal values when both look like numbers.
fn idc_values_equal(a: &str, b: &str) -> bool {
    if a == b {
        return true;
    }

    // Try decimal comparison: if both parse as numbers, compare numerically
    if let (Some(da), Some(db)) = (idc_parse_decimal(a), idc_parse_decimal(b)) {
        return da == db;
    }

    false
}

/// Parse a decimal string into a normalized form for comparison.
/// Returns None if the string is not a valid decimal number.
fn idc_parse_decimal(s: &str) -> Option<String> {
    let s = s.trim();
    if s.is_empty() {
        return None;
    }

    // Check if it looks like a decimal number
    let mut chars = s.chars().peekable();
    let negative = if chars.peek() == Some(&'-') {
        chars.next();
        true
    } else if chars.peek() == Some(&'+') {
        chars.next();
        false
    } else {
        false
    };

    let remaining: String = chars.collect();
    if remaining.is_empty() {
        return None;
    }

    // Split on decimal point
    let (int_part, frac_part) = if let Some(dot_pos) = remaining.find('.') {
        (&remaining[..dot_pos], &remaining[dot_pos + 1..])
    } else {
        (remaining.as_str(), "")
    };

    // Validate: all digits
    if !int_part.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }
    if !frac_part.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }

    // Normalize: strip leading zeros from integer part, strip trailing zeros from fraction
    let int_normalized = int_part.trim_start_matches('0');
    let int_normalized = if int_normalized.is_empty() {
        "0"
    } else {
        int_normalized
    };

    let frac_normalized = frac_part.trim_end_matches('0');

    // Check for zero
    if int_normalized == "0" && frac_normalized.is_empty() {
        return Some("0".to_string()); // Normalize +0 and -0 to "0"
    }

    let sign = if negative { "-" } else { "" };
    if frac_normalized.is_empty() {
        Some(format!("{}{}", sign, int_normalized))
    } else {
        Some(format!("{}{}.{}", sign, int_normalized, frac_normalized))
    }
}
