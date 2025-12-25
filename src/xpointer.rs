//! Provide XPointer processing APIs.
//!
//! This crate currently supports only the "Shorthand Pointer" defined in the XPointer Framework
//! and the two schemes `element` and `xmlns`.  \
//! The `xpointer` scheme may be supported in the future,
//! but it is a specification that has not yet reached Recommendation status.
//! Even if implemented, interoperability is unlikely to be guaranteed.
//!
//! # Reference
//! - [XPointer Framework](https://www.w3.org/TR/xptr-framework/)
//! - [XPointer element() Scheme](https://www.w3.org/TR/xptr-element/)
//! - [XPointer xmlns() Scheme](https://www.w3.org/TR/xptr-xmlns/)

use std::{collections::HashMap, num::IntErrorKind};

use crate::{
    XML_XML_NAMESPACE, XMLVersion,
    tree::{Document, DocumentFragment, Node, node::NodeSpec},
};

const VERSION: XMLVersion = XMLVersion::XML10;

pub struct XPointerResolver {
    parts: Vec<XPointerPart>,
}

impl XPointerResolver {
    /// Apply an XPointer to the `document` to resolve the sub-resource.
    ///
    /// If a sub-resource is successfully resolved, return `Some`, otherwise return `None`.
    pub fn resolve(&self, document: Document) -> Option<Node<dyn NodeSpec>> {
        let mut namespace_context = HashMap::from([("xml", XML_XML_NAMESPACE)]);
        for part in &self.parts {
            match part {
                XPointerPart::Shorthand(id) => {
                    if let Some(element) = document.get_element_by_id(id) {
                        return Some(element.into());
                    }
                }
                XPointerPart::Element { id, sequence } => {
                    let mut top = if let Some(id) = id.as_deref() {
                        document
                            .get_element_by_id(id)
                            .map(Node::<dyn NodeSpec>::from)
                    } else {
                        Some(Node::<dyn NodeSpec>::from(document.clone()))
                    };

                    for &(mut seq) in sequence {
                        let mut children = top.and_then(|top| top.first_element_child());
                        while seq > 1 && children.is_some() {
                            children = children.and_then(|ch| ch.next_element_sibling());
                            seq -= 1;
                        }
                        top = children.map(From::from);
                        if top.is_none() {
                            break;
                        }
                    }

                    if top.is_some() {
                        return top;
                    }
                }
                XPointerPart::XMLNs {
                    prefix,
                    namespace_name,
                } => {
                    namespace_context.insert(prefix, namespace_name);
                }
            }
        }

        None
    }

    /// Apply an XPointer to the `fragment` to resolve the sub-resource.
    ///
    /// This method is useful when applying an XPointer to a resource
    /// that has multiple element or text children of root node, such as
    /// an external general parsed entity.
    ///
    /// If a sub-resource is successfully resolved, return `Some`, otherwise return `None`.
    pub fn resolve_external_parsed_entity(
        &self,
        fragment: DocumentFragment,
    ) -> Option<Node<dyn NodeSpec>> {
        let mut elements = vec![];
        let mut children = fragment.first_child();
        while let Some(child) = children {
            if let Some(element) = child.as_element() {
                elements.push(element);
            }
            if let Some(first) = child.first_child() {
                children = Some(first);
            } else if let Some(next) = child.next_sibling() {
                children = Some(next);
            } else {
                children = None;
                let mut parent = child.parent_node();
                while let Some(now) = parent {
                    if let Some(next) = now.next_sibling() {
                        children = Some(next);
                        break;
                    }
                    parent = now.parent_node();
                }
            }
        }

        let mut namespace_context = HashMap::from([("xml", XML_XML_NAMESPACE)]);
        for part in &self.parts {
            match part {
                XPointerPart::Shorthand(id) => {
                    for elem in &elements {
                        if elem
                            .attributes()
                            .any(|att| att.is_id() && att.value() == id.as_ref())
                        {
                            return Some(elem.into());
                        }
                    }
                }
                XPointerPart::Element { id, sequence } => {
                    let mut top = if let Some(id) = id.as_deref() {
                        elements
                            .iter()
                            .find(|elem| {
                                elem.attributes()
                                    .any(|att| att.is_id() && att.value() == id)
                            })
                            .map(|elem| Node::<dyn NodeSpec>::from(elem.clone()))
                    } else {
                        Some(Node::<dyn NodeSpec>::from(fragment.clone()))
                    };

                    for &(mut seq) in sequence {
                        let mut children = top.and_then(|top| top.first_element_child());
                        while seq > 1 && children.is_some() {
                            children = children.and_then(|ch| ch.next_element_sibling());
                            seq -= 1;
                        }
                        top = children.map(From::from);
                        if top.is_none() {
                            break;
                        }
                    }

                    if top.is_some() {
                        return top;
                    }
                }
                XPointerPart::XMLNs {
                    prefix,
                    namespace_name,
                } => {
                    namespace_context.insert(prefix, namespace_name);
                }
            }
        }

        None
    }
}

enum XPointerPart {
    Shorthand(Box<str>),
    Element {
        id: Option<Box<str>>,
        sequence: Vec<usize>,
    },
    XMLNs {
        prefix: Box<str>,
        namespace_name: Box<str>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XPointerParseError {
    InvalidNCName,
    InvalidSchemeName,
    InvalidEscapedCharacter,
    InvalidCharacter,
    ParenthesNotFoundAfterSchemeName,
    UnmatchParentheses,
    SpaceFoundAfterAllPointerPart,
    ElementInvalidChildSequence,
    ElementEmptySequenceNumber,
    ElementNegativeSequenceNumber,
    ElementTooLargeSequenceNumber,
    XMLNSEqualMarkNotFound,
}

/// Parse `xpointer` as an XPointer string.
///
/// `xpointer` must not contain the fragment identifier delimiter `"#"`.
/// Furthermore, if `xpointer` is a string derived from a URI or XML,
/// any percent encoding or character references must be decoded.  \
/// However, escapes using the circumflex character (`"^"`) as defined in
/// [section 3.1 of XPointer Framework specification](https://www.w3.org/TR/xptr-framework/#syntax)
/// must be preserved because because it cannot correctly recognize the matching of parentheses.
///
/// If parsing fails, return `Err`.
///
/// # Syntax
/// ```text
/// [1] Pointer         ::= Shorthand | SchemeBased
/// [2] Shorthand       ::= NCName
/// [3] SchemeBased     ::= PointerPart (S? PointerPart)*
/// [4] PointerPart     ::= SchemeName '(' SchemeData ')'
/// [5] SchemeName      ::= QName
/// [6] SchemeData      ::= EscapedData*
/// [7] EscapedData     ::= NormalChar | '^(' | '^)' | '^^' | '(' SchemeData ')'
/// [8] NormalChar      ::= UnicodeChar - [()^]
/// [9] UnicodeChar     ::= [#x0-#x10FFFF]
/// ```
pub fn parse_xpointer(mut xpointer: &str) -> Result<XPointerResolver, XPointerParseError> {
    if VERSION.validate_ncname(xpointer) {
        // Shorthand Pointer
        return Ok(XPointerResolver {
            parts: vec![XPointerPart::Shorthand(xpointer.into())],
        });
    }

    let mut parts = vec![];
    while !xpointer.is_empty() {
        // parse SchemeName
        let mut cur = xpointer;
        cur = cur
            .strip_prefix(|c: char| VERSION.is_name_start_char(c) && c != ':')
            .ok_or(XPointerParseError::InvalidNCName)?;
        cur = cur.trim_start_matches(|c: char| VERSION.is_name_char(c) && c != ':');
        if let Some(rem) = cur.strip_prefix(':') {
            cur = rem
                .strip_prefix(|c: char| VERSION.is_name_start_char(c) && c != ':')
                .ok_or(XPointerParseError::InvalidSchemeName)?;
            cur = cur.trim_start_matches(|c: char| VERSION.is_name_char(c) && c != ':');
        }
        let scheme_name = &xpointer[..xpointer.len() - cur.len()];

        // remove '('
        xpointer = cur
            .strip_prefix('(')
            .ok_or(XPointerParseError::ParenthesNotFoundAfterSchemeName)?;

        let mut chars = xpointer.chars();
        let mut depth = 1;
        while depth > 0
            && let Some(c) = chars.next()
        {
            match c {
                ')' => depth -= 1,
                '(' => depth += 1,
                '^' => {
                    chars
                        .next()
                        .filter(|c| matches!(*c, '^' | '(' | ')'))
                        .ok_or(XPointerParseError::InvalidEscapedCharacter)?;
                }
                '\u{0}'..='\u{10FFFF}' => {}
            }
        }

        if depth > 0 {
            return Err(XPointerParseError::UnmatchParentheses);
        }

        let cur = chars.as_str();
        let scheme_data = &xpointer[..xpointer.len() - 1 - cur.len()];

        match scheme_name {
            "element" => {
                if let Ok(ret) = parse_element_scheme_data(scheme_data) {
                    parts.push(ret);
                }
            }
            "xmlns" => {
                if let Ok(ret) = parse_xmlns_scheme_data(scheme_data) {
                    parts.push(ret);
                }
            }
            _ => {
                // not supported, skip simply
            }
        }

        xpointer = cur.trim_start_matches(|c: char| VERSION.is_whitespace(c));
        if cur.len() != xpointer.len() && xpointer.is_empty() {
            return Err(XPointerParseError::SpaceFoundAfterAllPointerPart);
        }
    }

    Ok(XPointerResolver { parts })
}

fn parse_element_scheme_data(mut data: &str) -> Result<XPointerPart, XPointerParseError> {
    let mut id = None;
    // parse NCName
    if !data.starts_with('/') {
        let cur = data
            .strip_prefix(|c: char| VERSION.is_name_start_char(c) && c != ':')
            .ok_or(XPointerParseError::InvalidNCName)?
            .trim_start_matches(|c: char| VERSION.is_name_char(c) && c != ':');
        id = Some(data[..data.len() - cur.len()].into());
        data = cur;
    }

    let mut sequence = vec![];
    if let Some(data) = data.strip_prefix('/') {
        for seq in data.split('/') {
            match seq.parse::<usize>() {
                Ok(seq) => sequence.push(seq),
                Err(err) => match *err.kind() {
                    IntErrorKind::Empty => {
                        return Err(XPointerParseError::ElementEmptySequenceNumber);
                    }
                    IntErrorKind::InvalidDigit => {
                        return Err(XPointerParseError::ElementInvalidChildSequence);
                    }
                    IntErrorKind::NegOverflow => {
                        return Err(XPointerParseError::ElementNegativeSequenceNumber);
                    }
                    IntErrorKind::PosOverflow => {
                        return Err(XPointerParseError::ElementTooLargeSequenceNumber);
                    }
                    _ => todo!(),
                },
            }
        }
    } else if !data.is_empty() || id.is_none() {
        return Err(XPointerParseError::ElementInvalidChildSequence);
    }

    Ok(XPointerPart::Element { id, sequence })
}

fn parse_xmlns_scheme_data(mut data: &str) -> Result<XPointerPart, XPointerParseError> {
    // parse NCName
    let mut cur = data;
    cur = cur
        .strip_prefix(|c: char| VERSION.is_name_start_char(c) && c != ':')
        .ok_or(XPointerParseError::InvalidNCName)?;
    cur = cur.trim_start_matches(|c: char| VERSION.is_name_char(c) && c != ':');
    let prefix = data[..data.len() - cur.len()].into();

    // S? '=' S?
    data = cur.trim_start_matches(|c: char| VERSION.is_whitespace(c));
    data = data
        .strip_prefix('=')
        .ok_or(XPointerParseError::XMLNSEqualMarkNotFound)?;
    data = data.trim_start_matches(|c: char| VERSION.is_whitespace(c));

    // decode escaped data
    // EscapedData accepts all Unicode characters, so if it is valid as EscapedData,
    // syntax checking is unnecessary. Furthermore, since the validity as EscapedData
    // is guaranteed by the caller, there is no need to check it here.
    let mut cur = data.chars();
    let mut namespace_name = String::new();
    while let Some(c) = cur.next() {
        if c == '^' {
            let d = cur
                .next()
                .ok_or(XPointerParseError::InvalidEscapedCharacter)?;
            namespace_name.push(d);
        } else {
            namespace_name.push(c);
        }
    }

    Ok(XPointerPart::XMLNs {
        prefix,
        namespace_name: namespace_name.into(),
    })
}
