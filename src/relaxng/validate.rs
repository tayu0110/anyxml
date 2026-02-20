use std::{collections::BTreeMap, sync::Arc};

use crate::{
    XMLVersion,
    error::XMLError,
    relaxng::{
        Grammar, RelaxNGGrammar, RelaxNGSchema,
        grammar::{
            Context, Datatype, LocalName, ParamList, Pattern, PatternId, RelaxNGDefine,
            RelaxNGNonEmptyPattern, RelaxNGPattern, Uri,
        },
    },
    sax::{
        AttributeType, DefaultDecl, Locator, NamespaceStack,
        attributes::Attributes,
        contentspec::ContentSpec,
        error::SAXParseError,
        handler::{EntityResolver, ErrorHandler, SAXHandler},
        source::InputSource,
    },
    tree::{Attribute, Element, Node, NodeType, convert::NodeKind, node::NodeSpec},
    uri::{URIStr, URIString},
};

impl RelaxNGSchema {
    /// Validate XML document subtree whose root element is `element`.
    pub fn validate(&self, element: &Element) -> Result<(), XMLError> {
        self._grammar.validate(element)
    }
}

impl RelaxNGGrammar {
    /// Start to validate `element` using this grammar.
    ///
    /// # Reference
    /// ISO/IEC 19757-2:2008 9.4 Validity
    fn validate(&self, element: &Element) -> Result<(), XMLError> {
        let start = self.start.as_ref().ok_or(XMLError::RngValidNotAllowed)?;
        start.validate_element(element, self)
    }
}

impl RelaxNGDefine {
    /// # Reference
    /// - ISO/IEC 19757-2:2008 9.3.7 `element` and `attribute` pattern
    /// - ISO/IEC 19757-2:2008 9.4 Validity
    fn validate_element(
        &self,
        element: &Element,
        grammar: &RelaxNGGrammar,
    ) -> Result<(), XMLError> {
        let namespace_name = element.namespace_name();
        let local_name = element.local_name();
        if !self
            .name_class
            .try_match(&local_name, namespace_name.as_deref().unwrap_or_default())
        {
            return Err(XMLError::RngValidElement);
        }

        let attributes = element.attributes().collect::<Vec<_>>();
        let sequence = collect_child_sequence(element);

        let top = self.top.as_ref().ok_or(XMLError::RngValidNotAllowed)?;
        let mut attr_matches = vec![false; attributes.len()];
        let mut seq_matches = vec![false; sequence.len()];
        top.validate(
            &attributes,
            &mut attr_matches,
            &sequence,
            &mut seq_matches,
            grammar,
            true,
        )?;

        if attr_matches.into_iter().all(|b| b) && seq_matches.into_iter().all(|b| b) {
            Ok(())
        } else {
            Err(XMLError::RngValidElement)
        }
    }
}

impl RelaxNGPattern {
    /// If `weak` is `true`, this method tries weak-matching.
    ///
    /// # Reference
    /// - ISO/IEC 19757-2:2008 9.3.7 `element` and `attribute` pattern
    /// - ISO/IEC 19757-2:2008 9.4 Validity
    fn validate(
        &self,
        attributes: &[Attribute],
        attr_matches: &mut [bool],
        sequence: &[Node<dyn NodeSpec>],
        seq_matches: &mut [bool],
        grammar: &RelaxNGGrammar,
        weak: bool,
    ) -> Result<(), XMLError> {
        if let Some(pattern) = self.pattern.as_ref() {
            pattern.validate(
                attributes,
                attr_matches,
                sequence,
                seq_matches,
                grammar,
                weak,
            )
        } else {
            // ISO/IEC 19757-2:2008 9.3.3 `empty` pattern
            if sequence.is_empty()
                || (weak
                    && sequence.iter().all(|node| {
                        let ver = XMLVersion::default();
                        node.as_text()
                            .map(|text| text.data().chars().all(|c| ver.is_whitespace(c)))
                            .or_else(|| {
                                node.as_cdata_section()
                                    .map(|text| text.data().chars().all(|c| ver.is_whitespace(c)))
                            })
                            .unwrap_or_default()
                    }))
            {
                seq_matches.fill(true);
                Ok(())
            } else {
                seq_matches.fill(false);
                Err(XMLError::RngValidEmpty)
            }
        }
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 9.4 Validity
    fn validate_element(
        &self,
        element: &Element,
        grammar: &RelaxNGGrammar,
    ) -> Result<(), XMLError> {
        if let Some(pattern) = self.pattern.as_ref() {
            pattern.validate_element(element, grammar)
        } else {
            // ISO/IEC 19757-2:2008 9.3.3 `empty` pattern
            Err(XMLError::RngValidEmpty)
        }
    }
}

impl RelaxNGNonEmptyPattern {
    /// If `weak` is `true`, this method tries weak-matching.
    ///
    /// # Reference
    /// - ISO/IEC 19757-2:2008 9.3.7 `element` and `attribute` pattern
    /// - ISO/IEC 19757-2:2008 9.4 Validity
    fn validate(
        &self,
        attributes: &[Attribute],
        attr_matches: &mut [bool],
        sequence: &[Node<dyn NodeSpec>],
        seq_matches: &mut [bool],
        grammar: &RelaxNGGrammar,
        weak: bool,
    ) -> Result<(), XMLError> {
        match self {
            Self::Text => {
                if sequence
                    .iter()
                    .all(|node| matches!(node.node_type(), NodeType::CDATASection | NodeType::Text))
                {
                    seq_matches.fill(true);
                    Ok(())
                } else {
                    seq_matches.fill(false);
                    Err(XMLError::RngValidText)
                }
            }
            Self::Data {
                type_name,
                datatype_library,
                param,
                except_pattern,
            } => {
                let value = if sequence.len() == 1 {
                    sequence[0]
                        .as_text()
                        .ok_or(XMLError::RngValidData)?
                        .data()
                        .to_string()
                } else if weak && sequence.is_empty() {
                    "".to_owned()
                } else {
                    return Err(XMLError::RngValidData);
                };

                let params = param
                    .iter()
                    .map(|param| (param.name.as_ref().into(), param.value.as_ref().into()))
                    .collect::<BTreeMap<_, _>>();

                if let Some(library) = grammar.libraries.get(&datatype_library.to_string())
                    && library
                        .validate(
                            type_name,
                            &params,
                            &value,
                            &(URIString::parse("").unwrap().into(), BTreeMap::new()),
                        )
                        .unwrap_or_default()
                {
                    if let Some(except) = except_pattern.as_ref() {
                        if except
                            .pattern
                            .validate(
                                attributes,
                                attr_matches,
                                sequence,
                                seq_matches,
                                grammar,
                                weak,
                            )
                            .is_err()
                        {
                            seq_matches.fill(true);
                            Ok(())
                        } else {
                            seq_matches.fill(false);
                            Err(XMLError::RngValidData)
                        }
                    } else {
                        seq_matches.fill(true);
                        Ok(())
                    }
                } else {
                    seq_matches.fill(false);
                    Err(XMLError::RngValidData)
                }
            }
            Self::Value {
                type_name,
                datatype_library,
                value,
                ..
            } => {
                let lhs = if sequence.len() == 1 {
                    sequence[0]
                        .as_text()
                        .ok_or(XMLError::RngValidData)?
                        .data()
                        .to_string()
                } else if weak && sequence.is_empty() {
                    "".to_owned()
                } else {
                    return Err(XMLError::RngValidValue);
                };

                if let Some(library) = grammar.libraries.get(&datatype_library.to_string())
                    && library
                        .eq(
                            type_name,
                            &lhs,
                            &(URIString::parse("").unwrap().into(), BTreeMap::new()),
                            value,
                            &(URIString::parse("").unwrap().into(), BTreeMap::new()),
                        )
                        .unwrap_or_default()
                {
                    seq_matches.fill(true);
                    Ok(())
                } else {
                    seq_matches.fill(false);
                    Err(XMLError::RngValidValue)
                }
            }
            Self::List { pattern } => {
                if sequence.is_empty() {
                    return pattern.validate(
                        attributes,
                        attr_matches,
                        sequence,
                        seq_matches,
                        grammar,
                        false,
                    );
                }

                let document = sequence[0].owner_document();
                let mut strings = String::new();
                for node in sequence {
                    match node.downcast() {
                        NodeKind::Text(text) => strings.push_str(&text.data()),
                        NodeKind::CDATASection(cdata) => strings.push_str(&cdata.data()),
                        _ => return Err(XMLError::RngValidList),
                    }
                }

                let mut sequence = vec![];
                for token in strings
                    .split(|c| XMLVersion::default().is_whitespace(c))
                    .filter(|s| !s.is_empty())
                {
                    sequence.push(document.create_text(token).into());
                }

                let mut sm = vec![false; sequence.len()];
                pattern.validate(&[], attr_matches, &sequence, &mut sm, grammar, false)?;
                if sm.iter().all(|&m| m) {
                    seq_matches.fill(true);
                    Ok(())
                } else {
                    seq_matches.fill(false);
                    Err(XMLError::RngValidList)
                }
            }
            Self::Attribute {
                name_class,
                pattern,
            } => {
                for (i, attr) in attributes.iter().enumerate() {
                    let namespace_name = attr.namespace_name();
                    let local_name = attr.local_name();
                    let value = collect_child_sequence(attr);
                    if name_class
                        .try_match(&local_name, namespace_name.as_deref().unwrap_or_default())
                        && pattern
                            .validate(
                                &[],
                                &mut [],
                                &value,
                                &mut vec![false; value.len()],
                                grammar,
                                true,
                            )
                            .is_ok()
                    {
                        attr_matches[i] = true;
                        return Ok(());
                    }
                }
                Err(XMLError::RngValidAttribute)
            }
            Self::Ref { .. } => {
                let pos = sequence
                    .iter()
                    .position(|ch| matches!(ch.node_type(), NodeType::Element))
                    .ok_or(XMLError::RngValidRef)?;
                if sequence
                    .iter()
                    .enumerate()
                    .filter_map(|(i, ch)| (i != pos).then_some(ch))
                    .any(|ch| match ch.downcast() {
                        NodeKind::CDATASection(cdata) => cdata
                            .data()
                            .chars()
                            .any(|c| !XMLVersion::default().is_whitespace(c)),
                        NodeKind::Text(text) => text
                            .data()
                            .chars()
                            .any(|c| !XMLVersion::default().is_whitespace(c)),
                        _ => true,
                    })
                {
                    return Err(XMLError::RngValidRef);
                }
                let element = sequence[pos].as_element().ok_or(XMLError::RngValidRef)?;
                self.validate_element(&element, grammar)?;
                seq_matches.fill(true);
                Ok(())
            }
            Self::OneOrMore { pattern } => {
                pattern.handle_one_or_more(attributes, sequence, grammar, weak)?;
                attr_matches.iter_mut().for_each(|b| *b = true);
                seq_matches.iter_mut().for_each(|b| *b = true);
                Ok(())
            }
            Self::Choice { left, right } => {
                let mut am = attr_matches.to_owned();
                let mut sm = seq_matches.to_owned();
                let left = left.validate(attributes, &mut am, sequence, &mut sm, grammar, weak);
                if left.is_ok() && am.iter().all(|&m| m) && sm.iter().all(|&m| m) {
                    attr_matches
                        .iter_mut()
                        .zip(am)
                        .filter(|v| v.1)
                        .for_each(|v| *v.0 = true);
                    seq_matches
                        .iter_mut()
                        .zip(sm)
                        .filter(|v| v.1)
                        .for_each(|v| *v.0 = true);
                    return Ok(());
                }

                right.validate(
                    attributes,
                    attr_matches,
                    sequence,
                    seq_matches,
                    grammar,
                    weak,
                )
            }
            Self::Group { pattern } => {
                pattern[0].handle_group(&pattern[1], attributes, sequence, grammar, weak)?;
                attr_matches.fill(true);
                seq_matches.fill(true);
                Ok(())
            }
            Self::Interleave { pattern } => {
                pattern[0].handle_interleave(&pattern[1], attributes, sequence, grammar, weak)?;
                attr_matches.fill(true);
                seq_matches.fill(true);
                Ok(())
            }
        }
    }

    fn handle_one_or_more(
        &self,
        attributes: &[Attribute],
        sequence: &[Node<dyn NodeSpec>],
        grammar: &RelaxNGGrammar,
        weak: bool,
    ) -> Result<(), XMLError> {
        for mid in (1.min(sequence.len())..=sequence.len()).rev() {
            let (front, back) = sequence.split_at(mid);
            let mut attr_matches = vec![false; attributes.len()];
            let mut seq_matches = vec![false; front.len()];
            if self
                .validate(
                    attributes,
                    &mut attr_matches,
                    front,
                    &mut seq_matches,
                    grammar,
                    weak,
                )
                .is_ok()
            {
                if seq_matches.into_iter().any(|m| !m) {
                    continue;
                }
                if mid == sequence.len() && attr_matches.iter().all(|&a| a) {
                    return Ok(());
                }
                let attributes = attributes
                    .iter()
                    .zip(attr_matches)
                    .filter_map(|v| (!v.1).then_some(v.0.clone()))
                    .collect::<Vec<_>>();
                if self
                    .handle_one_or_more(&attributes, back, grammar, weak)
                    .is_ok()
                {
                    return Ok(());
                }
            }
        }

        Err(XMLError::RngValidOneOrMore)
    }

    /// Verify that `attributes` and `sequence` match the `interleave` pattern.  \
    /// `attributes` and `sequence` must match completely for all entries to return `Ok`.
    fn handle_interleave(
        &self,
        other: &Self,
        attributes: &[Attribute],
        sequence: &[Node<dyn NodeSpec>],
        grammar: &RelaxNGGrammar,
        weak: bool,
    ) -> Result<(), XMLError> {
        // Based on the constraints in Section 10.4,
        // it is possible to determine which pattern a name can match.
        //
        // Rerefence: 10.4 Restrictions on attributes
        let mut la = vec![];
        let mut ra = vec![];
        for attr in attributes {
            if self.contains_attribute_name(
                attr.namespace_name().as_deref().unwrap_or_default(),
                &attr.local_name(),
            ) {
                la.push(attr.clone());
            } else {
                ra.push(attr.clone());
            }
        }

        // Based on the constraints in Section 10.5,
        // it is possible to determine which pattern a name can match.
        //
        // Rerefence: 10.5 Restrictions on `interleave`
        let mut lm = vec![];
        let mut rm = vec![];
        for node in sequence {
            match node.downcast() {
                NodeKind::Element(element) => {
                    if self.contains_element_name(
                        element.namespace_name().as_deref().unwrap_or_default(),
                        &element.local_name(),
                        grammar,
                    ) {
                        lm.push(node.clone());
                    } else {
                        rm.push(node.clone());
                    }
                }
                _ => {
                    if self.contains_element_name("", "#text", grammar) {
                        lm.push(node.clone());
                    } else {
                        rm.push(node.clone());
                    }
                }
            }
        }

        let mut attr_matches = vec![false; attributes.len()];
        let mut seq_matches = vec![false; sequence.len()];
        self.validate(
            &la,
            &mut attr_matches[..la.len()],
            &lm,
            &mut seq_matches[..lm.len()],
            grammar,
            weak,
        )?;
        other.validate(
            &ra,
            &mut attr_matches[la.len()..],
            &rm,
            &mut seq_matches[lm.len()..],
            grammar,
            weak,
        )?;

        if attr_matches.iter().any(|&m| !m) || seq_matches.into_iter().any(|m| !m) {
            Err(XMLError::RngValidInterleave)
        } else {
            Ok(())
        }
    }

    /// Verify that `attributes` and `sequence` match the `group` pattern.  \
    /// `attributes` and `sequence` must match completely for all entries to return `Ok`.
    fn handle_group(
        &self,
        other: &Self,
        attributes: &[Attribute],
        sequence: &[Node<dyn NodeSpec>],
        grammar: &RelaxNGGrammar,
        weak: bool,
    ) -> Result<(), XMLError> {
        // Based on the constraints in Section 10.4,
        // it is possible to determine which pattern a name can match.
        //
        // Rerefence: 10.4 Restrictions on attributes
        let mut la = vec![];
        let mut ra = vec![];
        for attr in attributes {
            if self.contains_attribute_name(
                attr.namespace_name().as_deref().unwrap_or_default(),
                &attr.local_name(),
            ) {
                la.push(attr.clone());
            } else {
                ra.push(attr.clone());
            }
        }

        for mid in (0..=sequence.len()).rev() {
            let (front, back) = sequence.split_at(mid);
            let mut la_matches = vec![false; la.len()];
            let mut sm = vec![false; sequence.len()];
            if self
                .validate(
                    &la,
                    &mut la_matches,
                    front,
                    &mut sm[..front.len()],
                    grammar,
                    weak,
                )
                .is_ok()
                && la_matches.iter().all(|&m| m)
                && sm.iter().take(front.len()).all(|&m| m)
            {
                let mut ra_matches = vec![false; ra.len()];
                if other
                    .validate(
                        &ra,
                        &mut ra_matches,
                        back,
                        &mut sm[front.len()..],
                        grammar,
                        weak,
                    )
                    .is_ok()
                    && ra_matches.iter().all(|&m| m)
                    && sm.iter().skip(front.len()).all(|&m| m)
                {
                    return Ok(());
                }
            }
        }

        Err(XMLError::RngValidGroup)
    }

    fn contains_element_name(
        &self,
        namespace_name: &str,
        local_name: &str,
        grammar: &RelaxNGGrammar,
    ) -> bool {
        match self {
            Self::Text => namespace_name.is_empty() && local_name == "#text",
            Self::Data { .. } | Self::Value { .. } | Self::Attribute { .. } | Self::List { .. } => {
                false
            }
            Self::Ref { name } => {
                local_name != "#text"
                    && grammar.define.get(name.as_ref()).is_some_and(|define| {
                        define.name_class.try_match(local_name, namespace_name)
                    })
            }
            Self::OneOrMore { pattern } => {
                pattern.contains_element_name(namespace_name, local_name, grammar)
            }
            Self::Choice { left, right } => {
                left.pattern.as_ref().is_some_and(|left| {
                    left.contains_element_name(namespace_name, local_name, grammar)
                }) || right.contains_element_name(namespace_name, local_name, grammar)
            }
            Self::Group { pattern } | Self::Interleave { pattern } => {
                pattern[0].contains_element_name(namespace_name, local_name, grammar)
                    || pattern[1].contains_element_name(namespace_name, local_name, grammar)
            }
        }
    }

    fn contains_attribute_name(&self, namespace_name: &str, local_name: &str) -> bool {
        match self {
            Self::Text
            | Self::Data { .. }
            | Self::Value { .. }
            | Self::List { .. }
            | Self::Ref { .. } => false,
            Self::Attribute { name_class, .. } => name_class.try_match(local_name, namespace_name),
            Self::OneOrMore { pattern } => {
                pattern.contains_attribute_name(namespace_name, local_name)
            }
            Self::Choice { left, right } => {
                left.pattern
                    .as_ref()
                    .is_some_and(|left| left.contains_attribute_name(namespace_name, local_name))
                    || right.contains_attribute_name(namespace_name, local_name)
            }
            Self::Group { pattern } | Self::Interleave { pattern } => {
                pattern[0].contains_attribute_name(namespace_name, local_name)
                    || pattern[1].contains_attribute_name(namespace_name, local_name)
            }
        }
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 9.4 Validity
    fn validate_element(
        &self,
        element: &Element,
        grammar: &RelaxNGGrammar,
    ) -> Result<(), XMLError> {
        match self {
            Self::Text => Err(XMLError::RngValidText),
            Self::Data { .. } => Err(XMLError::RngValidData),
            Self::Value { .. } => Err(XMLError::RngValidValue),
            Self::List { .. } => Err(XMLError::RngValidList),
            Self::Attribute { .. } => Err(XMLError::RngValidAttribute),
            Self::Ref { name } => {
                let define = grammar
                    .define
                    .get(name.as_ref())
                    .ok_or(XMLError::RngValidRef)?;
                define.validate_element(element, grammar)
            }
            Self::OneOrMore { pattern } => pattern.validate_element(element, grammar),
            Self::Choice { left, right } => left
                .validate_element(element, grammar)
                .or_else(|_| right.validate_element(element, grammar)),
            Self::Group { .. } => Err(XMLError::RngValidGroup),
            Self::Interleave { .. } => Err(XMLError::RngValidInterleave),
        }
    }
}

fn collect_child_sequence(node: &Node<impl NodeSpec>) -> Vec<Node<dyn NodeSpec>> {
    let document = node.owner_document();
    let mut sequence = vec![];
    let mut children = node.first_child();
    let mut depth = 0;
    let mut buf = String::new();
    while let Some(child) = children {
        match child.downcast() {
            NodeKind::Text(text) => buf.push_str(&text.data()),
            NodeKind::CDATASection(cdata) => buf.push_str(&cdata.data()),
            NodeKind::Element(_) => {
                if !buf.is_empty() {
                    let text = document.create_text(buf.as_str());
                    sequence.push(text.into());
                    buf.clear();
                }
                sequence.push(child.clone());
            }
            _ => {}
        }
        if matches!(child.node_type(), NodeType::EntityReference)
            && let Some(first) = child.first_child()
        {
            depth += 1;
            children = Some(first);
        } else {
            children = child.next_sibling();
            if children.is_none() && depth > 0 {
                let mut now = child;
                while depth > 0
                    && let Some(parent) = now.parent_node()
                {
                    depth -= 1;
                    if let Some(next) = parent.next_sibling() {
                        children = Some(next);
                        break;
                    }
                    now = parent.into();
                }
            }
        }
    }

    if !buf.is_empty() {
        let text = document.create_text(buf.as_str());
        sequence.push(text.into());
    }
    sequence
}

macro_rules! generic_error {
    ($method:ident, $handler:expr, $code:expr, $level:expr, $message:literal, $( $args:expr ),*) => {
        #[allow(unused)]
        use $crate::error::XMLError::*;
        let ret = $crate::sax::error::SAXParseError {
            error: $code,
            level: $level,
            domain: $crate::error::XMLErrorDomain::RngValid,
            line: $handler.locator.line(),
            column: $handler.locator.column(),
            system_id: $handler.locator.system_id(),
            public_id: $handler.locator.public_id(),
            message: ::std::borrow::Cow::Owned(format!($message, $( $args ),*)),
        };
        $handler.last_error = Err(ret.clone());
        $handler.$method(ret);
    };
    ($method:ident, $handler:expr, $code:expr, $level:expr, $message:literal) => {
        #[allow(unused)]
        use $crate::error::XMLError::*;
        let ret = $crate::sax::error::SAXParseError {
            error: $code,
            level: $level,
            domain: $crate::error::XMLErrorDomain::RngValid,
            line: $handler.locator.line(),
            column: $handler.locator.column(),
            system_id: $handler.locator.system_id(),
            public_id: $handler.locator.public_id(),
            message: ::std::borrow::Cow::Borrowed($message),
        };
        $handler.last_error = Err(ret.clone());
        $handler.handler.$method(ret);
    };
    ($method:ident, $handler:expr, $code:expr, $level:expr, $message:expr) => {
        #[allow(unused)]
        use $crate::error::XMLError::*;
        let ret = $crate::sax::error::SAXParseError {
            error: $code,
            level: $level,
            domain: $crate::error::XMLErrorDomain::RngValid,
            line: $handler.locator.line(),
            column: $handler.locator.column(),
            system_id: $handler.locator.system_id(),
            public_id: $handler.locator.public_id(),
            message: ::std::borrow::Cow::Owned($message.into()),
        };
        $handler.last_error = Err(ret.clone());
        $handler.handler.$method(ret);
    };
}

macro_rules! validity_error {
    ($handler:expr, $code:ident, $message:literal, $( $args:expr ),*) => {
        generic_error!(
            error,
            $handler,
            $code,
            $crate::error::XMLErrorLevel::Error,
            $message,
            $( $args ),*
        );
    };
    ($handler:expr, $code:ident, $message:literal) => {
        validity_error!($handler, $code, $message, );
    };
    ($handler:expr, $code:ident, $message:expr) => {
        generic_error!(
            error,
            $handler,
            $code,
            $crate::error::XMLErrorLevel::Error,
            $message
        );
    };
}

pub(super) struct QName(pub(super) Uri, pub(super) LocalName);
pub(super) struct AttributeNode(pub(super) QName, pub(super) Arc<str>);

pub struct ValidateHandler<'a, H: SAXHandler> {
    pub child: H,
    grammar: &'a mut Grammar,

    // current pattern
    pattern: PatternId,

    // context
    locator: Arc<Locator>,
    base_uri: Arc<URIStr>,
    base_uri_stack: Vec<Arc<URIStr>>,
    ns_stack: NamespaceStack,
    last_error: Result<(), SAXParseError>,

    // text buffer
    // `text` can be validated as chunked character content,
    // but `value` and `data` cannot be validated correctly
    // unless all character content is collected.
    text: String,

    // Indicates whether the current content is mixed content.
    // It is necessary for weak-match checking.
    mixed: bool,
    mixed_stack: Vec<bool>,
}

impl Grammar {
    pub(super) fn new_validate_handler<H: SAXHandler>(
        &mut self,
        handler: H,
    ) -> ValidateHandler<'_, H> {
        let pattern = self.root;
        ValidateHandler {
            child: handler,
            grammar: self,
            pattern,
            locator: Arc::new(Locator::default()),
            base_uri: URIString::parse("").unwrap().into(),
            base_uri_stack: vec![],
            ns_stack: NamespaceStack::default(),
            last_error: Ok(()),
            text: String::new(),
            mixed: false,
            mixed_stack: vec![],
        }
    }

    pub(super) fn create_node(&mut self, pattern: Pattern) -> PatternId {
        if let Some(&index) = self.intern.get(&pattern) {
            index
        } else {
            let new = Arc::new(pattern);
            let at = self.patterns.len();
            self.intern.insert(new.clone(), at);
            self.patterns.push(new);
            self.nullable.push(-1);
            at
        }
    }

    fn nullable(&mut self, node: PatternId) -> bool {
        if self.nullable[node] >= 0 {
            return self.nullable[node] != 0;
        }
        let ret = match self.patterns[node].as_ref() {
            &Pattern::Group(p1, p2) => self.nullable(p1) && self.nullable(p2),
            &Pattern::Interleave(p1, p2) => self.nullable(p1) && self.nullable(p2),
            &Pattern::Choice(p1, p2) => self.nullable(p1) || self.nullable(p2),
            &Pattern::OneOrMore(p) => self.nullable(p),
            &Pattern::Element(_, _)
            | &Pattern::Attribute(_, _)
            | &Pattern::List(_)
            | &Pattern::Value(_, _, _)
            | &Pattern::Data(_, _)
            | &Pattern::DataExcept(_, _, _)
            | &Pattern::NotAllowed
            | &Pattern::After(_, _) => false,
            &Pattern::Empty | &Pattern::Text => true,
        };
        self.nullable[node] = ret as i8;
        ret
    }

    fn text_deriv(&mut self, context: &Context, pattern: PatternId, string: &str) -> PatternId {
        match (context, self.patterns[pattern].as_ref(), string) {
            (cx, &Pattern::Choice(p1, p2), s) => {
                let p1 = self.text_deriv(cx, p1, s);
                let p2 = self.text_deriv(cx, p2, s);
                self.choice(p1, p2)
            }
            (cx, &Pattern::Interleave(p1, p2), s) => {
                let r1 = self.text_deriv(cx, p1, s);
                let q1 = self.interleave(r1, p2);
                let r2 = self.text_deriv(cx, p2, s);
                let q2 = self.interleave(p1, r2);
                self.choice(q1, q2)
            }
            (cx, &Pattern::Group(p1, p2), s) => {
                let q = self.text_deriv(cx, p1, s);
                let p = self.group(q, p2);
                if self.nullable(p1) {
                    let q = self.text_deriv(cx, p2, s);
                    self.choice(p, q)
                } else {
                    p
                }
            }
            (cx, &Pattern::After(p1, p2), s) => {
                let q = self.text_deriv(cx, p1, s);
                self.after(q, p2)
            }
            (cx, &Pattern::OneOrMore(p), s) => {
                let q1 = self.text_deriv(cx, p, s);
                let r1 = self.create_node(Pattern::OneOrMore(p));
                let r2 = self.create_node(Pattern::Empty);
                let q2 = self.choice(r1, r2);
                self.group(q1, q2)
            }
            (_, Pattern::Text, _) => pattern,
            (cx1, Pattern::Value(dt, value, cx2), s) => {
                if self.datatype_equal(dt, value, cx2, s, cx1) {
                    self.create_node(Pattern::Empty)
                } else {
                    self.create_node(Pattern::NotAllowed)
                }
            }
            (cx, Pattern::Data(dt, params), s) => {
                if self.datatype_allows(dt, params, s, cx) {
                    self.create_node(Pattern::Empty)
                } else {
                    self.create_node(Pattern::NotAllowed)
                }
            }
            (cx, &Pattern::DataExcept(ref dt, ref params, p), s) => {
                if self.datatype_allows(dt, params, s, cx)
                    && let q = self.text_deriv(cx, p, s)
                    && !self.nullable(q)
                {
                    self.create_node(Pattern::Empty)
                } else {
                    self.create_node(Pattern::NotAllowed)
                }
            }
            (cx, &Pattern::List(p), s) => {
                let q = self.list_deriv(
                    cx,
                    p,
                    &s.split(|c| XMLVersion::default().is_whitespace(c))
                        .filter(|s| !s.is_empty())
                        .collect::<Vec<_>>(),
                );
                if self.nullable(q) {
                    self.create_node(Pattern::Empty)
                } else {
                    self.create_node(Pattern::NotAllowed)
                }
            }
            (_, _, _) => self.create_node(Pattern::NotAllowed),
        }
    }

    fn list_deriv(&mut self, cx: &Context, p: PatternId, list: &[&str]) -> PatternId {
        match list {
            [] => p,
            [h, t @ ..] => {
                let p = self.text_deriv(cx, p, h);
                self.list_deriv(cx, p, t)
            }
        }
    }

    fn choice(&mut self, p1: PatternId, p2: PatternId) -> PatternId {
        if p1 == p2 {
            return p1;
        }
        match (self.patterns[p1].as_ref(), self.patterns[p2].as_ref()) {
            (_, Pattern::NotAllowed) => p1,
            (Pattern::NotAllowed, _) => p2,
            (_, _) => self.create_node(Pattern::Choice(p1.min(p2), p1.max(p2))),
        }
    }

    fn group(&mut self, p1: PatternId, p2: PatternId) -> PatternId {
        match (self.patterns[p1].as_ref(), self.patterns[p2].as_ref()) {
            (_, Pattern::NotAllowed) => p2,
            (Pattern::NotAllowed, _) => p1,
            (_, Pattern::Empty) => p1,
            (Pattern::Empty, _) => p2,
            (_, _) => self.create_node(Pattern::Group(p1, p2)),
        }
    }

    fn interleave(&mut self, p1: PatternId, p2: PatternId) -> PatternId {
        match (self.patterns[p1].as_ref(), self.patterns[p2].as_ref()) {
            (_, Pattern::NotAllowed) => p2,
            (Pattern::NotAllowed, _) => p1,
            (_, Pattern::Empty) => p1,
            (Pattern::Empty, _) => p2,
            (_, _) => self.create_node(Pattern::Interleave(p1.min(p2), p1.max(p2))),
        }
    }

    fn one_or_more(&mut self, p: PatternId) -> PatternId {
        match self.patterns[p].as_ref() {
            Pattern::NotAllowed => p,
            _ => self.create_node(Pattern::OneOrMore(p)),
        }
    }

    fn after(&mut self, p1: PatternId, p2: PatternId) -> PatternId {
        match (self.patterns[p1].as_ref(), self.patterns[p2].as_ref()) {
            (_, Pattern::NotAllowed) => p2,
            (Pattern::NotAllowed, _) => p1,
            (_, _) => self.create_node(Pattern::After(p1, p2)),
        }
    }

    fn datatype_allows(
        &self,
        dt: &Datatype,
        params: &ParamList,
        string: &str,
        context: &Context,
    ) -> bool {
        self.libraries
            .get(&dt.0)
            .and_then(|lib| lib.validate(&dt.1, params, string, context))
            .is_some_and(|b| b)
    }

    fn datatype_equal(
        &self,
        dt: &Datatype,
        s1: &str,
        cx1: &Context,
        s2: &str,
        cx2: &Context,
    ) -> bool {
        self.libraries
            .get(&dt.0)
            .and_then(|lib| lib.eq(&dt.1, s1, cx1, s2, cx2))
            .is_some_and(|b| b)
    }

    fn apply_after(
        &mut self,
        f: &impl Fn(&mut Grammar, PatternId) -> PatternId,
        pattern: PatternId,
    ) -> PatternId {
        match *self.patterns[pattern] {
            Pattern::After(p1, p2) => {
                let p2 = f(self, p2);
                self.after(p1, p2)
            }
            Pattern::Choice(p1, p2) => {
                let q1 = self.apply_after(f, p1);
                let q2 = self.apply_after(f, p2);
                self.choice(q1, q2)
            }
            Pattern::NotAllowed => pattern,
            _ => unreachable!(),
        }
    }

    fn start_tag_open_deriv(&mut self, pattern: PatternId, qn: &QName) -> PatternId {
        match *self.patterns[pattern] {
            Pattern::Choice(p1, p2) => {
                let q1 = self.start_tag_open_deriv(p1, qn);
                let q2 = self.start_tag_open_deriv(p2, qn);
                self.choice(q1, q2)
            }
            Pattern::Element(ref nc, p) => {
                if nc.contains(qn) {
                    let r = self.create_node(Pattern::Empty);
                    self.after(p, r)
                } else {
                    self.create_node(Pattern::NotAllowed)
                }
            }
            Pattern::Interleave(p1, p2) => {
                let r = self.start_tag_open_deriv(p1, qn);
                let q1 = self.apply_after(
                    &move |slf: &mut Grammar, p: PatternId| slf.interleave(p, p2),
                    r,
                );
                let r = self.start_tag_open_deriv(p2, qn);
                let q2 =
                    self.apply_after(&move |slf: &mut Grammar, p: usize| slf.interleave(p1, p), r);
                self.choice(q1, q2)
            }
            Pattern::OneOrMore(p) => {
                let q = self.start_tag_open_deriv(p, qn);
                self.apply_after(
                    &move |slf: &mut Grammar, p: usize| {
                        let r = slf.create_node(Pattern::Empty);
                        let q = slf.choice(pattern, r);
                        slf.group(p, q)
                    },
                    q,
                )
            }
            Pattern::Group(p1, p2) => {
                let q = self.start_tag_open_deriv(p1, qn);
                let x = self.apply_after(&move |slf: &mut Grammar, p1: usize| slf.group(p1, p2), q);
                if self.nullable(p1) {
                    let q = self.start_tag_open_deriv(p2, qn);
                    self.choice(x, q)
                } else {
                    x
                }
            }
            Pattern::After(p1, p2) => {
                let q = self.start_tag_open_deriv(p1, qn);
                self.apply_after(&move |slf: &mut Grammar, p1: usize| slf.after(p1, p2), q)
            }
            _ => self.create_node(Pattern::NotAllowed),
        }
    }

    fn att_deriv(&mut self, cx: &Context, pattern: PatternId, att: &AttributeNode) -> PatternId {
        match (self.patterns[pattern].as_ref(), att) {
            (&Pattern::After(p1, p2), att) => {
                let q = self.att_deriv(cx, p1, att);
                self.after(q, p2)
            }
            (&Pattern::Choice(p1, p2), att) => {
                let q1 = self.att_deriv(cx, p1, att);
                let q2 = self.att_deriv(cx, p2, att);
                self.choice(q1, q2)
            }
            (&Pattern::Group(p1, p2), att) => {
                let r1 = self.att_deriv(cx, p1, att);
                let q1 = self.group(r1, p2);
                let r2 = self.att_deriv(cx, p2, att);
                let q2 = self.group(p1, r2);
                self.choice(q1, q2)
            }
            (&Pattern::Interleave(p1, p2), att) => {
                let r1 = self.att_deriv(cx, p1, att);
                let q1 = self.interleave(r1, p2);
                let r2 = self.att_deriv(cx, p2, att);
                let q2 = self.interleave(p1, r2);
                self.choice(q1, q2)
            }
            (&Pattern::OneOrMore(p), att) => {
                let q1 = self.att_deriv(cx, p, att);
                let r = self.create_node(Pattern::Empty);
                let q2 = self.choice(pattern, r);
                self.group(q1, q2)
            }
            (Pattern::Attribute(nc, p), AttributeNode(qn, s)) => {
                if nc.contains(qn) && self.value_match(cx, *p, s) {
                    self.create_node(Pattern::Empty)
                } else {
                    self.create_node(Pattern::NotAllowed)
                }
            }
            (_, _) => self.create_node(Pattern::NotAllowed),
        }
    }

    fn value_match(&mut self, cx: &Context, p: PatternId, s: &str) -> bool {
        (self.nullable(p) && s.chars().all(|c| XMLVersion::default().is_whitespace(c))) || {
            let q = self.text_deriv(cx, p, s);
            self.nullable(q)
        }
    }

    fn start_tag_close_deriv(&mut self, pattern: PatternId) -> PatternId {
        match self.patterns[pattern].as_ref() {
            &Pattern::After(p1, p2) => {
                let p1 = self.start_tag_close_deriv(p1);
                self.after(p1, p2)
            }
            &Pattern::Choice(p1, p2) => {
                let q1 = self.start_tag_close_deriv(p1);
                let q2 = self.start_tag_close_deriv(p2);
                self.choice(q1, q2)
            }
            &Pattern::Group(p1, p2) => {
                let q1 = self.start_tag_close_deriv(p1);
                let q2 = self.start_tag_close_deriv(p2);
                self.group(q1, q2)
            }
            &Pattern::Interleave(p1, p2) => {
                let q1 = self.start_tag_close_deriv(p1);
                let q2 = self.start_tag_close_deriv(p2);
                self.interleave(q1, q2)
            }
            &Pattern::OneOrMore(p) => {
                let q = self.start_tag_close_deriv(p);
                self.one_or_more(q)
            }
            Pattern::Attribute(_, _) => self.create_node(Pattern::NotAllowed),
            _ => pattern,
        }
    }

    fn end_tag_deriv(&mut self, p: PatternId) -> PatternId {
        match *self.patterns[p].as_ref() {
            Pattern::Choice(p1, p2) => {
                let (p1, p2) = (self.end_tag_deriv(p1), self.end_tag_deriv(p2));
                self.choice(p1, p2)
            }
            Pattern::After(p1, p2) => {
                if self.nullable(p1) {
                    p2
                } else {
                    self.create_node(Pattern::NotAllowed)
                }
            }
            _ => self.create_node(Pattern::NotAllowed),
        }
    }
}

impl<'a, H: SAXHandler> ErrorHandler for ValidateHandler<'a, H> {
    fn fatal_error(&mut self, error: SAXParseError) {
        self.child.fatal_error(error);
    }

    fn error(&mut self, error: SAXParseError) {
        self.child.error(error);
    }

    fn warning(&mut self, error: SAXParseError) {
        self.child.warning(error);
    }
}
impl<'a, H: SAXHandler> EntityResolver for ValidateHandler<'a, H> {
    fn resolve_entity(
        &mut self,
        name: &str,
        public_id: Option<&str>,
        base_uri: &URIStr,
        system_id: &URIStr,
    ) -> Result<InputSource<'static>, XMLError> {
        self.child
            .resolve_entity(name, public_id, base_uri, system_id)
    }

    fn get_external_subset(
        &mut self,
        name: &str,
        base_uri: Option<&URIStr>,
    ) -> Result<InputSource<'static>, XMLError> {
        self.child.get_external_subset(name, base_uri)
    }
}
impl<'a, H: SAXHandler> SAXHandler for ValidateHandler<'a, H> {
    fn set_document_locator(&mut self, locator: Arc<Locator>) {
        self.child.set_document_locator(locator.clone());
        self.base_uri = locator.system_id();
        self.base_uri_stack.clear();
        self.ns_stack.clear();
        self.last_error = Ok(());
        self.locator = locator;
        self.pattern = self.grammar.root;
        self.text.clear();
        self.mixed = false;
        self.mixed_stack.clear();
    }

    fn start_prefix_mapping(&mut self, prefix: Option<&str>, uri: &str) {
        self.ns_stack.push(prefix.unwrap_or_default(), uri);
        self.child.start_prefix_mapping(prefix, uri);
    }
    fn end_prefix_mapping(&mut self, prefix: Option<&str>) {
        // The popped prefix may not necessarily be `prefix`,
        // but the count matches up, so it's fine.
        self.ns_stack.pop();
        self.child.end_prefix_mapping(prefix);
    }

    fn start_element(
        &mut self,
        namespace_name: Option<&str>,
        local_name: Option<&str>,
        qname: &str,
        atts: &Attributes,
    ) {
        if !self.text.is_empty() {
            if !self
                .text
                .chars()
                .all(|c| XMLVersion::default().is_whitespace(c))
            {
                let ns_map = self
                    .ns_stack
                    .iter()
                    .map(|ns| (ns.prefix.clone(), ns.namespace_name.clone()))
                    .collect::<BTreeMap<_, _>>();
                let pattern = self.grammar.text_deriv(
                    &(self.base_uri.clone(), ns_map),
                    self.pattern,
                    &self.text,
                );

                if matches!(self.grammar.patterns[pattern].as_ref(), Pattern::NotAllowed) {
                    validity_error!(
                        self,
                        RngValidText,
                        "A text content '{}' cannot appear before the element '{}'.",
                        self.text,
                        qname
                    );
                } else {
                    self.pattern = pattern;
                }
            }
            self.text.clear();
        }
        let mut base_uri = self.base_uri.clone();
        if let Some(att) = atts.get_value_by_qname("xml:base") {
            if let Ok(uri) = URIString::parse(att) {
                base_uri = base_uri.resolve(&uri).into();
            } else {
                validity_error!(
                    self,
                    RngValidAttribute,
                    "The value '{}' for 'xml:base' attribute is invalid URI reference.",
                    att
                );
            }
        }

        self.base_uri_stack.push(base_uri.clone());
        self.base_uri = base_uri.clone();
        self.mixed_stack.push(true);
        self.mixed = false;

        let ns_map = self
            .ns_stack
            .iter()
            .map(|ns| (ns.prefix.clone(), ns.namespace_name.clone()))
            .collect::<BTreeMap<_, _>>();
        let cx = (base_uri, ns_map);
        let mut p = self.grammar.start_tag_open_deriv(
            self.pattern,
            &QName(
                namespace_name.unwrap_or_default().into(),
                local_name.unwrap_or_default().into(),
            ),
        );
        if matches!(self.grammar.patterns[p].as_ref(), Pattern::NotAllowed) {
            validity_error!(
                self,
                RngValidElement,
                "The element '{}' is not allowed here.",
                qname
            );
        } else {
            for att in atts.iter().filter(|att| !att.is_nsdecl()) {
                let attribute = AttributeNode(
                    QName(
                        att.namespace_name.clone().unwrap_or_default(),
                        att.local_name.clone().unwrap_or_default(),
                    ),
                    att.value.as_ref().into(),
                );
                let np = self.grammar.att_deriv(&cx, p, &attribute);
                if matches!(self.grammar.patterns[np].as_ref(), Pattern::NotAllowed) {
                    validity_error!(
                        self,
                        RngValidAttribute,
                        "The attribute '{}' is not allowed in the element '{}'.",
                        att.qname,
                        qname
                    );
                } else {
                    p = np;
                }
            }
            let p3 = self.grammar.start_tag_close_deriv(p);
            if matches!(self.grammar.patterns[p3].as_ref(), Pattern::NotAllowed) {
                validity_error!(
                    self,
                    RngValidAttribute,
                    "Some attributes are not specified in the element '{}'.",
                    qname
                );
            } else {
                self.pattern = p3;
            }
        }

        self.child
            .start_element(namespace_name, local_name, qname, atts);
    }
    fn end_element(&mut self, namespace_name: Option<&str>, local_name: Option<&str>, qname: &str) {
        if !self.text.is_empty() || !self.mixed {
            let ns_map = self
                .ns_stack
                .iter()
                .map(|ns| (ns.prefix.clone(), ns.namespace_name.clone()))
                .collect::<BTreeMap<_, _>>();
            let p =
                self.grammar
                    .text_deriv(&(self.base_uri.clone(), ns_map), self.pattern, &self.text);
            if self
                .text
                .chars()
                .all(|c| XMLVersion::default().is_whitespace(c))
            {
                self.pattern = self.grammar.choice(self.pattern, p);
            } else if matches!(self.grammar.patterns[p].as_ref(), Pattern::NotAllowed) {
                validity_error!(
                    self,
                    RngValidText,
                    "The text content '{}' is not allowed before eng tag '{}'.",
                    self.text,
                    qname
                );
            } else {
                self.pattern = p;
            }
            self.text.clear();
        }

        self.pattern = self.grammar.end_tag_deriv(self.pattern);
        if matches!(
            self.grammar.patterns[self.pattern].as_ref(),
            Pattern::NotAllowed
        ) {
            validity_error!(
                self,
                RngValidElement,
                "The content of '{}' is insufficient.",
                qname
            );
        }
        if let Some(base_uri) = self.base_uri_stack.pop() {
            self.base_uri = base_uri;
        }
        if let Some(mixed) = self.mixed_stack.pop() {
            self.mixed = mixed;
        }
        self.child.end_element(namespace_name, local_name, qname);
    }

    fn characters(&mut self, data: &str) {
        self.text.push_str(data);
        self.child.characters(data);
    }

    fn end_document(&mut self) {
        if !self.grammar.nullable(self.pattern) && self.last_error.is_ok() {
            validity_error!(
                self,
                RngValidUnknownError,
                "Finish validation unsuccessfully"
            );
        }
        self.child.end_document();
    }

    // Following callbacks are not used for RELAX NG validation.
    // They simply forward received events to the child handler.

    fn attribute_decl(
        &mut self,
        element_name: &str,
        attribute_name: &str,
        attribute_type: &AttributeType,
        default_decl: &DefaultDecl,
    ) {
        self.child
            .attribute_decl(element_name, attribute_name, attribute_type, default_decl);
    }
    fn comment(&mut self, data: &str) {
        self.child.comment(data);
    }
    fn declaration(&mut self, version: &str, encoding: Option<&str>, standalone: Option<bool>) {
        self.child.declaration(version, encoding, standalone);
    }
    fn element_decl(&mut self, name: &str, contentspec: &ContentSpec) {
        self.child.element_decl(name, contentspec);
    }
    fn external_entity_decl(&mut self, name: &str, public_id: Option<&str>, system_id: &URIStr) {
        self.child.external_entity_decl(name, public_id, system_id);
    }
    fn ignorable_whitespace(&mut self, data: &str) {
        self.child.ignorable_whitespace(data);
    }
    fn internal_entity_decl(&mut self, name: &str, value: &str) {
        self.child.internal_entity_decl(name, value);
    }
    fn notation_decl(&mut self, name: &str, public_id: Option<&str>, system_id: Option<&URIStr>) {
        self.child.notation_decl(name, public_id, system_id);
    }
    fn processing_instruction(&mut self, target: &str, data: Option<&str>) {
        self.child.processing_instruction(target, data);
    }
    fn skipped_entity(&mut self, name: &str) {
        self.child.skipped_entity(name);
    }
    fn start_cdata(&mut self) {
        self.child.start_cdata();
    }
    fn end_cdata(&mut self) {
        self.child.end_cdata();
    }
    fn start_document(&mut self) {
        self.child.start_document();
    }
    fn start_dtd(&mut self, name: &str, public_id: Option<&str>, system_id: Option<&URIStr>) {
        self.child.start_dtd(name, public_id, system_id);
    }
    fn end_dtd(&mut self) {
        self.child.end_dtd();
    }
    fn start_entity(&mut self, name: &str) {
        self.child.start_entity(name);
    }
    fn end_entity(&mut self) {
        self.child.end_entity();
    }
    fn unparsed_entity_decl(
        &mut self,
        name: &str,
        public_id: Option<&str>,
        system_id: &URIStr,
        notation_name: &str,
    ) {
        self.child
            .unparsed_entity_decl(name, public_id, system_id, notation_name);
    }
}
