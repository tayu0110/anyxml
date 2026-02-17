use std::collections::BTreeMap;

use crate::{
    XMLVersion,
    error::XMLError,
    relaxng::{
        RelaxNGGrammar, RelaxNGSchema,
        grammar::{RelaxNGDefine, RelaxNGNonEmptyPattern, RelaxNGPattern},
    },
    tree::{Attribute, Element, Node, NodeType, convert::NodeKind, node::NodeSpec},
};

impl RelaxNGSchema {
    /// Validate XML document subtree whose root element is `element`.
    pub fn validate(&self, element: &Element) -> Result<(), XMLError> {
        self.grammar.validate(element)
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
                        .validate(type_name, &params, &value)
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
                    && library.eq(type_name, &lhs, value).unwrap_or_default()
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
