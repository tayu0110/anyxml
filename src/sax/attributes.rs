use std::{collections::HashMap, ops::Index, sync::Arc};

use crate::error::XMLError;

pub struct Attribute {
    pub uri: Option<Arc<str>>,
    pub local_name: Option<Arc<str>>,
    pub qname: Arc<str>,
    pub value: Box<str>,
    // 0: is declared in DTD
    // 1: is specified explicitly (in other words, `value` is not the default value provided by DTD)
    // 2: is namespace declaration attribute
    // 3: has declaration dependency normalization
    pub(crate) flag: u8,
}

impl Attribute {
    pub(crate) fn set_declared(&mut self) {
        self.flag |= 1 << 0;
    }
    pub(crate) fn set_specified(&mut self) {
        self.flag |= 1 << 1;
    }
    pub(crate) fn set_nsdecl(&mut self) {
        self.flag |= 1 << 2;
    }
    pub(crate) fn set_declaration_dependent_normalization(&mut self) {
        self.flag |= 1 << 3;
    }

    pub fn is_declared(&self) -> bool {
        self.flag & (1 << 0) != 0
    }
    pub fn is_specified(&self) -> bool {
        self.flag & (1 << 1) != 0
    }
    pub fn is_nsdecl(&self) -> bool {
        self.flag & (1 << 2) != 0
    }
    /// Check if this attribute's value is modified by
    pub(crate) fn has_declaration_dependent_normalization(&self) -> bool {
        self.flag & (1 << 3) != 0
    }
}

pub struct Attributes {
    attributes: Vec<Attribute>,
    index_by_qname: HashMap<Arc<str>, usize>,
    // key      : local_name
    // value    : uri_map
    index_by_expanded_name: HashMap<Arc<str>, HashMap<Arc<str>, usize>>,
}

impl Attributes {
    pub(crate) fn new() -> Self {
        Self {
            attributes: vec![],
            index_by_qname: HashMap::new(),
            index_by_expanded_name: HashMap::new(),
        }
    }

    pub fn get_index_by_qname(&self, qname: &str) -> Option<usize> {
        self.index_by_qname.get(qname).copied()
    }

    pub fn get_index_by_expanded_name(
        &self,
        namespace_uri: Option<&str>,
        local_name: &str,
    ) -> Option<usize> {
        self.index_by_expanded_name
            .get(local_name)?
            .get(namespace_uri.unwrap_or(""))
            .copied()
    }

    pub fn len(&self) -> usize {
        self.attributes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn contains_qname(&self, qname: &str) -> bool {
        self.get_index_by_qname(qname).is_some()
    }

    pub fn contains_expanded_name(&self, namespace_uri: Option<&str>, local_name: &str) -> bool {
        self.get_index_by_expanded_name(namespace_uri, local_name)
            .is_some()
    }

    pub fn get_local_name(&self, index: usize) -> Option<&str> {
        self.attributes.get(index)?.local_name.as_deref()
    }

    pub fn get_qname(&self, index: usize) -> Option<&str> {
        Some(self.attributes.get(index)?.qname.as_ref())
    }

    pub fn get_namespace_uri(&self, index: usize) -> Option<&str> {
        self.attributes.get(index)?.uri.as_deref()
    }

    pub fn get_value(&self, index: usize) -> Option<&str> {
        Some(self.attributes.get(index)?.value.as_ref())
    }

    pub fn get_value_by_qname(&self, qname: &str) -> Option<&str> {
        let index = self.get_index_by_qname(qname)?;
        self.get_value(index)
    }

    pub fn get_value_by_expanded_name(
        &self,
        namespace_uri: Option<&str>,
        local_name: &str,
    ) -> Option<&str> {
        let index = self.get_index_by_expanded_name(namespace_uri, local_name)?;
        self.get_value(index)
    }

    pub fn iter(&self) -> std::slice::Iter<'_, Attribute> {
        self.attributes.iter()
    }

    pub(crate) fn push(&mut self, attribute: Attribute) -> Result<usize, (Attribute, XMLError)> {
        use std::collections::hash_map::Entry::*;

        let index = self.attributes.len();
        if let Some(local_name) = attribute.local_name.clone() {
            match self.index_by_expanded_name.entry(local_name) {
                Vacant(entry) => {
                    let namespace_uri = attribute.uri.clone().unwrap_or_default();
                    let new = HashMap::from([(namespace_uri, index)]);
                    entry.insert(new);
                }
                Occupied(mut entry) => {
                    let map = entry.get_mut();
                    let namespace_uri = attribute.uri.clone().unwrap_or_default();
                    match map.entry(namespace_uri) {
                        Vacant(entry) => {
                            entry.insert(index);
                        }
                        Occupied(_) => {
                            return Err((attribute, XMLError::ParserDuplicateAttributes));
                        }
                    }
                }
            }
            self.index_by_qname.insert(attribute.qname.clone(), index);
            self.attributes.push(attribute);
            Ok(index)
        } else {
            match self.index_by_qname.entry(attribute.qname.clone()) {
                Vacant(entry) => {
                    entry.insert(index);
                    self.attributes.push(attribute);
                    Ok(index)
                }
                Occupied(_) => Err((attribute, XMLError::ParserDuplicateAttributes)),
            }
        }
    }

    pub(crate) fn set_namespace(
        &mut self,
        index: usize,
        mut resolve_prefix: impl FnMut(&str) -> Option<Arc<str>>,
    ) {
        let attribute = &mut self.attributes[index];
        let prefix = if let Some(local_name) = attribute.local_name.clone() {
            if local_name.len() == attribute.qname.len() {
                // According to the namespace specification, attribute names without prefixes
                // do not belong to the default namespace, but rather belong to no namespace.
                // Therefore, we need to do nothing.
                attribute.uri = None;
                return;
            }
            let prefix_len = attribute.qname.len() - local_name.len() - 1;
            &attribute.qname[..prefix_len]
        } else if let Some((prefix, local_name)) = attribute.qname.split_once(':') {
            attribute.local_name = Some(local_name.into());
            prefix
        } else {
            attribute.local_name = Some(attribute.qname.clone());
            return;
        };
        attribute.uri = resolve_prefix(prefix);
    }
}

impl Index<usize> for Attributes {
    type Output = Attribute;

    fn index(&self, index: usize) -> &Self::Output {
        &self.attributes[index]
    }
}

impl<'a> IntoIterator for &'a Attributes {
    type IntoIter = std::slice::Iter<'a, Attribute>;
    type Item = &'a Attribute;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}
