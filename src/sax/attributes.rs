use std::{collections::HashMap, ops::Index, sync::Arc};

use crate::error::XMLError;

#[derive(Debug, Clone)]
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

    /// Check if this attribute is declared in DTD.
    pub fn is_declared(&self) -> bool {
        self.flag & (1 << 0) != 0
    }
    /// Check if this attribute is specified explicitly.
    ///
    /// In other words, check if this attribute originates from the default declaration.
    pub fn is_specified(&self) -> bool {
        self.flag & (1 << 1) != 0
    }
    /// Check if this attribute is a namespace declaration attribute.
    pub fn is_nsdecl(&self) -> bool {
        self.flag & (1 << 2) != 0
    }
    /// Check if this attribute's value is modified by
    pub(crate) fn has_declaration_dependent_normalization(&self) -> bool {
        self.flag & (1 << 3) != 0
    }
}

/// A list of attributes.
///
/// This list may contain namespace declarations.  
#[derive(Debug, Clone, Default)]
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

    /// Get the index of an attribute whose QName is `qname`.
    pub fn get_index_by_qname(&self, qname: &str) -> Option<usize> {
        self.index_by_qname.get(qname).copied()
    }

    /// Get the index of an attribute whose extended name is `{namespace_name}local_name`.
    pub fn get_index_by_expanded_name(
        &self,
        namespace_name: Option<&str>,
        local_name: &str,
    ) -> Option<usize> {
        self.index_by_expanded_name
            .get(local_name)?
            .get(namespace_name.unwrap_or(""))
            .copied()
    }

    /// The number of attributes contained in this list.
    pub fn len(&self) -> usize {
        self.attributes.len()
    }

    /// Check if this list has no attributes.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Check if this list has an attribute whose QName is `qname`.
    pub fn contains_qname(&self, qname: &str) -> bool {
        self.get_index_by_qname(qname).is_some()
    }

    /// Check if this list has an attribute whose extended name is `{namespace_name}local_name`.
    pub fn contains_expanded_name(&self, namespace_name: Option<&str>, local_name: &str) -> bool {
        self.get_index_by_expanded_name(namespace_name, local_name)
            .is_some()
    }

    /// Get the local name of `index`-th attribute in this list.
    pub fn get_local_name(&self, index: usize) -> Option<&str> {
        self.attributes.get(index)?.local_name.as_deref()
    }

    /// Get the QName of `index`-th attribute in this list.
    pub fn get_qname(&self, index: usize) -> Option<&str> {
        Some(self.attributes.get(index)?.qname.as_ref())
    }

    /// Get the namespace name of `index`-th attribute in this list.
    pub fn get_namespace_uri(&self, index: usize) -> Option<&str> {
        self.attributes.get(index)?.uri.as_deref()
    }

    /// Get the value of `index`-th attribute in this list.
    pub fn get_value(&self, index: usize) -> Option<&str> {
        Some(self.attributes.get(index)?.value.as_ref())
    }

    /// Get the value of an attribute whose QName is `qname`.
    pub fn get_value_by_qname(&self, qname: &str) -> Option<&str> {
        let index = self.get_index_by_qname(qname)?;
        self.get_value(index)
    }

    /// Get the value of an attribute whose extended name is `{namespace_name}local_name`.
    pub fn get_value_by_expanded_name(
        &self,
        namespace_name: Option<&str>,
        local_name: &str,
    ) -> Option<&str> {
        let index = self.get_index_by_expanded_name(namespace_name, local_name)?;
        self.get_value(index)
    }

    pub fn iter(&self) -> std::slice::Iter<'_, Attribute> {
        self.attributes.iter()
    }

    pub(crate) fn push(&mut self, attribute: Attribute) -> Result<usize, (Attribute, XMLError)> {
        use std::collections::hash_map::Entry::*;

        let index = self.attributes.len();
        match self.index_by_qname.entry(attribute.qname.clone()) {
            Vacant(entry) => {
                entry.insert(index);
            }
            Occupied(_) => return Err((attribute, XMLError::ParserDuplicateAttributes)),
        }
        if let Some(local_name) = attribute.local_name.clone() {
            match self.index_by_expanded_name.entry(local_name) {
                Vacant(entry) => {
                    let namespace_name = attribute.uri.clone().unwrap_or_default();
                    let new = HashMap::from([(namespace_name, index)]);
                    entry.insert(new);
                }
                Occupied(mut entry) => {
                    let map = entry.get_mut();
                    let namespace_name = attribute.uri.clone().unwrap_or_default();
                    match map.entry(namespace_name) {
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
        }
        self.attributes.push(attribute);
        Ok(index)
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
