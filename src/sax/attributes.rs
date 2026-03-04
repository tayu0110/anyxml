use std::ops::Index;

use crate::error::XMLError;

#[derive(Debug, Clone)]
pub struct Attribute {
    pub namespace_name: Option<String>,
    pub local_name: Option<String>,
    pub qname: String,
    pub value: String,
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
}

impl Attributes {
    pub(crate) fn new() -> Self {
        Self { attributes: vec![] }
    }

    /// Get the index of an attribute whose QName is `qname`.
    pub fn get_index_by_qname(&self, qname: &str) -> Option<usize> {
        self.attributes.iter().position(|att| att.qname == qname)
    }

    /// Get the index of an attribute whose extended name is `{namespace_name}local_name`.
    pub fn get_index_by_expanded_name(
        &self,
        namespace_name: Option<&str>,
        local_name: &str,
    ) -> Option<usize> {
        self.attributes.iter().position(|att| {
            att.local_name.as_deref() == Some(local_name)
                && att.namespace_name.as_deref() == namespace_name
        })
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
        self.attributes.get(index)?.namespace_name.as_deref()
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

    #[allow(clippy::result_large_err)]
    pub(crate) fn push(&mut self, attribute: Attribute) -> Result<usize, (Attribute, XMLError)> {
        let index = self.attributes.len();
        if self.get_index_by_qname(&attribute.qname).is_some() {
            return Err((attribute, XMLError::ParserDuplicateAttributes));
        }
        if let Some(local_name) = attribute.local_name.as_deref()
            && self
                .get_index_by_expanded_name(attribute.namespace_name.as_deref(), local_name)
                .is_some()
        {
            return Err((attribute, XMLError::ParserDuplicateAttributes));
        }
        self.attributes.push(attribute);
        Ok(index)
    }

    pub(crate) fn clear(&mut self) {
        self.attributes.clear();
    }

    pub(crate) fn drain(&mut self) -> std::vec::Drain<'_, Attribute> {
        self.attributes.drain(..)
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
