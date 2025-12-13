use std::{
    cell::RefCell,
    collections::{BTreeSet, HashMap},
    marker::PhantomData,
    rc::Rc,
};

use crate::{
    XML_NS_NAMESPACE, XML_XML_NAMESPACE,
    tree::{
        Attribute, Document, NodeType, XMLTreeError,
        attribute::AttributeSpec,
        namespace::{Namespace, NamespaceSpec},
        node::{InternalNodeSpec, Node, NodeCore, NodeSpec},
    },
};

pub struct ElementSpec {
    first_child: Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>>,
    last_child: Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>>,

    name: Rc<str>,
    local_name: Rc<str>,
    namespace: Option<Rc<RefCell<NodeCore<NamespaceSpec>>>>,

    attributes: AttributeMap,
    namespace_decl: NamespaceMap,
}

impl NodeSpec for ElementSpec {
    fn node_type(&self) -> NodeType {
        NodeType::Element
    }

    fn first_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>> {
        self.first_child.clone()
    }

    fn last_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>> {
        self.last_child.clone()
    }
}

impl InternalNodeSpec for ElementSpec {
    fn set_first_child(&mut self, new: Rc<RefCell<NodeCore<dyn NodeSpec>>>) {
        self.first_child = Some(new);
    }
    fn unset_first_child(&mut self) {
        self.first_child = None;
    }

    fn set_last_child(&mut self, new: Rc<RefCell<NodeCore<dyn NodeSpec>>>) {
        self.last_child = Some(new);
    }
    fn unset_last_child(&mut self) {
        self.last_child = None;
    }

    fn pre_child_insertion(
        &self,
        inserted_child: Node<dyn NodeSpec>,
        _preceding_node: Option<Node<dyn NodeSpec>>,
    ) -> Result<(), XMLTreeError> {
        match inserted_child.node_type() {
            NodeType::CDATASection
            | NodeType::Comment
            | NodeType::Element
            | NodeType::EntityReference
            | NodeType::ProcessingInstruction
            | NodeType::Text => Ok(()),
            _ => Err(XMLTreeError::UnacceptableHierarchy),
        }
    }
}

/// The internal or leaf node of the document tree that represents an element of the XML document.
///
/// It mostly covers the information provided by the "Element Information Item"
/// in the [XML Infoset](https://www.w3.org/TR/xml-infoset/).
///
/// # Reference
/// [2.2. Element Information Items](https://www.w3.org/TR/xml-infoset/#infoitem.element)
pub type Element = Node<ElementSpec>;

impl Element {
    pub(crate) fn new(
        qname: Rc<str>,
        namespace_name: Option<Rc<str>>,
        owner_document: Document,
    ) -> Result<Self, XMLTreeError> {
        let ret = Node::create_node(
            ElementSpec {
                first_child: None,
                last_child: None,
                name: qname.clone(),
                local_name: qname.clone(),
                namespace: None,
                attributes: AttributeMap::default(),
                namespace_decl: NamespaceMap::default(),
            },
            owner_document,
        );

        if let Some((prefix, local_name)) = qname.split_once(':')
            && !prefix.is_empty()
        {
            if prefix == "xmlns" {
                return Err(XMLTreeError::UnacceptablePrefix);
            }
            let Some(namespace_name) = namespace_name else {
                return Err(XMLTreeError::UnresolvablePrefix);
            };
            if (prefix == "xml" && namespace_name.as_ref() != XML_XML_NAMESPACE)
                || (prefix != "xml" && namespace_name.as_ref() == XML_XML_NAMESPACE)
            {
                return Err(XMLTreeError::UnacceptableNamespaceBinding);
            }
            if namespace_name.is_empty() {
                return Err(XMLTreeError::UnacceptableNamespaceName);
            }
            let mut namespace = Namespace::new(
                (!prefix.is_empty()).then(|| prefix.into()),
                namespace_name,
                ret.clone(),
            );
            namespace.as_implicit();
            ret.core.borrow_mut().spec.local_name = local_name.into();
            ret.core.borrow_mut().spec.namespace = Some(namespace.core);
        } else {
            if qname.starts_with(":") {
                return Err(XMLTreeError::EmptyPrefix);
            }

            if let Some(namespace_name) = namespace_name
                && !namespace_name.is_empty()
            {
                let mut namespace = Namespace::new(None, namespace_name, ret.clone());
                namespace.as_implicit();
                ret.core.borrow_mut().spec.namespace = Some(namespace.core);
            }
        }

        Ok(ret)
    }

    /// QName of this element.
    pub fn name(&self) -> Rc<str> {
        self.core.borrow().spec.name.clone()
    }

    /// LocalPart of the name of this element.
    pub fn local_name(&self) -> Rc<str> {
        self.core.borrow().spec.local_name.clone()
    }

    /// The namespace to which this element belongs if exists.
    pub(crate) fn namespace(&self) -> Option<Namespace> {
        self.core
            .borrow()
            .spec
            .namespace
            .clone()
            .map(|core| Namespace {
                core,
                owner_document: self.owner_document.clone(),
            })
    }

    /// Prefix of the name of this element if exists.
    pub fn prefix(&self) -> Option<Rc<str>> {
        self.namespace().and_then(|namespace| namespace.prefix())
    }

    /// The namespace name of the namespace to which this element belongs if exists.
    pub fn namespace_name(&self) -> Option<Rc<str>> {
        self.namespace().map(|namespace| namespace.namespace_name())
    }

    pub fn get_attribute_node(
        &self,
        local_name: &str,
        namespace_name: Option<&str>,
    ) -> Option<Attribute> {
        Some(Attribute {
            core: self
                .core
                .borrow()
                .spec
                .attributes
                .get(local_name, namespace_name)?,
            owner_document: self.owner_document.clone(),
        })
    }

    pub fn get_attribute(&self, local_name: &str, namespace_name: Option<&str>) -> Option<String> {
        self.get_attribute_node(local_name, namespace_name)
            .map(|attr| attr.value())
    }

    /// Set attributes for elements.
    ///
    /// When trying to set the namespace declaration attribute,
    /// it will be redirected to [`declare_namespace`](Element::declare_namespace).
    ///
    /// Specifying a QName with a prefix without specifying a namespace name is an error.  \
    /// Specifying a pair of prefix and namespace name that conflicts with an existing
    /// namespace declaration within this element is also an error.  \
    /// According to the namespace specification, attributes without a prefix are considered
    /// to belong to no namespace. Therefore, specifying a namespace name when specifying
    /// a name without a prefix is an error.
    pub fn set_attribute(
        &mut self,
        qname: &str,
        namespace_name: Option<&str>,
        value: Option<&str>,
    ) -> Result<(), XMLTreeError> {
        let mut attribute = if let Some((prefix, local_name)) = qname.split_once(':')
            && !prefix.is_empty()
        {
            if prefix == "xmlns" {
                if namespace_name != Some(XML_NS_NAMESPACE) || value.is_none() {
                    return Err(XMLTreeError::UnacceptableNamespaceBinding);
                }

                self.declare_namespace(Some(local_name), value.unwrap())?;
                return Ok(());
            }

            if let Some(namespace) = self
                .core
                .borrow()
                .spec
                .namespace_decl
                .get_by_prefix(Some(prefix))
                .map(|core| Namespace {
                    core,
                    owner_document: self.owner_document.clone(),
                })
            {
                Attribute::with_namespace(qname.into(), Some(namespace), self.clone())?
            } else {
                let ret = Attribute::new(
                    qname.into(),
                    namespace_name.map(|name| name.into()),
                    self.clone(),
                )?;

                // register implicit namespace declaration
                if let Some(namespace) = ret.namespace() {
                    self.core.borrow_mut().spec.namespace_decl.push(namespace)?;
                }

                ret
            }
        } else {
            if qname == "xmlns" {
                if namespace_name != Some(XML_NS_NAMESPACE) || value.is_none() {
                    return Err(XMLTreeError::UnacceptableNamespaceBinding);
                }

                self.declare_namespace(None, value.unwrap())?;
                return Ok(());
            }

            // According to the specification,
            // attributes without prefixes do not belong to a namespace,
            // so specifying a namespace for an attribute without a prefix is an error.
            if namespace_name.is_some() {
                return Err(XMLTreeError::UnacceptableNamespaceName);
            }

            Attribute::new(qname.into(), None, self.clone())?
        };
        if let Some(value) = value {
            let text = self.owner_document().create_text(value);
            attribute.append_child(text)?;
        }
        self.core
            .borrow_mut()
            .spec
            .attributes
            .push(attribute.clone())?;
        Ok(())
    }

    pub fn remove_attribute(
        &mut self,
        local_name: &str,
        namespace_name: Option<&str>,
    ) -> Option<String> {
        let attribute = self.get_attribute_node(local_name, namespace_name)?;
        self.remove_attribute_node(attribute.clone()).ok()?;
        Some(attribute.value())
    }

    pub fn remove_attribute_node(&mut self, attribute: Attribute) -> Result<(), XMLTreeError> {
        if attribute
            .owner_element()
            .is_none_or(|elem| !Rc::ptr_eq(&self.core, &elem.core))
        {
            return Err(XMLTreeError::UnspecifiedAttribute);
        }

        let prefix = attribute.prefix();
        let local_name = attribute.local_name();
        let namespace_name = attribute.namespace_name();
        if let Some(mut attribute) = self
            .core
            .borrow_mut()
            .spec
            .attributes
            .remove(&local_name, namespace_name.as_deref())
            .map(|core| Attribute {
                core,
                owner_document: self.owner_document.clone(),
            })
        {
            attribute.unset_owner_element();
        }
        // remove implicit namespace declaration if it is no longer used
        if let Some(namespace_name) = namespace_name
            && self
                .namespace()
                .is_some_and(|namespace| namespace.namespace_name() != namespace_name)
            && let core = self.core.borrow()
            && !core.spec.attributes.check_using_namespace(&namespace_name)
            && let Some(namespace) = core
                .spec
                .namespace_decl
                .get_by_prefix(prefix.as_deref())
                .map(|core| Namespace {
                    core,
                    owner_document: self.owner_document.clone(),
                })
            && namespace.is_implicit()
        {
            // prevent re-borrowing
            drop(core);
            self.core
                .borrow_mut()
                .spec
                .namespace_decl
                .remove(prefix.as_deref());
        }

        Ok(())
    }

    /// Check whether attributes matching both the local name and namespace name are specified.
    pub fn has_attribute(&self, local_name: &str, namespace_name: Option<&str>) -> bool {
        self.get_attribute_node(local_name, namespace_name)
            .is_some()
    }

    /// Check whether the `attribute` is specified for this element.
    ///
    /// # Note
    /// This method performs a stricter check than [`Element::has_attribute`].  \
    /// It returns `true` only if all values held by `attribute` match a certain attribute
    /// specified in this element, and only if that attribute and `attribute` are the same node.
    pub fn has_attribute_node(&self, attribute: Attribute) -> bool {
        self.get_attribute_node(
            &attribute.local_name(),
            attribute.namespace_name().as_deref(),
        )
        .is_some_and(|attr| attr.is_same_node(attribute))
    }

    /// Returns an iterator that scans attributes specified by this element.
    ///
    /// # Note
    /// Due to implementation limitations, it is not possible to prevent iterator invalidation.  \
    /// For example, by cloning an [`Element`] that has called [`Element::attributes`] and binding
    /// it to a mutable variable, you can modify attributes while retaining the iterator.  \
    /// Of course, the result of such an operation is undefined.
    pub fn attributes(&self) -> AttributeIter<'_> {
        AttributeIter {
            element: self.clone(),
            index: 0,
            _ref: PhantomData,
        }
    }

    /// Search for the prefix bound to the namespace name `namespace_name`.  \
    /// If it is not found, return [`None`].
    ///
    /// # Note
    /// A namespace may bind to more than one prefix.  \
    /// If multiple prefixes are bound to `namespace_name`, the first one found is returned.
    pub fn search_namespace(&self, namespace_name: &str) -> Option<Namespace> {
        let mut implicit = None::<Namespace>;
        let mut current = Some(Node::<dyn InternalNodeSpec>::from(self.clone()));
        while let Some(now) = current {
            if let Some(element) = now.as_element()
                && let Some(core) = element
                    .core
                    .borrow()
                    .spec
                    .namespace_decl
                    .get_by_namespace_name(namespace_name)
            {
                let ret = Namespace {
                    core,
                    owner_document: self.owner_document.clone(),
                };
                if let Some(implicit) = implicit.clone() {
                    if ret.prefix() != implicit.prefix() {
                        return Some(implicit);
                    } else if ret.is_explicit() {
                        return Some(ret);
                    } else {
                        // continue to search more explicit declaration
                    };
                } else {
                    if ret.is_explicit() {
                        return Some(ret);
                    }
                    implicit = Some(ret);
                }
            }
            current = now.parent_node();
        }
        implicit
    }

    /// Returns the namespace declaration on `self` or a nearest ancestor that binds `prefix`.  \
    /// If no such declaration exists, returns [`None`].
    ///
    /// # Note
    /// If `prefix` is bound to some namespace name, that namespace name is always unique.
    pub fn search_namespace_by_prefix(&self, prefix: Option<&str>) -> Option<Namespace> {
        let mut implicit = None::<Namespace>;
        let mut current = Some(Node::<dyn InternalNodeSpec>::from(self.clone()));
        while let Some(now) = current {
            if let Some(element) = now.as_element()
                && let Some(core) = element
                    .core
                    .borrow()
                    .spec
                    .namespace_decl
                    .get_by_prefix(prefix)
            {
                let ret = Namespace {
                    core,
                    owner_document: self.owner_document.clone(),
                };
                if let Some(implicit) = implicit.clone() {
                    if ret.namespace_name() != implicit.namespace_name() {
                        return Some(implicit);
                    } else if ret.is_explicit() {
                        return Some(ret);
                    } else {
                        // continue to search more explicit declaration
                    };
                } else {
                    if ret.is_explicit() {
                        return Some(ret);
                    }
                    implicit = Some(ret);
                }
            }
            current = now.parent_node();
        }
        implicit
    }

    /// Add a namespace declaration to this element.
    ///
    /// If the element specifies a prefix-namespace name pair that is prohibited by
    /// the namespace specification, or if the prefix is already bound to another namespace,
    /// an error is returned.
    pub fn declare_namespace(
        &mut self,
        prefix: Option<&str>,
        namespace_name: &str,
    ) -> Result<(), XMLTreeError> {
        if let Some(mut namespace) = self
            .core
            .borrow()
            .spec
            .namespace_decl
            .get_by_prefix(prefix)
            .map(|core| Namespace {
                core,
                owner_document: self.owner_document.clone(),
            })
        {
            // If it is already bound to a different namespace name, it is an error.
            if namespace.namespace_name().as_ref() == namespace_name {
                return Err(XMLTreeError::AlreadyBoundPrefix);
            }

            // otherwise, flags it as an explicit declaration.
            namespace.as_explicit();
            return Ok(());
        }
        if let Some(prefix) = prefix.filter(|prefix| !prefix.is_empty()) {
            if prefix == "xmlns" {
                return Err(XMLTreeError::UnacceptablePrefix);
            }
            if namespace_name == XML_NS_NAMESPACE
                || (prefix == "xml" && namespace_name != XML_XML_NAMESPACE)
                || (prefix != "xml" && namespace_name == XML_XML_NAMESPACE)
            {
                return Err(XMLTreeError::UnacceptableNamespaceBinding);
            }
            // In XML 1.0, unbinding of prefix using the empty namespace name is not allowed.
            if namespace_name.is_empty() {
                return Err(XMLTreeError::UnacceptableNamespaceName);
            }
        } else if namespace_name == XML_NS_NAMESPACE || namespace_name == XML_XML_NAMESPACE {
            return Err(XMLTreeError::UnacceptableNamespaceBinding);
        }

        let namespace = Namespace::new(
            prefix.map(|prefix| prefix.into()),
            namespace_name.into(),
            self.clone(),
        );
        self.core.borrow_mut().spec.namespace_decl.push(namespace)?;
        Ok(())
    }

    /// Remove explicit namespace declarations that bind `prefix`.
    ///
    /// If this element or attributes specified on this element have names that include
    /// `prefix` as a prefix, a declaration implicitly binding `prefix` is inserted.
    pub fn undeclare_namespace(&mut self, prefix: Option<&str>) {
        let Some(mut namespace) = self
            .core
            .borrow()
            .spec
            .namespace_decl
            .get_by_prefix(prefix)
            .map(|core| Namespace {
                core,
                owner_document: self.owner_document.clone(),
            })
        else {
            return;
        };
        if self.namespace_name() == Some(namespace.namespace_name())
            || self
                .core
                .borrow()
                .spec
                .attributes
                .check_using_namespace(&namespace.namespace_name())
        {
            namespace.as_implicit();
        } else if let Some(mut namespace) = self
            .core
            .borrow_mut()
            .spec
            .namespace_decl
            .remove(prefix)
            .map(|core| Namespace {
                core,
                owner_document: self.owner_document.clone(),
            })
        {
            namespace.unset_owner_element();
        }
    }

    /// Returns an iterator that scans namespace declarations on this element.
    ///
    /// Iterator elements do not contain implicit declarations.
    ///
    /// # Note
    /// Due to implementation limitations, it is not possible to prevent iterator invalidation.  \
    /// For example, by cloning an [`Element`] that has called [`Element::namespaces`] and binding
    /// it to a mutable variable, you can modify attributes while retaining the iterator.  \
    /// Of course, the result of such an operation is undefined.
    pub fn namespaces(&self) -> NamespaceIter<'_> {
        NamespaceIter {
            element: self.clone(),
            index: 0,
            _ref: PhantomData,
        }
    }

    /// If the element or any of its immediate ancestors has an `xml:space` attribute, it returns:
    ///
    /// - `Some(true)` if the value is "preserve"
    /// - `Some(false)` if the value is "default"
    /// - `None` for any other value
    ///
    /// If no such element exists, it returns `None`.
    ///
    /// # Reference
    /// [2.10 White Space Handling](https://www.w3.org/TR/xml/#sec-white-space)
    pub fn space_preserve(&self) -> Option<bool> {
        if let Some(attribute) = self.get_attribute_node("space", Some(XML_XML_NAMESPACE)) {
            let value = attribute.value();
            return if value == "preserve" {
                Some(true)
            } else if value == "default" {
                Some(false)
            } else {
                None
            };
        }

        let mut parent = self.parent_node();
        while let Some(now) = parent {
            parent = now.parent_node();
            if let Some(attribute) = now
                .as_element()
                .and_then(|element| element.get_attribute_node("space", Some(XML_XML_NAMESPACE)))
            {
                let value = attribute.value();
                return if value == "preserve" {
                    Some(true)
                } else if value == "default" {
                    Some(false)
                } else {
                    None
                };
            }
        }
        None
    }

    /// If any recent ancestor, including `self`, has an `xml:lang` attribute,
    /// return [`None`] if the attribute value is an empty string,
    /// otherwise return the attribute value wrapped in [`Some`].  \
    /// If no ancestor has an `xml:lang` attribute specified, return [`None`].
    ///
    /// # Reference
    /// [2.12 Language Identification](https://www.w3.org/TR/xml/#sec-lang-tag)
    pub fn language(&self) -> Option<String> {
        if let Some(attribute) = self.get_attribute_node("lang", Some(XML_XML_NAMESPACE)) {
            let value = attribute.value();
            return (!value.is_empty()).then_some(value);
        }

        let mut parent = self.parent_node();
        while let Some(now) = parent {
            parent = now.parent_node();
            if let Some(attribute) = now
                .as_element()
                .and_then(|element| element.get_attribute_node("lang", Some(XML_XML_NAMESPACE)))
            {
                let value = attribute.value();
                return (!value.is_empty()).then_some(value);
            }
        }
        None
    }

    /// Returns the descendant elements whose QName is `qname`.
    ///
    /// `self` is not included in the returned elements.
    pub fn get_elements_by_qname(&self, qname: &str) -> impl Iterator<Item = Element> {
        let mut children = self.first_child();
        let mut ret = vec![];
        while let Some(child) = children {
            if let Some(element) = child.as_element()
                && element.name().as_ref() == qname
            {
                ret.push(element);
            }
            if let Some(first) = child.first_child() {
                children = Some(first);
            } else if let Some(next) = child.next_sibling() {
                children = Some(next);
            } else {
                children = None;
                let mut parent = child.parent_node();
                while let Some(now) = parent {
                    if self.is_same_node(now.clone()) {
                        break;
                    }
                    if let Some(next) = now.next_sibling() {
                        children = Some(next);
                        break;
                    }
                    parent = now.parent_node();
                }
            }
        }
        ret.into_iter()
    }

    /// Returns the descendant elements whose expanded name is {`namespace_name`}`local_name`.
    ///
    /// `self` is not included in the returned elements.
    pub fn get_elements_by_expanded_name(
        &self,
        local_name: &str,
        namespace_name: Option<&str>,
    ) -> impl Iterator<Item = Element> {
        let mut children = self.first_child();
        let mut ret = vec![];
        while let Some(child) = children {
            if let Some(element) = child.as_element()
                && element.local_name().as_ref() == local_name
                && element.namespace_name().as_deref() == namespace_name
            {
                ret.push(element);
            }
            if let Some(first) = child.first_child() {
                children = Some(first);
            } else if let Some(next) = child.next_sibling() {
                children = Some(next);
            } else {
                children = None;
                let mut parent = child.parent_node();
                while let Some(now) = parent {
                    if self.is_same_node(now.clone()) {
                        break;
                    }
                    if let Some(next) = now.next_sibling() {
                        children = Some(next);
                        break;
                    }
                    parent = now.parent_node();
                }
            }
        }
        ret.into_iter()
    }
}

impl std::fmt::Display for Element {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{}", self.name())?;

        for namespace in self.namespaces() {
            if namespace.is_explicit() {
                write!(f, " {}", namespace)?;
            }
        }
        for attr in self.attributes() {
            if attr.is_specified() {
                write!(f, " {}", attr)?;
            }
        }

        let mut children = self.first_child();
        if children.is_none() {
            return write!(f, " />");
        }

        write!(f, ">")?;

        while let Some(child) = children {
            children = child.next_sibling();

            write!(f, "{}", child)?;
        }
        write!(f, "</{}>", self.name())
    }
}

#[derive(Default)]
struct AttributeMap {
    attributes: Vec<Rc<RefCell<NodeCore<AttributeSpec>>>>,
    // (namespace name, (local name, index))
    index: HashMap<Rc<str>, HashMap<Rc<str>, usize>>,
}

impl AttributeMap {
    fn get(
        &self,
        local_name: &str,
        namespace_name: Option<&str>,
    ) -> Option<Rc<RefCell<NodeCore<AttributeSpec>>>> {
        let &index = self
            .index
            .get(namespace_name.unwrap_or(""))?
            .get(local_name)?;
        self.attributes.get(index).cloned()
    }

    fn remove(
        &mut self,
        local_name: &str,
        namespace_name: Option<&str>,
    ) -> Option<Rc<RefCell<NodeCore<AttributeSpec>>>> {
        let map = self.index.get_mut(namespace_name.unwrap_or_default())?;
        let index = map.remove(local_name)?;
        if map.is_empty() {
            self.index.remove(namespace_name.unwrap_or_default());
        }
        let ret = self.attributes.remove(index);
        self.index
            .values_mut()
            .flat_map(|map| map.values_mut())
            .filter(|i| **i > index)
            .for_each(|index| *index -= 1);
        Some(ret)
    }

    fn check_using_namespace(&self, namespace_name: &str) -> bool {
        self.index.contains_key(namespace_name)
    }

    fn push(&mut self, attribute: Attribute) -> Result<(), XMLTreeError> {
        use std::collections::hash_map::Entry::*;

        let namespace_name = attribute.namespace_name().unwrap_or_default();
        let local_name = attribute.local_name();
        let index = self.attributes.len();
        match self.index.entry(namespace_name) {
            Vacant(entry) => {
                entry.insert(From::from([(local_name, index)]));
                self.attributes.push(attribute.core);
                Ok(())
            }
            Occupied(mut entry) => match entry.get_mut().entry(local_name) {
                Vacant(entry) => {
                    entry.insert(index);
                    self.attributes.push(attribute.core);
                    Ok(())
                }
                Occupied(_) => Err(XMLTreeError::DuplicateAttribute),
            },
        }
    }
}

pub struct AttributeIter<'a> {
    element: Element,
    index: usize,
    _ref: PhantomData<&'a Element>,
}

impl<'a> Iterator for AttributeIter<'a> {
    type Item = Attribute;

    fn next(&mut self) -> Option<Self::Item> {
        let map = &self.element.core.borrow().spec.attributes;
        if self.index >= map.attributes.len() {
            return None;
        }

        let ret = Attribute {
            core: map.attributes[self.index].clone(),
            owner_document: self.element.owner_document.clone(),
        };
        self.index += 1;
        Some(ret)
    }
}

#[derive(Default)]
struct NamespaceMap {
    data: Vec<Rc<RefCell<NodeCore<NamespaceSpec>>>>,
    // (namespace name, set of prefix)
    // Since a single namespace can bind multiple prefixes, prefixes are maintained as a set.
    namespace_name: HashMap<Rc<str>, BTreeSet<Rc<str>>>,
    prefix: HashMap<Rc<str>, usize>,
}

impl NamespaceMap {
    fn get_by_prefix(&self, prefix: Option<&str>) -> Option<Rc<RefCell<NodeCore<NamespaceSpec>>>> {
        let &index = self.prefix.get(prefix.unwrap_or(""))?;
        self.data.get(index).cloned()
    }

    fn get_by_namespace_name(
        &self,
        namespace_name: &str,
    ) -> Option<Rc<RefCell<NodeCore<NamespaceSpec>>>> {
        let prefix = self.namespace_name.get(namespace_name)?.first()?;
        self.get_by_prefix((!prefix.is_empty()).then_some(prefix))
    }

    fn remove(&mut self, prefix: Option<&str>) -> Option<Rc<RefCell<NodeCore<NamespaceSpec>>>> {
        use std::collections::hash_map::Entry::*;

        let prefix = prefix.unwrap_or_default();
        let index = self.prefix.remove(prefix)?;
        let namespace = self.data.remove(index);
        let namespace_name = namespace.borrow().spec.namespace_name.clone();
        if let Occupied(mut entry) = self.namespace_name.entry(namespace_name) {
            entry.get_mut().remove(prefix);
            if entry.get().is_empty() {
                entry.remove();
            }
        }
        for index in self.prefix.values_mut().filter(|i| **i > index) {
            *index -= 1;
        }
        Some(namespace)
    }

    fn push(&mut self, namespace: Namespace) -> Result<(), XMLTreeError> {
        let prefix = namespace.prefix().unwrap_or_default();
        let namespace_name = namespace.namespace_name();
        if let Some(index) = self.prefix.get(&prefix).copied() {
            return if self.data[index].borrow().spec.namespace_name != namespace.namespace_name() {
                Err(XMLTreeError::AlreadyBoundPrefix)
            } else {
                if namespace.is_explicit() {
                    self.data[index] = namespace.core;
                }
                Ok(())
            };
        }

        self.namespace_name
            .entry(namespace_name)
            .or_default()
            .insert(prefix.clone());
        self.prefix.insert(prefix, self.data.len());
        self.data.push(namespace.core);

        Ok(())
    }
}

pub struct NamespaceIter<'a> {
    element: Element,
    index: usize,
    _ref: PhantomData<&'a Element>,
}

impl<'a> Iterator for NamespaceIter<'a> {
    type Item = Namespace;

    fn next(&mut self) -> Option<Self::Item> {
        let map = &self.element.core.borrow().spec.namespace_decl;
        if self.index >= map.data.len() {
            return None;
        }

        while self.index < map.data.len() {
            let ret = Namespace {
                core: map.data[self.index].clone(),
                owner_document: self.element.owner_document.clone(),
            };
            self.index += 1;
            if ret.is_explicit() {
                return Some(ret);
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use crate::{sax::parser::XMLReaderBuilder, tree::TreeBuildHandler};

    #[test]
    fn get_language_tests() {
        let mut parser = XMLReaderBuilder::new()
            .set_handler(TreeBuildHandler::default())
            .build();

        const CASE: &str = r#"<root>
            <child xml:lang="ja">
                <child2 />
                <child3 xml:lang="ch" />
            </child>
            <child4 xml:lang="en">
                <child5 xml:lang="">
                    <child6 />
                    <child7 xml:lang="fr" />
                </child5>
            </child4>
        </root>"#;

        parser.parse_str(CASE, None).unwrap();
        assert!(!parser.handler.fatal_error);
        let document = parser.handler.document;
        let mut children = document.first_child();
        while let Some(child) = children {
            if let Some(element) = child.as_element() {
                let lang = element.language();
                match element.name().as_ref() {
                    "root" => assert_eq!(lang.as_deref(), None),
                    "child" => assert_eq!(lang.as_deref(), Some("ja")),
                    "child2" => assert_eq!(lang.as_deref(), Some("ja")),
                    "child3" => assert_eq!(lang.as_deref(), Some("ch")),
                    "child4" => assert_eq!(lang.as_deref(), Some("en")),
                    "child5" => assert_eq!(lang.as_deref(), None),
                    "child6" => assert_eq!(lang.as_deref(), None),
                    "child7" => assert_eq!(lang.as_deref(), Some("fr")),
                    _ => unreachable!(),
                }
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
    }
}
