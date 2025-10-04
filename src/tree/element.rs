use std::{
    cell::RefCell,
    collections::{BTreeSet, HashMap},
    rc::Rc,
};

use crate::{
    XML_NS_NAMESPACE, XML_XML_NAMESPACE,
    tree::{
        Attribute, Document, NodeType, XMLTreeError,
        attribute::AttributeSpec,
        namespace::{Namespace, NamespaceSpec},
        node::{GeneralInternalNodeSpec, InternalNodeSpec, InternalNodeType, Node, NodeCore},
    },
};

pub struct ElementSpecificData {
    name: Rc<str>,
    local_name: Rc<str>,
    namespace: Option<Rc<RefCell<NodeCore<NamespaceSpec>>>>,

    attributes: AttributeMap,
    namespace_decl: NamespaceMap,
}

impl InternalNodeType for ElementSpecificData {
    fn node_type(&self) -> NodeType {
        NodeType::Element
    }
}

pub type ElementSpec = GeneralInternalNodeSpec<ElementSpecificData>;
pub type Element = Node<ElementSpec>;

impl Element {
    pub(crate) fn new(
        qname: Rc<str>,
        namespace_name: Option<Rc<str>>,
        owner_document: Document,
    ) -> Result<Self, XMLTreeError> {
        let ret = Node::create_node(
            GeneralInternalNodeSpec::new(ElementSpecificData {
                name: qname.clone(),
                local_name: qname.clone(),
                namespace: None,
                attributes: AttributeMap::default(),
                namespace_decl: NamespaceMap::default(),
            }),
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
            ret.core.borrow_mut().spec.data.local_name = local_name.into();
            ret.core.borrow_mut().spec.data.namespace = Some(namespace.core);
        } else {
            if qname.starts_with(":") {
                return Err(XMLTreeError::EmptyPrefix);
            }

            if let Some(namespace_name) = namespace_name
                && !namespace_name.is_empty()
            {
                let mut namespace = Namespace::new(None, namespace_name, ret.clone());
                namespace.as_implicit();
                ret.core.borrow_mut().spec.data.namespace = Some(namespace.core);
            }
        }

        Ok(ret)
    }

    pub fn name(&self) -> Rc<str> {
        self.core.borrow().spec.data.name.clone()
    }

    pub fn local_name(&self) -> Rc<str> {
        self.core.borrow().spec.data.local_name.clone()
    }

    pub(crate) fn namespace(&self) -> Option<Namespace> {
        self.core
            .borrow()
            .spec
            .data
            .namespace
            .clone()
            .map(|core| Namespace {
                core,
                owner_document: self.owner_document.clone(),
            })
    }

    pub fn prefix(&self) -> Option<Rc<str>> {
        self.namespace().and_then(|namespace| namespace.prefix())
    }

    pub fn namespace_name(&self) -> Option<Rc<str>> {
        self.namespace().map(|namespace| namespace.namespace_name())
    }

    pub fn get_attribute(
        &self,
        local_name: &str,
        namespace_name: Option<&str>,
    ) -> Option<Attribute> {
        Some(Attribute {
            core: self
                .core
                .borrow()
                .spec
                .data
                .attributes
                .get(local_name, namespace_name)?,
            owner_document: self.owner_document.clone(),
        })
    }

    pub fn set_attribute(
        &mut self,
        qname: &str,
        namespace_name: Option<&str>,
        value: Option<&str>,
    ) -> Result<Attribute, XMLTreeError> {
        let mut attribute = Attribute::new(
            qname.into(),
            namespace_name.map(|name| name.into()),
            self.clone(),
        )?;
        if let Some(value) = value {
            let text = self.owner_document().create_text(value);
            attribute.append_child(text.into());
        }
        self.core
            .borrow_mut()
            .spec
            .data
            .attributes
            .push(attribute.clone())?;
        Ok(attribute)
    }

    /// Search for the prefix bound to the namespace name `namespace_name`.  \
    /// If it is not found, return `None`.
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
                    .data
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

    pub fn search_namespace_by_prefix(&self, prefix: Option<&str>) -> Option<Namespace> {
        let mut implicit = None::<Namespace>;
        let mut current = Some(Node::<dyn InternalNodeSpec>::from(self.clone()));
        while let Some(now) = current {
            if let Some(element) = now.as_element()
                && let Some(core) = element
                    .core
                    .borrow()
                    .spec
                    .data
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
            .data
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
        self.core
            .borrow_mut()
            .spec
            .data
            .namespace_decl
            .push(namespace)?;
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
            .data
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
                .data
                .attributes
                .check_using_namespace(&namespace.namespace_name())
        {
            namespace.as_implicit();
        } else {
            self.core
                .borrow_mut()
                .spec
                .data
                .namespace_decl
                .remove(prefix);
        }
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

#[derive(Default)]
struct NamespaceMap {
    // (namespace name, set of prefix)
    // Since a single namespace can bind multiple prefixes, prefixes are maintained as a set.
    namespace_name: HashMap<Rc<str>, BTreeSet<Rc<str>>>,
    prefix: HashMap<Rc<str>, Rc<RefCell<NodeCore<NamespaceSpec>>>>,
}

impl NamespaceMap {
    fn get_by_prefix(&self, prefix: Option<&str>) -> Option<Rc<RefCell<NodeCore<NamespaceSpec>>>> {
        self.prefix.get(prefix.unwrap_or("")).cloned()
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
        let namespace = self.prefix.remove(prefix)?;
        let namespace_name = namespace.borrow().spec.namespace_name.clone();
        if let Occupied(mut entry) = self.namespace_name.entry(namespace_name) {
            entry.get_mut().remove(prefix);
            if entry.get().is_empty() {
                entry.remove();
            }
        }
        Some(namespace)
    }

    fn push(&mut self, namespace: Namespace) -> Result<(), XMLTreeError> {
        let prefix = namespace.prefix().unwrap_or_default();
        let namespace_name = namespace.namespace_name();
        if let Some(core) = self.prefix.get(&prefix) {
            return if core.borrow().spec.namespace_name != namespace.namespace_name() {
                Err(XMLTreeError::AlreadyBoundPrefix)
            } else {
                if namespace.is_explicit() {
                    self.prefix.insert(prefix, namespace.core);
                }
                Ok(())
            };
        }

        self.namespace_name
            .entry(namespace_name)
            .or_default()
            .insert(prefix.clone());
        self.prefix.insert(prefix, namespace.core.clone());

        Ok(())
    }
}
