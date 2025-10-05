use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

use crate::{
    XML_NS_NAMESPACE, XML_XML_NAMESPACE,
    tree::{
        Element, NodeType, XMLTreeError,
        document_fragment::DocumentFragmentSpec,
        element::ElementSpec,
        namespace::{Namespace, NamespaceSpec},
        node::{GeneralInternalNodeSpec, InternalNodeType, Node, NodeCore},
    },
};

pub struct AttributeSpecificData {
    owner_element: Weak<RefCell<NodeCore<ElementSpec>>>,

    name: Rc<str>,
    local_name: Rc<str>,
    namespace: Option<Rc<RefCell<NodeCore<NamespaceSpec>>>>,
}

impl InternalNodeType for AttributeSpecificData {
    fn node_type(&self) -> NodeType {
        NodeType::Attribute
    }
}

pub type AttributeSpec = GeneralInternalNodeSpec<AttributeSpecificData>;
pub type Attribute = Node<AttributeSpec>;

impl Attribute {
    pub(crate) fn new(
        qname: Rc<str>,
        namespace_name: Option<Rc<str>>,
        owner_element: Element,
    ) -> Result<Self, XMLTreeError> {
        let elem = Rc::downgrade(&owner_element.core);
        let prev: Weak<RefCell<NodeCore<DocumentFragmentSpec>>> = Weak::new();
        let ret = Attribute {
            core: Rc::new(RefCell::new(NodeCore {
                parent_node: elem.clone(),
                previous_sibling: prev,
                next_sibling: None,
                spec: GeneralInternalNodeSpec {
                    first_child: None,
                    last_child: None,
                    data: AttributeSpecificData {
                        owner_element: elem,
                        name: qname.clone(),
                        local_name: qname.clone(),
                        namespace: None,
                    },
                },
            })),
            owner_document: owner_element.owner_document().core,
        };

        if qname.as_ref() == "xmlns" && namespace_name.as_deref() != Some(XML_NS_NAMESPACE) {
            return Err(XMLTreeError::UnacceptableNamespaceBinding);
        }

        if let Some((prefix, local_name)) = qname.split_once(':')
            && !prefix.is_empty()
        {
            let Some(namespace_name) = namespace_name else {
                return Err(XMLTreeError::UnresolvablePrefix);
            };
            if ((prefix == "xml") != (namespace_name.as_ref() == XML_XML_NAMESPACE))
                || ((prefix == "xmlns") != (namespace_name.as_ref() == XML_NS_NAMESPACE))
            {
                return Err(XMLTreeError::UnacceptableNamespaceBinding);
            }
            if namespace_name.is_empty() {
                return Err(XMLTreeError::UnacceptableNamespaceName);
            }
            let mut namespace = Namespace::new(
                (!prefix.is_empty()).then(|| prefix.into()),
                namespace_name,
                owner_element,
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
                let mut namespace = Namespace::new(None, namespace_name, owner_element);
                namespace.as_implicit();
                ret.core.borrow_mut().spec.data.namespace = Some(namespace.core);
            }
        }

        Ok(ret)
    }

    pub(crate) fn with_namespace(
        qname: Rc<str>,
        namespace: Option<Namespace>,
        owner_element: Element,
    ) -> Result<Self, XMLTreeError> {
        let elem = Rc::downgrade(&owner_element.core);
        let prev: Weak<RefCell<NodeCore<DocumentFragmentSpec>>> = Weak::new();
        if let Some((prefix, local_name)) = qname.split_once(':')
            && !prefix.is_empty()
        {
            let Some(namespace) = namespace else {
                return Err(XMLTreeError::UnresolvablePrefix);
            };
            if namespace.prefix().is_none_or(|pre| pre.as_ref() != prefix) {
                return Err(XMLTreeError::UnresolvablePrefix);
            }
            Ok(Attribute {
                core: Rc::new(RefCell::new(NodeCore {
                    parent_node: elem.clone(),
                    previous_sibling: prev,
                    next_sibling: None,
                    spec: GeneralInternalNodeSpec {
                        first_child: None,
                        last_child: None,
                        data: AttributeSpecificData {
                            owner_element: elem,
                            name: qname.clone(),
                            local_name: local_name.into(),
                            namespace: Some(namespace.core),
                        },
                    },
                })),
                owner_document: owner_element.owner_document().core,
            })
        } else {
            if qname.starts_with(':') {
                return Err(XMLTreeError::EmptyPrefix);
            }

            // According to the specification,
            // attributes without prefixes do not belong to a namespace,
            // so specifying a namespace for an attribute without a prefix is an error.
            if namespace.is_some() {
                return Err(XMLTreeError::UnacceptableNamespaceName);
            }

            Ok(Attribute {
                core: Rc::new(RefCell::new(NodeCore {
                    parent_node: elem.clone(),
                    previous_sibling: prev,
                    next_sibling: None,
                    spec: GeneralInternalNodeSpec {
                        first_child: None,
                        last_child: None,
                        data: AttributeSpecificData {
                            owner_element: elem,
                            name: qname.clone(),
                            local_name: qname.clone(),
                            namespace: None,
                        },
                    },
                })),
                owner_document: owner_element.owner_document().core,
            })
        }
    }

    pub fn owner_element(&self) -> Option<Element> {
        Some(Element {
            core: self.core.borrow().spec.data.owner_element.upgrade()?,
            owner_document: self.owner_document.clone(),
        })
    }

    pub(crate) fn unset_owner_element(&mut self) {
        let weak = Weak::new();
        self.core.borrow_mut().spec.data.owner_element = weak.clone();
        self.core.borrow_mut().parent_node = weak;
    }

    pub fn name(&self) -> Rc<str> {
        self.core.borrow().spec.data.name.clone()
    }

    pub fn local_name(&self) -> Rc<str> {
        self.core.borrow().spec.data.local_name.clone()
    }

    pub fn namespace(&self) -> Option<Namespace> {
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

    pub fn value(&self) -> String {
        let mut buf = String::new();
        let mut children = self.first_child();
        let mut depth = 1;
        while depth > 0
            && let Some(mut child) = children
        {
            if let Some(text) = child.as_text() {
                buf.push_str(&text.data());
                children = text.next_sibling();
            } else if let Some(first_child) = child.first_child() {
                children = Some(first_child);
                depth += 1;
            } else if let Some(next_sibling) = child.next_sibling() {
                children = Some(next_sibling);
            } else {
                children = Some(child.clone());
                while depth > 0
                    && let Some(parent) = child.parent_node()
                {
                    depth -= 1;
                    if let Some(next_sibling) = parent.next_sibling() {
                        children = Some(next_sibling);
                        break;
                    }
                    child = parent.into();
                }
            }
        }

        buf
    }
}
