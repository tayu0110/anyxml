use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

use crate::{
    XML_NS_NAMESPACE, XML_XML_NAMESPACE,
    save::write_escaped_att_value,
    sax::AttributeType,
    tree::{
        Element, NodeType, XMLTreeError,
        convert::NodeKind,
        document_fragment::DocumentFragmentSpec,
        element::ElementSpec,
        namespace::{Namespace, NamespaceSpec},
        node::{InternalNodeSpec, Node, NodeCore, NodeSpec},
    },
};

pub struct AttributeSpec {
    first_child: Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>>,
    last_child: Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>>,

    owner_element: Weak<RefCell<NodeCore<ElementSpec>>>,

    name: Rc<str>,
    local_name: Rc<str>,
    namespace: Option<Rc<RefCell<NodeCore<NamespaceSpec>>>>,

    specified: bool,
}

impl NodeSpec for AttributeSpec {
    fn node_type(&self) -> NodeType {
        NodeType::Attribute
    }

    fn first_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>> {
        self.first_child.clone()
    }

    fn last_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>> {
        self.last_child.clone()
    }
}

impl InternalNodeSpec for AttributeSpec {
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
        match inserted_child.downcast() {
            NodeKind::Text(_) => {}
            NodeKind::EntityReference(entref) => {
                let mut descendant = entref.first_child();
                while let Some(now) = descendant {
                    if !matches!(now.node_type(), NodeType::EntityReference | NodeType::Text) {
                        return Err(XMLTreeError::UnacceptableHierarchy);
                    }
                    if let Some(first) = now.first_child() {
                        descendant = Some(first);
                    } else if let Some(next) = now.next_sibling() {
                        descendant = Some(next);
                    } else {
                        descendant = now.parent_node().map(From::from);
                        while let Some(par) = descendant.as_ref() {
                            if let Some(next) = par.next_sibling() {
                                descendant = Some(next);
                                break;
                            }
                            descendant = par.parent_node().map(From::from);
                        }
                    }
                }
            }
            _ => return Err(XMLTreeError::UnacceptableHierarchy),
        }
        Ok(())
    }
}

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
                spec: AttributeSpec {
                    first_child: None,
                    last_child: None,
                    owner_element: elem,
                    name: qname.clone(),
                    local_name: qname.clone(),
                    namespace: None,
                    specified: true,
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
            ret.core.borrow_mut().spec.local_name = local_name.into();
            ret.core.borrow_mut().spec.namespace = Some(namespace.core);
        } else {
            if qname.starts_with(":") {
                return Err(XMLTreeError::EmptyPrefix);
            }

            if let Some(namespace_name) = namespace_name
                && !namespace_name.is_empty()
            {
                let mut namespace = Namespace::new(None, namespace_name, owner_element);
                namespace.as_implicit();
                ret.core.borrow_mut().spec.namespace = Some(namespace.core);
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
                    spec: AttributeSpec {
                        first_child: None,
                        last_child: None,
                        owner_element: elem,
                        name: qname.clone(),
                        local_name: local_name.into(),
                        namespace: Some(namespace.core),
                        specified: true,
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
                    spec: AttributeSpec {
                        first_child: None,
                        last_child: None,
                        owner_element: elem,
                        name: qname.clone(),
                        local_name: qname.clone(),
                        namespace: None,
                        specified: true,
                    },
                })),
                owner_document: owner_element.owner_document().core,
            })
        }
    }

    pub fn owner_element(&self) -> Option<Element> {
        Some(Element {
            core: self.core.borrow().spec.owner_element.upgrade()?,
            owner_document: self.owner_document.clone(),
        })
    }

    pub(crate) fn unset_owner_element(&mut self) {
        let weak = Weak::new();
        self.core.borrow_mut().spec.owner_element = weak.clone();
        self.core.borrow_mut().parent_node = weak;
    }

    pub fn name(&self) -> Rc<str> {
        self.core.borrow().spec.name.clone()
    }

    pub fn local_name(&self) -> Rc<str> {
        self.core.borrow().spec.local_name.clone()
    }

    pub fn namespace(&self) -> Option<Namespace> {
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

    pub fn is_specified(&self) -> bool {
        self.core.borrow().spec.specified
    }

    pub(crate) fn set_specified(&mut self) {
        self.core.borrow_mut().spec.specified = true;
    }

    pub(crate) fn unset_specified(&mut self) {
        self.core.borrow_mut().spec.specified = false;
    }

    pub fn is_id(&self) -> bool {
        self.owner_document()
            .document_type()
            .and_then(|doctype| {
                doctype.get_attlist_decl(&self.owner_element()?.name(), &self.name())
            })
            .is_some_and(|attlist| matches!(*attlist.attr_type(), AttributeType::ID))
    }
}

impl std::fmt::Display for Attribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}=\"", self.name())?;

        if let Some(child) = self.first_child() {
            let mut children = Some(child);
            while let Some(child) = children {
                children = child.next_sibling();

                match child.downcast() {
                    NodeKind::EntityReference(entref) => {
                        write!(f, "{}", entref)?;
                    }
                    NodeKind::Text(text) => {
                        write_escaped_att_value(f, &text.data(), false, &mut Some('"'))?;
                    }
                    _ => unreachable!(),
                }
            }
        }
        write!(f, "\"")
    }
}
