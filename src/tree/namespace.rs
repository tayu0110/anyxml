use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

use crate::{
    save::write_escaped_att_value,
    tree::{
        Element, NodeType,
        document_fragment::DocumentFragmentSpec,
        element::ElementSpec,
        node::{Node, NodeCore, NodeSpec},
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DeclareType {
    Implicit,
    Explicit,
}

pub struct NamespaceSpec {
    owner_element: Weak<RefCell<NodeCore<ElementSpec>>>,
    pub(super) prefix: Option<Rc<str>>,
    pub(super) namespace_name: Rc<str>,

    declare_type: DeclareType,
}

impl NodeSpec for NamespaceSpec {
    fn node_type(&self) -> NodeType {
        NodeType::Namespace
    }

    fn first_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>> {
        None
    }

    fn last_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>> {
        None
    }
}

pub type Namespace = Node<NamespaceSpec>;

impl Namespace {
    pub(crate) fn new(
        prefix: Option<Rc<str>>,
        namespace_name: Rc<str>,
        owner_element: Element,
    ) -> Namespace {
        let elem = Rc::downgrade(&owner_element.core);
        let prev: Weak<RefCell<NodeCore<DocumentFragmentSpec>>> = Weak::new();
        Namespace {
            core: Rc::new(RefCell::new(NodeCore {
                parent_node: elem.clone(),
                previous_sibling: prev,
                next_sibling: None,
                spec: NamespaceSpec {
                    owner_element: elem,
                    prefix,
                    namespace_name,
                    declare_type: DeclareType::Explicit,
                },
            })),
            owner_document: owner_element.owner_document.clone(),
        }
    }

    /// Returns elements with this namespace declaration attribute specified.
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

    pub fn prefix(&self) -> Option<Rc<str>> {
        self.core.borrow().spec.prefix.clone()
    }

    pub fn namespace_name(&self) -> Rc<str> {
        self.core.borrow().spec.namespace_name.clone()
    }

    pub(crate) fn is_explicit(&self) -> bool {
        matches!(self.core.borrow().spec.declare_type, DeclareType::Explicit)
    }

    pub(crate) fn as_explicit(&mut self) {
        self.core.borrow_mut().spec.declare_type = DeclareType::Explicit;
    }

    pub(crate) fn is_implicit(&self) -> bool {
        matches!(self.core.borrow().spec.declare_type, DeclareType::Implicit)
    }

    pub(crate) fn as_implicit(&mut self) {
        self.core.borrow_mut().spec.declare_type = DeclareType::Implicit
    }
}

impl std::fmt::Display for Namespace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_implicit() {
            return Ok(());
        }

        if let Some(prefix) = self.prefix() {
            write!(f, "xmlns:{}=", prefix)?;
        } else {
            write!(f, "xmlns=")?;
        }
        write_escaped_att_value(f, &self.namespace_name(), true, &mut None)
    }
}
