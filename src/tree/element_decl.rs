use std::{
    cell::{Ref, RefCell},
    rc::Rc,
};

use crate::{
    sax::contentspec::ContentSpec,
    tree::{
        Document, NodeType,
        node::{Node, NodeCore, NodeSpec},
    },
};

pub struct ElementDeclSpec {
    name: Rc<str>,
    content_spec: ContentSpec,
}

impl NodeSpec for ElementDeclSpec {
    fn node_type(&self) -> NodeType {
        NodeType::ElementDecl
    }

    fn first_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>> {
        None
    }

    fn last_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>> {
        None
    }
}

pub type ElementDecl = Node<ElementDeclSpec>;

impl ElementDecl {
    pub(crate) fn new(name: Rc<str>, content_spec: ContentSpec, owner_document: Document) -> Self {
        Node::create_node(ElementDeclSpec { name, content_spec }, owner_document)
    }

    /// Element name declared by this declaration.
    pub fn name(&self) -> Rc<str> {
        self.core.borrow().spec.name.clone()
    }

    /// ContentSpec of declaration target element.
    pub fn content_spec(&self) -> Ref<'_, ContentSpec> {
        Ref::map(self.core.borrow(), |core| &core.spec.content_spec)
    }

    /// Create new node and copy internal data to the new node.
    ///
    /// While [`Clone::clone`] merely copies the pointer, this method copies the internal data
    /// to new memory, creating a completely different node. Comparing the source node and
    /// the new node using [`Node::is_same_node`] will always return `false`.
    pub fn deep_copy(&self) -> Self {
        Node::create_node(
            ElementDeclSpec {
                name: self.name(),
                content_spec: self.content_spec().clone(),
            },
            self.owner_document(),
        )
    }
}

impl std::fmt::Display for ElementDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<!ELEMENT {} {}>", self.name(), self.content_spec())
    }
}
