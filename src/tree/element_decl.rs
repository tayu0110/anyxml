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
    name: Box<str>,
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
    pub(crate) fn new(name: Box<str>, content_spec: ContentSpec, owner_document: Document) -> Self {
        Node::create_node(ElementDeclSpec { name, content_spec }, owner_document)
    }

    pub fn name(&self) -> Ref<'_, str> {
        Ref::map(self.core.borrow(), |core| core.spec.name.as_ref())
    }

    pub fn content_spec(&self) -> Ref<'_, ContentSpec> {
        Ref::map(self.core.borrow(), |core| &core.spec.content_spec)
    }
}
