use std::{
    cell::{Ref, RefCell},
    rc::Rc,
};

use crate::tree::{
    Document, NodeType,
    node::{Node, NodeCore, NodeSpec},
};

pub struct CDATASectionSpec {
    data: String,
}

impl NodeSpec for CDATASectionSpec {
    fn node_type(&self) -> NodeType {
        NodeType::CDATASection
    }

    fn first_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>> {
        None
    }

    fn last_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>> {
        None
    }
}

pub type CDATASection = Node<CDATASectionSpec>;

impl CDATASection {
    pub(crate) fn new(data: String, owner_document: Document) -> Self {
        Node::create_node(CDATASectionSpec { data }, owner_document)
    }

    pub fn data(&self) -> Ref<'_, str> {
        Ref::map(self.core.borrow(), |core| core.spec.data.as_str())
    }

    pub fn push(&mut self, ch: char) {
        self.core.borrow_mut().spec.data.push(ch);
    }

    pub fn push_str(&mut self, string: &str) {
        self.core.borrow_mut().spec.data.push_str(string);
    }
}
