use std::{cell::RefCell, rc::Rc};

use crate::tree::{
    Document, NodeType,
    node::{Node, NodeCore, NodeSpec},
};

pub struct ProcessingInstructionSpec {
    target: Rc<str>,
    data: Option<Rc<str>>,
}

impl NodeSpec for ProcessingInstructionSpec {
    fn node_type(&self) -> NodeType {
        NodeType::ProcessingInstruction
    }

    fn first_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>> {
        None
    }

    fn last_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>> {
        None
    }
}

pub type ProcessingInstruction = Node<ProcessingInstructionSpec>;

impl ProcessingInstruction {
    pub(crate) fn new(target: Rc<str>, data: Option<Rc<str>>, owner_document: Document) -> Self {
        Node::create_node(ProcessingInstructionSpec { target, data }, owner_document)
    }

    pub fn target(&self) -> Rc<str> {
        self.core.borrow().spec.target.clone()
    }

    pub fn data(&self) -> Option<Rc<str>> {
        self.core.borrow().spec.data.clone()
    }
}
