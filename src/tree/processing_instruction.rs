use std::{
    cell::{Ref, RefCell},
    rc::Rc,
};

use crate::tree::{
    Document, NodeType,
    node::{Node, NodeCore, NodeSpec},
};

pub struct ProcessingInstructionSpec {
    target: Box<str>,
    data: Option<Box<str>>,
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
    pub(crate) fn new(target: Box<str>, data: Option<Box<str>>, owner_document: Document) -> Self {
        Node::create_node(ProcessingInstructionSpec { target, data }, owner_document)
    }

    pub fn target(&self) -> Ref<'_, str> {
        Ref::map(self.core.borrow(), |core| core.spec.target.as_ref())
    }

    pub fn data(&self) -> Option<Ref<'_, str>> {
        Ref::filter_map(self.core.borrow(), |core| core.spec.data.as_deref()).ok()
    }
}
