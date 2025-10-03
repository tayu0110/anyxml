use std::{
    cell::{Ref, RefCell},
    rc::Rc,
};

use crate::tree::{
    NodeType,
    node::{Node, NodeCore, NodeSpec},
};

pub struct ProcessingInstructionSpec {
    target: Box<str>,
    data: Box<str>,
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
    pub fn target(&self) -> Ref<'_, str> {
        Ref::map(self.core.borrow(), |core| {
            core.spec.target.as_ref()
        })
    }

    pub fn data(&self) -> Ref<'_, str> {
        Ref::map(self.core.borrow(), |core| core.spec.data.as_ref())
    }
}
