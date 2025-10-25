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

/// The leaf node of the document tree that represents a processing instruction of the XML document.
///
/// It mostly covers the information provided by the "Processing Instruction Information Item"
/// in the [XML Infoset](https://www.w3.org/TR/xml-infoset/).
///
/// # Reference
/// [2.4. Processing Instruction Information Items](https://www.w3.org/TR/xml-infoset/#infoitem.pi)
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

impl std::fmt::Display for ProcessingInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<?{}", self.target())?;
        if let Some(data) = self.data() {
            write!(f, " {}", data)?;
        }
        write!(f, "?>")
    }
}
