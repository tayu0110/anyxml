use std::{cell::RefCell, rc::Rc};

use crate::tree::{
    Document, NodeType,
    node::{InternalNodeSpec, Node, NodeCore, NodeSpec},
};

pub struct DocumentFragmentSpec {
    first_child: Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>>,
    last_child: Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>>,
}

impl NodeSpec for DocumentFragmentSpec {
    fn node_type(&self) -> NodeType {
        NodeType::DocumentFragment
    }

    fn first_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>> {
        self.first_child.clone()
    }

    fn last_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>> {
        self.last_child.clone()
    }
}

impl InternalNodeSpec for DocumentFragmentSpec {
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
}

pub type DocumentFragment = Node<DocumentFragmentSpec>;

impl DocumentFragment {
    pub(crate) fn new(owner_document: Document) -> Self {
        Self {
            core: Rc::new(RefCell::new(NodeCore {
                parent_node: owner_document.core.borrow().parent_node.clone(),
                previous_sibling: owner_document.core.borrow().previous_sibling.clone(),
                next_sibling: None,
                spec: DocumentFragmentSpec {
                    first_child: None,
                    last_child: None,
                },
            })),
            owner_document: owner_document.core.clone(),
        }
    }
}
