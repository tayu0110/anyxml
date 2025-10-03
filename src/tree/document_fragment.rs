use std::{cell::RefCell, rc::Rc};

use crate::tree::{
    Document, NodeType,
    node::{GeneralInternalNodeSpec, InternalNodeType, Node, NodeCore},
};

pub struct DocumentFragmentSpecificData {}

impl InternalNodeType for DocumentFragmentSpecificData {
    fn node_type(&self) -> super::NodeType {
        NodeType::DocumentFragment
    }
}

pub type DocumentFragmentSpec = GeneralInternalNodeSpec<DocumentFragmentSpecificData>;
pub type DocumentFragment = Node<DocumentFragmentSpec>;

impl DocumentFragment {
    pub(crate) fn new(owner_document: Document) -> Self {
        Self {
            core: Rc::new(RefCell::new(NodeCore {
                parent_node: owner_document.core.borrow().parent_node.clone(),
                previous_sibling: owner_document.core.borrow().previous_sibling.clone(),
                next_sibling: None,
                spec: GeneralInternalNodeSpec {
                    first_child: None,
                    last_child: None,
                    data: DocumentFragmentSpecificData {},
                },
            })),
            owner_document: owner_document.core.clone(),
        }
    }
}
