use std::{
    cell::{Ref, RefCell},
    rc::Rc,
};

use crate::tree::{
    Document, NodeType,
    node::{InternalNodeSpec, Node, NodeCore, NodeSpec},
};

pub struct EntityReferenceSpec {
    first_child: Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>>,
    last_child: Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>>,

    name: Box<str>,
}

impl NodeSpec for EntityReferenceSpec {
    fn node_type(&self) -> super::NodeType {
        NodeType::EntityReference
    }

    fn first_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>> {
        self.first_child.clone()
    }

    fn last_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>> {
        self.last_child.clone()
    }
}

impl InternalNodeSpec for EntityReferenceSpec {
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

pub type EntityReference = Node<EntityReferenceSpec>;

impl EntityReference {
    pub(crate) fn new(name: Box<str>, owner_document: Document) -> Self {
        Node::create_node(
            EntityReferenceSpec {
                first_child: None,
                last_child: None,
                name,
            },
            owner_document,
        )
    }

    pub fn name(&self) -> Ref<'_, str> {
        Ref::map(self.core.borrow(), |core| core.spec.name.as_ref())
    }
}
