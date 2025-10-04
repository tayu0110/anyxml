use std::cell::Ref;

use crate::tree::{
    Document, NodeType,
    node::{GeneralInternalNodeSpec, InternalNodeType, Node},
};

pub struct EntityReferenceSpecificData {
    name: Box<str>,
}

impl InternalNodeType for EntityReferenceSpecificData {
    fn node_type(&self) -> super::NodeType {
        NodeType::EntityReference
    }
}

pub type EntityReferenceSpec = GeneralInternalNodeSpec<EntityReferenceSpecificData>;
pub type EntityReference = Node<EntityReferenceSpec>;

impl EntityReference {
    pub(crate) fn new(name: Box<str>, owner_document: Document) -> Self {
        Node::create_node(
            GeneralInternalNodeSpec {
                first_child: None,
                last_child: None,
                data: EntityReferenceSpecificData { name },
            },
            owner_document,
        )
    }

    pub fn name(&self) -> Ref<'_, str> {
        Ref::map(self.core.borrow(), |core| core.spec.data.name.as_ref())
    }
}
