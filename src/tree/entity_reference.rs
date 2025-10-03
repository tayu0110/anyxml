use crate::tree::{
    NodeType,
    node::{GeneralInternalNodeSpec, InternalNodeType, Node},
};

pub struct EntityReferenceSpecificData {}

impl InternalNodeType for EntityReferenceSpecificData {
    fn node_type(&self) -> super::NodeType {
        NodeType::EntityReference
    }
}

pub type EntityReferenceSpec = GeneralInternalNodeSpec<EntityReferenceSpecificData>;
pub type EntityReference = Node<EntityReferenceSpec>;
