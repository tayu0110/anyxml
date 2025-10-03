use std::cell::Ref;

use anyxml_uri::uri::URIStr;

use crate::tree::{
    NodeType,
    node::{GeneralInternalNodeSpec, InternalNodeType, Node},
};

pub struct EntityDeclSpecificData {
    name: Box<str>,
    system_id: Option<Box<URIStr>>,
    public_id: Option<Box<str>>,
    notation_name: Option<Box<str>>,
}

impl InternalNodeType for EntityDeclSpecificData {
    fn node_type(&self) -> NodeType {
        NodeType::EntityDecl
    }
}

pub type EntityDeclSpec = GeneralInternalNodeSpec<EntityDeclSpecificData>;
pub type EntityDecl = Node<EntityDeclSpec>;

impl EntityDecl {
    pub fn name(&self) -> Ref<'_, str> {
        Ref::map(self.core.borrow(), |core| {
            core.spec.data.name.as_ref()
        })
    }

    pub fn system_id(&self) -> Option<Ref<'_, URIStr>> {
        Ref::filter_map(self.core.borrow(), |core| {
            core.spec.data.system_id.as_deref()
        })
        .ok()
    }

    pub fn public_id(&self) -> Option<Ref<'_, str>> {
        Ref::filter_map(self.core.borrow(), |core| {
            core.spec.data.public_id.as_deref()
        })
        .ok()
    }

    pub fn notation_name(&self) -> Option<Ref<'_, str>> {
        Ref::filter_map(self.core.borrow(), |core| {
            core.spec.data.notation_name.as_deref()
        })
        .ok()
    }
}
