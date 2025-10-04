use std::cell::Ref;

use anyxml_uri::uri::URIStr;

use crate::tree::{
    Document, NodeType,
    node::{GeneralInternalNodeSpec, InternalNodeType, Node},
};

pub struct EntityDeclSpecificData {
    name: Box<str>,
    system_id: Option<Box<URIStr>>,
    public_id: Option<Box<str>>,
    notation_name: Option<Box<str>>,
    value: Option<Box<str>>,
}

impl InternalNodeType for EntityDeclSpecificData {
    fn node_type(&self) -> NodeType {
        NodeType::EntityDecl
    }
}

pub type EntityDeclSpec = GeneralInternalNodeSpec<EntityDeclSpecificData>;
pub type EntityDecl = Node<EntityDeclSpec>;

impl EntityDecl {
    pub(crate) fn new_internal_entity_decl(
        name: Box<str>,
        value: Box<str>,
        owner_document: Document,
    ) -> Self {
        Node::create_node(
            GeneralInternalNodeSpec {
                first_child: None,
                last_child: None,
                data: EntityDeclSpecificData {
                    name,
                    system_id: None,
                    public_id: None,
                    notation_name: None,
                    value: Some(value),
                },
            },
            owner_document,
        )
    }

    pub(crate) fn new_external_entity_decl(
        name: Box<str>,
        system_id: Box<URIStr>,
        public_id: Option<Box<str>>,
        owner_document: Document,
    ) -> Self {
        Node::create_node(
            GeneralInternalNodeSpec {
                first_child: None,
                last_child: None,
                data: EntityDeclSpecificData {
                    name,
                    system_id: Some(system_id),
                    public_id,
                    notation_name: None,
                    value: None,
                },
            },
            owner_document,
        )
    }

    pub(crate) fn new_unparsed_entity_decl(
        name: Box<str>,
        system_id: Box<URIStr>,
        public_id: Option<Box<str>>,
        notation_name: Box<str>,
        owner_document: Document,
    ) -> Self {
        Node::create_node(
            GeneralInternalNodeSpec {
                first_child: None,
                last_child: None,
                data: EntityDeclSpecificData {
                    name,
                    system_id: Some(system_id),
                    public_id,
                    notation_name: Some(notation_name),
                    value: None,
                },
            },
            owner_document,
        )
    }

    pub fn name(&self) -> Ref<'_, str> {
        Ref::map(self.core.borrow(), |core| core.spec.data.name.as_ref())
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

    pub fn value(&self) -> Option<Ref<'_, str>> {
        Ref::filter_map(self.core.borrow(), |core| core.spec.data.value.as_deref()).ok()
    }
}
