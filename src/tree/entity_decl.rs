use std::{
    cell::{Ref, RefCell},
    rc::Rc,
};

use anyxml_uri::uri::URIStr;

use crate::tree::{
    Document, NodeType, XMLTreeError,
    node::{InternalNodeSpec, Node, NodeCore, NodeSpec},
};

pub struct EntityDeclSpec {
    first_child: Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>>,
    last_child: Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>>,

    name: Box<str>,
    system_id: Option<Box<URIStr>>,
    public_id: Option<Box<str>>,
    notation_name: Option<Box<str>>,
    value: Option<Box<str>>,
}

impl NodeSpec for EntityDeclSpec {
    fn node_type(&self) -> NodeType {
        NodeType::EntityDecl
    }

    fn first_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>> {
        self.first_child.clone()
    }

    fn last_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>> {
        self.last_child.clone()
    }
}

impl InternalNodeSpec for EntityDeclSpec {
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

    fn pre_child_insertion(
        &mut self,
        inserted_child: Node<dyn NodeSpec>,
        _preceding_node: Option<Node<dyn NodeSpec>>,
    ) -> Result<(), super::XMLTreeError> {
        match inserted_child.node_type() {
            NodeType::CDATASection
            | NodeType::Comment
            | NodeType::Element
            | NodeType::EntityReference
            | NodeType::ProcessingInstruction
            | NodeType::Text => Ok(()),
            _ => Err(XMLTreeError::UnacceptableHierarchy),
        }
    }
}

pub type EntityDecl = Node<EntityDeclSpec>;

impl EntityDecl {
    pub(crate) fn new_internal_entity_decl(
        name: Box<str>,
        value: Box<str>,
        owner_document: Document,
    ) -> Self {
        Node::create_node(
            EntityDeclSpec {
                first_child: None,
                last_child: None,
                name,
                system_id: None,
                public_id: None,
                notation_name: None,
                value: Some(value),
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
            EntityDeclSpec {
                first_child: None,
                last_child: None,
                name,
                system_id: Some(system_id),
                public_id,
                notation_name: None,
                value: None,
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
            EntityDeclSpec {
                first_child: None,
                last_child: None,
                name,
                system_id: Some(system_id),
                public_id,
                notation_name: Some(notation_name),
                value: None,
            },
            owner_document,
        )
    }

    pub fn name(&self) -> Ref<'_, str> {
        Ref::map(self.core.borrow(), |core| core.spec.name.as_ref())
    }

    pub fn system_id(&self) -> Option<Ref<'_, URIStr>> {
        Ref::filter_map(self.core.borrow(), |core| core.spec.system_id.as_deref()).ok()
    }

    pub fn public_id(&self) -> Option<Ref<'_, str>> {
        Ref::filter_map(self.core.borrow(), |core| core.spec.public_id.as_deref()).ok()
    }

    pub fn notation_name(&self) -> Option<Ref<'_, str>> {
        Ref::filter_map(self.core.borrow(), |core| {
            core.spec.notation_name.as_deref()
        })
        .ok()
    }

    pub fn value(&self) -> Option<Ref<'_, str>> {
        Ref::filter_map(self.core.borrow(), |core| core.spec.value.as_deref()).ok()
    }
}
