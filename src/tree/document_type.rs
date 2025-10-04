use std::{
    cell::{Ref, RefCell},
    collections::HashMap,
    rc::Rc,
};

use anyxml_uri::uri::URIStr;

use crate::tree::{
    AttlistDecl, Document, ElementDecl, EntityDecl, NodeType, NotationDecl,
    attlist_decl::AttlistDeclSpec,
    element_decl::ElementDeclSpec,
    entity_decl::EntityDeclSpec,
    node::{GeneralInternalNodeSpec, InternalNodeType, Node, NodeCore},
    notation_decl::NotationDeclSpec,
};

pub struct DocumentTypeSpecificData {
    element_decl: HashMap<Box<str>, Rc<RefCell<NodeCore<ElementDeclSpec>>>>,
    attlist_decl: HashMap<Box<str>, HashMap<Box<str>, Rc<RefCell<NodeCore<AttlistDeclSpec>>>>>,
    entity_decl: HashMap<Box<str>, Rc<RefCell<NodeCore<EntityDeclSpec>>>>,
    notation_decl: HashMap<Box<str>, Rc<RefCell<NodeCore<NotationDeclSpec>>>>,

    name: Box<str>,
    system_id: Option<Box<URIStr>>,
    public_id: Option<Box<str>>,
}

impl InternalNodeType for DocumentTypeSpecificData {
    fn node_type(&self) -> super::NodeType {
        NodeType::DocumentType
    }
}

pub type DocumentTypeSpec = GeneralInternalNodeSpec<DocumentTypeSpecificData>;
pub type DocumentType = Node<DocumentTypeSpec>;

impl DocumentType {
    pub(crate) fn new(
        name: Box<str>,
        system_id: Option<Box<URIStr>>,
        public_id: Option<Box<str>>,
        owner_document: Document,
    ) -> Self {
        Node::create_node(
            GeneralInternalNodeSpec {
                first_child: None,
                last_child: None,
                data: DocumentTypeSpecificData {
                    element_decl: Default::default(),
                    attlist_decl: Default::default(),
                    entity_decl: Default::default(),
                    notation_decl: Default::default(),
                    name,
                    system_id,
                    public_id,
                },
            },
            owner_document,
        )
    }

    pub fn get_element_decl(&self, name: &str) -> Option<ElementDecl> {
        self.core
            .borrow()
            .spec
            .data
            .element_decl
            .get(name)
            .map(|core| ElementDecl {
                core: core.clone(),
                owner_document: self.owner_document.clone(),
            })
    }

    pub fn get_attlist_decl(&self, elem_name: &str, attr_name: &str) -> Option<AttlistDecl> {
        self.core
            .borrow()
            .spec
            .data
            .attlist_decl
            .get(elem_name)?
            .get(attr_name)
            .map(|core| AttlistDecl {
                core: core.clone(),
                owner_document: self.owner_document.clone(),
            })
    }

    pub fn get_entity_decl(&self, name: &str) -> Option<EntityDecl> {
        self.core
            .borrow()
            .spec
            .data
            .entity_decl
            .get(name)
            .map(|core| EntityDecl {
                core: core.clone(),
                owner_document: self.owner_document.clone(),
            })
    }

    pub fn get_notation_decl(&self, name: &str) -> Option<NotationDecl> {
        self.core
            .borrow()
            .spec
            .data
            .notation_decl
            .get(name)
            .map(|core| NotationDecl {
                core: core.clone(),
                owner_document: self.owner_document.clone(),
            })
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
}
