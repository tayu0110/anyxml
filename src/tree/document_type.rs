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
    node::{InternalNodeSpec, Node, NodeCore, NodeSpec},
    notation_decl::NotationDeclSpec,
};

pub struct DocumentTypeSpec {
    first_child: Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>>,
    last_child: Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>>,

    element_decl: HashMap<Box<str>, Rc<RefCell<NodeCore<ElementDeclSpec>>>>,
    attlist_decl: HashMap<Box<str>, HashMap<Box<str>, Rc<RefCell<NodeCore<AttlistDeclSpec>>>>>,
    entity_decl: HashMap<Box<str>, Rc<RefCell<NodeCore<EntityDeclSpec>>>>,
    notation_decl: HashMap<Box<str>, Rc<RefCell<NodeCore<NotationDeclSpec>>>>,

    name: Box<str>,
    system_id: Option<Box<URIStr>>,
    public_id: Option<Box<str>>,
}

impl NodeSpec for DocumentTypeSpec {
    fn node_type(&self) -> NodeType {
        NodeType::DocumentType
    }

    fn first_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>> {
        self.first_child.clone()
    }

    fn last_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>> {
        self.last_child.clone()
    }
}

impl InternalNodeSpec for DocumentTypeSpec {
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

pub type DocumentType = Node<DocumentTypeSpec>;

impl DocumentType {
    pub(crate) fn new(
        name: Box<str>,
        system_id: Option<Box<URIStr>>,
        public_id: Option<Box<str>>,
        owner_document: Document,
    ) -> Self {
        Node::create_node(
            DocumentTypeSpec {
                first_child: None,
                last_child: None,
                element_decl: Default::default(),
                attlist_decl: Default::default(),
                entity_decl: Default::default(),
                notation_decl: Default::default(),
                name,
                system_id,
                public_id,
            },
            owner_document,
        )
    }

    pub fn get_element_decl(&self, name: &str) -> Option<ElementDecl> {
        self.core
            .borrow()
            .spec
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
            .notation_decl
            .get(name)
            .map(|core| NotationDecl {
                core: core.clone(),
                owner_document: self.owner_document.clone(),
            })
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
}
