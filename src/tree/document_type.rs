use std::{
    cell::{Ref, RefCell},
    collections::HashMap,
    rc::Rc,
};

use anyxml_uri::uri::URIStr;

use crate::tree::{
    AttlistDecl, Document, ElementDecl, EntityDecl, NodeType, NotationDecl, XMLTreeError,
    attlist_decl::AttlistDeclSpec,
    convert::NodeKind,
    element_decl::ElementDeclSpec,
    entity_decl::EntityDeclSpec,
    node::{InternalNodeSpec, Node, NodeCore, NodeSpec},
    notation_decl::NotationDeclSpec,
};

pub struct DocumentTypeSpec {
    first_child: Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>>,
    last_child: Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>>,

    /// (element name, node pointer)
    element_decl: HashMap<Box<str>, Rc<RefCell<NodeCore<ElementDeclSpec>>>>,
    /// (element name, (attribute name, node pointer))
    attlist_decl: HashMap<Box<str>, HashMap<Box<str>, Rc<RefCell<NodeCore<AttlistDeclSpec>>>>>,
    /// (entity name, node pointer)
    entity_decl: HashMap<Box<str>, Rc<RefCell<NodeCore<EntityDeclSpec>>>>,
    /// (notation name, node pointer)
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

    fn pre_child_removal(&mut self, removed_child: Node<dyn NodeSpec>) -> Result<(), XMLTreeError> {
        match removed_child.downcast() {
            NodeKind::ElementDecl(elementdecl) => {
                // According to the XML specification, element type declarations are unique,
                // so we can simply remove them.
                self.element_decl.remove::<str>(elementdecl.name().as_ref());
            }
            NodeKind::AttlistDecl(attlistdecl) => {
                // Attribute list declarations may be duplicated,
                // so duplicate declarations must be reinserted appropriately.
                let registered = self
                    .attlist_decl
                    .get_mut::<str>(attlistdecl.elem_name().as_ref())
                    .unwrap()
                    .get_mut::<str>(attlistdecl.attr_name().as_ref())
                    .unwrap();
                if Rc::ptr_eq(registered, &attlistdecl.core) {
                    let mut next = attlistdecl.next_sibling();
                    while let Some(now) = next {
                        if let Some(att) = now.as_attlist_decl()
                            && *attlistdecl.elem_name() == *att.elem_name()
                            && *attlistdecl.attr_name() == *att.attr_name()
                        {
                            *registered = att.core;
                            return Ok(());
                        }
                        next = now.next_sibling();
                    }
                    // not found
                    let map = self
                        .attlist_decl
                        .get_mut::<str>(attlistdecl.elem_name().as_ref())
                        .unwrap();
                    map.remove::<str>(attlistdecl.attr_name().as_ref());
                    if map.is_empty() {
                        self.attlist_decl
                            .remove::<str>(attlistdecl.elem_name().as_ref());
                    }
                }
            }
            NodeKind::EntityDecl(entity) => {
                // Entity declarations may be also duplicate.
                let registered = self
                    .entity_decl
                    .get_mut::<str>(entity.name().as_ref())
                    .unwrap();
                if Rc::ptr_eq(registered, &entity.core) {
                    let mut next = entity.next_sibling();
                    while let Some(now) = next {
                        if let Some(ent) = now.as_entity_decl()
                            && *entity.name() == *ent.name()
                        {
                            *registered = ent.core;
                            return Ok(());
                        }
                        next = now.next_sibling();
                    }
                    // not found
                    self.entity_decl.remove::<str>(entity.name().as_ref());
                }
            }
            NodeKind::NotationDecl(notation) => {
                // Notation declarations are also unique.
                self.notation_decl.remove::<str>(notation.name().as_ref());
            }
            _ => {}
        }
        Ok(())
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
