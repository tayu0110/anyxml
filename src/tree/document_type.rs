use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    save::write_quoted,
    tree::{
        AttlistDecl, Document, ElementDecl, EntityDecl, NodeType, NotationDecl, XMLTreeError,
        attlist_decl::AttlistDeclSpec,
        convert::NodeKind,
        element_decl::ElementDeclSpec,
        entity_decl::EntityDeclSpec,
        node::{InternalNodeSpec, Node, NodeCore, NodeSpec},
        notation_decl::NotationDeclSpec,
    },
    uri::URIStr,
};

pub struct DocumentTypeSpec {
    first_child: Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>>,
    last_child: Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>>,

    /// (element name, node pointer)
    element_decl: HashMap<Rc<str>, Rc<RefCell<NodeCore<ElementDeclSpec>>>>,
    /// (element name, (attribute name, node pointer))
    attlist_decl: HashMap<Rc<str>, HashMap<Rc<str>, Rc<RefCell<NodeCore<AttlistDeclSpec>>>>>,
    /// (entity name, node pointer)
    entity_decl: HashMap<Rc<str>, Rc<RefCell<NodeCore<EntityDeclSpec>>>>,
    /// (notation name, node pointer)
    notation_decl: HashMap<Rc<str>, Rc<RefCell<NodeCore<NotationDeclSpec>>>>,

    name: Rc<str>,
    system_id: Option<Rc<URIStr>>,
    public_id: Option<Rc<str>>,
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
                self.element_decl.remove(&*elementdecl.name());
            }
            NodeKind::AttlistDecl(attlistdecl) => {
                // Attribute list declarations may be duplicated,
                // so duplicate declarations must be reinserted appropriately.
                let registered = self
                    .attlist_decl
                    .get_mut(&*attlistdecl.elem_name())
                    .unwrap()
                    .get_mut(&*attlistdecl.attr_name())
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
                        .get_mut(&*attlistdecl.elem_name())
                        .unwrap();
                    map.remove(&*attlistdecl.attr_name());
                    if map.is_empty() {
                        self.attlist_decl.remove(&*attlistdecl.elem_name());
                    }
                }
            }
            NodeKind::EntityDecl(entity) => {
                // Entity declarations may be also duplicate.
                let registered = self.entity_decl.get_mut(&*entity.name()).unwrap();
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
                    self.entity_decl.remove(&*entity.name());
                }
            }
            NodeKind::NotationDecl(notation) => {
                // Notation declarations are also unique.
                self.notation_decl.remove(&*notation.name());
            }
            _ => {}
        }
        Ok(())
    }

    fn pre_child_insertion(
        &self,
        inserted_child: Node<dyn NodeSpec>,
        _preceding_node: Option<Node<dyn NodeSpec>>,
    ) -> Result<(), XMLTreeError> {
        match inserted_child.downcast() {
            NodeKind::ElementDecl(element) => {
                if self.element_decl.contains_key(&element.name()) {
                    return Err(XMLTreeError::DuplicateElementDecl);
                }
            }
            NodeKind::NotationDecl(notation) => {
                if self.notation_decl.contains_key(&notation.name()) {
                    return Err(XMLTreeError::DuplicateNotationDecl);
                }
            }
            NodeKind::AttlistDecl(_)
            | NodeKind::EntityDecl(_)
            | NodeKind::Comment(_)
            | NodeKind::ProcessingInstruction(_) => {}
            NodeKind::EntityReference(_) => {
                // TODO: Supports parameter entities
                return Err(XMLTreeError::Unsupported);
            }
            _ => return Err(XMLTreeError::UnacceptableHierarchy),
        }
        Ok(())
    }

    fn post_child_insertion(&mut self, inserted_child: Node<dyn NodeSpec>) {
        use std::collections::hash_map::Entry::*;

        match inserted_child.downcast() {
            NodeKind::AttlistDecl(attlist) => match self.attlist_decl.entry(attlist.elem_name()) {
                Vacant(entry) => {
                    entry.insert(From::from([(attlist.attr_name(), attlist.core.clone())]));
                }
                Occupied(mut entry) => match entry.get_mut().entry((*attlist.attr_name()).into()) {
                    Vacant(entry) => {
                        entry.insert(attlist.core.clone());
                    }
                    Occupied(mut entry) => {
                        let core = entry.get();
                        let mut preceding_node = attlist.previous_sibling();
                        while let Some(now) = preceding_node {
                            preceding_node = now.previous_sibling();
                            if let Some(now) = now.as_attlist_decl()
                                && Rc::ptr_eq(&now.core, core)
                            {
                                return;
                            }
                        }
                        entry.insert(attlist.core.clone());
                    }
                },
            },
            NodeKind::ElementDecl(element) => {
                self.element_decl.insert(element.name(), element.core);
            }
            NodeKind::EntityDecl(entity) => match self.entity_decl.entry(entity.name()) {
                Vacant(entry) => {
                    entry.insert(entity.core.clone());
                }
                Occupied(mut entry) => {
                    let core = entry.get();
                    let mut preceding_node = entity.previous_sibling();
                    while let Some(now) = preceding_node {
                        preceding_node = now.previous_sibling();
                        if let Some(now) = now.as_entity_decl()
                            && Rc::ptr_eq(&now.core, core)
                        {
                            return;
                        }
                    }
                    entry.insert(entity.core.clone());
                }
            },
            NodeKind::NotationDecl(notation) => {
                self.notation_decl.insert(notation.name(), notation.core);
            }
            _ => {}
        }
    }
}

pub type DocumentType = Node<DocumentTypeSpec>;

impl DocumentType {
    pub(crate) fn new(
        name: Rc<str>,
        system_id: Option<Rc<URIStr>>,
        public_id: Option<Rc<str>>,
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

    pub fn name(&self) -> Rc<str> {
        self.core.borrow().spec.name.clone()
    }

    pub fn system_id(&self) -> Option<Rc<URIStr>> {
        self.core.borrow().spec.system_id.clone()
    }

    pub fn public_id(&self) -> Option<Rc<str>> {
        self.core.borrow().spec.public_id.clone()
    }

    /// Create new node and copy internal data to the new node other than pointers to neighbor
    /// nodes and declarations.
    ///
    /// While [`Clone::clone`] merely copies the pointer, this method copies the internal data
    /// to new memory, creating a completely different node. Comparing the source node and
    /// the new node using [`Node::is_same_node`] will always return `false`.
    pub fn deep_copy(&self) -> Self {
        Node::create_node(
            DocumentTypeSpec {
                first_child: None,
                last_child: None,
                element_decl: Default::default(),
                attlist_decl: Default::default(),
                entity_decl: Default::default(),
                notation_decl: Default::default(),
                name: self.name(),
                system_id: self.system_id(),
                public_id: self.public_id(),
            },
            self.owner_document(),
        )
    }

    /// Perform a deep copy on all descendant nodes and construct a tree with the same structure.
    ///
    /// The link to the parent is not preserved.
    pub fn deep_copy_subtree(&self) -> Result<Self, XMLTreeError> {
        let mut ret = self.deep_copy();
        let mut children = self.first_child();
        while let Some(child) = children {
            children = child.next_sibling();
            ret.append_child(child.deep_copy_subtree()?)?;
        }
        Ok(ret)
    }
}

impl std::fmt::Display for DocumentType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<!DOCTYPE {}", self.name())?;
        if let Some(public_id) = self.public_id() {
            write!(f, " PUBLIC ")?;
            write_quoted(f, &public_id, false)?;
            if let Some(system_id) = self.system_id() {
                write!(f, " ")?;
                if let Some(system_id) = system_id.as_unescaped_str() {
                    write_quoted(f, &system_id, false)?;
                } else {
                    write_quoted(f, system_id.as_escaped_str(), false)?;
                }
            }
        } else if let Some(system_id) = self.system_id() {
            write!(f, " SYSTEM ")?;
            if let Some(system_id) = system_id.as_unescaped_str() {
                write_quoted(f, &system_id, false)?;
            } else {
                write_quoted(f, system_id.as_escaped_str(), false)?;
            }
        }

        if self.first_child().is_some() {
            writeln!(f, " [")?;
            let mut children = self.first_child();
            while let Some(now) = children {
                children = now.next_sibling();

                writeln!(f, "{}", now)?;
            }
            write!(f, "]")?;
        }

        write!(f, ">")?;
        Ok(())
    }
}
