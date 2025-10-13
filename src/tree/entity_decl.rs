use std::{cell::RefCell, rc::Rc};

use crate::{
    save::write_quoted,
    tree::{
        Document, NodeType, XMLTreeError,
        node::{InternalNodeSpec, Node, NodeCore, NodeSpec},
    },
    uri::URIStr,
};

pub struct EntityDeclSpec {
    first_child: Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>>,
    last_child: Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>>,

    name: Rc<str>,
    system_id: Option<Rc<URIStr>>,
    public_id: Option<Rc<str>>,
    notation_name: Option<Rc<str>>,
    value: Option<Rc<str>>,
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
        &self,
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
        name: Rc<str>,
        value: Rc<str>,
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
        name: Rc<str>,
        system_id: Rc<URIStr>,
        public_id: Option<Rc<str>>,
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
        name: Rc<str>,
        system_id: Rc<URIStr>,
        public_id: Option<Rc<str>>,
        notation_name: Rc<str>,
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

    pub fn name(&self) -> Rc<str> {
        self.core.borrow().spec.name.clone()
    }

    pub fn system_id(&self) -> Option<Rc<URIStr>> {
        self.core.borrow().spec.system_id.clone()
    }

    pub fn public_id(&self) -> Option<Rc<str>> {
        self.core.borrow().spec.public_id.clone()
    }

    pub fn notation_name(&self) -> Option<Rc<str>> {
        self.core.borrow().spec.notation_name.clone()
    }

    pub fn value(&self) -> Option<Rc<str>> {
        self.core.borrow().spec.value.clone()
    }
}

impl std::fmt::Display for EntityDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = self.name();
        if let Some(name) = name.strip_prefix('%') {
            write!(f, "<!ENTITY % {} ", name)?;
        } else {
            write!(f, "<!ENTITY {} ", name)?;
        }

        if let Some(value) = self.value() {
            write_quoted(f, &value, false)?;
        } else {
            if let Some(public_id) = self.public_id() {
                write!(f, "PUBLIC ")?;
                write_quoted(f, &public_id, false)?;
                if let Some(system_id) = self.system_id() {
                    write!(f, " ")?;
                    if let Some(system_id) = system_id.as_unescaped_str() {
                        write_quoted(f, &system_id, false)?;
                    } else {
                        write_quoted(f, system_id.as_escaped_str(), false)?;
                    }
                } else {
                    unreachable!()
                }
            } else if let Some(system_id) = self.system_id() {
                write!(f, "SYSTEM ")?;
                if let Some(system_id) = system_id.as_unescaped_str() {
                    write_quoted(f, &system_id, false)?;
                } else {
                    write_quoted(f, system_id.as_escaped_str(), false)?;
                }
            } else {
                unreachable!()
            }

            if let Some(notation_name) = self.notation_name() {
                write!(f, " NDATA {}", notation_name)?;
            }
        }
        write!(f, ">")
    }
}
