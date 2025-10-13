use std::{cell::RefCell, rc::Rc};

use crate::{
    save::write_quoted,
    tree::{
        Document, NodeType,
        node::{Node, NodeCore, NodeSpec},
    },
    uri::URIStr,
};

pub struct NotationDeclSpec {
    name: Rc<str>,
    system_id: Option<Rc<URIStr>>,
    public_id: Option<Rc<str>>,
}

impl NodeSpec for NotationDeclSpec {
    fn node_type(&self) -> NodeType {
        NodeType::NotationDecl
    }

    fn first_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>> {
        None
    }

    fn last_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>> {
        None
    }
}

pub type NotationDecl = Node<NotationDeclSpec>;

impl NotationDecl {
    pub(crate) fn new(
        name: Rc<str>,
        system_id: Option<Rc<URIStr>>,
        public_id: Option<Rc<str>>,
        owner_document: Document,
    ) -> Self {
        Node::create_node(
            NotationDeclSpec {
                name,
                system_id,
                public_id,
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
}

impl std::fmt::Display for NotationDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<!NOTATION {}", self.name())?;
        if let Some(public_id) = self.public_id() {
            write!(f, " PUBLIC ")?;
            write_quoted(f, &public_id, false)?;
            if let Some(system_id) = self.system_id() {
                if let Some(system_id) = system_id.as_unescaped_str() {
                    write!(f, " ")?;
                    write_quoted(f, &system_id, false)?;
                } else {
                    write!(f, " ")?;
                    write_quoted(f, system_id.as_escaped_str(), false)?;
                }
            }
        } else if let Some(system_id) = self.system_id() {
            if let Some(system_id) = system_id.as_unescaped_str() {
                write!(f, " SYSTEM ")?;
                write_quoted(f, &system_id, false)?;
            } else {
                write!(f, " SYSTEM ")?;
                write_quoted(f, system_id.as_escaped_str(), false)?;
            }
        }
        write!(f, ">")
    }
}
