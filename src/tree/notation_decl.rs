use std::{cell::RefCell, rc::Rc};

use crate::{
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
