use std::{
    cell::{Ref, RefCell},
    rc::Rc,
};

use anyxml_uri::uri::URIStr;

use crate::tree::{
    Document, NodeType,
    node::{Node, NodeCore, NodeSpec},
};

pub struct NotationDeclSpec {
    name: Rc<str>,
    system_id: Option<Box<URIStr>>,
    public_id: Option<Box<str>>,
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
        system_id: Option<Box<URIStr>>,
        public_id: Option<Box<str>>,
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

    pub fn system_id(&self) -> Option<Ref<'_, URIStr>> {
        Ref::filter_map(self.core.borrow(), |core| core.spec.system_id.as_deref()).ok()
    }

    pub fn public_id(&self) -> Option<Ref<'_, str>> {
        Ref::filter_map(self.core.borrow(), |core| core.spec.public_id.as_deref()).ok()
    }
}
