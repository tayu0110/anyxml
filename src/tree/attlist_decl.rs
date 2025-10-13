use std::{
    cell::{Ref, RefCell},
    rc::Rc,
};

use crate::{
    sax::{AttributeType, DefaultDecl},
    tree::{
        Document, NodeType,
        node::{Node, NodeCore, NodeSpec},
    },
};

pub struct AttlistDeclSpec {
    elem_name: Rc<str>,
    attr_name: Rc<str>,
    attr_type: AttributeType,
    default_decl: DefaultDecl,
}

impl NodeSpec for AttlistDeclSpec {
    fn node_type(&self) -> NodeType {
        NodeType::AttlistDecl
    }

    fn first_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>> {
        None
    }

    fn last_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>> {
        None
    }
}

pub type AttlistDecl = Node<AttlistDeclSpec>;

impl AttlistDecl {
    pub(crate) fn new(
        elem_name: Rc<str>,
        attr_name: Rc<str>,
        attr_type: AttributeType,
        default_decl: DefaultDecl,
        owner_document: Document,
    ) -> Self {
        Node::create_node(
            AttlistDeclSpec {
                elem_name,
                attr_name,
                attr_type,
                default_decl,
            },
            owner_document,
        )
    }

    pub fn elem_name(&self) -> Rc<str> {
        self.core.borrow().spec.elem_name.clone()
    }

    pub fn attr_name(&self) -> Rc<str> {
        self.core.borrow().spec.attr_name.clone()
    }

    pub fn attr_type(&self) -> Ref<'_, AttributeType> {
        Ref::map(self.core.borrow(), |core| &core.spec.attr_type)
    }

    pub fn default_decl(&self) -> Ref<'_, DefaultDecl> {
        Ref::map(self.core.borrow(), |core| &core.spec.default_decl)
    }
}

impl std::fmt::Display for AttlistDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "<!ATTLIST {} {} {} {}>",
            self.elem_name(),
            self.attr_name(),
            self.attr_type(),
            self.default_decl()
        )
    }
}
