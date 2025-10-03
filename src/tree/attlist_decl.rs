use std::{
    cell::{Ref, RefCell},
    rc::Rc,
};

use crate::{
    sax::{AttributeType, DefaultDecl},
    tree::{
        NodeType,
        node::{Node, NodeCore, NodeSpec},
    },
};

pub struct AttlistDeclSpec {
    elem_name: Box<str>,
    attr_name: Box<str>,
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
    pub fn elem_name(&self) -> Ref<'_, str> {
        Ref::map(self.core.borrow(), |core| {
            core.spec.elem_name.as_ref()
        })
    }

    pub fn attr_name(&self) -> Ref<'_, str> {
        Ref::map(self.core.borrow(), |core| {
            core.spec.attr_name.as_ref()
        })
    }

    pub fn attr_type(&self) -> Ref<'_, AttributeType> {
        Ref::map(self.core.borrow(), |core| &core.spec.attr_type)
    }

    pub fn default_decl(&self) -> Ref<'_, DefaultDecl> {
        Ref::map(self.core.borrow(), |core| &core.spec.default_decl)
    }
}
