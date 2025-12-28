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

    /// The target element name of this declaration.
    pub fn elem_name(&self) -> Rc<str> {
        self.core.borrow().spec.elem_name.clone()
    }

    /// The target attribute name of this declaration.
    pub fn attr_name(&self) -> Rc<str> {
        self.core.borrow().spec.attr_name.clone()
    }

    /// Attribute type. (i.e. CDATA, ID, ENTITY, ...)
    pub fn attr_type(&self) -> Ref<'_, AttributeType> {
        Ref::map(self.core.borrow(), |core| &core.spec.attr_type)
    }

    /// Default declaration. (#REQUIRED, #IMPLIED, #FIXED default value or unqualified default value)
    pub fn default_decl(&self) -> Ref<'_, DefaultDecl> {
        Ref::map(self.core.borrow(), |core| &core.spec.default_decl)
    }

    /// Create new node and copy internal data to the new node.
    ///
    /// While [`Clone::clone`] merely copies the pointer, this method copies the internal data
    /// to new memory, creating a completely different node. Comparing the source node and
    /// the new node using [`Node::is_same_node`] will always return `false`.
    pub fn deep_copy(&self) -> Self {
        Node::create_node(
            AttlistDeclSpec {
                elem_name: self.elem_name(),
                attr_name: self.attr_name(),
                attr_type: self.attr_type().clone(),
                default_decl: self.default_decl().clone(),
            },
            self.owner_document(),
        )
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
