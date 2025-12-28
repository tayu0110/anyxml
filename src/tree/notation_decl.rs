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

    /// Notation name.
    pub fn name(&self) -> Rc<str> {
        self.core.borrow().spec.name.clone()
    }

    /// System identifier of this notation.
    pub fn system_id(&self) -> Option<Rc<URIStr>> {
        self.core.borrow().spec.system_id.clone()
    }

    /// Public identifier of this notation.
    pub fn public_id(&self) -> Option<Rc<str>> {
        self.core.borrow().spec.public_id.clone()
    }

    /// Create new node and copy internal data to the new node.
    ///
    /// While [`Clone::clone`] merely copies the pointer, this method copies the internal data
    /// to new memory, creating a completely different node. Comparing the source node and
    /// the new node using [`Node::is_same_node`] will always return `false`.
    ///
    /// # Example
    /// ```rust
    /// use anyxml::tree::Document;
    ///
    /// let document = Document::new();
    /// let nota1 = document.create_notation_decl("notation", None, None);
    /// let nota2 = nota1.deep_copy();
    /// assert!(nota1.is_same_node(nota1.clone()));
    /// assert_eq!(nota1.name(), nota2.name());
    /// assert!(!nota1.is_same_node(nota2));
    /// ```
    pub fn deep_copy(&self) -> Self {
        Node::create_node(
            NotationDeclSpec {
                name: self.name(),
                system_id: self.system_id(),
                public_id: self.public_id(),
            },
            self.owner_document(),
        )
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
