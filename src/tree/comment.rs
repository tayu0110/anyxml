use std::{
    cell::{Ref, RefCell},
    rc::Rc,
};

use crate::tree::{
    Document, NodeType,
    node::{Node, NodeCore, NodeSpec},
};

pub struct CommentSpec {
    data: String,
}

impl NodeSpec for CommentSpec {
    fn node_type(&self) -> NodeType {
        NodeType::Comment
    }

    fn first_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>> {
        None
    }

    fn last_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>> {
        None
    }
}

/// The leaf node of the document tree that represents a comment of the XML document.
///
/// It mostly covers the information provided by the "Comment Information Item"
/// in the [XML Infoset](https://www.w3.org/TR/xml-infoset/).
///
/// # Reference
/// [2.7. Comment Information Items](https://www.w3.org/TR/xml-infoset/#infoitem.comment)
pub type Comment = Node<CommentSpec>;

impl Comment {
    pub(crate) fn new(data: String, owner_document: Document) -> Self {
        Node::create_node(CommentSpec { data }, owner_document)
    }

    /// The content of this comment.
    ///
    /// "&lt;!--" and "--&gt;" are not contained.
    pub fn data(&self) -> Ref<'_, str> {
        Ref::map(self.core.borrow(), |core| core.spec.data.as_str())
    }

    /// Add a character `ch` to this comment.
    pub fn push(&mut self, ch: char) {
        self.core.borrow_mut().spec.data.push(ch);
    }

    /// Add a string `string` to this comment.
    pub fn push_str(&mut self, string: &str) {
        self.core.borrow_mut().spec.data.push_str(string);
    }
}

impl std::fmt::Display for Comment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<!--{}-->", self.data())
    }
}
