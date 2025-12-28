use std::{
    cell::{Ref, RefCell},
    rc::Rc,
};

use crate::{
    save::write_escaped_char_data,
    tree::{
        Document, NodeType,
        node::{Node, NodeCore, NodeSpec},
    },
};

pub struct TextSpec {
    data: String,
}

impl NodeSpec for TextSpec {
    fn node_type(&self) -> NodeType {
        NodeType::Text
    }

    fn first_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>> {
        None
    }

    fn last_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>> {
        None
    }
}

pub type Text = Node<TextSpec>;

impl Text {
    pub(crate) fn new(data: String, owner_document: Document) -> Self {
        Node::create_node(TextSpec { data }, owner_document)
    }

    /// Return the character data of this text node.
    ///
    /// The returned data is literal data that does not contain escapes or references.
    pub fn data(&self) -> Ref<'_, str> {
        Ref::map(self.core.borrow(), |core| core.spec.data.as_str())
    }

    /// Add `ch` to the end of the character data.
    pub fn push(&mut self, ch: char) {
        self.core.borrow_mut().spec.data.push(ch);
    }

    /// Add `string` to the end of the character data.
    ///
    /// `string` is treated as literal data.  \
    /// For example, `"&lt;"` is treated as the literal character data `"&lt;"`, not as `"<"`.
    pub fn push_str(&mut self, string: &str) {
        self.core.borrow_mut().spec.data.push_str(string);
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
    /// let text1 = document.create_text("text node");
    /// let text2 = text1.deep_copy();
    /// assert!(text1.is_same_node(text1.clone()));
    /// assert_eq!(*text1.data(), *text2.data());
    /// assert!(!text1.is_same_node(text2));
    /// ```
    pub fn deep_copy(&self) -> Self {
        Node::create_node(
            TextSpec {
                data: self.data().to_owned(),
            },
            self.owner_document(),
        )
    }
}

impl std::fmt::Display for Text {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write_escaped_char_data(f, &self.data())
    }
}
