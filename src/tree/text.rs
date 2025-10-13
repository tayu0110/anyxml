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

    pub fn data(&self) -> Ref<'_, str> {
        Ref::map(self.core.borrow(), |core| core.spec.data.as_str())
    }

    pub fn push(&mut self, ch: char) {
        self.core.borrow_mut().spec.data.push(ch);
    }

    pub fn push_str(&mut self, string: &str) {
        self.core.borrow_mut().spec.data.push_str(string);
    }
}

impl std::fmt::Display for Text {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write_escaped_char_data(f, &self.data())
    }
}
