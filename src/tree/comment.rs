use std::{
    cell::{Ref, RefCell},
    rc::Rc,
};

use crate::tree::{
    NodeType,
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

pub type Comment = Node<CommentSpec>;

impl Comment {
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
