use std::{cell::RefCell, rc::Rc};

use crate::tree::{
    Document, NodeType, XMLTreeError,
    node::{InternalNodeSpec, Node, NodeCore, NodeSpec},
};

pub struct EntityReferenceSpec {
    first_child: Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>>,
    last_child: Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>>,

    name: Rc<str>,
}

impl NodeSpec for EntityReferenceSpec {
    fn node_type(&self) -> NodeType {
        NodeType::EntityReference
    }

    fn first_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>> {
        self.first_child.clone()
    }

    fn last_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>> {
        self.last_child.clone()
    }
}

impl InternalNodeSpec for EntityReferenceSpec {
    fn set_first_child(&mut self, new: Rc<RefCell<NodeCore<dyn NodeSpec>>>) {
        self.first_child = Some(new);
    }
    fn unset_first_child(&mut self) {
        self.first_child = None;
    }

    fn set_last_child(&mut self, new: Rc<RefCell<NodeCore<dyn NodeSpec>>>) {
        self.last_child = Some(new);
    }
    fn unset_last_child(&mut self) {
        self.last_child = None;
    }

    fn pre_child_insertion(
        &self,
        inserted_child: Node<dyn NodeSpec>,
        _preceding_node: Option<Node<dyn NodeSpec>>,
    ) -> Result<(), super::XMLTreeError> {
        match inserted_child.node_type() {
            NodeType::CDATASection
            | NodeType::Comment
            | NodeType::Element
            | NodeType::EntityReference
            | NodeType::ProcessingInstruction
            | NodeType::Text => Ok(()),
            _ => Err(XMLTreeError::UnacceptableHierarchy),
        }
    }
}

pub type EntityReference = Node<EntityReferenceSpec>;

impl EntityReference {
    pub(crate) fn new(name: Rc<str>, owner_document: Document) -> Self {
        Node::create_node(
            EntityReferenceSpec {
                first_child: None,
                last_child: None,
                name,
            },
            owner_document,
        )
    }

    /// Entity name.
    pub fn name(&self) -> Rc<str> {
        self.core.borrow().spec.name.clone()
    }

    /// Create new node and copy internal data to the new node other than pointers to neighbor nodes.
    ///
    /// While [`Clone::clone`] merely copies the pointer, this method copies the internal data
    /// to new memory, creating a completely different node. Comparing the source node and
    /// the new node using [`Node::is_same_node`] will always return `false`.
    pub fn deep_copy(&self) -> Self {
        Node::create_node(
            EntityReferenceSpec {
                first_child: None,
                last_child: None,
                name: self.name(),
            },
            self.owner_document(),
        )
    }

    /// Perform a deep copy on all descendant nodes and construct a tree with the same structure.
    ///
    /// The link to the parent is not preserved.
    pub fn deep_copy_subtree(&self) -> Result<Self, XMLTreeError> {
        let mut ret = self.deep_copy();
        let mut children = self.first_child();
        while let Some(child) = children {
            children = child.next_sibling();
            ret.append_child(child.deep_copy_subtree()?)?;
        }
        Ok(ret)
    }
}

impl std::fmt::Display for EntityReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = self.name();
        if name.starts_with('%') {
            write!(f, "{};", name)
        } else {
            write!(f, "&{};", name)
        }
    }
}
