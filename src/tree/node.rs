use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

use crate::tree::{
    NodeType, XMLTreeError,
    convert::NodeKind,
    document::{Document, DocumentSpec},
    document_fragment::DocumentFragmentSpec,
};

pub trait NodeSpec: std::any::Any {
    fn node_type(&self) -> NodeType;
    fn first_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>>;
    fn last_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>>;
}

pub trait InternalNodeSpec: NodeSpec {
    fn set_first_child(&mut self, new: Rc<RefCell<NodeCore<dyn NodeSpec>>>);
    fn unset_first_child(&mut self);

    fn set_last_child(&mut self, new: Rc<RefCell<NodeCore<dyn NodeSpec>>>);
    fn unset_last_child(&mut self);

    fn pre_child_removal(&mut self, removed_child: Node<dyn NodeSpec>) -> Result<(), XMLTreeError> {
        let _ = removed_child;
        Ok(())
    }

    /// Perform preprocessing when `inserted_child` is inserted following `preceding_node`.
    ///
    /// If there is no preceding node (i.e., `inserted_child` is inserted as the first child),
    /// `preceding_node` is `None`.
    ///
    /// Only perform precondition checks; do not cause side effects.
    fn pre_child_insertion(
        &self,
        inserted_child: Node<dyn NodeSpec>,
        preceding_node: Option<Node<dyn NodeSpec>>,
    ) -> Result<(), XMLTreeError> {
        let _ = (inserted_child, preceding_node);
        Ok(())
    }

    /// Perform postprocessing when `inserted_child` is inserted following `preceding_node`.
    ///
    /// If there is no preceding node (i.e., `inserted_child` is inserted as the first child),
    /// `preceding_node` is `None`.
    ///
    /// Assume all prerequisites are satisfied; errors must not occur.
    fn post_child_insertion(&mut self, inserted_child: Node<dyn NodeSpec>) {
        let _ = inserted_child;
    }
}

pub struct NodeCore<Spec: ?Sized> {
    pub(super) parent_node: Weak<RefCell<NodeCore<dyn InternalNodeSpec>>>,
    pub(super) previous_sibling: Weak<RefCell<NodeCore<dyn NodeSpec>>>,
    pub(super) next_sibling: Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>>,
    pub(super) spec: Spec,
}

pub struct Node<Spec: ?Sized> {
    pub(super) core: Rc<RefCell<NodeCore<Spec>>>,
    pub(super) owner_document: Rc<RefCell<NodeCore<DocumentSpec>>>,
}

impl<Spec: NodeSpec + ?Sized> Node<Spec> {
    pub fn node_type(&self) -> NodeType {
        self.core.borrow().spec.node_type()
    }

    pub fn parent_node(&self) -> Option<Node<dyn InternalNodeSpec>> {
        self.core.borrow().parent_node.upgrade().map(|core| Node {
            core,
            owner_document: self.owner_document.clone(),
        })
    }
    pub fn previous_sibling(&self) -> Option<Node<dyn NodeSpec>> {
        self.core
            .borrow()
            .previous_sibling
            .upgrade()
            .map(|core| Node {
                core: core as _,
                owner_document: self.owner_document.clone(),
            })
    }
    pub fn next_sibling(&self) -> Option<Node<dyn NodeSpec>> {
        self.core.borrow().next_sibling.clone().map(|core| Node {
            core: core as _,
            owner_document: self.owner_document.clone(),
        })
    }
    pub fn first_child(&self) -> Option<Node<dyn NodeSpec>> {
        self.core.borrow().spec.first_child().map(|core| Node {
            core,
            owner_document: self.owner_document.clone(),
        })
    }
    pub fn last_child(&self) -> Option<Node<dyn NodeSpec>> {
        self.core.borrow().spec.last_child().map(|core| Node {
            core,
            owner_document: self.owner_document.clone(),
        })
    }

    pub fn owner_document(&self) -> Document {
        Document {
            core: self.owner_document.clone(),
            owner_document: self.owner_document.clone(),
        }
    }

    fn set_paretn_node(&mut self, new: Node<dyn InternalNodeSpec>) {
        self.core.borrow_mut().parent_node = Rc::downgrade(&new.core) as _;
    }
    fn unset_parent_node(&mut self) {
        // Since the type size must be known at compile time,
        // create a weak reference to an arbitrary node as a dummy and upcast it.
        let weak: Weak<RefCell<NodeCore<DocumentFragmentSpec>>> = Weak::new();
        self.core.borrow_mut().parent_node = weak as _;
    }

    fn set_previous_sibling(&mut self, new: Node<dyn NodeSpec>) {
        self.core.borrow_mut().previous_sibling = Rc::downgrade(&new.core);
    }
    fn unset_previous_sibling(&mut self) {
        // Since the type size must be known at compile time,
        // create a weak reference to an arbitrary node as a dummy and upcast it.
        let weak: Weak<RefCell<NodeCore<DocumentFragmentSpec>>> = Weak::new();
        self.core.borrow_mut().previous_sibling = weak as _;
    }

    fn set_next_sibling(&mut self, new: Node<dyn NodeSpec>) {
        self.core.borrow_mut().next_sibling = Some(new.core.clone());
    }
    fn unset_next_sibling(&mut self) {
        self.core.borrow_mut().next_sibling = None;
    }
}

impl Node<dyn NodeSpec> {
    /// Detach the link between parent and self, and remove self from the sibling list.
    ///
    /// For attribute nodes, execute [`remove_attribute_node`](crate::tree::Element::remove_attribute_node) on the parent element for itself.  \
    /// For namespace nodes, execute [`undeclare_namespace`](crate::tree::Element::undeclare_namespace) on the parent element for itself.
    pub fn detach(&mut self) -> Result<(), XMLTreeError> {
        let mut parent_node = self.parent_node();
        if let Some(parent_node) = parent_node.as_mut() {
            match self.downcast() {
                NodeKind::Attribute(attribute) => {
                    if let Some(mut element) = attribute.owner_element() {
                        element.remove_attribute_node(attribute).ok();
                    }
                    return Ok(());
                }
                NodeKind::Namespace(namespace) => {
                    if let Some(mut element) = namespace.owner_element() {
                        element.undeclare_namespace(namespace.prefix().as_deref());
                    }
                    return Ok(());
                }
                _ => {
                    parent_node.pre_child_removal(self.clone())?;
                }
            }
        }
        self.unset_parent_node();
        let previous_sibling = self.previous_sibling();
        self.unset_previous_sibling();
        let next_sibling = self.next_sibling();
        self.unset_next_sibling();

        match (parent_node, previous_sibling, next_sibling) {
            (_, Some(mut previous_sibling), Some(mut next_sibling)) => {
                previous_sibling.set_next_sibling(next_sibling.clone());
                next_sibling.set_previous_sibling(previous_sibling.clone());
            }
            (Some(mut parent_node), Some(mut previous_sibling), None) => {
                previous_sibling.unset_next_sibling();
                parent_node.set_last_child(previous_sibling);
            }
            (Some(mut parent_node), None, Some(mut next_sibling)) => {
                next_sibling.unset_previous_sibling();
                parent_node.set_first_child(next_sibling);
            }
            (Some(mut parent_node), None, None) => {
                parent_node.unset_first_child();
                parent_node.unset_last_child();
            }
            (None, Some(mut previous_sibling), None) => {
                previous_sibling.unset_next_sibling();
            }
            (None, None, Some(mut next_sibling)) => {
                next_sibling.unset_previous_sibling();
            }
            (None, None, None) => {}
        }
        Ok(())
    }

    fn cyclic_reference_check(
        &self,
        reference_node: &Node<dyn NodeSpec>,
    ) -> Result<(), XMLTreeError> {
        let mut parent_node = Some(self.clone());
        while let Some(now) = parent_node {
            parent_node = now.parent_node().map(From::from);
            if Rc::ptr_eq(&reference_node.core, &now.core) {
                return Err(XMLTreeError::CyclicReference);
            }
        }
        Ok(())
    }

    fn pre_insertion_common_check(
        &self,
        new_sibling: &Node<dyn NodeSpec>,
    ) -> Result<(), XMLTreeError> {
        if matches!(
            new_sibling.node_type(),
            NodeType::Document
                | NodeType::DocumentFragment
                | NodeType::Attribute
                | NodeType::Namespace
        ) {
            return Err(XMLTreeError::UnacceptableHierarchy);
        }

        self.cyclic_reference_check(new_sibling)?;
        Ok(())
    }

    pub fn insert_previous_sibling(
        &mut self,
        mut new_sibling: Node<dyn NodeSpec>,
    ) -> Result<(), XMLTreeError> {
        if let Some(frag) = new_sibling.as_document_fragment() {
            let mut succeed = 0;
            while let Some(mut child) = frag.first_child() {
                let ret = self.insert_previous_sibling(child.clone());
                if ret.is_err() {
                    // rollback
                    for _ in 0..succeed {
                        if let Some(mut previous) = self.previous_sibling() {
                            previous.detach()?;
                            child.insert_previous_sibling(previous.clone())?;
                            child = previous;
                        }
                    }
                    return ret;
                }
                succeed += 1;
            }
            return Ok(());
        }
        self.pre_insertion_common_check(&new_sibling)?;
        if let Some(parent_node) = self.parent_node() {
            parent_node.pre_child_insertion(new_sibling.clone(), self.previous_sibling())?;
        }
        new_sibling.detach()?;
        new_sibling.set_next_sibling(self.clone());
        if let Some(mut previous_sibling) = self.previous_sibling() {
            previous_sibling.set_next_sibling(new_sibling.clone());
            new_sibling.set_previous_sibling(previous_sibling);
            if let Some(parent_node) = self.parent_node() {
                new_sibling.set_paretn_node(parent_node);
            }
        } else if let Some(mut parent_node) = self.parent_node() {
            parent_node.set_first_child(new_sibling.clone());
            new_sibling.set_paretn_node(parent_node);
        }
        self.set_previous_sibling(new_sibling.clone());
        if let Some(mut parent_node) = self.parent_node() {
            parent_node.post_child_insertion(new_sibling);
        }
        Ok(())
    }

    pub fn insert_next_sibling(
        &mut self,
        mut new_sibling: Node<dyn NodeSpec>,
    ) -> Result<(), XMLTreeError> {
        if let Some(mut frag) = new_sibling.as_document_fragment() {
            let mut succeed = 0;
            while let Some(child) = frag.last_child() {
                let ret = self.insert_next_sibling(child);
                if ret.is_err() {
                    // rollback
                    for _ in 0..succeed {
                        if let Some(mut next) = self.next_sibling() {
                            next.detach()?;
                            frag.append_child(next)?;
                        }
                    }
                    return ret;
                }
                succeed += 1;
            }
            return Ok(());
        }
        self.pre_insertion_common_check(&new_sibling)?;
        if let Some(parent_node) = self.parent_node() {
            parent_node.pre_child_insertion(new_sibling.clone(), Some(self.clone()))?;
        }
        new_sibling.detach()?;
        new_sibling.set_previous_sibling(self.clone());
        if let Some(mut next_sibling) = self.next_sibling() {
            next_sibling.set_previous_sibling(new_sibling.clone());
            new_sibling.set_next_sibling(next_sibling);
            if let Some(parent_node) = self.parent_node() {
                new_sibling.set_paretn_node(parent_node);
            }
        } else if let Some(mut parent_node) = self.parent_node() {
            parent_node.set_last_child(new_sibling.clone());
            new_sibling.set_paretn_node(parent_node);
        }
        self.set_next_sibling(new_sibling.clone());
        if let Some(mut parent_node) = self.parent_node() {
            parent_node.post_child_insertion(new_sibling);
        }
        Ok(())
    }
}

impl Node<dyn InternalNodeSpec> {
    pub fn detach(&mut self) -> Result<(), XMLTreeError> {
        Node::<dyn NodeSpec>::from(self.clone()).detach()
    }

    pub fn insert_previous_sibling(
        &mut self,
        new_sibling: Node<dyn NodeSpec>,
    ) -> Result<(), XMLTreeError> {
        Node::<dyn NodeSpec>::from(self.clone()).insert_previous_sibling(new_sibling)
    }

    pub fn insert_next_sibling(
        &mut self,
        new_sibling: Node<dyn NodeSpec>,
    ) -> Result<(), XMLTreeError> {
        Node::<dyn NodeSpec>::from(self.clone()).insert_next_sibling(new_sibling)
    }

    pub fn append_child(&mut self, mut new_child: Node<dyn NodeSpec>) -> Result<(), XMLTreeError> {
        if let Some(mut last_child) = self.last_child() {
            last_child.insert_next_sibling(new_child)?;
        } else {
            if let Some(mut frag) = new_child.as_document_fragment() {
                let Some(mut child) = frag.first_child() else {
                    return Ok(());
                };

                self.append_child(child.clone())?;
                return match self.append_child(frag.clone().into()) {
                    Ok(()) => Ok(()),
                    Err(err) => {
                        child.detach()?;
                        if let Some(mut first) = frag.first_child() {
                            first.insert_previous_sibling(child)?;
                        } else {
                            frag.append_child(child)?;
                        }
                        return Err(err);
                    }
                };
            }
            Node::<dyn NodeSpec>::from(self.clone()).pre_insertion_common_check(&new_child)?;
            self.pre_child_insertion(new_child.clone(), None)?;
            new_child.detach()?;
            new_child.set_paretn_node(self.clone());
            self.set_first_child(new_child.clone());
            self.set_last_child(new_child.clone());
            self.post_child_insertion(new_child);
        }
        Ok(())
    }

    fn pre_child_removal(&mut self, removed_child: Node<dyn NodeSpec>) -> Result<(), XMLTreeError> {
        self.core.borrow_mut().spec.pre_child_removal(removed_child)
    }

    fn pre_child_insertion(
        &self,
        inserted_child: Node<dyn NodeSpec>,
        preceding_node: Option<Node<dyn NodeSpec>>,
    ) -> Result<(), XMLTreeError> {
        self.core
            .borrow()
            .spec
            .pre_child_insertion(inserted_child, preceding_node)
    }

    fn post_child_insertion(&mut self, inserted_child: Node<dyn NodeSpec>) {
        self.core
            .borrow_mut()
            .spec
            .post_child_insertion(inserted_child);
    }
}

impl<Spec: NodeSpec + 'static> Node<Spec> {
    pub(crate) fn create_node(spec: Spec, owner_document: Document) -> Self {
        let weak: Weak<RefCell<NodeCore<DocumentFragmentSpec>>> = Weak::new();
        Node {
            core: Rc::new(RefCell::new(NodeCore {
                parent_node: weak.clone(),
                previous_sibling: weak,
                next_sibling: None,
                spec,
            })),
            owner_document: owner_document.owner_document.clone(),
        }
    }

    pub fn detach(&mut self) -> Result<(), XMLTreeError> {
        Node::<dyn NodeSpec>::from(self.clone()).detach()
    }

    pub fn insert_previous_sibling(
        &mut self,
        new_sibling: Node<dyn NodeSpec>,
    ) -> Result<(), XMLTreeError> {
        Node::<dyn NodeSpec>::from(self.clone()).insert_previous_sibling(new_sibling)
    }

    pub fn insert_next_sibling(
        &mut self,
        new_sibling: Node<dyn NodeSpec>,
    ) -> Result<(), XMLTreeError> {
        Node::<dyn NodeSpec>::from(self.clone()).insert_next_sibling(new_sibling)
    }
}

impl<Spec: InternalNodeSpec + ?Sized> Node<Spec> {
    fn set_first_child(&mut self, new: Node<dyn NodeSpec>) {
        self.core
            .borrow_mut()
            .spec
            .set_first_child(new.core.clone());
    }
    fn unset_first_child(&mut self) {
        self.core.borrow_mut().spec.unset_first_child();
    }

    fn set_last_child(&mut self, new: Node<dyn NodeSpec>) {
        self.core.borrow_mut().spec.set_last_child(new.core.clone());
    }
    fn unset_last_child(&mut self) {
        self.core.borrow_mut().spec.unset_last_child();
    }
}

impl<Spec: InternalNodeSpec + 'static> Node<Spec> {
    pub fn append_child(&mut self, new_child: Node<dyn NodeSpec>) -> Result<(), XMLTreeError> {
        Node::<dyn InternalNodeSpec>::from(self.clone()).append_child(new_child)
    }
}

impl<Spec: ?Sized> Clone for Node<Spec> {
    fn clone(&self) -> Self {
        Self {
            core: self.core.clone(),
            owner_document: self.owner_document.clone(),
        }
    }
}

impl<Spec: NodeSpec + 'static> From<Node<Spec>> for Node<dyn NodeSpec> {
    fn from(value: Node<Spec>) -> Self {
        Node {
            core: value.core,
            owner_document: value.owner_document,
        }
    }
}

impl From<Node<dyn InternalNodeSpec>> for Node<dyn NodeSpec> {
    fn from(value: Node<dyn InternalNodeSpec>) -> Self {
        Node {
            core: value.core,
            owner_document: value.owner_document,
        }
    }
}

impl<Spec: InternalNodeSpec + 'static> From<Node<Spec>> for Node<dyn InternalNodeSpec> {
    fn from(value: Node<Spec>) -> Self {
        Node {
            core: value.core,
            owner_document: value.owner_document,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cyclic_reference_tests() {
        let document = Document::new();
        let mut elem = document.create_element("elem".into(), None).unwrap();
        let mut elem2 = document.create_element("elem2".into(), None).unwrap();
        elem.append_child(elem2.clone().into()).unwrap();

        assert!(
            elem2
                .append_child(elem.clone().into())
                .is_err_and(|err| matches!(err, XMLTreeError::CyclicReference))
        );
        assert!(
            elem.insert_previous_sibling(elem.clone().into())
                .is_err_and(|err| matches!(err, XMLTreeError::CyclicReference))
        );
        assert!(
            elem.append_child(elem.clone().into())
                .is_err_and(|err| matches!(err, XMLTreeError::CyclicReference))
        );
        assert!(
            elem.insert_next_sibling(elem.clone().into())
                .is_err_and(|err| matches!(err, XMLTreeError::CyclicReference))
        );
        assert!(
            elem2
                .append_child(elem2.clone().into())
                .is_err_and(|err| matches!(err, XMLTreeError::CyclicReference))
        );
        assert!(
            elem2
                .insert_previous_sibling(elem2.clone().into())
                .is_err_and(|err| matches!(err, XMLTreeError::CyclicReference))
        );
        assert!(
            elem2
                .insert_next_sibling(elem2.clone().into())
                .is_err_and(|err| matches!(err, XMLTreeError::CyclicReference))
        );
    }
}
