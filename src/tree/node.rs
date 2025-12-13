use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

use crate::{
    XML_XML_NAMESPACE,
    tree::{
        Element, NodeType, XMLTreeError, compare_document_order,
        convert::NodeKind,
        document::{Document, DocumentSpec},
        document_fragment::DocumentFragmentSpec,
    },
    uri::URIString,
};

/// Common traits for node types.
///
/// This trait is not intended to be implemented by library users and should not be implemented.
pub trait NodeSpec: std::any::Any {
    /// Return the node type.
    ///
    /// For abstract node types, this method guarantees that conversion
    /// to the specified node type will always succeed.
    fn node_type(&self) -> NodeType;
    fn first_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>>;
    fn last_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>>;
}

/// Common traits of internal nodes (i.e., nodes that are not leaf nodes).
///
/// This trait is not intended to be implemented by library users and should not be implemented.
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

/// Pointer to a node.
///
/// Internally, it is a pair consisting of a reference counter for the node's data
/// and a reference counter for the document node that owns it.
///
/// `Node<dyn NodeSpec>` and `Node<dyn InternalNodeSpec>` are abstract node types.  \
/// They enable common operations on concrete node types that implement their respective traits,
/// as well as conversion to a specific concrete node type.
pub struct Node<Spec: ?Sized> {
    pub(super) core: Rc<RefCell<NodeCore<Spec>>>,
    pub(super) owner_document: Rc<RefCell<NodeCore<DocumentSpec>>>,
}

impl<Spec: NodeSpec + ?Sized> Node<Spec> {
    /// Return the node type.
    ///
    /// For abstract node types, this method guarantees that conversion
    /// to the specified node type will always succeed.
    pub fn node_type(&self) -> NodeType {
        self.core.borrow().spec.node_type()
    }

    /// Get the parent node of `self` if exsits.
    ///
    /// # Note
    /// [`Attribute`](crate::tree::Attribute) and [`Namespace`](crate::tree::Namespace) each return
    /// the same result as [`Attribute::owner_element`](crate::tree::Attribute::owner_element)
    /// and [`Namespace::owner_element`](crate::tree::Namespace::owner_element),
    /// but neither is a child of [`Element`](crate::tree::Element).
    pub fn parent_node(&self) -> Option<Node<dyn InternalNodeSpec>> {
        self.core.borrow().parent_node.upgrade().map(|core| Node {
            core,
            owner_document: self.owner_document.clone(),
        })
    }
    /// Get the previous sibling node of `self` if exsits.
    ///
    /// # Note
    /// [`Attribute`](crate::tree::Attribute) and [`Namespace`](crate::tree::Namespace)
    /// always return [`None`].
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
    /// Get the previous sibling whose node type is [`Element`](NodeType::Element).
    pub fn previous_element_sibling(&self) -> Option<Element> {
        let mut previous = self.previous_sibling();
        while let Some(now) = previous {
            previous = now.previous_sibling();

            if let Some(element) = now.as_element() {
                return Some(element);
            }
        }
        None
    }
    /// Get the next sibling node of `self` if exsits.
    ///
    /// # Note
    /// [`Attribute`](crate::tree::Attribute) and [`Namespace`](crate::tree::Namespace)
    /// always return [`None`].
    pub fn next_sibling(&self) -> Option<Node<dyn NodeSpec>> {
        self.core.borrow().next_sibling.clone().map(|core| Node {
            core: core as _,
            owner_document: self.owner_document.clone(),
        })
    }
    /// Get the next sibling whose node type is [`Element`](NodeType::Element).
    pub fn next_element_sibling(&self) -> Option<Element> {
        let mut next = self.next_sibling();
        while let Some(now) = next {
            next = now.next_sibling();

            if let Some(element) = now.as_element() {
                return Some(element);
            }
        }
        None
    }
    /// Get the first child node of `self` if exsits.
    pub fn first_child(&self) -> Option<Node<dyn NodeSpec>> {
        self.core.borrow().spec.first_child().map(|core| Node {
            core,
            owner_document: self.owner_document.clone(),
        })
    }
    /// Get the first child whose node type is [`Element`](NodeType::Element).
    pub fn first_element_child(&self) -> Option<Element> {
        let mut children = self.first_child();
        while let Some(child) = children {
            children = child.next_sibling();

            if let Some(element) = child.as_element() {
                return Some(element);
            }
        }
        None
    }
    /// Get the last child node of `self` if exsits.
    pub fn last_child(&self) -> Option<Node<dyn NodeSpec>> {
        self.core.borrow().spec.last_child().map(|core| Node {
            core,
            owner_document: self.owner_document.clone(),
        })
    }
    /// Get the first child whose node type is [`Element`](NodeType::Element).
    pub fn last_element_child(&self) -> Option<Element> {
        let mut children = self.last_child();
        while let Some(child) = children {
            children = child.previous_sibling();

            if let Some(element) = child.as_element() {
                return Some(element);
            }
        }
        None
    }

    /// Get the [`Document`] node that represents the owner document of `self`.
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

    pub(super) fn is_same_owner_document<Other: NodeSpec + ?Sized>(
        &self,
        other: &Node<Other>,
    ) -> bool {
        Rc::ptr_eq(&self.owner_document, &other.owner_document)
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

    fn do_insert_previous_sibling(
        &mut self,
        mut new_sibling: Node<dyn NodeSpec>,
    ) -> Result<(), XMLTreeError> {
        if self.parent_node().is_none() {
            return Err(XMLTreeError::UnacceptableHierarchy);
        }
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

    /// Insert `new_sibling` as a sibling preceding itself.
    ///
    /// Insertions that violate tree constraints (such as those that create cyclic references
    /// or insert siblings at the root node) are errors.  \
    /// Additionally, operations that generate expressions impossible in well-formed XML documents
    /// (such as placing Text outside document elements or inserting declarations into element
    /// content) are also errors.
    ///
    /// # Example
    /// ```rust
    /// use anyxml::tree::Document;
    ///
    /// let mut document = Document::new();
    /// let mut root = document.create_element("root", None).unwrap();
    /// let mut comment = document.create_comment("comment");
    /// // cyclic reference
    /// assert!(root.insert_previous_sibling(root.clone()).is_err());
    /// // multiple root
    /// assert!(root.insert_previous_sibling(comment.clone()).is_err());
    /// document.append_child(root.clone()).unwrap();
    /// root.insert_previous_sibling(comment).unwrap();
    /// assert!(root
    ///     .previous_sibling()
    ///     .and_then(|sib| sib.as_comment())
    ///     .is_some_and(|comment| &*comment.data() == "comment")
    /// );
    /// ```
    pub fn insert_previous_sibling(
        &mut self,
        new_sibling: impl Into<Node<dyn NodeSpec>>,
    ) -> Result<(), XMLTreeError> {
        self.do_insert_previous_sibling(new_sibling.into())
    }

    fn do_insert_next_sibling(
        &mut self,
        mut new_sibling: Node<dyn NodeSpec>,
    ) -> Result<(), XMLTreeError> {
        if self.parent_node().is_none() {
            return Err(XMLTreeError::UnacceptableHierarchy);
        }
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

    /// Insert `new_sibling` as a sibling following itself.
    ///
    /// Insertions that violate tree constraints (such as those that create cyclic references
    /// or insert siblings at the root node) are errors.  \
    /// Additionally, operations that generate expressions impossible in well-formed XML documents
    /// (such as placing Text outside document elements or inserting declarations into element
    /// content) are also errors.
    ///
    /// # Example
    /// ```rust
    /// use anyxml::tree::Document;
    ///
    /// let mut document = Document::new();
    /// let mut root = document.create_element("root", None).unwrap();
    /// let mut comment = document.create_comment("comment");
    /// // cyclic reference
    /// assert!(root.insert_next_sibling(root.clone()).is_err());
    /// // multiple root
    /// assert!(root.insert_next_sibling(comment.clone()).is_err());
    /// document.append_child(root.clone()).unwrap();
    /// root.insert_next_sibling(comment).unwrap();
    /// assert!(root
    ///     .next_sibling()
    ///     .and_then(|sib| sib.as_comment())
    ///     .is_some_and(|comment| &*comment.data() == "comment")
    /// );
    /// ```
    pub fn insert_next_sibling(
        &mut self,
        new_sibling: impl Into<Node<dyn NodeSpec>>,
    ) -> Result<(), XMLTreeError> {
        self.do_insert_next_sibling(new_sibling.into())
    }

    /// Replace the subtree rooted at `self` with the subtree rooted at `src`.
    ///
    /// If `self` has no parent, nothing happens.  \
    /// If `self` has a parent, the following occurs:
    /// - [`src.detach()`](Node::detach) is executed,
    /// - `src` is inserted as an adjacent sibling to `self`,
    /// - and finally [`self.detach()`](Node::detach) is executed.
    ///
    /// # Note
    /// `self` is never inserted into the position where `src` was.  \
    /// `self` simply becomes a subtree without a parent.
    pub fn replace_subtree<N: NodeSpec>(&mut self, src: &mut Node<N>) -> Result<(), XMLTreeError> {
        let Some(parent) = self.parent_node() else {
            return Ok(());
        };

        if let Some(mut document) = parent.as_document() {
            if let Some(mut prev) = self.previous_sibling() {
                self.detach()?;
                if let Err(err) = prev.insert_next_sibling(src) {
                    prev.insert_next_sibling(self)?;
                    return Err(err);
                }
            } else if let Some(mut next) = self.next_sibling() {
                self.detach()?;
                if let Err(err) = next.insert_previous_sibling(src) {
                    next.insert_previous_sibling(self)?;
                    return Err(err);
                }
            } else {
                self.detach()?;
                if let Err(err) = document.append_child(src) {
                    document.append_child(self)?;
                    return Err(err);
                }
            }
        } else {
            self.insert_previous_sibling(src)?;
            self.detach()?;
        }
        Ok(())
    }

    /// Check whether `self` and `other` are the same node.
    ///
    /// # Note
    /// This method does not perform equality comparison.  \
    /// For example, `is_same_node` will always return `false` for two [`Text`](crate::tree::Text)
    /// nodes containing exactly the same string generated from the same [`Document`],
    /// unless one is a clone of the other.
    ///
    /// # Example
    /// ```rust
    /// use anyxml::tree::Document;
    ///
    /// let document = Document::new();
    /// let text1 = document.create_text("Hello");
    /// let text2 = document.create_text("Hello");
    ///
    /// assert!(text1.is_same_node(text1.clone()));
    /// // These have the same character data, but are not the same node.
    /// assert!(!text1.is_same_node(text2.clone()));
    /// ```
    pub fn is_same_node(&self, other: impl Into<Self>) -> bool {
        let other: Self = other.into();
        Rc::ptr_eq(&self.core, &other.core)
    }

    /// Compare the positions of `self` and `other` in the document order.
    ///
    /// If `self` appears first, return `Less`;  \
    /// if it appears later, return `Greater`;  \
    /// if they are the same node, return `Equal`.
    ///
    /// If `self` and `other` do not belong to the same document tree
    /// (i.e., they have no common ancestor), return `None`.
    ///
    /// # Reference
    /// [5 Data Model in XPath 1.0](https://www.w3.org/TR/1999/REC-xpath-19991116/#dt-document-order)
    pub fn compare_document_order(
        &self,
        other: impl Into<Node<dyn NodeSpec>>,
    ) -> Option<std::cmp::Ordering> {
        compare_document_order(self.clone(), other.into())
    }

    /// Retrieve the base URI according to the [XML Base](https://www.w3.org/TR/xmlbase/).
    ///
    /// If the node is embedded as a descendant of a [`Document`] node, always return `Some`.  \
    /// If there are insufficient ancestor nodes to resolve the base URI, return `None`.
    pub fn base_uri(&self) -> Option<URIString> {
        let mut node = Some(self.clone());
        let mut uris: Vec<URIString> = vec![];
        let mut ret = None;
        while let Some(now) = node {
            if let Some(base) = now
                .as_element()
                .and_then(|elem| elem.get_attribute("base", Some(XML_XML_NAMESPACE)))
                .and_then(|base| URIString::parse(base).ok())
            {
                if base.scheme().is_some() {
                    ret = Some(
                        uris.into_iter()
                            .rev()
                            .fold(base, |base, rel| base.resolve(&rel)),
                    );
                    break;
                }
                uris.push(base);
            } else if let Some(document) = now.as_document() {
                let base = document.document_base_uri().as_ref().to_owned();
                ret = Some(
                    uris.into_iter()
                        .rev()
                        .fold(base, |base, rel| base.resolve(&rel)),
                );
                break;
            } else if let Some(entity) = now.as_entity_reference() {
                let name = entity.name();
                if let Some(base) = self
                    .owner_document()
                    .document_type()
                    .and_then(|doctype| doctype.get_entity_decl(&name))
                    .and_then(|decl| decl.system_id())
                {
                    let base = base.as_ref().to_owned();
                    ret = Some(
                        uris.into_iter()
                            .rev()
                            .fold(base, |base, rel| base.resolve(&rel)),
                    );
                    break;
                }
            }
            node = now.parent_node().map(From::from);
        }
        ret.map(|uri| {
            // The URI returned as the base URI must be an absolute URI.
            // However, at this point, only relative references have been resolved,
            // and a fragment may be included.
            // The URI returned as the base URI must be an absolute URI.
            // Therefore, resolving the relative reference without the fragment removes the fragment.
            //
            // Reference:
            // [Testing XML Base Conformance](https://www.w3.org/XML/2006/12/xmlbase-testing.html)
            // section 4. in "Discussion of few key example"
            uri.resolve(&URIString::parse("").unwrap())
        })
    }

    /// If `self` and its descendants contain character data, concatenate all of it
    /// and return the result.  \
    /// If they do not contain character data, return an empty string.
    ///
    /// # Note
    /// If descendants contain [`Comment`](crate::tree::Comment)
    /// or [`ProcessingInstruction`](crate::tree::ProcessingInstruction), these are also
    /// included in the result.  \
    /// For [`ProcessingInstruction`](crate::tree::ProcessingInstruction),
    /// the result is the data following the target.
    ///
    /// Additionally, the result for [`Document`] is an empty string.
    pub fn text_content(&self) -> String {
        let mut buf = String::new();
        fn collect_text_content(node: Node<dyn NodeSpec>, buf: &mut String) {
            match node.downcast() {
                NodeKind::Element(_)
                | NodeKind::Attribute(_)
                | NodeKind::EntityDecl(_)
                | NodeKind::EntityReference(_)
                | NodeKind::DocumentFragment(_) => {
                    let mut children = node.first_child();
                    while let Some(child) = children {
                        children = child.next_sibling();
                        collect_text_content(child, buf);
                    }
                }
                NodeKind::Text(text) => {
                    buf.push_str(&text.data());
                }
                NodeKind::CDATASection(cdata) => {
                    buf.push_str(&cdata.data());
                }
                NodeKind::Comment(comment) => {
                    buf.push_str(&comment.data());
                }
                NodeKind::ProcessingInstruction(pi) => {
                    buf.push_str(pi.data().as_deref().unwrap_or_default());
                }
                _ => {}
            }
        }
        collect_text_content(self.clone(), &mut buf);
        buf
    }
}

impl Node<dyn InternalNodeSpec> {
    /// See [Node::detach].
    pub fn detach(&mut self) -> Result<(), XMLTreeError> {
        Node::<dyn NodeSpec>::from(self.clone()).detach()
    }

    /// See [Node::insert_previous_sibling].
    pub fn insert_previous_sibling(
        &mut self,
        new_sibling: impl Into<Node<dyn NodeSpec>>,
    ) -> Result<(), XMLTreeError> {
        Node::<dyn NodeSpec>::from(self.clone()).insert_previous_sibling(new_sibling)
    }

    /// See [Node::insert_next_sibling].
    pub fn insert_next_sibling(
        &mut self,
        new_sibling: impl Into<Node<dyn NodeSpec>>,
    ) -> Result<(), XMLTreeError> {
        Node::<dyn NodeSpec>::from(self.clone()).insert_next_sibling(new_sibling)
    }

    /// See [Node::replace_subtree].
    pub fn replace_subtree<N: NodeSpec>(&mut self, src: &mut Node<N>) -> Result<(), XMLTreeError> {
        Node::<dyn NodeSpec>::from(self).replace_subtree(src)
    }

    fn do_append_child(&mut self, mut new_child: Node<dyn NodeSpec>) -> Result<(), XMLTreeError> {
        if let Some(mut last_child) = self.last_child() {
            last_child.insert_next_sibling(new_child)?;
        } else {
            if let Some(mut frag) = new_child.as_document_fragment() {
                let Some(mut child) = frag.first_child() else {
                    return Ok(());
                };

                self.append_child(child.clone())?;
                return match self.append_child(frag.clone()) {
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

    /// Insert `new_child` as a last child of `self`.
    ///
    /// Insertions that violate tree constraints (such as those that create cyclic references)
    /// are errors.  \
    /// Additionally, operations that generate expressions impossible in well-formed XML documents
    /// (such as placing Text outside document elements or inserting declarations into element
    /// content) are also errors.
    pub fn append_child(
        &mut self,
        new_child: impl Into<Node<dyn NodeSpec>>,
    ) -> Result<(), XMLTreeError> {
        self.do_append_child(new_child.into())
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

    /// Check whether `self` and `other` are the same node.
    ///
    /// For more details, please refer [`Node::is_same_node`] of [`Node<dyn NodeSpec>`]
    pub fn is_same_node(&self, other: impl Into<Node<dyn NodeSpec>>) -> bool {
        let left = Node::<dyn NodeSpec>::from(self);
        let right: Node<dyn NodeSpec> = other.into();
        Rc::ptr_eq(&left.core, &right.core)
    }

    /// Compare the positions of `self` and `other` in the document order.
    ///
    /// For more details, please refer [`Node::compare_document_order`] of [`Node<dyn NodeSpec>`].
    pub fn compare_document_order(
        &self,
        other: impl Into<Node<dyn NodeSpec>>,
    ) -> Option<std::cmp::Ordering> {
        compare_document_order(self.into(), other.into())
    }

    /// Retrieve the base URI according to the [XML Base](https://www.w3.org/TR/xmlbase/).
    ///
    /// For more details, please refer to [Node::base_uri].
    pub fn base_uri(&self) -> Option<URIString> {
        Node::<dyn NodeSpec>::from(self).base_uri()
    }

    /// Please see [Node::text_content].
    pub fn text_content(&self) -> String {
        Node::<dyn NodeSpec>::from(self).text_content()
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

    /// See [Node::detach].
    pub fn detach(&mut self) -> Result<(), XMLTreeError> {
        Node::<dyn NodeSpec>::from(self).detach()
    }

    /// See [Node::insert_previous_sibling].
    pub fn insert_previous_sibling(
        &mut self,
        new_sibling: impl Into<Node<dyn NodeSpec>>,
    ) -> Result<(), XMLTreeError> {
        Node::<dyn NodeSpec>::from(self).insert_previous_sibling(new_sibling)
    }

    /// See [Node::insert_next_sibling].
    pub fn insert_next_sibling(
        &mut self,
        new_sibling: impl Into<Node<dyn NodeSpec>>,
    ) -> Result<(), XMLTreeError> {
        Node::<dyn NodeSpec>::from(self).insert_next_sibling(new_sibling)
    }

    /// See [Node::replace_subtree].
    pub fn replace_subtree<N: NodeSpec>(&mut self, src: &mut Node<N>) -> Result<(), XMLTreeError> {
        Node::<dyn NodeSpec>::from(self).replace_subtree(src)
    }

    /// Check whether `self` and `other` are the same node.
    ///
    /// For more details, please refer [`Node::is_same_node`] of [`Node<dyn NodeSpec>`]
    pub fn is_same_node(&self, other: impl Into<Node<dyn NodeSpec>>) -> bool {
        let left = Node::<dyn NodeSpec>::from(self);
        let right: Node<dyn NodeSpec> = other.into();
        Rc::ptr_eq(&left.core, &right.core)
    }

    /// Compare the positions of `self` and `other` in the document order.
    ///
    /// For more details, please refer [`Node::compare_document_order`] of [`Node<dyn NodeSpec>`].
    pub fn compare_document_order(
        &self,
        other: impl Into<Node<dyn NodeSpec>>,
    ) -> Option<std::cmp::Ordering> {
        compare_document_order(self.into(), other.into())
    }

    /// Retrieve the base URI according to the [XML Base](https://www.w3.org/TR/xmlbase/).
    ///
    /// For more details, please refer to [Node::base_uri].
    pub fn base_uri(&self) -> Option<URIString> {
        Node::<dyn NodeSpec>::from(self).base_uri()
    }

    /// Please see [Node::text_content].
    pub fn text_content(&self) -> String {
        Node::<dyn NodeSpec>::from(self).text_content()
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
    /// See [Node::append_child].
    pub fn append_child(
        &mut self,
        new_child: impl Into<Node<dyn NodeSpec>>,
    ) -> Result<(), XMLTreeError> {
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

impl From<&Node<dyn NodeSpec>> for Node<dyn NodeSpec> {
    fn from(value: &Node<dyn NodeSpec>) -> Self {
        value.clone()
    }
}

impl From<&mut Node<dyn NodeSpec>> for Node<dyn NodeSpec> {
    fn from(value: &mut Node<dyn NodeSpec>) -> Self {
        value.clone()
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

impl<Spec: NodeSpec + 'static> From<&Node<Spec>> for Node<dyn NodeSpec> {
    fn from(value: &Node<Spec>) -> Self {
        value.clone().into()
    }
}

impl<Spec: NodeSpec + 'static> From<&mut Node<Spec>> for Node<dyn NodeSpec> {
    fn from(value: &mut Node<Spec>) -> Self {
        value.clone().into()
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

impl From<&Node<dyn InternalNodeSpec>> for Node<dyn NodeSpec> {
    fn from(value: &Node<dyn InternalNodeSpec>) -> Self {
        value.clone().into()
    }
}

impl From<&mut Node<dyn InternalNodeSpec>> for Node<dyn NodeSpec> {
    fn from(value: &mut Node<dyn InternalNodeSpec>) -> Self {
        value.clone().into()
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

impl<Spec: InternalNodeSpec + 'static> From<&Node<Spec>> for Node<dyn InternalNodeSpec> {
    fn from(value: &Node<Spec>) -> Self {
        value.clone().into()
    }
}

impl<Spec: InternalNodeSpec + 'static> From<&mut Node<Spec>> for Node<dyn InternalNodeSpec> {
    fn from(value: &mut Node<Spec>) -> Self {
        value.clone().into()
    }
}

impl std::fmt::Display for Node<dyn NodeSpec> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.downcast() {
            NodeKind::AttlistDecl(attlist) => write!(f, "{}", attlist),
            NodeKind::Attribute(attribute) => write!(f, "{}", attribute),
            NodeKind::CDATASection(cdata) => write!(f, "{}", cdata),
            NodeKind::Comment(comment) => write!(f, "{}", comment),
            NodeKind::Document(document) => write!(f, "{}", document),
            NodeKind::DocumentFragment(frag) => write!(f, "{}", frag),
            NodeKind::DocumentType(doctype) => write!(f, "{}", doctype),
            NodeKind::Element(element) => write!(f, "{}", element),
            NodeKind::ElementDecl(element) => write!(f, "{}", element),
            NodeKind::EntityDecl(entity) => write!(f, "{}", entity),
            NodeKind::EntityReference(entref) => write!(f, "{}", entref),
            NodeKind::Namespace(namespace) => write!(f, "{}", namespace),
            NodeKind::NotationDecl(notation) => write!(f, "{}", notation),
            NodeKind::ProcessingInstruction(pi) => write!(f, "{}", pi),
            NodeKind::Text(text) => write!(f, "{}", text),
        }
    }
}

impl std::fmt::Display for Node<dyn InternalNodeSpec> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", Node::<dyn NodeSpec>::from(self.clone()))
    }
}

#[cfg(test)]
mod tests {
    use crate::{sax::parser::XMLReaderBuilder, tree::TreeBuildHandler};

    use super::*;

    #[test]
    fn cyclic_reference_tests() {
        let mut document = Document::new();
        let mut elem = document.create_element("elem", None).unwrap();
        let mut elem2 = document.create_element("elem2", None).unwrap();
        elem.append_child(elem2.clone()).unwrap();
        document.append_child(elem.clone()).unwrap();

        assert!(
            elem2
                .append_child(elem.clone())
                .is_err_and(|err| matches!(err, XMLTreeError::CyclicReference))
        );
        assert!(
            elem.insert_previous_sibling(elem.clone())
                .is_err_and(|err| matches!(err, XMLTreeError::CyclicReference))
        );
        assert!(
            elem.append_child(elem.clone())
                .is_err_and(|err| matches!(err, XMLTreeError::CyclicReference))
        );
        assert!(
            elem.insert_next_sibling(elem.clone())
                .is_err_and(|err| matches!(err, XMLTreeError::CyclicReference))
        );
        assert!(
            elem2
                .append_child(elem2.clone())
                .is_err_and(|err| matches!(err, XMLTreeError::CyclicReference))
        );
        assert!(
            elem2
                .insert_previous_sibling(elem2.clone())
                .is_err_and(|err| matches!(err, XMLTreeError::CyclicReference))
        );
        assert!(
            elem2
                .insert_next_sibling(elem2.clone())
                .is_err_and(|err| matches!(err, XMLTreeError::CyclicReference))
        );
    }

    #[test]
    fn document_order_comparison_tests() {
        let mut parser = XMLReaderBuilder::new()
            .set_handler(TreeBuildHandler::default())
            .build();

        const CASE: &str = r#"<?xml version="1.0"?>
        <!DOCTYPE root [
            <!ELEMENT root   ANY>
            <!ATTLIST root   foo   CDATA #IMPLIED>
            <!ELEMENT child  ANY>
            <!ATTLIST child  bar   CDATA #IMPLIED
                             xmlns CDATA #FIXED "http://example.com/pre"
                             hoge  CDATA #IMPLIED>
            <!ELEMENT child2 ANY>
            <!ATTLIST child2 xmlns CDATA #FIXED "">
            <!ELEMENT child3 ANY>
            <!ENTITY  ent "in entity reference">
        ]>
        <!-- before document element -->
        <?pi before document element?>
        <root foo="foo">
            child of 'foo'
            <?pi in document element1?>
            <!-- in document element1 -->
            <child bar="bar" xmlns="http://example.com/pre" hoge="hoge">
                child of 'child'
                <child2 xmlns=""/>
                <![CDATA[child of 'child2']]>
                &ent;
                <?pi in document element2?>
                <!-- in document element2 -->
                <child3 xml:lang="ch" />
            </child>
        </root>
        <?pi after document element?>
        <!-- after document element -->"#;

        parser.parse_str(CASE, None).unwrap();
        assert!(!parser.handler.fatal_error);
        let document = parser.handler.document;
        let mut children = Some(Node::<dyn NodeSpec>::from(document.clone()));
        while let Some(now) = children {
            let mut others = Some(Node::<dyn NodeSpec>::from(document.clone()));
            let mut pre = true;
            while let Some(other) = others {
                if now.is_same_node(other.clone()) {
                    assert!(now.compare_document_order(other.clone()).unwrap().is_eq());
                    pre = false;
                    if let Some(element) = now.as_element() {
                        for att in element.attributes() {
                            assert!(att.compare_document_order(other.clone()).unwrap().is_gt());
                            assert!(now.compare_document_order(att).unwrap().is_lt());
                        }
                    }
                } else {
                    assert_eq!(
                        pre,
                        now.compare_document_order(other.clone()).unwrap().is_gt()
                    );
                    if let Some(element) = now.as_element() {
                        for att in element.attributes() {
                            assert_eq!(
                                pre,
                                att.compare_document_order(other.clone()).unwrap().is_gt()
                            );
                        }
                    }
                    if let Some(element) = other.as_element() {
                        for att in element.attributes() {
                            assert_eq!(pre, now.compare_document_order(att).unwrap().is_gt());
                        }
                    }
                }

                if let Some(first) = other.first_child() {
                    others = Some(first);
                } else if let Some(next) = other.next_sibling() {
                    others = Some(next);
                } else {
                    others = None;
                    let mut parent = other.parent_node();
                    while let Some(now) = parent {
                        if let Some(next) = now.next_sibling() {
                            others = Some(next);
                            break;
                        }
                        parent = now.parent_node();
                    }
                }
            }

            if let Some(first) = now.first_child() {
                children = Some(first);
            } else if let Some(next) = now.next_sibling() {
                children = Some(next);
            } else {
                children = None;
                let mut parent = now.parent_node();
                while let Some(now) = parent {
                    if let Some(next) = now.next_sibling() {
                        children = Some(next);
                        break;
                    }
                    parent = now.parent_node();
                }
            }
        }
    }

    #[test]
    fn base_uri_tests() {
        // Testing XML Base Conformance
        // https://www.w3.org/XML/2006/12/xmlbase-testing.html

        const CASES: &[&str] = &[
            r#"<elt xml:base="http://www.example.org/~Dürst/"/>"#,
            r#"<outer xml:base="http://www.example.org/one/two"> <inner xml:base=""/> </outer>"#,
            r#"<elt xml:base="http://www.example.org/one/two#frag"/>"#,
            r##"<outer xml:base="http://www.example.org/one/two"> <inner xml:base="#frag"/> </outer>"##,
        ];
        const TARGETS: &[&str] = &["elt", "inner", "elt", "inner"];
        const EXPECTED: &[&str] = &[
            "http://www.example.org/~Dürst/",
            "http://www.example.org/one/two",
            "http://www.example.org/one/two",
            "http://www.example.org/one/two",
        ];

        let mut parser = XMLReaderBuilder::new()
            .set_handler(TreeBuildHandler::default())
            .build();
        for (i, &case) in CASES.iter().enumerate() {
            parser.parse_str(case, None).unwrap();
            let document = parser.handler.document.clone();
            for elem in document.get_elements_by_qname(TARGETS[i]) {
                assert_eq!(
                    elem.base_uri().unwrap().as_unescaped_str().unwrap(),
                    EXPECTED[i]
                );
            }
        }
    }
}
