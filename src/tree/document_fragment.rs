use std::{cell::RefCell, rc::Rc};

use crate::tree::{
    Document, NodeType, XMLTreeError,
    convert::NodeKind,
    node::{InternalNodeSpec, Node, NodeCore, NodeSpec},
};

enum State {
    // cannot be determined
    None,
    // has only one element, but it is unclear whether content or document element
    HasAnElement,
    // has a document type
    HasDocumentType,
    // has document type and document element
    AsDocument,
    // has character data and zero or one element
    AsContent { elem: usize, text: usize },
    // has declarations
    AsDocumentType { decl: usize },
}

pub struct DocumentFragmentSpec {
    first_child: Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>>,
    last_child: Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>>,

    state: State,
}

impl NodeSpec for DocumentFragmentSpec {
    fn node_type(&self) -> NodeType {
        NodeType::DocumentFragment
    }

    fn first_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>> {
        self.first_child.clone()
    }

    fn last_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>> {
        self.last_child.clone()
    }
}

impl InternalNodeSpec for DocumentFragmentSpec {
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

    fn pre_child_removal(&mut self, removed_child: Node<dyn NodeSpec>) -> Result<(), XMLTreeError> {
        match removed_child.node_type() {
            NodeType::AttlistDecl
            | NodeType::ElementDecl
            | NodeType::EntityDecl
            | NodeType::NotationDecl => match &mut self.state {
                State::AsDocumentType { decl } => {
                    *decl -= 1;
                    if *decl == 0 {
                        self.state = State::None;
                    }
                }
                _ => unreachable!(),
            },
            NodeType::DocumentType => match self.state {
                State::AsDocument => self.state = State::HasAnElement,
                State::HasDocumentType => self.state = State::None,
                _ => unreachable!(),
            },
            NodeType::Element => match &mut self.state {
                State::AsContent { elem, text } => {
                    *elem -= 1;
                    if *elem == 0 && *text == 0 {
                        self.state = State::None;
                    }
                }
                State::AsDocument => self.state = State::HasDocumentType,
                State::HasAnElement => self.state = State::None,
                _ => unreachable!(),
            },
            NodeType::CDATASection | NodeType::Text => match &mut self.state {
                State::AsContent { elem, text } => {
                    *text -= 1;
                    if *elem == 0 && *text == 0 {
                        self.state = State::None;
                    }
                }
                _ => unreachable!(),
            },
            NodeType::EntityReference => match &mut self.state {
                State::AsContent { elem, text } => {
                    *text -= 1;
                    if *elem == 0 && *text == 0 {
                        self.state = State::None;
                    }
                }
                State::AsDocumentType { decl } => {
                    *decl -= 1;
                    if *decl == 0 {
                        self.state = State::None;
                    }
                }
                _ => unreachable!(),
            },
            NodeType::Comment | NodeType::ProcessingInstruction => {}
            _ => {}
        }
        Ok(())
    }

    fn pre_child_insertion(
        &self,
        inserted_child: Node<dyn NodeSpec>,
        mut preceding_node: Option<Node<dyn NodeSpec>>,
    ) -> Result<(), super::XMLTreeError> {
        match inserted_child.downcast() {
            NodeKind::AttlistDecl(_)
            | NodeKind::ElementDecl(_)
            | NodeKind::EntityDecl(_)
            | NodeKind::NotationDecl(_) => {
                if !matches!(self.state, State::None | State::AsDocumentType { .. }) {
                    return Err(XMLTreeError::UnacceptableHierarchy);
                }
            }
            NodeKind::DocumentType(_) => match self.state {
                State::AsContent { .. }
                | State::AsDocument
                | State::AsDocumentType { .. }
                | State::HasDocumentType => {
                    return Err(XMLTreeError::UnacceptableHierarchy);
                }
                State::HasAnElement => {
                    while let Some(prev) = preceding_node {
                        preceding_node = prev.previous_sibling();
                        if matches!(prev.node_type(), NodeType::Element) {
                            return Err(XMLTreeError::UnacceptableHorizontality);
                        }
                    }
                }
                State::None => {}
            },
            NodeKind::Element(_) => match self.state {
                State::HasDocumentType => {
                    while let Some(prev) = preceding_node.as_ref() {
                        if matches!(prev.node_type(), NodeType::DocumentType) {
                            break;
                        }
                        preceding_node = prev.previous_sibling();
                    }

                    if preceding_node.is_none() {
                        return Err(XMLTreeError::UnacceptableHorizontality);
                    }
                }
                State::AsDocument | State::AsDocumentType { .. } => {
                    return Err(XMLTreeError::UnacceptableHierarchy);
                }
                State::AsContent { .. } | State::HasAnElement | State::None => {}
            },
            NodeKind::CDATASection(_) | NodeKind::Text(_) => {
                if !matches!(
                    self.state,
                    State::AsContent { .. } | State::HasAnElement | State::None
                ) {
                    return Err(XMLTreeError::UnacceptableHierarchy);
                }
            }
            NodeKind::EntityReference(entity) => {
                // TODO: support parameter entities
                #[allow(clippy::if_same_then_else)]
                if entity.name().starts_with('%') {
                    return Err(XMLTreeError::Unsupported);
                } else if !matches!(
                    self.state,
                    State::AsContent { .. } | State::HasAnElement | State::None
                ) {
                    return Err(XMLTreeError::UnacceptableHierarchy);
                }
            }
            NodeKind::Comment(_) | NodeKind::ProcessingInstruction(_) => {}
            _ => return Err(XMLTreeError::UnacceptableHierarchy),
        }
        Ok(())
    }

    fn post_child_insertion(&mut self, inserted_child: Node<dyn NodeSpec>) {
        match inserted_child.downcast() {
            NodeKind::AttlistDecl(_)
            | NodeKind::ElementDecl(_)
            | NodeKind::EntityDecl(_)
            | NodeKind::NotationDecl(_) => match &mut self.state {
                State::AsDocumentType { decl } => *decl += 1,
                _ => self.state = State::AsDocumentType { decl: 1 },
            },
            NodeKind::DocumentType(_) => {
                if matches!(self.state, State::None) {
                    self.state = State::HasDocumentType;
                } else {
                    self.state = State::AsDocument;
                }
            }
            NodeKind::Element(_) => match &mut self.state {
                State::HasAnElement => self.state = State::AsContent { elem: 2, text: 0 },
                State::AsContent { elem, .. } => *elem += 1,
                State::None => self.state = State::HasAnElement,
                State::HasDocumentType => self.state = State::AsDocument,
                State::AsDocument | State::AsDocumentType { .. } => unreachable!(),
            },
            NodeKind::CDATASection(_) | NodeKind::Text(_) => match &mut self.state {
                State::AsContent { text, .. } => *text += 1,
                State::HasAnElement => self.state = State::AsContent { elem: 1, text: 1 },
                State::None => self.state = State::AsContent { elem: 0, text: 1 },
                _ => unreachable!(),
            },
            NodeKind::EntityReference(entity) => {
                if entity.name().starts_with('%') {
                    match &mut self.state {
                        State::AsDocumentType { decl } => *decl += 1,
                        State::None => self.state = State::AsDocumentType { decl: 1 },
                        _ => unreachable!(),
                    }
                } else {
                    match &mut self.state {
                        State::AsContent { text, .. } => *text += 1,
                        State::HasAnElement => self.state = State::AsContent { elem: 1, text: 1 },
                        State::None => self.state = State::AsContent { elem: 0, text: 1 },
                        _ => unreachable!(),
                    }
                }
            }
            NodeKind::Comment(_) | NodeKind::ProcessingInstruction(_) => {}
            _ => {}
        }
    }
}

pub type DocumentFragment = Node<DocumentFragmentSpec>;

impl DocumentFragment {
    pub(crate) fn new(owner_document: Document) -> Self {
        Self {
            core: Rc::new(RefCell::new(NodeCore {
                parent_node: owner_document.core.borrow().parent_node.clone(),
                previous_sibling: owner_document.core.borrow().previous_sibling.clone(),
                next_sibling: None,
                spec: DocumentFragmentSpec {
                    first_child: None,
                    last_child: None,
                    state: State::None,
                },
            })),
            owner_document: owner_document.core.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn document_fragment_doctype_insertion_test() {
        let document = Document::new();

        let mut doctype = document.create_document_type("root", None, None);
        let mut elem1 = document.create_element("elem1", None).unwrap();
        let elem2 = document.create_element("elem2", None).unwrap();
        let mut frag = document.create_document_fragment();

        //       frag
        //      /    \
        // doctype  elem1
        frag.append_child(doctype.clone()).unwrap();
        frag.append_child(elem1.clone()).unwrap();
        assert!(frag.append_child(elem2.clone()).is_err());
        assert!(
            frag.first_child()
                .and_then(|ch| ch.as_document_type())
                .is_some()
        );
        assert!(
            frag.last_child()
                .and_then(|ch| ch.as_element())
                .is_some_and(|elem| elem.name().as_ref() == "elem1")
        );

        //   frag
        //     |
        //  doctype
        elem1.detach().unwrap();
        assert!(
            frag.first_child()
                .and_then(|ch| ch.as_document_type())
                .is_some()
        );
        assert!(
            frag.last_child()
                .and_then(|ch| ch.as_document_type())
                .is_some()
        );

        assert!(doctype.insert_previous_sibling(elem1.clone()).is_err());
        assert!(
            frag.first_child()
                .and_then(|ch| ch.as_document_type())
                .is_some()
        );
        assert!(
            frag.last_child()
                .and_then(|ch| ch.as_document_type())
                .is_some()
        );

        doctype.detach().unwrap();
        assert!(frag.first_child().is_none());
        assert!(frag.last_child().is_none());

        frag.append_child(elem1.clone()).unwrap();
        assert!(
            frag.first_child()
                .and_then(|ch| ch.as_element())
                .is_some_and(|elem| elem.name().as_ref() == "elem1")
        );
        assert!(
            frag.last_child()
                .and_then(|ch| ch.as_element())
                .is_some_and(|elem| elem.name().as_ref() == "elem1")
        );
        frag.insert_previous_sibling(doctype.clone()).unwrap();

        doctype.detach().unwrap();

        assert!(frag.append_child(doctype.clone()).is_err());

        frag.append_child(elem2).unwrap();
        assert!(
            frag.first_child()
                .and_then(|ch| ch.as_element())
                .is_some_and(|elem| elem.name().as_ref() == "elem1")
        );
        assert!(
            frag.last_child()
                .and_then(|ch| ch.as_element())
                .is_some_and(|elem| elem.name().as_ref() == "elem2")
        );
        assert!(elem1.insert_previous_sibling(doctype).is_err());
    }

    #[test]
    fn document_fragment_insertion_to_other_test() {
        let document = Document::new();

        let mut elem = document.create_element("root", None).unwrap();
        let mut frag = document.create_document_fragment();

        frag.append_child(document.create_element("child1", None).unwrap())
            .unwrap();
        frag.append_child(document.create_text("text1")).unwrap();
        frag.append_child(document.create_element("child2", None).unwrap())
            .unwrap();

        elem.append_child(frag.clone()).unwrap();

        let mut children = elem.first_child();
        for expect in ["child1", "text1", "child2"] {
            match children.as_ref().unwrap().downcast() {
                NodeKind::Element(elem) => {
                    assert_eq!(elem.name().as_ref(), expect);
                }
                NodeKind::Text(text) => {
                    assert_eq!(&*text.data(), expect);
                }
                _ => unreachable!(),
            }
            children = children.unwrap().next_sibling();
        }
        assert!(children.is_none());
        assert!(frag.first_child().is_none());
        assert!(frag.last_child().is_none());
    }

    #[test]
    fn document_fragment_wrong_insertion_to_other_test() {
        let mut document = Document::new();

        let mut frag = document.create_document_fragment();
        frag.append_child(document.create_comment("comment1"))
            .unwrap();
        frag.append_child(document.create_comment("comment2"))
            .unwrap();
        frag.append_child(document.create_text("text1")).unwrap();
        frag.append_child(document.create_text("text2")).unwrap();
        frag.append_child(document.create_comment("comment3"))
            .unwrap();

        assert!(document.append_child(frag.clone()).is_err());
        assert!(document.first_child().is_none());
        assert!(document.last_child().is_none());

        let mut children = frag.first_child();
        for expect in ["comment1", "comment2", "text1", "text2", "comment3"] {
            match children.as_ref().expect(expect).downcast() {
                NodeKind::Comment(comment) => {
                    assert_eq!(&*comment.data(), expect);
                }
                NodeKind::Text(text) => {
                    assert_eq!(&*text.data(), expect);
                }
                _ => unreachable!(),
            }
            children = children.unwrap().next_sibling();
        }
        assert!(children.is_none());
    }
}
