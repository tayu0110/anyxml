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
    AsContent,
    // has declarations
    AsDocumentType,
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
                if !matches!(self.state, State::None | State::AsDocumentType) {
                    return Err(XMLTreeError::UnacceptableHierarchy);
                }
            }
            NodeKind::DocumentType(_) => match self.state {
                State::AsContent
                | State::AsDocument
                | State::AsDocumentType
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
                State::AsDocument | State::AsDocumentType => {
                    return Err(XMLTreeError::UnacceptableHierarchy);
                }
                State::AsContent | State::HasAnElement | State::None => {}
            },
            NodeKind::CDATASection(_) | NodeKind::Text(_) => {
                if !matches!(
                    self.state,
                    State::AsContent | State::HasAnElement | State::None
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
                    State::AsContent | State::HasAnElement | State::None
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
            | NodeKind::NotationDecl(_) => {
                self.state = State::AsDocumentType;
            }
            NodeKind::DocumentType(_) => {
                if matches!(self.state, State::None) {
                    self.state = State::HasDocumentType;
                } else {
                    self.state = State::AsDocument;
                }
            }
            NodeKind::Element(_) => match self.state {
                State::HasAnElement | State::AsContent => self.state = State::AsContent,
                State::None => self.state = State::HasAnElement,
                State::HasDocumentType => self.state = State::AsDocument,
                State::AsDocument | State::AsDocumentType => unreachable!(),
            },
            NodeKind::CDATASection(_) | NodeKind::Text(_) => {
                self.state = State::AsContent;
            }
            NodeKind::EntityReference(entity) => {
                if entity.name().starts_with('%') {
                    self.state = State::AsDocumentType;
                } else {
                    self.state = State::AsContent;
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
