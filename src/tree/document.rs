use std::{
    cell::{Ref, RefCell},
    rc::{Rc, Weak},
};

use crate::tree::{
    DocumentFragment, DocumentType, Element, NodeType, Text, XMLTreeError,
    document_fragment::DocumentFragmentSpec,
    document_type::DocumentTypeSpec,
    element::ElementSpec,
    node::{GeneralInternalNodeSpec, InternalNodeType, Node, NodeCore},
};

pub struct DocumentSpecificData {
    document_element: Option<Rc<RefCell<NodeCore<ElementSpec>>>>,
    document_type: Option<Rc<RefCell<NodeCore<DocumentTypeSpec>>>>,

    version: Option<Box<str>>,
    encoding: Option<Box<str>>,
    standalone: Option<bool>,
}

impl InternalNodeType for DocumentSpecificData {
    fn node_type(&self) -> super::NodeType {
        NodeType::Document
    }
}

pub type DocumentSpec = GeneralInternalNodeSpec<DocumentSpecificData>;
pub type Document = Node<DocumentSpec>;

impl Document {
    pub fn new() -> Self {
        let weak: Weak<RefCell<NodeCore<DocumentFragmentSpec>>> = Weak::new();
        let rc = Rc::new(RefCell::new(NodeCore {
            parent_node: weak.clone(),
            previous_sibling: weak.clone(),
            next_sibling: None,
            spec: GeneralInternalNodeSpec {
                first_child: None,
                last_child: None,
                data: DocumentSpecificData {
                    document_element: None,
                    document_type: None,
                    version: None,
                    encoding: None,
                    standalone: None,
                },
            },
        }));

        Self {
            core: rc.clone(),
            owner_document: rc.clone(),
        }
    }

    pub fn create_element(
        &self,
        qname: Rc<str>,
        namespace_name: Option<Rc<str>>,
    ) -> Result<Element, XMLTreeError> {
        Element::new(qname, namespace_name, self.clone())
    }

    pub fn create_text(&self, data: impl Into<String>) -> Text {
        Text::new(data.into(), self.clone())
    }

    pub fn create_document_fragment(&self) -> DocumentFragment {
        DocumentFragment::new(self.clone())
    }

    pub fn document_element(&self) -> Option<Element> {
        self.core
            .borrow()
            .spec
            .data
            .document_element
            .clone()
            .map(|core| Element {
                core,
                owner_document: self.owner_document.clone(),
            })
    }

    pub fn document_type(&self) -> Option<DocumentType> {
        self.core
            .borrow()
            .spec
            .data
            .document_type
            .clone()
            .map(|core| DocumentType {
                core,
                owner_document: self.owner_document.clone(),
            })
    }

    /// If XML declaration is present, return the version specified in the declaration.  \
    /// Otherwise, return `None`.
    pub fn version(&self) -> Option<Ref<'_, str>> {
        Ref::filter_map(self.core.borrow(), |core| core.spec.data.version.as_deref()).ok()
    }

    /// If XML declaration is present and it has the encoding declaration,
    /// return the encoding specified in the declaration.  \
    /// Otherwise, return `None`.
    pub fn encoding(&self) -> Option<Ref<'_, str>> {
        Ref::filter_map(self.core.borrow(), |core| {
            core.spec.data.encoding.as_deref()
        })
        .ok()
    }

    /// If XML declaration is present and it has the standalone declaration,
    /// return the boolean value specified in the declaration.  \
    /// Otherwise, return `None`.
    pub fn standalone(&self) -> Option<bool> {
        self.core.borrow().spec.data.standalone
    }
}

impl Default for Document {
    fn default() -> Self {
        Self::new()
    }
}
