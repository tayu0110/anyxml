use std::{
    cell::{Ref, RefCell},
    rc::{Rc, Weak},
};

use anyxml_uri::uri::{URIStr, URIString};

use crate::{
    sax::{AttributeType, DefaultDecl, contentspec::ContentSpec},
    tree::{
        AttlistDecl, CDATASection, Comment, DocumentFragment, DocumentType, Element, ElementDecl,
        EntityDecl, EntityReference, NodeType, NotationDecl, ProcessingInstruction, Text,
        XMLTreeError,
        convert::NodeKind,
        document_fragment::DocumentFragmentSpec,
        document_type::DocumentTypeSpec,
        element::ElementSpec,
        node::{InternalNodeSpec, Node, NodeCore, NodeSpec},
    },
};

pub struct DocumentSpec {
    first_child: Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>>,
    last_child: Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>>,

    document_element: Option<Rc<RefCell<NodeCore<ElementSpec>>>>,
    document_type: Option<Rc<RefCell<NodeCore<DocumentTypeSpec>>>>,

    version: Option<Box<str>>,
    encoding: Option<Box<str>>,
    standalone: Option<bool>,
    base_uri: Box<URIStr>,
}

impl NodeSpec for DocumentSpec {
    fn node_type(&self) -> NodeType {
        NodeType::Document
    }

    fn first_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>> {
        self.first_child.clone()
    }

    fn last_child(&self) -> Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>> {
        self.last_child.clone()
    }
}

impl InternalNodeSpec for DocumentSpec {
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
            NodeType::Element => {
                self.document_element = None;
            }
            NodeType::DocumentType => {
                self.document_type = None;
            }
            _ => {}
        }
        Ok(())
    }

    fn pre_child_insertion(
        &mut self,
        inserted_child: Node<dyn NodeSpec>,
        mut preceding_node: Option<Node<dyn NodeSpec>>,
    ) -> Result<(), XMLTreeError> {
        match inserted_child.downcast() {
            NodeKind::DocumentType(doctype) => {
                if self.document_type.is_some() {
                    return Err(XMLTreeError::MultipleDocumentType);
                }
                if self.document_element.is_some() {
                    while let Some(now) = preceding_node {
                        if matches!(now.node_type(), NodeType::Element) {
                            return Err(XMLTreeError::UnacceptableHorizontality);
                        }
                        preceding_node = now.previous_sibling();
                    }
                }
                self.document_type = Some(doctype.core);
            }
            NodeKind::Element(element) => {
                if self.document_element.is_some() {
                    return Err(XMLTreeError::MultipleDocumentElement);
                }
                if self.document_type.is_some() {
                    while let Some(now) = preceding_node.as_ref() {
                        if matches!(now.node_type(), NodeType::DocumentType) {
                            break;
                        }
                        preceding_node = now.previous_sibling();
                    }

                    if preceding_node.is_none() {
                        return Err(XMLTreeError::UnacceptableHorizontality);
                    }
                }
                self.document_element = Some(element.core);
            }
            NodeKind::Comment(_) | NodeKind::ProcessingInstruction(_) => {}
            _ => return Err(XMLTreeError::UnacceptableHierarchy),
        }
        Ok(())
    }
}

pub type Document = Node<DocumentSpec>;

impl Document {
    pub fn new() -> Self {
        let weak: Weak<RefCell<NodeCore<DocumentFragmentSpec>>> = Weak::new();
        let rc = Rc::new(RefCell::new(NodeCore {
            parent_node: weak.clone(),
            previous_sibling: weak.clone(),
            next_sibling: None,
            spec: DocumentSpec {
                first_child: None,
                last_child: None,
                document_element: None,
                document_type: None,
                version: None,
                encoding: None,
                standalone: None,
                base_uri: URIString::parse_file_path(std::env::current_dir().unwrap_or_default())
                    .unwrap_or_else(|_| URIString::parse("file:///").unwrap())
                    .resolve(&URIString::parse("document.xml").unwrap())
                    .into(),
            },
        }));

        Self {
            core: rc.clone(),
            owner_document: rc.clone(),
        }
    }

    pub fn create_document_type(
        &self,
        name: Box<str>,
        system_id: Option<Box<URIStr>>,
        public_id: Option<Box<str>>,
    ) -> DocumentType {
        DocumentType::new(name, system_id, public_id, self.clone())
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

    pub fn create_cdata_section(&self, data: impl Into<String>) -> CDATASection {
        CDATASection::new(data.into(), self.clone())
    }

    pub fn create_comment(&self, data: impl Into<String>) -> Comment {
        Comment::new(data.into(), self.clone())
    }

    pub fn create_processing_instruction(
        &self,
        target: Box<str>,
        data: Option<Box<str>>,
    ) -> ProcessingInstruction {
        ProcessingInstruction::new(target, data, self.clone())
    }

    pub fn create_entity_reference(&self, name: Box<str>) -> EntityReference {
        // TODO: try to expand contents
        EntityReference::new(name, self.clone())
    }

    pub fn create_document_fragment(&self) -> DocumentFragment {
        DocumentFragment::new(self.clone())
    }

    pub fn create_attlist_decl(
        &self,
        elem_name: Box<str>,
        attr_name: Box<str>,
        attr_type: AttributeType,
        default_decl: DefaultDecl,
    ) -> AttlistDecl {
        AttlistDecl::new(elem_name, attr_name, attr_type, default_decl, self.clone())
    }

    pub fn create_element_decl(&self, name: Box<str>, content_spec: ContentSpec) -> ElementDecl {
        ElementDecl::new(name, content_spec, self.clone())
    }

    pub fn create_internal_entity_decl(&self, name: Box<str>, value: Box<str>) -> EntityDecl {
        EntityDecl::new_internal_entity_decl(name, value, self.clone())
    }

    pub fn create_external_entity_decl(
        &self,
        name: Box<str>,
        system_id: Box<URIStr>,
        public_id: Option<Box<str>>,
    ) -> EntityDecl {
        EntityDecl::new_external_entity_decl(name, system_id, public_id, self.clone())
    }

    pub fn create_unparsed_entity_decl(
        &self,
        name: Box<str>,
        system_id: Box<URIStr>,
        public_id: Option<Box<str>>,
        notation_name: Box<str>,
    ) -> EntityDecl {
        EntityDecl::new_unparsed_entity_decl(
            name,
            system_id,
            public_id,
            notation_name,
            self.clone(),
        )
    }

    pub fn create_notation_decl(
        &self,
        name: Box<str>,
        system_id: Option<Box<URIStr>>,
        public_id: Option<Box<str>>,
    ) -> NotationDecl {
        NotationDecl::new(name, system_id, public_id, self.clone())
    }

    pub fn document_element(&self) -> Option<Element> {
        self.core
            .borrow()
            .spec
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
        Ref::filter_map(self.core.borrow(), |core| core.spec.version.as_deref()).ok()
    }

    pub fn set_version(&mut self, version: Option<&str>) {
        self.core.borrow_mut().spec.version = version.map(|version| version.into());
    }

    /// If XML declaration is present and it has the encoding declaration,
    /// return the encoding specified in the declaration.  \
    /// Otherwise, return `None`.
    pub fn encoding(&self) -> Option<Ref<'_, str>> {
        Ref::filter_map(self.core.borrow(), |core| core.spec.encoding.as_deref()).ok()
    }

    pub fn set_encoding(&mut self, encoding: Option<&str>) {
        self.core.borrow_mut().spec.encoding = encoding.map(|encoding| encoding.into());
    }

    /// If XML declaration is present and it has the standalone declaration,
    /// return the boolean value specified in the declaration.  \
    /// Otherwise, return `None`.
    pub fn standalone(&self) -> Option<bool> {
        self.core.borrow().spec.standalone
    }

    pub fn set_standalone(&mut self, standalone: Option<bool>) {
        self.core.borrow_mut().spec.standalone = standalone;
    }

    pub fn base_uri(&self) -> Ref<'_, URIStr> {
        Ref::map(self.core.borrow(), |core| core.spec.base_uri.as_ref())
    }

    pub fn set_base_uri(&mut self, base_uri: Box<URIStr>) -> Result<(), XMLTreeError> {
        if !base_uri.is_absolute() {
            return Err(XMLTreeError::BaseURINotAbsolute);
        }

        self.core.borrow_mut().spec.base_uri = base_uri;
        Ok(())
    }
}

impl Default for Document {
    fn default() -> Self {
        Self::new()
    }
}
