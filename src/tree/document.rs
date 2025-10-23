use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

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
    uri::{URIStr, URIString},
};

pub struct DocumentSpec {
    first_child: Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>>,
    last_child: Option<Rc<RefCell<NodeCore<dyn NodeSpec>>>>,

    document_element: Option<Rc<RefCell<NodeCore<ElementSpec>>>>,
    document_type: Option<Rc<RefCell<NodeCore<DocumentTypeSpec>>>>,

    version: Option<Rc<str>>,
    encoding: Option<Rc<str>>,
    standalone: Option<bool>,
    base_uri: Rc<URIStr>,
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
        &self,
        inserted_child: Node<dyn NodeSpec>,
        mut preceding_node: Option<Node<dyn NodeSpec>>,
    ) -> Result<(), XMLTreeError> {
        match inserted_child.node_type() {
            NodeType::DocumentType => {
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
            }
            NodeType::Element => {
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
            }
            NodeType::Comment | NodeType::ProcessingInstruction => {}
            _ => return Err(XMLTreeError::UnacceptableHierarchy),
        }
        Ok(())
    }

    fn post_child_insertion(&mut self, inserted_child: Node<dyn NodeSpec>) {
        match inserted_child.downcast() {
            NodeKind::DocumentType(doctype) => {
                self.document_type = Some(doctype.core);
            }
            NodeKind::Element(element) => {
                self.document_element = Some(element.core);
            }
            _ => {}
        }
    }
}

/// The root node of the document tree that represents the XML document itself.
///
/// It mostly covers the information provided by the "Document Information Item"
/// in the [XML Infoset](https://www.w3.org/TR/xml-infoset/).
///
/// # Reference
/// [2.1. The Document Information Item](https://www.w3.org/TR/xml-infoset/#infoitem.document)
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
        name: impl Into<Rc<str>>,
        system_id: Option<Rc<URIStr>>,
        public_id: Option<Rc<str>>,
    ) -> DocumentType {
        DocumentType::new(name.into(), system_id, public_id, self.clone())
    }

    pub fn create_element(
        &self,
        qname: impl Into<Rc<str>>,
        namespace_name: Option<Rc<str>>,
    ) -> Result<Element, XMLTreeError> {
        Element::new(qname.into(), namespace_name, self.clone())
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
        target: impl Into<Rc<str>>,
        data: Option<Rc<str>>,
    ) -> ProcessingInstruction {
        ProcessingInstruction::new(target.into(), data, self.clone())
    }

    pub fn create_entity_reference(&self, name: impl Into<Rc<str>>) -> EntityReference {
        // TODO: try to expand contents
        EntityReference::new(name.into(), self.clone())
    }

    pub fn create_document_fragment(&self) -> DocumentFragment {
        DocumentFragment::new(self.clone())
    }

    pub fn create_attlist_decl(
        &self,
        elem_name: impl Into<Rc<str>>,
        attr_name: impl Into<Rc<str>>,
        attr_type: AttributeType,
        default_decl: DefaultDecl,
    ) -> AttlistDecl {
        AttlistDecl::new(
            elem_name.into(),
            attr_name.into(),
            attr_type,
            default_decl,
            self.clone(),
        )
    }

    pub fn create_element_decl(
        &self,
        name: impl Into<Rc<str>>,
        content_spec: ContentSpec,
    ) -> ElementDecl {
        ElementDecl::new(name.into(), content_spec, self.clone())
    }

    pub fn create_internal_entity_decl(
        &self,
        name: impl Into<Rc<str>>,
        value: impl Into<Rc<str>>,
    ) -> EntityDecl {
        EntityDecl::new_internal_entity_decl(name.into(), value.into(), self.clone())
    }

    pub fn create_external_entity_decl(
        &self,
        name: impl Into<Rc<str>>,
        system_id: impl Into<Rc<URIStr>>,
        public_id: Option<Rc<str>>,
    ) -> EntityDecl {
        EntityDecl::new_external_entity_decl(name.into(), system_id.into(), public_id, self.clone())
    }

    pub fn create_unparsed_entity_decl(
        &self,
        name: impl Into<Rc<str>>,
        system_id: impl Into<Rc<URIStr>>,
        public_id: Option<Rc<str>>,
        notation_name: impl Into<Rc<str>>,
    ) -> EntityDecl {
        EntityDecl::new_unparsed_entity_decl(
            name.into(),
            system_id.into(),
            public_id,
            notation_name.into(),
            self.clone(),
        )
    }

    pub fn create_notation_decl(
        &self,
        name: impl Into<Rc<str>>,
        system_id: Option<Rc<URIStr>>,
        public_id: Option<Rc<str>>,
    ) -> NotationDecl {
        NotationDecl::new(name.into(), system_id, public_id, self.clone())
    }

    /// Returns the document element. If no document element exists, return [`None`].
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

    /// Returns the document type. If no document type exists, return [`None`].
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
    /// Otherwise, return [`None`].
    pub fn version(&self) -> Option<Rc<str>> {
        self.core.borrow().spec.version.clone()
    }

    pub fn set_version(&mut self, version: Option<&str>) {
        self.core.borrow_mut().spec.version = version.map(|version| version.into());
    }

    /// If XML declaration is present and it has the encoding declaration,
    /// return the encoding specified in the declaration.  \
    /// Otherwise, return [`None`].
    pub fn encoding(&self) -> Option<Rc<str>> {
        self.core.borrow().spec.encoding.clone()
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

    /// Return the base URI of this document.
    ///
    /// # Note
    /// The [`Node::base_uri`] method is common to all node types and,
    /// due to implementation constraints, returns a [`URIString`].  \
    /// URI strings are often short, so performance differences should rarely be noticeable.
    /// However, since no allocation occurs, [`Document::document_base_uri`] might be more efficient.
    pub fn document_base_uri(&self) -> Rc<URIStr> {
        self.core.borrow().spec.base_uri.clone()
    }

    /// Set the base URI for the document.
    ///
    /// The base URI must be an absolute URI.
    pub fn set_document_base_uri(
        &mut self,
        base_uri: impl Into<Rc<URIStr>>,
    ) -> Result<(), XMLTreeError> {
        let base_uri = base_uri.into();
        if !base_uri.is_absolute() {
            return Err(XMLTreeError::BaseURINotAbsolute);
        }

        self.core.borrow_mut().spec.base_uri = base_uri;
        Ok(())
    }

    /// Returns the element with an ID attribute whose attribute value is `id`.
    ///
    /// For invalid XML documents, multiple elements may have the same ID attribute value.  \
    /// In such cases, the earliest element appearing in document order is returned.
    pub fn get_element_by_id(&self, id: &str) -> Option<Element> {
        let mut children = self.document_element().map(Node::<dyn NodeSpec>::from);
        while let Some(child) = children {
            if let Some(element) = child.as_element() {
                for att in element.attributes().filter(|att| att.is_id()) {
                    if att.value() == id {
                        return Some(element);
                    }
                }
            }
            if let Some(first) = child.first_child() {
                children = Some(first);
            } else if let Some(next) = child.next_sibling() {
                children = Some(next);
            } else {
                children = None;
                let mut parent = child.parent_node();
                while let Some(now) = parent {
                    if let Some(next) = now.next_sibling() {
                        children = Some(next);
                        break;
                    }
                    parent = now.parent_node();
                }
            }
        }
        None
    }

    /// Returns the elements whose QName is `qname`.
    pub fn get_element_by_qname(&self, qname: &str) -> impl Iterator<Item = Element> {
        let mut children = self.document_element().map(Node::<dyn NodeSpec>::from);
        let mut ret = vec![];
        while let Some(child) = children {
            if let Some(element) = child.as_element()
                && element.name().as_ref() == qname
            {
                ret.push(element);
            }
            if let Some(first) = child.first_child() {
                children = Some(first);
            } else if let Some(next) = child.next_sibling() {
                children = Some(next);
            } else {
                children = None;
                let mut parent = child.parent_node();
                while let Some(now) = parent {
                    if let Some(next) = now.next_sibling() {
                        children = Some(next);
                        break;
                    }
                    parent = now.parent_node();
                }
            }
        }
        ret.into_iter()
    }

    /// Returns the elements whose QName is `qname`.
    pub fn get_element_by_expanded_name(
        &self,
        local_name: &str,
        namespace_name: Option<&str>,
    ) -> impl Iterator<Item = Element> {
        let mut children = self.document_element().map(Node::<dyn NodeSpec>::from);
        let mut ret = vec![];
        while let Some(child) = children {
            if let Some(element) = child.as_element()
                && element.local_name().as_ref() == local_name
                && element.namespace_name().as_deref() == namespace_name
            {
                ret.push(element);
            }
            if let Some(first) = child.first_child() {
                children = Some(first);
            } else if let Some(next) = child.next_sibling() {
                children = Some(next);
            } else {
                children = None;
                let mut parent = child.parent_node();
                while let Some(now) = parent {
                    if let Some(next) = now.next_sibling() {
                        children = Some(next);
                        break;
                    }
                    parent = now.parent_node();
                }
            }
        }
        ret.into_iter()
    }
}

impl Default for Document {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Display for Document {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut top_of_document = true;
        if self.version().is_some() || self.encoding().is_some() || self.standalone().is_some() {
            write!(
                f,
                "<?xml version=\"{}\"",
                self.version().as_deref().unwrap_or("1.0")
            )?;
            if let Some(encoding) = self.encoding() {
                write!(f, " encoding=\"{}\"", encoding)?;
            }
            if let Some(standalone) = self.standalone() {
                if standalone {
                    write!(f, " standalone=\"yes\"")?;
                } else {
                    write!(f, " standalone=\"no\"")?;
                }
            }
            write!(f, "?>")?;
            top_of_document = false;
        }

        let mut children = self.first_child();
        while let Some(child) = children {
            children = child.next_sibling();
            if !top_of_document {
                writeln!(f)?;
            }
            write!(f, "{}", child)?;
            top_of_document = false;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn document_element_insertion_test() {
        let mut document = Document::new();
        assert!(document.document_element().is_none());
        assert!(document.first_child().is_none());
        assert!(document.last_child().is_none());
        let mut elem = document.create_element("root", None).unwrap();
        document.append_child(elem.clone()).unwrap();
        assert!(document.document_element().is_some());
        assert!(document.document_element().is_some());
        assert!(document.document_element().is_some());

        let elem2 = document.create_element("root2", None).unwrap();
        assert!(document.append_child(elem2.clone()).is_err());
        assert!(
            document
                .document_element()
                .is_some_and(|elem| elem.name().as_ref() == "root")
        );
        assert!(
            document
                .first_child()
                .is_some_and(|elem| elem.as_element().unwrap().name().as_ref() == "root")
        );
        assert!(
            document
                .last_child()
                .is_some_and(|elem| elem.as_element().unwrap().name().as_ref() == "root")
        );
        assert!(
            elem.parent_node()
                .is_some_and(|doc| matches!(doc.node_type(), NodeType::Document))
        );
        assert!(elem2.parent_node().is_none());

        elem.detach().unwrap();

        assert!(document.document_element().is_none());
        assert!(document.first_child().is_none());
        assert!(document.last_child().is_none());
        assert!(elem.parent_node().is_none());
    }

    #[test]
    fn document_type_insertion_test() {
        let mut document = Document::new();
        let mut doctype = document.create_document_type("root", None, None);
        document.append_child(doctype.clone()).unwrap();
        assert!(document.document_type().is_some());
        assert!(
            document.first_child().is_some_and(|doctype| &*doctype
                .as_document_type()
                .unwrap()
                .name()
                == "root")
        );
        assert!(
            document.last_child().is_some_and(|doctype| &*doctype
                .as_document_type()
                .unwrap()
                .name()
                == "root")
        );

        let doctype2 = document.create_document_type("root2", None, None);
        assert!(document.append_child(doctype2).is_err());
        assert!(
            document
                .document_type()
                .is_some_and(|doctype| &*doctype.name() == "root")
        );
        assert!(
            document.first_child().is_some_and(|doctype| &*doctype
                .as_document_type()
                .unwrap()
                .name()
                == "root")
        );
        assert!(
            document.last_child().is_some_and(|doctype| &*doctype
                .as_document_type()
                .unwrap()
                .name()
                == "root")
        );

        doctype.detach().unwrap();
        assert!(document.document_type().is_none());
        assert!(document.first_child().is_none());
        assert!(document.last_child().is_none());
        assert!(doctype.parent_node().is_none());
    }

    #[test]
    fn document_element_insertion_before_document_type_test() {
        let mut document = Document::new();
        let mut doctype = document.create_document_type("root", None, None);
        let root = document.create_element("root", None).unwrap();
        document.append_child(doctype.clone()).unwrap();
        assert!(doctype.insert_previous_sibling(root.clone()).is_err());

        assert!(document.document_type().is_some());
        assert!(document.document_element().is_none());
        assert!(
            document
                .first_child()
                .is_some_and(|doctype| matches!(doctype.node_type(), NodeType::DocumentType))
        );
        assert!(
            document
                .last_child()
                .is_some_and(|doctype| matches!(doctype.node_type(), NodeType::DocumentType))
        );
        assert!(doctype.parent_node().is_some());
        assert!(doctype.previous_sibling().is_none());
        assert!(root.parent_node().is_none());
        assert!(root.next_sibling().is_none());
    }

    #[test]
    fn document_type_insertion_after_document_element_test() {
        let mut document = Document::new();
        let doctype = document.create_document_type("root", None, None);
        let mut root = document.create_element("root", None).unwrap();
        document.append_child(root.clone()).unwrap();
        assert!(root.insert_next_sibling(doctype.clone()).is_err());

        assert!(document.document_type().is_none());
        assert!(document.document_element().is_some());
        assert!(
            document
                .first_child()
                .is_some_and(|root| matches!(root.node_type(), NodeType::Element))
        );
        assert!(
            document
                .last_child()
                .is_some_and(|root| matches!(root.node_type(), NodeType::Element))
        );
        assert!(doctype.parent_node().is_none());
        assert!(doctype.previous_sibling().is_none());
        assert!(root.parent_node().is_some());
        assert!(root.next_sibling().is_none());
    }
}
