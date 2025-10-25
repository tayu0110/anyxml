pub mod attlist_decl;
pub mod attribute;
pub mod cdata_section;
pub mod comment;
pub mod convert;
pub mod document;
pub mod document_fragment;
pub mod document_type;
pub mod element;
pub mod element_decl;
pub mod entity_decl;
pub mod entity_reference;
pub mod namespace;
pub mod node;
pub mod notation_decl;
pub mod processing_instruction;
pub mod text;

use std::sync::Arc;

pub use attlist_decl::AttlistDecl;
pub use attribute::Attribute;
pub use cdata_section::CDATASection;
pub use comment::Comment;
pub use document::Document;
pub use document_fragment::DocumentFragment;
pub use document_type::DocumentType;
pub use element::Element;
pub use element_decl::ElementDecl;
pub use entity_decl::EntityDecl;
pub use entity_reference::EntityReference;
pub use node::Node;
pub use notation_decl::NotationDecl;
pub use processing_instruction::ProcessingInstruction;
pub use text::Text;

use crate::{
    error::XMLError,
    sax::{
        AttributeType, DefaultDecl, Locator,
        attributes::Attributes,
        contentspec::ContentSpec,
        error::SAXParseError,
        handler::{DefaultSAXHandler, EntityResolver, ErrorHandler, SAXHandler},
        source::InputSource,
    },
    tree::{
        convert::NodeKind,
        node::{InternalNodeSpec, NodeSpec},
    },
    uri::URIStr,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NodeType {
    Element = 1,
    Attribute = 2,
    Text = 3,
    CDATASection = 4,
    EntityReference = 5,
    EntityDecl = 6,
    ProcessingInstruction = 7,
    Comment = 8,
    Document = 9,
    DocumentType = 10,
    DocumentFragment = 11,
    NotationDecl = 12,

    ElementDecl = 128,
    AttlistDecl = 129,
    Namespace = 130,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XMLTreeError {
    EmptyPrefix,
    UnacceptablePrefix,
    UnresolvablePrefix,
    AlreadyBoundPrefix,
    UnacceptableNamespaceBinding,
    UnacceptableNamespaceName,
    MultipleDocumentElement,
    DuplicateAttribute,
    UnspecifiedAttribute,
    MultipleDocumentType,
    DuplicateElementDecl,
    DuplicateNotationDecl,
    CyclicReference,
    UnacceptableHierarchy,
    UnacceptableHorizontality,
    BaseURINotAbsolute,
    Unsupported,
}

pub struct TreeBuildHandler<H: SAXHandler = DefaultSAXHandler> {
    node: Node<dyn InternalNodeSpec>,
    pub document: Document,
    pub handler: H,
    pub fatal_error: bool,
    in_cdata: bool,
}

impl<H: SAXHandler> TreeBuildHandler<H> {
    pub fn with_handler(handler: H) -> Self {
        let document = Document::new();
        Self {
            node: document.clone().into(),
            document,
            handler,
            fatal_error: false,
            in_cdata: false,
        }
    }
}

impl Default for TreeBuildHandler {
    fn default() -> Self {
        let document = Document::new();
        Self {
            node: document.clone().into(),
            document,
            handler: DefaultSAXHandler,
            fatal_error: false,
            in_cdata: false,
        }
    }
}

impl<H: SAXHandler> EntityResolver for TreeBuildHandler<H> {
    fn get_external_subset(
        &mut self,
        name: &str,
        base_uri: Option<&URIStr>,
    ) -> Result<InputSource<'static>, XMLError> {
        self.handler.get_external_subset(name, base_uri)
    }

    fn resolve_entity(
        &mut self,
        name: &str,
        public_id: Option<&str>,
        base_uri: &URIStr,
        system_id: &URIStr,
    ) -> Result<InputSource<'static>, XMLError> {
        self.handler
            .resolve_entity(name, public_id, base_uri, system_id)
    }
}

impl<H: SAXHandler> ErrorHandler for TreeBuildHandler<H> {
    fn error(&mut self, error: SAXParseError) {
        self.handler.error(error);
    }

    fn fatal_error(&mut self, error: SAXParseError) {
        self.fatal_error = true;
        self.handler.fatal_error(error);
    }

    fn warning(&mut self, error: SAXParseError) {
        self.handler.warning(error);
    }
}

impl<H: SAXHandler> SAXHandler for TreeBuildHandler<H> {
    fn attribute_decl(
        &mut self,
        element_name: &str,
        attribute_name: &str,
        attribute_type: &AttributeType,
        default_decl: &DefaultDecl,
    ) {
        self.node
            .append_child(self.document.create_attlist_decl(
                element_name,
                attribute_name,
                attribute_type.clone(),
                default_decl.clone(),
            ))
            .unwrap();
        self.handler
            .attribute_decl(element_name, attribute_name, attribute_type, default_decl);
    }

    fn characters(&mut self, data: &str) {
        match self.node.last_child().map(|ch| ch.downcast()) {
            Some(NodeKind::CDATASection(mut cdata)) => {
                if self.in_cdata {
                    cdata.push_str(data);
                } else {
                    self.node
                        .append_child(self.document.create_text(data))
                        .unwrap();
                }
            }
            Some(NodeKind::Text(mut text)) => text.push_str(data),
            _ => self
                .node
                .append_child(self.document.create_text(data))
                .unwrap(),
        }
        self.handler.characters(data);
    }

    fn comment(&mut self, data: &str) {
        self.node
            .append_child(self.document.create_comment(data))
            .unwrap();
        self.handler.comment(data);
    }

    fn declaration(&mut self, version: &str, encoding: Option<&str>, standalone: Option<bool>) {
        self.document.set_version(Some(version));
        self.document.set_encoding(encoding);
        self.document.set_standalone(standalone);
        self.handler.declaration(version, encoding, standalone);
    }

    fn element_decl(&mut self, name: &str, contentspec: &ContentSpec) {
        self.node
            .append_child(self.document.create_element_decl(name, contentspec.clone()))
            .unwrap();
        self.handler.element_decl(name, contentspec);
    }

    fn external_entity_decl(&mut self, name: &str, public_id: Option<&str>, system_id: &URIStr) {
        self.node
            .append_child(self.document.create_external_entity_decl(
                name,
                system_id,
                public_id.map(|id| id.into()),
            ))
            .unwrap();
        self.handler
            .external_entity_decl(name, public_id, system_id);
    }

    fn ignorable_whitespace(&mut self, data: &str) {
        match self.node.last_child().map(|ch| ch.downcast()) {
            Some(NodeKind::CDATASection(mut cdata)) => {
                if self.in_cdata {
                    cdata.push_str(data);
                } else {
                    self.node
                        .append_child(self.document.create_text(data))
                        .unwrap();
                }
            }
            Some(NodeKind::Text(mut text)) => text.push_str(data),
            _ => self
                .node
                .append_child(self.document.create_text(data))
                .unwrap(),
        }
        self.handler.ignorable_whitespace(data);
    }

    fn internal_entity_decl(&mut self, name: &str, value: &str) {
        self.node
            .append_child(self.document.create_internal_entity_decl(name, value))
            .unwrap();
        self.handler.internal_entity_decl(name, value);
    }

    fn notation_decl(&mut self, name: &str, public_id: Option<&str>, system_id: Option<&URIStr>) {
        self.node
            .append_child(self.document.create_notation_decl(
                name,
                system_id.map(|id| id.into()),
                public_id.map(|id| id.into()),
            ))
            .unwrap();
        self.handler.notation_decl(name, public_id, system_id);
    }

    fn processing_instruction(&mut self, target: &str, data: Option<&str>) {
        self.node
            .append_child(
                self.document
                    .create_processing_instruction(target, data.map(|data| data.into())),
            )
            .unwrap();
        self.handler.processing_instruction(target, data);
    }

    fn set_document_locator(&mut self, locator: Arc<Locator>) {
        self.document = Document::new();
        self.node = self.document.clone().into();
        self.document
            .set_document_base_uri(locator.system_id().as_ref())
            .ok();
        self.handler.set_document_locator(locator);
    }

    fn skipped_entity(&mut self, name: &str) {
        if name != "[dtd]" {
            // we should create an empty entity reference,
            // so we should use `EntityReference::new`, not but `self.document.create_entity_reference`.
            self.node
                .append_child(EntityReference::new(name.into(), self.document.clone()))
                .unwrap();
        }
        self.handler.skipped_entity(name);
    }

    fn start_cdata(&mut self) {
        self.in_cdata = true;
        self.node
            .append_child(self.document.create_cdata_section(""))
            .unwrap();
        self.handler.start_cdata();
    }
    fn end_cdata(&mut self) {
        self.in_cdata = false;
        self.handler.end_cdata();
    }

    fn start_document(&mut self) {
        self.handler.start_document();
    }
    fn end_document(&mut self) {
        self.handler.end_document();
    }

    fn start_dtd(&mut self, name: &str, public_id: Option<&str>, system_id: Option<&URIStr>) {
        let doctype = self.document.create_document_type(
            name,
            system_id.map(|id| id.into()),
            public_id.map(|id| id.into()),
        );
        self.node.append_child(doctype.clone()).unwrap();
        self.node = doctype.into();
        self.handler.start_dtd(name, public_id, system_id);
    }
    fn end_dtd(&mut self) {
        if let Some(parent) = self.node.parent_node() {
            self.node = parent;
        }
        self.handler.end_dtd();
    }

    fn start_element(
        &mut self,
        uri: Option<&str>,
        local_name: Option<&str>,
        qname: &str,
        atts: &Attributes,
    ) {
        if let Ok(mut elem) = self
            .document
            .create_element(qname, uri.map(|uri| uri.into()))
        {
            for att in atts {
                if att.is_nsdecl() {
                    if att.qname.as_ref() == "xmlns" {
                        elem.declare_namespace(None, &att.value).ok();
                    } else {
                        elem.declare_namespace(att.local_name.as_deref(), &att.value)
                            .ok();
                    }
                } else {
                    // TODO:
                    // In the current implementation, only attribute values after entity expansion can
                    // be retrieved, so entity references disappear when the document is written back.

                    elem.set_attribute(&att.qname, att.uri.as_deref(), Some(&att.value))
                        .ok();

                    let local_name = att.local_name.as_deref().unwrap_or(&att.qname);
                    if let Some(mut attribute) =
                        elem.get_attribute_node(local_name, att.uri.as_deref())
                    {
                        if att.is_specified() {
                            attribute.set_specified();
                        } else {
                            attribute.unset_specified();
                        }
                    }
                }
            }
            self.node.append_child(elem.clone()).unwrap();
            self.node = elem.into();
        }
        self.handler.start_element(uri, local_name, qname, atts);
    }
    fn end_element(&mut self, uri: Option<&str>, local_name: Option<&str>, qname: &str) {
        if let Some(parent) = self.node.parent_node() {
            self.node = parent;
        }
        self.handler.end_element(uri, local_name, qname);
    }

    fn start_entity(&mut self, name: &str) {
        if name != "[dtd]" {
            // we should create an empty entity reference,
            // so we should use `EntityReference::new`, not but `self.document.create_entity_reference`.
            let ent = EntityReference::new(name.into(), self.document.clone());
            self.node.append_child(ent.clone()).unwrap();
            self.node = ent.into();
        }
        self.handler.start_entity(name);
    }
    fn end_entity(&mut self) {
        if matches!(self.node.node_type(), NodeType::EntityReference)
            && let Some(parent) = self.node.parent_node()
        {
            self.node = parent;
        }
        self.handler.end_entity();
    }

    fn start_prefix_mapping(&mut self, prefix: Option<&str>, uri: &str) {
        self.handler.start_prefix_mapping(prefix, uri);
    }
    fn end_prefix_mapping(&mut self, prefix: Option<&str>) {
        self.handler.end_prefix_mapping(prefix);
    }

    fn unparsed_entity_decl(
        &mut self,
        name: &str,
        public_id: Option<&str>,
        system_id: &URIStr,
        notation_name: &str,
    ) {
        self.node
            .append_child(self.document.create_unparsed_entity_decl(
                name,
                system_id,
                public_id.map(|id| id.into()),
                notation_name,
            ))
            .unwrap();
        self.handler
            .unparsed_entity_decl(name, public_id, system_id, notation_name);
    }
}

fn compare_document_order(
    left: Node<dyn NodeSpec>,
    right: Node<dyn NodeSpec>,
) -> Option<std::cmp::Ordering> {
    use std::cmp::Ordering::*;

    if left.is_same_node(right.clone()) {
        return Some(Equal);
    }
    if !left.is_same_owner_document(&right) {
        return None;
    }

    let mut ldepth = 0;
    let mut lp = left.clone();
    while let Some(par) = lp.parent_node() {
        ldepth += 1;
        lp = par.into();
    }
    let mut rdepth = 0;
    let mut rp = right.clone();
    while let Some(par) = rp.parent_node() {
        rdepth += 1;
        rp = par.into();
    }

    // they are not on the same document tree
    if !lp.is_same_node(rp) {
        return None;
    }

    let mut lp = left.clone();
    let mut rp = right.clone();
    while ldepth > rdepth {
        ldepth -= 1;
        lp = lp.parent_node().unwrap().into();
    }
    if lp.is_same_node(rp.clone()) {
        return Some(Greater);
    }
    while ldepth < rdepth {
        rdepth -= 1;
        rp = rp.parent_node().unwrap().into();
    }
    if lp.is_same_node(rp.clone()) {
        return Some(Less);
    }

    while let Some(lpar) = lp.parent_node()
        && let Some(rpar) = rp.parent_node()
        && !lpar.is_same_node(rpar.clone())
    {
        lp = lpar.into();
        rp = rpar.into();
    }

    match (lp.node_type(), rp.node_type()) {
        // Attribute and Namespace precede descendant nodes of the parent element,
        // so if the other is neither an Attribute nor a Namespace, it must precede it.
        // Additionally, the Namespace precedes the Attribute.
        //
        // Reference:
        // https://www.w3.org/TR/1999/REC-xpath-19991116/#dt-document-order
        (NodeType::Namespace, NodeType::Namespace) => {
            let lp = lp.as_namespace().unwrap();
            let rp = rp.as_namespace().unwrap();
            // Since prefixes are unique within elements,
            // they can be safely used for comparison.
            return lp
                .prefix()
                .unwrap_or_default()
                .partial_cmp(&rp.prefix().unwrap_or_default());
        }
        (NodeType::Namespace, _) => return Some(Less),
        (_, NodeType::Namespace) => return Some(Greater),
        (NodeType::Attribute, NodeType::Attribute) => {
            let elem = lp.parent_node().unwrap().as_element().unwrap();
            for att in elem.attributes() {
                if lp.is_same_node(att.clone()) {
                    return Some(Less);
                } else if rp.is_same_node(att.clone()) {
                    return Some(Greater);
                }
            }
            // is it possible to reach here?
            return None;
        }
        (NodeType::Attribute, _) => return Some(Less),
        (_, NodeType::Attribute) => return Some(Greater),
        // DocumentType always precedes Element.
        (NodeType::DocumentType, NodeType::Element) => return Some(Less),
        (NodeType::Element, NodeType::DocumentType) => return Some(Greater),
        _ => {}
    }

    while let Some(rprev) = rp.previous_sibling() {
        if lp.is_same_node(rprev.clone()) {
            return Some(Less);
        }
        rp = rprev;
    }
    Some(Greater)
}
