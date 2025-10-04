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

use anyxml_uri::uri::URIStr;
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
    tree::{convert::NodeKind, node::InternalNodeSpec},
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
    DuplicateAttribute,
    BaseURINotAbsolute,
}

pub struct TreeBuildHandler<H: SAXHandler = DefaultSAXHandler> {
    node: Node<dyn InternalNodeSpec>,
    document: Document,
    handler: H,
    in_cdata: bool,
}

impl<H: SAXHandler> TreeBuildHandler<H> {
    pub fn with_handler(handler: H) -> Self {
        let document = Document::new();
        Self {
            node: document.clone().into(),
            document,
            handler,
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
        self.node.append_child(
            self.document
                .create_attlist_decl(
                    element_name.into(),
                    attribute_name.into(),
                    attribute_type.clone(),
                    default_decl.clone(),
                )
                .into(),
        );
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
                        .append_child(self.document.create_text(data).into());
                }
            }
            Some(NodeKind::Text(mut text)) => text.push_str(data),
            _ => self
                .node
                .append_child(self.document.create_text(data).into()),
        }
        self.handler.characters(data);
    }

    fn comment(&mut self, data: &str) {
        self.node
            .append_child(self.document.create_comment(data).into());
        self.handler.comment(data);
    }

    fn declaration(&mut self, version: &str, encoding: Option<&str>, standalone: Option<bool>) {
        self.document.set_version(Some(version));
        self.document.set_encoding(encoding);
        self.document.set_standalone(standalone);
        self.handler.declaration(version, encoding, standalone);
    }

    fn element_decl(&mut self, name: &str, contentspec: &ContentSpec) {
        self.node.append_child(
            self.document
                .create_element_decl(name.into(), contentspec.clone())
                .into(),
        );
        self.handler.element_decl(name, contentspec);
    }

    fn external_entity_decl(&mut self, name: &str, public_id: Option<&str>, system_id: &URIStr) {
        self.node.append_child(
            self.document
                .create_external_entity_decl(
                    name.into(),
                    system_id.into(),
                    public_id.map(|id| id.into()),
                )
                .into(),
        );
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
                        .append_child(self.document.create_text(data).into());
                }
            }
            Some(NodeKind::Text(mut text)) => text.push_str(data),
            _ => self
                .node
                .append_child(self.document.create_text(data).into()),
        }
        self.handler.ignorable_whitespace(data);
    }

    fn internal_entity_decl(&mut self, name: &str, value: &str) {
        self.node.append_child(
            self.document
                .create_internal_entity_decl(name.into(), value.into())
                .into(),
        );
        self.handler.internal_entity_decl(name, value);
    }

    fn notation_decl(&mut self, name: &str, public_id: Option<&str>, system_id: Option<&URIStr>) {
        self.node.append_child(
            self.document
                .create_notation_decl(
                    name.into(),
                    system_id.map(|id| id.into()),
                    public_id.map(|id| id.into()),
                )
                .into(),
        );
        self.handler.notation_decl(name, public_id, system_id);
    }

    fn processing_instruction(&mut self, target: &str, data: Option<&str>) {
        self.node.append_child(
            self.document
                .create_processing_instruction(target.into(), data.map(|data| data.into()))
                .into(),
        );
        self.handler.processing_instruction(target, data);
    }

    fn set_document_locator(&mut self, locator: Arc<Locator>) {
        self.document
            .set_base_uri(locator.system_id().as_ref().into())
            .ok();
        self.handler.set_document_locator(locator);
    }

    fn skipped_entity(&mut self, name: &str) {
        if name != "[dtd]" {
            // we should create an empty entity reference,
            // so we should use `EntityReference::new`, not but `self.document.create_entity_reference`.
            self.node
                .append_child(EntityReference::new(name.into(), self.document.clone()).into());
        }
        self.handler.skipped_entity(name);
    }

    fn start_cdata(&mut self) {
        self.in_cdata = true;
        self.node
            .append_child(self.document.create_cdata_section("").into());
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
            name.into(),
            system_id.map(|id| id.into()),
            public_id.map(|id| id.into()),
        );
        self.node.append_child(doctype.clone().into());
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
            .create_element(qname.into(), uri.map(|uri| uri.into()))
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
                    elem.set_attribute(&att.qname, att.uri.as_deref(), Some(&att.value))
                        .ok();
                }
            }
            self.node.append_child(elem.clone().into());
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
            self.node.append_child(ent.clone().into());
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
        self.node.append_child(
            self.document
                .create_unparsed_entity_decl(
                    name.into(),
                    system_id.into(),
                    public_id.map(|id| id.into()),
                    notation_name.into(),
                )
                .into(),
        );
        self.handler
            .unparsed_entity_decl(name, public_id, system_id, notation_name);
    }
}
