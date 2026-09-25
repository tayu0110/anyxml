use std::sync::Arc;

use crate::{
    error::XMLError,
    sax::{
        AttributeType, Attributes, ContentSpec, DefaultDecl, EntityResolver, ErrorHandler,
        InputSource, Locator, SAXHandler, error::SAXParseError,
    },
    uri::URIStr,
    xsd::Schema,
};

pub struct ValidateHandler<'a, H: SAXHandler + ?Sized> {
    schema: &'a mut Schema,
    pub use_schema_location_hint: bool,
    locator: Arc<Locator>,
    pub child: H,
}

impl<'a, H: SAXHandler> ValidateHandler<'a, H> {
    pub(super) fn new(schema: &'a mut Schema, child: H) -> Self {
        ValidateHandler {
            schema,
            use_schema_location_hint: false,
            locator: Arc::new(Locator::default()),
            child,
        }
    }
}

impl<H: SAXHandler + ?Sized> EntityResolver for ValidateHandler<'_, H> {
    fn resolve_entity(
        &mut self,
        name: &str,
        public_id: Option<&str>,
        base_uri: &URIStr,
        system_id: &URIStr,
    ) -> Result<InputSource<'static>, XMLError> {
        self.child
            .resolve_entity(name, public_id, base_uri, system_id)
    }
    fn get_external_subset(
        &mut self,
        name: &str,
        base_uri: Option<&URIStr>,
    ) -> Result<InputSource<'static>, XMLError> {
        self.child.get_external_subset(name, base_uri)
    }
}

impl<H: SAXHandler + ?Sized> ErrorHandler for ValidateHandler<'_, H> {
    fn fatal_error(&mut self, error: SAXParseError) {
        self.child.fatal_error(error);
    }
    fn error(&mut self, error: SAXParseError) {
        self.child.error(error);
    }
    fn warning(&mut self, error: SAXParseError) {
        self.child.warning(error);
    }
}

impl<H: SAXHandler + ?Sized> SAXHandler for ValidateHandler<'_, H> {
    fn set_document_locator(&mut self, locator: Arc<Locator>) {
        self.locator = locator.clone();
        self.child.set_document_locator(locator);
    }

    fn start_element(
        &mut self,
        namespace_name: Option<&str>,
        local_name: Option<&str>,
        qname: &str,
        atts: &Attributes,
    ) {
        self.child
            .start_element(namespace_name, local_name, qname, atts);
    }

    fn end_element(&mut self, namespace_name: Option<&str>, local_name: Option<&str>, qname: &str) {
        self.child.end_element(namespace_name, local_name, qname);
    }

    fn characters(&mut self, data: &str) {
        self.child.characters(data);
    }

    // unused callbacks

    fn attribute_decl(
        &mut self,
        element_name: &str,
        attribute_name: &str,
        attribute_type: &AttributeType,
        default_decl: &DefaultDecl,
    ) {
        self.child
            .attribute_decl(element_name, attribute_name, attribute_type, default_decl);
    }
    fn comment(&mut self, data: &str) {
        self.child.comment(data);
    }
    fn declaration(&mut self, version: &str, encoding: Option<&str>, standalone: Option<bool>) {
        self.child.declaration(version, encoding, standalone);
    }
    fn element_decl(&mut self, name: &str, contentspec: &ContentSpec) {
        self.child.element_decl(name, contentspec);
    }
    fn start_cdata(&mut self) {
        self.child.start_cdata();
    }
    fn end_cdata(&mut self) {
        self.child.end_cdata();
    }
    fn start_document(&mut self) {
        self.child.start_document();
    }
    fn end_document(&mut self) {
        self.child.end_document();
    }
    fn start_dtd(&mut self, name: &str, public_id: Option<&str>, system_id: Option<&URIStr>) {
        self.child.start_dtd(name, public_id, system_id);
    }
    fn end_dtd(&mut self) {
        self.child.end_dtd();
    }
    fn start_entity(&mut self, name: &str) {
        self.child.start_entity(name);
    }
    fn end_entity(&mut self) {
        self.child.end_entity();
    }
    fn start_prefix_mapping(&mut self, prefix: Option<&str>, namespace_name: &str) {
        self.child.start_prefix_mapping(prefix, namespace_name);
    }
    fn end_prefix_mapping(&mut self, prefix: Option<&str>) {
        self.child.end_prefix_mapping(prefix);
    }
    fn external_entity_decl(&mut self, name: &str, public_id: Option<&str>, system_id: &URIStr) {
        self.child.external_entity_decl(name, public_id, system_id);
    }
    fn ignorable_whitespace(&mut self, data: &str) {
        self.child.ignorable_whitespace(data);
    }
    fn internal_entity_decl(&mut self, name: &str, value: &str) {
        self.child.internal_entity_decl(name, value);
    }
    fn notation_decl(&mut self, name: &str, public_id: Option<&str>, system_id: Option<&URIStr>) {
        self.child.notation_decl(name, public_id, system_id);
    }
    fn processing_instruction(&mut self, target: &str, data: Option<&str>) {
        self.child.processing_instruction(target, data);
    }
    fn skipped_entity(&mut self, name: &str) {
        self.child.skipped_entity(name);
    }
    fn unparsed_entity_decl(
        &mut self,
        name: &str,
        public_id: Option<&str>,
        system_id: &URIStr,
        notation_name: &str,
    ) {
        self.child
            .unparsed_entity_decl(name, public_id, system_id, notation_name);
    }
}
