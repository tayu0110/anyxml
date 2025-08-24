use std::{fs::File, sync::Arc};

use anyxml_uri::uri::URIStr;

use crate::{
    error::XMLError,
    sax::{
        Attribute, AttributeType, ContentSpec, DefaultDecl, Locator, error::SAXParseError,
        source::InputSource,
    },
};

pub trait ContentHandler {
    fn characters(&self, data: &str) {
        let _ = data;
    }

    fn declaration(&self, version: &str, encoding: Option<&str>, standalone: Option<bool>) {
        let _ = (version, encoding, standalone);
    }

    fn ignorable_whitespace(&self, data: &str) {
        let _ = data;
    }

    fn processing_instruction(&self, target: &str, data: Option<&str>) {
        let _ = (target, data);
    }

    fn set_document_locator(&self, locator: Arc<Locator>) {
        let _ = locator;
    }

    fn skipped_entity(&self, name: &str) {
        let _ = name;
    }

    fn start_document(&self) {}
    fn end_document(&self) {}

    fn start_element(
        &self,
        uri: Option<&str>,
        local_name: Option<&str>,
        qname: &str,
        atts: &[Attribute],
    ) {
        let _ = (uri, local_name, qname, atts);
    }
    fn end_element(&self, uri: Option<&str>, local_name: Option<&str>, qname: &str) {
        let _ = (uri, local_name, qname);
    }

    fn start_prefix_mapping(&self, prefix: Option<&str>, uri: &str) {
        let _ = (prefix, uri);
    }
    fn end_prefix_mapping(&self, prefix: Option<&str>) {
        let _ = prefix;
    }
}

pub trait DeclHandler {
    fn attribute_decl(
        &self,
        element_name: &str,
        attribute_name: &str,
        attribute_type: &AttributeType,
        default_decl: &DefaultDecl,
    ) {
        let _ = (element_name, attribute_name, attribute_type, default_decl);
    }

    fn element_decl(&self, name: &str, contentspec: &ContentSpec) {
        let _ = (name, contentspec);
    }

    fn external_entity_decl(&self, name: &str, public_id: Option<&str>, system_id: &URIStr) {
        let _ = (name, public_id, system_id);
    }

    fn internal_entity_decl(&self, name: &str, value: &str) {
        let _ = (name, value);
    }
}

pub trait DTDHandler {
    fn notation_decl(&self, name: &str, public_id: Option<&str>, system_id: Option<&URIStr>) {
        let _ = (name, public_id, system_id);
    }

    fn unparsed_entity_decl(
        &self,
        name: &str,
        public_id: Option<&str>,
        system_id: &URIStr,
        notation_name: &str,
    ) {
        let _ = (name, public_id, system_id, notation_name);
    }
}

pub trait EntityResolver {
    fn get_external_subset(
        &self,
        name: &str,
        base_uri: Option<&URIStr>,
    ) -> Result<InputSource<'static>, XMLError> {
        let _ = (name, base_uri);
        Err(XMLError::IONotFound)
    }

    fn resolve_entity(
        &self,
        name: &str,
        public_id: Option<&str>,
        base_uri: &URIStr,
        system_id: &URIStr,
    ) -> Result<InputSource<'static>, XMLError> {
        let _ = (name, public_id, base_uri, system_id);
        Err(XMLError::IONotFound)
    }
}

pub trait ErrorHandler {
    fn error(&self, error: SAXParseError) {
        let _ = error;
    }

    fn fatal_error(&self, error: SAXParseError) {
        let _ = error;
    }

    fn warning(&self, error: SAXParseError) {
        let _ = error;
    }
}

pub trait LexicalHandler {
    fn comment(&self, data: &str) {
        let _ = data;
    }

    fn start_cdata(&self) {}
    fn end_cdata(&self) {}

    fn start_dtd(&self, name: &str, public_id: Option<&str>, system_id: Option<&URIStr>) {
        let _ = (name, public_id, system_id);
    }
    fn end_dtd(&self) {}

    fn start_entity(&self, name: &str) {
        let _ = name;
    }
    fn end_entity(&self) {}
}

pub struct DefaultSAXHandler;

impl ContentHandler for DefaultSAXHandler {}
impl DeclHandler for DefaultSAXHandler {}
impl DTDHandler for DefaultSAXHandler {}
impl EntityResolver for DefaultSAXHandler {
    fn resolve_entity(
        &self,
        _name: &str,
        _public_id: Option<&str>,
        base_uri: &URIStr,
        system_id: &URIStr,
    ) -> Result<InputSource<'static>, XMLError> {
        let path = base_uri.resolve(system_id);
        let file = File::open(path.path())?;
        let mut reader = InputSource::from_reader(file, None)?;
        reader.set_system_id(path);
        Ok(reader)
    }
}
impl ErrorHandler for DefaultSAXHandler {
    fn error(&self, error: SAXParseError) {
        eprintln!("{error}")
    }

    fn fatal_error(&self, error: SAXParseError) {
        eprintln!("{error}")
    }

    fn warning(&self, error: SAXParseError) {
        eprintln!("{error}")
    }
}
impl LexicalHandler for DefaultSAXHandler {}
