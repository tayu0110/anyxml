use std::{fs::File, sync::Arc};

use anyxml_uri::uri::URIStr;

use crate::{
    error::XMLError,
    sax::{
        Attribute, AttributeType, ContentSpec, DefaultDecl, Locator, error::SAXParseError,
        source::InputSource,
    },
};

pub trait SAXHandler: EntityResolver {
    /// # Reference
    /// [`ContentHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ContentHandler.html)
    fn characters(&self, data: &str) {
        let _ = data;
    }

    /// # Reference
    /// [`ContentHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ContentHandler.html)
    fn declaration(&self, version: &str, encoding: Option<&str>, standalone: Option<bool>) {
        let _ = (version, encoding, standalone);
    }

    /// # Reference
    /// [`ContentHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ContentHandler.html)
    fn ignorable_whitespace(&self, data: &str) {
        let _ = data;
    }

    /// # Reference
    /// [`ContentHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ContentHandler.html)
    fn processing_instruction(&self, target: &str, data: Option<&str>) {
        let _ = (target, data);
    }

    /// # Reference
    /// [`ContentHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ContentHandler.html)
    fn set_document_locator(&self, locator: Arc<Locator>) {
        let _ = locator;
    }

    /// # Reference
    /// [`ContentHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ContentHandler.html)
    fn skipped_entity(&self, name: &str) {
        let _ = name;
    }

    /// # Reference
    /// [`ContentHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ContentHandler.html)
    fn start_document(&self) {}
    /// # Reference
    /// [`ContentHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ContentHandler.html)
    fn end_document(&self) {}

    /// # Reference
    /// [`ContentHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ContentHandler.html)
    fn start_element(
        &self,
        uri: Option<&str>,
        local_name: Option<&str>,
        qname: &str,
        atts: &[Attribute],
    ) {
        let _ = (uri, local_name, qname, atts);
    }
    /// # Reference
    /// [`ContentHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ContentHandler.html)
    fn end_element(&self, uri: Option<&str>, local_name: Option<&str>, qname: &str) {
        let _ = (uri, local_name, qname);
    }

    /// # Reference
    /// [`ContentHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ContentHandler.html)
    fn start_prefix_mapping(&self, prefix: Option<&str>, uri: &str) {
        let _ = (prefix, uri);
    }
    /// # Reference
    /// [`ContentHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ContentHandler.html)
    fn end_prefix_mapping(&self, prefix: Option<&str>) {
        let _ = prefix;
    }

    /// # Reference
    /// [`DeclHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ext/DeclHandler.html)
    fn attribute_decl(
        &self,
        element_name: &str,
        attribute_name: &str,
        attribute_type: &AttributeType,
        default_decl: &DefaultDecl,
    ) {
        let _ = (element_name, attribute_name, attribute_type, default_decl);
    }

    /// # Reference
    /// [`DeclHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ext/DeclHandler.html)
    fn element_decl(&self, name: &str, contentspec: &ContentSpec) {
        let _ = (name, contentspec);
    }

    /// # Reference
    /// [`DeclHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ext/DeclHandler.html)
    fn external_entity_decl(&self, name: &str, public_id: Option<&str>, system_id: &URIStr) {
        let _ = (name, public_id, system_id);
    }

    /// # Reference
    /// [`DeclHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ext/DeclHandler.html)
    fn internal_entity_decl(&self, name: &str, value: &str) {
        let _ = (name, value);
    }

    /// # Reference
    /// [`DTDHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/DTDHandler.html)
    fn notation_decl(&self, name: &str, public_id: Option<&str>, system_id: Option<&URIStr>) {
        let _ = (name, public_id, system_id);
    }

    /// # Reference
    /// [`DTDHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/DTDHandler.html)
    fn unparsed_entity_decl(
        &self,
        name: &str,
        public_id: Option<&str>,
        system_id: &URIStr,
        notation_name: &str,
    ) {
        let _ = (name, public_id, system_id, notation_name);
    }

    /// # Reference
    /// [`ErrorHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ErrorHandler.html)
    fn error(&self, error: SAXParseError) {
        let _ = error;
    }

    /// # Reference
    /// [`ErrorHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ErrorHandler.html)
    fn fatal_error(&self, error: SAXParseError) {
        let _ = error;
    }

    /// # Reference
    /// [`ErrorHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ErrorHandler.html)
    fn warning(&self, error: SAXParseError) {
        let _ = error;
    }

    /// # Reference
    /// [`LexicalHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ext/LexicalHandler.html)
    fn comment(&self, data: &str) {
        let _ = data;
    }

    /// # Reference
    /// [`LexicalHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ext/LexicalHandler.html)
    fn start_cdata(&self) {}
    /// # Reference
    /// [`LexicalHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ext/LexicalHandler.html)
    fn end_cdata(&self) {}

    /// # Reference
    /// [`LexicalHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ext/LexicalHandler.html)
    fn start_dtd(&self, name: &str, public_id: Option<&str>, system_id: Option<&URIStr>) {
        let _ = (name, public_id, system_id);
    }
    /// # Reference
    /// [`LexicalHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ext/LexicalHandler.html)
    fn end_dtd(&self) {}

    /// # Reference
    /// [`LexicalHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ext/LexicalHandler.html)
    fn start_entity(&self, name: &str) {
        let _ = name;
    }
    /// # Reference
    /// [`LexicalHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ext/LexicalHandler.html)
    fn end_entity(&self) {}
}

pub trait EntityResolver {
    /// # Reference
    /// [`EntityResolver2` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ext/EntityResolver2.html)
    fn get_external_subset(
        &self,
        name: &str,
        base_uri: Option<&URIStr>,
    ) -> Result<InputSource<'static>, XMLError> {
        let _ = (name, base_uri);
        Err(XMLError::IONotFound)
    }

    /// By default, it attempts to retrieve local files using `base_uri` and `system_id`.
    ///
    /// When handling untrusted XML documents, it is recommended to implement custom logic
    /// to prevent unexpected access to local resources.
    ///
    /// # Reference
    /// [`EntityResolver2` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ext/EntityResolver2.html)
    fn resolve_entity(
        &self,
        name: &str,
        public_id: Option<&str>,
        base_uri: &URIStr,
        system_id: &URIStr,
    ) -> Result<InputSource<'static>, XMLError> {
        let (_, _) = (name, public_id);
        let path = base_uri.resolve(system_id);
        let file = File::open(path.path())?;
        let mut reader = InputSource::from_reader(file, None)?;
        reader.set_system_id(path);
        Ok(reader)
    }
}

pub struct DefaultSAXHandler;

impl SAXHandler for DefaultSAXHandler {
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
impl EntityResolver for DefaultSAXHandler {}
