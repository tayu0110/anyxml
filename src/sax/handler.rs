use std::{fmt::Write as _, fs::File, sync::Arc};

use anyxml_uri::uri::URIStr;

use crate::{
    error::XMLError,
    sax::{
        AttributeType, ContentSpec, DefaultDecl, Locator, attributes::Attributes,
        error::SAXParseError, source::InputSource,
    },
};

pub trait SAXHandler: EntityResolver {
    /// # Reference
    /// [`ContentHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ContentHandler.html)
    fn characters(&mut self, data: &str) {
        let _ = data;
    }

    /// # Reference
    /// [`ContentHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ContentHandler.html)
    fn declaration(&mut self, version: &str, encoding: Option<&str>, standalone: Option<bool>) {
        let _ = (version, encoding, standalone);
    }

    /// # Reference
    /// [`ContentHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ContentHandler.html)
    fn ignorable_whitespace(&mut self, data: &str) {
        let _ = data;
    }

    /// # Reference
    /// [`ContentHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ContentHandler.html)
    fn processing_instruction(&mut self, target: &str, data: Option<&str>) {
        let _ = (target, data);
    }

    /// # Reference
    /// [`ContentHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ContentHandler.html)
    fn set_document_locator(&mut self, locator: Arc<Locator>) {
        let _ = locator;
    }

    /// # Reference
    /// [`ContentHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ContentHandler.html)
    fn skipped_entity(&mut self, name: &str) {
        let _ = name;
    }

    /// # Reference
    /// [`ContentHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ContentHandler.html)
    fn start_document(&mut self) {}
    /// # Reference
    /// [`ContentHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ContentHandler.html)
    fn end_document(&mut self) {}

    /// # Reference
    /// [`ContentHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ContentHandler.html)
    fn start_element(
        &mut self,
        uri: Option<&str>,
        local_name: Option<&str>,
        qname: &str,
        atts: &Attributes,
    ) {
        let _ = (uri, local_name, qname, atts);
    }
    /// # Reference
    /// [`ContentHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ContentHandler.html)
    fn end_element(&mut self, uri: Option<&str>, local_name: Option<&str>, qname: &str) {
        let _ = (uri, local_name, qname);
    }

    /// # Reference
    /// [`ContentHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ContentHandler.html)
    fn start_prefix_mapping(&mut self, prefix: Option<&str>, uri: &str) {
        let _ = (prefix, uri);
    }
    /// # Reference
    /// [`ContentHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ContentHandler.html)
    fn end_prefix_mapping(&mut self, prefix: Option<&str>) {
        let _ = prefix;
    }

    /// # Reference
    /// [`DeclHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ext/DeclHandler.html)
    fn attribute_decl(
        &mut self,
        element_name: &str,
        attribute_name: &str,
        attribute_type: &AttributeType,
        default_decl: &DefaultDecl,
    ) {
        let _ = (element_name, attribute_name, attribute_type, default_decl);
    }

    /// # Reference
    /// [`DeclHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ext/DeclHandler.html)
    fn element_decl(&mut self, name: &str, contentspec: &ContentSpec) {
        let _ = (name, contentspec);
    }

    /// # Reference
    /// [`DeclHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ext/DeclHandler.html)
    fn external_entity_decl(&mut self, name: &str, public_id: Option<&str>, system_id: &URIStr) {
        let _ = (name, public_id, system_id);
    }

    /// # Reference
    /// [`DeclHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ext/DeclHandler.html)
    fn internal_entity_decl(&mut self, name: &str, value: &str) {
        let _ = (name, value);
    }

    /// # Reference
    /// [`DTDHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/DTDHandler.html)
    fn notation_decl(&mut self, name: &str, public_id: Option<&str>, system_id: Option<&URIStr>) {
        let _ = (name, public_id, system_id);
    }

    /// # Reference
    /// [`DTDHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/DTDHandler.html)
    fn unparsed_entity_decl(
        &mut self,
        name: &str,
        public_id: Option<&str>,
        system_id: &URIStr,
        notation_name: &str,
    ) {
        let _ = (name, public_id, system_id, notation_name);
    }

    /// # Reference
    /// [`ErrorHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ErrorHandler.html)
    fn error(&mut self, error: SAXParseError) {
        let _ = error;
    }

    /// # Reference
    /// [`ErrorHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ErrorHandler.html)
    fn fatal_error(&mut self, error: SAXParseError) {
        let _ = error;
    }

    /// # Reference
    /// [`ErrorHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ErrorHandler.html)
    fn warning(&mut self, error: SAXParseError) {
        let _ = error;
    }

    /// # Reference
    /// [`LexicalHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ext/LexicalHandler.html)
    fn comment(&mut self, data: &str) {
        let _ = data;
    }

    /// # Reference
    /// [`LexicalHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ext/LexicalHandler.html)
    fn start_cdata(&mut self) {}
    /// # Reference
    /// [`LexicalHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ext/LexicalHandler.html)
    fn end_cdata(&mut self) {}

    /// # Reference
    /// [`LexicalHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ext/LexicalHandler.html)
    fn start_dtd(&mut self, name: &str, public_id: Option<&str>, system_id: Option<&URIStr>) {
        let _ = (name, public_id, system_id);
    }
    /// # Reference
    /// [`LexicalHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ext/LexicalHandler.html)
    fn end_dtd(&mut self) {}

    /// # Reference
    /// [`LexicalHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ext/LexicalHandler.html)
    fn start_entity(&mut self, name: &str) {
        let _ = name;
    }
    /// # Reference
    /// [`LexicalHandler` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ext/LexicalHandler.html)
    fn end_entity(&mut self) {}
}

pub trait EntityResolver {
    /// # Reference
    /// [`EntityResolver2` interface in Java SAX API](https://docs.oracle.com/javase/jp/21/docs/api/java.xml/org/xml/sax/ext/EntityResolver2.html)
    fn get_external_subset(
        &mut self,
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
        &mut self,
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
    fn error(&mut self, error: SAXParseError) {
        eprintln!("{error}")
    }

    fn fatal_error(&mut self, error: SAXParseError) {
        eprintln!("{error}")
    }

    fn warning(&mut self, error: SAXParseError) {
        eprintln!("{error}")
    }
}
impl EntityResolver for DefaultSAXHandler {}

pub struct DebugHandler<Child: SAXHandler = DefaultSAXHandler> {
    pub buffer: String,
    pub child: Child,
}

impl EntityResolver for DebugHandler {
    fn get_external_subset(
        &mut self,
        name: &str,
        base_uri: Option<&URIStr>,
    ) -> Result<InputSource<'static>, XMLError> {
        writeln!(
            self.buffer,
            "getExternalSubset({name}, {:?})",
            base_uri.map(|uri| uri.as_escaped_str())
        )
        .ok();
        self.child.get_external_subset(name, base_uri)
    }

    fn resolve_entity(
        &mut self,
        name: &str,
        public_id: Option<&str>,
        base_uri: &URIStr,
        system_id: &URIStr,
    ) -> Result<InputSource<'static>, XMLError> {
        writeln!(
            self.buffer,
            "resolveEntity({name}, {public_id:?}, {}, {})",
            base_uri.as_escaped_str(),
            system_id.as_escaped_str()
        )
        .ok();
        self.child
            .resolve_entity(name, public_id, base_uri, system_id)
    }
}

impl SAXHandler for DebugHandler {
    fn characters(&mut self, data: &str) {
        writeln!(self.buffer, "characters({data})").ok();
        self.child.characters(data);
    }

    fn declaration(&mut self, version: &str, encoding: Option<&str>, standalone: Option<bool>) {
        writeln!(
            self.buffer,
            "declaration({version}, {encoding:?}, {standalone:?})"
        )
        .ok();
        self.child.declaration(version, encoding, standalone);
    }

    fn ignorable_whitespace(&mut self, data: &str) {
        writeln!(self.buffer, "ignorableWhitespace({data})").ok();
        self.child.ignorable_whitespace(data);
    }

    fn processing_instruction(&mut self, target: &str, data: Option<&str>) {
        writeln!(self.buffer, "processingInstruction({target}, {data:?})").ok();
        self.child.processing_instruction(target, data);
    }

    fn set_document_locator(&mut self, locator: Arc<Locator>) {
        writeln!(self.buffer, "setDocumentLocator()").ok();
        self.child.set_document_locator(locator);
    }

    fn skipped_entity(&mut self, name: &str) {
        writeln!(self.buffer, "skippedEntity({name})").ok();
        self.child.skipped_entity(name);
    }

    fn start_document(&mut self) {
        writeln!(self.buffer, "startDocument()").ok();
        self.child.start_document();
    }
    fn end_document(&mut self) {
        writeln!(self.buffer, "endDocument()").ok();
        self.child.end_document();
    }

    fn start_element(
        &mut self,
        uri: Option<&str>,
        local_name: Option<&str>,
        qname: &str,
        atts: &Attributes,
    ) {
        write!(self.buffer, "startElement({uri:?}, {local_name:?}, {qname}").ok();
        for att in atts {
            write!(self.buffer, ", ").ok();
            if let Some(local_name) = att.local_name.as_deref() {
                write!(self.buffer, "{{{:?}}}{local_name}='{}'", att.uri, att.value).ok();
            } else {
                write!(self.buffer, "{}='{}'", att.qname, att.value).ok();
            }
        }
        writeln!(self.buffer, ")").ok();
        self.child.start_element(uri, local_name, qname, atts);
    }
    fn end_element(&mut self, uri: Option<&str>, local_name: Option<&str>, qname: &str) {
        writeln!(self.buffer, "endElement({uri:?}, {local_name:?}, {qname})").ok();
        self.child.end_element(uri, local_name, qname);
    }

    fn start_prefix_mapping(&mut self, prefix: Option<&str>, uri: &str) {
        writeln!(self.buffer, "startPrefixMapping({prefix:?}, {uri})").ok();
        self.child.start_prefix_mapping(prefix, uri);
    }
    fn end_prefix_mapping(&mut self, prefix: Option<&str>) {
        writeln!(self.buffer, "endPrefixMapping({prefix:?})").ok();
        self.child.end_prefix_mapping(prefix);
    }

    fn attribute_decl(
        &mut self,
        element_name: &str,
        attribute_name: &str,
        attribute_type: &AttributeType,
        default_decl: &DefaultDecl,
    ) {
        writeln!(
            self.buffer,
            "attributeDecl({element_name}, {attribute_name}, {attribute_type:?}, {default_decl:?})"
        )
        .ok();
        self.child
            .attribute_decl(element_name, attribute_name, attribute_type, default_decl);
    }

    fn element_decl(&mut self, name: &str, contentspec: &ContentSpec) {
        writeln!(self.buffer, "elementDecl({name}, {contentspec:?})").ok();
        self.child.element_decl(name, contentspec);
    }

    fn external_entity_decl(&mut self, name: &str, public_id: Option<&str>, system_id: &URIStr) {
        writeln!(
            self.buffer,
            "externalEntityDecl({name}, {public_id:?}, {system_id:?})"
        )
        .ok();
        self.child.external_entity_decl(name, public_id, system_id);
    }

    fn internal_entity_decl(&mut self, name: &str, value: &str) {
        writeln!(self.buffer, "internalEntityDecl({name}, {value})").ok();
        self.child.internal_entity_decl(name, value);
    }

    fn notation_decl(&mut self, name: &str, public_id: Option<&str>, system_id: Option<&URIStr>) {
        writeln!(
            self.buffer,
            "notationDecl({name}, {public_id:?}, {system_id:?})"
        )
        .ok();
        self.child.notation_decl(name, public_id, system_id);
    }

    fn unparsed_entity_decl(
        &mut self,
        name: &str,
        public_id: Option<&str>,
        system_id: &URIStr,
        notation_name: &str,
    ) {
        writeln!(
            self.buffer,
            "unparsedEntityDecl({name}, {public_id:?}, {}, {notation_name})",
            system_id.as_escaped_str()
        )
        .ok();
        self.child
            .unparsed_entity_decl(name, public_id, system_id, notation_name);
    }

    fn error(&mut self, error: SAXParseError) {
        self.child.error(error);
    }

    fn fatal_error(&mut self, error: SAXParseError) {
        self.child.fatal_error(error);
    }

    fn warning(&mut self, error: SAXParseError) {
        self.child.warning(error);
    }

    fn comment(&mut self, data: &str) {
        writeln!(self.buffer, "comment({data})").ok();
        self.child.comment(data);
    }

    fn start_cdata(&mut self) {
        writeln!(self.buffer, "startCDATA()").ok();
        self.child.start_cdata();
    }
    fn end_cdata(&mut self) {
        writeln!(self.buffer, "endCDATA()").ok();
        self.child.end_cdata();
    }

    fn start_dtd(&mut self, name: &str, public_id: Option<&str>, system_id: Option<&URIStr>) {
        writeln!(
            self.buffer,
            "startDTD({name}, {public_id:?}, {:?})",
            system_id.map(|id| id.as_escaped_str())
        )
        .ok();
        self.child.start_dtd(name, public_id, system_id);
    }
    fn end_dtd(&mut self) {
        writeln!(self.buffer, "endDTD()").ok();
        self.child.end_dtd();
    }

    fn start_entity(&mut self, name: &str) {
        writeln!(self.buffer, "startEntity({name})").ok();
        self.child.start_entity(name);
    }
    fn end_entity(&mut self) {
        writeln!(self.buffer, "endEntity()").ok();
        self.child.end_entity();
    }
}

impl Default for DebugHandler {
    fn default() -> Self {
        Self {
            buffer: String::new(),
            child: DefaultSAXHandler,
        }
    }
}
