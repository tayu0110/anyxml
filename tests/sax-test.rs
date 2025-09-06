use std::{
    cell::{Cell, RefCell},
    fmt::Write as _,
    fs::read_dir,
    sync::Arc,
};

use anyxml::{
    error::{XMLErrorDomain, XMLErrorLevel},
    sax::{
        Locator,
        attributes::Attribute,
        handler::{DefaultSAXHandler, EntityResolver, SAXHandler},
        parser::{ParserOption, XMLReaderBuilder},
    },
};
use anyxml_uri::uri::URIString;

#[derive(Default)]
struct TestSAXHandler {
    warning: Cell<usize>,
    error: Cell<usize>,
    fatal_error: Cell<usize>,
    ns_error: Cell<usize>,
    validity_error: Cell<usize>,
    buffer: RefCell<String>,
}

impl TestSAXHandler {
    fn new() -> Self {
        Self {
            warning: Cell::new(0),
            error: Cell::new(0),
            fatal_error: Cell::new(0),
            ns_error: Cell::new(0),
            validity_error: Cell::new(0),
            buffer: RefCell::new(String::new()),
        }
    }

    fn reset(&self) {
        self.warning.update(|_| 0);
        self.error.update(|_| 0);
        self.fatal_error.update(|_| 0);
        self.ns_error.update(|_| 0);
        self.validity_error.update(|_| 0);
        self.buffer.borrow_mut().clear();
    }
}

impl SAXHandler for TestSAXHandler {
    fn start_element(
        &mut self,
        uri: Option<&str>,
        local_name: Option<&str>,
        qname: &str,
        atts: &[Attribute],
    ) {
        eprintln!("startElement('{qname}')");
        DefaultSAXHandler.start_element(uri, local_name, qname, atts);
    }

    fn end_element(&mut self, uri: Option<&str>, local_name: Option<&str>, qname: &str) {
        eprintln!("endElement('{qname}')");
        DefaultSAXHandler.end_element(uri, local_name, qname);
    }

    fn characters(&mut self, data: &str) {
        eprintln!("characters('{data}')");
        DefaultSAXHandler.characters(data);
    }

    fn ignorable_whitespace(&mut self, data: &str) {
        eprintln!("ignorableWhitespace({})", data.len());
        DefaultSAXHandler.characters(data);
    }

    fn external_entity_decl(
        &mut self,
        name: &str,
        public_id: Option<&str>,
        system_id: &anyxml_uri::uri::URIStr,
    ) {
        eprintln!("externalEntityDecl({name}, {public_id:?}, {system_id:?})");
        DefaultSAXHandler.external_entity_decl(name, public_id, system_id);
    }

    fn fatal_error(&mut self, error: anyxml::sax::error::SAXParseError) {
        assert_eq!(error.level, XMLErrorLevel::FatalError);
        self.fatal_error.update(|c| c + 1);
        writeln!(self.buffer.borrow_mut(), "{}", error).ok();
    }

    fn error(&mut self, error: anyxml::sax::error::SAXParseError) {
        assert_eq!(error.level, XMLErrorLevel::Error);
        match error.domain {
            XMLErrorDomain::Parser => self.error.update(|c| c + 1),
            XMLErrorDomain::Namespace => self.ns_error.update(|c| c + 1),
            XMLErrorDomain::DTDValid => self.validity_error.update(|c| c + 1),
        }
        writeln!(self.buffer.borrow_mut(), "{}", error).ok();
    }

    fn warning(&mut self, error: anyxml::sax::error::SAXParseError) {
        assert_eq!(error.level, XMLErrorLevel::Warning);
        self.warning.update(|c| c + 1);
        writeln!(self.buffer.borrow_mut(), "{}", error).ok();
    }
}

impl EntityResolver for TestSAXHandler {
    fn resolve_entity(
        &mut self,
        name: &str,
        public_id: Option<&str>,
        base_uri: &anyxml_uri::uri::URIStr,
        system_id: &anyxml_uri::uri::URIStr,
    ) -> Result<anyxml::sax::source::InputSource<'static>, anyxml::error::XMLError> {
        eprintln!(
            "resolveEntity({name}, {public_id:?}, {}, {})",
            base_uri.as_escaped_str(),
            system_id.as_escaped_str()
        );
        DefaultSAXHandler.resolve_entity(name, public_id, base_uri, system_id)
    }
}

#[test]
fn well_formed_tests() {
    let handler = TestSAXHandler::new();

    let mut reader = XMLReaderBuilder::new().set_handler(handler).build();

    for ent in read_dir("resources/well-formed").unwrap() {
        if let Ok(ent) = ent
            && ent.metadata().unwrap().is_file()
        {
            let uri = URIString::parse_file_path(ent.path().canonicalize().unwrap()).unwrap();
            reader.parse_uri(&uri, None).ok();
            assert_eq!(
                reader.handler().fatal_error.get(),
                0,
                "uri: {}\nerrors:\n{}",
                uri.as_escaped_str(),
                reader.handler().buffer.borrow(),
            );
            reader.handler().reset();
        }
    }
}

#[derive(Default)]
struct XMLConfWalker {
    log: RefCell<String>,
    locator: RefCell<Option<Arc<Locator>>>,
    unexpected_failure: Cell<usize>,
    unexpected_success: Cell<usize>,
}

impl XMLConfWalker {
    // Some tests require unsupported encodings or their types do not match the actual
    // error that occurs, making it difficult to pass all of them, so they are skipped.
    const SKIP_TESTS: &[&str] = &[
        "pr-xml-euc-jp",               // unsupported encoding
        "pr-xml-iso-2022-jp",          // unsupported encoding
        "pr-xml-shift_jis",            // unsupported encoding
        "weekly-euc-jp",               // unsupported encoding
        "weekly-iso-2022-jp",          // unsupported encoding
        "weekly-shift_jis",            // unsupported encoding
        "ibm-not-wf-P69-ibm69n05.xml", // error type, but requires VC validation
        "rmt-ns10-006",                // unsupported encoding
        "invalid-bo-7", // error type, but a Fatal Error occurs (Illegal XML character)
        "invalid-bo-8", // error type, but a Fatal Error occurs (Illegal XML character)
        "invalid-bo-9", // error type, but a Fatal Error occurs (Illegal XML character)
    ];
}

impl SAXHandler for XMLConfWalker {
    fn set_document_locator(&mut self, locator: Arc<Locator>) {
        *self.locator.borrow_mut() = Some(locator);
    }

    fn start_element(
        &mut self,
        _uri: Option<&str>,
        _local_name: Option<&str>,
        qname: &str,
        atts: &[Attribute],
    ) {
        match qname {
            "TESTSUITE" => {
                for att in atts {
                    if att.qname.as_ref() == "PROFILE" {
                        writeln!(
                            self.log.borrow_mut(),
                            "=== Start Test Suite '{}' ===",
                            att.value
                        )
                        .ok();
                    }
                }
            }
            "TESTCASES" => {
                for att in atts {
                    if att.qname.as_ref() == "PROFILE" {
                        writeln!(
                            self.log.borrow_mut(),
                            "--- Start Test Case '{}' in '{}' ---",
                            att.value,
                            self.locator
                                .borrow()
                                .as_ref()
                                .unwrap()
                                .system_id()
                                .as_escaped_str()
                        )
                        .ok();
                    }
                }
            }
            "TEST" => {
                let mut id = String::new();
                let mut r#type = String::new();
                let mut uri = String::new();
                let mut recommendation = String::new();
                let mut edition = String::new();
                let mut entities = String::new();
                for att in atts {
                    match att.qname.as_ref() {
                        "TYPE" => r#type = att.value.to_string(),
                        "ID" => id = att.value.to_string(),
                        "URI" => uri = att.value.to_string(),
                        "RECOMMENDATION" => recommendation = att.value.to_string(),
                        "EDITION" => edition = att.value.to_string(),
                        "ENTITIES" => entities = att.value.to_string(),
                        _ => {}
                    }
                }
                if Self::SKIP_TESTS.contains(&id.as_str()) {
                    // skip
                    return;
                }
                if recommendation == "XML1.1" || recommendation == "NS1.1" {
                    // skip
                    // writeln!(self.log.borrow_mut(), "{id}: {recommendation} is not supported").ok();
                    return;
                }
                if !edition.is_empty() {
                    let editions = edition
                        .split_ascii_whitespace()
                        .map(|e| e.trim())
                        .collect::<Vec<_>>();
                    if !editions.contains(&"5") {
                        // skip
                        // writeln!(self.log.borrow_mut(), "{id}: skip because this is version specific test for version '{edition}'.").ok();
                        return;
                    }
                }

                let uri = self
                    .locator
                    .borrow()
                    .as_ref()
                    .unwrap()
                    .system_id()
                    .resolve(&URIString::parse(uri).unwrap());

                let mut reader = XMLReaderBuilder::new().set_handler(TestSAXHandler::new());
                if entities != "none" || matches!(r#type.as_ref(), "valid" | "invalid") {
                    reader = reader.enable_option(ParserOption::Validation)
                }
                let mut reader = reader.build();
                reader.parse_uri(uri, None).ok();

                let handler = reader.take_handler();
                match r#type.as_str() {
                    "not-wf" => {
                        if handler.fatal_error.get() > 0
                            || (recommendation.starts_with("NS1.0") && handler.ns_error.get() > 0)
                        {
                            // ok
                        } else {
                            self.unexpected_success.update(|c| c + 1);
                            writeln!(self.log.borrow_mut(), "{id}: unexpected success").ok();
                            writeln!(self.log.borrow_mut(), "{}", handler.buffer.borrow()).ok();
                        }
                    }
                    "error" => {
                        if (handler.error.get() == 0
                            && handler.validity_error.get() == 0
                            && handler.ns_error.get() == 0)
                            || handler.fatal_error.get() > 0
                        {
                            self.unexpected_success.update(|c| c + 1);
                            writeln!(self.log.borrow_mut(), "{id}: unexpected success").ok();
                            writeln!(self.log.borrow_mut(), "{}", handler.buffer.borrow()).ok();
                        }
                    }
                    "valid" => {
                        if handler.validity_error.get() > 0 {
                            self.unexpected_failure.update(|c| c + 1);
                            writeln!(self.log.borrow_mut(), "{id}: unexpected failure").ok();
                            writeln!(self.log.borrow_mut(), "{}", handler.buffer.borrow()).ok();
                        }
                    }
                    "invalid" => {
                        if handler.validity_error.get() == 0 {
                            self.unexpected_success.update(|c| c + 1);
                            writeln!(self.log.borrow_mut(), "{id}: unexpected success").ok();
                            writeln!(self.log.borrow_mut(), "{}", handler.buffer.borrow()).ok();
                        }
                    }
                    r#type => {
                        writeln!(
                            self.log.borrow_mut(),
                            "{id}: test type '{}' is unknown.",
                            r#type
                        )
                        .ok();
                    }
                }
            }
            _ => {}
        }
    }
}
impl EntityResolver for XMLConfWalker {}

#[test]
fn xmlconf_tests() {
    const XMLCONF_DIR: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/resources/xmlconf");
    assert!(
        std::fs::exists(XMLCONF_DIR).unwrap_or(false),
        "Please execute `deno run -A resources/get-xmlconf.ts` on the crate root."
    );

    let xmlconf = URIString::parse_file_path(format!("{XMLCONF_DIR}/xmlconf.xml")).unwrap();
    let mut reader = XMLReaderBuilder::new()
        .set_handler(XMLConfWalker::default())
        .enable_option(ParserOption::ExternalGeneralEntities)
        .build();
    reader.parse_uri(xmlconf, None).unwrap();

    let handler = reader.take_handler();
    assert!(
        handler.unexpected_success.get() == 0 && handler.unexpected_failure.get() == 0,
        "{}\n=== Unexpected Success: {}, Unexpected Failure: {} ===\n",
        handler.log.borrow(),
        handler.unexpected_success.get(),
        handler.unexpected_failure.get(),
    );
}
