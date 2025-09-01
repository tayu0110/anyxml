#![allow(clippy::arc_with_non_send_sync)]

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
        handler::{ContentHandler, ErrorHandler},
        parser::{ParserOption, XMLReaderBuilder},
    },
};
use anyxml_uri::uri::URIString;

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

impl ContentHandler for TestSAXHandler {
    fn start_element(
        &self,
        _uri: Option<&str>,
        _local_name: Option<&str>,
        qname: &str,
        _atts: &[anyxml::sax::Attribute],
    ) {
        eprintln!("startElement('{qname}')");
    }

    fn end_element(&self, _uri: Option<&str>, _local_name: Option<&str>, qname: &str) {
        eprintln!("endElement('{qname}')");
    }

    fn characters(&self, data: &str) {
        eprintln!("characters('{data}')");
    }

    fn ignorable_whitespace(&self, data: &str) {
        eprintln!("ignorableWhitespace({})", data.len());
    }
}

impl ErrorHandler for TestSAXHandler {
    fn fatal_error(&self, error: anyxml::sax::error::SAXParseError) {
        assert_eq!(error.level, XMLErrorLevel::FatalError);
        self.fatal_error.update(|c| c + 1);
        writeln!(self.buffer.borrow_mut(), "{}", error).ok();
    }

    fn error(&self, error: anyxml::sax::error::SAXParseError) {
        assert_eq!(error.level, XMLErrorLevel::Error);
        match error.domain {
            XMLErrorDomain::Parser => self.error.update(|c| c + 1),
            XMLErrorDomain::Namespace => self.ns_error.update(|c| c + 1),
            XMLErrorDomain::DTDValid => self.validity_error.update(|c| c + 1),
        }
        writeln!(self.buffer.borrow_mut(), "{}", error).ok();
    }

    fn warning(&self, error: anyxml::sax::error::SAXParseError) {
        assert_eq!(error.level, XMLErrorLevel::Warning);
        self.warning.update(|c| c + 1);
        writeln!(self.buffer.borrow_mut(), "{}", error).ok();
    }
}

#[test]
fn well_formed_tests() {
    let handler = Arc::new(TestSAXHandler::new());

    let mut reader = XMLReaderBuilder::new()
        .set_error_handler(handler.clone() as _)
        .build();

    for ent in read_dir("resources/well-formed").unwrap() {
        if let Ok(ent) = ent
            && ent.metadata().unwrap().is_file()
        {
            let uri = URIString::parse_file_path(ent.path().canonicalize().unwrap()).unwrap();
            reader.parse_uri(&uri, None).ok();
            assert_eq!(
                handler.fatal_error.get(),
                0,
                "uri: {}\nerrors:\n{}",
                uri.as_escaped_str(),
                handler.buffer.borrow(),
            );
            handler.reset();
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

impl ContentHandler for XMLConfWalker {
    fn set_document_locator(&self, locator: Arc<Locator>) {
        *self.locator.borrow_mut() = Some(locator);
    }

    fn start_element(
        &self,
        _uri: Option<&str>,
        _local_name: Option<&str>,
        qname: &str,
        atts: &[anyxml::sax::Attribute],
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

                let handler = Arc::new(TestSAXHandler::new());
                let mut reader = XMLReaderBuilder::new().set_error_handler(handler.clone() as _);
                if entities != "none" || matches!(r#type.as_ref(), "valid" | "invalid") {
                    reader = reader.enable_option(ParserOption::Validation)
                }
                // if uri.as_escaped_str().contains("el06.xml") {
                //     reader = reader.set_content_handler(Arc::new(TestSAXHandler::new()));
                // }
                let mut reader = reader.build();
                reader.parse_uri(uri, None).ok();

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
                        // skip
                        // writeln!(
                        //     self.log.borrow_mut(),
                        //     "{id}: test type 'error' is not yet supported.",
                        //     r#type
                        // )
                        // .ok();
                    }
                    "valid" => {
                        // skip
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

#[test]
fn xmlconf_tests() {
    const XMLCONF_DIR: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/resources/xmlconf");
    assert!(
        std::fs::exists(XMLCONF_DIR).unwrap_or(false),
        "Please execute `deno run -A resources/get-xmlconf.ts on the crate root.`"
    );

    let handler = Arc::new(XMLConfWalker::default());
    let xmlconf = URIString::parse_file_path(format!("{XMLCONF_DIR}/xmlconf.xml")).unwrap();
    let mut reader = XMLReaderBuilder::new()
        .set_content_handler(handler.clone() as _)
        .enable_option(ParserOption::ExternalGeneralEntities)
        .build();
    reader.parse_uri(xmlconf, None).unwrap();

    assert!(
        handler.unexpected_success.get() == 0 && handler.unexpected_failure.get() == 0,
        "{}\n=== Unexpected Success: {}, Unexpected Failure: {} ===\n",
        handler.log.borrow(),
        handler.unexpected_success.get(),
        handler.unexpected_failure.get(),
    );
}
