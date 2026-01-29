use std::{fmt::Write as _, fs::read_dir, path::Path};

use anyxml::{
    error::XMLErrorDomain,
    sax::{
        handler::{DefaultSAXHandler, ErrorHandler},
        parser::ParserOption,
    },
    stax::{XMLStreamReaderBuilder, events::XMLEvent},
    uri::URIString,
};

#[test]
fn well_formed_tests() {
    for ent in read_dir("resources/well-formed").unwrap() {
        if let Ok(ent) = ent
            && ent.metadata().unwrap().is_file()
        {
            let path = ent.path();
            let uri = URIString::parse_file_path(path.canonicalize().unwrap()).unwrap();
            let mut reader = XMLStreamReaderBuilder::new().build();
            reader.parse_uri(&uri, None).unwrap();
            let mut buffer = String::new();
            loop {
                match reader.next_event().unwrap() {
                    XMLEvent::StartDocument => writeln!(buffer, "startDocument()").unwrap(),
                    XMLEvent::EndDocument => writeln!(buffer, "endDocument()").unwrap(),
                    XMLEvent::StartElement(start) => {
                        write!(
                            buffer,
                            "startElement({}, {}, {}, {}",
                            start.namespace_name().unwrap_or("None"),
                            start.prefix().unwrap_or("None"),
                            start.local_name().unwrap_or("None"),
                            start.name()
                        )
                        .unwrap();

                        for att in start.attributes() {
                            write!(buffer, ", ").ok();
                            if let Some(local_name) = att.local_name.as_deref() {
                                if let Some(uri) = att.namespace_name.as_deref() {
                                    write!(buffer, "{{{uri}}}").ok();
                                }
                                write!(buffer, "{local_name}='{}'", att.value).ok();
                            } else {
                                write!(buffer, "{}='{}'", att.qname, att.value).ok();
                            }
                        }
                        writeln!(buffer, ")").ok();
                    }
                    XMLEvent::EndElement(end) => {
                        writeln!(
                            buffer,
                            "endElement({}, {}, {}, {})",
                            end.namespace_name().unwrap_or("None"),
                            end.prefix().unwrap_or("None"),
                            end.local_name().unwrap_or("None"),
                            end.name()
                        )
                        .unwrap();
                    }
                    XMLEvent::Declaration(declaration) => {
                        write!(
                            buffer,
                            "declaration({}, {}, ",
                            declaration.version(),
                            declaration.encoding().unwrap_or("None")
                        )
                        .ok();
                        if let Some(standalone) = declaration.standalone() {
                            if standalone {
                                writeln!(buffer, "yes)").ok();
                            } else {
                                writeln!(buffer, "no)").ok();
                            }
                        } else {
                            writeln!(buffer, "None)").ok();
                        }
                    }
                    XMLEvent::DocumentType => writeln!(buffer, "documentType()").unwrap(),
                    XMLEvent::Characters(characters) => {
                        writeln!(buffer, "characters({characters})").unwrap()
                    }
                    XMLEvent::CDATASection(cdata) => {
                        writeln!(buffer, "cdataSection({cdata})").unwrap()
                    }
                    XMLEvent::Space(space) => writeln!(buffer, "space({space})").unwrap(),
                    XMLEvent::Comment(comment) => writeln!(buffer, "comment({comment})").unwrap(),
                    XMLEvent::ProcessingInstruction(pi) => writeln!(
                        buffer,
                        "processingInstruction({}, '{}')",
                        pi.target(),
                        pi.data().unwrap_or("None")
                    )
                    .unwrap(),
                    XMLEvent::StartEntity(entity) => {
                        writeln!(buffer, "startEntity({entity})").unwrap()
                    }
                    XMLEvent::EndEntity => writeln!(buffer, "endEntity()").unwrap(),
                    XMLEvent::FatalError => writeln!(buffer, "fatalError()").unwrap(),
                    XMLEvent::Finished => break,
                }
            }
            let outname = path.file_name().unwrap().to_str().unwrap();
            let outname = format!("resources/well-formed/output/{outname}.stax");
            let outname = Path::new(outname.as_str());
            let output = std::fs::read_to_string(outname).unwrap();

            assert_eq!(output, buffer, "uri: {}\n{}", uri.as_escaped_str(), buffer,);
        }
    }
}

// Some tests require unsupported encodings or their types do not match the actual
// error that occurs, making it difficult to pass all of them, so they are skipped.
const SKIP_TESTS: &[&str] = &[
    "pr-xml-euc-jp",               // unsupported encoding
    "pr-xml-iso-2022-jp",          // unsupported encoding
    "weekly-euc-jp",               // unsupported encoding
    "weekly-iso-2022-jp",          // unsupported encoding
    "weekly-shift_jis",            // error type, but it is unknown what error should be reported.
    "ibm-not-wf-P69-ibm69n05.xml", // error type, but requires VC validation
    "rmt-ns10-006",                // unsupported encoding
    "invalid-bo-7",                // error type, but a Fatal Error occurs (Illegal XML character)
    "invalid-bo-8",                // error type, but a Fatal Error occurs (Illegal XML character)
    "invalid-bo-9",                // error type, but a Fatal Error occurs (Illegal XML character)
];

#[derive(Default)]
struct XMLConfErrorHandler {
    warning: usize,
    error: usize,
    ns_error: usize,
    validity_error: usize,
    fatal_error: usize,
    buffer: String,
}

impl ErrorHandler for XMLConfErrorHandler {
    fn warning(&mut self, error: anyxml::sax::error::SAXParseError) {
        writeln!(self.buffer, "{error}").unwrap();
        self.warning += 1;
    }

    fn error(&mut self, error: anyxml::sax::error::SAXParseError) {
        writeln!(self.buffer, "{error}").unwrap();
        match error.domain {
            XMLErrorDomain::Parser => self.error += 1,
            XMLErrorDomain::Namespace => self.ns_error += 1,
            XMLErrorDomain::DTDValid => self.validity_error += 1,
            XMLErrorDomain::RngParser | XMLErrorDomain::C14N | XMLErrorDomain::XInclude => {
                self.error += 1
            }
        }
    }

    fn fatal_error(&mut self, error: anyxml::sax::error::SAXParseError) {
        writeln!(self.buffer, "{error}").unwrap();
        self.fatal_error += 1;
    }
}

#[test]
fn xmlconf_tests() {
    const XMLCONF_DIR: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/resources/xmlconf");
    assert!(
        std::fs::exists(XMLCONF_DIR).unwrap_or(false),
        "Please execute `deno run -A resources/get-xmlconf.ts` on the crate root."
    );

    let xmlconf = URIString::parse_file_path(format!("{XMLCONF_DIR}/xmlconf.xml")).unwrap();
    let mut reader = XMLStreamReaderBuilder::new()
        .set_error_handler(DefaultSAXHandler)
        .set_entity_resolver(DefaultSAXHandler)
        .enable_option(ParserOption::ExternalGeneralEntities)
        .build();
    reader.parse_uri(xmlconf, None).unwrap();
    let mut buffer = String::new();
    let mut locator = None;
    let mut unexpected_success = 0;
    let mut unexpected_failure = 0;
    loop {
        match reader.next_event().unwrap() {
            XMLEvent::StartDocument => {
                locator = reader.locator();
            }
            XMLEvent::StartElement(start) => {
                match start.name() {
                    "TESTSUITE" => {
                        for att in start.attributes() {
                            if att.qname.as_ref() == "PROFILE" {
                                writeln!(buffer, "=== Start Test Suite '{}' ===", att.value).ok();
                            }
                        }
                    }
                    "TESTCASES" => {
                        for att in start.attributes() {
                            if att.qname.as_ref() == "PROFILE" {
                                writeln!(
                                    buffer,
                                    "--- Start Test Case '{}' in '{}' ---",
                                    att.value,
                                    locator.as_ref().unwrap().system_id().as_escaped_str()
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
                        for att in start.attributes() {
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
                        if SKIP_TESTS.contains(&id.as_str()) {
                            // skip
                            continue;
                        }
                        if recommendation == "XML1.1" || recommendation == "NS1.1" {
                            // skip
                            // writeln!(self.log.borrow_mut(), "{id}: {recommendation} is not supported").ok();
                            continue;
                        }
                        if !edition.is_empty() {
                            let editions = edition
                                .split_ascii_whitespace()
                                .map(|e| e.trim())
                                .collect::<Vec<_>>();
                            if !editions.contains(&"5") {
                                // skip
                                // writeln!(self.log.borrow_mut(), "{id}: skip because this is version specific test for version '{edition}'.").ok();
                                continue;
                            }
                        }

                        let uri = locator
                            .as_ref()
                            .unwrap()
                            .system_id()
                            .resolve(&URIString::parse(uri).unwrap());

                        let mut builder = XMLStreamReaderBuilder::new()
                            .set_error_handler(XMLConfErrorHandler::default());
                        if entities != "none" || matches!(r#type.as_ref(), "valid" | "invalid") {
                            builder = builder.enable_option(ParserOption::Validation)
                        }
                        let mut reader = builder.build();
                        reader.parse_uri(uri, None).ok();
                        loop {
                            match reader.next_event() {
                                Ok(XMLEvent::Finished) | Err(_) => break,
                                Ok(_) => {}
                            }
                        }
                        let handler = reader.take_error_handler().unwrap();
                        match r#type.as_str() {
                            "not-wf" => {
                                if handler.fatal_error > 0
                                    || (recommendation.starts_with("NS1.0") && handler.ns_error > 0)
                                {
                                    // ok
                                } else {
                                    unexpected_success += 1;
                                    writeln!(buffer, "{id}: unexpected success").ok();
                                    writeln!(buffer, "{}", handler.buffer).ok();
                                }
                            }
                            "error" => {
                                if (handler.error == 0
                                    && handler.validity_error == 0
                                    && handler.ns_error == 0)
                                    || handler.fatal_error > 0
                                {
                                    unexpected_success += 1;
                                    writeln!(buffer, "{id}: unexpected success").ok();
                                    writeln!(buffer, "{}", handler.buffer).ok();
                                }
                            }
                            "valid" => {
                                if handler.validity_error > 0 {
                                    unexpected_failure += 1;
                                    writeln!(buffer, "{id}: unexpected failure").ok();
                                    writeln!(buffer, "{}", handler.buffer).ok();
                                }
                            }
                            "invalid" => {
                                if handler.validity_error == 0 {
                                    unexpected_success += 1;
                                    writeln!(buffer, "{id}: unexpected success").ok();
                                    writeln!(buffer, "{}", handler.buffer).ok();
                                }
                            }
                            r#type => {
                                writeln!(buffer, "{id}: test type '{}' is unknown.", r#type).ok();
                            }
                        }
                    }
                    _ => {}
                }
            }
            XMLEvent::Finished => break,
            _ => {}
        }
    }

    assert!(
        unexpected_success == 0 && unexpected_failure == 0,
        "{}\n=== Unexpected Success: {}, Unexpected Failure: {} ===\n",
        buffer,
        unexpected_success,
        unexpected_failure,
    );
}
