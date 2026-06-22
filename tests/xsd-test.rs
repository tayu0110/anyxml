use std::{path::Path, sync::Arc};

use anyxml::{
    error::{XMLError, XMLErrorDomain},
    sax::{
        Attributes, DefaultParserSpec, DefaultSAXHandler, EntityResolver, ErrorHandler,
        InputSource, Locator, ParseError, SAXHandler, XMLReader, error::SAXParseError,
    },
    tree::{Document, TreeBuildHandler},
    uri::{URIStr, URIString},
    xsd::XSDSchema,
};

const XSTS_BASE_DIRNAME: &str = "resources/xmlschema2006-11-06/";
const XSTS_NAMESPACE: &str = "http://www.w3.org/XML/2004/xml-schema-test-suite/";
const XLINK_NAMESPACE: &str = "http://www.w3.org/1999/xlink";

// Although namespace names must not be relative references, the test suite does not adhere to
// this rule, resulting in a large number of namespace errors; therefore, implement a handler
// to suppress them.
// In practice, whether a namespace name is absolute or relative has no essential impact,
// so this is a sufficient workaround.
struct NSErrorSuppressingHandler;
impl SAXHandler for NSErrorSuppressingHandler {}
impl EntityResolver for NSErrorSuppressingHandler {
    fn resolve_entity(
        &mut self,
        name: &str,
        public_id: Option<&str>,
        base_uri: &URIStr,
        system_id: &URIStr,
    ) -> Result<InputSource<'static>, XMLError> {
        DefaultSAXHandler.resolve_entity(name, public_id, base_uri, system_id)
    }
    fn get_external_subset(
        &mut self,
        name: &str,
        base_uri: Option<&URIStr>,
    ) -> Result<InputSource<'static>, XMLError> {
        DefaultSAXHandler.get_external_subset(name, base_uri)
    }
}
impl ErrorHandler for NSErrorSuppressingHandler {
    fn fatal_error(&mut self, error: SAXParseError) {
        if !matches!(error.domain, XMLErrorDomain::Namespace)
            && !matches!(
                error.error,
                XMLError::XMLParseError(ParseError::NamespaceNameNotAbsoluteURI)
            )
        {
            DefaultSAXHandler.fatal_error(error);
        }
    }
    fn error(&mut self, error: SAXParseError) {
        if !matches!(error.domain, XMLErrorDomain::Namespace)
            && !matches!(
                error.error,
                XMLError::XMLParseError(ParseError::NamespaceNameNotAbsoluteURI)
            )
        {
            DefaultSAXHandler.error(error);
        }
    }
    fn warning(&mut self, error: SAXParseError) {
        if !matches!(error.domain, XMLErrorDomain::Namespace)
            && !matches!(
                error.error,
                XMLError::XMLParseError(ParseError::NamespaceNameNotAbsoluteURI)
            )
        {
            DefaultSAXHandler.warning(error);
        }
    }
}

struct XSTSWalker<'a> {
    reader: XMLReader<DefaultParserSpec<'a>, TreeBuildHandler<NSErrorSuppressingHandler>>,
    locator: Arc<Locator>,

    schema: Option<XSDSchema>,
    instance: Option<Document>,
    validity: String,
    status: String,

    schema_test_case: usize,
    schema_test_failure: usize,
    instance_test_case: usize,
    instance_test_failure: usize,
}

impl XSTSWalker<'_> {
    fn new() -> Self {
        Self {
            reader: XMLReader::builder()
                .set_handler(TreeBuildHandler::with_handler(NSErrorSuppressingHandler))
                .build(),
            locator: Arc::new(Locator::default()),
            schema: None,
            instance: None,
            validity: String::new(),
            status: String::new(),
            schema_test_case: 0,
            schema_test_failure: 0,
            instance_test_case: 0,
            instance_test_failure: 0,
        }
    }
}

impl ErrorHandler for XSTSWalker<'_> {}
impl EntityResolver for XSTSWalker<'_> {}
impl SAXHandler for XSTSWalker<'_> {
    fn set_document_locator(&mut self, locator: Arc<Locator>) {
        self.locator = locator;
        self.schema = None;
        self.instance = None;
        self.schema_test_case = 0;
        self.schema_test_failure = 0;
        self.instance_test_case = 0;
        self.instance_test_failure = 0;
    }

    fn start_element(
        &mut self,
        namespace_name: Option<&str>,
        local_name: Option<&str>,
        _qname: &str,
        atts: &Attributes,
    ) {
        if namespace_name != Some(XSTS_NAMESPACE) {
            return;
        }

        match local_name {
            Some("testSet") => {
                let name = atts.get_value_by_qname("name").unwrap();
                let contributer = atts.get_value_by_qname("contributer").unwrap_or("unknown");
                eprintln!("=== Start testset: {name} ===");
                eprintln!("===    path       : {} ===", self.locator.system_id());
                eprintln!("===    contributer: {} ===", contributer);
            }
            Some("testGroup") => {
                let name = atts.get_value_by_qname("name").unwrap();
                eprintln!("----- Start test group: {name} -----");
                self.schema = None;
                self.instance = None;
            }
            Some("schemaTest") => {
                let name = atts.get_value_by_qname("name").unwrap();
                eprintln!("....... Start schema test: {name} .......");
                // Since some test cases do not include `current`,
                // I'll set it to “accepted” by default.
                self.status = "accepted".into();
            }
            Some("schemaDocument") => {
                let href = atts
                    .get_value_by_expanded_name(Some(XLINK_NAMESPACE), "href")
                    .unwrap();
                let href = self
                    .locator
                    .system_id()
                    .resolve(&URIString::parse(href).unwrap());
                self.reader.parse_uri(&href, None).unwrap();
                assert!(!self.reader.handler.fatal_error);
                self.schema = XSDSchema::from_schema(&self.reader.handler.document).ok();
            }
            Some("instanceTest") => {
                let name = atts.get_value_by_qname("name").unwrap();
                eprintln!("....... Start instance test: {name} .......");
                // Since some test cases do not include `current`,
                // I'll set it to “accepted” by default.
                self.status = "accepted".into();
            }
            Some("instanceDocument") => {
                let href = atts
                    .get_value_by_expanded_name(Some(XLINK_NAMESPACE), "href")
                    .unwrap();
                let href = self
                    .locator
                    .system_id()
                    .resolve(&URIString::parse(href).unwrap());
                // Since the instance documents are broken (and not even well-formed)
                // in some test cases, I will enable skipping those tests.
                self.instance = self
                    .reader
                    .parse_uri(&href, None)
                    .ok()
                    .filter(|_| !self.reader.handler.fatal_error)
                    .map(|_| self.reader.handler.document.clone());
            }
            Some("expected") => {
                self.validity = atts.get_value_by_qname("validity").unwrap().into();
            }
            Some("current") => {
                self.status = atts.get_value_by_qname("status").unwrap().into();
            }
            _ => {}
        }
    }
    fn end_element(
        &mut self,
        namespace_name: Option<&str>,
        local_name: Option<&str>,
        _qname: &str,
    ) {
        if namespace_name != Some(XSTS_NAMESPACE) {
            return;
        }

        match local_name {
            Some("testSet") => {
                eprintln!();
                eprintln!("Complete:");
                eprintln!(
                    "    schema test failure  : {:>4}/{}",
                    self.schema_test_failure, self.schema_test_case
                );
                eprintln!(
                    "    instance test failure: {:>4}/{}",
                    self.instance_test_failure, self.instance_test_case
                );
                eprintln!();
            }
            Some("schemaTest") => {
                if !matches!(self.validity.as_str(), "valid" | "invalid") {
                    eprintln!(
                        "Skip because validity is neither 'valid' nor 'invalid' (actual={})",
                        self.validity
                    );
                    return;
                }
                if !matches!(self.status.as_str(), "accepted" | "stable") {
                    eprintln!(
                        "Skip because status is neither 'acepted' nor 'stable' (actual='{}')",
                        self.status
                    );
                    return;
                }

                self.schema_test_case += 1;
                if self.schema.is_some() && self.validity != "valid" {
                    eprintln!("Unexpected success to parse schema");
                    self.schema_test_failure += 1;
                } else if self.schema.is_none() && self.validity != "invalid" {
                    eprintln!("Unexpected fail to parse schema");
                    self.schema_test_failure += 1;
                }
            }
            Some("instanceTest") => {
                if !matches!(self.status.as_str(), "accepted" | "stable") {
                    eprintln!(
                        "Skip because status is neither 'acepted' nor 'stable' (actual='{}')",
                        self.status
                    );
                    return;
                }
                let Some(instance) = self.instance.as_ref() else {
                    eprintln!("Skip because this instance document is non-well-formed document.");
                    return;
                };
                let Some(schema) = self.schema.as_ref() else {
                    eprintln!("Skip because the schema is not ready.");
                    return;
                };
                self.instance_test_case += 1;
                let errors = schema.validate(instance);
                let instance_path = instance.document_base_uri();
                match self.validity.as_str() {
                    "valid" => {
                        if !errors.is_empty() {
                            eprintln!("Unexpected fail to validate instance '{}'", instance_path);
                            eprintln!("    errors: {:?}", errors);
                            self.instance_test_failure += 1;
                        }
                    }
                    "invalid" => {
                        if errors.is_empty() {
                            eprintln!(
                                "Unexpected success to validate instance '{}'",
                                instance_path
                            );
                            self.instance_test_failure += 1;
                        }
                    }
                    "implementation-defined" => {
                        eprintln!(
                            "Implementation defined result for '{}': errors: {:?}",
                            instance_path, errors
                        );
                    }
                    "implementation-dependent" => {
                        eprintln!(
                            "Implementation dependent result for '{}': errors: {:?}",
                            instance_path, errors
                        );
                    }
                    "indeterminate" => {
                        eprintln!(
                            "Indeterminate result for '{}': errors: {:?}",
                            instance_path, errors
                        );
                    }
                    _ => unreachable!(),
                }
            }
            _ => {}
        }
    }
}

#[test]
fn test_suite() {
    let base = Path::new(XSTS_BASE_DIRNAME).canonicalize().unwrap();
    let base = URIString::parse_file_path(base).unwrap();
    let mut reader = XMLReader::builder()
        .set_handler(TreeBuildHandler::with_handler(NSErrorSuppressingHandler))
        .build();
    reader
        .parse_uri(base.resolve(&URIString::parse("suite.xml").unwrap()), None)
        .unwrap();
    assert!(!reader.handler.fatal_error);

    let suite = reader.handler.document;
    let mut reader = XMLReader::builder().set_handler(XSTSWalker::new()).build();
    let mut case = 0;
    let mut fail = 0;
    for testset_ref in suite.get_elements_by_expanded_name("testSetRef", Some(XSTS_NAMESPACE)) {
        let href = testset_ref
            .get_attribute("href", Some(XLINK_NAMESPACE))
            .unwrap();
        let href = base.resolve(&URIString::parse(&href).unwrap());
        reader.parse_uri(&href, None).unwrap();
        case += reader.handler.schema_test_case + reader.handler.instance_test_case;
        fail += reader.handler.schema_test_failure + reader.handler.instance_test_failure;
    }
    eprintln!("Complete all test: case: {} failure: {}", case, fail);
    assert_eq!(fail, 0);
}
