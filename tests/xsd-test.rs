use std::{convert::Infallible, str::FromStr};

use anyxml::{
    error::XMLError,
    sax::{
        DefaultParserSpec, DefaultSAXHandler, EntityResolver, ErrorHandler, InputSource,
        ParseError, SAXHandler, XMLReader, error::SAXParseError,
    },
    tree::{Element, TreeBuildHandler},
    uri::{URIStr, URIString},
    xsd::XSDSchema,
};

const XML_XSTS_NAMESPACE: &str = "http://www.w3.org/XML/2004/xml-schema-test-suite/";
const XML_XLINK_NAMESPACE: &str = "http://www.w3.org/1999/xlink";

#[derive(Default)]
struct TestResult {
    st_total: usize,
    st_skipped: usize,
    st_unexpected_success: usize,
    st_unexpected_failure: usize,
    it_total: usize,
    it_skipped: usize,
    it_unexpected_success: usize,
    it_unexpected_failure: usize,
}

#[derive(Clone, Copy)]
enum TestOutcome {
    Valid,
    Invalid,
    NotKnown,
    RuntimeSchemaError,
    ImplementationDefined,
    ImplementationDependent,
    Indeterminate,
    InvalidLatent,
}

impl FromStr for TestOutcome {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "valid" => Ok(Self::Valid),
            "invalid" => Ok(Self::Invalid),
            "notKnown" => Ok(Self::NotKnown),
            "runtime-schema-error" => Ok(Self::RuntimeSchemaError),
            "implementation-defined" => Ok(Self::ImplementationDefined),
            "implementation-dependent" => Ok(Self::ImplementationDependent),
            "indeterminate" => Ok(Self::Indeterminate),
            "invalid-latent" => Ok(Self::InvalidLatent),
            _ => unreachable!(),
        }
    }
}

#[derive(Default)]
struct SchemaTestHandler {
    fatal_error: usize,
    error: usize,
}

impl EntityResolver for SchemaTestHandler {
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
impl ErrorHandler for SchemaTestHandler {
    fn warning(&mut self, _error: SAXParseError) {
        // supress warnings
        // DefaultSAXHandler.warning(error);
    }
    fn error(&mut self, error: SAXParseError) {
        if matches!(error.error, XMLError::XSDError(_)) {
            self.error += 1;
        }
        if !matches!(
            error.error,
            XMLError::XMLParseError(ParseError::NamespaceNameNotAbsoluteURI)
        ) {
            // The test suite uses a large number of namespace names that are not absolute URIs,
            // but since they are not fundamental errors, I will suppress they here.
            DefaultSAXHandler.error(error);
        }
    }
    fn fatal_error(&mut self, error: SAXParseError) {
        self.fatal_error += 1;
        DefaultSAXHandler.fatal_error(error);
    }
}
impl SAXHandler for SchemaTestHandler {}

fn schema_test(node: Element, base_uri: &URIStr, out: &mut TestResult) {
    let name = node
        .get_attribute("name", None)
        .unwrap_or_else(|| "unknown".into());
    eprintln!("--- Start schema test '{name}' ---");
    let validity = node
        .get_elements_by_expanded_name("expected", Some(XML_XSTS_NAMESPACE))
        .next()
        .and_then(|e| e.get_attribute("validity", None))
        .unwrap()
        .parse::<TestOutcome>()
        .unwrap();
    for schema in node.get_elements_by_expanded_name("schemaDocument", Some(XML_XSTS_NAMESPACE)) {
        let href = schema
            .get_attribute("href", Some(XML_XLINK_NAMESPACE))
            .unwrap();
        let href = URIString::parse(href).unwrap();
        let href = base_uri.resolve(&href);
        eprintln!("----- Start parsing '{href}' -----");

        let mut handler = SchemaTestHandler::default();
        let schema = XSDSchema::parse_uri(&href, None, Some(&mut handler));
        out.st_total += 1;
        match validity {
            TestOutcome::Valid => {
                if schema.is_err() || handler.fatal_error != 0 || handler.error != 0 {
                    out.st_unexpected_failure += 1;
                    eprintln!("The schema test '{href}' is unexpected failure.");
                }
            }
            TestOutcome::Invalid => {
                if schema.is_ok() && handler.fatal_error == 0 && handler.error == 0 {
                    out.st_unexpected_success += 1;
                    eprintln!("The schema test '{href}' is unexpected success.");
                }
            }
            TestOutcome::NotKnown => {
                out.st_skipped += 1;
                eprintln!("'notKnown' is is meaningless for schema tests.");
            }
            TestOutcome::RuntimeSchemaError => {
                out.st_skipped += 1;
                eprintln!("'runtime-schema-error' is is meaningless for schema tests.");
            }
            TestOutcome::ImplementationDefined => {
                out.st_skipped += 1;
                eprintln!(
                    "Implementation defined: error={}, fatal error={}",
                    handler.error, handler.fatal_error
                );
            }
            TestOutcome::ImplementationDependent => {
                out.st_skipped += 1;
                eprintln!(
                    "Implementation dependent: error={}, fatal error={}",
                    handler.error, handler.fatal_error
                );
            }
            TestOutcome::Indeterminate => {
                out.st_skipped += 1;
                eprintln!(
                    "Inderminate: error={}, fatal error={}",
                    handler.error, handler.fatal_error
                );
            }
            TestOutcome::InvalidLatent => {
                out.st_skipped += 1;
                eprintln!(
                    "InvalidLatent: error={}, fatal error={}",
                    handler.error, handler.fatal_error
                );
            }
        }
    }
}

fn instance_test(node: Element, base_uri: &URIStr, out: &mut TestResult) {
    let name = node
        .get_attribute("name", None)
        .unwrap_or_else(|| "unknown".into());
    eprintln!("--- Start instance test '{name}' ---");
    let validity = node
        .get_elements_by_expanded_name("expected", Some(XML_XSTS_NAMESPACE))
        .next()
        .and_then(|e| e.get_attribute("validity", None))
        .unwrap()
        .parse::<TestOutcome>()
        .unwrap();
    for instance in node.get_elements_by_expanded_name("instanceDocument", Some(XML_XSTS_NAMESPACE))
    {
        let href = instance
            .get_attribute("href", Some(XML_XLINK_NAMESPACE))
            .unwrap();
        let href = URIString::parse(href).unwrap();
        let href = base_uri.resolve(&href);
        eprintln!("----- Start parsing '{href}' -----");

        let mut schema = XSDSchema::default();
        let mut validator = schema.new_validate_handler(SchemaTestHandler::default());
        validator.use_schema_location_hint = true;
        let mut reader = XMLReader::builder().set_handler(validator).build();
        reader.parse_uri(&href, None).ok();
        let fatal_error = reader.handler.child.fatal_error;
        if fatal_error > 0 {
            eprintln!("The instance test target '{href}' is corrupted, so skip this.");
            out.it_skipped += 1;
            continue;
        }
        let error = reader.handler.child.error;
        out.it_total += 1;
        match validity {
            TestOutcome::Valid => {
                if error > 0 {
                    out.it_unexpected_failure += 1;
                    eprint!("The instance test '{href}' is unexpected failure.");
                }
            }
            TestOutcome::Invalid => {
                if error == 0 {
                    out.it_unexpected_success += 1;
                    eprintln!("The instance test '{href}' is unexpected success.");
                }
            }
            TestOutcome::NotKnown => {
                out.it_skipped += 1;
                eprintln!("'notKnown' is not supported for instance tests.");
            }
            TestOutcome::RuntimeSchemaError => {
                if error == 0 {
                    out.it_unexpected_success += 1;
                    eprintln!("'runtime-schema-error' could not detected for '{href}'.");
                }
            }
            TestOutcome::ImplementationDefined => {
                out.it_skipped += 1;
                eprintln!(
                    "Implementation defined: error={}, fatal error={}",
                    error, fatal_error
                );
            }
            TestOutcome::ImplementationDependent => {
                out.it_skipped += 1;
                eprintln!(
                    "Implementation dependent: error={}, fatal error={}",
                    error, fatal_error
                );
            }
            TestOutcome::Indeterminate => {
                out.it_skipped += 1;
                eprintln!("Inderminate: error={}, fatal error={}", error, fatal_error);
            }
            TestOutcome::InvalidLatent => {
                out.it_skipped += 1;
                eprintln!(
                    "InvalidLatent: error={}, fatal error={}",
                    error, fatal_error
                );
            }
        }
    }
}

fn testset(
    uri: &URIStr,
    reader: &mut XMLReader<DefaultParserSpec, TreeBuildHandler>,
) -> TestResult {
    reader.parse_uri(uri, None).unwrap();

    let testset = reader.handler.document.document_element().unwrap();
    let contributor = testset
        .get_attribute("contributor", None)
        .unwrap_or_else(|| "unknown".into());
    let name = testset
        .get_attribute("name", None)
        .unwrap_or_else(|| "unknown".into());
    let banner = format!("=== Start test set '{name}' ===");
    eprintln!("{banner}");
    eprintln!("\tname       : {name}");
    eprintln!("\tcontributor: {contributor}");
    eprintln!("\tURI        : {uri}");
    eprintln!("{}", "=".repeat(banner.len()));

    let mut test_result = TestResult::default();
    for group in testset.get_elements_by_expanded_name("testGroup", Some(XML_XSTS_NAMESPACE)) {
        let name = group
            .get_attribute("name", None)
            .unwrap_or_else(|| "unknown".into());

        eprintln!("### Start test group '{name}' ###");
        for st in group.get_elements_by_expanded_name("schemaTest", Some(XML_XSTS_NAMESPACE)) {
            schema_test(st, uri, &mut test_result);
        }
        for it in group.get_elements_by_expanded_name("instanceTest", Some(XML_XSTS_NAMESPACE)) {
            instance_test(it, uri, &mut test_result);
        }
    }
    eprintln!();
    eprintln!(
        "schema test  : total={}, skipped={}, unexpected success={}, unexpected failure={}",
        test_result.st_total,
        test_result.st_skipped,
        test_result.st_unexpected_success,
        test_result.st_unexpected_failure
    );
    eprintln!(
        "instance test: total={}, skipped={}, unexpected success={}, unexpected failure={}",
        test_result.it_total,
        test_result.it_skipped,
        test_result.it_unexpected_success,
        test_result.it_unexpected_failure
    );
    eprintln!("=== End test set '{name}' ===");
    eprintln!();
    test_result
}

#[test]
fn xsts_tests() {
    std::env::set_current_dir(env!("CARGO_MANIFEST_DIR")).unwrap();

    let mut handler = TreeBuildHandler::default();
    handler.expand_entity_reference = true;
    handler.coalescing = true;
    handler.ignoring_comments = true;
    let mut reader = XMLReader::builder().set_handler(handler).build();
    reader
        .parse_uri(
            URIString::parse("resources/xmlschema2006-11-06/suite.xml").unwrap(),
            None,
        )
        .unwrap();

    let suite = reader.handler.document.clone();
    let base_uri = suite.document_base_uri();
    let mut result = TestResult::default();
    for reference in suite.get_elements_by_expanded_name("testSetRef", Some(XML_XSTS_NAMESPACE)) {
        let href = reference
            .get_attribute("href", Some(XML_XLINK_NAMESPACE))
            .unwrap();
        let href = URIString::parse(href).unwrap();
        let href = base_uri.resolve(&href);
        let ret = testset(&href, &mut reader);
        result.st_total += ret.st_total;
        result.st_skipped += ret.st_skipped;
        result.st_unexpected_success += ret.st_unexpected_success;
        result.st_unexpected_failure += ret.st_unexpected_failure;
        result.it_total += ret.it_total;
        result.it_skipped += ret.it_skipped;
        result.it_unexpected_success += ret.it_unexpected_success;
        result.it_unexpected_failure += ret.it_unexpected_failure;
    }

    eprintln!("====================================");
    eprintln!("Schema test:");
    eprintln!("\ttotal              : {}", result.st_total);
    eprintln!("\tskipped            : {}", result.st_skipped);
    eprintln!("\tunexpected success : {}", result.st_unexpected_success);
    eprintln!("\tunexpected failure : {}", result.st_unexpected_failure);
    eprintln!("Instance test:");
    eprintln!("\ttotal              : {}", result.it_total);
    eprintln!("\tskipped            : {}", result.it_skipped);
    eprintln!("\tunexpected success : {}", result.it_unexpected_success);
    eprintln!("\tunexpected failure : {}", result.it_unexpected_failure);
    eprintln!("====================================");
    assert!(
        result.st_unexpected_success == 0
            && result.st_unexpected_failure == 0
            && result.it_unexpected_success == 0
            && result.it_unexpected_failure == 0
    );
}
