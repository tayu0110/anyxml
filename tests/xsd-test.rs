use std::path::Path;

use anyxml::{
    error::{XMLError, XMLErrorDomain},
    sax::{
        DefaultSAXHandler, EntityResolver, ErrorHandler, InputSource, ParseError, SAXHandler,
        XMLReader, error::SAXParseError,
    },
    tree::{Element, TreeBuildHandler},
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

fn run_schema_test(base: &URIStr, element: Element, fail: &mut usize) -> Option<XSDSchema> {
    let name = element.get_attribute("name", None).unwrap();
    eprintln!("....... Start schema test: {name} .......");
    // Although a test suite schema may contain multiple `schemaDocument` elements,
    // the current API can only accept a single schema document, so only the first one is used.
    let schema_document = element
        .get_elements_by_expanded_name("schemaDocument", Some(XSTS_NAMESPACE))
        .next()
        .unwrap();
    let href = schema_document
        .get_attribute("href", Some(XLINK_NAMESPACE))
        .unwrap();
    let expected = element
        .get_elements_by_expanded_name("expected", Some(XSTS_NAMESPACE))
        .next()
        .unwrap();
    let validity = expected.get_attribute("validity", None).unwrap();
    if !matches!(validity.as_str(), "valid" | "invalid") {
        eprintln!(
            "Skip schema test '{}' because validity is neither 'valid' nor 'invalid' (actual={})",
            name, validity
        );
        return None;
    }
    let current = element
        .get_elements_by_expanded_name("current", Some(XSTS_NAMESPACE))
        .next()
        .unwrap();
    let status = current.get_attribute("status", None).unwrap();
    if !matches!(status.as_str(), "accepted" | "stable") {
        eprintln!(
            "Skip schema test '{}' because status is neither 'acepted' nor 'stable' (actual='{}')",
            name, status
        );
        return None;
    }

    let schema_path = base.resolve(&URIString::parse(&href).unwrap());
    let mut reader = XMLReader::builder()
        .set_handler(TreeBuildHandler::with_handler(NSErrorSuppressingHandler))
        .build();
    reader.parse_uri(&schema_path, None).unwrap();
    assert!(!reader.handler.fatal_error);
    let schema = reader.handler.document;
    match XSDSchema::from_schema_with_base_path(&schema, Some(Path::new(schema_path.path()))) {
        Ok(validator) => {
            if validity != "valid" {
                eprintln!("Unexpected success to parse schema {}", schema_path);
                *fail += 1;
            }
            Some(validator)
        }
        Err(err) => {
            if validity != "invalid" {
                eprintln!(
                    "Unexpected fail to parse schema {} because {}",
                    schema_path, err
                );
                *fail += 1;
            }
            None
        }
    }
}

fn run_instance_test(base: &URIStr, element: Element, validator: &XSDSchema, fail: &mut usize) {
    let name = element.get_attribute("name", None).unwrap();
    eprintln!("....... Start instance test: {name} .......");
    // Although a test suite schema may contain multiple `schemaDocument` elements,
    // the current API can only accept a single schema document, so only the first one is used.
    let instance_document = element
        .get_elements_by_expanded_name("instanceDocument", Some(XSTS_NAMESPACE))
        .next()
        .unwrap();
    let href = instance_document
        .get_attribute("href", Some(XLINK_NAMESPACE))
        .unwrap();
    let expected = element
        .get_elements_by_expanded_name("expected", Some(XSTS_NAMESPACE))
        .next()
        .unwrap();
    let validity = expected.get_attribute("validity", None).unwrap();
    let current = element
        .get_elements_by_expanded_name("current", Some(XSTS_NAMESPACE))
        .next()
        .unwrap();
    let status = current.get_attribute("status", None).unwrap();
    if !matches!(status.as_str(), "accepted" | "stable") {
        eprintln!(
            "Skip schema test '{}' because status is neither 'acepted' nor 'stable' (actual='{}')",
            name, status
        );
        return;
    }

    let instance_path = base.resolve(&URIString::parse(&href).unwrap());
    let mut reader = XMLReader::builder()
        .set_handler(TreeBuildHandler::with_handler(NSErrorSuppressingHandler))
        .build();
    reader.parse_uri(&instance_path, None).unwrap();
    assert!(!reader.handler.fatal_error);
    let instance = reader.handler.document;
    let errors = validator.validate(&instance);
    match validity.as_str() {
        "valid" => {
            if !errors.is_empty() {
                eprintln!("Unexpected fail to validate instance '{}'", instance_path);
                eprintln!("    errors: {:?}", errors);
                *fail += 1;
            }
        }
        "invalid" => {
            if errors.is_empty() {
                eprintln!(
                    "Unexpected success to validate instance '{}'",
                    instance_path
                );
                *fail += 1;
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

fn test_set(path: &URIStr) {
    let mut reader = XMLReader::builder()
        .set_handler(TreeBuildHandler::with_handler(NSErrorSuppressingHandler))
        .build();
    reader.parse_uri(path, None).unwrap();
    assert!(!reader.handler.fatal_error);

    let testset = reader.handler.document;
    let root = testset.document_element().unwrap();
    let testset_name = root.get_attribute("name", None).unwrap();
    let testset_contributor = root
        .get_attribute("contributer", None)
        .unwrap_or_else(|| "unknown".into());
    eprintln!("=== Start testset: {testset_name} ===");
    eprintln!("===    path       : {} ===", path);
    eprintln!("===    contributer: {} ===", testset_contributor);

    let mut fail = 0;
    for test_group in testset.get_elements_by_expanded_name("testGroup", Some(XSTS_NAMESPACE)) {
        let name = test_group.get_attribute("name", None).unwrap();
        eprintln!("----- Start test group: {name} -----");
        let Some(schema_test) = test_group
            .get_elements_by_expanded_name("schemaTest", Some(XSTS_NAMESPACE))
            .next()
        else {
            eprintln!("Skip because standalone (using xsi) validation is not supported.");
            continue;
        };

        let Some(validator) = run_schema_test(path, schema_test, &mut fail) else {
            continue;
        };
        let instance_test = test_group
            .get_elements_by_expanded_name("instanceTest", Some(XSTS_NAMESPACE))
            .collect::<Vec<_>>();
        for instance_test in instance_test {
            run_instance_test(path, instance_test, &validator, &mut fail);
        }
    }
    assert_eq!(fail, 0);
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
    for testset_ref in suite.get_elements_by_expanded_name("testSetRef", Some(XSTS_NAMESPACE)) {
        let href = testset_ref
            .get_attribute("href", Some(XLINK_NAMESPACE))
            .unwrap();
        let href = base.resolve(&URIString::parse(&href).unwrap());
        test_set(&href);
    }
}
