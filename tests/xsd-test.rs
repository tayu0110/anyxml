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

fn get_skip_reason(
    list: &'static [(&str, &str, &str, &'static str)],
    test_set: &str,
    test_group: &str,
    test_name: &str,
) -> Option<&'static str> {
    list.iter().find_map(|item| {
        ((item.0, item.1, item.2) == (test_set, test_group, test_name)).then_some(item.3)
    })
}

fn schema_test(
    node: Element,
    base_uri: &URIStr,
    test_set: &str,
    test_group: &str,
    out: &mut TestResult,
) -> Vec<URIString> {
    let name = node
        .get_attribute("name", None)
        .unwrap_or_else(|| "unknown".into());
    eprintln!("--- Start schema test '{name}' ---");
    if let Some(reason) = get_skip_reason(SKIP_SCHEMA_TESTS, test_set, test_group, name.as_str()) {
        eprintln!("test '{}' is skipped because {}", name, reason);
        out.st_skipped += 1;
        return vec![];
    }
    let validity = node
        .get_elements_by_expanded_name("expected", Some(XML_XSTS_NAMESPACE))
        .next()
        .and_then(|e| e.get_attribute("validity", None))
        .unwrap()
        .parse::<TestOutcome>()
        .unwrap();
    let mut ret = vec![];
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
                } else {
                    ret.push(href);
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
    ret
}

fn instance_test(
    node: Element,
    base_uri: &URIStr,
    test_set: &str,
    test_group: &str,
    schemas: &[URIString],
    out: &mut TestResult,
) {
    let name = node
        .get_attribute("name", None)
        .unwrap_or_else(|| "unknown".into());
    eprintln!("--- Start instance test '{name}' ---");
    if let Some(reason) = get_skip_reason(SKIP_INSTANCE_TESTS, test_set, test_group, name.as_str())
    {
        eprintln!("test '{}' is skipped because {}", name, reason);
        out.it_skipped += 1;
        return;
    }
    let validity = node
        .get_elements_by_expanded_name("expected", Some(XML_XSTS_NAMESPACE))
        .next()
        .and_then(|e| e.get_attribute("validity", None))
        .unwrap()
        .parse::<TestOutcome>()
        .unwrap();
    let mut handler = SchemaTestHandler::default();
    let mut schema = XSDSchema::default();
    for sc in schemas {
        if let Ok(sc) = XSDSchema::parse_uri(sc, None, Some(&mut handler)) {
            schema.extend(sc);
        }
    }
    for instance in node.get_elements_by_expanded_name("instanceDocument", Some(XML_XSTS_NAMESPACE))
    {
        let href = instance
            .get_attribute("href", Some(XML_XLINK_NAMESPACE))
            .unwrap();
        let href = URIString::parse(href).unwrap();
        let href = base_uri.resolve(&href);
        eprintln!("----- Start parsing '{href}' -----");

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
                    eprintln!("The instance test '{href}' is unexpected failure.");
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
    let test_set_name = testset
        .get_attribute("name", None)
        .unwrap_or_else(|| "unknown".into());
    let banner = format!("=== Start test set '{test_set_name}' ===");
    eprintln!("{banner}");
    eprintln!("\tname       : {test_set_name}");
    eprintln!("\tcontributor: {contributor}");
    eprintln!("\tURI        : {uri}");
    eprintln!("{}", "=".repeat(banner.len()));

    let mut test_result = TestResult::default();
    for group in testset.get_elements_by_expanded_name("testGroup", Some(XML_XSTS_NAMESPACE)) {
        let test_group_name = group
            .get_attribute("name", None)
            .unwrap_or_else(|| "unknown".into());

        eprintln!("### Start test group '{test_group_name}' ###");
        let mut schemas = vec![];
        for st in group.get_elements_by_expanded_name("schemaTest", Some(XML_XSTS_NAMESPACE)) {
            schemas.extend(schema_test(
                st,
                uri,
                &test_set_name,
                &test_group_name,
                &mut test_result,
            ));
        }
        for it in group.get_elements_by_expanded_name("instanceTest", Some(XML_XSTS_NAMESPACE)) {
            instance_test(
                it,
                uri,
                &test_set_name,
                &test_group_name,
                &schemas,
                &mut test_result,
            );
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
    eprintln!("=== End test set '{test_set_name}' ===");
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

/// Tuple of test set, test group schema test name and reason.
const SKIP_SCHEMA_TESTS: &[(&str, &str, &str, &str)] = &[
    (
        "MS-Regex2006-07-15",
        "HighSurrogates",
        "HighSurrogates",
        "'HighSurrogates' is unacceptable block in XSD 1.0 regex",
    ),
    (
        "MS-Element2006-07-15",
        "elemE001",
        "elemE001",
        "'{,5}' is invalid quantifier in XSD 1.0 regex.",
    ),
    (
        "MS-Element2006-07-15",
        "elemE002",
        "elemE002",
        "'{,5}' is invalid quantifier in XSD 1.0 regex.",
    ),
    (
        "MS-Element2006-07-15",
        "elemE003",
        "elemE003",
        "'{,5}' is invalid quantifier in XSD 1.0 regex.",
    ),
    (
        "MS-Element2006-07-15",
        "elemE004",
        "elemE004",
        "'{,5}' is invalid quantifier in XSD 1.0 regex.",
    ),
    (
        "MS-Element2006-07-15",
        "elemE005",
        "elemE005",
        "'{,5}' is invalid quantifier in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "reC65",
        "reC65",
        "'{,2}' is invalid quantifier in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "reC66",
        "reC66",
        "'{,2}' is invalid quantifier in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "reC67",
        "reC67",
        "'{,2}' is invalid quantifier in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "reC68",
        "reC68",
        "'{,2}' is invalid quantifier in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "reDG2",
        "reDG2",
        "'\\[{Nd}' is invalid regular expression in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "reF20",
        "reF20",
        "'[^a-d-b-c]' is invalid charRange in XSD 1.0 regex because '-' is a valid character range only at the beginning or end of a positive character group.",
    ),
    (
        "MS-Regex2006-07-15",
        "reF21",
        "reF21",
        "'[^a-d-b-c]' is invalid charRange in XSD 1.0 regex because '-' is a valid character range only at the beginning or end of a positive character group.",
    ),
    (
        "MS-Regex2006-07-15",
        "reF22",
        "reF22",
        "'[^a-d-b-c]' is invalid charRange in XSD 1.0 regex because '-' is a valid character range only at the beginning or end of a positive character group.",
    ),
    (
        "MS-Regex2006-07-15",
        "reF23",
        "reF23",
        "'[^a-d-b-c]' is invalid charRange in XSD 1.0 regex because '-' is a valid character range only at the beginning or end of a positive character group.",
    ),
    (
        "MS-Regex2006-07-15",
        "reF58",
        "reF58",
        "'[^[a-b]]' is invalid in XSD 1.0 regex because '[a-b]' is not match to charGroup.",
    ),
    (
        "MS-Regex2006-07-15",
        "reG26",
        "reG26",
        "'[a-c-1-4x-z-7-9]*' is invalid charRange in XSD 1.0 regex because '-' is a valid character range only at the beginning or end of a positive character group.",
    ),
    (
        "MS-Regex2006-07-15",
        "reG27",
        "reG27",
        "'[a-c-1-4x-z-7-9]*' is invalid charRange in XSD 1.0 regex because '-' is a valid character range only at the beginning or end of a positive character group.",
    ),
    (
        "MS-Regex2006-07-15",
        "reG28",
        "reG28",
        "'[a-c-1-4x-z-7-9]*' is invalid charRange in XSD 1.0 regex because '-' is a valid character range only at the beginning or end of a positive character group.",
    ),
    (
        "MS-Regex2006-07-15",
        "reG29",
        "reG29",
        "'[a-c-1-4x-z-7-9]*' is invalid charRange in XSD 1.0 regex because '-' is a valid character range only at the beginning or end of a positive character group.",
    ),
    (
        "MS-Regex2006-07-15",
        "reG30",
        "reG30",
        "'[a-c-1-4x-z-7-9]*' is invalid charRange in XSD 1.0 regex because '-' is a valid character range only at the beginning or end of a positive character group.",
    ),
    (
        "MS-Regex2006-07-15",
        "reG31",
        "reG31",
        "'[a-c-1-4x-z-7-9]*' is invalid charRange in XSD 1.0 regex because '-' is a valid character range only at the beginning or end of a positive character group.",
    ),
    (
        "MS-Regex2006-07-15",
        "reG32",
        "reG32",
        "'[a-c-1-4x-z-7-9]*' is invalid charRange in XSD 1.0 regex because '-' is a valid character range only at the beginning or end of a positive character group.",
    ),
    (
        "MS-Regex2006-07-15",
        "reG33",
        "reG33",
        "'[a-c-1-4x-z-7-9]*' is invalid charRange in XSD 1.0 regex because '-' is a valid character range only at the beginning or end of a positive character group.",
    ),
    (
        "MS-Regex2006-07-15",
        "reH19",
        "reH19",
        "'[a-a-x-x]' is invalid charRange in XSD 1.0 regex because '-' is a valid character range only at the beginning or end of a positive character group.",
    ),
    (
        "MS-Regex2006-07-15",
        "reH20",
        "reH20",
        "'[a-a-x-x]' is invalid charRange in XSD 1.0 regex because '-' is a valid character range only at the beginning or end of a positive character group.",
    ),
    (
        "MS-Regex2006-07-15",
        "reH21",
        "reH21",
        "'[a-a-x-x]' is invalid charRange in XSD 1.0 regex because '-' is a valid character range only at the beginning or end of a positive character group.",
    ),
    (
        "MS-Regex2006-07-15",
        "reM77",
        "reM77",
        "'LowSurrogates' is unacceptable block in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "reN75",
        "reN75",
        "'HighSurrogates' is unacceptable block in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_9",
        "RegexTest_9",
        "'\\u' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_11",
        "RegexTest_11",
        "'\\u' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_14",
        "RegexTest_14",
        "'\\u' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_15",
        "RegexTest_15",
        "'\\u' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_16",
        "RegexTest_16",
        "'\\u' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_17",
        "RegexTest_17",
        "'\\u' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_23",
        "RegexTest_23",
        "'\\p{klsak' is invalid character property in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_27",
        "RegexTest_27",
        "'(?r:foo)' is unknown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_28",
        "RegexTest_28",
        "'(?c:foo)' is unknown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_30",
        "RegexTest_30",
        "'(?n:(foo)(\\s+)(bar))' is unknown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_33",
        "RegexTest_33",
        "'(?e:foo)' is unknown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_34",
        "RegexTest_34",
        "'(?+i:foo)' is unknown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_43",
        "RegexTest_43",
        "'a-\\d' is invalid charRange in XSD 1.0 regex because MultiCharEsc is not allowed.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_44",
        "RegexTest_44",
        "'5-\\D' is invalid charRange in XSD 1.0 regex because MultiCharEsc is not allowed.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_45",
        "RegexTest_45",
        "'6-\\s' is invalid charRange in XSD 1.0 regex because MultiCharEsc is not allowed.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_46",
        "RegexTest_46",
        "'c-\\S' is invalid charRange in XSD 1.0 regex because MultiCharEsc is not allowed.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_47",
        "RegexTest_47",
        "'7-\\w' is invalid charRange in XSD 1.0 regex because MultiCharEsc is not allowed.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_48",
        "RegexTest_48",
        "'a-\\W' is invalid charRange in XSD 1.0 regex because MultiCharEsc is not allowed.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_49",
        "RegexTest_49",
        "'f-\\p{Lu}' is invalid charRange in XSD 1.0 regex because catEsc is not allowed.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_50",
        "RegexTest_50",
        "'1-\\P{Ll}' is invalid charRange in XSD 1.0 regex because catEsc is not allowed.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_51",
        "RegexTest_51",
        "'\\p' without charProp is invalid escape seqence in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_52",
        "RegexTest_52",
        "'\\P' without charProp is invalid escape seqence in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_53",
        "RegexTest_53",
        "'\\p' without charProp is invalid escape seqence in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_54",
        "RegexTest_54",
        "'\\P' without charProp is invalid escape seqence in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_55",
        "RegexTest_55",
        "'\\p' without charProp is invalid escape seqence in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_56",
        "RegexTest_56",
        "'\\p' without charProp is invalid escape seqence in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_57",
        "RegexTest_57",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_58",
        "RegexTest_58",
        "'\\u' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_65",
        "RegexTest_65",
        "'\\o' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_113",
        "RegexTest_113",
        "'foo(?(?#COMMENT)foo)' is unkown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_116",
        "RegexTest_116",
        "'foo(?(?afdfoo)bar)' is unkown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_119",
        "RegexTest_119",
        "'(foo) (?#foo) \\s+ (?#followed by...' is unkown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_120",
        "RegexTest_120",
        "'(foo) (?#foo) \\s+ (?#followed by...' is unkown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_121",
        "RegexTest_121",
        "'(foo) (?#foo) \\s+ (?#followed by...' is unkown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_141",
        "RegexTest_141",
        "'\\077' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_142",
        "RegexTest_142",
        "'\\77' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_143",
        "RegexTest_143",
        "'\\176' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_145",
        "RegexTest_145",
        "'\\300' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_146",
        "RegexTest_146",
        "'\\300' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_147",
        "RegexTest_147",
        "'\\477' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_148",
        "RegexTest_148",
        "'\\777' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_149",
        "RegexTest_149",
        "'\\7770' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_150",
        "RegexTest_150",
        "'\\7' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_151",
        "RegexTest_151",
        "'\\077' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_152",
        "RegexTest_152",
        "'\\77' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_154",
        "RegexTest_154",
        "'\\40' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_155",
        "RegexTest_155",
        "'\\040' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_156",
        "RegexTest_156",
        "'\\176' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_157",
        "RegexTest_157",
        "'\\377' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_158",
        "RegexTest_158",
        "'\\400' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_163",
        "RegexTest_163",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_164",
        "RegexTest_164",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_165",
        "RegexTest_165",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_166",
        "RegexTest_166",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_167",
        "RegexTest_167",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_168",
        "RegexTest_168",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_169",
        "RegexTest_169",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_170",
        "RegexTest_170",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_171",
        "RegexTest_171",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_172",
        "RegexTest_172",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_173",
        "RegexTest_173",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_174",
        "RegexTest_174",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_178",
        "RegexTest_178",
        "'\\c?*' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_194",
        "RegexTest_194",
        "'\\A' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_195",
        "RegexTest_195",
        "'\\A' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_196",
        "RegexTest_196",
        "'\\A' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_197",
        "RegexTest_197",
        "'\\A' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_198",
        "RegexTest_198",
        "'\\A' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_199",
        "RegexTest_199",
        "'\\A' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_200",
        "RegexTest_200",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_201",
        "RegexTest_201",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_202",
        "RegexTest_202",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_203",
        "RegexTest_203",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_204",
        "RegexTest_204",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_205",
        "RegexTest_205",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_206",
        "RegexTest_206",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_207",
        "RegexTest_207",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_208",
        "RegexTest_208",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_209",
        "RegexTest_209",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_210",
        "RegexTest_210",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_211",
        "RegexTest_211",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_212",
        "RegexTest_212",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_213",
        "RegexTest_213",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_214",
        "RegexTest_214",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_215",
        "RegexTest_215",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_216",
        "RegexTest_216",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_217",
        "RegexTest_217",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_218",
        "RegexTest_218",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_220",
        "RegexTest_220",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_221",
        "RegexTest_221",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_222",
        "RegexTest_222",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_226",
        "RegexTest_226",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_230",
        "RegexTest_230",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_232",
        "RegexTest_232",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_233",
        "RegexTest_233",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_294",
        "RegexTest_294",
        "'[0-9]+?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_295",
        "RegexTest_295",
        "'[0-9]+?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_299",
        "RegexTest_299",
        "'[abcd]{0,16}*' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_300",
        "RegexTest_300",
        "'[abcd]{1,}*' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_301",
        "RegexTest_301",
        "'[abcd]{1}*' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_302",
        "RegexTest_302",
        "'[abcd]{0,16}?*' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_303",
        "RegexTest_303",
        "'[abcd]{1,}?*' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_304",
        "RegexTest_304",
        "'[abcd]{1}?*' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_305",
        "RegexTest_305",
        "'[abcd]*+' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_306",
        "RegexTest_306",
        "'[abcd]+*' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_307",
        "RegexTest_307",
        "'[abcd]?*' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_308",
        "RegexTest_308",
        "'[abcd]*?+' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_309",
        "RegexTest_309",
        "'[abcd]+?*' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_310",
        "RegexTest_310",
        "'[abcd]??*' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_314",
        "RegexTest_314",
        "'*?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_315",
        "RegexTest_315",
        "'*?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_316",
        "RegexTest_316",
        "'*?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_317",
        "RegexTest_317",
        "'*?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_440",
        "RegexTest_440",
        "'\\u' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_442",
        "RegexTest_442",
        "'[' cannot appear standalone as Char in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_443",
        "RegexTest_443",
        "'[' cannot appear standalone as Char in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_448",
        "RegexTest_448",
        "']' cannot appear standalone as Char in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_449",
        "RegexTest_449",
        "']' cannot appear standalone as Char in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_450",
        "RegexTest_450",
        "']' cannot appear standalone as Char in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_451",
        "RegexTest_451",
        "']' cannot appear standalone as Char in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_458",
        "RegexTest_458",
        "']' cannot appear standalone as Char in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_464",
        "RegexTest_464",
        "'[[abcd]-[bc]]' is invalid in XSD 1.0 regex because charClassExpr cannot contain another charClassExpr.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_465",
        "RegexTest_465",
        "'[' cannot appear standalone as Char in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_471",
        "RegexTest_471",
        "'[' cannot appear standalone as Char in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_472",
        "RegexTest_472",
        "'[' cannot appear standalone as Char in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_473",
        "RegexTest_473",
        "'[' cannot appear standalone as Char in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_477",
        "RegexTest_477",
        "'[' cannot appear standalone as Char in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_478",
        "RegexTest_478",
        "'[' cannot appear standalone as Char in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_481",
        "RegexTest_481",
        "Named capturing group is not supported in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_482",
        "RegexTest_482",
        "'(?:b*)' is unknown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_483",
        "RegexTest_483",
        "Positive Lookahead is not supported in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_484",
        "RegexTest_484",
        "Negative Lookahead is not supported in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_487",
        "RegexTest_487",
        "Atomic grouping is not supported in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_516",
        "RegexTest_516",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_517",
        "RegexTest_517",
        "'\\060' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_518",
        "RegexTest_518",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_519",
        "RegexTest_519",
        "'\\u' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_521",
        "RegexTest_521",
        "'(d+?)(e*?)(f??)' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_523",
        "RegexTest_523",
        "Conditional subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_524",
        "RegexTest_524",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_586",
        "RegexTest_586",
        "']' cannot appear standalone as Char in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_587",
        "RegexTest_587",
        "']' cannot appear standalone as Char in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_592",
        "RegexTest_592",
        "']' cannot appear standalone as Char in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_593",
        "RegexTest_593",
        "']' cannot appear standalone as Char in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_594",
        "RegexTest_594",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_595",
        "RegexTest_595",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_596",
        "RegexTest_596",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_597",
        "RegexTest_597",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_598",
        "RegexTest_598",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_599",
        "RegexTest_599",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_600",
        "RegexTest_600",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_601",
        "RegexTest_601",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_602",
        "RegexTest_602",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_603",
        "RegexTest_603",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_604",
        "RegexTest_604",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_605",
        "RegexTest_605",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_648",
        "RegexTest_648",
        "'.+?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_655",
        "RegexTest_655",
        "')(' is invalid in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_688",
        "RegexTest_688",
        "'\\10' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_696",
        "RegexTest_696",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_697",
        "RegexTest_697",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_698",
        "RegexTest_698",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_699",
        "RegexTest_699",
        "'\\2' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_700",
        "RegexTest_700",
        "'\\2' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_701",
        "RegexTest_701",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_702",
        "RegexTest_702",
        "'\\2' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_703",
        "RegexTest_703",
        "'\\2' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_704",
        "RegexTest_704",
        "'\\2' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_705",
        "RegexTest_705",
        "'\\2' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_706",
        "RegexTest_706",
        "'\\2' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_707",
        "RegexTest_707",
        "'\\2' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_717",
        "RegexTest_717",
        "'b*?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_718",
        "RegexTest_718",
        "'{0,}?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_719",
        "RegexTest_719",
        "'b+?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_724",
        "RegexTest_724",
        "'{1,}?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_725",
        "RegexTest_725",
        "'{1,3}?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_726",
        "RegexTest_726",
        "'{3,4}?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_727",
        "RegexTest_727",
        "'{4,5}?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_728",
        "RegexTest_728",
        "'b??' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_729",
        "RegexTest_729",
        "'b??' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_730",
        "RegexTest_730",
        "'{0,1}?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_731",
        "RegexTest_731",
        "'b??' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_732",
        "RegexTest_732",
        "'b??' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_733",
        "RegexTest_733",
        "'{0,1}?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_743",
        "RegexTest_743",
        "'.*?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_755",
        "RegexTest_755",
        "']' cannot appear standalone as Char in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_756",
        "RegexTest_756",
        "']' cannot appear standalone as Char in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_761",
        "RegexTest_761",
        "']' cannot appear standalone as Char in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_762",
        "RegexTest_762",
        "']' cannot appear standalone as Char in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_781",
        "RegexTest_781",
        "'.+?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_782",
        "RegexTest_782",
        "'.*?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_783",
        "RegexTest_783",
        "'{0,5}?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_790",
        "RegexTest_790",
        "'{0,1}?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_791",
        "RegexTest_791",
        "')(' is invalid in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_824",
        "RegexTest_824",
        "'\\10' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_826",
        "RegexTest_826",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_827",
        "RegexTest_827",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_836",
        "RegexTest_836",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_837",
        "RegexTest_837",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_841",
        "RegexTest_841",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_842",
        "RegexTest_842",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_843",
        "RegexTest_843",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_844",
        "RegexTest_844",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_845",
        "RegexTest_845",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_846",
        "RegexTest_846",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_847",
        "RegexTest_847",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_848",
        "RegexTest_848",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_851",
        "RegexTest_851",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_852",
        "RegexTest_852",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_853",
        "RegexTest_853",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_854",
        "RegexTest_854",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_855",
        "RegexTest_855",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_856",
        "RegexTest_856",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_857",
        "RegexTest_857",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_861",
        "RegexTest_861",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_862",
        "RegexTest_862",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_863",
        "RegexTest_863",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_864",
        "RegexTest_864",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_865",
        "RegexTest_865",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_866",
        "RegexTest_866",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_870",
        "RegexTest_870",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_879",
        "RegexTest_879",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_880",
        "RegexTest_880",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_888",
        "RegexTest_888",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_889",
        "RegexTest_889",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_890",
        "RegexTest_890",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_891",
        "RegexTest_891",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_892",
        "RegexTest_892",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_893",
        "RegexTest_893",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_894",
        "RegexTest_894",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_895",
        "RegexTest_895",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_896",
        "RegexTest_896",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_897",
        "RegexTest_897",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_898",
        "RegexTest_898",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_899",
        "RegexTest_899",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_900",
        "RegexTest_900",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_901",
        "RegexTest_901",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_902",
        "RegexTest_902",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_903",
        "RegexTest_903",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_904",
        "RegexTest_904",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_905",
        "RegexTest_905",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_906",
        "RegexTest_906",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_907",
        "RegexTest_907",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_908",
        "RegexTest_908",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_909",
        "RegexTest_909",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_910",
        "RegexTest_910",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_911",
        "RegexTest_911",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_912",
        "RegexTest_912",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_913",
        "RegexTest_913",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_914",
        "RegexTest_914",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_915",
        "RegexTest_915",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_916",
        "RegexTest_916",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_917",
        "RegexTest_917",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_918",
        "RegexTest_918",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_919",
        "RegexTest_919",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_920",
        "RegexTest_920",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_921",
        "RegexTest_921",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_922",
        "RegexTest_922",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_923",
        "RegexTest_923",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_924",
        "RegexTest_924",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_925",
        "RegexTest_925",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_926",
        "RegexTest_926",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_928",
        "RegexTest_928",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_929",
        "RegexTest_929",
        "Comment syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_930",
        "RegexTest_930",
        "Comment syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_936",
        "RegexTest_936",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_937",
        "RegexTest_937",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_938",
        "RegexTest_938",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_939",
        "RegexTest_939",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_940",
        "RegexTest_940",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_941",
        "RegexTest_941",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_942",
        "RegexTest_942",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_943",
        "RegexTest_943",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_944",
        "RegexTest_944",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_945",
        "RegexTest_945",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_946",
        "RegexTest_946",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_949",
        "RegexTest_949",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_950",
        "RegexTest_950",
        "Conditional subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_951",
        "RegexTest_951",
        "Conditional subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_952",
        "RegexTest_952",
        "Conditional subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_953",
        "RegexTest_953",
        "Conditional subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_954",
        "RegexTest_954",
        "Conditional subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_955",
        "RegexTest_955",
        "Conditional subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_956",
        "RegexTest_956",
        "Conditional subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_957",
        "RegexTest_957",
        "Conditional subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_958",
        "RegexTest_958",
        "Conditional subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_959",
        "RegexTest_959",
        "Conditional subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_960",
        "RegexTest_960",
        "Conditional subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_961",
        "RegexTest_961",
        "Conditional subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_962",
        "RegexTest_962",
        "Conditional subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_963",
        "RegexTest_963",
        "Conditional subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_964",
        "RegexTest_964",
        "Conditional subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_976",
        "RegexTest_976",
        "'\\2' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_977",
        "RegexTest_977",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_988",
        "RegexTest_988",
        "Once-only subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_989",
        "RegexTest_989",
        "'[' cannot appear standalone as Char in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_990",
        "RegexTest_990",
        "'[' cannot appear standalone as Char in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_991",
        "RegexTest_991",
        "'[' cannot appear standalone as Char in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_994",
        "RegexTest_994",
        "'[a[:xyz:]b]' is invalid in XSD 1.0 regex because charClassExpr cannot contain another charClassExpr.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_995",
        "RegexTest_995",
        "Once-only subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_996",
        "RegexTest_996",
        "Once-only subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_997",
        "RegexTest_997",
        "Once-only subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1000",
        "RegexTest_1000",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1001",
        "RegexTest_1001",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1002",
        "RegexTest_1002",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1003",
        "RegexTest_1003",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1004",
        "RegexTest_1004",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1007",
        "RegexTest_1007",
        "Once-only subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1008",
        "RegexTest_1008",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1009",
        "RegexTest_1009",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1010",
        "RegexTest_1010",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1011",
        "RegexTest_1011",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1012",
        "RegexTest_1012",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1013",
        "RegexTest_1013",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1014",
        "RegexTest_1014",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1015",
        "RegexTest_1015",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1016",
        "RegexTest_1016",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1017",
        "RegexTest_1017",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1018",
        "RegexTest_1018",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1019",
        "RegexTest_1019",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1070",
        "RegexTest_1070",
        "']' cannot appear standalone as Char in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1071",
        "RegexTest_1071",
        "']' cannot appear standalone as Char in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1076",
        "RegexTest_1076",
        "']' cannot appear standalone as Char in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1077",
        "RegexTest_1077",
        "']' cannot appear standalone as Char in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1078",
        "RegexTest_1078",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1079",
        "RegexTest_1079",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1080",
        "RegexTest_1080",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1081",
        "RegexTest_1081",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1082",
        "RegexTest_1082",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1083",
        "RegexTest_1083",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1084",
        "RegexTest_1084",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1085",
        "RegexTest_1085",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1086",
        "RegexTest_1086",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1087",
        "RegexTest_1087",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1088",
        "RegexTest_1088",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1089",
        "RegexTest_1089",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1132",
        "RegexTest_1132",
        "'.+?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1139",
        "RegexTest_1139",
        "')(' is invalid in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1172",
        "RegexTest_1172",
        "'\\10' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1180",
        "RegexTest_1180",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1181",
        "RegexTest_1181",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1182",
        "RegexTest_1182",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1184",
        "RegexTest_1184",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1185",
        "RegexTest_1185",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1186",
        "RegexTest_1186",
        "'\\2' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1187",
        "RegexTest_1187",
        "'\\2' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1188",
        "RegexTest_1188",
        "'\\2' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1189",
        "RegexTest_1189",
        "'\\2' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1190",
        "RegexTest_1190",
        "'\\2' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1191",
        "RegexTest_1191",
        "'\\2' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1201",
        "RegexTest_1201",
        "'b*?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1202",
        "RegexTest_1202",
        "'{0,}?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1203",
        "RegexTest_1203",
        "'b+?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1208",
        "RegexTest_1208",
        "'{1,}?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1209",
        "RegexTest_1209",
        "'{1,3}?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1210",
        "RegexTest_1210",
        "'{3,4}?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1211",
        "RegexTest_1211",
        "'{4,5}?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1212",
        "RegexTest_1212",
        "'b??' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1213",
        "RegexTest_1213",
        "'b??' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1214",
        "RegexTest_1214",
        "'{0,1}?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1215",
        "RegexTest_1215",
        "'b??' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1216",
        "RegexTest_1216",
        "'b??' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1217",
        "RegexTest_1217",
        "'{0,1}?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1227",
        "RegexTest_1227",
        "'.*?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1239",
        "RegexTest_1239",
        "']' cannot appear standalone as Char in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1240",
        "RegexTest_1240",
        "']' cannot appear standalone as Char in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1245",
        "RegexTest_1245",
        "']' cannot appear standalone as Char in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1246",
        "RegexTest_1246",
        "']' cannot appear standalone as Char in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1264",
        "RegexTest_1264",
        "'a**' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1265",
        "RegexTest_1265",
        "'.+?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1266",
        "RegexTest_1266",
        "'.*?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1267",
        "RegexTest_1267",
        "'{0,5}?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1274",
        "RegexTest_1274",
        "'{0,1}?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1275",
        "RegexTest_1275",
        "')(' is invalid in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1308",
        "RegexTest_1308",
        "'\\10' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1310",
        "RegexTest_1310",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1311",
        "RegexTest_1311",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1320",
        "RegexTest_1320",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1321",
        "RegexTest_1321",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1322",
        "RegexTest_1322",
        "Negative lookahead assertion is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1323",
        "RegexTest_1323",
        "Positive lookahead assertion is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1324",
        "RegexTest_1324",
        "Positive lookahead assertion is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1325",
        "RegexTest_1325",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1326",
        "RegexTest_1326",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1327",
        "RegexTest_1327",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1328",
        "RegexTest_1328",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1329",
        "RegexTest_1329",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1330",
        "RegexTest_1330",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1331",
        "RegexTest_1331",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1332",
        "RegexTest_1332",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1335",
        "RegexTest_1335",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1336",
        "RegexTest_1336",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1337",
        "RegexTest_1337",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1338",
        "RegexTest_1338",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1339",
        "RegexTest_1339",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1340",
        "RegexTest_1340",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1341",
        "RegexTest_1341",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1345",
        "RegexTest_1345",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1346",
        "RegexTest_1346",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1347",
        "RegexTest_1347",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1348",
        "RegexTest_1348",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1349",
        "RegexTest_1349",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1350",
        "RegexTest_1350",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1354",
        "RegexTest_1354",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1363",
        "RegexTest_1363",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1364",
        "RegexTest_1364",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1365",
        "RegexTest_1365",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1372",
        "RegexTest_1372",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1373",
        "RegexTest_1373",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1374",
        "RegexTest_1374",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1375",
        "RegexTest_1375",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1376",
        "RegexTest_1376",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1377",
        "RegexTest_1377",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1378",
        "RegexTest_1378",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1379",
        "RegexTest_1379",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1380",
        "RegexTest_1380",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1381",
        "RegexTest_1381",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1382",
        "RegexTest_1382",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1383",
        "RegexTest_1383",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1384",
        "RegexTest_1384",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1385",
        "RegexTest_1385",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1386",
        "RegexTest_1386",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1387",
        "RegexTest_1387",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1388",
        "RegexTest_1388",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1389",
        "RegexTest_1389",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1390",
        "RegexTest_1390",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1391",
        "RegexTest_1391",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1392",
        "RegexTest_1392",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1393",
        "RegexTest_1393",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1394",
        "RegexTest_1394",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1395",
        "RegexTest_1395",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1396",
        "RegexTest_1396",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1397",
        "RegexTest_1397",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1398",
        "RegexTest_1398",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1399",
        "RegexTest_1399",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1400",
        "RegexTest_1400",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1401",
        "RegexTest_1401",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1402",
        "RegexTest_1402",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1403",
        "RegexTest_1403",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1404",
        "RegexTest_1404",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1405",
        "RegexTest_1405",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1406",
        "RegexTest_1406",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1407",
        "RegexTest_1407",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1408",
        "RegexTest_1408",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1409",
        "RegexTest_1409",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1410",
        "RegexTest_1410",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1412",
        "RegexTest_1412",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1413",
        "RegexTest_1413",
        "Comment syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1414",
        "RegexTest_1414",
        "Comment syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1420",
        "RegexTest_1420",
        "Non-capturing group syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1421",
        "RegexTest_1421",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1422",
        "RegexTest_1422",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1423",
        "RegexTest_1423",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1424",
        "RegexTest_1424",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1425",
        "RegexTest_1425",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1426",
        "RegexTest_1426",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1427",
        "RegexTest_1427",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1428",
        "RegexTest_1428",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1429",
        "RegexTest_1429",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1430",
        "RegexTest_1430",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1433",
        "RegexTest_1433",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1434",
        "RegexTest_1434",
        "Conditional subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1435",
        "RegexTest_1435",
        "Conditional subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1436",
        "RegexTest_1436",
        "Conditional subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1437",
        "RegexTest_1437",
        "Conditional subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1438",
        "RegexTest_1438",
        "Conditional subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1439",
        "RegexTest_1439",
        "Conditional subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1440",
        "RegexTest_1440",
        "Conditional subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1441",
        "RegexTest_1441",
        "Conditional subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1442",
        "RegexTest_1442",
        "Conditional subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1443",
        "RegexTest_1443",
        "Conditional subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1444",
        "RegexTest_1444",
        "Conditional subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1445",
        "RegexTest_1445",
        "Conditional subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1446",
        "RegexTest_1446",
        "Conditional subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1447",
        "RegexTest_1447",
        "Conditional subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1448",
        "RegexTest_1448",
        "Conditional subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1449",
        "RegexTest_1449",
        "Conditional subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1450",
        "RegexTest_1450",
        "Conditional subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1451",
        "RegexTest_1451",
        "Negative lookahead assertion is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1452",
        "RegexTest_1452",
        "Negative lookahead assertion is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1453",
        "RegexTest_1453",
        "Positive lookahead assertion is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1454",
        "RegexTest_1454",
        "Positive lookahead assertion is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1455",
        "RegexTest_1455",
        "Positive lookahead assertion is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1456",
        "RegexTest_1456",
        "Positive lookahead assertion is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1472",
        "RegexTest_1472",
        "Once-only subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1473",
        "RegexTest_1473",
        "'[[:]' is invalid in XSD 1.0 regex because charClassExpr cannot contain '['.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1474",
        "RegexTest_1474",
        "'[[=]' is invalid in XSD 1.0 regex because charClassExpr cannot contain '['.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1475",
        "RegexTest_1475",
        "'[[.]' is invalid in XSD 1.0 regex because charClassExpr cannot contain '['.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1476",
        "RegexTest_1476",
        "'[a[:xyz:' is invalid in XSD 1.0 regex because charClassExpr is not closed by ']'.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1477",
        "RegexTest_1477",
        "'[a[:xyz:]' is invalid in XSD 1.0 regex because charClassExpr is not closed by ']'.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1478",
        "RegexTest_1478",
        "'[a[:xyz:]b]' is invalid in XSD 1.0 regex because charClassExpr cannot contain another charClassExpr.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1479",
        "RegexTest_1479",
        "Once-only subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1480",
        "RegexTest_1480",
        "Once-only subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1481",
        "RegexTest_1481",
        "Once-only subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1484",
        "RegexTest_1484",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1485",
        "RegexTest_1485",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1486",
        "RegexTest_1486",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1487",
        "RegexTest_1487",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1488",
        "RegexTest_1488",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1491",
        "RegexTest_1491",
        "Once-only subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1492",
        "RegexTest_1492",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1493",
        "RegexTest_1493",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1494",
        "RegexTest_1494",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1495",
        "RegexTest_1495",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1496",
        "RegexTest_1496",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1497",
        "RegexTest_1497",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1498",
        "RegexTest_1498",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1499",
        "RegexTest_1499",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1500",
        "RegexTest_1500",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1501",
        "RegexTest_1501",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1502",
        "RegexTest_1502",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1503",
        "RegexTest_1503",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1543",
        "RegexTest_1543",
        "Once-only subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_1544",
        "RegexTest_1544",
        "Positive lookahead assertion is unacceptable in XSD 1.0 regex",
    ),
];
/// Tuple of test set, test group instance test name and reason.
const SKIP_INSTANCE_TESTS: &[(&str, &str, &str, &str)] = &[
    (
        "MS-Regex2006-07-15",
        "reDG2",
        "reDG2.i",
        "'\\[{Nd}' is invalid regular expression in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "reF20",
        "reF20.i",
        "'[^a-d-b-c]' is invalid charRange in XSD 1.0 regex because '-' is a valid character range only at the beginning or end of a positive character group.",
    ),
    (
        "MS-Regex2006-07-15",
        "reF21",
        "reF21.i",
        "'[^a-d-b-c]' is invalid charRange in XSD 1.0 regex because '-' is a valid character range only at the beginning or end of a positive character group.",
    ),
    (
        "MS-Regex2006-07-15",
        "reF22",
        "reF22.i",
        "'[^a-d-b-c]' is invalid charRange in XSD 1.0 regex because '-' is a valid character range only at the beginning or end of a positive character group.",
    ),
    (
        "MS-Regex2006-07-15",
        "reF23",
        "reF23.i",
        "'[^a-d-b-c]' is invalid charRange in XSD 1.0 regex because '-' is a valid character range only at the beginning or end of a positive character group.",
    ),
    (
        "MS-Regex2006-07-15",
        "reG26",
        "reG26.v",
        "'[a-c-1-4x-z-7-9]*' is invalid charRange in XSD 1.0 regex because '-' is a valid character range only at the beginning or end of a positive character group.",
    ),
    (
        "MS-Regex2006-07-15",
        "reG27",
        "reG27.v",
        "'[a-c-1-4x-z-7-9]*' is invalid charRange in XSD 1.0 regex because '-' is a valid character range only at the beginning or end of a positive character group.",
    ),
    (
        "MS-Regex2006-07-15",
        "reG28",
        "reG28.v",
        "'[a-c-1-4x-z-7-9]*' is invalid charRange in XSD 1.0 regex because '-' is a valid character range only at the beginning or end of a positive character group.",
    ),
    (
        "MS-Regex2006-07-15",
        "reG29",
        "reG29.v",
        "'[a-c-1-4x-z-7-9]*' is invalid charRange in XSD 1.0 regex because '-' is a valid character range only at the beginning or end of a positive character group.",
    ),
    (
        "MS-Regex2006-07-15",
        "reG30",
        "reG30.v",
        "'[a-c-1-4x-z-7-9]*' is invalid charRange in XSD 1.0 regex because '-' is a valid character range only at the beginning or end of a positive character group.",
    ),
    (
        "MS-Regex2006-07-15",
        "reG31",
        "reG31.v",
        "'[a-c-1-4x-z-7-9]*' is invalid charRange in XSD 1.0 regex because '-' is a valid character range only at the beginning or end of a positive character group.",
    ),
    (
        "MS-Regex2006-07-15",
        "reG32",
        "reG32.v",
        "'[a-c-1-4x-z-7-9]*' is invalid charRange in XSD 1.0 regex because '-' is a valid character range only at the beginning or end of a positive character group.",
    ),
    (
        "MS-Regex2006-07-15",
        "reG33",
        "reG33.v",
        "'[a-c-1-4x-z-7-9]*' is invalid charRange in XSD 1.0 regex because '-' is a valid character range only at the beginning or end of a positive character group.",
    ),
    (
        "MS-Regex2006-07-15",
        "reH19",
        "reH19.v",
        "'[a-a-x-x]' is invalid charRange in XSD 1.0 regex because '-' is a valid character range only at the beginning or end of a positive character group.",
    ),
    (
        "MS-Regex2006-07-15",
        "reH20",
        "reH20.i",
        "'[a-a-x-x]' is invalid charRange in XSD 1.0 regex because '-' is a valid character range only at the beginning or end of a positive character group.",
    ),
    (
        "MS-Regex2006-07-15",
        "reH21",
        "reH21.i",
        "'[a-a-x-x]' is invalid charRange in XSD 1.0 regex because '-' is a valid character range only at the beginning or end of a positive character group.",
    ),
    (
        "MS-Regex2006-07-15",
        "reM77",
        "reM77.i",
        "'LowSurrogates' is unacceptable block in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "reN75",
        "reN75.i",
        "'HighSurrogates' is unacceptable block in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_9",
        "RegexTest_9.i",
        "'\\u' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_11",
        "RegexTest_11.i",
        "'\\u' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_15",
        "RegexTest_15.i",
        "'\\u' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_16",
        "RegexTest_16.i",
        "'\\u' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_17",
        "RegexTest_17.i",
        "'\\u' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_23",
        "RegexTest_23.i",
        "'\\p{klsak' is invalid character property in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_27",
        "RegexTest_27.i",
        "'(?r:foo)' is unknown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_28",
        "RegexTest_28.i",
        "'(?c:foo)' is unknown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_30",
        "RegexTest_30.v",
        "'(?n:(foo)(\\s+)(bar))' is unknown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_33",
        "RegexTest_33.i",
        "'(?e:foo)' is unknown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_34",
        "RegexTest_34.v",
        "'(?+i:foo)' is unknown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_43",
        "RegexTest_43.i",
        "'a-\\d' is invalid charRange in XSD 1.0 regex because MultiCharEsc is not allowed.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_44",
        "RegexTest_44.i",
        "'5-\\D' is invalid charRange in XSD 1.0 regex because MultiCharEsc is not allowed.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_45",
        "RegexTest_45.i",
        "'6-\\s' is invalid charRange in XSD 1.0 regex because MultiCharEsc is not allowed.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_46",
        "RegexTest_46.i",
        "'c-\\S' is invalid charRange in XSD 1.0 regex because MultiCharEsc is not allowed.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_47",
        "RegexTest_47.i",
        "'7-\\w' is invalid charRange in XSD 1.0 regex because MultiCharEsc is not allowed.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_48",
        "RegexTest_48.i",
        "'a-\\W' is invalid charRange in XSD 1.0 regex because MultiCharEsc is not allowed.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_49",
        "RegexTest_49.i",
        "'f-\\p{Lu}' is invalid charRange in XSD 1.0 regex because catEsc is not allowed.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_50",
        "RegexTest_50.i",
        "'1-\\P{Ll}' is invalid charRange in XSD 1.0 regex because catEsc is not allowed.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_51",
        "RegexTest_51.i",
        "'\\p' without charProp is invalid escape seqence in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_52",
        "RegexTest_52.i",
        "'\\P' without charProp is invalid escape seqence in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_53",
        "RegexTest_53.i",
        "'\\p' without charProp is invalid escape seqence in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_54",
        "RegexTest_54.i",
        "'\\P' without charProp is invalid escape seqence in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_55",
        "RegexTest_55.i",
        "'\\p' without charProp is invalid escape seqence in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_56",
        "RegexTest_56.i",
        "'\\p' without charProp is invalid escape seqence in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_57",
        "RegexTest_57.v",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_58",
        "RegexTest_58.v",
        "'\\u' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_65",
        "RegexTest_65.i",
        "'\\o' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_113",
        "RegexTest_113.i",
        "'foo(?(?#COMMENT)foo)' is unkown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_116",
        "RegexTest_116.i",
        "'foo(?(?afdfoo)bar)' is unkown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_119",
        "RegexTest_119.i",
        "'(foo) (?#foo) \\s+ (?#followed by...' is unkown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_120",
        "RegexTest_120.i",
        "'(foo) (?#foo) \\s+ (?#followed by...' is unkown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_121",
        "RegexTest_121.i",
        "'(foo) (?#foo) \\s+ (?#followed by...' is unkown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_141",
        "RegexTest_141.i",
        "'\\077' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_142",
        "RegexTest_142.i",
        "'\\77' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_143",
        "RegexTest_143.i",
        "'\\176' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_145",
        "RegexTest_145.i",
        "'\\300' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_146",
        "RegexTest_146.i",
        "'\\300' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_147",
        "RegexTest_147.i",
        "'\\477' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_148",
        "RegexTest_148.i",
        "'\\777' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_149",
        "RegexTest_149.i",
        "'\\7770' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_150",
        "RegexTest_150.i",
        "'\\7' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_151",
        "RegexTest_151.i",
        "'\\077' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_152",
        "RegexTest_152.i",
        "'\\77' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_154",
        "RegexTest_154.i",
        "'\\40' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_155",
        "RegexTest_155.i",
        "'\\040' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_156",
        "RegexTest_156.i",
        "'\\176' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_157",
        "RegexTest_157.i",
        "'\\377' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_158",
        "RegexTest_158.i",
        "'\\400' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_163",
        "RegexTest_163.i",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_164",
        "RegexTest_164.i",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_165",
        "RegexTest_165.i",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_166",
        "RegexTest_166.i",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_167",
        "RegexTest_167.i",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_168",
        "RegexTest_168.i",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_169",
        "RegexTest_169.i",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_170",
        "RegexTest_170.i",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_171",
        "RegexTest_171.i",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_172",
        "RegexTest_172.i",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_173",
        "RegexTest_173.i",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_174",
        "RegexTest_174.i",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_194",
        "RegexTest_194.v",
        "'\\A' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_195",
        "RegexTest_195.v",
        "'\\A' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_196",
        "RegexTest_196.v",
        "'\\A' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_197",
        "RegexTest_197.i",
        "'\\A' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_198",
        "RegexTest_198.i",
        "'\\A' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_199",
        "RegexTest_199.i",
        "'\\A' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_200",
        "RegexTest_200.v",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_201",
        "RegexTest_201.v",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_202",
        "RegexTest_202.v",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_203",
        "RegexTest_203.i",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_204",
        "RegexTest_204.i",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_205",
        "RegexTest_205.i",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_206",
        "RegexTest_206.i",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_207",
        "RegexTest_207.i",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_208",
        "RegexTest_208.i",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_209",
        "RegexTest_209.v",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_210",
        "RegexTest_210.v",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_211",
        "RegexTest_211.v",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_212",
        "RegexTest_212.i",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_213",
        "RegexTest_213.i",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_214",
        "RegexTest_214.i",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_215",
        "RegexTest_215.i",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_216",
        "RegexTest_216.i",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_217",
        "RegexTest_217.i",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_218",
        "RegexTest_218.i",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_220",
        "RegexTest_220.i",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_221",
        "RegexTest_221.i",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_222",
        "RegexTest_222.i",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_226",
        "RegexTest_226.i",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_230",
        "RegexTest_230.i",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_232",
        "RegexTest_232.i",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_233",
        "RegexTest_233.i",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_294",
        "RegexTest_294.v",
        "'[0-9]+?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_295",
        "RegexTest_295.v",
        "'[0-9]+?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_315",
        "RegexTest_315.v",
        "'*?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_316",
        "RegexTest_316.v",
        "'*?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_317",
        "RegexTest_317.v",
        "'*?' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_440",
        "RegexTest_440.i",
        "'\\u' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_464",
        "RegexTest_464.v",
        "'[[abcd]-[bc]]' is invalid in XSD 1.0 regex because charClassExpr cannot contain another charClassExpr.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_478",
        "RegexTest_478.v",
        "'[' cannot appear standalone as Char in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_481",
        "RegexTest_481.i",
        "Named capturing group is not supported in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_482",
        "RegexTest_482.v",
        "'(?:b*)' is unknown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_483",
        "RegexTest_483.v",
        "Positive Lookahead is not supported in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_484",
        "RegexTest_484.i",
        "Negative Lookahead is not supported in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_487",
        "RegexTest_487.i",
        "Atomic grouping is not supported in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_516",
        "RegexTest_516.v",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_517",
        "RegexTest_517.v",
        "'\\060' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_518",
        "RegexTest_518.v",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_519",
        "RegexTest_519.v",
        "'\\u' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_521",
        "RegexTest_521.i",
        "'(d+?)(e*?)(f??)' is invalid in XSD 1.0 regex becuase a quantifier cannot follows another quantifier.",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_523",
        "RegexTest_523.i",
        "Conditional subpattern syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_524",
        "RegexTest_524.v",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
];
