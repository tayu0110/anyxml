use std::collections::HashMap;

use anyxml::{
    error::{XMLError, XMLErrorDomain},
    relaxng::RelaxNGSchema,
    sax::{
        error::SAXParseError,
        handler::{DefaultSAXHandler, EntityResolver, ErrorHandler, SAXHandler},
        parser::XMLReaderBuilder,
        source::InputSource,
    },
    tree::{Element, Node, node::InternalNodeSpec},
    uri::{URIStr, URIString},
    xpath::evaluate_uri,
};

struct ValidationTestHandler {
    error: usize,
}
impl ErrorHandler for ValidationTestHandler {
    fn fatal_error(&mut self, error: SAXParseError) {
        if matches!(error.domain, XMLErrorDomain::RngValid) {
            self.error += 1;
        }
        DefaultSAXHandler.fatal_error(error);
    }
    fn error(&mut self, error: SAXParseError) {
        if matches!(error.domain, XMLErrorDomain::RngValid) {
            self.error += 1;
        }
        DefaultSAXHandler.error(error);
    }
    fn warning(&mut self, error: SAXParseError) {
        if matches!(error.domain, XMLErrorDomain::RngValid) {
            self.error += 1;
        }
        DefaultSAXHandler.warning(error);
    }
}
impl EntityResolver for ValidationTestHandler {}
impl SAXHandler for ValidationTestHandler {}

#[test]
fn spectest() {
    let uri = URIString::parse("resources/relaxng/spectest.xml").unwrap();

    let testsuites = evaluate_uri("//testSuite", uri.as_ref(), None)
        .unwrap()
        .as_nodeset()
        .unwrap();

    for testsuite in &testsuites {
        handle_testsuite(
            testsuite.as_element().unwrap(),
            |sch: &mut RelaxNGSchema, e: Element| {
                assert!(sch.validate(&e, ValidationTestHandler { error: 0 }).is_ok())
            },
            |sch: &mut RelaxNGSchema, e: Element| {
                assert!(
                    sch.validate(&e, ValidationTestHandler { error: 0 })
                        .is_err()
                )
            },
        );
    }
}

#[test]
fn spectest_streaming_validation() {
    let uri = URIString::parse("resources/relaxng/spectest.xml").unwrap();

    let testsuites = evaluate_uri("//testSuite", uri.as_ref(), None)
        .unwrap()
        .as_nodeset()
        .unwrap();

    for testsuite in &testsuites {
        handle_testsuite(
            testsuite.as_element().unwrap(),
            |sch: &mut RelaxNGSchema, e: Element| {
                let mut handler = sch.new_validate_handler(ValidationTestHandler { error: 0 });
                let mut reader = XMLReaderBuilder::new().set_handler(&mut handler).build();
                assert!(reader.parse_str(&e.to_string(), None).is_ok());
                assert_eq!(reader.handler.child.error, 0);
            },
            |sch: &mut RelaxNGSchema, e: Element| {
                let mut handler = sch.new_validate_handler(ValidationTestHandler { error: 0 });
                let mut reader = XMLReaderBuilder::new().set_handler(&mut handler).build();
                assert!(reader.parse_str(&e.to_string(), None).is_ok());
                assert_ne!(reader.handler.child.error, 0);
            },
        );
    }
}

fn handle_testsuite(
    testsuite: Element,
    valid_runner: fn(&mut RelaxNGSchema, Element),
    invalid_runner: fn(&mut RelaxNGSchema, Element),
) {
    if let Some(documentation) = testsuite.get_elements_by_qname("documentation").next() {
        eprintln!("=== test suite: {} ===", documentation.text_content());
    } else {
        eprintln!("=== test suite: (no documentation) ===");
    }

    let mut skip = false;
    for requires in testsuite.get_elements_by_qname("requires") {
        if let Some(library) = requires.get_attribute("datatypeLibrary", None) {
            eprintln!("--- skip because datatypeLibrary '{library}' is not supported. ---");
            skip = true;
        }
    }
    if skip {
        return;
    }

    for testcase in testsuite.get_elements_by_qname("testCase") {
        if let Some(documentation) = testcase.get_elements_by_qname("documentation").next() {
            eprintln!("=== test case: {} ===", documentation.text_content());
        } else {
            eprintln!("=== test case: (no documentation) ===");
        }

        let mut skip = false;
        for requires in testcase.get_elements_by_qname("requires") {
            if let Some(library) = requires.get_attribute("datatypeLibrary", None) {
                eprintln!("--- skip because datatypeLibrary '{library}' is not supported. ---");
                skip = true;
            }
        }

        if skip {
            continue;
        }

        let resource_map = build_resources(&testcase);

        if let Some(correct) = testcase.get_elements_by_qname("correct").next() {
            handle_correct_case(
                testcase.clone(),
                correct.first_element_child().unwrap(),
                resource_map,
                valid_runner,
                invalid_runner,
            );
        } else {
            let incorrect = testcase.get_elements_by_qname("incorrect").next().unwrap();
            handle_incorrect_case(
                testcase.clone(),
                incorrect.first_element_child().unwrap(),
                resource_map,
            );
        }
    }
}

fn build_resources(testcase: &Element) -> HashMap<String, String> {
    let mut children = testcase.first_element_child();
    let mut ret = HashMap::new();
    while let Some(child) = children {
        children = child.next_element_sibling();

        if matches!(child.local_name().as_ref(), "resource" | "dir") {
            let mut children = Some(child.clone());
            let mut dirs: Vec<String> = vec![];
            while let Some(now) = children {
                if now.local_name().as_ref() == "resource" {
                    let name = now.get_attribute("name", None).unwrap();
                    let content = now
                        .first_element_child()
                        .map(|ch| format!("{ch}"))
                        .unwrap_or_default();
                    let name = dirs.iter().fold(String::new(), |s, v| s + v) + name.as_str();
                    ret.insert(name, content);
                } else if now.local_name().as_ref() == "dir" {
                    let name = now.get_attribute("name", None).unwrap();
                    dirs.push(format!("{name}/"));
                }
                if now.local_name().as_ref() == "dir"
                    && let Some(first) = now.first_element_child()
                {
                    children = Some(first);
                } else if !child.is_same_node(&now)
                    && let Some(next) = now.next_element_sibling()
                {
                    children = Some(next);
                } else {
                    children = None;
                    if !child.is_same_node(&now) {
                        let mut now = Node::<dyn InternalNodeSpec>::from(now);
                        while let Some(par) = now.parent_node() {
                            if child.is_same_node(&par) {
                                break;
                            }
                            if par.as_element().unwrap().local_name().as_ref() == "dir" {
                                dirs.pop();
                            }
                            if let Some(next) = par.next_element_sibling() {
                                children = Some(next);
                                break;
                            }
                            now = par
                        }
                    }
                }
            }
        }
    }
    ret
}

struct CustomResourseHandler {
    resource_map: HashMap<URIString, String>,
}

impl EntityResolver for CustomResourseHandler {
    fn resolve_entity(
        &mut self,
        name: &str,
        public_id: Option<&str>,
        base_uri: &URIStr,
        system_id: &URIStr,
    ) -> Result<InputSource<'static>, XMLError> {
        let uri = base_uri.resolve(system_id);
        if let Some(contents) = self.resource_map.get(&uri) {
            let mut source = InputSource::from_content(contents);
            source.set_system_id(uri);
            return Ok(source);
        }

        DefaultSAXHandler.resolve_entity(name, public_id, base_uri, system_id)
    }
}
impl ErrorHandler for CustomResourseHandler {
    fn error(&mut self, error: SAXParseError) {
        DefaultSAXHandler.error(error);
    }
    fn fatal_error(&mut self, error: SAXParseError) {
        DefaultSAXHandler.fatal_error(error);
    }
    fn warning(&mut self, error: SAXParseError) {
        DefaultSAXHandler.warning(error);
    }
}
impl SAXHandler for CustomResourseHandler {}

fn handle_correct_case(
    testcase: Element,
    schema: Element,
    resource_map: HashMap<String, String>,
    valid_runner: fn(&mut RelaxNGSchema, Element),
    invalid_runner: fn(&mut RelaxNGSchema, Element),
) {
    let document = schema.owner_document();
    let schema = format!("{schema}");
    eprintln!("--- begin correct schema ---");
    eprintln!("{schema}");
    eprintln!("--- end correct schema ---");

    let base_uri = document.base_uri().unwrap();
    let resource_map = resource_map
        .into_iter()
        .map(|(uri, content)| {
            let uri = base_uri.resolve(&URIString::parse(uri).unwrap());
            (uri, content)
        })
        .collect::<HashMap<_, _>>();

    let mut schema = RelaxNGSchema::parse_str(
        &schema,
        base_uri,
        Some(CustomResourseHandler { resource_map }),
    )
    .unwrap();

    for valid in testcase.get_elements_by_qname("valid") {
        let valid = valid.first_element_child().unwrap();
        eprintln!("----- begin valid test case -----");
        eprintln!("{valid}");
        eprintln!("----- end valid test case -----");
        valid_runner(&mut schema, valid);
    }

    for invalid in testcase.get_elements_by_qname("invalid") {
        let invalid = invalid.first_element_child().unwrap();
        eprintln!("----- begin invalid test case -----");
        eprintln!("{invalid}");
        eprintln!("----- end invalid test case -----");
        invalid_runner(&mut schema, invalid);
    }
}

fn handle_incorrect_case(
    _testcase: Element,
    schema: Element,
    resource_map: HashMap<String, String>,
) {
    let document = schema.owner_document();
    let schema = format!("{schema}");
    eprintln!("--- begin incorrect schema ---");
    eprintln!("{schema}");
    eprintln!("--- end incorrect schema ---");

    let base_uri = document.base_uri().unwrap();
    let resource_map = resource_map
        .into_iter()
        .map(|(uri, content)| {
            let uri = base_uri.resolve(&URIString::parse(uri).unwrap());
            (uri, content)
        })
        .collect::<HashMap<_, _>>();

    let schema = RelaxNGSchema::parse_str(
        &schema,
        base_uri,
        Some(CustomResourseHandler { resource_map }),
    );

    assert!(schema.is_err());
}
