use anyxml::{
    relaxng::RelaxNGSchema, sax::handler::DefaultSAXHandler, tree::Element, uri::URIString,
    xpath::evaluate_uri,
};

#[test]
fn spectest() {
    let uri = URIString::parse(format!(
        "{}/resources/relaxng/spectest.xml",
        env!("CARGO_MANIFEST_DIR"),
    ))
    .unwrap();

    let testsuites = evaluate_uri("//testSuite", uri.as_ref(), None)
        .unwrap()
        .as_nodeset()
        .unwrap();

    for testsuite in &testsuites {
        handle_testsuite(testsuite.as_element().unwrap());
    }
}

fn handle_testsuite(testsuite: Element) {
    let documentation = testsuite
        .get_elements_by_qname("documentation")
        .next()
        .unwrap();
    eprintln!("=== test suite: {} ===", documentation.text_content());

    for testcase in testsuite.get_elements_by_qname("testCase") {
        if let Some(documentation) = testcase.get_elements_by_qname("documentation").next() {
            eprintln!("=== test case: {} ===", documentation.text_content());
        } else {
            eprintln!("=== test case: (no documentation) ===");
        }

        if let Some(correct) = testcase.get_elements_by_qname("correct").next() {
            handle_correct_case(testcase.clone(), correct.first_element_child().unwrap());
        } else {
            let incorrect = testcase.get_elements_by_qname("incorrect").next().unwrap();
            handle_incorrect_case(testcase.clone(), incorrect.first_element_child().unwrap());
        }
    }
}

fn handle_correct_case(testcase: Element, schema: Element) {
    let document = schema.owner_document();
    let schema = format!("{schema}");
    eprintln!("--- begin correct schema ---");
    eprintln!("{schema}");
    eprintln!("--- end correct schema ---");

    let schema = RelaxNGSchema::parse_str(
        &schema,
        document.base_uri().unwrap(),
        None::<DefaultSAXHandler>,
    )
    .unwrap();

    for valid in testcase.get_elements_by_qname("valid") {
        eprintln!("----- begin valid test case -----");
        eprintln!("{valid}");
        eprintln!("----- end valid test case -----");
        assert!(schema.validate(&valid).is_ok());
    }

    for invalid in testcase.get_elements_by_qname("invalid") {
        eprintln!("----- begin invalid test case -----");
        eprintln!("{invalid}");
        eprintln!("----- end invalid test case -----");
        assert!(schema.validate(&invalid).is_ok());
    }
}

fn handle_incorrect_case(_testcase: Element, schema: Element) {
    let document = schema.owner_document();
    let schema = format!("{schema}");
    eprintln!("--- begin incorrect schema ---");
    eprintln!("{schema}");
    eprintln!("--- end incorrect schema ---");

    let schema = RelaxNGSchema::parse_str(
        &schema,
        document.base_uri().unwrap(),
        None::<DefaultSAXHandler>,
    );

    assert!(schema.is_err());
}
