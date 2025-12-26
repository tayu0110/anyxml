use anyxml::{
    sax::parser::XMLReaderBuilder,
    tree::{Element, TreeBuildHandler},
    uri::URIString,
    xpath::evaluate_uri,
    xpointer::parse_xpointer,
};

#[test]
fn testsuite() {
    let uri = URIString::parse("resources/xpointer/testsuite.xml").unwrap();

    let testcases = evaluate_uri("//testCases", uri.as_ref(), None)
        .unwrap()
        .as_nodeset()
        .unwrap();

    for testcases in &testcases {
        handle_testcase(testcases.as_element().unwrap());
    }
}

fn handle_testcase(testcases: Element) {
    if let Some(documentation) = testcases.get_elements_by_qname("description").next() {
        eprintln!("=== test suite: {} ===", documentation.text_content());
    } else {
        eprintln!("=== test suite: (no documentation) ===");
    }

    let mut reader = XMLReaderBuilder::new()
        .set_handler(TreeBuildHandler::default())
        .build();
    for testcase in testcases.get_elements_by_qname("testCase") {
        let resource = testcase
            .get_elements_by_qname("resource")
            .next()
            .unwrap()
            .text_content();
        reader.parse_str(&resource, None).unwrap();
        assert!(!reader.handler.fatal_error);
        let document = reader.handler.document.clone();

        let xpointer = testcase
            .get_elements_by_qname("xpointer")
            .next()
            .unwrap()
            .text_content();
        let resolver = parse_xpointer(&xpointer);

        let result = testcase.get_elements_by_qname("result").next().unwrap();
        if result
            .get_attribute("unparsable", None)
            .is_some_and(|att| att == "yes")
        {
            assert!(
                resolver.is_err(),
                "expect: unparsable, xpointer: {xpointer}"
            );
        } else if result
            .get_attribute("unresolvable", None)
            .is_some_and(|att| att == "yes")
        {
            let resolver = resolver.unwrap();
            let sub_resource = resolver.resolve(document);
            assert!(
                sub_resource.is_none(),
                "expect: unresolvable, xpointer: {xpointer}, {}",
                sub_resource.unwrap()
            );
        } else {
            let resolver = resolver.unwrap();
            let sub_resource = resolver.resolve(document).unwrap();
            assert_eq!(sub_resource.text_content(), result.text_content());
        }
    }
}
