use std::{fmt::Write as _, path::Path};

use anyxml::{
    sax::parser::XMLReaderBuilder,
    tree::{TreeBuildHandler, convert::NodeKind},
    uri::URIString,
    xpath::{self, XPathObject},
};

#[test]
fn xpath_evaluation_tests() {
    let mut reader = XMLReaderBuilder::new()
        .set_handler(TreeBuildHandler::default())
        .build();
    reader
        .parse_uri(
            URIString::parse_file_path(
                Path::new("resources/xpath/testsuite.xml")
                    .canonicalize()
                    .unwrap(),
            )
            .unwrap(),
            None,
        )
        .unwrap();
    let document = reader.handler.document.clone();

    let mut expression = xpath::compile("//test-cases").unwrap();
    let XPathObject::NodeSet(test_cases) = expression.evaluate(document).unwrap() else {
        unreachable!()
    };

    for test_cases in test_cases.iter().map(|case| case.as_element().unwrap()) {
        let input_file = test_cases
            .get_elements_by_qname("input-file")
            .next()
            .unwrap()
            .text_content();
        reader
            .parse_uri(
                URIString::parse_file_path(Path::new(input_file.as_str()).canonicalize().unwrap())
                    .unwrap(),
                None,
            )
            .unwrap();
        let document = reader.handler.document.clone();
        for test_case in test_cases.get_elements_by_qname("test-case") {
            let xpath = test_case
                .get_elements_by_qname("xpath")
                .next()
                .unwrap()
                .text_content();
            let output = test_case
                .get_elements_by_qname("output")
                .next()
                .unwrap()
                .text_content();

            let mut buf = String::new();
            eprintln!("input_file: {input_file}, xpath: {xpath}");
            let mut expression = xpath::compile(&xpath).unwrap();
            match expression.evaluate(document.clone()).unwrap() {
                XPathObject::Boolean(boolean) => write!(buf, "{boolean},").unwrap(),
                XPathObject::Number(number) => write!(buf, "{number},").unwrap(),
                XPathObject::String(string) => write!(buf, "{string},").unwrap(),
                XPathObject::NodeSet(nodeset) => {
                    for node in &nodeset {
                        match node.downcast() {
                            NodeKind::Document(document) => {
                                let object = XPathObject::from(document);
                                write!(buf, "{},", object.as_string().unwrap()).unwrap()
                            }
                            NodeKind::Element(element) => {
                                let object = XPathObject::from(element);
                                write!(buf, "{},", object.as_string().unwrap()).unwrap()
                            }
                            NodeKind::Attribute(attribute) => {
                                let object = XPathObject::from(attribute);
                                write!(buf, "{},", object.as_string().unwrap()).unwrap()
                            }
                            NodeKind::Namespace(namespace) => {
                                let object = XPathObject::from(namespace);
                                write!(buf, "{},", object.as_string().unwrap()).unwrap()
                            }
                            NodeKind::ProcessingInstruction(pi) => {
                                let object = XPathObject::from(pi);
                                write!(buf, "{},", object.as_string().unwrap()).unwrap()
                            }
                            NodeKind::Comment(comment) => {
                                let object = XPathObject::from(comment);
                                write!(buf, "{},", object.as_string().unwrap()).unwrap()
                            }
                            NodeKind::Text(text) => {
                                let object = XPathObject::from(text);
                                write!(buf, "{},", object.as_string().unwrap()).unwrap()
                            }
                            NodeKind::CDATASection(cdata) => {
                                let object = XPathObject::from(cdata);
                                write!(buf, "{},", object.as_string().unwrap()).unwrap()
                            }
                            _ => unreachable!(),
                        }
                    }
                }
            }

            assert_eq!(
                buf, output,
                "\n--- input-file: {input_file} xpath: {xpath} ---\nbuf: {buf}\noutput:{output}"
            );
        }
    }
}
