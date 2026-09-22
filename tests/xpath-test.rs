use std::fmt::Write as _;

use anyxml::{
    error::XMLError,
    sax::{ParserOption, XMLReader},
    tree::{TreeBuildHandler, convert::NodeKind},
    uri::URIString,
    xpath::XPathObject,
};

#[test]
fn xpath_evaluation_tests() -> Result<(), XMLError> {
    let mut reader = XMLReader::builder()
        .set_handler(TreeBuildHandler::default())
        .enable_option(ParserOption::ExternalGeneralEntities)
        .build();
    reader.parse_uri(URIString::parse("resources/xpath/testsuite.xml")?, None)?;
    let document = reader.handler.document.clone();

    let test_cases = document.xpath("//test-cases")?.as_nodeset()?;

    for test_cases in test_cases.iter().map(|case| case.as_element().unwrap()) {
        let input_file = test_cases.xpath("./input-file[1]")?.as_nodeset()?[0].text_content();
        reader.parse_uri(URIString::parse(input_file.as_str())?, None)?;
        let document = reader.handler.document.clone();
        for test_case in test_cases.xpath("./test-case")?.as_nodeset()? {
            let xpath = test_case.xpath("./xpath[1]")?.as_nodeset()?[0].text_content();
            let output = test_case.xpath("./output[1]")?.as_nodeset()?[0].text_content();

            let mut buf = String::new();
            eprintln!("input_file: {input_file}, xpath: {xpath}");
            match document.xpath(&xpath).unwrap() {
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

    Ok(())
}
