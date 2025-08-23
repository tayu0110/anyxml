use anyxml::sax::parser::XMLReaderBuilder;

#[test]
fn well_formed_tests() {
    let mut reader = XMLReaderBuilder::new().build();
    reader
        .parse_uri("resources/well-formed/tag1.xml", None)
        .unwrap();
}
