use anyxml::sax::parser::XMLReaderBuilder;
use anyxml_uri::uri::URIString;

#[test]
fn well_formed_tests() {
    let mut reader = XMLReaderBuilder::new().build();
    reader
        .parse_uri(
            URIString::parse("resources/well-formed/tag1.xml").unwrap(),
            None,
        )
        .unwrap();
}
