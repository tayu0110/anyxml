use anyxml::{
    c14n::CanonicalizeHandler,
    sax::parser::{ParserOption, XMLReaderBuilder},
    uri::URIString,
};

#[test]
fn c14n10_example1_test() {
    let uri = URIString::parse_file_path("resources/c14n10/example01.xml").unwrap();

    let handler = CanonicalizeHandler::default();
    let mut reader = XMLReaderBuilder::new().set_handler(handler).build();

    reader.parse_uri(uri.as_ref(), None).unwrap();
    assert_eq!(
        reader.handler.buffer,
        include_str!("../resources/c14n10/result/example01.xml")
    );

    reader.handler.with_comment = true;
    reader.parse_uri(uri.as_ref(), None).unwrap();
    assert_eq!(
        reader.handler.buffer,
        include_str!("../resources/c14n10/result/example01-with-comment.xml")
    );
}

#[test]
fn c14n10_example2_test() {
    let uri = URIString::parse_file_path("resources/c14n10/example02.xml").unwrap();

    let handler = CanonicalizeHandler::default();
    let mut reader = XMLReaderBuilder::new().set_handler(handler).build();

    reader.parse_uri(uri.as_ref(), None).unwrap();
    assert_eq!(
        reader.handler.buffer,
        include_str!("../resources/c14n10/result/example02.xml")
    );
}

#[test]
fn c14n10_example3_test() {
    let uri = URIString::parse_file_path("resources/c14n10/example03.xml").unwrap();

    let handler = CanonicalizeHandler::default();
    let mut reader = XMLReaderBuilder::new().set_handler(handler).build();

    reader.parse_uri(uri.as_ref(), None).unwrap();
    assert_eq!(
        reader.handler.buffer,
        include_str!("../resources/c14n10/result/example03.xml")
    );
}

#[test]
fn c14n10_example4_test() {
    let uri = URIString::parse_file_path("resources/c14n10/example04.xml").unwrap();

    let handler = CanonicalizeHandler::default();
    let mut reader = XMLReaderBuilder::new().set_handler(handler).build();

    reader.parse_uri(uri.as_ref(), None).unwrap();
    assert_eq!(
        reader.handler.buffer,
        include_str!("../resources/c14n10/result/example04.xml")
    );
}

#[test]
fn c14n10_example5_test() {
    let uri = URIString::parse_file_path("resources/c14n10/example05.xml").unwrap();

    let handler = CanonicalizeHandler::default();
    let mut reader = XMLReaderBuilder::new()
        .enable_option(ParserOption::ExternalGeneralEntities)
        .set_handler(handler)
        .build();

    reader.parse_uri(uri.as_ref(), None).unwrap();
    assert_eq!(
        reader.handler.buffer,
        include_str!("../resources/c14n10/result/example05.xml")
    );
}

#[test]
fn c14n10_example6_test() {
    let uri = URIString::parse_file_path("resources/c14n10/example06.xml").unwrap();

    let handler = CanonicalizeHandler::default();
    let mut reader = XMLReaderBuilder::new().set_handler(handler).build();

    reader.parse_uri(uri.as_ref(), None).unwrap();
    assert_eq!(
        reader.handler.buffer,
        include_str!("../resources/c14n10/result/example06.xml")
    );
}
