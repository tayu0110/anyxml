use anyxml::{
    c14n::{CanonicalizeHandler, CanonicalizeMethod},
    sax::{ParserConfig, ParserOption, XMLReader},
    uri::URIString,
};

fn run_c14n(test: &str, method: CanonicalizeMethod, comment: bool, config: ParserConfig) {
    let method_str = match method {
        CanonicalizeMethod::C14N10 => "c14n10",
        CanonicalizeMethod::C14N11 => "c14n11",
    };

    let uri = URIString::parse_file_path(format!("resources/{method_str}/{test}.xml",)).unwrap();

    let mut handler = CanonicalizeHandler::default();
    handler.method = method;
    handler.with_comment = comment;
    let mut reader = XMLReader::builder()
        .set_parser_config(config)
        .set_handler(handler)
        .build();

    reader.parse_uri(uri.as_ref(), None).unwrap();

    let path = if comment {
        format!("resources/{method_str}/result/{test}-with-comment.xml")
    } else {
        format!("resources/{method_str}/result/{test}.xml")
    };
    assert_eq!(
        reader.handler.buffer,
        std::fs::read_to_string(path).unwrap()
    );
}

#[test]
fn c14n10_example1_test() {
    run_c14n(
        "example01",
        CanonicalizeMethod::C14N10,
        false,
        ParserConfig::default(),
    );
    run_c14n(
        "example01",
        CanonicalizeMethod::C14N10,
        true,
        ParserConfig::default(),
    );
}

#[test]
fn c14n10_example2_test() {
    run_c14n(
        "example02",
        CanonicalizeMethod::C14N10,
        false,
        ParserConfig::default(),
    );
}

#[test]
fn c14n10_example3_test() {
    run_c14n(
        "example03",
        CanonicalizeMethod::C14N10,
        false,
        ParserConfig::default(),
    );
}

#[test]
fn c14n10_example4_test() {
    run_c14n(
        "example04",
        CanonicalizeMethod::C14N10,
        false,
        ParserConfig::default(),
    );
}

#[test]
fn c14n10_example5_test() {
    run_c14n(
        "example05",
        CanonicalizeMethod::C14N10,
        false,
        ParserConfig::default() | ParserOption::ExternalGeneralEntities,
    );
}

#[test]
fn c14n10_example6_test() {
    run_c14n(
        "example06",
        CanonicalizeMethod::C14N10,
        false,
        ParserConfig::default(),
    );
}

#[test]
fn c14n11_example1_test() {
    run_c14n(
        "example01",
        CanonicalizeMethod::C14N11,
        false,
        ParserConfig::default(),
    );
    run_c14n(
        "example01",
        CanonicalizeMethod::C14N11,
        true,
        ParserConfig::default(),
    );
}

#[test]
fn c14n11_example2_test() {
    run_c14n(
        "example02",
        CanonicalizeMethod::C14N11,
        false,
        ParserConfig::default(),
    );
}

#[test]
fn c14n11_example3_test() {
    run_c14n(
        "example03",
        CanonicalizeMethod::C14N11,
        false,
        ParserConfig::default(),
    );
}

#[test]
fn c14n11_example4_test() {
    run_c14n(
        "example04",
        CanonicalizeMethod::C14N11,
        false,
        ParserConfig::default(),
    );
}

#[test]
fn c14n11_example5_test() {
    run_c14n(
        "example05",
        CanonicalizeMethod::C14N11,
        false,
        ParserConfig::default() | ParserOption::ExternalGeneralEntities,
    );
}

#[test]
fn c14n11_example6_test() {
    run_c14n(
        "example06",
        CanonicalizeMethod::C14N11,
        false,
        ParserConfig::default(),
    );
}
