use std::{
    cell::{Cell, RefCell},
    fs::read_dir,
    sync::Arc,
};

use anyxml::{
    error::XMLErrorLevel,
    sax::{handler::ErrorHandler, parser::XMLReaderBuilder},
};
use anyxml_uri::uri::URIString;

struct TestSAXHandler {
    warning: Cell<usize>,
    error: Cell<usize>,
    fatal_error: Cell<usize>,
    buffer: RefCell<String>,
}

impl TestSAXHandler {
    fn new() -> Self {
        Self {
            warning: Cell::new(0),
            error: Cell::new(0),
            fatal_error: Cell::new(0),
            buffer: RefCell::new(String::new()),
        }
    }

    fn reset(&self) {
        self.warning.update(|_| 0);
        self.error.update(|_| 0);
        self.fatal_error.update(|_| 0);
        self.buffer.borrow_mut().clear();
    }
}

impl ErrorHandler for TestSAXHandler {
    fn fatal_error(&self, error: anyxml::sax::error::SAXParseError) {
        use std::fmt::Write;
        assert_eq!(error.level, XMLErrorLevel::FatalError);
        self.fatal_error.update(|c| c + 1);
        writeln!(self.buffer.borrow_mut(), "{}", error).ok();
    }

    fn error(&self, error: anyxml::sax::error::SAXParseError) {
        use std::fmt::Write;
        assert_eq!(error.level, XMLErrorLevel::Error);
        self.error.update(|c| c + 1);
        writeln!(self.buffer.borrow_mut(), "{}", error).ok();
    }

    fn warning(&self, error: anyxml::sax::error::SAXParseError) {
        use std::fmt::Write;
        assert_eq!(error.level, XMLErrorLevel::Warning);
        self.warning.update(|c| c + 1);
        writeln!(self.buffer.borrow_mut(), "{}", error).ok();
    }
}

#[test]
fn well_formed_tests() {
    #[allow(clippy::arc_with_non_send_sync)]
    let handler = Arc::new(TestSAXHandler::new());

    let mut reader = XMLReaderBuilder::new()
        .set_error_handler(handler.clone() as _)
        .build();

    for ent in read_dir("resources/well-formed").unwrap() {
        if let Ok(ent) = ent
            && ent.metadata().unwrap().is_file()
        {
            let uri = URIString::parse_file_path(ent.path().canonicalize().unwrap()).unwrap();
            reader.parse_uri(&uri, None).ok();
            assert_eq!(
                handler.fatal_error.get(),
                0,
                "uri: {}\nerrors:\n{}",
                uri.as_escaped_str(),
                handler.buffer.borrow(),
            );
            handler.reset();
        }
    }
}
