# anyxml
`anyxml` is a fully spec-conformant XML library.

# Features
The current implementation supports the following features:

- parse XML 1.0 document
- validate XML 1.0 document using DTD
- handle namespace conforming to XML Namespace 1.0

## Parser
You can use a SAX-like API designed with reference to Java SAX API.

The key difference from the Java API is that SAX handlers are provided solely as two traits: `SAXHandler` and `EntityResolver`.  \
This approach reduces opportunities to use `Rc`/`Arc` or internal mutability.

### Example
```rust
use std::fmt::Write as _;

use anyxml::sax::{
    Attribute,
    handler::{EntityResolver, SAXHandler},
    parser::XMLReaderBuilder,
};

#[derive(Default)]
struct ExampleHandler {
    buffer: String,
}
impl EntityResolver for ExampleHandler {}
impl SAXHandler for ExampleHandler {
    fn start_document(&mut self) {
        writeln!(self.buffer, "start document").ok();
    }
    fn end_document(&mut self) {
        writeln!(self.buffer, "end document").ok();
    }

    fn start_element(
        &mut self,
        _uri: Option<&str>,
        _local_name: Option<&str>,
        qname: &str,
        atts: &[Attribute]
    ) {
        writeln!(self.buffer, "start element {qname}").ok();
    }
    fn end_element(
        &mut self,
        _uri: Option<&str>,
        _local_name: Option<&str>,
        qname: &str
    ) {
        writeln!(self.buffer, "end element {qname}").ok();
    }

    fn characters(&mut self, data: &str) {
        writeln!(self.buffer, "characters '{data}'").ok();
    }
}

let mut reader = XMLReaderBuilder::new()
    .set_handler(ExampleHandler::default())
    .build();
reader.parse_str(r#"<?xml version="1.0"?><greeting>Hello!!</greeting>"#, None).ok();

let handler = reader.handler;
assert_eq!(r#"start document
start element greeting
characters 'Hello!!'
end element greeting
end document
"#, handler.buffer);
```

# Conformance
This crate conforms to the following specifications:

- [Extensible Markup Language (XML) 1.0 (Fifth Edition)](https://www.w3.org/TR/2008/REC-xml-20081126/)
- [Namespaces in XML 1.0 (Third Edition)](https://www.w3.org/TR/2009/REC-xml-names-20091208/)

# Tests
This crate passes the following tests:

- [XML Conformance Test Suites](https://www.w3.org/XML/Test/)
- some self-made tests