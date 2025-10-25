# anyxml
`anyxml` is a fully spec-conformant XML library.

# Features
The current implementation supports the following features:

- parse XML 1.0 document
    - [x] DTD parsing
    - [x] Entity reference substitution (both general entity and parameter entity are supported)
    - [x] Character reference substitution
    - [x] Attribute value normalization
    - [x] Default attribute value handling
- validate XML 1.0 document using DTD
- handle namespace conforming to XML Namespace 1.0
- build, modify and serialize XML document trees

## Parser
You can use a SAX-like API designed with reference to Java SAX API.

The key difference from the Java API is that SAX handlers are provided solely as three traits: `SAXHandler`, `EntityResolver` and `ErrorHandler`.  \
This approach reduces opportunities to use `Rc`/`Arc` or internal mutability.

### Example
```rust
use std::fmt::Write as _;

use anyxml::sax::{
    attributes::Attributes,
    handler::{EntityResolver, ErrorHandler, SAXHandler},
    parser::XMLReaderBuilder,
};

#[derive(Default)]
struct ExampleHandler {
    buffer: String,
}
impl EntityResolver for ExampleHandler {}
impl ErrorHandler for ExampleHandler {}
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
        _atts: &Attributes,
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

## Parser (Progressive)
SAX-like parsers appear to retrieve all data from a specific source at once, but in some cases, applications may want to provide data incrementally.  \
This crate also supports such feature, which libxml2 calls as "Push type parser" or "Progressive type parser".

In this crate, this feature is called the "Progressive Parser".  \
This is because "push" and "pull" are generally used as terms to classify how the parser delivers the parsing results to the application, rather than how the source is provided to the parser.

When parsing XML documents that retrieve external resources, note that the application must set the appropriate base URI for the parser before starting parsing.  \
By default, the current directory is set as the base URI.

### Example
```rust
use anyxml::sax::{
    attributes::Attributes,
    handler::DebugHandler,
    parser::XMLReaderBuilder,
};

let mut reader = XMLReaderBuilder::new()
    .set_handler(DebugHandler::default())
    .progressive_parser()
    .build();
let source = br#"<greeting>Hello!!</greeting>"#;

for chunk in source.chunks(5) {
    reader.parse_chunk(chunk, false).ok();
}
// Note that the last chunk must set `finish` to `true`.
// As shown below, it's okay for an empty chunk.
reader.parse_chunk([], true).ok();

let handler = reader.handler;
assert_eq!(r#"setDocumentLocator()
startDocument()
startElement(None, greeting, greeting)
characters(Hello!!)
endElement(None, greeting, greeting)
endDocument()
"#, handler.buffer);
```

## Parser (StAX)
This crate also supports StAX (Streaming API for XML) style parser.  \
Unlike SAX parsers, which cannot control the timing of event reports, applications can retrieve events from StAX parsers at arbitrary moments.

StAX parser does not require event handlers, but applications can configure user-defined `EntityResolver` and `ErrorHandler`.  \
To capture all errors except unrecoverable fatal error, configuring `ErrorHandler` is mandatory. If no `ErrorHandler` is configured, only the last error can be retrieved.

### Example
```rust
use anyxml::stax::{XMLStreamReader, events::XMLEvent::*};

let mut reader = XMLStreamReader::default();
reader.parse_str(r#"<greeting>Hello!!</greeting>"#, None).unwrap();

assert!(matches!(reader.next_event().unwrap(), StartDocument));
assert!(matches!(reader.next_event().unwrap(), StartElement(_)));
assert!(matches!(reader.next_event().unwrap(), Characters("Hello!!")));
assert!(matches!(reader.next_event().unwrap(), EndElement(_)));
assert!(matches!(reader.next_event().unwrap(), EndDocument));
assert!(matches!(reader.next_event().unwrap(), Finished));
```

## Tree Manipulation
This API represents the entire XML document as a tree and provides methods for manipulating it.

When parsing an XML document to construct the tree, you can use handlers for SAX parsers.
Even without a document, you can build the tree by creating and editing various nodes starting from the `Document` node.

This API assumes namespace support, so it may not accept prefixed names without a specified namespace name.

### Example
```rust
use anyxml::{
    sax::parser::XMLReaderBuilder,
    tree::TreeBuildHandler,
};

let mut reader = XMLReaderBuilder::new().set_handler(TreeBuildHandler::default()).build();
reader.parse_str(r#"<greeting>Hello</greeting>"#, None).unwrap();

// If a fatal error occurs, the constructed tree is meaningless.
assert!(!reader.handler.fatal_error);
let document = reader.handler.document;

let mut root = document
    .first_child()
    .unwrap()
    .as_element()
    .unwrap();
assert_eq!(root.name().as_ref(), "greeting");

let text = root
    .first_child()
    .unwrap()
    .as_text()
    .unwrap();
assert_eq!(&*text.data(), "Hello");

// modify the document tree
root.append_child(document.create_text(" World!!")).unwrap();
// serialize the document tree
assert_eq!(document.to_string(), r#"<greeting>Hello World!!</greeting>"#);
```


# Conformance
This crate conforms to the following specifications:

- [Extensible Markup Language (XML) 1.0 (Fifth Edition)](https://www.w3.org/TR/2008/REC-xml-20081126/)
- [Namespaces in XML 1.0 (Third Edition)](https://www.w3.org/TR/2009/REC-xml-names-20091208/)
- [XML Base (Second Edition)](https://www.w3.org/TR/2009/REC-xmlbase-20090128/)
- [xml:id Version 1.0](https://www.w3.org/TR/2005/REC-xml-id-20050909/)

# Tests
This crate passes the following tests:

- [XML Conformance Test Suites](https://www.w3.org/XML/Test/)
- [xml:id Conformance Test Suites](https://www.w3.org/XML/Test/xml-id/)
- some self-made tests