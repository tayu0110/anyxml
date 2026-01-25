# anyxml
`anyxml` is a fully spec-conformant XML library.

# Features
The current implementation supports the following features:

- parse XML 1.0 document
    - [x] DTD parsing
    - [x] General entity reference substitution
    - [x] Parameter entity reference substitution
    - [x] Character reference substitution
    - [x] Attribute value normalization
    - [x] Default attribute value handling
- validate XML 1.0 document
    - [x] DTD (parser embeded)
    - [x] RELAX NG (for document tree, XML Syntax only)
    - [ ] XML Schema
- handle namespace conforming to XML Namespace 1.0
- build, modify and serialize XML document trees
- execute XPath and lookup specific node in the document tree
    - [x] XPath 1.0
    - [ ] XPath 2.0 or later
- resolve an alternative URI of external identifiers or URI using XML Catalogs
- merge XML documents using XInclude.
- canonicalize XML document.
    - [x] C14N 1.0 (only octet stream source is supported now)

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
        _namespace_name: Option<&str>,
        _local_name: Option<&str>,
        qname: &str,
        _atts: &Attributes,
    ) {
        writeln!(self.buffer, "start element {qname}").ok();
    }
    fn end_element(
        &mut self,
        _namespace_name: Option<&str>,
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
use anyxml::stax::{
    events::XMLEvent::*,
    XMLStreamReader
};

let mut reader = XMLStreamReader::default();
reader
    .parse_str(r#"<greeting>Hello!!</greeting>"#, None)
    .unwrap();

assert!(matches!(reader.next_event(), Ok(StartDocument)));
assert!(matches!(reader.next_event(), Ok(StartElement(_))));
assert!(matches!(reader.next_event(), Ok(Characters("Hello!!"))));
assert!(matches!(reader.next_event(), Ok(EndElement(_))));
assert!(matches!(reader.next_event(), Ok(EndDocument)));
assert!(matches!(reader.next_event(), Ok(Finished)));
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
    tree::TreeBuildHandler
};

let mut reader = XMLReaderBuilder::new()
    .set_handler(TreeBuildHandler::default())
    .build();
reader
    .parse_str(r#"<greeting>Hello</greeting>"#, None)
    .unwrap();

// If a fatal error occurs, the constructed tree is meaningless.
assert!(!reader.handler.fatal_error);
let document = reader.handler.document;

let mut root = document.first_child().unwrap().as_element().unwrap();
assert_eq!(root.name().as_ref(), "greeting");

let text = root.first_child().unwrap().as_text().unwrap();
assert_eq!(&*text.data(), "Hello");

// modify the document tree
root.append_child(document.create_text(" World!!")).unwrap();
// serialize the document tree
assert_eq!(
    document.to_string(),
    r#"<greeting>Hello World!!</greeting>"#
);
```

## XPath Execution
This crate supports XPath, enabling the search for specific nodes within the document tree.

In the current implementation, only XPath 1.0 is available; features not explicitly defined in the XPath 1.0 specification (such as functions defined in the XSLT or XPointer specifications) cannot be used.

In the following example, the `evaluate_str` function compiles the XPath, parses the document, and evaluates the XPath all at once.  \
If you use the same XPath repeatedly, you can use the `compile` function to obtain a precompiled XPath expression.

### Example
```rust
use anyxml::xpath::evaluate_str;

const DOCUMENT: &str = r#"<root>
    <greeting xml:lang='en'>Hello</greeting>
    <greeting xml:lang='ja'>こんにちは</greeting>
    <greeting xml:lang='ch'>你好</greeting>
</root>
"#;
const XPATH: &str = "//greeting[lang('ja')]/text()";

let text = evaluate_str(XPATH, DOCUMENT, None)
    .unwrap()
    .as_string()
    .unwrap();
assert_eq!(text.as_ref(), "こんにちは");
```

# Conformance
This crate conforms to the following specifications:

- [Extensible Markup Language (XML) 1.0 (Fifth Edition)](https://www.w3.org/TR/2008/REC-xml-20081126/)
- [Namespaces in XML 1.0 (Third Edition)](https://www.w3.org/TR/2009/REC-xml-names-20091208/)
- [XML Base (Second Edition)](https://www.w3.org/TR/2009/REC-xmlbase-20090128/)
- [xml:id Version 1.0](https://www.w3.org/TR/2005/REC-xml-id-20050909/)
- [XML Path Language (XPath) Version 1.0](https://www.w3.org/TR/1999/REC-xpath-19991116/)
- [XML Catalogs (OASIS Standard V1.1, 7 October 2005)](https://groups.oasis-open.org/higherlogic/ws/public/download/14810/xml-catalogs.pdf/latest)
- [ISO/IEC 19757-2:2008 Part 2: Regular-grammar-based validation — RELAX NG](https://www.iso.org/standard/52348.html)
- XPointer specifications
    - [XPointer Framework](https://www.w3.org/TR/xptr-framework/)
    - [XPointer element() Scheme](https://www.w3.org/TR/xptr-element/)
    - [XPointer xmlns() Scheme](https://www.w3.org/TR/xptr-xmlns/)
- [XML Inclusions (XInclude) Version 1.0 (Second Edition)](https://www.w3.org/TR/2006/REC-xinclude-20061115/)
- [Canonical XML Version 1.0 W3C Recommendation 15 March 2001](https://www.w3.org/TR/xml-c14n10/)

# Tests
This crate passes the following tests:

- [XML Conformance Test Suites](https://www.w3.org/XML/Test/)
- [xml:id Conformance Test Suites](https://www.w3.org/XML/Test/xml-id/)
- [OASIS XSLT Test Suites](https://www.oasis-open.org/committees/document.php?document_id=12171&wg_abbrev=xslt) (for XPath)
    - This link is already broken and requires the Wayback Machine to access it.
- [RELAX NG Test Suite by James Clark](https://gnosis.cx/download/gnosis/xml/relax/spectest.xml)
- [XML Inclusions (XInclude) Conformance Test Suites](https://www.w3.org/XML/Test/XInclude/)
- some self-made tests

# Performance
Currently, I won't make any specific comments regarding performance.  \
I only care about two things: expanding features and ensuring proper operation.

Some crates improve performance by not adhering to the specification—such as skipping DTD reading, entity expansion, or value normalization—but this crate prioritizes conforming to the specification as much as possible.

# Command Line Utility
As a related project, a simple command line utility [`anyxml-cli`](https://crates.io/crates/anyxml-cli) is also provided.
