//! XML canonicalization APIs.
//!
//! The current implementation supports only C14N 1.0 and can only use octet streams
//! as document sources.
//!
//! Support for C14N 1.1 and Exclusive XML C14N, as well as support for XPath node set input,
//! is planned for future expansion.
//!
//! # Example
//! ```rust
//! use anyxml::{
//!     c14n::CanonicalizeHandler,
//!     sax::parser::{ParserOption, XMLReaderBuilder},
//!     uri::URIString,
//! };
//!
//! const DOC: &str = r#"<?xml version="1.0"?>
//!
//! <?xml-stylesheet   href="doc.xsl"
//!    type="text/xsl"   ?>
//!
//! <!DOCTYPE doc SYSTEM "doc.dtd">
//!
//! <doc>Hello, world!<!-- Comment 1 --></doc>
//!
//! <?pi-without-data     ?>
//!
//! <!-- Comment 2 -->
//!
//! <!-- Comment 3 -->"#;
//!
//! let mut reader = XMLReaderBuilder::new()
//!     .set_handler(CanonicalizeHandler::default())
//!     .build();
//! reader.parse_str(DOC, None).ok();
//!
//! assert_eq!(reader.handler.buffer, r#"<?xml-stylesheet href="doc.xsl"
//!    type="text/xsl"   ?>
//! <doc>Hello, world!</doc>
//! <?pi-without-data?>"#);
//! ```
//!
//! # Reference
//! - [Canonical XML Version 1.0 W3C Recommendation 15 March 2001](https://www.w3.org/TR/xml-c14n10/)

use std::{borrow::Cow, collections::HashMap, fmt::Write as _, sync::Arc};

use crate::{
    error::{XMLError, XMLErrorDomain, XMLErrorLevel},
    sax::{
        Locator,
        attributes::Attributes,
        error::SAXParseError,
        handler::{DefaultSAXHandler, EntityResolver, ErrorHandler, SAXHandler},
        source::InputSource,
    },
    uri::URIStr,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum CanonicalizeMethod {
    #[default]
    C14N10,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Position {
    Before,
    InDocumentElement,
    After,
}

/// SAX handler to support streaming XML canonicalization.
///
/// By configuring this handler for the SAX parser and executing parsing,
/// it is possible to canonicalize an octet stream XML document source.
pub struct CanonicalizeHandler<H: SAXHandler = DefaultSAXHandler> {
    /// Canonicalize method. C14N 1.0 is only supported now.
    pub method: CanonicalizeMethod,
    /// If `true`, comments are retained. Default is `false`.
    pub with_comment: bool,
    /// If `true`, writes the canonical XML string to `buffer`.
    /// If `false`, no writing occurs.
    ///
    /// Useful when only interested in receiving events. Default is `true`.
    pub serialize: bool,
    /// Buffer to which the canonical XML string is written.
    ///
    /// Automatically initialized when parsing begins.
    pub buffer: String,
    /// Child handler that receives canonicalized events.
    pub child: H,

    state: Position,
    depth: usize,
    namespace_stack: HashMap<String, Vec<(usize, String)>>,
    localtor: Option<Arc<Locator>>,
}

impl<H: SAXHandler> CanonicalizeHandler<H> {
    /// Initialize the handler with the user-defined handler as its child.
    pub fn with_handler(handler: H) -> Self {
        Self {
            method: Default::default(),
            with_comment: false,
            child: handler,
            serialize: true,
            buffer: Default::default(),
            state: Position::Before,
            depth: 0,
            namespace_stack: HashMap::new(),
            localtor: None,
        }
    }
}

impl<H: SAXHandler> ErrorHandler for CanonicalizeHandler<H> {
    fn error(&mut self, error: SAXParseError) {
        self.child.error(error);
    }

    fn fatal_error(&mut self, error: SAXParseError) {
        self.child.fatal_error(error);
    }

    fn warning(&mut self, error: SAXParseError) {
        self.child.warning(error);
    }
}

impl<H: SAXHandler> EntityResolver for CanonicalizeHandler<H> {
    fn get_external_subset(
        &mut self,
        name: &str,
        base_uri: Option<&URIStr>,
    ) -> Result<InputSource<'static>, XMLError> {
        self.child.get_external_subset(name, base_uri)
    }

    fn resolve_entity(
        &mut self,
        name: &str,
        public_id: Option<&str>,
        base_uri: &URIStr,
        system_id: &URIStr,
    ) -> Result<InputSource<'static>, XMLError> {
        self.child
            .resolve_entity(name, public_id, base_uri, system_id)
    }
}

impl<H: SAXHandler> SAXHandler for CanonicalizeHandler<H> {
    fn characters(&mut self, data: &str) {
        if self.serialize {
            let mut data = data;
            while !data.is_empty() {
                if let Some(pos) = data
                    .bytes()
                    .position(|b| matches!(b, b'&' | b'<' | b'>' | 0xD))
                {
                    self.buffer.push_str(&data[..pos]);
                    match data.as_bytes()[pos] {
                        b'&' => self.buffer.push_str("&amp;"),
                        b'<' => self.buffer.push_str("&lt;"),
                        b'>' => self.buffer.push_str("&gt;"),
                        0xD => self.buffer.push_str("&#xD;"),
                        _ => {}
                    }
                    data = &data[pos + 1..];
                } else {
                    self.buffer.push_str(data);
                    break;
                }
            }
        }

        self.child.characters(data);
    }

    fn start_document(&mut self) {
        self.child.start_document();
    }
    fn end_document(&mut self) {
        self.child.end_document();
    }

    fn start_element(
        &mut self,
        namespace_name: Option<&str>,
        local_name: Option<&str>,
        qname: &str,
        atts: &Attributes,
    ) {
        let mut general_atts = vec![];
        let mut ns_atts = vec![];
        for att in atts {
            if !att.is_nsdecl() {
                general_atts.push(att);
            } else if let Some(stack) = self.namespace_stack.get(
                att.local_name
                    .as_deref()
                    .filter(|&l| l != "xmlns")
                    .unwrap_or_default(),
            ) && stack.last().is_some_and(|l| l.0 == self.depth)
            {
                ns_atts.push(att);
            }
        }

        general_atts.sort_unstable_by_key(|att| (att.uri.as_deref(), att.local_name.as_deref()));
        ns_atts.sort_unstable_by_key(|att| att.local_name.as_deref().filter(|&l| l != "xmlns"));

        let mut retained_atts = Attributes::new();
        for att in ns_atts {
            retained_atts.push(att.clone()).ok();
        }
        for att in general_atts {
            retained_atts.push(att.clone()).ok();
        }
        if self.serialize {
            write!(self.buffer, "<{}", qname).ok();
            for att in &retained_atts {
                write!(self.buffer, " {}=\"", att.qname).ok();
                let mut value = att.value.as_ref();
                while let Some(pos) = value
                    .bytes()
                    .position(|b| matches!(b, b'&' | b'<' | b'"' | 0x9 | 0xA | 0xD))
                {
                    self.buffer.push_str(&value[..pos]);
                    match value.as_bytes()[pos] {
                        b'&' => self.buffer.push_str("&amp;"),
                        b'<' => self.buffer.push_str("&lt;"),
                        b'"' => self.buffer.push_str("&quot;"),
                        0x9 => self.buffer.push_str("&#x9;"),
                        0xA => self.buffer.push_str("&#xA;"),
                        0xD => self.buffer.push_str("&#xD;"),
                        _ => {}
                    }
                    value = &value[pos + 1..];
                }
                self.buffer.push_str(value);
                write!(self.buffer, "\"").ok();
            }
            write!(self.buffer, ">").ok();
        }

        self.state = Position::InDocumentElement;
        self.depth += 1;
        self.child
            .start_element(namespace_name, local_name, qname, &retained_atts);
    }
    fn end_element(&mut self, namespace_name: Option<&str>, local_name: Option<&str>, qname: &str) {
        self.depth -= 1;
        if self.depth == 0 {
            self.state = Position::After;
        }

        if self.serialize {
            write!(self.buffer, "</{}>", qname).ok();
        }
        self.child.end_element(namespace_name, local_name, qname);
    }

    fn start_prefix_mapping(&mut self, prefix: Option<&str>, uri: &str) {
        let pre = prefix.unwrap_or_default();
        if let Some(stack) = self.namespace_stack.get_mut(pre) {
            if stack.last().is_none_or(|l| l.1 != uri) {
                stack.push((self.depth, uri.to_owned()));
                self.child.start_prefix_mapping(prefix, uri);
            }
        } else if !pre.is_empty() || !uri.is_empty() {
            let pre = pre.to_owned();
            let stack = self.namespace_stack.entry(pre).or_default();
            stack.push((self.depth, uri.to_owned()));
            self.child.start_prefix_mapping(prefix, uri);
        }
    }
    fn end_prefix_mapping(&mut self, prefix: Option<&str>) {
        let pre = prefix.unwrap_or_default();
        if let Some(stack) = self.namespace_stack.get_mut(pre) {
            stack.pop_if(|l| l.0 == self.depth);
            if stack.is_empty() {
                self.namespace_stack.remove(pre);
            }
            self.child.end_prefix_mapping(prefix);
        }
    }

    fn processing_instruction(&mut self, target: &str, data: Option<&str>) {
        if self.serialize {
            if matches!(self.state, Position::After) {
                writeln!(self.buffer).ok();
            }

            write!(self.buffer, "<?{}", target).ok();
            if let Some(data) = data {
                write!(self.buffer, " {}", data).ok();
            }
            write!(self.buffer, "?>").ok();

            if matches!(self.state, Position::Before) {
                writeln!(self.buffer).ok();
            }
        }
        self.child.processing_instruction(target, data);
    }

    fn comment(&mut self, data: &str) {
        if !self.with_comment {
            return;
        }
        if self.serialize {
            if matches!(self.state, Position::After) {
                writeln!(self.buffer).ok();
            }
            write!(self.buffer, "<!--{}-->", data).ok();
            if matches!(self.state, Position::Before) {
                writeln!(self.buffer).ok();
            }
        }
        self.child.comment(data);
    }

    fn set_document_locator(&mut self, locator: Arc<Locator>) {
        self.depth = 0;
        self.state = Position::Before;
        self.buffer.clear();
        self.namespace_stack.clear();
        self.localtor = Some(locator.clone());
        self.child.set_document_locator(locator);
    }

    fn skipped_entity(&mut self, name: &str) {
        if let Some(localtor) = self.localtor.as_deref() {
            self.fatal_error(SAXParseError {
                error: XMLError::C14NUnresolvableEntityReference,
                level: XMLErrorLevel::FatalError,
                domain: XMLErrorDomain::C14N,
                line: localtor.line(),
                column: localtor.column(),
                system_id: localtor.system_id(),
                public_id: localtor.public_id(),
                message: Cow::Owned(format!(
                    "An unresolvable entity reference '{}' appeared during the canonicalization process.",
                    name
                )),
            });
        }
        self.child.skipped_entity(name);
    }
}

impl Default for CanonicalizeHandler {
    fn default() -> Self {
        Self {
            method: Default::default(),
            with_comment: false,
            serialize: true,
            buffer: String::default(),
            child: DefaultSAXHandler,
            state: Position::Before,
            depth: 0,
            namespace_stack: Default::default(),
            localtor: None,
        }
    }
}
