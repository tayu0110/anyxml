//! APIs for parsing RELAX NG schema document and validate XML document using parsed schema.
//!
//! The current implementation supports only RELAX NG schemas using XML syntax and only
//! supports tree validation. Parsing schemas using Compact Syntax and streaming validation
//! that can be inserted into SAX handlers are not supported at this time.
//!
//! # Reference
//! - [XML Catalogs (OASIS Standard V1.1, 7 October 2005)](https://groups.oasis-open.org/higherlogic/ws/public/download/14810/xml-catalogs.pdf/latest)

mod datatype_library;
mod grammar;
mod validate;

use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    io::Read,
};

use crate::{
    XML_NS_NAMESPACE, XML_XML_NAMESPACE, XMLVersion,
    error::XMLError,
    relaxng::{datatype_library::RelaxNGDatatypeLibraries, grammar::RelaxNGGrammar},
    sax::{
        handler::{ErrorHandler, SAXHandler},
        parser::XMLReaderBuilder,
    },
    tree::{
        Document, Element, Namespace, Node, NodeType, TreeBuildHandler,
        convert::NodeKind,
        node::{InternalNodeSpec, NodeSpec},
    },
    uri::{URIStr, URIString, rfc2396::validate_rfc2396_absolute_uri},
};

pub const XML_RELAX_NG_NAMESPACE: &str = "http://relaxng.org/ns/structure/1.0";

macro_rules! generic_error {
    ( $doc:expr, $method:ident, $handler:expr, $code:expr, $level:expr, $message:literal, $( $args:expr ),*) => {
        #[allow(unused)]
        use $crate::error::XMLError::*;
        $handler.$method($crate::sax::error::SAXParseError {
            error: $code,
            level: $level,
            domain: $crate::error::XMLErrorDomain::RngParser,
            line: 0,
            column: 0,
            system_id: $doc.document_base_uri().as_ref().into(),
            public_id: None,
            message: ::std::borrow::Cow::Owned(format!($message, $( $args ),*)),
        })
    };
    ( $doc:expr, $method:ident, $handler:expr, $code:expr, $level:expr, $message:literal) => {
        #[allow(unused)]
        use $crate::error::XMLError::*;
        $handler.$method($crate::sax::error::SAXParseError {
            error: $code,
            level: $level,
            domain: $crate::error::XMLErrorDomain::RngParser,
            line: 0,
            column: 0,
            system_id: $doc.document_base_uri().as_ref().into(),
            public_id: None,
            message: ::std::borrow::Cow::Borrowed($message),
        })
    };
    ( $doc:expr, $method:ident, $handler:expr, $code:expr, $level:expr, $message:expr) => {
        #[allow(unused)]
        use $crate::error::XMLError::*;
        $handler.$method($crate::sax::error::SAXParseError {
            error: $code,
            level: $level,
            domain: $crate::error::XMLErrorDomain::RngParser,
            line: 0,
            column: 0,
            system_id: $doc.document_base_uri().as_ref().into(),
            public_id: None,
            message: ::std::borrow::Cow::Owned($message.into()),
        })
    };
}

macro_rules! fatal_error {
    ($context:expr, $handler:expr, $code:ident, $message:literal, $( $args:expr ),*) => {
        #[allow(unused)]
        use $crate::error::XMLError::*;
        generic_error!(
            $context.document,
            fatal_error,
            $handler,
            $code,
            $crate::error::XMLErrorLevel::FatalError,
            $message,
            $( $args ),*
        );
        $context.last_error = Err($code);
    };
    ($context:expr, $handler:expr, $code:ident, $message:literal) => {
        fatal_error!($context, $handler, $code, $message, );
    };
    ($context:expr, $handler:expr, $code:ident, $message:expr) => {
        generic_error!(
            $context.document,
            fatal_error,
            $handler,
            $code,
            $crate::error::XMLErrorLevel::FatalError,
            $message
        );
        $context.last_error = Err($code);
    };
}

macro_rules! error {
    ($context:expr, $handler:expr, $code:ident, $message:literal, $( $args:expr ),*) => {
        #[allow(unused)]
        use $crate::error::XMLError::*;
        generic_error!(
            $context.document,
            error,
            $handler,
            $code,
            $crate::error::XMLErrorLevel::Error,
            $message,
            $( $args ),*
        );
        $context.last_error = Err($code);
    };
    ($context:expr, $handler:expr, $code:ident, $message:literal) => {
        error!($context, $handler, $code, $message, );
    };
    ($context:expr, $handler:expr, $code:ident, $message:expr) => {
        generic_error!(
            $context.document,
            error,
            $handler,
            $code,
            $crate::error::XMLErrorLevel::Error,
            $message
        );
        $context.last_error = Err($code);
    };
}

pub struct RelaxNGSchema {
    grammar: RelaxNGGrammar,
}

impl RelaxNGSchema {
    /// Parse RELAX NG schema using `uri` and `encoding`.
    ///
    /// If a custom [`EntityResolver`] or [`ErrorHandler`] is required,
    /// it can be specified using `handler`.
    ///
    /// If the document cannot be parsed for any reason,
    /// or the parsed document cannot be recognized as the RELAX NG schema, return [`Err`].
    pub fn parse_uri<Handler: SAXHandler>(
        uri: impl AsRef<URIStr>,
        encoding: Option<&str>,
        handler: Option<Handler>,
    ) -> Result<Self, XMLError> {
        if let Some(handler) = handler {
            let mut reader = XMLReaderBuilder::new()
                .set_handler(TreeBuildHandler::with_handler(handler))
                .build();
            reader.parse_uri(uri, encoding)?;
            Self::do_parse_relaxng(reader.handler)
        } else {
            let mut reader = XMLReaderBuilder::new()
                .set_handler(TreeBuildHandler::default())
                .build();
            reader.parse_uri(uri, encoding)?;
            Self::do_parse_relaxng(reader.handler)
        }
    }

    /// Parse RELAX NG schema using `reader`, `encoding` and `uri`.
    ///
    /// If a custom [`EntityResolver`] or [`ErrorHandler`] is required,
    /// it can be specified using `handler`.
    ///
    /// If the document cannot be parsed for any reason,
    /// or the parsed document cannot be recognized as the RELAX NG schema, return [`Err`].
    pub fn parse_reader<'a, Handler: SAXHandler>(
        reader: impl Read + 'a,
        encoding: Option<&str>,
        uri: impl AsRef<URIStr>,
        handler: Option<Handler>,
    ) -> Result<Self, XMLError> {
        if let Some(handler) = handler {
            let mut parser = XMLReaderBuilder::new()
                .set_handler(TreeBuildHandler::with_handler(handler))
                .build();
            parser.parse_reader(reader, encoding, Some(uri.as_ref()))?;
            Self::do_parse_relaxng(parser.handler)
        } else {
            let mut parser = XMLReaderBuilder::new()
                .set_handler(TreeBuildHandler::default())
                .build();
            parser.parse_reader(reader, encoding, Some(uri.as_ref()))?;
            Self::do_parse_relaxng(parser.handler)
        }
    }

    /// Parse RELAX NG schema using `schema` and `uri`.
    ///
    /// If a custom [`EntityResolver`] or [`ErrorHandler`] is required,
    /// it can be specified using `handler`.
    ///
    /// If the document cannot be parsed for any reason,
    /// or the parsed document cannot be recognized as the RELAX NG schema, return [`Err`].
    pub fn parse_str<Handler: SAXHandler>(
        schema: &str,
        uri: impl AsRef<URIStr>,
        handler: Option<Handler>,
    ) -> Result<Self, XMLError> {
        if let Some(handler) = handler {
            let mut parser = XMLReaderBuilder::new()
                .set_handler(TreeBuildHandler::with_handler(handler))
                .build();
            parser.parse_str(schema, Some(uri.as_ref()))?;
            Self::do_parse_relaxng(parser.handler)
        } else {
            let mut parser = XMLReaderBuilder::new()
                .set_handler(TreeBuildHandler::default())
                .build();
            parser.parse_str(schema, Some(uri.as_ref()))?;
            Self::do_parse_relaxng(parser.handler)
        }
    }

    fn do_parse_relaxng<Handler: SAXHandler>(
        handler: TreeBuildHandler<Handler>,
    ) -> Result<Self, XMLError> {
        let TreeBuildHandler {
            document, handler, ..
        } = handler;
        let mut context = RelaxNGSchemaParseContext::new(document);

        Ok(Self {
            grammar: context.parse_relaxng(handler)?,
        })
    }
}

pub struct RelaxNGSchemaParseContext {
    document: Document,
    external_ref_loop_guard: HashSet<URIString>,
    include_loop_guard: HashSet<URIString>,
    /// `context_node_stack` is used to propagate namespace context to included external content
    /// by `externalRef` or `include` element.  \
    /// Because namespace resolution of QName prefix are occured after 7.11 QName, that is performed
    /// after 7.7 `externalRef` element and 7.8 `include` element, the namespace context of the
    /// original document should be propagate to included external document.  \
    /// Each `externalRef` and `include` elements should be appended to this stack.
    context_node_stack: Vec<Element>,
    datatype_libraries: RelaxNGDatatypeLibraries,
    defines: HashMap<String, Element>,
    last_error: Result<(), XMLError>,
}

impl RelaxNGSchemaParseContext {
    fn new(document: Document) -> Self {
        Self {
            document,
            external_ref_loop_guard: HashSet::new(),
            include_loop_guard: HashSet::new(),
            context_node_stack: vec![],
            datatype_libraries: RelaxNGDatatypeLibraries::default(),
            defines: HashMap::new(),
            last_error: Ok(()),
        }
    }

    fn parse_relaxng<Handler: SAXHandler>(
        &mut self,
        mut handler: Handler,
    ) -> Result<RelaxNGGrammar, XMLError> {
        let Some(mut document_element) = self.document.document_element() else {
            return Err(XMLError::RngParseUnknownError);
        };
        self.external_ref_loop_guard.clear();
        self.include_loop_guard.clear();
        self.context_node_stack.clear();
        self.defines.clear();
        self.last_error = Ok(());
        self.handle_pattern(&mut document_element, &mut handler, Cow::Borrowed(""))?;
        self.last_error.clone()?;
        let mut grammar = self
            .document
            .create_element("grammar", Some(XML_RELAX_NG_NAMESPACE.into()))?;
        let mut start = self
            .document
            .create_element("start", Some(XML_RELAX_NG_NAMESPACE.into()))?;
        start.append_child(document_element)?;
        grammar.append_child(start)?;
        self.document.append_child(&grammar)?;
        // ISO/IEC 19757-2:2008 7.18 `combine` attribute
        let grammars = grammar
            .get_elements_by_expanded_name("grammar", Some(XML_RELAX_NG_NAMESPACE))
            .collect::<Vec<_>>();
        for mut grammar in grammars {
            self.bundle_start_and_define(&mut grammar, &mut handler)?;
        }
        self.last_error.clone()?;
        // ISO/IEC 19757-2:2008 7.19 `grammar` element
        let mut num_define = 0;
        self.flatten_grammar(
            &mut grammar,
            &mut handler,
            &mut HashMap::new(),
            &mut num_define,
        )?;
        self.last_error.clone()?;
        // ISO/IEC 19757-2:2008 7.20 `define` and `ref` elements
        self.remove_unreachable_define(&mut grammar)?;
        self.normalize_define(&mut grammar.clone(), &mut grammar, &mut num_define)?;
        self.normalize_ref(&mut grammar, &mut handler, false, false)?;
        self.last_error.clone()?;
        for (_, mut define) in self.defines.extract_if(|_, define| {
            define
                .first_child()
                .and_then(|ch| ch.as_element())
                .is_none_or(|elem| elem.local_name().as_ref() != "element")
        }) {
            define.detach()?;
        }
        // ISO/IEC 19757-2:2008 7.21 `notAllowed` element
        let mut removable_except = vec![];
        normalize_not_allowed(&mut grammar, &mut removable_except)?;
        self.last_error.clone()?;
        for mut except in removable_except {
            except.detach()?;
        }
        let mut memo = HashSet::new();
        let mut next = vec![
            grammar
                .first_child()
                .and_then(|ch| ch.as_element())
                .ok_or(XMLError::RngParseUnknownError)?,
        ];
        assert_eq!(next[0].local_name().as_ref(), "start");
        while let Some(start) = next.pop() {
            let mut children = start.first_child();
            while let Some(child) = children {
                if let Some(name) = child
                    .as_element()
                    .filter(|elem| elem.local_name().as_ref() == "ref")
                    .and_then(|elem| elem.get_attribute("name", None))
                    && let Some(define) = self.defines.get(&name).cloned()
                    && memo.insert(name)
                {
                    next.push(define);
                }

                if let Some(first) = child.first_child() {
                    children = Some(first);
                } else if let Some(next) = child.next_sibling() {
                    children = Some(next);
                } else {
                    children = None;
                    let mut now = child.clone();
                    while let Some(parent) = now.parent_node() {
                        if parent.is_same_node(&start) {
                            break;
                        } else if let Some(next) = parent.next_sibling() {
                            children = Some(next);
                            break;
                        }
                        now = parent.into();
                    }
                }
            }
        }
        self.defines.shrink_to_fit();
        for (_, mut define) in self.defines.extract_if(|key, _| !memo.contains(key)) {
            define.detach()?;
        }
        // ISO/IEC 19757-2:2008 7.22 `empty` element
        normalize_empty(&mut grammar)?;
        self.verify_prohibited_paths(&grammar, &mut HashSet::new(), &mut handler)?;
        self.last_error.clone()?;
        let mut grammar = RelaxNGGrammar::try_from(grammar)?;
        grammar.libraries = self.datatype_libraries.clone();
        Ok(grammar)
    }

    /// `ns_context` is used to propagate `ns` attribute value to included external content
    /// by `externalRef` or `include` element.  \
    /// This should be updated by each elements with `ns` attribute.
    fn handle_pattern<Handler: SAXHandler>(
        &mut self,
        element: &mut Element,
        handler: &mut Handler,
        mut ns_context: Cow<'_, str>,
    ) -> Result<(), XMLError> {
        if element.namespace_name().as_deref() != Some(XML_RELAX_NG_NAMESPACE) {
            return Err(XMLError::RngParseUnacceptablePattern);
        }

        if let Some(ns) = element.get_attribute("ns", None) {
            ns_context = Cow::Owned(ns);
        }

        match element.local_name().as_ref() {
            "value" => {
                // Only elements that do not have string children can have foreign elements as children.
                // `value` cannot have foreign elements as children.
                let mut children = element.first_child();
                while let Some(mut child) = children {
                    children = child.next_sibling();

                    match child.node_type() {
                        NodeType::CDATASection | NodeType::Text => {}
                        NodeType::Comment | NodeType::ProcessingInstruction => {
                            child.detach()?;
                        }
                        _ => {
                            fatal_error!(
                                self,
                                handler,
                                RngParseUnacceptablePattern,
                                "'value' pattern must not have any data other than text, comment or pi.",
                            );
                            child.detach()?;
                        }
                    }
                }
                remove_foreign_attribute(element)?;
                // The child string of `value` must not be removed, even if it contains only whitespaces.
                // (ISO/IEC 19757-2:2008 7.3 Whitespace)

                if let Some(ty) = element.remove_attribute("type", None) {
                    element.set_attribute(
                        "type",
                        None,
                        Some(ty.trim_matches(|c| XMLVersion::default().is_whitespace(c))),
                    )?;
                } else {
                    // ISO/IEC 19757-2:2008 7.5 `type` attribute of `value` element
                    element.set_attribute("type", None, Some("token"))?;
                    element.remove_attribute("datatypeLibrary", None);
                    element.set_attribute("datatypeLibrary", None, Some(""))?;
                }

                self.check_attribute_constraint(element, handler, [], [("type", validate_ncname)])?;

                // ISO/IEC 19757-2:2008 7.10 `ns` attribute
                if !element.has_attribute("ns", None) {
                    element.set_attribute("ns", None, Some(&ns_context))?;
                }

                let type_name = element
                    .get_attribute("type", None)
                    .ok_or(XMLError::RngParseUnknownError)?;
                let datatype_library = element
                    .get_attribute("datatypeLibrary", None)
                    .ok_or(XMLError::RngParseUnknownError)
                    .and_then(|uri| Ok(URIString::parse(uri)?))?;
                // ISO/IEC 19757-2:2008 7.17 Constraints
                if let Some(library) = self.datatype_libraries.get(&datatype_library) {
                    if !library.contains(&type_name) {
                        fatal_error!(
                            self,
                            handler,
                            RngParseUnresolvableDatatypeLibrary,
                            "The type '{}' of the datatype library '{}' is unresolvabale.",
                            type_name,
                            datatype_library
                                .as_unescaped_str()
                                .as_deref()
                                .unwrap_or(datatype_library.as_escaped_str())
                        );
                    }
                } else {
                    fatal_error!(
                        self,
                        handler,
                        RngParseUnresolvableDatatypeLibrary,
                        "The datatype library '{}' is unresolvabale.",
                        datatype_library
                            .as_unescaped_str()
                            .as_deref()
                            .unwrap_or(datatype_library.as_escaped_str())
                    );
                }
            }
            local_name => {
                remove_foreign_element_child(element)?;
                remove_foreign_attribute(element)?;
                remove_whitespace_child(element)?;
                match local_name {
                    "element" => {
                        let mut children = element.first_child();

                        // ISO/IEC 19757-2:2008 7.9 `name` attribute of `element` and `attribute` elements
                        if let Some(name) = element.remove_attribute("name", None) {
                            let name =
                                name.trim_matches(|c| XMLVersion::default().is_whitespace(c));
                            validate_qname(name)?;
                            let document = element.owner_document();
                            let mut name_element = document
                                .create_element("name", Some(XML_RELAX_NG_NAMESPACE.into()))?;
                            name_element.append_child(document.create_text(name))?;
                            if let Some(first) = children.as_mut() {
                                first.insert_previous_sibling(&name_element)?;
                            } else {
                                element.append_child(&name_element)?;
                            }
                            children = Some(name_element.into());
                        }

                        // 'name' has already been removed and therefore does not exist.
                        self.check_attribute_constraint(element, handler, [], [])?;

                        if let Some(mut child) =
                            children.as_ref().and_then(|child| child.as_element())
                        {
                            children = child.next_sibling();

                            self.handle_name_class(
                                &mut child,
                                handler,
                                ns_context.as_ref().into(),
                            )?;
                        } else {
                            fatal_error!(
                                self,
                                handler,
                                RngParseUnacceptablePattern,
                                "'element' without an attribute 'name' must have a nameClass as child, but not found."
                            );
                        }

                        if children.is_none() {
                            fatal_error!(
                                self,
                                handler,
                                RngParseUnacceptablePattern,
                                "'element' must have a pattern as child, but not found."
                            );
                        }

                        while let Some(mut child) = children {
                            children = child.next_sibling();

                            if let Some(mut pattern) = child.as_element() {
                                self.handle_pattern(
                                    &mut pattern,
                                    handler,
                                    ns_context.as_ref().into(),
                                )?;
                            } else {
                                fatal_error!(
                                    self,
                                    handler,
                                    RngParseUnacceptablePattern,
                                    "A {:?} must not be present at this position as a child of 'element'.",
                                    child.node_type()
                                );
                                child.detach()?;
                            }
                        }

                        // ISO/IEC 19757-2:2008 7.13 Number of child elements
                        group_children(element)?;
                    }
                    "attribute" => {
                        let mut children = element.first_child();

                        // ISO/IEC 19757-2:2008 7.9 `name` attribute of `element` and `attribute` elements
                        if let Some(name) = element.remove_attribute("name", None) {
                            let name =
                                name.trim_matches(|c| XMLVersion::default().is_whitespace(c));
                            validate_qname(name)?;
                            let document = element.owner_document();
                            let mut name_element = document
                                .create_element("name", Some(XML_RELAX_NG_NAMESPACE.into()))?;
                            name_element.append_child(document.create_text(name))?;

                            if !element.has_attribute("ns", None) {
                                name_element.set_attribute("ns", None, Some(""))?;
                            }

                            if let Some(first) = children.as_mut() {
                                first.insert_previous_sibling(&name_element)?;
                            } else {
                                element.append_child(&name_element)?;
                            }
                            children = Some(name_element.into());
                        }

                        // 'name' has already been removed and therefore does not exist.
                        self.check_attribute_constraint(element, handler, [], [])?;

                        if let Some(mut child) =
                            children.as_ref().and_then(|child| child.as_element())
                        {
                            children = child.next_sibling();

                            self.handle_name_class(
                                &mut child,
                                handler,
                                ns_context.as_ref().into(),
                            )?;
                        } else {
                            fatal_error!(
                                self,
                                handler,
                                RngParseUnacceptablePattern,
                                "'attribute' without an attribute 'name' must have a nameClass as child, but not found."
                            );
                        }

                        if children.is_none() {
                            element.append_child(
                                self.document
                                    .create_element("text", Some(XML_RELAX_NG_NAMESPACE.into()))?,
                            )?;
                        } else {
                            while let Some(mut child) = children {
                                children = child.next_sibling();

                                if let Some(mut pattern) = child.as_element() {
                                    self.handle_pattern(
                                        &mut pattern,
                                        handler,
                                        ns_context.as_ref().into(),
                                    )?;
                                    break;
                                } else {
                                    fatal_error!(
                                        self,
                                        handler,
                                        RngParseUnacceptablePattern,
                                        "A {:?} must not be present at this position as a child of 'attribute'.",
                                        child.node_type()
                                    );
                                    child.detach()?;
                                }
                            }

                            if children.is_some() {
                                fatal_error!(
                                    self,
                                    handler,
                                    RngParseUnacceptablePattern,
                                    "'attribute' pattern cannot have more then only one pattern children."
                                );
                                while let Some(mut child) = children {
                                    children = child.next_sibling();
                                    child.detach()?;
                                }
                            }
                        }

                        // ISO/IEC 19757-2:2008 7.13 Number of child elements
                        group_children(element)?;
                        // ISO/IEC 19757-2:2008 7.17 Constraints
                        self.check_attribute_name_constraint(element, handler)?;
                    }
                    "group" | "interleave" | "choice" | "optional" | "zeroOrMore" | "oneOrMore"
                    | "list" | "mixed" => {
                        self.check_attribute_constraint(element, handler, [], [])?;

                        let mut children = element.first_child();
                        if children.is_none() {
                            fatal_error!(
                                self,
                                handler,
                                RngParseUnacceptablePattern,
                                "'{}' must have a pattern as child, but not found.",
                                local_name
                            );
                        }

                        while let Some(mut child) = children {
                            children = child.next_sibling();

                            if let Some(mut pattern) = child.as_element() {
                                self.handle_pattern(
                                    &mut pattern,
                                    handler,
                                    ns_context.as_ref().into(),
                                )?;
                            } else {
                                fatal_error!(
                                    self,
                                    handler,
                                    RngParseUnacceptablePattern,
                                    "A {:?} must not be present at this position as a child of '{}'.",
                                    child.node_type(),
                                    local_name
                                );
                                child.detach()?;
                            }
                        }

                        // ISO/IEC 19757-2:2008 7.13 Number of child elements
                        group_children(element)?;
                        match local_name {
                            // ISO/IEC 19757-2:2008 7.14 mixed element
                            "mixed" => decompose_mixed(element)?,
                            // ISO/IEC 19757-2:2008 7.15 optional element
                            "optional" => decompose_optional(element)?,
                            // ISO/IEC 19757-2:2008 7.16 zeroOrMore element
                            "zeroOrMore" => decompose_zero_or_more(element)?,
                            _ => {}
                        }
                    }
                    "ref" | "parentRef" => {
                        self.check_attribute_constraint(
                            element,
                            handler,
                            [("name", validate_ncname)],
                            [],
                        )?;

                        let mut children = element.first_child();
                        if children.is_some() {
                            error!(
                                self,
                                handler,
                                RngParseUnacceptablePattern,
                                "'{}' must not have a pattern as child.",
                                local_name
                            );

                            while let Some(mut child) = children {
                                children = child.next_sibling();
                                child.detach()?;
                            }
                        }
                    }
                    "empty" | "text" | "notAllowed" => {
                        self.check_attribute_constraint(element, handler, [], [])?;

                        let mut children = element.first_child();
                        if children.is_some() {
                            error!(
                                self,
                                handler,
                                RngParseUnacceptablePattern,
                                "'{}' must not have a pattern as child.",
                                local_name
                            );

                            while let Some(mut child) = children {
                                children = child.next_sibling();
                                child.detach()?;
                            }
                        }
                    }
                    "data" => {
                        if let Some(ty) = element.remove_attribute("type", None) {
                            element.set_attribute(
                                "type",
                                None,
                                Some(ty.trim_matches(|c| XMLVersion::default().is_whitespace(c))),
                            )?;
                        }

                        self.check_attribute_constraint(
                            element,
                            handler,
                            [("type", validate_ncname)],
                            [],
                        )?;

                        let mut params = HashMap::new();
                        let mut children = element.first_child();
                        // check 'param'
                        while let Some(child) = children.as_mut() {
                            if let Some(mut element) = child.as_element() {
                                if element.local_name().as_ref() == "param" {
                                    self.handle_param(&mut element, handler)?;
                                    let name = element
                                        .get_attribute("name", None)
                                        .ok_or(XMLError::RngParseUnknownError)?;
                                    let value = element
                                        .first_child()
                                        .and_then(|ch| ch.as_text())
                                        .map(|text| text.data().to_string())
                                        .unwrap_or_default();
                                    params.insert(name, value);
                                } else {
                                    break;
                                }
                            } else {
                                fatal_error!(
                                    self,
                                    handler,
                                    RngParseUnacceptablePattern,
                                    "A {:?} must not be present at this position as a child of '{}'.",
                                    child.node_type(),
                                    local_name
                                );
                                child.detach()?;
                            }

                            children = child.next_sibling();
                        }

                        // check 'exceptPattern'
                        while let Some(child) = children.as_mut() {
                            if let Some(mut element) = child.as_element() {
                                if element.local_name().as_ref() == "except" {
                                    self.handle_except_pattern(
                                        &mut element,
                                        handler,
                                        ns_context.as_ref().into(),
                                    )?;
                                    children = child.next_sibling();
                                    break;
                                } else {
                                    fatal_error!(
                                        self,
                                        handler,
                                        RngParseUnacceptablePattern,
                                        "'{}' must not be present at this position as a child of '{}'.",
                                        element.name(),
                                        local_name
                                    );
                                    element.detach()?;
                                }
                            } else {
                                fatal_error!(
                                    self,
                                    handler,
                                    RngParseUnacceptablePattern,
                                    "A {:?} must not be present at this position as a child of '{}'.",
                                    child.node_type(),
                                    local_name
                                );
                                child.detach()?;
                            }

                            children = child.next_sibling();
                        }

                        // Remove unnecessary elements after reporting errros.
                        while let Some(mut child) = children {
                            children = child.next_sibling();
                            if let Some(element) = child.as_element() {
                                fatal_error!(
                                    self,
                                    handler,
                                    RngParseUnacceptablePattern,
                                    "'{}' must not be present at this position as a child of '{}'.",
                                    element.name(),
                                    local_name
                                );
                            } else {
                                fatal_error!(
                                    self,
                                    handler,
                                    RngParseUnacceptablePattern,
                                    "A {:?} must not be present at this position as a child of '{}'.",
                                    child.node_type(),
                                    local_name
                                );
                            }
                            child.detach()?;
                        }

                        let type_name = element
                            .get_attribute("type", None)
                            .ok_or(XMLError::RngParseUnknownError)?;
                        let datatype_library = element
                            .get_attribute("datatypeLibrary", None)
                            .ok_or(XMLError::RngParseUnknownError)
                            .and_then(|uri| Ok(URIString::parse(uri)?))?;
                        // ISO/IEC 19757-2:2008 7.17 Constraints
                        if let Some(library) = self.datatype_libraries.get(&datatype_library) {
                            if !library.contains(&type_name) {
                                fatal_error!(
                                    self,
                                    handler,
                                    RngParseUnresolvableDatatypeLibrary,
                                    "The type '{}' of the datatype library '{}' is unresolvabale.",
                                    type_name,
                                    datatype_library
                                        .as_unescaped_str()
                                        .as_deref()
                                        .unwrap_or(datatype_library.as_escaped_str())
                                );
                            } else if library
                                .validate_params(&type_name, &params)
                                .is_none_or(|b| !b)
                            {
                                fatal_error!(
                                    self,
                                    handler,
                                    RngParseUnresolvableDatatypeLibrary,
                                    "The params for the type '{}' of the datatype library '{}' is invalid.",
                                    type_name,
                                    datatype_library
                                        .as_unescaped_str()
                                        .as_deref()
                                        .unwrap_or(datatype_library.as_escaped_str())
                                );
                            }
                        } else {
                            fatal_error!(
                                self,
                                handler,
                                RngParseUnresolvableDatatypeLibrary,
                                "The datatype library '{}' is unresolvabale.",
                                datatype_library
                                    .as_unescaped_str()
                                    .as_deref()
                                    .unwrap_or(datatype_library.as_escaped_str())
                            );
                        }
                    }
                    "externalRef" => {
                        self.check_attribute_constraint(
                            element,
                            handler,
                            [("href", validate_uri)],
                            [],
                        )?;

                        let mut children = element.first_child();
                        if children.is_some() {
                            error!(
                                self,
                                handler,
                                RngParseUnacceptablePattern,
                                "'{}' must not have a pattern as child.",
                                local_name
                            );

                            while let Some(mut child) = children {
                                children = child.next_sibling();
                                child.detach()?;
                            }
                        }

                        // ISO/IEC 19757-2:2008 7.7 `externalRef` element
                        // `externalRef` is replaced with included pattern,
                        // so replace `element` with the returned element.
                        *element =
                            self.load_external_ref(element, handler, ns_context.as_ref().into())?;
                    }
                    "grammar" => {
                        self.check_attribute_constraint(element, handler, [], [])?;

                        let mut children = element.first_child();
                        while let Some(mut child) = children {
                            children = child.next_sibling();

                            if let Some(mut element) = child.as_element() {
                                self.handle_grammar_content(
                                    &mut element,
                                    handler,
                                    ns_context.as_ref().into(),
                                )?;
                            } else {
                                error!(
                                    self,
                                    handler,
                                    RngParseUnacceptablePattern,
                                    "A {:?} must not be present at this position as a child of '{}'.",
                                    child.node_type(),
                                    local_name
                                );
                                child.detach()?;
                            }
                        }
                    }
                    _ => {
                        fatal_error!(
                            self,
                            handler,
                            RngParseUnacceptablePattern,
                            "The element '{}' does not match to a pattern element.",
                            local_name
                        );
                        return Err(XMLError::RngParseUnacceptablePattern);
                    }
                }

                if local_name != "data" {
                    // ISO/IEC 19757-2:2008 7.4 `datatypeLibrary` attribute
                    element.remove_attribute("datatypeLibrary", None);
                }
                // ISO/IEC 19757-2:2008 7.10 `ns` attribute
                element.remove_attribute("ns", None);
            }
        }

        // 'xml:base' is no longer necessary.
        element.remove_attribute("base", Some(XML_XML_NAMESPACE));
        Ok(())
    }

    fn handle_param<Handler: ErrorHandler>(
        &mut self,
        element: &mut Element,
        handler: &mut Handler,
    ) -> Result<(), XMLError> {
        if element.namespace_name().as_deref() != Some(XML_RELAX_NG_NAMESPACE) {
            return Err(XMLError::RngParseUnacceptablePattern);
        }

        if element.local_name().as_ref() != "param" {
            fatal_error!(
                self,
                handler,
                RngParseUnacceptablePattern,
                "'param' is expected, but '{}' is found.",
                element.name()
            );
            return Err(RngParseUnacceptablePattern);
        }

        // Only elements that do not have string children can have foreign elements as children.
        // `param` cannot have foreign elements as children.
        let mut children = element.first_child();
        while let Some(mut child) = children {
            children = child.next_sibling();

            match child.node_type() {
                NodeType::CDATASection | NodeType::Text => {}
                NodeType::Comment | NodeType::ProcessingInstruction => {
                    child.detach()?;
                }
                _ => {
                    fatal_error!(
                        self,
                        handler,
                        RngParseUnacceptablePattern,
                        "'value' pattern must not have any data other than text, comment or pi.",
                    );
                    child.detach()?;
                }
            }
        }
        remove_foreign_attribute(element)?;
        // The child string of `param` must not be removed, even if it contains only whitespaces.
        // (ISO/IEC 19757-2:2008 7.3 Whitespace)

        self.check_attribute_constraint(element, handler, [("name", validate_ncname)], [])?;

        self.normalize_string_child(element, handler, false)?;

        // ISO/IEC 19757-2:2008 7.4 `datatypeLibrary` attribute
        element.remove_attribute("datatypeLibrary", None);
        // ISO/IEC 19757-2:2008 7.10 `ns` attribute
        element.remove_attribute("ns", None);

        Ok(())
    }

    fn handle_except_pattern<Handler: SAXHandler>(
        &mut self,
        element: &mut Element,
        handler: &mut Handler,
        mut ns_context: Cow<'_, str>,
    ) -> Result<(), XMLError> {
        if element.namespace_name().as_deref() != Some(XML_RELAX_NG_NAMESPACE) {
            return Err(XMLError::RngParseUnacceptablePattern);
        }

        if element.local_name().as_ref() != "except" {
            fatal_error!(
                self,
                handler,
                RngParseUnacceptablePattern,
                "'except' is expected, but '{}' is found.",
                element.name()
            );
            return Err(RngParseUnacceptablePattern);
        }

        if let Some(ns) = element.get_attribute("ns", None) {
            ns_context = Cow::Owned(ns);
        }

        remove_foreign_element_child(element)?;
        remove_foreign_attribute(element)?;
        remove_whitespace_child(element)?;

        self.check_attribute_constraint(element, handler, [], [])?;

        let mut children = element.first_child();
        if children.is_none() {
            fatal_error!(
                self,
                handler,
                RngParseUnacceptablePattern,
                "'except' must have at least one pattern as child, but not found."
            );
        }

        while let Some(mut child) = children {
            children = child.next_sibling();

            if let Some(mut element) = child.as_element() {
                self.handle_pattern(&mut element, handler, ns_context.as_ref().into())?;
            } else {
                fatal_error!(
                    self,
                    handler,
                    RngParseUnacceptablePattern,
                    "A {:?} must not be present at this position as a child of 'except'.",
                    child.node_type()
                );
                child.detach()?;
            }
        }

        // ISO/IEC 19757-2:2008 7.4 `datatypeLibrary` attribute
        element.remove_attribute("datatypeLibrary", None);
        // ISO/IEC 19757-2:2008 7.10 `ns` attribute
        element.remove_attribute("ns", None);
        // ISO/IEC 19757-2:2008 7.13 Number of child elements
        group_children(element)?;

        // 'xml:base' is no longer necessary.
        element.remove_attribute("base", Some(XML_XML_NAMESPACE));
        Ok(())
    }

    fn handle_grammar_content<Handler: SAXHandler>(
        &mut self,
        element: &mut Element,
        handler: &mut Handler,
        mut ns_context: Cow<'_, str>,
    ) -> Result<(), XMLError> {
        if element.namespace_name().as_deref() != Some(XML_RELAX_NG_NAMESPACE) {
            return Err(XMLError::RngParseUnacceptablePattern);
        }

        match element.local_name().as_ref() {
            "start" => self.handle_start(element, handler, ns_context),
            "define" => self.handle_define(element, handler, ns_context),
            "div" => {
                if let Some(ns) = element.get_attribute("ns", None) {
                    ns_context = Cow::Owned(ns);
                }

                remove_foreign_element_child(element)?;
                remove_foreign_attribute(element)?;
                remove_whitespace_child(element)?;

                self.check_attribute_constraint(element, handler, [], [])?;

                let mut children = element.first_child();
                while let Some(mut child) = children {
                    children = child.next_sibling();

                    if let Some(mut child) = child.as_element() {
                        self.handle_grammar_content(
                            &mut child,
                            handler,
                            ns_context.as_ref().into(),
                        )?;
                    } else {
                        fatal_error!(
                            self,
                            handler,
                            RngParseUnacceptablePattern,
                            "A {:?} must not be present at this position as a child of 'div'.",
                            child.node_type()
                        );
                        child.detach()?;
                    }
                }

                // ISO/IEC 19757-2:2008 7.4 `datatypeLibrary` attribute
                element.remove_attribute("datatypeLibrary", None);
                // ISO/IEC 19757-2:2008 7.10 `ns` attribute
                element.remove_attribute("ns", None);
                // ISO/IEC 19757-2:2008 7.12 `div` element
                flatten_div(element)?;

                Ok(())
            }
            "include" => {
                if let Some(ns) = element.get_attribute("ns", None) {
                    ns_context = Cow::Owned(ns);
                }

                remove_foreign_element_child(element)?;
                remove_foreign_attribute(element)?;
                remove_whitespace_child(element)?;

                self.check_attribute_constraint(element, handler, [("href", validate_uri)], [])?;

                let mut children = element.first_child();
                while let Some(mut child) = children {
                    children = child.next_sibling();

                    if let Some(mut child) = child.as_element() {
                        self.handle_include_content(
                            &mut child,
                            handler,
                            ns_context.as_ref().into(),
                        )?;
                    } else {
                        fatal_error!(
                            self,
                            handler,
                            RngParseUnacceptablePattern,
                            "A {:?} must not be present at this position as a child of 'include'.",
                            child.node_type()
                        );
                        child.detach()?;
                    }
                }

                // ISO/IEC 19757-2:2008 7.4 `datatypeLibrary` attribute
                element.remove_attribute("datatypeLibrary", None);

                // ISO/IEC 19757-2:2008 7.8 `include` element
                // `include` is replaced with `div`, so replace `element` with the returned `div`.
                *element = self.load_include(element, handler, ns_context.as_ref().into())?;

                // ISO/IEC 19757-2:2008 7.10 `ns` attribute
                element.remove_attribute("ns", None);
                // ISO/IEC 19757-2:2008 7.12 `div` element
                // In Chapter 7.8, `include` is replaced with `div`, so it is flattened here.
                flatten_div(element)?;
                Ok(())
            }
            _ => {
                fatal_error!(
                    self,
                    handler,
                    RngParseUnacceptablePattern,
                    "The element '{}' does not match to a grammarContent element.",
                    element.local_name()
                );
                Err(XMLError::RngParseUnacceptablePattern)
            }
        }
    }

    fn handle_include_content<Handler: SAXHandler>(
        &mut self,
        element: &mut Element,
        handler: &mut Handler,
        mut ns_context: Cow<'_, str>,
    ) -> Result<(), XMLError> {
        if element.namespace_name().as_deref() != Some(XML_RELAX_NG_NAMESPACE) {
            return Err(XMLError::RngParseUnacceptablePattern);
        }

        match element.local_name().as_ref() {
            "start" => self.handle_start(element, handler, ns_context),
            "define" => self.handle_define(element, handler, ns_context),
            "div" => {
                if let Some(ns) = element.get_attribute("ns", None) {
                    ns_context = Cow::Owned(ns);
                }

                remove_foreign_element_child(element)?;
                remove_foreign_attribute(element)?;
                remove_whitespace_child(element)?;

                self.check_attribute_constraint(element, handler, [], [])?;

                let mut children = element.first_child();
                while let Some(mut child) = children {
                    children = child.next_sibling();

                    if let Some(mut child) = child.as_element() {
                        self.handle_include_content(
                            &mut child,
                            handler,
                            ns_context.as_ref().into(),
                        )?;
                    } else {
                        fatal_error!(
                            self,
                            handler,
                            RngParseUnacceptablePattern,
                            "A {:?} must not be present at this position as a child of 'div'.",
                            child.node_type()
                        );
                        child.detach()?;
                    }
                }

                // ISO/IEC 19757-2:2008 7.4 `datatypeLibrary` attribute
                element.remove_attribute("datatypeLibrary", None);
                // ISO/IEC 19757-2:2008 7.10 `ns` attribute
                element.remove_attribute("ns", None);
                // ISO/IEC 19757-2:2008 7.12 `div` element
                flatten_div(element)?;

                Ok(())
            }
            _ => {
                fatal_error!(
                    self,
                    handler,
                    RngParseUnacceptablePattern,
                    "The element '{}' does not match to a includeContent element.",
                    element.local_name()
                );
                Err(XMLError::RngParseUnacceptablePattern)
            }
        }
    }

    fn handle_start<Handler: SAXHandler>(
        &mut self,
        element: &mut Element,
        handler: &mut Handler,
        mut ns_context: Cow<'_, str>,
    ) -> Result<(), XMLError> {
        if element.namespace_name().as_deref() != Some(XML_RELAX_NG_NAMESPACE) {
            return Err(XMLError::RngParseUnacceptablePattern);
        }

        if element.local_name().as_ref() != "start" {
            fatal_error!(
                self,
                handler,
                RngParseUnacceptablePattern,
                "'start' is expected, but '{}' is found.",
                element.name()
            );
            return Err(RngParseUnacceptablePattern);
        }

        if let Some(ns) = element.get_attribute("ns", None) {
            ns_context = Cow::Owned(ns);
        }

        remove_foreign_element_child(element)?;
        remove_foreign_attribute(element)?;
        remove_whitespace_child(element)?;

        self.check_attribute_constraint(element, handler, [], [("combine", validate_combine)])?;

        let mut children = element.first_child();
        if children.is_none() {
            fatal_error!(
                self,
                handler,
                RngParseUnacceptablePattern,
                "'start' must have at least one pattern as child, but not found."
            );
        }

        while let Some(mut child) = children {
            children = child.next_sibling();

            if let Some(mut element) = child.as_element() {
                self.handle_pattern(&mut element, handler, ns_context.as_ref().into())?;
                break;
            } else {
                fatal_error!(
                    self,
                    handler,
                    RngParseUnacceptablePattern,
                    "A {:?} must not be present at this position as a child of 'start'.",
                    child.node_type()
                );
                child.detach()?;
            }
        }

        if children.is_some() {
            fatal_error!(
                self,
                handler,
                RngParseUnacceptablePattern,
                "Just only one child is allowed on 'start', but more than two children are found."
            );
            while let Some(mut child) = children {
                children = child.next_sibling();
                child.detach()?;
            }
        }

        // ISO/IEC 19757-2:2008 7.4 `datatypeLibrary` attribute
        element.remove_attribute("datatypeLibrary", None);
        // ISO/IEC 19757-2:2008 7.10 `ns` attribute
        element.remove_attribute("ns", None);

        Ok(())
    }

    fn handle_define<Handler: SAXHandler>(
        &mut self,
        element: &mut Element,
        handler: &mut Handler,
        mut ns_context: Cow<'_, str>,
    ) -> Result<(), XMLError> {
        if element.namespace_name().as_deref() != Some(XML_RELAX_NG_NAMESPACE) {
            return Err(XMLError::RngParseUnacceptablePattern);
        }

        if element.local_name().as_ref() != "define" {
            fatal_error!(
                self,
                handler,
                RngParseUnacceptablePattern,
                "'define' is expected, but '{}' is found.",
                element.name()
            );
            return Err(RngParseUnacceptablePattern);
        }

        if let Some(ns) = element.get_attribute("ns", None) {
            ns_context = Cow::Owned(ns);
        }

        remove_foreign_element_child(element)?;
        remove_foreign_attribute(element)?;
        remove_whitespace_child(element)?;

        self.check_attribute_constraint(
            element,
            handler,
            [("name", validate_ncname)],
            [("combine", validate_combine)],
        )?;

        let mut children = element.first_child();
        if children.is_none() {
            fatal_error!(
                self,
                handler,
                RngParseUnacceptablePattern,
                "'define' must have at least one pattern as child, but not found."
            );
        }

        while let Some(mut child) = children {
            children = child.next_sibling();

            if let Some(mut element) = child.as_element() {
                self.handle_pattern(&mut element, handler, ns_context.as_ref().into())?;
            } else {
                fatal_error!(
                    self,
                    handler,
                    RngParseUnacceptablePattern,
                    "A {:?} must not be present at this position as a child of 'define'.",
                    child.node_type()
                );
                child.detach()?;
            }
        }

        // ISO/IEC 19757-2:2008 7.4 `datatypeLibrary` attribute
        element.remove_attribute("datatypeLibrary", None);
        // ISO/IEC 19757-2:2008 7.10 `ns` attribute
        element.remove_attribute("ns", None);
        // ISO/IEC 19757-2:2008 7.13 Number of child elements
        group_children(element)?;

        Ok(())
    }

    fn handle_name_class<Handler: ErrorHandler>(
        &mut self,
        element: &mut Element,
        handler: &mut Handler,
        mut ns_context: Cow<'_, str>,
    ) -> Result<(), XMLError> {
        if element.namespace_name().as_deref() != Some(XML_RELAX_NG_NAMESPACE) {
            return Err(XMLError::RngParseUnacceptablePattern);
        }

        match element.local_name().as_ref() {
            "name" => {
                // Only elements that do not have string children can have foreign elements as children.
                // `name` cannot have foreign elements as children.
                let mut children = element.first_child();
                while let Some(mut child) = children {
                    children = child.next_sibling();

                    match child.node_type() {
                        NodeType::CDATASection | NodeType::Text => {}
                        NodeType::Comment | NodeType::ProcessingInstruction => {
                            child.detach()?;
                        }
                        _ => {
                            fatal_error!(
                                self,
                                handler,
                                RngParseUnacceptablePattern,
                                "'value' pattern must not have any data other than text, comment or pi.",
                            );
                            child.detach()?;
                        }
                    }
                }
                remove_foreign_attribute(element)?;

                self.check_attribute_constraint(element, handler, [], [])?;

                self.normalize_string_child(element, handler, true)?;

                // ISO/IEC 19757-2:2008 7.4 `datatypeLibrary` attribute
                element.remove_attribute("datatypeLibrary", None);

                // ISO/IEC 19757-2:2008 7.10 `ns` attribute
                if !element.has_attribute("ns", None) {
                    element.set_attribute("ns", None, Some(&ns_context))?;
                }

                // ISO/IEC 19757-2:2008 7.11 QNames
                if let Some(mut text_node) = element.first_child().and_then(|child| child.as_text())
                    && let text = text_node.data()
                    && let Some((prefix, local)) = text.split_once(':')
                {
                    let document = element.owner_document();
                    let local = document.create_text(local);
                    if let Some(namespace) = self.resolve_namespace_prefix(prefix, Some(element)) {
                        let namespace_name = namespace.namespace_name();
                        element.remove_attribute("ns", None);
                        element.set_attribute("ns", None, Some(&namespace_name))?;
                    } else {
                        fatal_error!(
                            self,
                            handler,
                            RngParseUnresolvableNamespacePrefix,
                            "The namespace prefix '{}' is unresolvable.",
                            prefix
                        );
                    }
                    drop(text);
                    text_node.detach()?;
                    element.append_child(local)?;
                }
            }
            local_name @ ("anyName" | "nsName") => {
                if let Some(ns) = element.get_attribute("ns", None) {
                    ns_context = Cow::Owned(ns);
                }

                remove_foreign_element_child(element)?;
                remove_foreign_attribute(element)?;
                remove_whitespace_child(element)?;

                self.check_attribute_constraint(element, handler, [], [])?;

                let mut children = element.first_child();
                while let Some(mut child) = children {
                    children = child.next_sibling();

                    if let Some(mut child) = child.as_element() {
                        self.handle_except_name_class(
                            &mut child,
                            handler,
                            ns_context.as_ref().into(),
                        )?;
                        break;
                    } else {
                        fatal_error!(
                            self,
                            handler,
                            RngParseUnacceptablePattern,
                            "A {:?} must not be present at this position as a child of 'define'.",
                            child.node_type()
                        );
                        child.detach()?;
                    }
                }

                if children.is_some() {
                    fatal_error!(
                        self,
                        handler,
                        RngParseUnacceptablePattern,
                        "At most one child is allowed on '{}', but more than two children are found.",
                        element.local_name()
                    );

                    while let Some(mut child) = children {
                        children = child.next_sibling();
                        child.detach()?;
                    }
                }

                // ISO/IEC 19757-2:2008 7.4 `datatypeLibrary` attribute
                element.remove_attribute("datatypeLibrary", None);

                // ISO/IEC 19757-2:2008 7.10 `ns` attribute
                if local_name == "nsName" {
                    if !element.has_attribute("ns", None) {
                        element.set_attribute("ns", None, Some(&ns_context))?;
                    }
                } else {
                    element.remove_attribute("ns", None);
                }

                // ISO/IEC 19757-2:2008 7.17 Constraints
                self.check_name_class_constraint(element, handler)?;
            }
            "choice" => {
                if let Some(ns) = element.get_attribute("ns", None) {
                    ns_context = Cow::Owned(ns);
                }

                remove_foreign_element_child(element)?;
                remove_foreign_attribute(element)?;
                remove_whitespace_child(element)?;

                self.check_attribute_constraint(element, handler, [], [])?;

                let mut children = element.first_child();
                while let Some(mut child) = children {
                    children = child.next_sibling();

                    if let Some(mut child) = child.as_element() {
                        self.handle_name_class(&mut child, handler, ns_context.as_ref().into())?;
                    } else {
                        fatal_error!(
                            self,
                            handler,
                            RngParseUnacceptablePattern,
                            "A {:?} must not be present at this position as a child of 'choice'.",
                            child.node_type()
                        );
                        child.detach()?;
                    }
                }

                // ISO/IEC 19757-2:2008 7.4 `datatypeLibrary` attribute
                element.remove_attribute("datatypeLibrary", None);
                // ISO/IEC 19757-2:2008 7.10 `ns` attribute
                element.remove_attribute("ns", None);
            }
            _ => {
                fatal_error!(
                    self,
                    handler,
                    RngParseUnacceptablePattern,
                    "The element '{}' does not match to a nameClass element.",
                    element.local_name()
                );
                return Err(XMLError::RngParseUnacceptablePattern);
            }
        }

        // 'xml:base' is no longer necessary.
        element.remove_attribute("base", Some(XML_XML_NAMESPACE));
        Ok(())
    }

    fn handle_except_name_class<Handler: ErrorHandler>(
        &mut self,
        element: &mut Element,
        handler: &mut Handler,
        mut ns_context: Cow<'_, str>,
    ) -> Result<(), XMLError> {
        if element.namespace_name().as_deref() != Some(XML_RELAX_NG_NAMESPACE) {
            return Err(XMLError::RngParseUnacceptablePattern);
        }

        if element.local_name().as_ref() != "except" {
            fatal_error!(
                self,
                handler,
                RngParseUnacceptablePattern,
                "'except' is expected, but '{}' is found.",
                element.name()
            );
            return Err(RngParseUnacceptablePattern);
        }

        if let Some(ns) = element.get_attribute("ns", None) {
            ns_context = Cow::Owned(ns);
        }

        remove_foreign_element_child(element)?;
        remove_foreign_attribute(element)?;
        remove_whitespace_child(element)?;

        self.check_attribute_constraint(element, handler, [], [])?;

        let mut children = element.first_child();
        if children.is_none() {
            fatal_error!(
                self,
                handler,
                RngParseUnacceptablePattern,
                "'except' must have at least one pattern as child, but not found."
            );
        }

        while let Some(mut child) = children {
            children = child.next_sibling();

            if let Some(mut element) = child.as_element() {
                self.handle_name_class(&mut element, handler, ns_context.as_ref().into())?;
            } else {
                fatal_error!(
                    self,
                    handler,
                    RngParseUnacceptablePattern,
                    "A {:?} must not be present at this position as a child of 'except'.",
                    child.node_type()
                );
                child.detach()?;
            }
        }

        // ISO/IEC 19757-2:2008 7.4 `datatypeLibrary` attribute
        element.remove_attribute("datatypeLibrary", None);
        // ISO/IEC 19757-2:2008 7.10 `ns` attribute
        element.remove_attribute("ns", None);
        // ISO/IEC 19757-2:2008 7.13 Number of child elements
        group_children(element)?;

        Ok(())
    }

    /// # Note
    /// The ISO specification permits constructing patterns from resources other than XML,
    /// but the current specification of this API does not support handling resources other than XML.
    ///
    /// # Reference
    /// - ISO/IEC 19757-2:2008 7.6 href attribute
    /// - ISO/IEC 19757-2:2008 7.7 externalRef element
    fn load_external_ref<Handler: SAXHandler>(
        &mut self,
        external_ref: &mut Element,
        handler: &mut Handler,
        mut ns_context: Cow<'_, str>,
    ) -> Result<Element, XMLError> {
        assert_eq!(
            external_ref.namespace_name().as_deref(),
            Some(XML_RELAX_NG_NAMESPACE)
        );
        assert_eq!(external_ref.local_name().as_ref(), "externalRef");

        let href = external_ref
            .get_attribute("href", None)
            .ok_or(XMLError::RngParseUnknownError)
            .and_then(|href| Ok(URIString::parse(href)?))?;
        // Since `external_ref` is a descendant of `Document`, `base_uri` will never fail.
        let mut href = external_ref
            .base_uri()
            .map(|base_uri| base_uri.resolve(&href))
            .unwrap();

        // ISO/IEC 19757-2:2008 7.6 href attribute
        if href.fragment().is_some() {
            error!(
                self,
                handler,
                RngParseHRefIncludeFragment,
                "The URI that refers to an XML external resource must not contain a fragment identifier."
            );
            // Recover by removing the fragment identifier.
            href = href.resolve(&URIString::parse("").unwrap());
        }

        if !self.external_ref_loop_guard.insert(href.clone()) {
            fatal_error!(
                self,
                handler,
                RngParseExternalRefLoop,
                "'externalRef' causes reference loop for '{}'.",
                href.as_unescaped_str()
                    .as_deref()
                    .unwrap_or(href.as_escaped_str())
            );
            return Err(XMLError::RngParseExternalRefLoop);
        }

        let mut reader = XMLReaderBuilder::new()
            .set_handler(TreeBuildHandler::with_handler(handler))
            .build();
        reader.parse_uri(href.as_ref(), None)?;
        let TreeBuildHandler {
            document, handler, ..
        } = reader.handler;
        let Some(mut document_element) = document.document_element() else {
            return Err(XMLError::RngParseExternalRefParseFailure);
        };

        if let Some(ns) = external_ref.get_attribute("ns", None) {
            ns_context = Cow::Owned(ns);
        }

        self.context_node_stack.push(external_ref.clone());
        self.handle_pattern(&mut document_element, handler, ns_context.as_ref().into())?;
        self.context_node_stack.pop();
        self.external_ref_loop_guard.remove(&href);
        document_element.detach()?;

        if let Some(ns) = external_ref.get_attribute("ns", None)
            && !document_element.has_attribute("ns", None)
        {
            document_element.set_attribute("ns", None, Some(&ns))?;
        }
        let mut pattern = external_ref.owner_document().import_node(document_element);
        external_ref.replace_subtree(&mut pattern)?;
        Ok(pattern)
    }

    /// # Note
    /// The ISO specification permits constructing patterns from resources other than XML,
    /// but the current specification of this API does not support handling resources other than XML.
    ///
    /// # Reference
    /// - ISO/IEC 19757-2:2008 7.6 href attribute
    /// - ISO/IEC 19757-2:2008 7.8 include element
    fn load_include<Handler: SAXHandler>(
        &mut self,
        include: &mut Element,
        handler: &mut Handler,
        mut ns_context: Cow<'_, str>,
    ) -> Result<Element, XMLError> {
        assert_eq!(
            include.namespace_name().as_deref(),
            Some(XML_RELAX_NG_NAMESPACE)
        );
        assert_eq!(include.local_name().as_ref(), "include");

        let href = include
            .get_attribute("href", None)
            .ok_or(XMLError::RngParseUnknownError)
            .and_then(|href| Ok(URIString::parse(href)?))?;
        // Since `include` is a descendant of `Document`, `base_uri` will never fail.
        let mut href = include
            .base_uri()
            .map(|base_uri| base_uri.resolve(&href))
            .unwrap();

        // ISO/IEC 19757-2:2008 7.6 href attribute
        if href.fragment().is_some() {
            error!(
                self,
                handler,
                RngParseHRefIncludeFragment,
                "The URI that refers to an XML external resource must not contain a fragment identifier."
            );
            // Recover by removing the fragment identifier.
            href = href.resolve(&URIString::parse("").unwrap());
        }

        if !self.include_loop_guard.insert(href.clone()) {
            fatal_error!(
                self,
                handler,
                RngParseIncludeLoop,
                "'include' causes reference loop for '{}'.",
                href.as_unescaped_str()
                    .as_deref()
                    .unwrap_or(href.as_escaped_str())
            );
            return Err(XMLError::RngParseIncludeLoop);
        }

        let mut reader = XMLReaderBuilder::new()
            .set_handler(TreeBuildHandler::with_handler(handler))
            .build();
        reader.parse_uri(href.as_ref(), None)?;
        let TreeBuildHandler {
            document, handler, ..
        } = reader.handler;
        let Some(mut grammar) = document.document_element() else {
            return Err(XMLError::RngParseIncludeParseFailure);
        };

        if grammar.namespace_name().as_deref() != Some(XML_RELAX_NG_NAMESPACE)
            || grammar.local_name().as_ref() != "grammar"
        {
            fatal_error!(
                self,
                handler,
                RngParseIncludeParseFailure,
                "An included element must be 'grammar' pattern, but '{}' is found.",
                include.name()
            );
            return Err(XMLError::RngParseIncludeParseFailure);
        }

        if let Some(ns) = include.get_attribute("ns", None) {
            ns_context = Cow::Owned(ns);
        }

        self.context_node_stack.push(include.clone());
        self.handle_pattern(&mut grammar, handler, ns_context.as_ref().into())?;
        self.context_node_stack.pop();
        self.include_loop_guard.remove(&href);
        grammar.detach()?;

        let mut start = false;
        let mut define = vec![];
        let mut children = include.first_child();
        // collect 'start' and 'define' components of `include`
        while let Some(mut child) = children {
            if let Some(element) = child.as_element() {
                if element.local_name().as_ref() == "start" {
                    start = true;
                } else if element.local_name().as_ref() == "define" {
                    define.push(element.clone());
                } else if element.local_name().as_ref() == "div"
                    && let Some(first) = element.first_child()
                {
                    children = Some(first);
                    continue;
                }
            }
            if let Some(next) = child.next_sibling() {
                children = Some(next);
            } else {
                children = None;
                while let Some(par) = child.parent_node() {
                    if let Some(next) = par.next_sibling() {
                        children = Some(next);
                        break;
                    }
                    child = par.into();
                }
            }
        }

        define.sort_unstable_by_key(|def| def.get_attribute("name", None));
        define.dedup_by_key(|def| def.get_attribute("name", None));

        // remove 'start' and 'define' components of `grammar`
        // following ISO/IEC 19757-2:2008 7.8 include element
        let mut used_define = vec![false; define.len()];
        let mut children = grammar.first_child();
        let mut remove_start = false;
        while let Some(mut child) = children {
            let mut remove = false;
            if let Some(element) = child.as_element() {
                if start && element.local_name().as_ref() == "start" {
                    remove = true;
                    // Used to check that the imported `grammar` contains at least one `start`.
                    remove_start = true;
                } else if element.local_name().as_ref() == "define" {
                    let name = element.get_attribute("name", None);
                    if let Ok(pos) =
                        define.binary_search_by_key(&name, |def| def.get_attribute("name", None))
                    {
                        used_define[pos] = true;
                        remove = true;
                    }
                } else if element.local_name().as_ref() == "div"
                    && let Some(first) = element.first_child()
                {
                    children = Some(first);
                    if remove {
                        child.detach()?;
                    }
                    continue;
                }
            }
            if let Some(next) = child.next_sibling() {
                children = Some(next);
            } else {
                children = None;
                let mut now = child.clone();
                while let Some(par) = now.parent_node() {
                    if let Some(next) = par.next_sibling() {
                        children = Some(next);
                        break;
                    }
                    now = par.into();
                }
            }

            if remove {
                child.detach()?;
            }
        }

        if start && !remove_start {
            fatal_error!(
                self,
                handler,
                RngParseInsufficientStartInInclude,
                "'include' contains 'start', but included grammar does not contain."
            );
        }

        // "if the include element has a define component,
        // then the grammar element shall have at least one define component with the same name"
        // (ISO/IEC 19757-2:2008 7.8 include element)
        for i in 0..define.len() {
            if !used_define[i] {
                fatal_error!(
                    self,
                    handler,
                    RngParseInsufficientDefineInInclude,
                    "'include' contains 'define' whose 'name' is '{}', but included grammar does not contain.",
                    define[i]
                        .get_attribute("name", None)
                        .as_deref()
                        .unwrap_or("(null)")
                );
            }
        }

        let document = include.owner_document();
        let mut div = document.create_element("div", Some(XML_RELAX_NG_NAMESPACE.into()))?;
        for att in include.attributes() {
            if att.local_name().as_ref() != "href" {
                div.set_attribute(
                    &att.name(),
                    att.namespace_name().as_deref(),
                    Some(&att.value()),
                )?;
            }
        }
        // According to the specification, `grammar` is replaced with `div`,
        // and that `div` becomes a child of `include`. However, since it is
        // immediately flattened, we will flatten it here.
        let mut children = grammar.first_child();
        while let Some(mut child) = children {
            children = child.next_sibling();
            child.detach()?;
            div.append_child(document.import_node(child))?;
        }

        let mut children = include.first_child();
        while let Some(mut child) = children {
            children = child.next_sibling();
            child.detach()?;
            div.append_child(child)?;
        }
        include.replace_subtree(&div)?;

        // Flattening for `div` elements substituted with `include` is performed by the caller.
        // If the `div` has no children, performing flattening here would result in no elements
        // being returned.

        Ok(div)
    }

    fn resolve_namespace_prefix(
        &self,
        prefix: &str,
        element: Option<&Element>,
    ) -> Option<Namespace> {
        if let Some(element) = element
            && let Some(namespace) = element.search_namespace_by_prefix(Some(prefix))
        {
            return Some(namespace);
        }

        for element in self.context_node_stack.iter().rev() {
            if let Some(namespace) = element.search_namespace_by_prefix(Some(prefix)) {
                return Some(namespace);
            }
        }
        None
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 7.19 grammar element
    fn flatten_grammar<Handler: ErrorHandler>(
        &mut self,
        grammar: &mut Element,
        handler: &mut Handler,
        parent_define: &mut HashMap<String, (String, Element)>,
        num_define: &mut usize,
    ) -> Result<(), XMLError> {
        assert_eq!(grammar.local_name().as_ref(), "grammar");
        assert_eq!(
            grammar.namespace_name().as_deref(),
            Some(XML_RELAX_NG_NAMESPACE)
        );

        let mut start = None;
        let mut in_scope_define = HashMap::new();
        let mut children = grammar.first_element_child();
        while let Some(mut element) = children {
            children = element.next_element_sibling();

            if element.local_name().as_ref() == "define" {
                if let Some(name) = element.remove_attribute("name", None) {
                    let alias = format!("define_alias{}", num_define);
                    *num_define += 1;
                    element.set_attribute("name", None, Some(&alias))?;
                    in_scope_define.insert(name, (alias, element));
                }
            } else if element.local_name().as_ref() == "start" {
                start = Some(element);
            }
        }

        let Some(start) = start else {
            fatal_error!(
                self,
                handler,
                RngParseStartNotFoundInGrammar,
                "'grammar' has at least one 'start' element as child, but not found."
            );
            return Err(XMLError::RngParseStartNotFoundInGrammar);
        };

        let mut children = grammar.first_element_child();
        while let Some(mut element) = children {
            match element.local_name().as_ref() {
                "grammar" => {
                    if let Some(next) = element.next_element_sibling() {
                        children = Some(next);
                    } else {
                        children = None;
                        let mut now = element.clone();
                        while let Some(parent) = now.parent_node() {
                            if grammar.is_same_node(&parent) {
                                break;
                            }
                            if let Some(next) = parent.next_element_sibling() {
                                children = Some(next);
                                break;
                            }
                            now = parent.as_element().unwrap();
                        }
                    }

                    self.flatten_grammar(&mut element, handler, &mut in_scope_define, num_define)?;
                    continue;
                }
                local_name @ ("ref" | "parentRef") => match local_name {
                    "ref" => {
                        if let Some(name) = element.remove_attribute("name", None) {
                            if let Some(define) = in_scope_define.get_mut(&name) {
                                element.set_attribute("name", None, Some(&define.0))?;
                            } else {
                                fatal_error!(
                                    self,
                                    handler,
                                    RngParseUnresolvableRefName,
                                    "The 'name' attribute of 'ref' has a value '{}', but it is unresolvable.",
                                    name
                                );
                            }
                        }
                    }
                    "parentRef" => {
                        if let Some(name) = element.get_attribute("name", None) {
                            if let Some(define) = parent_define.get_mut(&name) {
                                // replace 'parentRef' to 'ref'
                                let mut r#ref = self
                                    .document
                                    .create_element("ref", Some(XML_RELAX_NG_NAMESPACE.into()))?;
                                r#ref.set_attribute("name", None, Some(&define.0))?;
                                element.replace_subtree(&r#ref)?;
                                element = r#ref;
                            } else {
                                fatal_error!(
                                    self,
                                    handler,
                                    RngParseUnresolvableRefName,
                                    "The 'name' attribute of 'parentRef' has a value '{}', but it is unresolvable.",
                                    name
                                );
                            }
                        }
                    }
                    _ => {}
                },
                _ => {}
            }

            if let Some(first) = element.first_element_child() {
                children = Some(first);
            } else if let Some(next) = element.next_element_sibling() {
                children = Some(next);
            } else {
                children = None;
                let mut now = element.clone();
                while let Some(parent) = now.parent_node() {
                    if grammar.is_same_node(&parent) {
                        break;
                    }
                    if let Some(next) = parent.next_element_sibling() {
                        children = Some(next);
                        break;
                    }
                    now = parent.as_element().unwrap();
                }
            }
        }

        let document = self.document.clone();
        let mut master_grammar = document
            .document_element()
            .ok_or(XMLError::RngParseUnknownError)?;
        assert_eq!(master_grammar.local_name().as_ref(), "grammar");
        assert_eq!(
            master_grammar.namespace_name().as_deref(),
            Some(XML_RELAX_NG_NAMESPACE)
        );
        for (_, (alias, define)) in in_scope_define {
            master_grammar.append_child(&define)?;
            self.defines.insert(alias, define);
        }

        if !grammar.is_same_node(master_grammar) {
            let mut children = start.first_child();
            while let Some(mut child) = children {
                children = child.next_sibling();
                child.detach()?;
                grammar.insert_previous_sibling(child)?;
            }
            grammar.detach()?;
        }
        Ok(())
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 7.20 define and ref elements
    fn remove_unreachable_define(&mut self, grammar: &mut Element) -> Result<(), XMLError> {
        let mut next = vec![];
        let mut children = grammar.first_element_child();
        while let Some(element) = children {
            children = element.next_element_sibling();

            if element.local_name().as_ref() == "start" {
                next.push(element);
            }
        }

        let mut seen = HashSet::new();
        while let Some(start) = next.pop() {
            let mut children = start.first_element_child();
            while let Some(element) = children {
                if element.local_name().as_ref() == "ref" {
                    let name = element
                        .get_attribute("name", None)
                        .ok_or(XMLError::RngParseUnknownError)?;
                    let define = self
                        .defines
                        .get(&name)
                        .ok_or(XMLError::RngParseUnknownError)?;
                    if seen.insert(name) {
                        next.push(define.clone());
                    }
                }

                if let Some(first) = element.first_element_child() {
                    children = Some(first);
                } else if let Some(next) = element.next_element_sibling() {
                    children = Some(next);
                } else {
                    children = None;
                    let mut now = element;
                    while let Some(parent) = now.parent_node() {
                        if start.is_same_node(&parent) {
                            break;
                        }
                        if let Some(next) = parent.next_element_sibling() {
                            children = Some(next);
                            break;
                        }
                        now = parent.as_element().unwrap();
                    }
                }
            }
        }

        self.defines.shrink_to_fit();
        for (_, mut define) in self.defines.extract_if(|name, _| !seen.contains(name)) {
            define.detach()?;
        }

        Ok(())
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 7.20 define and ref elements
    fn normalize_define(
        &mut self,
        grammar: &mut Element,
        context_node: &mut Element,
        num_define: &mut usize,
    ) -> Result<(), XMLError> {
        let mut children = context_node.first_element_child();
        if context_node.local_name().as_ref() == "element"
            && context_node
                .parent_node()
                .and_then(|par| par.as_element())
                .is_none_or(|elem| elem.local_name().as_ref() != "define")
        {
            let alias = format!("define_alias{}", num_define);
            *num_define += 1;
            let mut define = self
                .document
                .create_element("define", Some(XML_RELAX_NG_NAMESPACE.into()))?;
            define.set_attribute("name", None, Some(&alias))?;
            let mut r#ref = self
                .document
                .create_element("ref", Some(XML_RELAX_NG_NAMESPACE.into()))?;
            r#ref.set_attribute("name", None, Some(&alias))?;
            context_node.insert_previous_sibling(&r#ref)?;
            define.append_child(context_node.clone())?;
            grammar.append_child(&define)?;
            self.defines.insert(alias, define);
            *context_node = r#ref;
        }

        while let Some(mut context_node) = children {
            children = context_node.next_element_sibling();
            self.normalize_define(grammar, &mut context_node, num_define)?;
        }

        Ok(())
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 7.20 define and ref elements
    fn normalize_ref<Handler: ErrorHandler>(
        &mut self,
        context_node: &mut Element,
        handler: &mut Handler,
        mut start: bool,
        mut element: bool,
    ) -> Result<(), XMLError> {
        let mut expand = false;
        let mut children = context_node.first_element_child();
        match context_node.local_name().as_ref() {
            "start" => start = true,
            "element" => element = true,
            "ref" => {
                expand = start || element;
            }
            _ => {}
        }

        while let Some(mut context_node) = children {
            children = context_node.next_element_sibling();
            self.normalize_ref(&mut context_node, handler, start, element)?;
        }

        if expand {
            self.expand_ref(context_node, handler, &mut HashSet::new())?;
        }

        Ok(())
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 7.20 define and ref elements
    fn expand_ref<Handler: ErrorHandler>(
        &mut self,
        context_node: &mut Element,
        handler: &mut Handler,
        used_names: &mut HashSet<String>,
    ) -> Result<(), XMLError> {
        if context_node.local_name().as_ref() == "ref"
            && let Some(name) = context_node.get_attribute("name", None)
        {
            if !used_names.insert(name.clone()) {
                fatal_error!(
                    self,
                    handler,
                    RngParseRefLoop,
                    "A reference loop is detected at 'ref' whose 'name' value is '{}'.",
                    name
                );
                return Err(XMLError::RngParseRefLoop);
            }

            let define = self
                .defines
                .get(&name)
                .ok_or(XMLError::RngParseUnknownError)?;

            if define
                .first_child()
                .and_then(|ch| ch.as_element())
                .is_some_and(|elem| elem.local_name().as_ref() == "element")
            {
                return Ok(());
            }

            fn deep_copy<Spec: InternalNodeSpec>(
                document: &Document,
                context_node: &mut Node<Spec>,
                source: &Node<dyn NodeSpec>,
            ) -> Result<(), XMLError> {
                match source.downcast() {
                    NodeKind::Element(element) => {
                        let mut copied =
                            document.create_element(element.name(), element.namespace_name())?;
                        for att in element.attributes() {
                            copied.set_attribute(
                                att.name().as_ref(),
                                att.namespace_name().as_deref(),
                                Some(att.value().as_str()),
                            )?;
                        }

                        let mut children = source.first_child();
                        while let Some(child) = children {
                            children = child.next_sibling();
                            deep_copy(document, &mut copied, &child)?;
                        }
                        context_node.append_child(copied)?;
                    }
                    NodeKind::Text(text) => {
                        context_node.append_child(document.create_text(&*text.data()))?;
                    }
                    _ => unreachable!(),
                }
                Ok(())
            }

            let mut frag = self.document.create_document_fragment();
            let mut children = define.first_child();
            while let Some(child) = children {
                children = child.next_sibling();
                deep_copy(&self.document, &mut frag, &child)?;
            }

            let mut children = frag.first_child();
            while let Some(child) = children {
                children = child.next_sibling();
                if let Some(mut element) = child.as_element() {
                    self.expand_ref(&mut element, handler, used_names)?;
                }
            }

            context_node.insert_previous_sibling(frag)?;
            context_node.detach()?;
        } else {
            let mut children = context_node.first_child();
            while let Some(child) = children {
                children = child.next_sibling();
                if let Some(mut element) = child.as_element() {
                    self.expand_ref(&mut element, handler, used_names)?;
                }
            }
        }
        Ok(())
    }

    fn check_attribute_constraint<Handler: ErrorHandler, const M: usize, const O: usize>(
        &mut self,
        element: &mut Element,
        handler: &mut Handler,
        mandatory: [(&'static str, fn(&str) -> Result<(), XMLError>); M],
        optional: [(&'static str, fn(&str) -> Result<(), XMLError>); O],
    ) -> Result<(), XMLError> {
        let mut use_mandatory = [false; M];
        let mut use_optional = [false; O];

        let mut remove = vec![];

        for mut att in element.attributes() {
            let value = att.value();
            att.set_value(value.trim_matches(|c| XMLVersion::default().is_whitespace(c)))?;

            if att.name().as_ref() == "xml:base" {
                continue;
            }

            if att.namespace().is_some() {
                error!(
                    self,
                    handler,
                    RngParseUnacceptableAttribute,
                    "The attribute '{}' is not allowed on '{}'.",
                    att.name(),
                    element.name()
                );
                remove.push(att);
                continue;
            }

            if let Some(pos) = mandatory.iter().position(|&m| m.0 == att.name().as_ref()) {
                use_mandatory[pos] = true;
                mandatory[pos].1(&att.value())?;
            } else if let Some(pos) = optional.iter().position(|&m| m.0 == att.name().as_ref()) {
                use_optional[pos] = true;
                optional[pos].1(&att.value())?;
            } else if att.name().as_ref() == "ns" {
                // this is always allowed

                // no op.
            } else if att.name().as_ref() == "datatypeLibrary" {
                // this is always allowed

                let value = att.value();
                if !value.is_empty() && validate_rfc2396_absolute_uri(&value).is_err() {
                    error!(
                        self,
                        handler,
                        RngParseDatatypeLibraryURINotAbsolute,
                        "The attribute 'datatypeLibrary' must have an absolute URI value, but '{}' is not.",
                        value
                    );
                    remove.push(att);
                }
            } else {
                error!(
                    self,
                    handler,
                    RngParseUnacceptableAttribute,
                    "The attribute '{}' is not allowed on '{}'.",
                    att.name(),
                    element.name()
                );
                remove.push(att);
            }
        }

        for att in remove {
            element.remove_attribute_node(att)?;
        }

        let mut bad = false;
        for i in 0..M {
            if !use_mandatory[i] {
                fatal_error!(
                    self,
                    handler,
                    RngParseInsufficientAttribute,
                    "'{}' must have an attribute '{}', but not found.",
                    element.name(),
                    mandatory[i].0
                );
                bad = true;
            }
        }

        if bad {
            return Err(XMLError::RngParseInsufficientAttribute);
        }

        // ISO/IEC 19757-2:2008 7.4 `datatypeLibrary` attribute
        if !element.has_attribute("datatypeLibrary", None) {
            element.set_attribute(
                "datatypeLibrary",
                None,
                Some(
                    &element
                        .parent_node()
                        .and_then(|parent| parent.as_element())
                        .and_then(|parent| parent.get_attribute("datatypeLibrary", None))
                        .unwrap_or_default(),
                ),
            )?;
        }

        Ok(())
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 7.17 Constraints
    fn check_name_class_constraint<Handler: ErrorHandler>(
        &mut self,
        name_class: &Element,
        handler: &mut Handler,
    ) -> Result<(), XMLError> {
        match name_class.local_name().as_ref() {
            "anyName" => {
                let mut children = name_class.first_child();
                while let Some(child) = children {
                    children = child.next_sibling();

                    if let Some(except) = child
                        .as_element()
                        .filter(|elem| elem.local_name().as_ref() == "except")
                        && except
                            .get_elements_by_expanded_name("anyName", Some(XML_RELAX_NG_NAMESPACE))
                            .next()
                            .is_some()
                    {
                        fatal_error!(
                            self,
                            handler,
                            RngParseUnacceptablePattern,
                            "An 'except' element that is a child of an 'anyName' element shall not have any 'anyName' descendant elements."
                        );
                    }
                }
            }
            "nsName" => {
                let mut children = name_class.first_child();
                while let Some(child) = children {
                    children = child.next_sibling();

                    if let Some(except) = child
                        .as_element()
                        .filter(|elem| elem.local_name().as_ref() == "except")
                        && (except
                            .get_elements_by_expanded_name("anyName", Some(XML_RELAX_NG_NAMESPACE))
                            .next()
                            .is_some()
                            || except
                                .get_elements_by_expanded_name(
                                    "nsName",
                                    Some(XML_RELAX_NG_NAMESPACE),
                                )
                                .next()
                                .is_some())
                    {
                        fatal_error!(
                            self,
                            handler,
                            RngParseUnacceptablePattern,
                            "An 'except' element that is a child of an 'nsName' element shall not have any 'nsName' or 'anyName' descendant elements."
                        );
                    }
                }
            }
            _ => {}
        }

        Ok(())
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 7.17 Constraints
    fn check_attribute_name_constraint<Handler: ErrorHandler>(
        &mut self,
        attribute: &Element,
        handler: &mut Handler,
    ) -> Result<(), XMLError> {
        let mut children = attribute.first_child();
        while let Some(child) = children {
            if let Some(first) = child.first_child() {
                children = Some(first);
            } else if !child.is_same_node(attribute.first_child().unwrap())
                && let Some(next) = child.next_sibling()
            {
                children = Some(next);
            } else {
                children = None;
                let mut now = child.clone();
                while let Some(parent) = now.parent_node() {
                    if parent.is_same_node(attribute)
                        || parent.is_same_node(attribute.first_child().unwrap())
                    {
                        break;
                    }
                    if let Some(next) = parent.next_sibling() {
                        children = Some(next);
                        break;
                    }
                    now = parent.into();
                }
            }

            if let Some(element) = child.as_element() {
                match element.local_name().as_ref() {
                    "name" => {
                        if let Some(ns) = element.get_attribute("ns", None) {
                            if ns.is_empty()
                                && let Some(text) =
                                    element.first_child().and_then(|ch| ch.as_text())
                                && &*text.data() == "xmlns"
                            {
                                fatal_error!(
                                    self,
                                    handler,
                                    RngParseUnacceptableAttribute,
                                    "A 'name' element at the descendant of the first child of an 'attribute' element and that has an 'ns' attribute with empty value shall not have 'xmlns' as its content."
                                );
                            } else if ns == XML_NS_NAMESPACE {
                                // The specification targets `http://www.w3.org/2000/xmlns`
                                // (without the trailing slash) as the constraint. However,
                                // considering the intent, it should also generate an error
                                // for `http://www.w3.org/2000/xmlns/`, which conforms to the
                                // namespace specification.
                                fatal_error!(
                                    self,
                                    handler,
                                    RngParseUnacceptableAttribute,
                                    "A 'name' element at the descendant of the first child of an 'attribute' element shall not have an 'ns' attribute with value '{}'.",
                                    XML_NS_NAMESPACE
                                );
                            } else if XML_NS_NAMESPACE[..XML_NS_NAMESPACE.len() - 1] == ns {
                                // The specification states that the namespace name binding `xmlns`
                                // is `http://www.w3.org/2000/xmlns` (without the trailing slash),
                                // which appears to contradict the namespace specification.
                                // However, it correctly reports an error as per the specification.
                                fatal_error!(
                                    self,
                                    handler,
                                    RngParseUnacceptableAttribute,
                                    "A 'name' element at the descendant of the first child of an 'attribute' element shall not have an 'ns' attribute with value '{}'.",
                                    &XML_NS_NAMESPACE[..XML_NS_NAMESPACE.len() - 1]
                                );
                            }
                        }
                    }
                    "nsName" => {
                        if let Some(ns) = element.get_attribute("ns", None) {
                            if ns == XML_NS_NAMESPACE {
                                // The specification targets `http://www.w3.org/2000/xmlns`
                                // (without the trailing slash) as the constraint. However,
                                // considering the intent, it should also generate an error
                                // for `http://www.w3.org/2000/xmlns/`, which conforms to the
                                // namespace specification.
                                fatal_error!(
                                    self,
                                    handler,
                                    RngParseUnacceptableAttribute,
                                    "A 'nsName' element at the descendant of the first child of an 'attribute' element shall not have an 'ns' attribute with value '{}'.",
                                    XML_NS_NAMESPACE
                                );
                            } else if XML_NS_NAMESPACE[..XML_NS_NAMESPACE.len() - 1] == ns {
                                // The specification states that the namespace name binding `xmlns`
                                // is `http://www.w3.org/2000/xmlns` (without the trailing slash),
                                // which appears to contradict the namespace specification.
                                // However, it correctly reports an error as per the specification.
                                fatal_error!(
                                    self,
                                    handler,
                                    RngParseUnacceptableAttribute,
                                    "A 'name' element at the descendant of the first child of an 'attribute' element shall not have an 'ns' attribute with value '{}'.",
                                    &XML_NS_NAMESPACE[..XML_NS_NAMESPACE.len() - 1]
                                );
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        Ok(())
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 7.18 `combine` attribute
    fn bundle_start_and_define<Handler: ErrorHandler>(
        &mut self,
        grammar: &mut Element,
        handler: &mut Handler,
    ) -> Result<(), XMLError> {
        assert_eq!(grammar.local_name().as_ref(), "grammar");
        assert_eq!(
            grammar.namespace_name().as_deref(),
            Some(XML_RELAX_NG_NAMESPACE)
        );

        let document = grammar.owner_document();

        // for 'start'
        let mut combine_start = None;
        let mut frag_start = document.create_document_fragment();
        let mut num_start_without_combine = 0;

        // for 'define'
        // key  : 'name' of 'define'
        // value: (num_define_without_combine, fragment, combine)
        let mut define_mapping = HashMap::new();

        let mut children = grammar.first_child();
        while let Some(mut child) = children {
            if let Some(mut element) = child.as_element() {
                match element.local_name().as_ref() {
                    "start" => {
                        if let Some(combine) = element.remove_attribute("combine", None) {
                            if let Some(other) = combine_start.as_deref()
                                && other != combine
                            {
                                fatal_error!(
                                    self,
                                    handler,
                                    RngParseUnacceptableCombine,
                                    "The 'combine' attribute values of multiple 'start' elements within the 'grammar' element are inconsistent."
                                );
                            } else {
                                combine_start = Some(combine);
                            }
                        } else {
                            num_start_without_combine += 1;
                            if num_start_without_combine == 2 {
                                fatal_error!(
                                    self,
                                    handler,
                                    RngParseMultipleStartWithoutCombine,
                                    "Multiple 'start' element without 'combine' attribute appear."
                                );
                            }
                        }
                        let mut children = element.first_child();
                        while let Some(child) = children {
                            children = child.next_sibling();
                            frag_start.append_child(child)?;
                        }
                    }
                    "define" => {
                        if let Some(name) = element.remove_attribute("name", None) {
                            let (num_define_without_combine, frag_define, combine_define) =
                                define_mapping.entry(name).or_insert_with(|| {
                                    (0, document.create_document_fragment(), None)
                                });
                            if let Some(combine) = element.remove_attribute("combine", None) {
                                if let Some(other) = combine_define.as_deref()
                                    && other != combine
                                {
                                    fatal_error!(
                                        self,
                                        handler,
                                        RngParseUnacceptableCombine,
                                        "The 'combine' attribute values of multiple 'define' elements within the 'grammar' element are inconsistent."
                                    );
                                } else {
                                    *combine_define = Some(combine);
                                }
                            } else {
                                *num_define_without_combine += 1;
                                if *num_define_without_combine == 2 {
                                    fatal_error!(
                                        self,
                                        handler,
                                        RngParseMultipleStartWithoutCombine,
                                        "Multiple 'define' element without 'combine' attribute appear."
                                    );
                                }
                            }
                            let mut children = element.first_child();
                            while let Some(child) = children {
                                children = child.next_sibling();
                                frag_define.append_child(child)?;
                            }
                        }
                    }
                    _ => {}
                }
            }
            children = child.next_sibling();
            child.detach()?;
        }

        assert!(grammar.first_child().is_none());

        // reconstruct 'start'
        if combine_start.is_some() || num_start_without_combine > 0 {
            let combine = combine_start.unwrap_or_else(|| "choice".to_owned());
            let mut start =
                document.create_element("start", Some(XML_RELAX_NG_NAMESPACE.into()))?;
            let mut combine =
                document.create_element(combine, Some(XML_RELAX_NG_NAMESPACE.into()))?;
            grammar.append_child(&start)?;
            start.append_child(&combine)?;
            combine.append_child(frag_start)?;
            group_children(&mut combine)?;
        } else {
            fatal_error!(
                self,
                handler,
                RngParseStartNotFoundInGrammar,
                "'start' is not found in 'grammar'"
            );
        }

        // reconstruct 'define'
        for (name, (_, frag, combine)) in define_mapping {
            let combine = combine.unwrap_or_else(|| "choice".to_owned());
            let mut define =
                document.create_element("define", Some(XML_RELAX_NG_NAMESPACE.into()))?;
            let mut combine =
                document.create_element(combine, Some(XML_RELAX_NG_NAMESPACE.into()))?;
            grammar.append_child(&define)?;
            define.append_child(&combine)?;
            combine.append_child(frag)?;
            group_children(&mut combine)?;
            define.set_attribute("name", None, Some(&name))?;
        }

        Ok(())
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 10.2 Prohibited paths
    fn verify_prohibited_paths<Handler: ErrorHandler>(
        &mut self,
        element: &Element,
        memo: &mut HashSet<String>,
        handler: &mut Handler,
    ) -> Result<(), XMLError> {
        let mut ret = Ok(());
        let mut children = element.first_child();
        let mut buf = HashSet::new();
        while let Some(child) = children {
            children = child.next_sibling();

            if let Some(elem) = child.as_element() {
                ret = ret.and(self.verify_prohibited_paths(&elem, &mut buf, handler));
            }
        }

        macro_rules! report_prohibited_path {
            ($ancestor:literal, $cwd:literal) => {
                if buf.contains($cwd) {
                    fatal_error!(
                        self,
                        handler,
                        RngParseProhibitedPath,
                        "'{}//{}' path is not allowed.",
                        $ancestor,
                        $cwd
                    );
                    ret = Err(XMLError::RngParseProhibitedPath);
                }
            };
        }

        match element.local_name().as_ref() {
            "attribute" => {
                // ISO/IEC 19757-2:2008 10.2.2 `attribute` pattern
                report_prohibited_path!("attribute", "ref");
                report_prohibited_path!("attribute", "attribute");
            }
            "oneOrMore" => {
                // ISO/IEC 19757-2:2008 10.2.3 `oneOrMore` pattern
                report_prohibited_path!("oneOrMore", "group//attribute");
                report_prohibited_path!("oneOrMore", "interleave//attribute");
            }
            "list" => {
                // ISO/IEC 19757-2:2008 10.2.4 `list` pattern
                report_prohibited_path!("list", "list");
                report_prohibited_path!("list", "ref");
                report_prohibited_path!("list", "attribute");
                report_prohibited_path!("list", "text");
                report_prohibited_path!("list", "interleave");
            }
            "except"
                if element
                    .parent_node()
                    .and_then(|par| par.as_element())
                    .is_some_and(|par| par.local_name().as_ref() == "data") =>
            {
                // ISO/IEC 19757-2:2008 10.2.5 `except` element in `data` pattern
                report_prohibited_path!("data/except", "attribute");
                report_prohibited_path!("data/except", "ref");
                report_prohibited_path!("data/except", "text");
                report_prohibited_path!("data/except", "list");
                report_prohibited_path!("data/except", "group");
                report_prohibited_path!("data/except", "interleave");
                report_prohibited_path!("data/except", "oneOrMore");
                report_prohibited_path!("data/except", "empty");
            }
            "start" => {
                // ISO/IEC 19757-2:2008 10.2.6 `start` element
                report_prohibited_path!("start", "attribute");
                report_prohibited_path!("start", "data");
                report_prohibited_path!("start", "value");
                report_prohibited_path!("start", "text");
                report_prohibited_path!("start", "list");
                report_prohibited_path!("start", "group");
                report_prohibited_path!("start", "interleave");
                report_prohibited_path!("start", "oneOrMore");
                report_prohibited_path!("start", "empty");
            }
            "group" if buf.contains("attribute") => {
                buf.insert("group//attribute".into());
            }
            "interleave" if buf.contains("attribute") => {
                buf.insert("interleave//attribute".into());
            }
            _ => {}
        }
        memo.insert(element.local_name().as_ref().into());
        memo.extend(buf);

        ret
    }

    fn normalize_string_child<Handler: ErrorHandler>(
        &mut self,
        element: &mut Element,
        handler: &mut Handler,
        trim: bool,
    ) -> Result<(), XMLError> {
        let mut children = element.first_child();
        while let Some(mut child) = children {
            if let Some(mut text) = child.as_text() {
                let mut siblings = child.next_sibling();
                while let Some(mut sibling) = siblings {
                    siblings = sibling.next_sibling();
                    match sibling.downcast() {
                        NodeKind::Text(mut now) => {
                            now.detach()?;
                            text.push_str(&now.data());
                        }
                        NodeKind::CDATASection(mut now) => {
                            now.detach()?;
                            text.push_str(&now.data());
                        }
                        _ => {
                            fatal_error!(
                                self,
                                handler,
                                RngParseUnacceptablePattern,
                                "Only a string is allowed as a child of 'param', but {:?} is found.",
                                sibling.node_type()
                            );
                            sibling.detach()?;
                        }
                    }
                }

                if trim {
                    let d = text.data();
                    let data = d.trim_matches(|c| XMLVersion::default().is_whitespace(c));
                    if text.data().len() != data.len() {
                        let new = element.owner_document().create_text(data);
                        drop(d);
                        text.detach()?;
                        element.append_child(new)?;
                    }
                }
                break;
            } else if let Some(mut cdata) = child.as_cdata_section() {
                let mut siblings = child.next_sibling();
                while let Some(mut sibling) = siblings {
                    siblings = sibling.next_sibling();
                    match sibling.downcast() {
                        NodeKind::Text(mut now) => {
                            now.detach()?;
                            cdata.push_str(&now.data());
                        }
                        NodeKind::CDATASection(mut now) => {
                            now.detach()?;
                            cdata.push_str(&now.data());
                        }
                        _ => {
                            fatal_error!(
                                self,
                                handler,
                                RngParseUnacceptablePattern,
                                "Only a string is allowed as a child of 'param', but {:?} is found.",
                                sibling.node_type()
                            );
                            sibling.detach()?;
                        }
                    }
                }

                let text = if trim {
                    element.owner_document().create_text(
                        cdata
                            .data()
                            .trim_matches(|c| XMLVersion::default().is_whitespace(c)),
                    )
                } else {
                    element.owner_document().create_text(&*cdata.data())
                };

                cdata.detach()?;
                element.append_child(text)?;
                break;
            } else {
                children = child.next_sibling();
                fatal_error!(
                    self,
                    handler,
                    RngParseUnacceptablePattern,
                    "Only a string is allowed as a child of 'param', but {:?} is found.",
                    child.node_type()
                );
                child.detach()?;
            }
        }

        if element.first_child().is_none() {
            let text = element.owner_document().create_text("");
            element.append_child(text)?;
        }
        Ok(())
    }
}

/// # Reference
/// ISO/IEC 19757-2:2008 7.21 `notAllowed` element
fn normalize_not_allowed(
    context_node: &mut Element,
    removable_except: &mut Vec<Element>,
) -> Result<(), XMLError> {
    match context_node.local_name().as_ref() {
        "notAllowed" => Ok(()),
        "attribute" | "list" | "group" | "interleave" | "oneOrMore" => {
            let mut children = context_node.first_child();
            while let Some(child) = children {
                if let Some(mut element) = child.as_element() {
                    normalize_not_allowed(&mut element, removable_except)?;

                    if element.local_name().as_ref() == "notAllowed" {
                        element.detach()?;
                        context_node.insert_previous_sibling(&element)?;
                        context_node.detach()?;
                        *context_node = element;
                        return Ok(());
                    }
                    children = child.next_sibling();
                } else {
                    children = child.next_sibling();
                }
            }
            Ok(())
        }
        "choice" => {
            let mut first = context_node
                .first_child()
                .and_then(|ch| ch.as_element())
                .ok_or(XMLError::RngParseUnknownError)?;
            let mut second = context_node
                .last_child()
                .and_then(|ch| ch.as_element())
                .ok_or(XMLError::RngParseUnknownError)?;
            if first.is_same_node(&second) {
                return Err(XMLError::RngParseUnknownError);
            }
            normalize_not_allowed(&mut first, removable_except)?;
            normalize_not_allowed(&mut second, removable_except)?;

            match (first.local_name().as_ref(), second.local_name().as_ref()) {
                (_, "notAllowed") => {
                    first.detach()?;
                    context_node.insert_previous_sibling(&first)?;
                    context_node.detach()?;
                    *context_node = first;
                }
                ("notAllowed", _) => {
                    second.detach()?;
                    context_node.insert_previous_sibling(&second)?;
                    context_node.detach()?;
                    *context_node = second;
                }
                _ => {}
            }
            Ok(())
        }
        "except" => {
            if let Some(mut element) = context_node.first_child().and_then(|ch| ch.as_element()) {
                normalize_not_allowed(&mut element, removable_except)?;

                if element.local_name().as_ref() == "notAllowed" {
                    removable_except.push(element);
                }
            }
            Ok(())
        }
        _ => {
            let mut children = context_node.first_child();
            while let Some(child) = children {
                if let Some(mut element) = child.as_element() {
                    normalize_not_allowed(&mut element, removable_except)?;
                    children = element.next_sibling();
                } else {
                    children = child.next_sibling();
                }
            }
            Ok(())
        }
    }
}

/// Reference
/// ISO/IEC 19757-2:2008 7.22 empty element
fn normalize_empty(context_node: &mut Element) -> Result<(), XMLError> {
    match context_node.local_name().as_ref() {
        "choice" => {
            let mut first = context_node
                .first_child()
                .and_then(|ch| ch.as_element())
                .ok_or(XMLError::RngParseUnknownError)?;
            let mut second = context_node
                .last_child()
                .and_then(|ch| ch.as_element())
                .ok_or(XMLError::RngParseUnknownError)?;

            if first.is_same_node(&second) {
                return Err(XMLError::RngParseUnknownError);
            }
            normalize_empty(&mut first)?;
            normalize_empty(&mut second)?;

            match (first.local_name().as_ref(), second.local_name().as_ref()) {
                ("empty", "empty") => {
                    first.detach()?;
                    context_node.insert_previous_sibling(&first)?;
                    context_node.detach()?;
                    *context_node = first;
                }
                (_, "empty") => {
                    second.detach()?;
                    first.insert_previous_sibling(second)?;
                }
                _ => {}
            }
        }
        "group" | "interleave" => {
            let mut first = context_node
                .first_child()
                .and_then(|ch| ch.as_element())
                .ok_or(XMLError::RngParseUnknownError)?;
            let mut second = context_node
                .last_child()
                .and_then(|ch| ch.as_element())
                .ok_or(XMLError::RngParseUnknownError)?;

            if first.is_same_node(&second) {
                return Err(XMLError::RngParseUnknownError);
            }
            normalize_empty(&mut first)?;
            normalize_empty(&mut second)?;

            match (first.local_name().as_ref(), second.local_name().as_ref()) {
                ("empty", "empty") => {
                    first.detach()?;
                    context_node.insert_previous_sibling(&first)?;
                    context_node.detach()?;
                    *context_node = first;
                }
                ("empty", _) => {
                    second.detach()?;
                    context_node.insert_previous_sibling(&second)?;
                    context_node.detach()?;
                    *context_node = second;
                }
                (_, "empty") => {
                    first.detach()?;
                    context_node.insert_previous_sibling(&first)?;
                    context_node.detach()?;
                    *context_node = first;
                }
                _ => {}
            }
        }
        "oneOrMore" => {
            let mut first = context_node
                .first_child()
                .and_then(|ch| ch.as_element())
                .ok_or(XMLError::RngParseUnknownError)?;
            normalize_empty(&mut first)?;

            if first.local_name().as_ref() == "empty" {
                first.detach()?;
                context_node.insert_previous_sibling(&first)?;
                context_node.detach()?;
                *context_node = first;
            }
        }
        _ => {
            let mut children = context_node.first_child();
            while let Some(child) = children {
                if let Some(mut element) = child.as_element() {
                    normalize_empty(&mut element)?;
                    children = element.next_sibling();
                } else {
                    children = child.next_sibling();
                }
            }
        }
    }
    Ok(())
}

/// Flatten children of `div` and remove 'div' element.
///
/// Return a node that was a last child of `div`.
///
/// # Reference
/// ISO/IEC 19757-2:2008 7.12 `div` element
fn flatten_div(div: &mut Element) -> Result<Node<dyn NodeSpec>, XMLError> {
    assert_eq!(
        div.namespace_name().as_deref(),
        Some(XML_RELAX_NG_NAMESPACE)
    );
    assert_eq!(div.local_name().as_ref(), "div");

    let mut prev: Node<dyn NodeSpec> = div.clone().into();
    let mut children = div.first_child();
    while let Some(mut child) = children {
        children = child.next_sibling();
        child.detach()?;
        prev.insert_next_sibling(&child)?;
        prev = child;
    }

    div.detach()?;
    Ok(prev)
}

/// # Reference
/// ISO/IEC 19757-2:2008 7.13 Number of child elements
fn group_children(element: &mut Element) -> Result<(), XMLError> {
    // There are no children
    let Some(mut first) = element.first_child() else {
        return Ok(());
    };
    match element.local_name().as_ref() {
        "define" | "oneOrMore" | "zeroOrMore" | "optional" | "list" | "mixed" => {
            // There is exactly one child
            if element.last_child().unwrap().is_same_node(&first) {
                return Ok(());
            }

            let document = element.owner_document();
            while let Some(second) = first.next_sibling() {
                let mut group =
                    document.create_element("group", Some(XML_RELAX_NG_NAMESPACE.into()))?;
                first.insert_previous_sibling(&group)?;
                group.append_child(first)?;
                group.append_child(second)?;
                first = group.into();
            }
        }
        "element" => {
            if let Some(mut first) = first.next_sibling()
                && !first.is_same_node(element.last_child().unwrap())
            {
                let document = element.owner_document();
                while let Some(second) = first.next_sibling() {
                    let mut group =
                        document.create_element("group", Some(XML_RELAX_NG_NAMESPACE.into()))?;
                    first.insert_previous_sibling(&group)?;
                    group.append_child(first)?;
                    group.append_child(second)?;
                    first = group.into();
                }
            }
        }
        "except" => {
            // There are more than one children
            if !element.last_child().unwrap().is_same_node(&first) {
                let document = element.owner_document();
                while let Some(second) = first.next_sibling() {
                    let mut choice =
                        document.create_element("choice", Some(XML_RELAX_NG_NAMESPACE.into()))?;
                    first.insert_previous_sibling(&choice)?;
                    choice.append_child(first)?;
                    choice.append_child(second)?;
                    first = choice.into();
                }
            }
        }
        "attribute" => {
            // There is exactly one child
            if element.last_child().unwrap().is_same_node(&first) {
                element.append_child(element.owner_document().create_text(""))?;
            }
        }
        "choice" | "group" | "interleave" => {
            let name = element.local_name().clone();
            let document = element.owner_document();
            while let Some(second) = first.next_sibling() {
                let mut group =
                    document.create_element(name.as_ref(), Some(XML_RELAX_NG_NAMESPACE.into()))?;
                first.replace_subtree(&mut group)?;
                group.append_child(first)?;
                group.append_child(second)?;
                first = group.into();
            }
            element.replace_subtree(&first)?;
            *element = first.as_element().unwrap();
        }
        _ => unreachable!("name: {}", element.local_name()),
    }

    Ok(())
}

/// Decompose `mixed` to 'interleave' element including 'text' element.
///
/// `mixed` is replaced to replacement 'interleave' element.
///
/// # Reference
/// ISO/IEC 19757-2:2008 7.14 `mixed` element
fn decompose_mixed(mixed: &mut Element) -> Result<(), XMLError> {
    assert_eq!(mixed.local_name().as_ref(), "mixed");
    assert_eq!(
        mixed.namespace_name().as_deref(),
        Some(XML_RELAX_NG_NAMESPACE)
    );

    let document = mixed.owner_document();
    let mut interleave =
        document.create_element("interleave", Some(XML_RELAX_NG_NAMESPACE.into()))?;
    let mut children = mixed.first_child();
    while let Some(child) = children {
        children = child.next_sibling();
        interleave.append_child(child)?;
    }

    interleave
        .append_child(document.create_element("text", Some(XML_RELAX_NG_NAMESPACE.into()))?)?;

    mixed.insert_previous_sibling(&interleave)?;
    mixed.detach()?;
    *mixed = interleave;

    Ok(())
}

/// Decompose `optional` to 'choice' element including 'empty' element.
///
/// `optional` is replaced to replacement 'choice' element.
///
/// # Reference
/// ISO/IEC 19757-2:2008 7.15 `optional` element
fn decompose_optional(optional: &mut Element) -> Result<(), XMLError> {
    assert_eq!(optional.local_name().as_ref(), "optional");
    assert_eq!(
        optional.namespace_name().as_deref(),
        Some(XML_RELAX_NG_NAMESPACE)
    );

    let document = optional.owner_document();
    let mut choice = document.create_element("choice", Some(XML_RELAX_NG_NAMESPACE.into()))?;
    let mut children = optional.first_child();
    while let Some(child) = children {
        children = child.next_sibling();
        choice.append_child(child)?;
    }

    choice.append_child(document.create_element("empty", Some(XML_RELAX_NG_NAMESPACE.into()))?)?;

    optional.insert_previous_sibling(&choice)?;
    optional.detach()?;
    *optional = choice;

    Ok(())
}

/// Decompose `zeroOrMore` to 'choice' element with two children.
///
/// `zero_or_more` is replaced to replacement 'choice' element.
///
/// # Reference
/// ISO/IEC 19757-2:2008 7.16 `zeroOrMore` element
fn decompose_zero_or_more(zero_or_more: &mut Element) -> Result<(), XMLError> {
    assert_eq!(zero_or_more.local_name().as_ref(), "zeroOrMore");
    assert_eq!(
        zero_or_more.namespace_name().as_deref(),
        Some(XML_RELAX_NG_NAMESPACE)
    );

    let document = zero_or_more.owner_document();
    let mut one_or_more =
        document.create_element("oneOrMore", Some(XML_RELAX_NG_NAMESPACE.into()))?;
    let mut children = zero_or_more.first_child();
    while let Some(child) = children {
        children = child.next_sibling();
        one_or_more.append_child(child)?;
    }

    let mut choice = document.create_element("choice", Some(XML_RELAX_NG_NAMESPACE.into()))?;
    choice.append_child(one_or_more)?;
    choice.append_child(document.create_element("empty", Some(XML_RELAX_NG_NAMESPACE.into()))?)?;

    zero_or_more.insert_previous_sibling(&choice)?;
    zero_or_more.detach()?;
    *zero_or_more = choice;

    Ok(())
}

fn remove_foreign_element_child(element: &Element) -> Result<(), XMLError> {
    let mut children = element.first_child();
    while let Some(mut child) = children {
        children = child.next_sibling();

        if let Some(element) = child.as_element()
            && element
                .namespace_name()
                .as_deref()
                .is_none_or(|ns| ns != XML_RELAX_NG_NAMESPACE)
        {
            child.detach()?;
        }
    }
    Ok(())
}

fn remove_foreign_attribute(element: &mut Element) -> Result<(), XMLError> {
    if let Some(base_uri) = element.base_uri() {
        let base_uri = base_uri
            .as_unescaped_str()
            .unwrap_or(Cow::Borrowed(base_uri.as_escaped_str()));
        element.remove_attribute("base", Some(XML_XML_NAMESPACE));
        element.set_attribute("xml:base", Some(XML_XML_NAMESPACE), Some(&base_uri))?;
    }

    let mut atts = vec![];
    for att in element.attributes() {
        // Although the specification permits removing `xml:base`,
        // it is retained because there is no appropriate way to preserve the base URI.
        //
        // Additionally, namespace declarations are retained for use in resolving QName prefixes.
        if att
            .namespace_name()
            .as_deref()
            .is_some_and(|ns| ns != XML_RELAX_NG_NAMESPACE)
            && att.name().as_ref() != "xml:base"
            && att.prefix().as_deref() != Some("xmlns")
            && att.name().as_ref() != "xmlns"
        {
            atts.push(att);
        }
    }

    for att in atts {
        element.remove_attribute_node(att)?;
    }

    Ok(())
}

fn remove_whitespace_child(element: &Element) -> Result<(), XMLError> {
    let mut children = element.first_child();
    while let Some(child) = children {
        children = child.next_sibling();

        if let Some(mut text) = child.as_text() {
            if text
                .data()
                .chars()
                .all(|c| XMLVersion::default().is_whitespace(c))
            {
                text.detach()?;
            }
        } else if let Some(mut text) = child.as_cdata_section()
            && text
                .data()
                .chars()
                .all(|c| XMLVersion::default().is_whitespace(c))
        {
            text.detach()?;
        }
    }
    Ok(())
}

fn validate_qname(s: &str) -> Result<(), XMLError> {
    if XMLVersion::default().validate_qname(s) {
        Ok(())
    } else {
        Err(XMLError::RngParseInvalidQName)
    }
}

fn validate_ncname(s: &str) -> Result<(), XMLError> {
    if XMLVersion::default().validate_ncname(s) {
        Ok(())
    } else {
        Err(XMLError::RngParseInvalidNCName)
    }
}

fn validate_uri(s: &str) -> Result<(), XMLError> {
    if URIString::parse(s).is_ok() {
        Ok(())
    } else {
        Err(XMLError::RngParseInvalidAnyURI)
    }
}

fn validate_combine(s: &str) -> Result<(), XMLError> {
    if matches!(s, "choice" | "interleave") {
        Ok(())
    } else {
        Err(XMLError::RngParseUnacceptableCombine)
    }
}

#[cfg(test)]
mod tests {
    use crate::sax::handler::DefaultSAXHandler;

    use super::*;

    #[test]
    fn simplification_tests() {
        let base_uri = URIString::parse_file_path(env!("CARGO_MANIFEST_DIR")).unwrap();

        let path =
            base_uri.resolve(&URIString::parse("resources/relaxng/spec-example.rng").unwrap());
        eprintln!("{}", path.as_unescaped_str().unwrap());
        let schema = RelaxNGSchema::parse_uri(path, None, None::<DefaultSAXHandler>).unwrap();

        let schema = format!("{}", schema.grammar);
        assert_eq!(
            schema,
            r#"<grammar xmlns="http://relaxng.org/ns/structure/1.0"><start><ref name="define_alias0"/></start><define name="define_alias0"><element><name ns="">foo</name><group><ref name="define_alias1"/><ref name="define_alias2"/></group></element></define><define name="define_alias1"><element><name ns="http://www.example.com/n1">bar1</name><empty/></element></define><define name="define_alias2"><element><name ns="http://www.example.com/n2">bar2</name><empty/></element></define></grammar>"#
        );
    }
}
