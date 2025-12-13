use std::{
    borrow::Cow,
    collections::{BTreeMap, HashMap, HashSet},
    io::Read,
    sync::Arc,
};

use crate::{
    XML_NS_NAMESPACE, XML_XML_NAMESPACE, XMLVersion,
    error::XMLError,
    sax::{
        handler::{ErrorHandler, SAXHandler},
        parser::XMLReaderBuilder,
    },
    tree::{
        Attribute, Document, Element, Namespace, Node, NodeType, TreeBuildHandler,
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

    pub fn validate(&self, element: &Element) -> Result<(), XMLError> {
        self.grammar.validate(element)
    }
}

pub trait RelaxNGDatatypeLibrary: Send + Sync {
    /// If a type named `type_name` exists in the library, return `true`,
    /// otherwise return `false`.
    fn contains(&self, type_name: &str) -> bool;
    /// If a type named `type_name` exists in the library, return [`Some`] wrapped around
    /// [`true`] if `value` is a valid representation of that type, or [`false`] otherwise.
    ///
    /// Even if a type exists, if the arguments sufficient for determination are not included
    /// in `params`, [`None`] is returned.
    ///
    /// If a type named `type_name` does not exist in the library, return [`None`].
    ///
    /// # Reference
    /// ISO/IEC 19757-2:2008 9.3.8 data and value pattern
    fn validate(
        &self,
        type_name: &str,
        params: &HashMap<String, String>,
        value: &str,
    ) -> Option<bool>;
    /// If a type named `type_name` exists in the library, return [`Some`] wrapped around
    /// [`true`] if `params` is a valid parameter list of that type, or [`false`] otherwise.
    ///
    /// If a type named `type_name` does not exist in the library, return [`None`].
    fn validate_params(&self, type_name: &str, params: &HashMap<String, String>) -> Option<bool>;
    /// If a type named `type_name` exists in the library, return [`Some`] wrapped around
    /// [`true`] if `lhs` and `rhs` are equal as representations of that type,
    /// or [`false`] if they are not equal.
    ///
    /// If a type named `type_name` does not exist in the library, returns [`None`].
    ///
    /// # Reference
    /// ISO/IEC 19757-2:2008 9.3.8 data and value pattern
    fn eq(&self, type_name: &str, lhs: &str, rhs: &str) -> Option<bool>;
}

pub struct RelaxNGBuiltinDatatypeLibrary;

impl RelaxNGDatatypeLibrary for RelaxNGBuiltinDatatypeLibrary {
    fn contains(&self, type_name: &str) -> bool {
        matches!(type_name, "string" | "token")
    }

    fn validate(
        &self,
        type_name: &str,
        _params: &HashMap<String, String>,
        _value: &str,
    ) -> Option<bool> {
        match type_name {
            "string" | "token" => Some(true),
            _ => None,
        }
    }

    fn validate_params(&self, type_name: &str, params: &HashMap<String, String>) -> Option<bool> {
        self.contains(type_name).then_some(params.is_empty())
    }

    fn eq(&self, type_name: &str, lhs: &str, rhs: &str) -> Option<bool> {
        match type_name {
            "string" => Some(lhs == rhs),
            "token" => {
                let mut lhs = lhs
                    .split(|c: char| XMLVersion::default().is_whitespace(c))
                    .filter(|s| !s.is_empty());
                let mut rhs = rhs
                    .split(|c: char| XMLVersion::default().is_whitespace(c))
                    .filter(|s| !s.is_empty());

                Some(
                    lhs.by_ref().zip(rhs.by_ref()).all(|(lhs, rhs)| lhs == rhs)
                        && lhs.next().is_none()
                        && rhs.next().is_none(),
                )
            }
            _ => None,
        }
    }
}

#[derive(Clone)]
pub struct RelaxNGDatatypeLibraries {
    map: HashMap<URIString, Arc<dyn RelaxNGDatatypeLibrary>>,
}

impl RelaxNGDatatypeLibraries {
    fn get(&self, namespace_name: &URIStr) -> Option<&dyn RelaxNGDatatypeLibrary> {
        self.map.get(namespace_name).map(|library| &**library)
    }

    fn insert(
        &mut self,
        namespace_name: URIString,
        library: Arc<dyn RelaxNGDatatypeLibrary>,
    ) -> Option<Arc<dyn RelaxNGDatatypeLibrary>> {
        self.map.insert(namespace_name, library)
    }
}

impl Default for RelaxNGDatatypeLibraries {
    fn default() -> Self {
        let mut libraries = Self {
            map: HashMap::new(),
        };
        libraries.insert(
            URIString::parse("").unwrap(),
            Arc::new(RelaxNGBuiltinDatatypeLibrary),
        );
        libraries
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
        self.last_error?;
        let mut grammar = self
            .document
            .create_element("grammar", Some(XML_RELAX_NG_NAMESPACE.into()))?;
        let mut start = self
            .document
            .create_element("start", Some(XML_RELAX_NG_NAMESPACE.into()))?;
        start.append_child(document_element)?;
        grammar.append_child(start)?;
        self.document.append_child(&grammar)?;
        let mut num_define = 0;
        self.flatten_grammar(
            &mut grammar,
            &mut handler,
            &mut HashMap::new(),
            &mut num_define,
        )?;
        self.last_error?;
        // ISO/IEC 19757-2:2008 7.20 `define` and `ref` elements
        self.normalize_define_and_ref(
            &mut grammar.clone(),
            &mut grammar,
            &mut handler,
            &mut num_define,
            false,
            false,
        )?;
        self.last_error?;
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
        self.last_error?;
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
        self.last_error?;
        let mut grammar = RelaxNGGrammar::try_from(grammar)?;
        grammar.libraries = self.datatype_libraries.clone();
        grammar.verify_content_type()?;
        grammar.verify_attribute_uniqueness()?;
        grammar.verify_attribute_repeat()?;
        grammar.verify_element_name_uniqueness()?;
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
                remove_foreign_attribute(element)?;
                // The child string of `value` must not be removed, even if it contains only whitespaces.
                // (ISO/IEC 19757-2:2008 7.3 Whitespace)

                self.check_attribute_constraint(element, handler, [], [("type", validate_ncname)])?;

                // ISO/IEC 19757-2:2008 7.5 `type` attribute of `value` element
                if !element.has_attribute("type", None) {
                    element.set_attribute("type", None, Some("token"))?;
                    element.remove_attribute("datatypeLibrary", None);
                    element.set_attribute("datatypeLibrary", None, Some(""))?;
                }

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
                        self.check_attribute_constraint(
                            element,
                            handler,
                            [],
                            [("name", validate_qname)],
                        )?;
                        let mut children = element.first_child();

                        // ISO/IEC 19757-2:2008 7.9 `name` attribute of `element` and `attribute` elements
                        if let Some(name) = element.get_attribute_node("name", None) {
                            let document = element.owner_document();
                            let mut name_element = document
                                .create_element("name", Some(XML_RELAX_NG_NAMESPACE.into()))?;
                            name_element.append_child(
                                document.create_text(
                                    name.value()
                                        .trim_matches(|c| XMLVersion::default().is_whitespace(c)),
                                ),
                            )?;
                            if let Some(first) = children.as_mut() {
                                first.insert_previous_sibling(&name_element)?;
                            } else {
                                element.append_child(&name_element)?;
                            }
                            children = Some(name_element.into());
                            element.remove_attribute_node(name)?;
                        }

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
                        self.check_attribute_constraint(
                            element,
                            handler,
                            [],
                            [("name", validate_qname)],
                        )?;
                        let mut children = element.first_child();

                        // ISO/IEC 19757-2:2008 7.9 `name` attribute of `element` and `attribute` elements
                        if let Some(name) = element.get_attribute_node("name", None) {
                            let document = element.owner_document();
                            let mut name_element = document
                                .create_element("name", Some(XML_RELAX_NG_NAMESPACE.into()))?;
                            name_element.append_child(
                                document.create_text(
                                    name.value()
                                        .trim_matches(|c| XMLVersion::default().is_whitespace(c)),
                                ),
                            )?;

                            if !element.has_attribute("ns", None) {
                                name_element.set_attribute("ns", None, Some(""))?;
                            }

                            if let Some(first) = children.as_mut() {
                                first.insert_previous_sibling(&name_element)?;
                            } else {
                                element.append_child(&name_element)?;
                            }
                            children = Some(name_element.into());
                            element.remove_attribute_node(name)?;
                        }

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
                                    "A {:?} must not be present at this position as a child of 'attribute'.",
                                    child.node_type()
                                );
                                child.detach()?;
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

                        // ISO/IEC 19757-2:2008 7.18 `combine` attribute
                        self.bundle_start_and_define(element, handler)?;
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

                Ok(())
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
                Ok(())
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

                Ok(())
            }
            _ => {
                fatal_error!(
                    self,
                    handler,
                    RngParseUnacceptablePattern,
                    "The element '{}' does not match to a nameClass element.",
                    element.local_name()
                );
                Err(XMLError::RngParseUnacceptablePattern)
            }
        }
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
        let pattern = external_ref.owner_document().import_node(document_element);
        external_ref.insert_previous_sibling(&pattern)?;
        external_ref.detach()?;
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
        // Since `external_ref` is a descendant of `Document`, `base_uri` will never fail.
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
        while let Some(mut child) = children {
            let mut remove = false;
            if let Some(element) = child.as_element() {
                if start && element.local_name().as_ref() == "start" {
                    remove = true;
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

        // "f the include element has a define component,
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
                div.set_attribute(&att.name(), None, Some(&att.value()))?;
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
        include.insert_previous_sibling(&div)?;
        include.detach()?;

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
        parent_define: &mut HashMap<String, (String, usize, Element)>,
        num_define: &mut usize,
    ) -> Result<(), XMLError> {
        assert_eq!(grammar.local_name().as_ref(), "grammar");
        assert_eq!(
            grammar.namespace_name().as_deref(),
            Some(XML_RELAX_NG_NAMESPACE)
        );

        let mut start = None;
        let mut in_scope_define = HashMap::new();
        let mut children = grammar.first_child();
        while let Some(child) = children {
            children = child.next_sibling();

            if let Some(mut element) = child.as_element() {
                if element.local_name().as_ref() == "define" {
                    if let Some(name) = element.remove_attribute("name", None) {
                        let alias = format!("define_alias{}", num_define);
                        *num_define += 1;
                        element.set_attribute("name", None, Some(&alias))?;
                        in_scope_define.insert(name, (alias, 0, element));
                    }
                } else if element.local_name().as_ref() == "start" {
                    start = Some(element);
                }
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

        // search 'ref', 'parentRef' and 'grammar' that is reachable
        let mut seen = HashSet::new();
        let mut nt = vec![start.clone()];
        while let Some(start) = nt.pop() {
            let mut children = start.first_child();
            while let Some(mut child) = children {
                if let Some(mut element) = child.as_element() {
                    match element.local_name().as_ref() {
                        "grammar" => {
                            if let Some(next) = child.next_sibling() {
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

                            self.flatten_grammar(
                                &mut element,
                                handler,
                                &mut in_scope_define,
                                num_define,
                            )?;
                            continue;
                        }
                        local_name @ ("ref" | "parentRef") => match local_name {
                            "ref" => {
                                if let Some(name) = element.get_attribute("name", None) {
                                    if let Some(define) = in_scope_define.get_mut(&name) {
                                        element.remove_attribute("name", None);
                                        element.set_attribute("name", None, Some(&define.0))?;
                                        define.1 += 1;
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
                                        let mut r#ref = self.document.create_element(
                                            "ref",
                                            Some(XML_RELAX_NG_NAMESPACE.into()),
                                        )?;
                                        r#ref.remove_attribute("name", None);
                                        r#ref.set_attribute("name", None, Some(&define.0))?;
                                        element.insert_previous_sibling(&r#ref)?;
                                        element.detach()?;
                                        child = r#ref.into();
                                        define.1 += 1;
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

            for define in in_scope_define
                .iter()
                .filter(|&def| def.1.1 > 0 && !seen.contains(def.0) && seen.insert(def.0.clone()))
                .map(|def| def.1.2.clone())
            {
                nt.push(define);
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
        for (_, (alias, count, mut define)) in in_scope_define {
            define.detach()?;
            if count > 0 {
                master_grammar.append_child(&define)?;
                self.defines.insert(alias, define);
            }
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
    fn normalize_define_and_ref<Handler: ErrorHandler>(
        &mut self,
        grammar: &mut Element,
        context_node: &mut Element,
        handler: &mut Handler,
        num_define: &mut usize,
        mut start: bool,
        mut element: bool,
    ) -> Result<(), XMLError> {
        let mut expand = false;
        let mut children = context_node.first_child();
        match context_node.local_name().as_ref() {
            "start" => start = true,
            "element" => {
                if context_node
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
                    grammar.append_child(define)?;
                    *context_node = r#ref;
                }
                element = true;
            }
            "ref" => {
                expand = start || element;
            }
            _ => {}
        }

        while let Some(child) = children {
            children = child.next_sibling();
            if let Some(mut context_node) = child.as_element() {
                self.normalize_define_and_ref(
                    grammar,
                    &mut context_node,
                    handler,
                    num_define,
                    start,
                    element,
                )?;
            }
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

        for att in element.attributes() {
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
                                fatal_error!(
                                    self,
                                    handler,
                                    RngParseUnacceptableAttribute,
                                    "A 'name' element at the descendant of the first child of an 'attribute' element shall not have an 'ns' attribute with value '{}'.",
                                    XML_NS_NAMESPACE
                                );
                            }
                        }
                    }
                    "nsName" => {
                        if let Some(ns) = element.get_attribute("ns", None)
                            && ns == XML_NS_NAMESPACE
                        {
                            fatal_error!(
                                self,
                                handler,
                                RngParseUnacceptableAttribute,
                                "A 'nsName' element at the descendant of the first child of an 'attribute' element shall not have an 'ns' attribute with value '{}'.",
                                XML_NS_NAMESPACE
                            );
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
        let combine = combine_start.unwrap_or_else(|| "choice".to_owned());
        let mut start = document.create_element("start", Some(XML_RELAX_NG_NAMESPACE.into()))?;
        let mut combine = document.create_element(combine, Some(XML_RELAX_NG_NAMESPACE.into()))?;
        grammar.append_child(&start)?;
        start.append_child(&combine)?;
        combine.append_child(frag_start)?;
        group_children(&mut combine)?;

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
        "define" | "oneOrMore" | "zeroOrMoe" | "optional" | "list" | "mixed" => {
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
            // There is exactly one child
            if element.last_child().unwrap().is_same_node(&first) {
                first.detach()?;
                element.insert_previous_sibling(&first)?;
                element.detach()?;
                *element = first.as_element().ok_or(XMLError::RngParseUnknownError)?;
                return Ok(());
            }

            let name = element.local_name().clone();
            let document = element.owner_document();
            while let Some(second) = first.next_sibling() {
                let mut group =
                    document.create_element(name.as_ref(), Some(XML_RELAX_NG_NAMESPACE.into()))?;
                first.insert_previous_sibling(&group)?;
                group.append_child(first)?;
                group.append_child(second)?;
                first = group.into();
            }
        }
        _ => unreachable!(),
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
        element.remove_attribute("xml:base", None);
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
            && (att.prefix().as_deref() == Some("xmlns") || att.name().as_ref() == "xmlns")
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

/// # Reference
/// - ISO/IEC 19757-2:2008 4.2.3 Expressions
///     - Define the relationship between the maximum and minimum values for content types
/// - ISO/IEC 19757-2:2008 10.3 String sequences
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum ContentType {
    Empty = 0,
    Complex = 1,
    Simple = 2,
}

impl ContentType {
    fn groupable(&self, other: ContentType) -> bool {
        match self {
            ContentType::Empty => true,
            ContentType::Complex => matches!(other, ContentType::Empty | ContentType::Complex),
            ContentType::Simple => matches!(other, ContentType::Empty),
        }
    }
}

struct RelaxNGGrammar {
    /// If the child of 'start' is 'notAllowed', this field is `None`, otherwise `Some`.
    start: Option<RelaxNGPattern>,
    define: BTreeMap<String, RelaxNGDefine>,
    libraries: RelaxNGDatatypeLibraries,
}

impl RelaxNGGrammar {
    /// # Reference
    /// ISO/IEC 19757-2:2008 10.3 String sequences
    fn verify_content_type(&self) -> Result<(), XMLError> {
        for define in self.define.values() {
            define.verify_content_type()?;
        }
        Ok(())
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 10.4 Restrictions on attributes
    fn verify_attribute_uniqueness(&self) -> Result<(), XMLError> {
        if let Some(start) = self.start.as_ref() {
            start.verify_attribute_uniqueness(&mut vec![])?;
        }
        for define in self.define.values() {
            define.verify_attribute_uniqueness()?;
        }
        Ok(())
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 10.4 Restrictions on attributes
    fn verify_attribute_repeat(&self) -> Result<(), XMLError> {
        if let Some(start) = self.start.as_ref() {
            start.verify_attribute_with_infinite_name_class(false)?;
        }
        for define in self.define.values() {
            define.verify_attribute_with_infinite_name_class()?;
        }
        Ok(())
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 10.5 Restrictions on `interleave`
    fn verify_element_name_uniqueness(&self) -> Result<(), XMLError> {
        if let Some(start) = self.start.as_ref() {
            start.verify_element_name_uniqueness(self, &mut vec![], &mut 0)?;
        }
        for define in self.define.values() {
            define.verify_element_name_uniqueness(self)?;
        }
        Ok(())
    }

    /// Start to validate `element` using this grammar.
    ///
    /// # Reference
    /// ISO/IEC 19757-2:2008 9.4 Validity
    fn validate(&self, element: &Element) -> Result<(), XMLError> {
        let start = self.start.as_ref().ok_or(XMLError::RngValidNotAllowed)?;
        start.validate_element(element, self)
    }
}

impl TryFrom<Element> for RelaxNGGrammar {
    type Error = XMLError;

    fn try_from(grammar: Element) -> Result<Self, Self::Error> {
        if grammar.local_name().as_ref() != "grammar" {
            return Err(XMLError::RngParseUnknownError);
        }

        let mut start = None;
        let mut define = BTreeMap::new();
        let mut children = grammar.first_child();
        while let Some(child) = children {
            children = child.next_sibling();

            let element = child.as_element().ok_or(XMLError::RngParseUnknownError)?;
            match element.local_name().as_ref() {
                "start" => {
                    let top = element
                        .first_child()
                        .and_then(|ch| ch.as_element())
                        .ok_or(XMLError::RngParseUnknownError)?;
                    if top.local_name().as_ref() != "notAllowed" {
                        start = Some(RelaxNGPattern::try_from(top)?);
                    }
                }
                "define" => {
                    let name = element
                        .get_attribute("name", None)
                        .ok_or(XMLError::RngParseUnknownError)?;
                    define.insert(name, RelaxNGDefine::try_from(element)?);
                }
                _ => return Err(XMLError::RngParseUnknownError),
            }
        }

        Ok(Self {
            start,
            define,
            libraries: RelaxNGDatatypeLibraries::default(),
        })
    }
}

impl std::fmt::Display for RelaxNGGrammar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "<grammar xmlns=\"http://relaxng.org/ns/structure/1.0\"><start>"
        )?;
        if let Some(top) = self.start.as_ref() {
            write!(f, "{top}")?;
        } else {
            write!(f, "<notAllowed/>")?;
        }
        write!(f, "</start>")?;
        for define in self.define.values() {
            write!(f, "{define}")?;
        }
        write!(f, "</grammar>")
    }
}

struct RelaxNGDefine {
    name: Box<str>,
    name_class: RelaxNGNameClass,
    /// If the second child of 'element' is 'notAllowed', this field is `None`, otherwise `Some`.
    top: Option<RelaxNGPattern>,
}

impl RelaxNGDefine {
    /// # Reference
    /// ISO/IEC 19757-2:2008 10.3 String sequences
    fn verify_content_type(&self) -> Result<(), XMLError> {
        let top = self
            .top
            .as_ref()
            .ok_or(XMLError::RngParseUngroupablePattern)?;

        if top.verify_content_type().is_some() {
            Ok(())
        } else {
            Err(XMLError::RngParseUngroupablePattern)
        }
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 10.4 Restrictions on attributes
    fn verify_attribute_uniqueness(&self) -> Result<(), XMLError> {
        if let Some(top) = self.top.as_ref() {
            top.verify_attribute_uniqueness(&mut vec![])
        } else {
            Ok(())
        }
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 10.4 Restrictions on attributes
    fn verify_attribute_with_infinite_name_class(&self) -> Result<(), XMLError> {
        if let Some(top) = self.top.as_ref() {
            top.verify_attribute_with_infinite_name_class(false)
        } else {
            Ok(())
        }
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 10.5 Restrictions on `interleave`
    fn verify_element_name_uniqueness(&self, grammar: &RelaxNGGrammar) -> Result<(), XMLError> {
        if let Some(top) = self.top.as_ref() {
            top.verify_element_name_uniqueness(grammar, &mut vec![], &mut 0)
        } else {
            Ok(())
        }
    }

    /// # Reference
    /// - ISO/IEC 19757-2:2008 9.3.7 `element` and `attribute` pattern
    /// - ISO/IEC 19757-2:2008 9.4 Validity
    fn validate_element(
        &self,
        element: &Element,
        grammar: &RelaxNGGrammar,
    ) -> Result<(), XMLError> {
        let namespace_name = element.namespace_name();
        let local_name = element.local_name();
        if !self
            .name_class
            .try_match(&local_name, namespace_name.as_deref().unwrap_or_default())
        {
            return Err(XMLError::RngValidElement);
        }

        let attributes = element.attributes().collect::<Vec<_>>();
        let sequence = collect_child_sequence(element);

        let top = self.top.as_ref().ok_or(XMLError::RngValidNotAllowed)?;
        let mut attr_matches = vec![false; attributes.len()];
        let mut seq_matches = vec![false; sequence.len()];
        top.validate(
            &attributes,
            &mut attr_matches,
            &sequence,
            &mut seq_matches,
            grammar,
            true,
        )?;

        if attr_matches.into_iter().all(|b| b) && seq_matches.into_iter().all(|b| b) {
            Ok(())
        } else {
            Err(XMLError::RngValidElement)
        }
    }
}

impl TryFrom<Element> for RelaxNGDefine {
    type Error = XMLError;

    fn try_from(define: Element) -> Result<Self, Self::Error> {
        let element = define
            .first_child()
            .and_then(|ch| ch.as_element())
            .filter(|elem| elem.local_name().as_ref() == "element")
            .ok_or(XMLError::RngParseUnknownError)?;

        let name = define
            .get_attribute("name", None)
            .ok_or(XMLError::RngParseUnknownError)?
            .into();
        let name_class = element
            .first_child()
            .and_then(|ch| ch.as_element())
            .ok_or(XMLError::RngParseUnknownError)?;
        let top = element
            .last_child()
            .and_then(|ch| ch.as_element())
            .ok_or(XMLError::RngParseUnknownError)?;

        if name_class
            .next_sibling()
            .is_none_or(|next| !top.is_same_node(next))
        {
            return Err(XMLError::RngParseUnknownError);
        }

        if top.local_name().as_ref() == "notAllowed" {
            Ok(Self {
                name,
                name_class: name_class.try_into()?,
                top: None,
            })
        } else {
            Ok(Self {
                name,
                name_class: name_class.try_into()?,
                top: Some(top.try_into()?),
            })
        }
    }
}

impl std::fmt::Display for RelaxNGDefine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "<define name=\"{}\"><element>{}",
            self.name, self.name_class
        )?;
        if let Some(top) = self.top.as_ref() {
            write!(f, "{top}")?
        } else {
            write!(f, "<notAllowed/>")?
        }
        write!(f, "</element></define>")
    }
}

struct RelaxNGPattern {
    /// If 'pattern' is 'empty', this field is `None`, otherwise `Some`.
    pattern: Option<RelaxNGNonEmptyPattern>,
}

impl RelaxNGPattern {
    /// # Reference
    /// ISO/IEC 19757-2:2008 10.3 String sequences
    fn verify_content_type(&self) -> Option<ContentType> {
        if let Some(pattern) = self.pattern.as_ref() {
            pattern.verify_content_type()
        } else {
            Some(ContentType::Empty)
        }
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 10.4 Restrictions on attributes
    fn verify_attribute_uniqueness<'a>(
        &'a self,
        name_classes: &mut Vec<&'a RelaxNGNameClass>,
    ) -> Result<(), XMLError> {
        if let Some(pattern) = self.pattern.as_ref() {
            pattern.verify_attribute_uniqueness(name_classes)
        } else {
            Ok(())
        }
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 10.4 Restrictions on attributes
    fn verify_attribute_with_infinite_name_class(&self, repeat: bool) -> Result<(), XMLError> {
        if let Some(pattern) = self.pattern.as_ref() {
            pattern.verify_attribute_with_infinite_name_class(repeat)
        } else {
            Ok(())
        }
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 10.5 Restrictions on `interleave`
    fn verify_element_name_uniqueness<'a>(
        &'a self,
        grammar: &'a RelaxNGGrammar,
        name_classes: &mut Vec<&'a RelaxNGNameClass>,
        text_count: &mut i32,
    ) -> Result<(), XMLError> {
        if let Some(pattern) = self.pattern.as_ref() {
            pattern.verify_element_name_uniqueness(grammar, name_classes, text_count)
        } else {
            Ok(())
        }
    }

    /// If `weak` is `true`, this method tries weak-matching.
    ///
    /// # Reference
    /// - ISO/IEC 19757-2:2008 9.3.7 `element` and `attribute` pattern
    /// - ISO/IEC 19757-2:2008 9.4 Validity
    fn validate(
        &self,
        attributes: &[Attribute],
        attr_matches: &mut [bool],
        sequence: &[Node<dyn NodeSpec>],
        seq_matches: &mut [bool],
        grammar: &RelaxNGGrammar,
        weak: bool,
    ) -> Result<(), XMLError> {
        if let Some(pattern) = self.pattern.as_ref() {
            pattern.validate(
                attributes,
                attr_matches,
                sequence,
                seq_matches,
                grammar,
                weak,
            )
        } else {
            // ISO/IEC 19757-2:2008 9.3.3 `empty` pattern
            if attributes.is_empty() {
                if sequence.is_empty()
                    || (weak
                        && sequence.iter().all(|node| {
                            let ver = XMLVersion::default();
                            node.as_text()
                                .map(|text| text.data().chars().all(|c| ver.is_whitespace(c)))
                                .or_else(|| {
                                    node.as_cdata_section().map(|text| {
                                        text.data().chars().all(|c| ver.is_whitespace(c))
                                    })
                                })
                                .unwrap_or_default()
                        }))
                {
                    Ok(())
                } else {
                    Err(XMLError::RngValidEmpty)
                }
            } else {
                Err(XMLError::RngValidEmpty)
            }
        }
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 9.4 Validity
    fn validate_element(
        &self,
        element: &Element,
        grammar: &RelaxNGGrammar,
    ) -> Result<(), XMLError> {
        if let Some(pattern) = self.pattern.as_ref() {
            pattern.validate_element(element, grammar)
        } else {
            // ISO/IEC 19757-2:2008 9.3.3 `empty` pattern
            Err(XMLError::RngValidEmpty)
        }
    }
}

impl TryFrom<Element> for RelaxNGPattern {
    type Error = XMLError;

    fn try_from(value: Element) -> Result<Self, Self::Error> {
        match value.local_name().as_ref() {
            "empty" => Ok(Self { pattern: None }),
            _ => Ok(Self {
                pattern: Some(value.try_into()?),
            }),
        }
    }
}

impl std::fmt::Display for RelaxNGPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(pattern) = self.pattern.as_ref() {
            write!(f, "{pattern}")
        } else {
            write!(f, "<empty/>")
        }
    }
}

enum RelaxNGNonEmptyPattern {
    Text,
    Data {
        type_name: Box<str>,
        datatype_library: Box<URIStr>,
        param: Vec<RelaxNGParam>,
        except_pattern: Option<Box<RelaxNGExceptPattern>>,
    },
    Value {
        type_name: Box<str>,
        datatype_library: Box<URIStr>,
        ns: Box<str>,
        value: Box<str>,
    },
    List {
        pattern: Box<RelaxNGPattern>,
    },
    Attribute {
        name_class: RelaxNGNameClass,
        pattern: Box<RelaxNGPattern>,
    },
    Ref {
        name: Box<str>,
    },
    OneOrMore {
        pattern: Box<RelaxNGNonEmptyPattern>,
    },
    Choice {
        left: Box<RelaxNGPattern>,
        right: Box<RelaxNGNonEmptyPattern>,
    },
    Group {
        pattern: [Box<RelaxNGNonEmptyPattern>; 2],
    },
    Interleave {
        pattern: [Box<RelaxNGNonEmptyPattern>; 2],
    },
}

impl RelaxNGNonEmptyPattern {
    /// # Reference
    /// ISO/IEC 19757-2:2008 10.3 String sequences
    fn verify_content_type(&self) -> Option<ContentType> {
        match self {
            Self::Text => Some(ContentType::Complex),
            Self::Data { except_pattern, .. } => {
                if let Some(pattern) = except_pattern {
                    pattern
                        .pattern
                        .verify_content_type()
                        .map(|_| ContentType::Simple)
                } else {
                    Some(ContentType::Simple)
                }
            }
            Self::Value { .. } => Some(ContentType::Simple),
            Self::List { .. } => Some(ContentType::Simple),
            Self::Attribute { pattern, .. } => {
                pattern.verify_content_type().map(|_| ContentType::Empty)
            }
            Self::Ref { .. } => Some(ContentType::Complex),
            Self::OneOrMore { pattern } => pattern
                .verify_content_type()
                .filter(|ct| !matches!(ct, ContentType::Simple)),
            Self::Choice { left, right } => {
                let ct1 = left.verify_content_type()?;
                let ct2 = right.verify_content_type()?;
                Some(ct1.max(ct2))
            }
            Self::Group { pattern } | Self::Interleave { pattern } => {
                let ct1 = pattern[0].verify_content_type()?;
                let ct2 = pattern[1].verify_content_type()?;
                ct1.groupable(ct2).then(|| ct1.max(ct2))
            }
        }
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 10.4 Restrictions on attributes
    fn verify_attribute_uniqueness<'a>(
        &'a self,
        name_classes: &mut Vec<&'a RelaxNGNameClass>,
    ) -> Result<(), XMLError> {
        match self {
            Self::Text | Self::Data { .. } | Self::Value { .. } | Self::Ref { .. } => Ok(()),
            Self::List { pattern } => {
                // It is necessary to inspect the attributes of list descendants,
                // but since they must not be inherited by ancestors, `name_classes`
                // must not be passed.
                //
                // ```
                // A pattern p1 is defined to occur in a pattern p2 if
                // - p1 is p2, or
                // - p2 is a choice, interleave, group or oneOrMore element
                //   and p1 occurs in one or more children of p2.
                // ```
                pattern.verify_attribute_uniqueness(&mut vec![])
            }
            Self::Attribute { name_class, .. } => {
                name_classes.push(name_class);
                Ok(())
            }
            Self::OneOrMore { pattern } => pattern.verify_attribute_uniqueness(name_classes),
            Self::Choice { left, right } => {
                left.verify_attribute_uniqueness(name_classes)?;
                right.verify_attribute_uniqueness(name_classes)?;
                Ok(())
            }
            Self::Group { pattern } | Self::Interleave { pattern } => {
                let mut buf = vec![];
                pattern[0].verify_attribute_uniqueness(&mut buf)?;
                pattern[1].verify_attribute_uniqueness(&mut buf)?;

                for (i, &l) in buf.iter().enumerate() {
                    for &r in buf.iter().skip(i + 1) {
                        if l.has_non_empty_intersection(r) {
                            return Err(XMLError::RngParseConflictAttributeNameClass);
                        }
                    }
                }

                name_classes.extend(buf);
                Ok(())
            }
        }
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 10.4 Restrictions on attributes
    fn verify_attribute_with_infinite_name_class(&self, repeat: bool) -> Result<(), XMLError> {
        match self {
            Self::Text | Self::Data { .. } | Self::Value { .. } | Self::Ref { .. } => Ok(()),
            Self::List { pattern } => pattern.verify_attribute_with_infinite_name_class(repeat),
            Self::Attribute {
                name_class,
                pattern,
            } => {
                let has_infinite_name_class = name_class.has_infinite_name_class();
                if has_infinite_name_class && !repeat {
                    Err(XMLError::RngParseUnrepeatedAttributeWithInfiniteNameClass)
                } else if has_infinite_name_class
                    && !matches!(pattern.pattern, Some(RelaxNGNonEmptyPattern::Text))
                {
                    Err(XMLError::RngParseUnacceptablePattern)
                } else {
                    Ok(())
                }
            }
            Self::OneOrMore { pattern } => pattern.verify_attribute_with_infinite_name_class(true),
            Self::Choice { left, right } => {
                left.verify_attribute_with_infinite_name_class(repeat)?;
                right.verify_attribute_with_infinite_name_class(repeat)?;
                Ok(())
            }
            Self::Group { pattern } | Self::Interleave { pattern } => {
                pattern[0].verify_attribute_with_infinite_name_class(repeat)?;
                pattern[1].verify_attribute_with_infinite_name_class(repeat)?;
                Ok(())
            }
        }
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 10.5 Restrictions on `interleave`
    fn verify_element_name_uniqueness<'a>(
        &'a self,
        grammar: &'a RelaxNGGrammar,
        name_classes: &mut Vec<&'a RelaxNGNameClass>,
        text_count: &mut i32,
    ) -> Result<(), XMLError> {
        match self {
            Self::Text => {
                *text_count += 1;
                Ok(())
            }
            Self::Data { .. } | Self::Value { .. } | Self::Attribute { .. } => Ok(()),
            Self::List { pattern } => {
                // It is necessary to inspect the attributes of list descendants,
                // but since they must not be inherited by ancestors, `name_classes`
                // must not be passed.
                //
                // ```
                // A pattern p1 is defined to occur in a pattern p2 if
                // - p1 is p2, or
                // - p2 is a choice, interleave, group or oneOrMore element
                //   and p1 occurs in one or more children of p2.
                // ```
                pattern.verify_element_name_uniqueness(grammar, &mut vec![], &mut 0)
            }
            Self::Ref { name } => {
                let define = grammar
                    .define
                    .get(name.as_ref())
                    .ok_or(XMLError::RngParseUnknownError)?;
                name_classes.push(&define.name_class);
                Ok(())
            }
            Self::OneOrMore { pattern } => {
                pattern.verify_element_name_uniqueness(grammar, name_classes, text_count)
            }
            Self::Choice { left, right } => {
                left.verify_element_name_uniqueness(grammar, name_classes, text_count)?;
                right.verify_element_name_uniqueness(grammar, name_classes, text_count)?;
                Ok(())
            }
            Self::Group { pattern } => {
                pattern[0].verify_element_name_uniqueness(grammar, name_classes, text_count)?;
                pattern[1].verify_element_name_uniqueness(grammar, name_classes, text_count)?;
                Ok(())
            }
            Self::Interleave { pattern } => {
                let mut buf = vec![];
                let mut count = 0;
                pattern[0].verify_element_name_uniqueness(grammar, &mut buf, &mut count)?;
                pattern[1].verify_element_name_uniqueness(grammar, &mut buf, &mut count)?;

                if count > 1 {
                    return Err(XMLError::RngParseConflictAttributeNameClass);
                }
                for (i, &l) in buf.iter().enumerate() {
                    for &r in buf.iter().skip(i + 1) {
                        if l.has_non_empty_intersection(r) {
                            return Err(XMLError::RngParseConflictAttributeNameClass);
                        }
                    }
                }

                name_classes.extend(buf);
                *text_count += count;
                Ok(())
            }
        }
    }

    /// If `weak` is `true`, this method tries weak-matching.
    ///
    /// # Reference
    /// - ISO/IEC 19757-2:2008 9.3.7 `element` and `attribute` pattern
    /// - ISO/IEC 19757-2:2008 9.4 Validity
    fn validate(
        &self,
        attributes: &[Attribute],
        attr_matches: &mut [bool],
        sequence: &[Node<dyn NodeSpec>],
        seq_matches: &mut [bool],
        grammar: &RelaxNGGrammar,
        weak: bool,
    ) -> Result<(), XMLError> {
        match self {
            Self::Text => {
                if sequence
                    .iter()
                    .all(|node| matches!(node.node_type(), NodeType::CDATASection | NodeType::Text))
                {
                    Ok(())
                } else {
                    Err(XMLError::RngValidText)
                }
            }
            Self::Data {
                type_name,
                datatype_library,
                param,
                except_pattern,
            } => {
                if seq_matches.len() != 1 {
                    return Err(XMLError::RngValidData);
                }
                let value = &sequence[0].text_content();

                let params = param
                    .iter()
                    .map(|param| (param.name.to_string(), param.value.to_string()))
                    .collect::<HashMap<_, _>>();

                if let Some(library) = grammar.libraries.get(datatype_library)
                    && library
                        .validate(type_name, &params, value)
                        .unwrap_or_default()
                {
                    if let Some(except) = except_pattern.as_ref() {
                        if except
                            .pattern
                            .validate(
                                attributes,
                                attr_matches,
                                sequence,
                                seq_matches,
                                grammar,
                                weak,
                            )
                            .is_err()
                        {
                            seq_matches[0] = true;
                            Ok(())
                        } else {
                            seq_matches[0] = false;
                            Err(XMLError::RngValidData)
                        }
                    } else {
                        seq_matches[0] = true;
                        Ok(())
                    }
                } else {
                    seq_matches[0] = false;
                    Err(XMLError::RngValidData)
                }
            }
            Self::Value {
                type_name,
                datatype_library,
                value,
                ..
            } => {
                if seq_matches.len() != 1 {
                    return Err(XMLError::RngValidValue);
                }
                let lhs = &sequence[0].text_content();

                if let Some(library) = grammar.libraries.get(datatype_library)
                    && library.eq(type_name, lhs, value).unwrap_or_default()
                {
                    seq_matches[0] = true;
                    Ok(())
                } else {
                    seq_matches[0] = false;
                    Err(XMLError::RngValidValue)
                }
            }
            Self::List { pattern } => {
                if !attributes.is_empty() {
                    return Err(XMLError::RngValidList);
                }

                if sequence.is_empty() {
                    return pattern.validate(
                        attributes,
                        attr_matches,
                        sequence,
                        seq_matches,
                        grammar,
                        weak,
                    );
                }

                let document = sequence[0].owner_document();
                let mut strings = String::new();
                for node in sequence {
                    match node.downcast() {
                        NodeKind::Text(text) => strings.push_str(&text.data()),
                        NodeKind::CDATASection(cdata) => strings.push_str(&cdata.data()),
                        _ => return Err(XMLError::RngValidList),
                    }
                }

                let mut sequence = vec![];
                for token in strings
                    .split(|c| XMLVersion::default().is_whitespace(c))
                    .filter(|s| !s.is_empty())
                {
                    sequence.push(document.create_text(token).into());
                }
                pattern.validate(&[], attr_matches, &sequence, seq_matches, grammar, weak)
            }
            Self::Attribute {
                name_class,
                pattern,
            } => {
                for (i, attr) in attributes.iter().enumerate() {
                    let namespace_name = attr.namespace_name();
                    let local_name = attr.local_name();
                    let value = collect_child_sequence(attr);
                    if name_class
                        .try_match(&local_name, namespace_name.as_deref().unwrap_or_default())
                        && pattern
                            .validate(
                                &[],
                                &mut [],
                                &value,
                                &mut vec![false; value.len()],
                                grammar,
                                true,
                            )
                            .is_ok()
                    {
                        attr_matches[i] = true;
                        return Ok(());
                    }
                }
                Err(XMLError::RngValidAttribute)
            }
            Self::Ref { .. } => {
                if !attributes.is_empty() || sequence.len() != 1 {
                    return Err(XMLError::RngValidRef);
                }
                let element = sequence[0].as_element().ok_or(XMLError::RngValidRef)?;
                self.validate_element(&element, grammar)?;
                seq_matches[0] = true;
                Ok(())
            }
            Self::OneOrMore { pattern } => {
                pattern.handle_one_or_more(attributes, sequence, grammar, weak)?;
                attr_matches.iter_mut().for_each(|b| *b = true);
                seq_matches.iter_mut().for_each(|b| *b = true);
                Ok(())
            }
            Self::Choice { left, right } => {
                let mut am = attr_matches.to_owned();
                let mut sm = seq_matches.to_owned();
                let left = left.validate(attributes, &mut am, sequence, &mut sm, grammar, weak);
                if left.is_ok() {
                    attr_matches
                        .iter_mut()
                        .zip(am)
                        .filter(|v| v.1)
                        .for_each(|v| *v.0 = true);
                    seq_matches
                        .iter_mut()
                        .zip(sm)
                        .filter(|v| v.1)
                        .for_each(|v| *v.0 = true);
                    return Ok(());
                }
                right.validate(
                    attributes,
                    attr_matches,
                    sequence,
                    seq_matches,
                    grammar,
                    weak,
                )
            }
            Self::Group { pattern } => {
                pattern[0].handle_group(&pattern[1], attributes, sequence, grammar, weak)?;
                attr_matches.iter_mut().for_each(|b| *b = true);
                seq_matches.iter_mut().for_each(|b| *b = true);
                Ok(())
            }
            Self::Interleave { pattern } => {
                let mut left = vec![];
                let mut right = vec![];
                for node in sequence {
                    match node.downcast() {
                        NodeKind::Element(element) => {
                            if pattern[0].contains_element_name(
                                element.namespace_name().as_deref().unwrap(),
                                &element.local_name(),
                                grammar,
                            ) {
                                left.push(node.clone());
                            } else {
                                right.push(node.clone());
                            }
                        }
                        _ => {
                            if pattern[0].contains_element_name("", "#text", grammar) {
                                left.push(node.clone());
                            } else {
                                right.push(node.clone());
                            }
                        }
                    }
                }

                let mut lm = vec![false; left.len()];
                pattern[0].validate(attributes, attr_matches, &left, &mut lm, grammar, weak)?;
                let mut rm = vec![false; right.len()];
                pattern[1].validate(attributes, attr_matches, &right, &mut rm, grammar, weak)?;

                if lm.into_iter().any(|m| !m) || rm.into_iter().any(|m| !m) {
                    return Err(XMLError::RngValidInterleave);
                }

                seq_matches.iter_mut().for_each(|m| *m = true);
                Ok(())
            }
        }
    }

    fn handle_one_or_more(
        &self,
        attributes: &[Attribute],
        sequence: &[Node<dyn NodeSpec>],
        grammar: &RelaxNGGrammar,
        weak: bool,
    ) -> Result<(), XMLError> {
        for mid in (1.min(sequence.len())..=sequence.len()).rev() {
            let (front, back) = sequence.split_at(mid);
            let mut attr_matches = vec![false; attributes.len()];
            let mut seq_matches = vec![false; front.len()];
            if self
                .validate(
                    attributes,
                    &mut attr_matches,
                    front,
                    &mut seq_matches,
                    grammar,
                    weak,
                )
                .is_ok()
            {
                if seq_matches.into_iter().any(|m| !m) {
                    continue;
                }
                if mid == sequence.len() && attr_matches.iter().all(|&a| a) {
                    return Ok(());
                }
                let attributes = attributes
                    .iter()
                    .zip(attr_matches)
                    .filter_map(|v| (!v.1).then_some(v.0.clone()))
                    .collect::<Vec<_>>();
                if self
                    .handle_one_or_more(&attributes, back, grammar, weak)
                    .is_ok()
                {
                    return Ok(());
                }
            }
        }

        Err(XMLError::RngValidOneOrMore)
    }

    fn handle_group(
        &self,
        other: &Self,
        attributes: &[Attribute],
        sequence: &[Node<dyn NodeSpec>],
        grammar: &RelaxNGGrammar,
        weak: bool,
    ) -> Result<(), XMLError> {
        for mid in (1.min(sequence.len())..=sequence.len()).rev() {
            let (front, back) = sequence.split_at(mid);
            let mut attr_matches = vec![false; attributes.len()];
            let mut seq_matches = vec![false; sequence.len()];
            if self
                .validate(
                    attributes,
                    &mut attr_matches,
                    front,
                    &mut seq_matches[..front.len()],
                    grammar,
                    weak,
                )
                .is_ok()
            {
                if seq_matches.iter().take(front.len()).any(|&m| !m) {
                    continue;
                }
                if other
                    .validate(
                        attributes,
                        &mut attr_matches,
                        back,
                        &mut seq_matches[front.len()..],
                        grammar,
                        weak,
                    )
                    .is_ok()
                {
                    return Ok(());
                }
            }
        }

        Err(XMLError::RngValidOneOrMore)
    }

    fn contains_element_name(
        &self,
        namespace_name: &str,
        local_name: &str,
        grammar: &RelaxNGGrammar,
    ) -> bool {
        match self {
            Self::Text => namespace_name.is_empty() && local_name == "#text",
            Self::Data { .. } | Self::Value { .. } | Self::Attribute { .. } | Self::List { .. } => {
                false
            }
            Self::Ref { name } => {
                local_name != "#text"
                    && grammar.define.get(name.as_ref()).is_some_and(|define| {
                        define.name_class.try_match(local_name, namespace_name)
                    })
            }
            Self::OneOrMore { pattern } => {
                pattern.contains_element_name(namespace_name, local_name, grammar)
            }
            Self::Choice { left, right } => {
                left.pattern.as_ref().is_some_and(|left| {
                    left.contains_element_name(namespace_name, local_name, grammar)
                }) || right.contains_element_name(namespace_name, local_name, grammar)
            }
            Self::Group { pattern } | Self::Interleave { pattern } => {
                pattern[0].contains_element_name(namespace_name, local_name, grammar)
                    || pattern[1].contains_element_name(namespace_name, local_name, grammar)
            }
        }
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 9.4 Validity
    fn validate_element(
        &self,
        element: &Element,
        grammar: &RelaxNGGrammar,
    ) -> Result<(), XMLError> {
        match self {
            Self::Text => Err(XMLError::RngValidText),
            Self::Data { .. } => Err(XMLError::RngValidData),
            Self::Value { .. } => Err(XMLError::RngValidValue),
            Self::List { .. } => Err(XMLError::RngValidList),
            Self::Attribute { .. } => Err(XMLError::RngValidAttribute),
            Self::Ref { name } => {
                let define = grammar
                    .define
                    .get(name.as_ref())
                    .ok_or(XMLError::RngValidRef)?;
                define.validate_element(element, grammar)
            }
            Self::OneOrMore { pattern } => pattern.validate_element(element, grammar),
            Self::Choice { left, right } => left
                .validate_element(element, grammar)
                .or_else(|_| right.validate_element(element, grammar)),
            Self::Group { .. } => Err(XMLError::RngValidGroup),
            Self::Interleave { .. } => Err(XMLError::RngValidInterleave),
        }
    }
}

impl TryFrom<Element> for RelaxNGNonEmptyPattern {
    type Error = XMLError;

    fn try_from(value: Element) -> Result<Self, Self::Error> {
        match value.local_name().as_ref() {
            "text" => Ok(Self::Text),
            "data" => {
                let mut param = vec![];
                let mut children = value.first_child();
                while let Some(child) = children {
                    children = child.next_sibling();

                    param.push(
                        child
                            .as_element()
                            .ok_or(XMLError::RngParseUnknownError)?
                            .try_into()?,
                    );
                }

                if children
                    .as_ref()
                    .is_some_and(|ch| !value.last_child().unwrap().is_same_node(ch))
                {
                    return Err(XMLError::RngParseUnknownError);
                }

                Ok(Self::Data {
                    type_name: value
                        .get_attribute("type", None)
                        .ok_or(XMLError::RngParseUnknownError)?
                        .into(),
                    datatype_library: URIString::parse(
                        value
                            .get_attribute("datatypeLibrary", None)
                            .ok_or(XMLError::RngParseUnknownError)?,
                    )?
                    .into(),
                    param,
                    except_pattern: children
                        .and_then(|ch| ch.as_element())
                        .map(RelaxNGExceptPattern::try_from)
                        .transpose()?
                        .map(|ch| ch.into()),
                })
            }
            "value" => Ok(Self::Value {
                type_name: value
                    .get_attribute("type", None)
                    .ok_or(XMLError::RngParseUnknownError)?
                    .into(),
                datatype_library: URIString::parse(
                    value
                        .get_attribute("datatypeLibrary", None)
                        .ok_or(XMLError::RngParseUnknownError)?,
                )?
                .into(),
                ns: value
                    .get_attribute("ns", None)
                    .ok_or(XMLError::RngParseUnknownError)?
                    .into(),
                value: value
                    .first_child()
                    .map(|ch| ch.text_content())
                    .ok_or(XMLError::RngParseUnknownError)?
                    .into(),
            }),
            "list" => Ok(Self::List {
                pattern: Box::new(
                    value
                        .first_child()
                        .and_then(|ch| ch.as_element())
                        .ok_or(XMLError::RngParseUnknownError)?
                        .try_into()?,
                ),
            }),
            "attribute" => Ok(Self::Attribute {
                name_class: value
                    .first_child()
                    .and_then(|ch| ch.as_element())
                    .ok_or(XMLError::RngParseUnknownError)?
                    .try_into()?,
                pattern: Box::new(
                    value
                        .last_child()
                        .and_then(|ch| ch.as_element())
                        .ok_or(XMLError::RngParseUnknownError)?
                        .try_into()?,
                ),
            }),
            "ref" => Ok(Self::Ref {
                name: value
                    .get_attribute("name", None)
                    .ok_or(XMLError::RngParseUnknownError)?
                    .into(),
            }),
            "oneOrMore" => Ok(Self::OneOrMore {
                pattern: Box::new(
                    value
                        .first_child()
                        .and_then(|ch| ch.as_element())
                        .ok_or(XMLError::RngParseUnknownError)?
                        .try_into()?,
                ),
            }),
            "choice" => Ok(Self::Choice {
                left: Box::new(
                    value
                        .first_child()
                        .and_then(|ch| ch.as_element())
                        .ok_or(XMLError::RngParseUnknownError)?
                        .try_into()?,
                ),
                right: Box::new(
                    value
                        .last_child()
                        .and_then(|ch| ch.as_element())
                        .ok_or(XMLError::RngParseUnknownError)?
                        .try_into()?,
                ),
            }),
            "group" => Ok(Self::Group {
                pattern: [
                    Box::new(
                        value
                            .first_child()
                            .and_then(|ch| ch.as_element())
                            .ok_or(XMLError::RngParseUnknownError)?
                            .try_into()?,
                    ),
                    Box::new(
                        value
                            .last_child()
                            .and_then(|ch| ch.as_element())
                            .ok_or(XMLError::RngParseUnknownError)?
                            .try_into()?,
                    ),
                ],
            }),
            "interleave" => Ok(Self::Interleave {
                pattern: [
                    Box::new(
                        value
                            .first_child()
                            .and_then(|ch| ch.as_element())
                            .ok_or(XMLError::RngParseUnknownError)?
                            .try_into()?,
                    ),
                    Box::new(
                        value
                            .last_child()
                            .and_then(|ch| ch.as_element())
                            .ok_or(XMLError::RngParseUnknownError)?
                            .try_into()?,
                    ),
                ],
            }),
            _ => Err(XMLError::RngParseUnknownError),
        }
    }
}

impl std::fmt::Display for RelaxNGNonEmptyPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Text => write!(f, "<text/>"),
            Self::Data {
                type_name,
                datatype_library,
                param,
                except_pattern,
            } => {
                write!(
                    f,
                    "<data type=\"{type_name}\" datatypeLibrary=\"{}\">",
                    datatype_library
                        .as_unescaped_str()
                        .as_deref()
                        .unwrap_or(datatype_library.as_escaped_str())
                )?;
                for param in param {
                    write!(f, "{param}")?;
                }
                if let Some(except) = except_pattern {
                    write!(f, "{except}")?;
                }
                write!(f, "</data>")
            }
            Self::Value {
                type_name,
                datatype_library,
                ns,
                value,
            } => {
                write!(
                    f,
                    "<value datatypeLibrary=\"{}\" type=\"{type_name}\" ns=\"{ns}\">{value}</value>",
                    datatype_library
                        .as_unescaped_str()
                        .as_deref()
                        .unwrap_or(datatype_library.as_escaped_str())
                )
            }
            Self::List { pattern } => {
                write!(f, "<list>{pattern}</list>")
            }
            Self::Attribute {
                name_class,
                pattern,
            } => write!(f, "<attribute>{name_class}{pattern}</attribute>"),
            Self::Ref { name } => write!(f, "<ref name=\"{name}\"/>"),
            Self::OneOrMore { pattern } => write!(f, "<oneOrMore>{pattern}</oneOrMore>"),
            Self::Choice { left, right } => write!(f, "<choice>{left}{right}</choice>"),
            Self::Group { pattern } => write!(f, "<group>{}{}</group>", pattern[0], pattern[1]),
            Self::Interleave { pattern } => {
                write!(f, "<interleave>{}{}</interleave>", pattern[0], pattern[1])
            }
        }
    }
}

struct RelaxNGParam {
    name: Box<str>,
    value: Box<str>,
}

impl TryFrom<Element> for RelaxNGParam {
    type Error = XMLError;

    fn try_from(value: Element) -> Result<Self, Self::Error> {
        if value.local_name().as_ref() != "param" {
            return Err(XMLError::RngParseUnknownError);
        }

        Ok(Self {
            name: value
                .get_attribute("name", None)
                .ok_or(XMLError::RngParseUnknownError)?
                .into(),
            value: value
                .first_child()
                .map(|ch| ch.text_content())
                .ok_or(XMLError::RngParseUnknownError)?
                .into(),
        })
    }
}

impl std::fmt::Display for RelaxNGParam {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<param name=\"{}\">{}</param>", self.name, self.value)
    }
}

struct RelaxNGExceptPattern {
    pattern: RelaxNGPattern,
}

impl TryFrom<Element> for RelaxNGExceptPattern {
    type Error = XMLError;

    fn try_from(value: Element) -> Result<Self, Self::Error> {
        if value.local_name().as_ref() != "except" {
            return Err(XMLError::RngParseUnknownError);
        }

        Ok(Self {
            pattern: value
                .first_child()
                .and_then(|ch| ch.as_element())
                .ok_or(XMLError::RngParseUnknownError)?
                .try_into()?,
        })
    }
}

impl std::fmt::Display for RelaxNGExceptPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<except>{}</except>", self.pattern)
    }
}

enum RelaxNGNameClass {
    AnyName {
        except: Option<RelaxNGExceptNameClass>,
    },
    NsName {
        ns: Box<str>,
        except: Option<RelaxNGExceptNameClass>,
    },
    Name {
        ns: Box<str>,
        value: Box<str>,
    },
    Choice {
        name_class: [Box<RelaxNGNameClass>; 2],
    },
}

impl RelaxNGNameClass {
    /// Try to match the pair of `local_name` and `namespace_name` to nameClass.
    ///
    /// This method does not verify if `local_name` is a valid NCName and not verify
    /// `namespace_name` is a valid namespace name.
    ///
    /// If successfully match, return `true`, otherwise return `false`.
    fn try_match(&self, local_name: &str, namespace_name: &str) -> bool {
        match self {
            RelaxNGNameClass::AnyName { except } => except
                .as_ref()
                .map(|name_class| !name_class.name_class.try_match(local_name, namespace_name))
                .unwrap_or(true),
            RelaxNGNameClass::NsName { ns, except } => {
                namespace_name == &**ns
                    && except
                        .as_ref()
                        .map(|name_class| {
                            !name_class.name_class.try_match(local_name, namespace_name)
                        })
                        .unwrap_or(true)
            }
            RelaxNGNameClass::Name { ns, value } => {
                namespace_name == &**ns && local_name == &**value
            }
            RelaxNGNameClass::Choice { name_class } => {
                name_class[0].try_match(local_name, namespace_name)
                    || name_class[1].try_match(local_name, namespace_name)
            }
        }
    }

    /// Check if two nameClasses have non-empty intersection of acceptable name set.
    ///
    /// If have, return `true`, otherwise return `false`.
    fn has_non_empty_intersection(&self, other: &Self) -> bool {
        fn _has_non_empty_intersection(lhs: &RelaxNGNameClass, rhs: &RelaxNGNameClass) -> bool {
            match lhs {
                RelaxNGNameClass::AnyName { except } => {
                    // Both `local_name` and `namesace_name` are invalid names,
                    // but if `rhs` contains anyName, it should match.
                    if !rhs.try_match("*", "*") {
                        return false;
                    }
                    if let Some(except) = except.as_ref() {
                        return !_has_non_empty_intersection(&except.name_class, rhs);
                    }
                    true
                }
                RelaxNGNameClass::NsName { ns, except } => {
                    // `local_name` is a invalid name, but if `rhs` contains anyName or nsName
                    // with the namespace name as same as `ns`, it should match.
                    if !rhs.try_match("*", ns) {
                        return false;
                    }
                    if let Some(except) = except.as_ref() {
                        return !_has_non_empty_intersection(&except.name_class, rhs);
                    }
                    true
                }
                RelaxNGNameClass::Name { ns, value } => rhs.try_match(value, ns),
                RelaxNGNameClass::Choice { name_class } => {
                    _has_non_empty_intersection(&name_class[0], rhs)
                        || _has_non_empty_intersection(&name_class[1], rhs)
                }
            }
        }

        _has_non_empty_intersection(self, other) || _has_non_empty_intersection(other, self)
    }

    /// Check if `self` or its descendant nameClass is `anyName` or `nsName`.
    fn has_infinite_name_class(&self) -> bool {
        match self {
            Self::AnyName { .. } | Self::NsName { .. } => true,
            Self::Name { .. } => false,
            Self::Choice { name_class } => {
                name_class[0].has_infinite_name_class() || name_class[1].has_infinite_name_class()
            }
        }
    }
}

impl TryFrom<Element> for RelaxNGNameClass {
    type Error = XMLError;

    fn try_from(value: Element) -> Result<Self, Self::Error> {
        match value.local_name().as_ref() {
            "anyName" => Ok(Self::AnyName {
                except: value
                    .first_child()
                    .and_then(|ch| ch.as_element())
                    .map(RelaxNGExceptNameClass::try_from)
                    .transpose()?,
            }),
            "nsName" => Ok(Self::NsName {
                ns: value
                    .get_attribute("ns", None)
                    .ok_or(XMLError::RngParseUnknownError)?
                    .into(),
                except: value
                    .first_child()
                    .and_then(|ch| ch.as_element())
                    .map(RelaxNGExceptNameClass::try_from)
                    .transpose()?,
            }),
            "name" => Ok(Self::Name {
                ns: value
                    .get_attribute("ns", None)
                    .ok_or(XMLError::RngParseUnknownError)?
                    .into(),
                value: value
                    .first_child()
                    .map(|ch| ch.text_content())
                    .ok_or(XMLError::RngParseUnknownError)?
                    .into(),
            }),
            "choice" => Ok(Self::Choice {
                name_class: [
                    Box::new(
                        value
                            .first_child()
                            .and_then(|ch| ch.as_element())
                            .ok_or(XMLError::RngParseUnknownError)?
                            .try_into()?,
                    ),
                    Box::new(
                        value
                            .last_child()
                            .and_then(|ch| ch.as_element())
                            .ok_or(XMLError::RngParseUnknownError)?
                            .try_into()?,
                    ),
                ],
            }),
            _ => Err(XMLError::RngParseUnknownError),
        }
    }
}

impl std::fmt::Display for RelaxNGNameClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::AnyName { except } => {
                write!(f, "<anyName>")?;
                if let Some(except) = except {
                    write!(f, "{except}")?;
                }
                write!(f, "</anyName>")
            }
            Self::NsName { ns, except } => {
                write!(f, "<nsName ns=\"{ns}\">")?;
                if let Some(except) = except {
                    write!(f, "{except}")?;
                }
                write!(f, "</nsName>")
            }
            Self::Name { ns, value } => write!(f, "<name ns=\"{ns}\">{value}</name>"),
            Self::Choice { name_class } => {
                write!(f, "<choice>{}{}</choice>", name_class[0], name_class[1])
            }
        }
    }
}

struct RelaxNGExceptNameClass {
    name_class: Box<RelaxNGNameClass>,
}

impl TryFrom<Element> for RelaxNGExceptNameClass {
    type Error = XMLError;

    fn try_from(value: Element) -> Result<Self, Self::Error> {
        if value.local_name().as_ref() != "except" {
            return Err(XMLError::RngParseUnknownError);
        }

        Ok(Self {
            name_class: Box::new(
                value
                    .first_child()
                    .and_then(|ch| ch.as_element())
                    .ok_or(XMLError::RngParseUnknownError)?
                    .try_into()?,
            ),
        })
    }
}

impl std::fmt::Display for RelaxNGExceptNameClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<except>{}</except>", self.name_class)
    }
}

fn collect_child_sequence(node: &Node<impl NodeSpec>) -> Vec<Node<dyn NodeSpec>> {
    let mut sequence = vec![];
    let mut children = node.first_child();
    let mut depth = 0;
    while let Some(child) = children {
        if matches!(
            child.node_type(),
            NodeType::Element | NodeType::Text | NodeType::CDATASection
        ) {
            sequence.push(child.clone());
        }
        if matches!(child.node_type(), NodeType::EntityReference)
            && let Some(first) = child.first_child()
        {
            depth += 1;
            children = Some(first);
        } else {
            children = child.next_sibling();
            if children.is_none() && depth > 0 {
                let mut now = child;
                while depth > 0
                    && let Some(parent) = now.parent_node()
                {
                    depth -= 1;
                    if let Some(next) = parent.next_sibling() {
                        children = Some(next);
                        break;
                    }
                    now = parent.into();
                }
            }
        }
    }
    sequence
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
