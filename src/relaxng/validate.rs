use std::{collections::BTreeMap, sync::Arc};

use crate::{
    XML_NS_NAMESPACE, XMLVersion,
    error::XMLError,
    relaxng::{
        Grammar, RelaxNGSchema,
        grammar::{Context, Datatype, LocalName, ParamList, Pattern, PatternId, Uri},
    },
    sax::{
        self, AttributeType, DefaultDecl, Locator, NamespaceStack,
        attributes::Attributes,
        contentspec::ContentSpec,
        error::SAXParseError,
        handler::{EntityResolver, ErrorHandler, SAXHandler},
        source::InputSource,
    },
    tree::{Element, convert::NodeKind},
    uri::{URIStr, URIString},
};

impl RelaxNGSchema {
    /// Validate XML document subtree whose root element is `element`.
    pub fn validate<H: SAXHandler>(
        &mut self,
        element: &Element,
        handler: H,
    ) -> Result<(), XMLError> {
        let mut handler = self.new_validate_handler(handler);
        if let Some(base_uri) = element.base_uri() {
            handler.set_document_locator(Arc::new(Locator::new(base_uri.into(), None, 0, 0)));
        } else {
            handler.set_document_locator(Arc::new(Locator::new(
                element.owner_document().document_base_uri().as_ref().into(),
                None,
                0,
                0,
            )));
        }
        handler.start_document();
        handler.validate_element(element)?;
        handler.end_document();
        handler.last_error.map_err(|e| e.error)
    }
}

macro_rules! generic_error {
    ($method:ident, $handler:expr, $code:expr, $level:expr, $message:literal, $( $args:expr ),*) => {
        #[allow(unused)]
        use $crate::error::XMLError::*;
        let ret = $crate::sax::error::SAXParseError {
            error: $code,
            level: $level,
            domain: $crate::error::XMLErrorDomain::RngValid,
            line: $handler.locator.line(),
            column: $handler.locator.column(),
            system_id: $handler.locator.system_id(),
            public_id: $handler.locator.public_id(),
            message: ::std::borrow::Cow::Owned(format!($message, $( $args ),*)),
        };
        $handler.last_error = Err(ret.clone());
        $handler.$method(ret);
    };
    ($method:ident, $handler:expr, $code:expr, $level:expr, $message:literal) => {
        #[allow(unused)]
        use $crate::error::XMLError::*;
        let ret = $crate::sax::error::SAXParseError {
            error: $code,
            level: $level,
            domain: $crate::error::XMLErrorDomain::RngValid,
            line: $handler.locator.line(),
            column: $handler.locator.column(),
            system_id: $handler.locator.system_id(),
            public_id: $handler.locator.public_id(),
            message: ::std::borrow::Cow::Borrowed($message),
        };
        $handler.last_error = Err(ret.clone());
        $handler.handler.$method(ret);
    };
    ($method:ident, $handler:expr, $code:expr, $level:expr, $message:expr) => {
        #[allow(unused)]
        use $crate::error::XMLError::*;
        let ret = $crate::sax::error::SAXParseError {
            error: $code,
            level: $level,
            domain: $crate::error::XMLErrorDomain::RngValid,
            line: $handler.locator.line(),
            column: $handler.locator.column(),
            system_id: $handler.locator.system_id(),
            public_id: $handler.locator.public_id(),
            message: ::std::borrow::Cow::Owned($message.into()),
        };
        $handler.last_error = Err(ret.clone());
        $handler.handler.$method(ret);
    };
}

macro_rules! validity_error {
    ($handler:expr, $code:ident, $message:literal, $( $args:expr ),*) => {
        generic_error!(
            error,
            $handler,
            $code,
            $crate::error::XMLErrorLevel::Error,
            $message,
            $( $args ),*
        );
    };
    ($handler:expr, $code:ident, $message:literal) => {
        validity_error!($handler, $code, $message, );
    };
    ($handler:expr, $code:ident, $message:expr) => {
        generic_error!(
            error,
            $handler,
            $code,
            $crate::error::XMLErrorLevel::Error,
            $message
        );
    };
}

pub(super) struct QName(pub(super) Uri, pub(super) LocalName);
pub(super) struct AttributeNode(pub(super) QName, pub(super) Arc<str>);

pub struct ValidateHandler<'a, H: SAXHandler> {
    pub child: H,
    grammar: &'a mut Grammar,

    // current pattern
    pattern: PatternId,

    // context
    locator: Arc<Locator>,
    base_uri: Arc<URIStr>,
    base_uri_stack: Vec<Arc<URIStr>>,
    ns_stack: NamespaceStack,
    pub last_error: Result<(), SAXParseError>,

    // text buffer
    // `text` can be validated as chunked character content,
    // but `value` and `data` cannot be validated correctly
    // unless all character content is collected.
    text: String,

    // Indicates whether the current content is mixed content.
    // It is necessary for weak-match checking.
    mixed: bool,
    mixed_stack: Vec<bool>,
}

impl<'a, H: SAXHandler> ValidateHandler<'a, H> {
    fn validate_element(&mut self, element: &Element) -> Result<(), XMLError> {
        let mut atts = Attributes::new();
        for att in element.attributes() {
            let mut att = sax::attributes::Attribute {
                namespace_name: att.namespace_name().map(|ns| ns.as_ref().into()),
                local_name: Some(att.local_name().as_ref().into()),
                qname: att.name().as_ref().into(),
                value: att.value().into(),
                flag: 0,
            };
            att.set_specified();
            atts.push(att).map_err(|err| err.1)?;
        }
        for ns in element.namespaces() {
            self.start_prefix_mapping(ns.prefix().as_deref(), &ns.namespace_name());

            let mut att = if ns.prefix().is_none() {
                sax::attributes::Attribute {
                    namespace_name: Some(XML_NS_NAMESPACE.into()),
                    local_name: Some("xmlns".into()),
                    qname: "xmlns".into(),
                    value: ns.namespace_name().as_ref().into(),
                    flag: 0,
                }
            } else {
                sax::attributes::Attribute {
                    namespace_name: Some(XML_NS_NAMESPACE.into()),
                    local_name: ns.prefix().as_deref().map(|pre| pre.into()),
                    qname: format!("xmlns:{}", ns.prefix().unwrap()).into(),
                    value: ns.namespace_name().as_ref().into(),
                    flag: 0,
                }
            };
            att.set_specified();
            att.set_nsdecl();
            atts.push(att).map_err(|err| err.1)?;
        }
        self.start_element(
            element.namespace_name().as_deref(),
            Some(&element.local_name()),
            &element.name(),
            &atts,
        );
        self.last_error.clone().map_err(|err| err.error)?;

        let mut children = element.first_child();
        while let Some(child) = children {
            match child.downcast() {
                NodeKind::Element(elem) => {
                    self.validate_element(&elem)?;
                }
                NodeKind::Text(text) => {
                    self.validate_text(&text.data())?;
                }
                NodeKind::CDATASection(cdata) => {
                    self.start_cdata();
                    self.validate_text(&cdata.data())?;
                    self.end_cdata();
                }
                NodeKind::Comment(comment) => {
                    self.comment(&comment.data());
                }
                NodeKind::ProcessingInstruction(pi) => {
                    self.processing_instruction(&pi.target(), pi.data().as_deref());
                }
                NodeKind::EntityReference(ent) => {
                    if let Some(first) = ent.first_child() {
                        children = Some(first);
                        continue;
                    }
                }
                _ => {}
            }

            if let Some(next) = child.next_sibling() {
                children = Some(next);
            } else {
                children = None;
                let mut parent = child.parent_node();
                while let Some(par) = parent.filter(|p| !p.is_same_node(element)) {
                    if let Some(next) = par.next_sibling() {
                        children = Some(next);
                        break;
                    }
                    parent = par.parent_node();
                }
            }
        }

        self.end_element(
            element.namespace_name().as_deref(),
            Some(&element.local_name()),
            &element.name(),
        );
        self.last_error.clone().map_err(|err| err.error)?;
        for ns in element.namespaces() {
            self.end_prefix_mapping(ns.prefix().as_deref());
        }
        Ok(())
    }

    fn validate_text(&mut self, text: &str) -> Result<(), XMLError> {
        self.characters(text);
        self.last_error.clone().map_err(|err| err.error)
    }
}

impl<'a, H: SAXHandler> ErrorHandler for ValidateHandler<'a, H> {
    fn fatal_error(&mut self, error: SAXParseError) {
        self.child.fatal_error(error);
    }

    fn error(&mut self, error: SAXParseError) {
        self.child.error(error);
    }

    fn warning(&mut self, error: SAXParseError) {
        self.child.warning(error);
    }
}
impl<'a, H: SAXHandler> EntityResolver for ValidateHandler<'a, H> {
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

    fn get_external_subset(
        &mut self,
        name: &str,
        base_uri: Option<&URIStr>,
    ) -> Result<InputSource<'static>, XMLError> {
        self.child.get_external_subset(name, base_uri)
    }
}
impl<'a, H: SAXHandler> SAXHandler for ValidateHandler<'a, H> {
    fn set_document_locator(&mut self, locator: Arc<Locator>) {
        self.child.set_document_locator(locator.clone());
        self.base_uri = locator.system_id();
        self.base_uri_stack.clear();
        self.ns_stack.clear();
        self.last_error = Ok(());
        self.locator = locator;
        self.pattern = self.grammar.root;
        self.text.clear();
        self.mixed = false;
        self.mixed_stack.clear();
    }

    fn start_prefix_mapping(&mut self, prefix: Option<&str>, uri: &str) {
        self.ns_stack.push(prefix.unwrap_or_default(), uri);
        self.child.start_prefix_mapping(prefix, uri);
    }
    fn end_prefix_mapping(&mut self, prefix: Option<&str>) {
        // The popped prefix may not necessarily be `prefix`,
        // but the count matches up, so it's fine.
        self.ns_stack.pop();
        self.child.end_prefix_mapping(prefix);
    }

    fn start_element(
        &mut self,
        namespace_name: Option<&str>,
        local_name: Option<&str>,
        qname: &str,
        atts: &Attributes,
    ) {
        if !self.text.is_empty() {
            if !self
                .text
                .chars()
                .all(|c| XMLVersion::default().is_whitespace(c))
            {
                let ns_map = self
                    .ns_stack
                    .iter()
                    .map(|ns| (ns.prefix.clone(), ns.namespace_name.clone()))
                    .collect::<BTreeMap<_, _>>();
                let pattern = self.grammar.text_deriv(
                    &(self.base_uri.clone(), ns_map),
                    self.pattern,
                    &self.text,
                );

                if matches!(self.grammar.patterns[pattern].as_ref(), Pattern::NotAllowed) {
                    validity_error!(
                        self,
                        RngValidText,
                        "A text content '{}' cannot appear before the element '{}'.",
                        self.text,
                        qname
                    );
                } else {
                    self.pattern = pattern;
                }
            }
            self.text.clear();
        }
        let mut base_uri = self.base_uri.clone();
        if let Some(att) = atts.get_value_by_qname("xml:base") {
            if let Ok(uri) = URIString::parse(att) {
                base_uri = base_uri.resolve(&uri).into();
            } else {
                validity_error!(
                    self,
                    RngValidAttribute,
                    "The value '{}' for 'xml:base' attribute is invalid URI reference.",
                    att
                );
            }
        }

        self.base_uri_stack.push(base_uri.clone());
        self.base_uri = base_uri.clone();
        self.mixed_stack.push(true);
        self.mixed = false;

        let ns_map = self
            .ns_stack
            .iter()
            .map(|ns| (ns.prefix.clone(), ns.namespace_name.clone()))
            .collect::<BTreeMap<_, _>>();
        let cx = (base_uri, ns_map);
        let mut p = self.grammar.start_tag_open_deriv(
            self.pattern,
            &QName(
                namespace_name.unwrap_or_default().into(),
                local_name.unwrap_or_default().into(),
            ),
        );
        if matches!(self.grammar.patterns[p].as_ref(), Pattern::NotAllowed) {
            validity_error!(
                self,
                RngValidElement,
                "The element '{}' is not allowed here.",
                qname
            );
        } else {
            for att in atts.iter().filter(|att| !att.is_nsdecl()) {
                let attribute = AttributeNode(
                    QName(
                        att.namespace_name.clone().unwrap_or_default(),
                        att.local_name.clone().unwrap_or_default(),
                    ),
                    att.value.as_ref().into(),
                );
                let np = self.grammar.att_deriv(&cx, p, &attribute);
                if matches!(self.grammar.patterns[np].as_ref(), Pattern::NotAllowed) {
                    validity_error!(
                        self,
                        RngValidAttribute,
                        "The attribute '{}' is not allowed in the element '{}'.",
                        att.qname,
                        qname
                    );
                } else {
                    p = np;
                }
            }
            let p3 = self.grammar.start_tag_close_deriv(p);
            if matches!(self.grammar.patterns[p3].as_ref(), Pattern::NotAllowed) {
                validity_error!(
                    self,
                    RngValidAttribute,
                    "Some attributes are not specified in the element '{}'.",
                    qname
                );
            } else {
                self.pattern = p3;
            }
        }

        self.child
            .start_element(namespace_name, local_name, qname, atts);
    }
    fn end_element(&mut self, namespace_name: Option<&str>, local_name: Option<&str>, qname: &str) {
        if !self.text.is_empty() || !self.mixed {
            let ns_map = self
                .ns_stack
                .iter()
                .map(|ns| (ns.prefix.clone(), ns.namespace_name.clone()))
                .collect::<BTreeMap<_, _>>();
            let p =
                self.grammar
                    .text_deriv(&(self.base_uri.clone(), ns_map), self.pattern, &self.text);
            if self
                .text
                .chars()
                .all(|c| XMLVersion::default().is_whitespace(c))
            {
                self.pattern = self.grammar.choice(self.pattern, p);
            } else if matches!(self.grammar.patterns[p].as_ref(), Pattern::NotAllowed) {
                validity_error!(
                    self,
                    RngValidText,
                    "The text content '{}' is not allowed before eng tag '{}'.",
                    self.text,
                    qname
                );
            } else {
                self.pattern = p;
            }
            self.text.clear();
        }

        self.pattern = self.grammar.end_tag_deriv(self.pattern);
        if matches!(
            self.grammar.patterns[self.pattern].as_ref(),
            Pattern::NotAllowed
        ) {
            validity_error!(
                self,
                RngValidElement,
                "The content of '{}' is insufficient.",
                qname
            );
        }
        if let Some(base_uri) = self.base_uri_stack.pop() {
            self.base_uri = base_uri;
        }
        if let Some(mixed) = self.mixed_stack.pop() {
            self.mixed = mixed;
        }
        self.child.end_element(namespace_name, local_name, qname);
    }

    fn characters(&mut self, data: &str) {
        self.text.push_str(data);
        self.child.characters(data);
    }

    fn end_document(&mut self) {
        if !self.grammar.nullable(self.pattern) && self.last_error.is_ok() {
            validity_error!(
                self,
                RngValidUnknownError,
                "Finish validation unsuccessfully"
            );
        }
        self.child.end_document();
    }

    // Following callbacks are not used for RELAX NG validation.
    // They simply forward received events to the child handler.

    fn attribute_decl(
        &mut self,
        element_name: &str,
        attribute_name: &str,
        attribute_type: &AttributeType,
        default_decl: &DefaultDecl,
    ) {
        self.child
            .attribute_decl(element_name, attribute_name, attribute_type, default_decl);
    }
    fn comment(&mut self, data: &str) {
        self.child.comment(data);
    }
    fn declaration(&mut self, version: &str, encoding: Option<&str>, standalone: Option<bool>) {
        self.child.declaration(version, encoding, standalone);
    }
    fn element_decl(&mut self, name: &str, contentspec: &ContentSpec) {
        self.child.element_decl(name, contentspec);
    }
    fn external_entity_decl(&mut self, name: &str, public_id: Option<&str>, system_id: &URIStr) {
        self.child.external_entity_decl(name, public_id, system_id);
    }
    fn ignorable_whitespace(&mut self, data: &str) {
        self.child.ignorable_whitespace(data);
    }
    fn internal_entity_decl(&mut self, name: &str, value: &str) {
        self.child.internal_entity_decl(name, value);
    }
    fn notation_decl(&mut self, name: &str, public_id: Option<&str>, system_id: Option<&URIStr>) {
        self.child.notation_decl(name, public_id, system_id);
    }
    fn processing_instruction(&mut self, target: &str, data: Option<&str>) {
        self.child.processing_instruction(target, data);
    }
    fn skipped_entity(&mut self, name: &str) {
        self.child.skipped_entity(name);
    }
    fn start_cdata(&mut self) {
        self.child.start_cdata();
    }
    fn end_cdata(&mut self) {
        self.child.end_cdata();
    }
    fn start_document(&mut self) {
        self.child.start_document();
    }
    fn start_dtd(&mut self, name: &str, public_id: Option<&str>, system_id: Option<&URIStr>) {
        self.child.start_dtd(name, public_id, system_id);
    }
    fn end_dtd(&mut self) {
        self.child.end_dtd();
    }
    fn start_entity(&mut self, name: &str) {
        self.child.start_entity(name);
    }
    fn end_entity(&mut self) {
        self.child.end_entity();
    }
    fn unparsed_entity_decl(
        &mut self,
        name: &str,
        public_id: Option<&str>,
        system_id: &URIStr,
        notation_name: &str,
    ) {
        self.child
            .unparsed_entity_decl(name, public_id, system_id, notation_name);
    }
}

impl Grammar {
    pub(super) fn new_validate_handler<H: SAXHandler>(
        &mut self,
        handler: H,
    ) -> ValidateHandler<'_, H> {
        let pattern = self.root;
        ValidateHandler {
            child: handler,
            grammar: self,
            pattern,
            locator: Arc::new(Locator::default()),
            base_uri: URIString::parse("").unwrap().into(),
            base_uri_stack: vec![],
            ns_stack: NamespaceStack::default(),
            last_error: Ok(()),
            text: String::new(),
            mixed: false,
            mixed_stack: vec![],
        }
    }

    pub(super) fn create_node(&mut self, pattern: Pattern) -> PatternId {
        if let Some(&index) = self.intern.get(&pattern) {
            index
        } else {
            let new = Arc::new(pattern);
            let at = self.patterns.len();
            self.intern.insert(new.clone(), at);
            self.patterns.push(new);
            self.nullable.push(-1);
            at
        }
    }

    fn nullable(&mut self, node: PatternId) -> bool {
        if self.nullable[node] >= 0 {
            return self.nullable[node] != 0;
        }
        let ret = match self.patterns[node].as_ref() {
            &Pattern::Group(p1, p2) => self.nullable(p1) && self.nullable(p2),
            &Pattern::Interleave(p1, p2) => self.nullable(p1) && self.nullable(p2),
            &Pattern::Choice(p1, p2) => self.nullable(p1) || self.nullable(p2),
            &Pattern::OneOrMore(p) => self.nullable(p),
            &Pattern::Element(_, _)
            | &Pattern::Attribute(_, _)
            | &Pattern::List(_)
            | &Pattern::Value(_, _, _)
            | &Pattern::Data(_, _)
            | &Pattern::DataExcept(_, _, _)
            | &Pattern::NotAllowed
            | &Pattern::After(_, _) => false,
            &Pattern::Empty | &Pattern::Text => true,
        };
        self.nullable[node] = ret as i8;
        ret
    }

    fn text_deriv(&mut self, context: &Context, pattern: PatternId, string: &str) -> PatternId {
        match (context, self.patterns[pattern].as_ref(), string) {
            (cx, &Pattern::Choice(p1, p2), s) => {
                let p1 = self.text_deriv(cx, p1, s);
                let p2 = self.text_deriv(cx, p2, s);
                self.choice(p1, p2)
            }
            (cx, &Pattern::Interleave(p1, p2), s) => {
                let r1 = self.text_deriv(cx, p1, s);
                let q1 = self.interleave(r1, p2);
                let r2 = self.text_deriv(cx, p2, s);
                let q2 = self.interleave(p1, r2);
                self.choice(q1, q2)
            }
            (cx, &Pattern::Group(p1, p2), s) => {
                let q = self.text_deriv(cx, p1, s);
                let p = self.group(q, p2);
                if self.nullable(p1) {
                    let q = self.text_deriv(cx, p2, s);
                    self.choice(p, q)
                } else {
                    p
                }
            }
            (cx, &Pattern::After(p1, p2), s) => {
                let q = self.text_deriv(cx, p1, s);
                self.after(q, p2)
            }
            (cx, &Pattern::OneOrMore(p), s) => {
                let q1 = self.text_deriv(cx, p, s);
                let r1 = self.create_node(Pattern::OneOrMore(p));
                let r2 = self.create_node(Pattern::Empty);
                let q2 = self.choice(r1, r2);
                self.group(q1, q2)
            }
            (_, Pattern::Text, _) => pattern,
            (cx1, Pattern::Value(dt, value, cx2), s) => {
                if self.datatype_equal(dt, value, cx2, s, cx1) {
                    self.create_node(Pattern::Empty)
                } else {
                    self.create_node(Pattern::NotAllowed)
                }
            }
            (cx, Pattern::Data(dt, params), s) => {
                if self.datatype_allows(dt, params, s, cx) {
                    self.create_node(Pattern::Empty)
                } else {
                    self.create_node(Pattern::NotAllowed)
                }
            }
            (cx, &Pattern::DataExcept(ref dt, ref params, p), s) => {
                if self.datatype_allows(dt, params, s, cx)
                    && let q = self.text_deriv(cx, p, s)
                    && !self.nullable(q)
                {
                    self.create_node(Pattern::Empty)
                } else {
                    self.create_node(Pattern::NotAllowed)
                }
            }
            (cx, &Pattern::List(p), s) => {
                let q = self.list_deriv(
                    cx,
                    p,
                    &s.split(|c| XMLVersion::default().is_whitespace(c))
                        .filter(|s| !s.is_empty())
                        .collect::<Vec<_>>(),
                );
                if self.nullable(q) {
                    self.create_node(Pattern::Empty)
                } else {
                    self.create_node(Pattern::NotAllowed)
                }
            }
            (_, _, _) => self.create_node(Pattern::NotAllowed),
        }
    }

    fn list_deriv(&mut self, cx: &Context, p: PatternId, list: &[&str]) -> PatternId {
        match list {
            [] => p,
            [h, t @ ..] => {
                let p = self.text_deriv(cx, p, h);
                self.list_deriv(cx, p, t)
            }
        }
    }

    fn choice(&mut self, p1: PatternId, p2: PatternId) -> PatternId {
        if p1 == p2 {
            return p1;
        }
        match (self.patterns[p1].as_ref(), self.patterns[p2].as_ref()) {
            (_, Pattern::NotAllowed) => p1,
            (Pattern::NotAllowed, _) => p2,
            (_, _) => self.create_node(Pattern::Choice(p1.min(p2), p1.max(p2))),
        }
    }

    fn group(&mut self, p1: PatternId, p2: PatternId) -> PatternId {
        match (self.patterns[p1].as_ref(), self.patterns[p2].as_ref()) {
            (_, Pattern::NotAllowed) => p2,
            (Pattern::NotAllowed, _) => p1,
            (_, Pattern::Empty) => p1,
            (Pattern::Empty, _) => p2,
            (_, _) => self.create_node(Pattern::Group(p1, p2)),
        }
    }

    fn interleave(&mut self, p1: PatternId, p2: PatternId) -> PatternId {
        match (self.patterns[p1].as_ref(), self.patterns[p2].as_ref()) {
            (_, Pattern::NotAllowed) => p2,
            (Pattern::NotAllowed, _) => p1,
            (_, Pattern::Empty) => p1,
            (Pattern::Empty, _) => p2,
            (_, _) => self.create_node(Pattern::Interleave(p1.min(p2), p1.max(p2))),
        }
    }

    fn one_or_more(&mut self, p: PatternId) -> PatternId {
        match self.patterns[p].as_ref() {
            Pattern::NotAllowed => p,
            _ => self.create_node(Pattern::OneOrMore(p)),
        }
    }

    fn after(&mut self, p1: PatternId, p2: PatternId) -> PatternId {
        match (self.patterns[p1].as_ref(), self.patterns[p2].as_ref()) {
            (_, Pattern::NotAllowed) => p2,
            (Pattern::NotAllowed, _) => p1,
            (_, _) => self.create_node(Pattern::After(p1, p2)),
        }
    }

    fn datatype_allows(
        &self,
        dt: &Datatype,
        params: &ParamList,
        string: &str,
        context: &Context,
    ) -> bool {
        self.libraries
            .get(&dt.0)
            .and_then(|lib| lib.validate(&dt.1, params, string, context))
            .is_some_and(|b| b)
    }

    fn datatype_equal(
        &self,
        dt: &Datatype,
        s1: &str,
        cx1: &Context,
        s2: &str,
        cx2: &Context,
    ) -> bool {
        self.libraries
            .get(&dt.0)
            .and_then(|lib| lib.eq(&dt.1, s1, cx1, s2, cx2))
            .is_some_and(|b| b)
    }

    fn apply_after(
        &mut self,
        f: &impl Fn(&mut Grammar, PatternId) -> PatternId,
        pattern: PatternId,
    ) -> PatternId {
        match *self.patterns[pattern] {
            Pattern::After(p1, p2) => {
                let p2 = f(self, p2);
                self.after(p1, p2)
            }
            Pattern::Choice(p1, p2) => {
                let q1 = self.apply_after(f, p1);
                let q2 = self.apply_after(f, p2);
                self.choice(q1, q2)
            }
            Pattern::NotAllowed => pattern,
            _ => unreachable!(),
        }
    }

    fn start_tag_open_deriv(&mut self, pattern: PatternId, qn: &QName) -> PatternId {
        match *self.patterns[pattern] {
            Pattern::Choice(p1, p2) => {
                let q1 = self.start_tag_open_deriv(p1, qn);
                let q2 = self.start_tag_open_deriv(p2, qn);
                self.choice(q1, q2)
            }
            Pattern::Element(ref nc, p) => {
                if nc.contains(qn) {
                    let r = self.create_node(Pattern::Empty);
                    self.after(p, r)
                } else {
                    self.create_node(Pattern::NotAllowed)
                }
            }
            Pattern::Interleave(p1, p2) => {
                let r = self.start_tag_open_deriv(p1, qn);
                let q1 = self.apply_after(
                    &move |slf: &mut Grammar, p: PatternId| slf.interleave(p, p2),
                    r,
                );
                let r = self.start_tag_open_deriv(p2, qn);
                let q2 =
                    self.apply_after(&move |slf: &mut Grammar, p: usize| slf.interleave(p1, p), r);
                self.choice(q1, q2)
            }
            Pattern::OneOrMore(p) => {
                let q = self.start_tag_open_deriv(p, qn);
                self.apply_after(
                    &move |slf: &mut Grammar, p: usize| {
                        let r = slf.create_node(Pattern::Empty);
                        let q = slf.choice(pattern, r);
                        slf.group(p, q)
                    },
                    q,
                )
            }
            Pattern::Group(p1, p2) => {
                let q = self.start_tag_open_deriv(p1, qn);
                let x = self.apply_after(&move |slf: &mut Grammar, p1: usize| slf.group(p1, p2), q);
                if self.nullable(p1) {
                    let q = self.start_tag_open_deriv(p2, qn);
                    self.choice(x, q)
                } else {
                    x
                }
            }
            Pattern::After(p1, p2) => {
                let q = self.start_tag_open_deriv(p1, qn);
                self.apply_after(&move |slf: &mut Grammar, p1: usize| slf.after(p1, p2), q)
            }
            _ => self.create_node(Pattern::NotAllowed),
        }
    }

    fn att_deriv(&mut self, cx: &Context, pattern: PatternId, att: &AttributeNode) -> PatternId {
        match (self.patterns[pattern].as_ref(), att) {
            (&Pattern::After(p1, p2), att) => {
                let q = self.att_deriv(cx, p1, att);
                self.after(q, p2)
            }
            (&Pattern::Choice(p1, p2), att) => {
                let q1 = self.att_deriv(cx, p1, att);
                let q2 = self.att_deriv(cx, p2, att);
                self.choice(q1, q2)
            }
            (&Pattern::Group(p1, p2), att) => {
                let r1 = self.att_deriv(cx, p1, att);
                let q1 = self.group(r1, p2);
                let r2 = self.att_deriv(cx, p2, att);
                let q2 = self.group(p1, r2);
                self.choice(q1, q2)
            }
            (&Pattern::Interleave(p1, p2), att) => {
                let r1 = self.att_deriv(cx, p1, att);
                let q1 = self.interleave(r1, p2);
                let r2 = self.att_deriv(cx, p2, att);
                let q2 = self.interleave(p1, r2);
                self.choice(q1, q2)
            }
            (&Pattern::OneOrMore(p), att) => {
                let q1 = self.att_deriv(cx, p, att);
                let r = self.create_node(Pattern::Empty);
                let q2 = self.choice(pattern, r);
                self.group(q1, q2)
            }
            (Pattern::Attribute(nc, p), AttributeNode(qn, s)) => {
                if nc.contains(qn) && self.value_match(cx, *p, s) {
                    self.create_node(Pattern::Empty)
                } else {
                    self.create_node(Pattern::NotAllowed)
                }
            }
            (_, _) => self.create_node(Pattern::NotAllowed),
        }
    }

    fn value_match(&mut self, cx: &Context, p: PatternId, s: &str) -> bool {
        (self.nullable(p) && s.chars().all(|c| XMLVersion::default().is_whitespace(c))) || {
            let q = self.text_deriv(cx, p, s);
            self.nullable(q)
        }
    }

    fn start_tag_close_deriv(&mut self, pattern: PatternId) -> PatternId {
        match self.patterns[pattern].as_ref() {
            &Pattern::After(p1, p2) => {
                let p1 = self.start_tag_close_deriv(p1);
                self.after(p1, p2)
            }
            &Pattern::Choice(p1, p2) => {
                let q1 = self.start_tag_close_deriv(p1);
                let q2 = self.start_tag_close_deriv(p2);
                self.choice(q1, q2)
            }
            &Pattern::Group(p1, p2) => {
                let q1 = self.start_tag_close_deriv(p1);
                let q2 = self.start_tag_close_deriv(p2);
                self.group(q1, q2)
            }
            &Pattern::Interleave(p1, p2) => {
                let q1 = self.start_tag_close_deriv(p1);
                let q2 = self.start_tag_close_deriv(p2);
                self.interleave(q1, q2)
            }
            &Pattern::OneOrMore(p) => {
                let q = self.start_tag_close_deriv(p);
                self.one_or_more(q)
            }
            Pattern::Attribute(_, _) => self.create_node(Pattern::NotAllowed),
            _ => pattern,
        }
    }

    fn end_tag_deriv(&mut self, p: PatternId) -> PatternId {
        match *self.patterns[p].as_ref() {
            Pattern::Choice(p1, p2) => {
                let (p1, p2) = (self.end_tag_deriv(p1), self.end_tag_deriv(p2));
                self.choice(p1, p2)
            }
            Pattern::After(p1, p2) => {
                if self.nullable(p1) {
                    p2
                } else {
                    self.create_node(Pattern::NotAllowed)
                }
            }
            _ => self.create_node(Pattern::NotAllowed),
        }
    }
}
