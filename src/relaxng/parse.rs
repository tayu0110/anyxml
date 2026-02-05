use std::sync::Arc;

use anyxml_uri::{rfc2396::validate_rfc2396_absolute_uri, uri::URIString};

use crate::{
    XMLVersion,
    error::XMLError,
    relaxng::XML_RELAX_NG_NAMESPACE,
    sax::{
        AttributeType, DefaultDecl, Locator,
        attributes::{Attribute, Attributes},
        contentspec::ContentSpec,
        error::SAXParseError,
        handler::{DefaultSAXHandler, EntityResolver, ErrorHandler, SAXHandler},
        parser::XMLReaderBuilder,
        source::InputSource,
    },
    uri::URIStr,
};

macro_rules! generic_error {
    ($method:ident, $handler:expr, $code:expr, $level:expr, $message:literal, $( $args:expr ),*) => {
        #[allow(unused)]
        use $crate::error::XMLError::*;
        let ret = $crate::sax::error::SAXParseError {
            error: $code,
            level: $level,
            domain: $crate::error::XMLErrorDomain::RngParser,
            line: $handler.locator.line(),
            column: $handler.locator.column(),
            system_id: $handler.locator.system_id(),
            public_id: $handler.locator.public_id(),
            message: ::std::borrow::Cow::Owned(format!($message, $( $args ),*)),
        };
        $handler.last_error = Err(ret.clone());
        $handler.handler.$method(ret);
    };
    ($method:ident, $handler:expr, $code:expr, $level:expr, $message:literal) => {
        #[allow(unused)]
        use $crate::error::XMLError::*;
        let ret = $crate::sax::error::SAXParseError {
            error: $code,
            level: $level,
            domain: $crate::error::XMLErrorDomain::RngParser,
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
            domain: $crate::error::XMLErrorDomain::RngParser,
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

macro_rules! error {
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
        error!($handler, $code, $message, );
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DivContentType {
    Grammar,
    Include,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExceptType {
    Pattern,
    NameClass,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ChoiceType {
    Pattern,
    NameClass,
}

enum RelaxNGNodeType {
    Element(Option<Box<str>>),
    Attribute(Option<Box<str>>),
    Group,
    Interleave,
    Choice(ChoiceType),
    Optional,
    ZeroOrMore,
    OneOrMore,
    List,
    Mixed,
    Ref(Box<str>),
    ParentRef(Box<str>),
    Empty,
    Text,
    Value {
        r#type: Option<Box<str>>,
        value: Box<str>,
    },
    Data(Box<str>),
    NotAllowed,
    ExternalRef(Box<URIStr>),
    Grammar,
    Param {
        name: Box<str>,
        value: Box<str>,
    },
    Except(ExceptType),
    Start(Option<Box<str>>),
    Define {
        name: Box<str>,
        combine: Option<Box<str>>,
    },
    Div(DivContentType),
    Include(Box<URIStr>),
    Name(Box<str>),
    AnyName,
    NsName,
}

impl RelaxNGNodeType {
    fn typename(&self) -> &'static str {
        use RelaxNGNodeType::*;

        match self {
            Element(_) => "element",
            Attribute(_) => "attribute",
            Group => "group",
            Interleave => "interleave",
            Choice(_) => "choice",
            Optional => "optional",
            ZeroOrMore => "zeroOrMore",
            OneOrMore => "oneOrMore",
            List => "list",
            Mixed => "mixed",
            Ref(_) => "ref",
            ParentRef(_) => "parentRef",
            Empty => "empty",
            Text => "text",
            Value { .. } => "value",
            Data(_) => "data",
            NotAllowed => "notAllowed",
            ExternalRef(_) => "externalRef",
            Grammar => "grammar",
            Param { .. } => "param",
            Except(_) => "except",
            Start(_) => "start",
            Define { .. } => "define",
            Div(_) => "div",
            Include(_) => "include",
            Name(_) => "name",
            AnyName => "anyName",
            NsName => "nsName",
        }
    }

    fn is_pattern(&self) -> bool {
        use RelaxNGNodeType::*;

        matches!(
            self,
            Element(_)
                | Attribute(_)
                | Group
                | Interleave
                | Choice(ChoiceType::Pattern)
                | Optional
                | ZeroOrMore
                | OneOrMore
                | List
                | Mixed
                | Ref(_)
                | ParentRef(_)
                | Empty
                | Text
                | Value { .. }
                | Data(_)
                | NotAllowed
                | ExternalRef(_)
                | Grammar
        )
    }

    fn is_except_pattern(&self) -> bool {
        matches!(self, RelaxNGNodeType::Except(ExceptType::Pattern))
    }

    fn is_param(&self) -> bool {
        matches!(self, RelaxNGNodeType::Param { .. })
    }

    fn is_grammar_content(&self) -> bool {
        use RelaxNGNodeType::*;

        matches!(
            self,
            Start(_) | Define { .. } | Div(DivContentType::Grammar) | Include(_)
        )
    }

    fn is_include_content(&self) -> bool {
        use RelaxNGNodeType::*;

        matches!(
            self,
            Start(_) | Define { .. } | Div(DivContentType::Include)
        )
    }

    fn is_name_class(&self) -> bool {
        use RelaxNGNodeType::*;

        matches!(
            self,
            Name(_) | AnyName | NsName | Choice(ChoiceType::NameClass)
        )
    }
}

struct RelaxNGNode {
    base_uri: Option<Arc<URIStr>>,
    datatype_library: Option<Arc<str>>,
    ns: Option<Box<str>>,
    r#type: RelaxNGNodeType,
    children: Vec<usize>,
}

struct RelaxNGParseHandler<H: SAXHandler = DefaultSAXHandler> {
    handler: H,

    /// When set to `true`, the context is initialized at the start of parsing.
    ///
    /// When processing `externalRef` or `include`, the context must be preserved,
    /// so it should be set to `false`.
    init: bool,

    ignore_depth: usize,
    locator: Arc<Locator>,
    locator_stack: Vec<Arc<Locator>>,
    last_error: Result<(), SAXParseError>,
    unrecoverable: bool,

    cur: usize,
    tree: Vec<RelaxNGNode>,
    node_stack: Vec<usize>,
    text: String,
}

impl<H: SAXHandler> RelaxNGParseHandler<H> {
    /// Create a RELAX NG element that does not permit attributes other than `xml:base`,
    /// `ns` and `datatypeLibrary`.
    ///
    /// If any attribute is found that is not an foreign attribute other than `xml:base`
    /// `ns' or `datatypeLibrary`, report an error.
    fn new_node<'a>(
        &mut self,
        r#type: RelaxNGNodeType,
        atts: impl IntoIterator<Item = &'a Attribute> + 'a,
    ) -> RelaxNGNode {
        let mut base_uri = None;
        let mut datatype_library = None;
        let mut ns = None;
        for att in atts {
            let value = att
                .value
                .trim_matches(|c| XMLVersion::default().is_whitespace(c));
            match att.qname.as_ref() {
                "xml:base" => {
                    // The base URI will be resolved when the node is pushed,
                    // so here I only verify that it is a URI reference.
                    if let Ok(uri) = URIString::parse(value) {
                        base_uri = Some(uri.into());
                    } else {
                        error!(
                            self,
                            RngParseInvalidAnyURI,
                            "The value '{}' of 'xml:base' is not a valid URI reference.",
                            value
                        );
                    }
                }
                "datatypeLibrary" => {
                    if !value.is_empty() && validate_rfc2396_absolute_uri(value).is_err() {
                        error!(
                            self,
                            RngParseDatatypeLibraryURINotAbsolute,
                            "The attribute 'datatypeLibrary' must have an absolute URI value, but '{}' is not.",
                            value
                        );
                    } else {
                        datatype_library = Some(value.into());
                    }
                }
                "ns" => {
                    ns = Some(value.into());
                }
                _ if att
                    .namespace_name
                    .as_deref()
                    .is_some_and(|ns| ns != XML_RELAX_NG_NAMESPACE) =>
                {
                    // foreign attribute
                    continue;
                }
                qname => {
                    if let (Some(namespace_name), Some(local_name)) =
                        (att.namespace_name.as_deref(), att.local_name.as_deref())
                    {
                        error!(
                            self,
                            RngParseUnacceptableAttribute,
                            "The attribute '{{{}}}{}' is not allowed on '{}'.",
                            namespace_name,
                            local_name,
                            r#type.typename()
                        );
                    } else {
                        error!(
                            self,
                            RngParseUnacceptableAttribute,
                            "The attribute '{}' is not allowed on '{}'.",
                            qname,
                            r#type.typename()
                        );
                    }
                }
            }
        }

        RelaxNGNode {
            base_uri,
            datatype_library,
            ns,
            r#type,
            children: vec![],
        }
    }

    fn context_node_name(&self) -> Option<&str> {
        if self.tree.is_empty() {
            None
        } else {
            Some(self.tree[self.cur].r#type.typename())
        }
    }

    /// Add `node` as the last child of the context node.
    ///
    /// Return `true` if the addition succeeds without violating any constraints,
    /// `false` otherwise.  \
    /// If added successfully, the added node becomes the new context node.
    fn push_node(&mut self, mut node: RelaxNGNode) -> bool {
        if self.tree.is_empty() {
            if node.base_uri.is_none() {
                node.base_uri = Some(self.locator.system_id());
            }

            return if node.r#type.is_pattern() {
                self.cur = 0;
                self.tree.push(node);
                true
            } else {
                error!(
                    self,
                    RngParseUnacceptablePattern,
                    "'{}' cannot be a root pattern element.",
                    node.r#type.typename()
                );
                self.ignore_depth += 1;
                false
            };
        }

        match node.r#type {
            RelaxNGNodeType::Except(ref mut ty) => {
                if self.tree[self.cur].r#type.is_pattern() {
                    *ty = ExceptType::Pattern;
                } else if self.tree[self.cur].r#type.is_name_class() {
                    *ty = ExceptType::NameClass;
                } else {
                    error!(
                        self,
                        RngParseUnacceptablePattern,
                        "'except' cannot be a child of '{}'.",
                        self.tree[self.cur].r#type.typename()
                    );
                    self.ignore_depth += 1;
                    return false;
                }
            }
            RelaxNGNodeType::Choice(ref mut ty) => {
                if self.tree[self.cur].r#type.is_name_class() {
                    *ty = ChoiceType::NameClass;
                } else {
                    *ty = ChoiceType::Pattern;
                }
            }
            RelaxNGNodeType::Div(ref mut ty) => match self.tree[self.cur].r#type {
                RelaxNGNodeType::Grammar => *ty = DivContentType::Grammar,
                RelaxNGNodeType::Include(_) => *ty = DivContentType::Include,
                RelaxNGNodeType::Div(divty) => *ty = divty,
                ref ty => {
                    error!(
                        self,
                        RngParseUnacceptablePattern,
                        "'div' cannot be a child of '{}'.",
                        ty.typename()
                    );
                    self.ignore_depth += 1;
                    return false;
                }
            },
            _ => {}
        }

        match &self.tree[self.cur].r#type {
            RelaxNGNodeType::Element(None) if self.tree[self.cur].children.is_empty() => {
                if !node.r#type.is_name_class() {
                    error!(
                        self,
                        RngParseUnacceptablePattern,
                        "The element '{}' cannot be a first child of 'element' that does not have 'name' attribute.",
                        node.r#type.typename()
                    );
                    self.ignore_depth += 1;
                    return false;
                }
            }
            ty @ (RelaxNGNodeType::Element(_)
            | RelaxNGNodeType::Group
            | RelaxNGNodeType::Interleave
            | RelaxNGNodeType::Choice(ChoiceType::Pattern)
            | RelaxNGNodeType::Optional
            | RelaxNGNodeType::ZeroOrMore
            | RelaxNGNodeType::OneOrMore
            | RelaxNGNodeType::List
            | RelaxNGNodeType::Mixed
            | RelaxNGNodeType::Except(ExceptType::Pattern)
            | RelaxNGNodeType::Define { .. }) => {
                if !node.r#type.is_pattern() {
                    error!(
                        self,
                        RngParseUnacceptablePattern,
                        "The element '{}' cannot be a child of '{}'",
                        node.r#type.typename(),
                        ty.typename()
                    );
                    self.ignore_depth += 1;
                    return false;
                }
            }
            ty @ (RelaxNGNodeType::Ref(_)
            | RelaxNGNodeType::ParentRef(_)
            | RelaxNGNodeType::Empty
            | RelaxNGNodeType::Text
            | RelaxNGNodeType::NotAllowed
            | RelaxNGNodeType::ExternalRef(_)) => {
                error!(
                    self,
                    RngParseUnacceptablePattern,
                    "'{}' cannot have any children.",
                    ty.typename()
                );
                self.ignore_depth += 1;
                return false;
            }
            ty @ (RelaxNGNodeType::Value { .. }
            | RelaxNGNodeType::Param { .. }
            | RelaxNGNodeType::Name(_)) => {
                error!(
                    self,
                    RngParseUnacceptablePattern,
                    "'{}' cannot have any element children.",
                    ty.typename()
                );
                self.ignore_depth += 1;
                return false;
            }
            RelaxNGNodeType::Data(_) => {
                if node.r#type.is_param() {
                    if self.tree[self.cur]
                        .children
                        .last()
                        .is_some_and(|&l| self.tree[l].r#type.is_except_pattern())
                    {
                        error!(
                            self,
                            RngParseUnacceptablePattern,
                            "The `param` child of `data` cannot be followed by `except`."
                        );
                        self.ignore_depth += 1;
                        return false;
                    }
                } else if node.r#type.is_except_pattern() {
                    if self.tree[self.cur]
                        .children
                        .last()
                        .is_some_and(|&l| self.tree[l].r#type.is_except_pattern())
                    {
                        error!(
                            self,
                            RngParseUnacceptablePattern,
                            "The element 'data' cannot have more than one 'except' as its last child."
                        );
                        self.ignore_depth += 1;
                        return false;
                    }
                } else {
                    error!(
                        self,
                        RngParseUnacceptablePattern,
                        "The element '{}' cannot be a child of 'data'",
                        node.r#type.typename()
                    );
                    self.ignore_depth += 1;
                    return false;
                }
            }
            RelaxNGNodeType::Attribute(None) => {
                if self.tree[self.cur].children.is_empty() {
                    if !node.r#type.is_name_class() {
                        error!(
                            self,
                            RngParseUnacceptablePattern,
                            "The element '{}' cannot be a first child of 'attribute' that does not have 'name' attribute.",
                            node.r#type.typename()
                        );
                        self.ignore_depth += 1;
                        return false;
                    }
                } else if !node.r#type.is_pattern() {
                    error!(
                        self,
                        RngParseUnacceptablePattern,
                        "The element '{}' cannot be a child of 'attribute'",
                        node.r#type.typename()
                    );
                    self.ignore_depth += 1;
                    return false;
                } else if self.tree[self.cur].children.len() == 1 {
                    error!(
                        self,
                        RngParseUnacceptablePattern,
                        "The element 'attribute' cannot have more than one pattern child.",
                    );
                    self.ignore_depth += 1;
                    return false;
                }
            }
            ty @ (RelaxNGNodeType::Attribute(Some(_)) | RelaxNGNodeType::Start(_)) => {
                // The `attribute` element has at most one child, while the `start` element
                // has exactly one child, so their patterns differ.
                // However, since all children must be scanned to distinguish them, they are
                // processed using the same pattern.

                if !node.r#type.is_pattern() {
                    error!(
                        self,
                        RngParseUnacceptablePattern,
                        "The element '{}' cannot be a child of '{}'",
                        node.r#type.typename(),
                        ty.typename()
                    );
                    self.ignore_depth += 1;
                    return false;
                } else if !self.tree[self.cur].children.is_empty() {
                    error!(
                        self,
                        RngParseUnacceptablePattern,
                        "The element '{}' cannot have more than one pattern child.",
                        ty.typename()
                    );
                    self.ignore_depth += 1;
                    return false;
                }
            }
            ty @ (RelaxNGNodeType::Grammar | RelaxNGNodeType::Div(DivContentType::Grammar)) => {
                if !node.r#type.is_grammar_content() {
                    error!(
                        self,
                        RngParseUnacceptablePattern,
                        "The element '{}' cannot be a child of '{}'",
                        node.r#type.typename(),
                        ty.typename()
                    );
                    self.ignore_depth += 1;
                    return false;
                }
            }
            ty @ (RelaxNGNodeType::Div(DivContentType::Include) | RelaxNGNodeType::Include(_)) => {
                if !node.r#type.is_include_content() {
                    error!(
                        self,
                        RngParseUnacceptablePattern,
                        "The element '{}' cannot be a child of '{}'",
                        node.r#type.typename(),
                        ty.typename()
                    );
                    self.ignore_depth += 1;
                    return false;
                }
            }
            ty @ (RelaxNGNodeType::AnyName | RelaxNGNodeType::NsName) => {
                if !matches!(node.r#type, RelaxNGNodeType::Except(ExceptType::NameClass)) {
                    error!(
                        self,
                        RngParseUnacceptablePattern,
                        "The element '{}' cannot be a child of '{}'",
                        node.r#type.typename(),
                        ty.typename()
                    );
                    self.ignore_depth += 1;
                    return false;
                } else if !self.tree[self.cur].children.is_empty() {
                    error!(
                        self,
                        RngParseUnacceptablePattern,
                        "The element 'start' cannot have more than one child."
                    );
                    self.ignore_depth += 1;
                    return false;
                }
            }
            ty @ (RelaxNGNodeType::Choice(ChoiceType::NameClass)
            | RelaxNGNodeType::Except(ExceptType::NameClass)) => {
                if !node.r#type.is_name_class() {
                    error!(
                        self,
                        RngParseUnacceptablePattern,
                        "The element '{}' cannot be a child of '{}'",
                        node.r#type.typename(),
                        ty.typename()
                    );
                    self.ignore_depth += 1;
                    return false;
                }
            }
        }

        // resolve base URI
        if let Some(base_uri) = node.base_uri.as_mut() {
            if !base_uri.is_absolute() {
                let new = self.tree[self.cur]
                    .base_uri
                    .as_deref()
                    .unwrap()
                    .resolve(base_uri);
                *base_uri = new.into();
            }
        } else {
            node.base_uri = self.tree[self.cur].base_uri.clone();
        }

        // ISO/IEC 19757-2:2008 7.4 `datatypeLibrary` attribute
        if node.datatype_library.is_none() {
            // Although only `data` and `value` are required,
            // adding it to all elements is acceptable.
            node.datatype_library = self.tree[self.cur].datatype_library.clone();
        }

        // ISO/IEC 19757-2:2008 7.6 `href` attribute
        match &mut node.r#type {
            RelaxNGNodeType::ExternalRef(href) | RelaxNGNodeType::Include(href) => {
                if !href.is_absolute() {
                    let abs = node.base_uri.as_deref().unwrap().resolve(href);
                    *href = abs.into();
                }

                if href.fragment().is_some() {
                    error!(
                        self,
                        RngParseHRefIncludeFragment,
                        "The URI that refers to an XML external resource must not contain a fragment identifier."
                    );
                    // Recover by removing the fragment identifier.
                    *href = href.resolve(&URIString::parse("").unwrap()).into();
                }
            }
            _ => {}
        }

        let new = self.tree.len();
        self.tree[self.cur].children.push(new);
        self.node_stack.push(self.cur);
        self.cur = new;
        self.tree.push(node);
        true
    }

    fn pop_node(&mut self) {
        if let Some(prev) = self.node_stack.pop() {
            self.cur = prev;
        }
    }
}

impl<H: SAXHandler> SAXHandler for RelaxNGParseHandler<H> {
    fn set_document_locator(&mut self, locator: Arc<Locator>) {
        self.handler.set_document_locator(locator.clone());

        if self.init {
            self.ignore_depth = 0;
            self.locator = locator;
            self.locator_stack.clear();
            self.last_error = Ok(());
            self.unrecoverable = false;

            self.cur = 0;
            self.tree.clear();
            self.node_stack.clear();
            self.text.clear();
        } else {
            self.locator_stack.push(self.locator.clone());
            self.locator = locator;
        }
    }

    fn start_element(
        &mut self,
        namespace_name: Option<&str>,
        local_name: Option<&str>,
        qname: &str,
        atts: &Attributes,
    ) {
        self.handler
            .start_element(namespace_name, local_name, qname, atts);
        if self.ignore_depth > 0 || self.unrecoverable {
            self.ignore_depth += 1;
            return;
        }

        // Only elements that do not have string children can have foreign elements as children.
        // `value`, `param` and `name` cannot have foreign elements (and RELAX NG elements)
        // as children.
        //
        // ISO/IEC 19757-2:2008 6 Full syntax
        if matches!(self.context_node_name(), Some("value" | "param" | "name")) {
            error!(
                self,
                RngParseUnacceptablePattern,
                "'{}' pattern must not have any data other than text, comment or pi.",
                self.context_node_name().unwrap()
            );
            self.ignore_depth += 1;
            return;
        }

        // foreign element
        if namespace_name != Some(XML_RELAX_NG_NAMESPACE) {
            self.ignore_depth += 1;
            return;
        }

        let local_name = local_name.unwrap();

        const VERSION: XMLVersion = XMLVersion::XML10;
        let node = match local_name {
            name @ ("element" | "attribute") => {
                if let Some(index) = atts.get_index_by_expanded_name(None, "name") {
                    let value = atts
                        .get_value(index)
                        .unwrap()
                        .trim_matches(|c| VERSION.is_whitespace(c));
                    if !VERSION.validate_qname(value) {
                        error!(
                            self,
                            RngParseInvalidQName,
                            "The value '{}' of 'name' on '{}' is not a QName.",
                            value,
                            name
                        );
                    }
                    self.new_node(
                        if name == "element" {
                            RelaxNGNodeType::Element(Some(value.into()))
                        } else {
                            RelaxNGNodeType::Attribute(Some(value.into()))
                        },
                        atts.iter()
                            .enumerate()
                            .filter_map(|(i, att)| (i != index).then_some(att)),
                    )
                } else {
                    self.new_node(
                        if name == "element" {
                            RelaxNGNodeType::Element(None)
                        } else {
                            RelaxNGNodeType::Attribute(None)
                        },
                        atts,
                    )
                }
            }
            "group" => self.new_node(RelaxNGNodeType::Group, atts),
            "interleave" => self.new_node(RelaxNGNodeType::Interleave, atts),
            "choice" => self.new_node(RelaxNGNodeType::Choice(ChoiceType::Pattern), atts),
            "optional" => self.new_node(RelaxNGNodeType::Optional, atts),
            "zeroOrMore" => self.new_node(RelaxNGNodeType::ZeroOrMore, atts),
            "oneOrMore" => self.new_node(RelaxNGNodeType::OneOrMore, atts),
            "list" => self.new_node(RelaxNGNodeType::List, atts),
            "mixed" => self.new_node(RelaxNGNodeType::Mixed, atts),
            name @ ("ref" | "parentRef" | "param") => {
                if let Some(index) = atts.get_index_by_expanded_name(None, "name") {
                    let value = atts
                        .get_value(index)
                        .unwrap()
                        .trim_matches(|c| VERSION.is_whitespace(c));
                    if !VERSION.validate_ncname(value) {
                        error!(
                            self,
                            RngParseInvalidNCName,
                            "The value '{}' of 'name' on '{}' is not a NCName.",
                            value,
                            name
                        );
                    }
                    self.new_node(
                        if name == "ref" {
                            RelaxNGNodeType::Ref(value.into())
                        } else if name == "param" {
                            RelaxNGNodeType::Param {
                                name: value.into(),
                                value: "".into(),
                            }
                        } else {
                            RelaxNGNodeType::ParentRef(value.into())
                        },
                        atts.iter()
                            .enumerate()
                            .filter_map(|(i, att)| (i != index).then_some(att)),
                    )
                } else {
                    error!(
                        self,
                        RngParseInsufficientAttribute,
                        "'{}' must have an attribute 'name', but not found.",
                        name
                    );
                    self.ignore_depth += 1;
                    return;
                }
            }
            "empty" => self.new_node(RelaxNGNodeType::Empty, atts),
            "text" => self.new_node(RelaxNGNodeType::Text, atts),
            "value" => {
                if let Some(index) = atts.get_index_by_expanded_name(None, "type") {
                    let value = atts
                        .get_value(index)
                        .unwrap()
                        .trim_matches(|c| VERSION.is_whitespace(c));
                    if !VERSION.validate_ncname(value) {
                        error!(
                            self,
                            RngParseInvalidNCName,
                            "The value '{}' of 'type' on 'value' is not a NCName.",
                            value
                        );
                    }
                    self.new_node(
                        RelaxNGNodeType::Value {
                            r#type: Some(value.into()),
                            value: "".into(),
                        },
                        atts.iter()
                            .enumerate()
                            .filter_map(|(i, att)| (i != index).then_some(att)),
                    )
                } else {
                    self.new_node(
                        RelaxNGNodeType::Value {
                            r#type: None,
                            value: "".into(),
                        },
                        atts,
                    )
                }
            }
            "data" => {
                if let Some(index) = atts.get_index_by_expanded_name(None, "type") {
                    let value = atts
                        .get_value(index)
                        .unwrap()
                        .trim_matches(|c| VERSION.is_whitespace(c));
                    if !VERSION.validate_ncname(value) {
                        error!(
                            self,
                            RngParseInvalidNCName,
                            "The value '{}' of 'name' on 'data' is not a NCName.",
                            value
                        );
                    }
                    self.new_node(
                        RelaxNGNodeType::Data(value.into()),
                        atts.iter()
                            .enumerate()
                            .filter_map(|(i, att)| (i != index).then_some(att)),
                    )
                } else {
                    error!(
                        self,
                        RngParseInsufficientAttribute,
                        "'data' must have an attribute 'type', but not found."
                    );
                    self.ignore_depth += 1;
                    return;
                }
            }
            "notAllowed" => self.new_node(RelaxNGNodeType::NotAllowed, atts),
            name @ ("externalRef" | "include") => {
                if let Some(index) = atts.get_index_by_expanded_name(None, "href") {
                    let value = atts
                        .get_value(index)
                        .unwrap()
                        .trim_matches(|c| VERSION.is_whitespace(c));
                    match URIString::parse(value) {
                        Ok(uri) => self.new_node(
                            if name == "externalRef" {
                                RelaxNGNodeType::ExternalRef(uri.into())
                            } else {
                                RelaxNGNodeType::Include(uri.into())
                            },
                            atts.iter()
                                .enumerate()
                                .filter_map(|(i, att)| (i != index).then_some(att)),
                        ),
                        Err(_) => {
                            error!(
                                self,
                                RngParseInvalidAnyURI,
                                "The value '{}' of 'href' on '{}' is not a URI.",
                                value,
                                name
                            );
                            self.ignore_depth += 1;
                            return;
                        }
                    }
                } else {
                    error!(
                        self,
                        RngParseInsufficientAttribute,
                        "'{}' must have an attribute 'href', but not found.",
                        name
                    );
                    self.ignore_depth += 1;
                    return;
                }
            }
            "grammar" => self.new_node(RelaxNGNodeType::Grammar, atts),
            "except" => self.new_node(RelaxNGNodeType::Except(ExceptType::Pattern), atts),
            "start" => {
                if let Some(index) = atts.get_index_by_expanded_name(None, "combine") {
                    let value = atts
                        .get_value(index)
                        .unwrap()
                        .trim_matches(|c| VERSION.is_whitespace(c));
                    if value != "choice" || value != "interleave" {
                        error!(
                            self,
                            RngParseUnacceptableCombine,
                            "The value of 'combine' of 'start' must be 'choice' or 'interleave', but '{}' is specified.",
                            value
                        );
                    }
                    self.new_node(
                        RelaxNGNodeType::Start(Some(value.into())),
                        atts.iter()
                            .enumerate()
                            .filter_map(|(i, att)| (i != index).then_some(att)),
                    )
                } else {
                    self.new_node(RelaxNGNodeType::Start(None), atts)
                }
            }
            "define" => {
                let Some(ni) = atts.get_index_by_expanded_name(None, "name") else {
                    error!(
                        self,
                        RngParseInsufficientAttribute,
                        "'define' must have an attribute 'name', but not found."
                    );
                    self.ignore_depth += 1;
                    return;
                };

                let name = atts
                    .get_value(ni)
                    .unwrap()
                    .trim_matches(|c| VERSION.is_whitespace(c));
                if !VERSION.validate_ncname(name) {
                    error!(
                        self,
                        RngParseInvalidNCName,
                        "The value '{}' of 'name' on 'define' is not a NCName.",
                        name
                    );
                }

                if let Some(ci) = atts.get_index_by_expanded_name(None, "combine") {
                    let combine = atts
                        .get_value(ci)
                        .unwrap()
                        .trim_matches(|c| VERSION.is_whitespace(c));
                    if combine != "choice" || combine != "interleave" {
                        error!(
                            self,
                            RngParseUnacceptableCombine,
                            "The value of 'combine' of 'define' must be 'choice' or 'interleave', but '{}' is specified.",
                            combine
                        );
                    }
                    self.new_node(
                        RelaxNGNodeType::Define {
                            name: name.into(),
                            combine: Some(combine.into()),
                        },
                        atts.iter()
                            .enumerate()
                            .filter_map(|(i, att)| (i != ni && i != ci).then_some(att)),
                    )
                } else {
                    self.new_node(
                        RelaxNGNodeType::Define {
                            name: name.into(),
                            combine: None,
                        },
                        atts.iter()
                            .enumerate()
                            .filter_map(|(i, att)| (i != ni).then_some(att)),
                    )
                }
            }
            "div" => self.new_node(RelaxNGNodeType::Div(DivContentType::Grammar), atts),
            "name" => self.new_node(RelaxNGNodeType::Name("".into()), atts),
            "anyName" => self.new_node(RelaxNGNodeType::AnyName, atts),
            "nsName" => self.new_node(RelaxNGNodeType::NsName, atts),
            _ => {
                error!(
                    self,
                    RngParseUnacceptablePattern,
                    "The element '{}' does not match to a pattern element.",
                    local_name
                );
                self.ignore_depth += 1;
                return;
            }
        };

        if !self.push_node(node) {
            self.ignore_depth += 1;
        }
    }
    fn end_element(&mut self, namespace_name: Option<&str>, local_name: Option<&str>, qname: &str) {
        self.handler.end_element(namespace_name, local_name, qname);
        if self.ignore_depth > 0 || !self.unrecoverable {
            self.ignore_depth -= 1;
        }

        if !self.text.is_empty() {
            let text = self
                .text
                .trim_matches(|c| XMLVersion::default().is_whitespace(c));
            match &mut self.tree[self.cur].r#type {
                RelaxNGNodeType::Value { value, .. } | RelaxNGNodeType::Param { value, .. } => {
                    *value = text.into()
                }
                RelaxNGNodeType::Name(name) => {
                    *name = text.into();
                    if !XMLVersion::default().validate_qname(text) {
                        error!(
                            self,
                            RngParseInvalidQName,
                            "The text content '{}' of 'name' is not a QName.",
                            text
                        );
                    }
                }
                _ => {
                    // For RELAX NG elements that cannot contain text, an error must
                    // be raised, but this is left to the `characters` callback.
                }
            }
            self.text.clear();
        }

        match &self.tree[self.cur].r#type {
            RelaxNGNodeType::Element(None) => {
                if self.tree[self.cur]
                    .children
                    .last()
                    .is_none_or(|&l| !self.tree[l].r#type.is_pattern())
                {
                    error!(
                        self,
                        RngParseUnacceptablePattern,
                        "The element 'element' must have exactly one nameClass child and at least one pattern children."
                    );
                }
            }
            ty @ (RelaxNGNodeType::Element(Some(_))
            | RelaxNGNodeType::Group
            | RelaxNGNodeType::Interleave
            | RelaxNGNodeType::Choice(ChoiceType::Pattern)
            | RelaxNGNodeType::Optional
            | RelaxNGNodeType::ZeroOrMore
            | RelaxNGNodeType::OneOrMore
            | RelaxNGNodeType::List
            | RelaxNGNodeType::Mixed
            | RelaxNGNodeType::Except(ExceptType::Pattern)
            | RelaxNGNodeType::Define { .. }) => {
                if self.tree[self.cur].children.is_empty() {
                    error!(
                        self,
                        RngParseUnacceptablePattern,
                        "The element '{}' must have at least one pattern children.",
                        ty.typename()
                    );
                }
            }
            RelaxNGNodeType::Attribute(None) => {
                if self.tree[self.cur].children.is_empty() {
                    error!(
                        self,
                        RngParseUnacceptablePattern,
                        "The element 'attribute' that does not have 'name' attribute must have exactly one nameClass child."
                    );
                }
            }
            RelaxNGNodeType::Start(_) => {
                if self.tree[self.cur].children.is_empty() {
                    error!(
                        self,
                        RngParseUnacceptablePattern,
                        "The element 'start' must have exactly one pattern child."
                    );
                }
            }
            ty @ (RelaxNGNodeType::Choice(ChoiceType::NameClass)
            | RelaxNGNodeType::Except(ExceptType::NameClass)) => {
                if self.tree[self.cur].children.is_empty() {
                    error!(
                        self,
                        RngParseUnacceptablePattern,
                        "The element '{}' must have at least one nameClass children.",
                        ty.typename()
                    );
                }
            }
            _ => {}
        }

        // ISO/IEC 19757-2:2008 7.5 `type` attribute of `value` element
        if let RelaxNGNodeType::Value { r#type, .. } = &mut &mut self.tree[self.cur].r#type
            && r#type.is_none()
        {
            *r#type = Some("token".into());
            self.tree[self.cur].datatype_library = None;
        }

        self.pop_node();
    }

    fn characters(&mut self, data: &str) {
        self.handler.characters(data);
        let old = self.text.len();
        // ISO/IEC 19757-2:2008 7.3 Whitespace
        // Do not remove whitespace from `value` or `param`.
        //
        // ISO/IEC 19757-2:2008 6 Full syntax
        // Any element can also have as children strings that consist entirely
        // of whitespace characters
        if self.text.is_empty() && !matches!(self.context_node_name(), Some("value" | "param")) {
            let trimmed = data.trim_start_matches(|c| XMLVersion::default().is_whitespace(c));
            self.text.push_str(trimmed);
        } else {
            self.text.push_str(data);
        }

        if old == 0
            && !self.text.is_empty()
            && !matches!(self.context_node_name(), Some("value" | "param" | "name"))
        {
            error!(
                self,
                RngParseUnacceptableString,
                "Elements other than 'value', 'param', and 'name' must not contain any non-whitespace characters."
            );
        }
    }

    // The following callbacks are not used for RELAX NG schema parsing.

    fn attribute_decl(
        &mut self,
        element_name: &str,
        attribute_name: &str,
        attribute_type: &AttributeType,
        default_decl: &DefaultDecl,
    ) {
        self.handler
            .attribute_decl(element_name, attribute_name, attribute_type, default_decl);
    }
    fn comment(&mut self, data: &str) {
        self.handler.comment(data);
    }
    fn declaration(&mut self, version: &str, encoding: Option<&str>, standalone: Option<bool>) {
        self.handler.declaration(version, encoding, standalone);
    }
    fn element_decl(&mut self, name: &str, contentspec: &ContentSpec) {
        self.handler.element_decl(name, contentspec);
    }
    fn start_cdata(&mut self) {
        self.handler.start_cdata();
    }
    fn end_cdata(&mut self) {
        self.handler.end_cdata();
    }
    fn start_document(&mut self) {
        self.handler.start_document();
    }
    fn end_document(&mut self) {
        self.handler.end_document();
    }
    fn start_dtd(&mut self, name: &str, public_id: Option<&str>, system_id: Option<&URIStr>) {
        self.handler.start_dtd(name, public_id, system_id);
    }
    fn end_dtd(&mut self) {
        self.handler.end_dtd();
    }
    fn start_entity(&mut self, name: &str) {
        self.handler.start_entity(name);
    }
    fn end_entity(&mut self) {
        self.handler.end_entity();
    }
    fn start_prefix_mapping(&mut self, prefix: Option<&str>, uri: &str) {
        self.handler.start_prefix_mapping(prefix, uri);
    }
    fn end_prefix_mapping(&mut self, prefix: Option<&str>) {
        self.handler.end_prefix_mapping(prefix);
    }
    fn external_entity_decl(&mut self, name: &str, public_id: Option<&str>, system_id: &URIStr) {
        self.handler
            .external_entity_decl(name, public_id, system_id);
    }
    fn ignorable_whitespace(&mut self, data: &str) {
        self.handler.ignorable_whitespace(data);
    }
    fn internal_entity_decl(&mut self, name: &str, value: &str) {
        self.handler.internal_entity_decl(name, value);
    }
    fn notation_decl(&mut self, name: &str, public_id: Option<&str>, system_id: Option<&URIStr>) {
        self.handler.notation_decl(name, public_id, system_id);
    }
    fn processing_instruction(&mut self, target: &str, data: Option<&str>) {
        self.handler.processing_instruction(target, data);
    }
    fn skipped_entity(&mut self, name: &str) {
        self.handler.skipped_entity(name);
    }
    fn unparsed_entity_decl(
        &mut self,
        name: &str,
        public_id: Option<&str>,
        system_id: &URIStr,
        notation_name: &str,
    ) {
        self.handler
            .unparsed_entity_decl(name, public_id, system_id, notation_name);
    }
}

impl<H: SAXHandler> ErrorHandler for RelaxNGParseHandler<H> {
    fn fatal_error(&mut self, error: SAXParseError) {
        self.handler.fatal_error(error);
    }

    fn error(&mut self, error: SAXParseError) {
        self.handler.error(error);
    }

    fn warning(&mut self, error: SAXParseError) {
        self.handler.warning(error);
    }
}

impl<H: SAXHandler> EntityResolver for RelaxNGParseHandler<H> {
    fn resolve_entity(
        &mut self,
        name: &str,
        public_id: Option<&str>,
        base_uri: &URIStr,
        system_id: &URIStr,
    ) -> Result<InputSource<'static>, XMLError> {
        self.handler
            .resolve_entity(name, public_id, base_uri, system_id)
    }

    fn get_external_subset(
        &mut self,
        name: &str,
        base_uri: Option<&URIStr>,
    ) -> Result<InputSource<'static>, XMLError> {
        self.handler.get_external_subset(name, base_uri)
    }
}
