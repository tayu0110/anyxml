use std::{
    collections::{BTreeMap, HashMap, HashSet},
    sync::Arc,
};

use crate::{
    XML_NS_NAMESPACE, XMLVersion,
    error::XMLError,
    relaxng::{
        XML_RELAX_NG_NAMESPACE,
        datatype_library::RelaxNGDatatypeLibraries,
        grammar::{Grammar, NameClass, Pattern},
    },
    sax::{
        AttributeType, DefaultDecl, Locator, NamespaceStack,
        attributes::{Attribute, Attributes},
        contentspec::ContentSpec,
        error::SAXParseError,
        handler::{DefaultSAXHandler, EntityResolver, ErrorHandler, SAXHandler},
        parser::XMLReaderBuilder,
        source::InputSource,
    },
    uri::{URIStr, rfc2396::validate_rfc2396_absolute_uri, uri::URIString},
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
pub(super) enum DivContentType {
    Grammar,
    Include,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ExceptType {
    Pattern,
    NameClass,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ChoiceType {
    Pattern,
    NameClass,
}

#[derive(Clone)]
pub(super) enum RelaxNGNodeType {
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
        /// Validating the `value` pattern requires the context of the `value` element itself.
        /// While the base URI is held by the `RelaxNGNode`, it does not retain namespace mappings,
        /// so they are stored here.
        ns_map: BTreeMap<Arc<str>, Arc<str>>,
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

    // helper patterns
    ExternalRefRoot,
    IncludeRoot,
}

impl RelaxNGNodeType {
    pub(super) fn typename(&self) -> &'static str {
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
            ExternalRefRoot => "#externalRefRoot",
            IncludeRoot => "#includeRoot",
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

#[derive(Clone)]
pub(super) struct RelaxNGNode {
    pub(super) base_uri: Option<Arc<URIStr>>,
    pub(super) datatype_library: Option<Arc<str>>,
    pub(super) ns: Option<Arc<str>>,
    xmlns: HashMap<Arc<str>, Box<str>>,
    pub(super) r#type: RelaxNGNodeType,
    pub(super) children: Vec<usize>,
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

pub(super) struct RelaxNGParseHandler<H: SAXHandler = DefaultSAXHandler> {
    handler: H,
    pub(super) datatype_libraries: RelaxNGDatatypeLibraries,

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
    pub(super) tree: Vec<RelaxNGNode>,
    node_stack: Vec<usize>,
    text: String,
    defines: HashMap<String, usize>,
}

impl<H: SAXHandler> RelaxNGParseHandler<H> {
    pub(super) fn with_handler(handler: H) -> Self {
        Self {
            handler,
            datatype_libraries: RelaxNGDatatypeLibraries::default(),
            init: true,
            ignore_depth: 0,
            locator: Arc::new(Locator::new(
                URIString::parse("").unwrap().into(),
                None,
                1,
                1,
            )),
            locator_stack: vec![],
            last_error: Ok(()),
            unrecoverable: false,
            cur: 0,
            tree: vec![],
            node_stack: vec![],
            text: String::new(),
            defines: HashMap::new(),
        }
    }

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
        let mut xmlns = HashMap::new();
        for att in atts {
            if att.is_nsdecl() {
                if att.qname.as_ref() == "xmlns" {
                    xmlns.insert("".into(), att.value.clone());
                } else {
                    xmlns.insert(
                        att.local_name.clone().unwrap_or_default(),
                        att.value.clone(),
                    );
                }
                continue;
            }
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
            xmlns,
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
            } else if let Some(base_uri) = node.base_uri.as_mut().filter(|uri| !uri.is_absolute()) {
                *base_uri = self.locator.system_id().resolve(base_uri).into();
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
                    return false;
                }
            }
            RelaxNGNodeType::Choice(ref mut ty) => {
                if self.tree[self.cur].r#type.is_name_class() {
                    *ty = ChoiceType::NameClass;
                } else if matches!(
                    self.tree[self.cur].r#type,
                    RelaxNGNodeType::Element(_) | RelaxNGNodeType::Attribute(_)
                ) {
                    if !self.tree[self.cur].children.is_empty() {
                        *ty = ChoiceType::Pattern;
                    } else {
                        match self.tree[self.cur].r#type {
                            RelaxNGNodeType::Element(None) | RelaxNGNodeType::Attribute(None) => {
                                *ty = ChoiceType::NameClass;
                            }
                            _ => *ty = ChoiceType::Pattern,
                        }
                    }
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
                    return false;
                }
            },
            _ => {}
        }

        match self.tree[self.cur].r#type {
            RelaxNGNodeType::Element(None) if self.tree[self.cur].children.is_empty() => {
                if !node.r#type.is_name_class() {
                    error!(
                        self,
                        RngParseUnacceptablePattern,
                        "The element '{}' cannot be a first child of 'element' that does not have 'name' attribute.",
                        node.r#type.typename()
                    );
                    return false;
                }
            }
            ref ty @ (RelaxNGNodeType::Element(_)
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
                    return false;
                }
            }
            RelaxNGNodeType::ExternalRefRoot => {
                if !node.r#type.is_pattern() {
                    error!(
                        self,
                        RngParseExternalRefParseFailure,
                        "The element '{}' cannot be the root element of the external resource referenced by 'externalRef'.",
                        node.r#type.typename()
                    );
                    return false;
                }
            }
            RelaxNGNodeType::IncludeRoot => {
                if node.r#type.typename() != "grammar" {
                    error!(
                        self,
                        RngParseIncludeParseFailure,
                        "The element '{}' cannot be the root element of the external resource referenced by 'include'.",
                        node.r#type.typename()
                    );
                    return false;
                }
            }
            ref ty @ (RelaxNGNodeType::Ref(_)
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
                return false;
            }
            ref ty @ (RelaxNGNodeType::Value { .. }
            | RelaxNGNodeType::Param { .. }
            | RelaxNGNodeType::Name(_)) => {
                error!(
                    self,
                    RngParseUnacceptablePattern,
                    "'{}' cannot have any element children.",
                    ty.typename()
                );
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
                        return false;
                    }
                } else {
                    error!(
                        self,
                        RngParseUnacceptablePattern,
                        "The element '{}' cannot be a child of 'data'",
                        node.r#type.typename()
                    );
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
                        return false;
                    }
                } else if !node.r#type.is_pattern() {
                    error!(
                        self,
                        RngParseUnacceptablePattern,
                        "The element '{}' cannot be a child of 'attribute'",
                        node.r#type.typename()
                    );
                    return false;
                } else if self.tree[self.cur].children.len() == 2 {
                    error!(
                        self,
                        RngParseUnacceptablePattern,
                        "The element 'attribute' cannot have more than one pattern child.",
                    );
                    return false;
                }
            }
            ref ty @ (RelaxNGNodeType::Attribute(Some(_)) | RelaxNGNodeType::Start(_)) => {
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
                    return false;
                } else if !self.tree[self.cur].children.is_empty() {
                    error!(
                        self,
                        RngParseUnacceptablePattern,
                        "The element '{}' cannot have more than one pattern child.",
                        ty.typename()
                    );
                    return false;
                }
            }
            ref ty @ (RelaxNGNodeType::Grammar | RelaxNGNodeType::Div(DivContentType::Grammar)) => {
                if !node.r#type.is_grammar_content() {
                    error!(
                        self,
                        RngParseUnacceptablePattern,
                        "The element '{}' cannot be a child of '{}'",
                        node.r#type.typename(),
                        ty.typename()
                    );
                    return false;
                }
            }
            ref ty @ (RelaxNGNodeType::Div(DivContentType::Include)
            | RelaxNGNodeType::Include(_)) => {
                if !node.r#type.is_include_content() {
                    error!(
                        self,
                        RngParseUnacceptablePattern,
                        "The element '{}' cannot be a child of '{}'",
                        node.r#type.typename(),
                        ty.typename()
                    );
                    return false;
                }
            }
            ref ty @ (RelaxNGNodeType::AnyName | RelaxNGNodeType::NsName) => {
                if !matches!(node.r#type, RelaxNGNodeType::Except(ExceptType::NameClass)) {
                    error!(
                        self,
                        RngParseUnacceptablePattern,
                        "The element '{}' cannot be a child of '{}'",
                        node.r#type.typename(),
                        ty.typename()
                    );
                    return false;
                } else if !self.tree[self.cur].children.is_empty() {
                    error!(
                        self,
                        RngParseUnacceptablePattern,
                        "The element 'start' cannot have more than one child."
                    );
                    return false;
                }
            }
            ref ty @ (RelaxNGNodeType::Choice(ChoiceType::NameClass)
            | RelaxNGNodeType::Except(ExceptType::NameClass)) => {
                if !node.r#type.is_name_class() {
                    error!(
                        self,
                        RngParseUnacceptablePattern,
                        "The element '{}' cannot be a child of '{}'",
                        node.r#type.typename(),
                        ty.typename()
                    );
                    return false;
                }
            }
        }

        // resolve base URI
        if matches!(
            self.tree[self.cur].r#type,
            RelaxNGNodeType::ExternalRefRoot | RelaxNGNodeType::IncludeRoot
        ) {
            if node.base_uri.is_none() {
                node.base_uri = Some(self.locator.system_id());
            } else if let Some(base_uri) = node.base_uri.as_mut().filter(|uri| !uri.is_absolute()) {
                *base_uri = self.locator.system_id().resolve(base_uri).into();
            }
        } else if let Some(base_uri) = node.base_uri.as_mut() {
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
            node.datatype_library = self.tree[self.cur]
                .datatype_library
                .clone()
                .or_else(|| Some("".into()));
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

    /// Process simplification defined from ISO/IEC 19757-2:2008 7.7 to 7.22.
    pub(super) fn simplification(&mut self) -> Result<Grammar, SAXParseError> {
        self.last_error.clone()?;
        self.resolve_external_resource();
        self.last_error.clone()?;
        self.normalize_names(0, &mut NamespaceStack::default());
        self.last_error.clone()?;

        // `flatten_div` does not report any errors.
        self.flatten_div(0);
        // `simplification_13_to_16` also does not report any errors.
        self.simplification_13_to_16(0);

        self.check_constraints(0, true, true, true);
        self.last_error.clone()?;

        self.combine_start_and_define(0);
        self.last_error.clone()?;

        // Wrap the existing pattern P in the form `<grammar><start>P</start></grammar>`.
        if !matches!(self.tree[0].r#type, RelaxNGNodeType::Grammar) {
            let grammar = self.tree.len();
            self.tree.push(RelaxNGNode {
                base_uri: None,
                datatype_library: None,
                ns: None,
                xmlns: HashMap::new(),
                r#type: RelaxNGNodeType::Grammar,
                children: vec![grammar + 1],
            });
            self.tree.push(RelaxNGNode {
                base_uri: None,
                datatype_library: None,
                ns: None,
                xmlns: HashMap::new(),
                r#type: RelaxNGNodeType::Start(None),
                children: vec![grammar],
            });
            self.tree.swap(0, grammar);
        }
        let mut num_define = 0;
        self.flatten_grammar(0, &HashMap::new(), &mut HashMap::new(), &mut num_define);
        self.last_error.clone()?;

        let mut used_define = HashSet::new();
        self.simplification_20(0, usize::MAX, &mut num_define, &mut used_define);
        self.last_error.clone()?;

        // remove unreachable `define`
        let start = self.tree[0].children[0];
        used_define.retain(|&def| {
            let ch = self.tree[def].children[0];
            matches!(self.tree[ch].r#type, RelaxNGNodeType::Element(_))
        });
        self.tree[0]
            .children
            .retain(|&ch| ch == start || used_define.contains(&ch));

        // `simplification_21_to_22` also does not report any errors.
        self.simplification_21_to_22(usize::MAX, 0);
        self.remove_unreachable_define();

        self.check_prohibited_paths(0, false, false, false, false, false, false, false);
        self.last_error.clone()?;

        self.check_content_type(0);
        self.last_error.clone()?;

        let grammar = self.build();
        self.last_error.clone()?;
        let mut names = vec![];
        let mut memo = HashSet::new();
        self.check_attributes_restrictions(grammar.root, false, &grammar, &mut names, &mut memo);
        self.last_error.clone()?;
        names.clear();
        memo.clear();
        self.check_interleave_restrictions(grammar.root, &grammar, &mut names, &mut memo);
        self.last_error.clone().map(|_| grammar)
    }

    /// process `externalRef` and `include`.
    ///
    /// This method assumes that the simplifications from 7.1 to 7.6 have been completed.  \
    /// As an important assumption, `externalRef` and `include` hold URIs in their `href`
    /// attributes that are absolutized and without fragment identifiers.
    ///
    /// # Reference
    /// - ISO/IEC 19757-2:2008 7.7 `externalRef` element
    /// - ISO/IEC 19757-2:2008 7.8 `include` element
    fn resolve_external_resource(&mut self) {
        self.init = false;
        self.resolve_external_resource_recurse(usize::MAX, 0, 0);
        self.init = true;
    }
    fn resolve_external_resource_recurse(&mut self, parent: usize, nth: usize, current: usize) {
        match self.tree[current].r#type {
            RelaxNGNodeType::ExternalRef(_) => {
                // ISO/IEC 19757-2:2008 7.7 `externalRef` element

                let RelaxNGNodeType::ExternalRef(href) = std::mem::replace(
                    &mut self.tree[current].r#type,
                    RelaxNGNodeType::ExternalRefRoot,
                ) else {
                    unreachable!();
                };
                self.cur = current;
                self.init = false;
                let old = self.locator_stack.len();

                if self
                    .locator_stack
                    .iter()
                    .any(|loc| *loc.system_id() == *href)
                {
                    error!(
                        self,
                        RngParseExternalRefLoop,
                        "'externalRef' causes reference loop for '{}'.",
                        href
                    );
                    self.unrecoverable = true;
                    return;
                }

                let mut reader = XMLReaderBuilder::new().set_handler(&mut *self).build();
                if let Err(err) = reader.parse_uri(&href, None) {
                    error!(
                        self,
                        RngParseExternalRefParseFailure,
                        "Failed to parse '{}' for 'externalRef' because of '{}'.",
                        href,
                        err
                    );
                    return;
                } else if self.tree[current].children.is_empty() {
                    error!(
                        self,
                        RngParseExternalRefParseFailure,
                        "Failed to parse '{}' for 'externalRef'.",
                        href
                    );
                    return;
                }

                let new = self.tree[current].children[0];
                // If the imported element does not have an `ns` attribute,
                // it inherits the `ns` attribute from the `externalRef` element.
                if self.tree[new].ns.is_none() {
                    self.tree[new].ns = self.tree[current].ns.take();
                }

                // Recursively expand the imported elements.
                self.resolve_external_resource_recurse(current, 0, new);

                // If the root of the expanded pattern is `externalRef`,
                // `new` will be overwritten, so retrieve `new` again.
                let new = self.tree[current].children[0];
                if parent != usize::MAX {
                    // To remove `externalRef`, replace the child that referenced
                    // `externalRef` with the imported element.
                    self.tree[parent].children[nth] = new;
                } else {
                    // If there is no parent, simply swap.
                    self.tree.swap(0, new);
                }
                self.locator_stack.pop();
                assert_eq!(old, self.locator_stack.len());
                self.init = false;
            }
            RelaxNGNodeType::Include(_) => {
                // ISO/IEC 19757-2:2008 7.8 `include` element
                self.load_include(current);
            }
            _ => {
                let len = self.tree[current].children.len();
                for nth in 0..len {
                    let next = self.tree[current].children[nth];
                    self.resolve_external_resource_recurse(current, nth, next);
                }
            }
        }
    }
    fn load_include(
        &mut self,
        current: usize,
    ) -> Option<(Vec<(usize, usize)>, HashMap<Box<str>, Vec<(usize, usize)>>)> {
        match self.tree[current].r#type {
            RelaxNGNodeType::Include(_) => {
                let mut start = vec![];
                let mut define = HashMap::new();
                self.find_component(current, &mut start, &mut define);

                let RelaxNGNodeType::Include(href) =
                    std::mem::replace(&mut self.tree[current].r#type, RelaxNGNodeType::IncludeRoot)
                else {
                    unreachable!();
                };
                self.cur = current;
                self.init = false;
                let old_locator_stack_depth = self.locator_stack.len();
                let old_num_children = self.tree[current].children.len();

                if self
                    .locator_stack
                    .iter()
                    .any(|loc| *loc.system_id() == *href)
                {
                    error!(
                        self,
                        RngParseIncludeLoop, "'include' causes reference loop for '{}'.", href
                    );
                    self.unrecoverable = true;
                    return None;
                }

                let mut reader = XMLReaderBuilder::new().set_handler(&mut *self).build();
                if reader.parse_uri(&href, None).is_err()
                    || self.tree[current].children.len() == old_num_children
                {
                    error!(
                        self,
                        RngParseIncludeParseFailure, "Failed to parse '{}' for 'include'.", href
                    );
                    return None;
                }

                let new = self.tree[current].children[old_num_children];
                assert!(matches!(self.tree[new].r#type, RelaxNGNodeType::Grammar));
                let mut gstart = vec![];
                let mut gdefine = HashMap::new();
                self.expand_grammr(new, &mut gstart, &mut gdefine);

                if !start.is_empty() {
                    if gstart.is_empty() {
                        error!(
                            self,
                            RngParseInsufficientStartInInclude,
                            "'include' contains 'start', but included grammar does not contain."
                        );
                    }
                    // Remove the `start` component from the imported `grammar`.
                    // Since actual removal is costly, fill it with invalid values instead.
                    for (par, nth) in gstart.drain(..) {
                        self.tree[par].children[nth] = usize::MAX;
                    }
                }
                if start.len() < gstart.len() {
                    std::mem::swap(&mut start, &mut gstart);
                }
                start.extend(gstart);

                for name in define.keys() {
                    match gdefine.remove(name) {
                        Some(def) => {
                            // If there is a `define` with the same name in the `include` component,
                            // delete the `define` in the imported `grammar`.
                            for (par, nth) in def {
                                self.tree[par].children[nth] = usize::MAX;
                            }
                        }
                        None => {
                            error!(
                                self,
                                RngParseInsufficientDefineInInclude,
                                "'include' contains 'define' whose 'name' is '{}', but included grammar does not contain.",
                                name
                            );
                        }
                    }
                }
                if define.len() < gdefine.len() {
                    std::mem::swap(&mut define, &mut gdefine);
                }
                define.extend(gdefine);

                self.locator_stack.pop();
                assert_eq!(old_locator_stack_depth, self.locator_stack.len());
                self.init = false;

                // Replace `include` with `div`.
                // The replaced `div` inherits both the children of the original `include`
                // and the children of the imported `grammar`.
                self.tree[current].r#type = RelaxNGNodeType::Div(DivContentType::Grammar);
                self.tree[current].children.pop();
                let gch = std::mem::take(&mut self.tree[new].children);
                self.tree[current]
                    .children
                    .extend(gch.into_iter().filter(|&ch| ch != usize::MAX));

                Some((start, define))
            }
            _ => unreachable!(),
        }
    }
    fn expand_grammr(
        &mut self,
        current: usize,
        start: &mut Vec<(usize, usize)>,
        define: &mut HashMap<Box<str>, Vec<(usize, usize)>>,
    ) {
        let len = self.tree[current].children.len();
        for i in 0..len {
            let ch = self.tree[current].children[i];
            match self.tree[ch].r#type {
                RelaxNGNodeType::Start(_) => {
                    start.push((current, i));
                    self.resolve_external_resource_recurse(current, i, ch);
                }
                RelaxNGNodeType::Define { ref name, .. } => {
                    define.entry(name.clone()).or_default().push((current, i));
                    self.resolve_external_resource_recurse(current, i, ch);
                }
                RelaxNGNodeType::Div(_) => {
                    self.find_component(ch, start, define);
                }
                RelaxNGNodeType::Include(_) => {
                    if let Some((mut s, mut d)) = self.load_include(ch) {
                        if start.len() < s.len() {
                            std::mem::swap(start, &mut s);
                        }
                        start.extend(s);
                        if define.len() < d.len() {
                            std::mem::swap(define, &mut d);
                        }
                        define.extend(d);
                    }
                }
                _ => {}
            }
        }
    }
    fn find_component(
        &mut self,
        current: usize,
        start: &mut Vec<(usize, usize)>,
        define: &mut HashMap<Box<str>, Vec<(usize, usize)>>,
    ) {
        let len = self.tree[current].children.len();
        for i in 0..len {
            let ch = self.tree[current].children[i];
            match self.tree[ch].r#type {
                RelaxNGNodeType::Start(_) => {
                    start.push((current, i));
                    self.resolve_external_resource_recurse(current, i, ch);
                }
                RelaxNGNodeType::Define { ref name, .. } => {
                    define.entry(name.clone()).or_default().push((current, i));
                    self.resolve_external_resource_recurse(current, i, ch);
                }
                RelaxNGNodeType::Div(_) => {
                    self.find_component(ch, start, define);
                }
                _ => {}
            }
        }
    }

    /// # Reference
    /// - ISO/IEC 19757-2:2008 7.9 `name` attribute of `element` and `attribute` elements
    /// - ISO/IEC 19757-2:2008 7.10 `ns` attribute
    /// - ISO/IEC 19757-2:2008 7.11 QNames
    fn normalize_names(&mut self, current: usize, ns_stack: &mut NamespaceStack) {
        let old_ns_stack_depth = ns_stack.len();
        for (pre, nsname) in &self.tree[current].xmlns {
            ns_stack.push(pre, nsname);
        }

        let ns = self.tree[current].ns.clone();
        match self.tree[current].r#type {
            RelaxNGNodeType::Element(ref mut name) if name.is_some() => {
                let name = name.take().unwrap();
                let pos = self.tree.len();
                let node = RelaxNGNode {
                    base_uri: self.tree[current].base_uri.clone(),
                    datatype_library: None,
                    ns: self.tree[current].ns.clone(),
                    xmlns: HashMap::new(),
                    r#type: RelaxNGNodeType::Name(name),
                    children: vec![],
                };
                self.tree.push(node);
                self.tree[current].children.insert(0, pos);
            }
            RelaxNGNodeType::Attribute(ref mut name) if name.is_some() => {
                let name = name.take().unwrap();
                let pos = self.tree.len();
                let node = RelaxNGNode {
                    base_uri: self.tree[current].base_uri.clone(),
                    datatype_library: None,
                    // If an attribute element has a name attribute but no ns attribute,
                    // then an ns="" attribute is added to the name child element.
                    ns: Some(self.tree[current].ns.as_deref().unwrap_or_default().into()),
                    xmlns: HashMap::new(),
                    r#type: RelaxNGNodeType::Name(name),
                    children: vec![],
                };
                self.tree.push(node);
                self.tree[current].children.insert(0, pos);
            }
            RelaxNGNodeType::Name(ref mut name) => {
                if let Some((prefix, local_name)) = name.split_once(':') {
                    if let Some(namespace) = ns_stack.get(prefix) {
                        *name = local_name.into();
                        self.tree[current].ns = Some(namespace.namespace_name);
                    } else {
                        error!(
                            self,
                            RngParseUnresolvableNamespacePrefix,
                            "The namespace prefix '{}' is unresolvable.",
                            prefix
                        );
                    }
                }
            }
            RelaxNGNodeType::Value { ref mut ns_map, .. } => {
                // Update the namespace mapping using `ns_stack` information.
                // I think this should be done before importing external resources,
                // but if each external resource is namespace well-formed, constructing
                // it here should be fine.

                for namespace in &*ns_stack {
                    ns_map.insert(namespace.prefix.clone(), namespace.namespace_name.clone());
                }

                let ns = ns.unwrap_or_default();
                if ns.is_empty() {
                    ns_map.remove("");
                } else {
                    ns_map.insert("".into(), ns);
                }
            }
            _ => {}
        }

        let len = self.tree[current].children.len();
        for i in 0..len {
            let ch = self.tree[current].children[i];
            if ch == usize::MAX {
                continue;
            }

            // ISO/IEC 19757-2:2008 7.9
            // `attribute`s that have a `name` attribute but do not have an `ns` attribute
            // must be treated as having no namespace and must not inherit `ns`.
            // ```
            // If an attribute element has a name attribute but no ns attribute,
            // then an ns="" attribute is added to the name child element.
            // ```
            if self.tree[ch].ns.is_none()
                && !matches!(self.tree[ch].r#type, RelaxNGNodeType::Attribute(ref name) if name.is_some())
            {
                self.tree[ch].ns = self.tree[current].ns.clone().or_else(|| Some("".into()));
            }
            self.normalize_names(ch, ns_stack);
        }

        ns_stack.truncate(old_ns_stack_depth);
    }

    /// # Reference
    /// - ISO/IEC 19757-2:2008 7.12 `div` element
    fn flatten_div(&mut self, current: usize) {
        let len = self.tree[current].children.len();
        for i in 0..len {
            let ch = self.tree[current].children[i];
            if ch != usize::MAX {
                self.flatten_div(ch);
            }
        }

        match self.tree[current].r#type {
            RelaxNGNodeType::Grammar | RelaxNGNodeType::Div(_) => {
                // In full syntax, `grammar`, `div`, and `include` can directly contain `div`,
                // but since `include` is already fully expanded, only `grammar` and `div`
                // should match.

                let mut children = vec![];
                let len = self.tree[current].children.len();
                for i in 0..len {
                    let ch = self.tree[current].children[i];
                    if ch != usize::MAX {
                        match self.tree[ch].r#type {
                            RelaxNGNodeType::Div(_) => {
                                children.append(&mut self.tree[ch].children);
                            }
                            _ => {
                                children.push(ch);
                            }
                        }
                    }
                }

                self.tree[current].children = children;
            }
            _ => {}
        }
    }

    /// # Reference
    /// - ISO/IEC 19757-2:2008 7.13 Number of child elements
    /// - ISO/IEC 19757-2:2008 7.14 `mixed` element
    /// - ISO/IEC 19757-2:2008 7.15 `optional` element
    /// - ISO/IEC 19757-2:2008 7.16 `zeroOrMore` element
    fn simplification_13_to_16(&mut self, current: usize) {
        self.tree[current].children.retain(|&c| c != usize::MAX);

        let len = self.tree[current].children.len();
        for i in 0..len {
            let ch = self.tree[current].children[i];
            self.simplification_13_to_16(ch);
        }

        match self.tree[current].r#type {
            RelaxNGNodeType::Define { .. } | RelaxNGNodeType::List | RelaxNGNodeType::OneOrMore => {
                self.fold_children_with(current, RelaxNGNodeType::Group, 1);
            }
            RelaxNGNodeType::ZeroOrMore => {
                self.fold_children_with(current, RelaxNGNodeType::Group, 1);
                // ISO/IEC 19757-2:2008 7.16 `zeroOrMore` element
                // replace `zeroOrMore` to `choice`
                self.tree[current].r#type = RelaxNGNodeType::Choice(ChoiceType::Pattern);
                let new = self.tree.len();
                let children =
                    std::mem::replace(&mut self.tree[current].children, vec![new, new + 1]);
                // append `oneOrMore` with `zeroOrMore`'s children
                self.tree.push(RelaxNGNode {
                    base_uri: None,
                    datatype_library: None,
                    ns: None,
                    xmlns: HashMap::new(),
                    r#type: RelaxNGNodeType::OneOrMore,
                    children,
                });
                // append `empty`
                self.tree.push(RelaxNGNode {
                    base_uri: None,
                    datatype_library: None,
                    ns: None,
                    xmlns: HashMap::new(),
                    r#type: RelaxNGNodeType::Empty,
                    children: vec![],
                });
            }
            RelaxNGNodeType::Optional => {
                self.fold_children_with(current, RelaxNGNodeType::Group, 1);
                // ISO/IEC 19757-2:2008 7.15 `optional` element
                // replace `optional` to `choice`
                self.tree[current].r#type = RelaxNGNodeType::Choice(ChoiceType::Pattern);
                // append `empty`
                let new = self.tree.len();
                self.tree[current].children.push(new);
                self.tree.push(RelaxNGNode {
                    base_uri: None,
                    datatype_library: None,
                    ns: None,
                    xmlns: HashMap::new(),
                    r#type: RelaxNGNodeType::Empty,
                    children: vec![],
                });
            }
            RelaxNGNodeType::Mixed => {
                self.fold_children_with(current, RelaxNGNodeType::Group, 1);
                // ISO/IEC 19757-2:2008 7.14 `mixed` element
                // replace `mixed` to `interleave`
                self.tree[current].r#type = RelaxNGNodeType::Interleave;
                // append `text`
                let new = self.tree.len();
                self.tree[current].children.push(new);
                self.tree.push(RelaxNGNode {
                    base_uri: None,
                    datatype_library: None,
                    ns: None,
                    xmlns: HashMap::new(),
                    r#type: RelaxNGNodeType::Text,
                    children: vec![],
                });
            }
            RelaxNGNodeType::Element(_) => {
                self.fold_children_with(current, RelaxNGNodeType::Group, 2);
            }
            RelaxNGNodeType::Attribute(_) => {
                if self.tree[current].children.len() == 1 {
                    let new = self.tree.len();
                    self.tree[current].children.push(new);
                    self.tree.push(RelaxNGNode {
                        base_uri: None,
                        datatype_library: None,
                        ns: None,
                        xmlns: HashMap::new(),
                        r#type: RelaxNGNodeType::Text,
                        children: vec![],
                    });
                }
            }
            RelaxNGNodeType::Except(ExceptType::NameClass) => {
                self.fold_children_with(current, RelaxNGNodeType::Choice(ChoiceType::NameClass), 1);
            }
            RelaxNGNodeType::Except(ExceptType::Pattern) => {
                self.fold_children_with(current, RelaxNGNodeType::Choice(ChoiceType::Pattern), 1);
            }
            RelaxNGNodeType::Choice(_) | RelaxNGNodeType::Group | RelaxNGNodeType::Interleave => {
                if self.tree[current].children.len() == 1 {
                    // Since they have only one child, they replace themselves with their child.
                    let ch = self.tree[current].children[0];
                    self.tree.swap(current, ch);
                } else {
                    self.fold_children_with(current, self.tree[current].r#type.clone(), 2);
                }
            }
            _ => {}
        }
    }
    fn fold_children_with(&mut self, current: usize, r#type: RelaxNGNodeType, num_children: usize) {
        while self.tree[current].children.len() > num_children {
            let second = self.tree[current].children.pop().unwrap();
            let first = self.tree[current].children.pop().unwrap();
            let new = self.tree.len();
            self.tree[current].children.push(new);
            self.tree.push(RelaxNGNode {
                base_uri: None,
                datatype_library: None,
                ns: None,
                xmlns: HashMap::new(),
                r#type: r#type.clone(),
                children: vec![first, second],
            });
        }
    }

    /// # Reference
    /// - ISO/IEC 19757-2:2008 7.17 Constraints
    fn check_constraints(
        &mut self,
        current: usize,
        allow_anyname: bool,
        allow_nsname: bool,
        allow_xmlns: bool,
    ) {
        match self.tree[current].r#type {
            RelaxNGNodeType::AnyName => {
                if !allow_anyname {
                    error!(
                        self,
                        RngParseUnacceptablePattern,
                        "An 'except' element that is a child of an 'anyName' element shall not have any 'anyName' descendant elements."
                    );
                    return;
                }
                if !self.tree[current].children.is_empty() {
                    let ch = self.tree[current].children[0];
                    self.check_constraints(ch, false, allow_nsname, allow_xmlns);
                }
            }
            RelaxNGNodeType::NsName => {
                if !allow_nsname {
                    error!(
                        self,
                        RngParseUnacceptablePattern,
                        "An 'except' element that is a child of an 'nsName' element shall not have any 'nsName' or 'anyName' descendant elements."
                    );
                    return;
                }
                if !allow_xmlns
                    && self.tree[current].ns.as_deref().is_some_and(|ns| {
                        XML_NS_NAMESPACE.starts_with(ns)
                            && XML_NS_NAMESPACE.len().abs_diff(ns.len()) <= 1
                    })
                {
                    // The specification targets `http://www.w3.org/2000/xmlns`
                    // (without the trailing slash) as the constraint. However,
                    // considering the intent, it should also generate an error
                    // for `http://www.w3.org/2000/xmlns/`, which conforms to the
                    // namespace specification.
                    error!(
                        self,
                        RngParseUnacceptableAttribute,
                        "A 'name' element at the descendant of the first child of an 'attribute' element shall not have an 'ns' attribute with value '{}'.",
                        XML_NS_NAMESPACE
                    );
                }
                if !self.tree[current].children.is_empty() {
                    let ch = self.tree[current].children[0];
                    self.check_constraints(ch, false, false, allow_xmlns);
                }
            }
            RelaxNGNodeType::Name(ref name) => {
                if !allow_xmlns {
                    if self.tree[current]
                        .ns
                        .as_deref()
                        .is_none_or(|ns| ns.is_empty())
                        && name.as_ref() == "xmlns"
                    {
                        error!(
                            self,
                            RngParseUnacceptableAttribute,
                            "A 'name' element at the descendant of the first child of an 'attribute' element and that has an 'ns' attribute with empty value shall not have 'xmlns' as its content."
                        );
                    } else if self.tree[current].ns.as_deref().is_some_and(|ns| {
                        XML_NS_NAMESPACE.starts_with(ns)
                            && XML_NS_NAMESPACE.len().abs_diff(ns.len()) <= 1
                    }) {
                        // The specification targets `http://www.w3.org/2000/xmlns`
                        // (without the trailing slash) as the constraint. However,
                        // considering the intent, it should also generate an error
                        // for `http://www.w3.org/2000/xmlns/`, which conforms to the
                        // namespace specification.
                        error!(
                            self,
                            RngParseUnacceptableAttribute,
                            "A 'name' element at the descendant of the first child of an 'attribute' element shall not have an 'ns' attribute with value '{}'.",
                            XML_NS_NAMESPACE
                        );
                    }
                }
            }
            RelaxNGNodeType::Attribute(_) => {
                let nc = self.tree[current].children[0];
                self.check_constraints(nc, allow_anyname, allow_nsname, false);
                let pat = self.tree[current].children[1];
                self.check_constraints(pat, allow_anyname, allow_nsname, allow_xmlns);
            }
            RelaxNGNodeType::Data(ref type_name) => {
                if let Some(datatype_library) = self.tree[current].datatype_library.as_deref() {
                    if let Some(library) = self.datatype_libraries.get(datatype_library) {
                        let mut params = BTreeMap::new();
                        let len = self.tree[current].children.len();
                        for i in 0..len {
                            let ch = self.tree[current].children[i];
                            if let RelaxNGNodeType::Param { name, value } = &self.tree[ch].r#type {
                                params.insert(name.as_ref().into(), value.as_ref().into());
                            }
                        }
                        if !library.contains(type_name) {
                            error!(
                                self,
                                RngParseUnresolvableDatatypeLibrary,
                                "The type '{}' of the datatype library '{}' is unresolvabale.",
                                type_name,
                                datatype_library
                            );
                        } else if library
                            .validate_params(type_name, &params)
                            .is_none_or(|b| !b)
                        {
                            error!(
                                self,
                                RngParseUnresolvableDatatypeLibrary,
                                "The params for the type '{}' of the datatype library '{}' is invalid.",
                                type_name,
                                datatype_library
                            );
                        }
                    } else {
                        error!(
                            self,
                            RngParseUnresolvableDatatypeLibrary,
                            "The datatype library '{}' is unresolvabale.",
                            datatype_library
                        );
                    }

                    if let Some(&ch) = self.tree[current].children.last()
                        && matches!(self.tree[ch].r#type, RelaxNGNodeType::Except(_))
                    {
                        self.check_constraints(ch, allow_anyname, allow_nsname, allow_xmlns);
                    }
                }
            }
            RelaxNGNodeType::Value { ref r#type, .. } => {
                if let Some(datatype_library) = self.tree[current].datatype_library.as_deref() {
                    let type_name = r#type.as_deref().unwrap();
                    if let Some(library) = self.datatype_libraries.get(datatype_library) {
                        if !library.contains(type_name) {
                            error!(
                                self,
                                RngParseUnresolvableDatatypeLibrary,
                                "The type '{}' of the datatype library '{}' is unresolvabale.",
                                type_name,
                                datatype_library
                            );
                        }
                    } else {
                        error!(
                            self,
                            RngParseUnresolvableDatatypeLibrary,
                            "The datatype library '{}' is unresolvabale.",
                            datatype_library
                        );
                    }
                }
            }
            _ => {
                let len = self.tree[current].children.len();
                for i in 0..len {
                    let ch = self.tree[current].children[i];
                    self.check_constraints(ch, allow_anyname, allow_nsname, allow_xmlns);
                }
            }
        }
    }

    /// # Reference
    /// - ISO/IEC 19757-2:2008 7.18 `combine` attribute
    fn combine_start_and_define(&mut self, current: usize) {
        let len = self.tree[current].children.len();
        for i in 0..len {
            let ch = self.tree[current].children[i];
            self.combine_start_and_define(ch);
        }

        if matches!(self.tree[current].r#type, RelaxNGNodeType::Grammar) {
            let mut combine_start = None;
            let mut starts = vec![];
            let mut num_start_without_combine = 0;

            // for 'define'
            // key  : 'name' of 'define'
            // value: (num_define_without_combine, combine, defines)
            let mut define_mapping = HashMap::new();

            let mut others = vec![];

            let len = self.tree[current].children.len();
            for i in 0..len {
                let ch = self.tree[current].children[i];
                match &self.tree[ch].r#type {
                    RelaxNGNodeType::Start(com) => {
                        if let Some(com) = com.as_deref() {
                            if let Some(coms) = combine_start {
                                if coms != com {
                                    error!(
                                        self,
                                        RngParseUnacceptableCombine,
                                        "The 'combine' attribute values of multiple 'start' elements within the 'grammar' element are inconsistent."
                                    );
                                }
                            } else {
                                combine_start = Some(com);
                            }
                        } else {
                            num_start_without_combine += 1;
                            if num_start_without_combine == 2 {
                                error!(
                                    self,
                                    RngParseMultipleStartWithoutCombine,
                                    "Multiple 'start' element without 'combine' attribute appear."
                                );
                            }
                        }
                        starts.push(ch);
                    }
                    RelaxNGNodeType::Define { name, combine } => {
                        let (num_define_without_combine, combine_define, defines) = define_mapping
                            .entry(name.clone())
                            .or_insert_with(|| (0, None, vec![]));
                        if let Some(combine) = combine.as_deref() {
                            if let Some(other) = combine_define.as_deref()
                                && other != combine
                            {
                                error!(
                                    self,
                                    RngParseUnacceptableCombine,
                                    "The 'combine' attribute values of multiple 'define' elements within the 'grammar' element are inconsistent."
                                );
                            } else {
                                *combine_define = Some(combine.to_owned());
                            }
                        } else {
                            *num_define_without_combine += 1;
                            if *num_define_without_combine == 2 {
                                error!(
                                    self,
                                    RngParseMultipleStartWithoutCombine,
                                    "Multiple 'define' element without 'combine' attribute appear."
                                );
                            }
                        }
                        defines.push(ch);
                    }
                    _ => others.push(ch),
                }
            }

            // reconstruct 'start'
            if starts.len() == 1 {
                self.tree[current].children.push(starts[0]);
            } else if let Some(combine) = combine_start {
                let mut second = self.tree[starts.pop().unwrap()].children[0];

                let ty = if combine == "choice" {
                    RelaxNGNodeType::Choice(ChoiceType::Pattern)
                } else {
                    RelaxNGNodeType::Interleave
                };

                while let Some(next) = starts.pop() {
                    let next = self.tree[next].children[0];
                    let node = RelaxNGNode {
                        base_uri: None,
                        datatype_library: None,
                        ns: None,
                        xmlns: HashMap::new(),
                        r#type: ty.clone(),
                        children: vec![next, second],
                    };
                    second = self.tree.len();
                    self.tree.push(node);
                }

                let new = self.tree.len();
                self.tree[current].children.push(new);
                self.tree.push(RelaxNGNode {
                    base_uri: None,
                    datatype_library: None,
                    ns: None,
                    xmlns: HashMap::new(),
                    r#type: RelaxNGNodeType::Start(None),
                    children: vec![second],
                });
            } else if starts.is_empty() {
                error!(
                    self,
                    RngParseStartNotFoundInGrammar, "'start' is not found in 'grammar'"
                );
            }

            // reconstruct 'define'
            for (name, (_, combine, mut defines)) in define_mapping {
                if defines.len() == 1 {
                    self.tree[current].children.push(defines[0]);
                } else if let Some(combine) = combine {
                    let mut second = self.tree[defines.pop().unwrap()].children[0];

                    let ty = if combine == "choice" {
                        RelaxNGNodeType::Choice(ChoiceType::Pattern)
                    } else {
                        RelaxNGNodeType::Interleave
                    };

                    while let Some(next) = defines.pop() {
                        let next = self.tree[next].children[0];
                        let node = RelaxNGNode {
                            base_uri: None,
                            datatype_library: None,
                            ns: None,
                            xmlns: HashMap::new(),
                            r#type: ty.clone(),
                            children: vec![next, second],
                        };
                        second = self.tree.len();
                        self.tree.push(node);
                    }

                    let new = self.tree.len();
                    self.tree[current].children.push(new);
                    self.tree.push(RelaxNGNode {
                        base_uri: None,
                        datatype_library: None,
                        ns: None,
                        xmlns: HashMap::new(),
                        r#type: RelaxNGNodeType::Define {
                            name,
                            combine: None,
                        },
                        children: vec![second],
                    });
                }
            }

            self.tree[current].children.drain(..len);
            self.tree[current].children.extend(others);
        }
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 7.19 `grammar` element
    fn flatten_grammar(
        &mut self,
        current: usize,
        parent_define: &HashMap<Box<str>, (String, usize)>,
        in_scope_define: &mut HashMap<Box<str>, (String, usize)>,
        num_define: &mut usize,
    ) {
        let len = self.tree[current].children.len();
        for i in 0..len {
            let ch = self.tree[current].children[i];
            if let RelaxNGNodeType::Define { ref mut name, .. } = self.tree[ch].r#type {
                let alias = format!("{}", num_define);
                in_scope_define.insert(name.clone(), (alias.clone(), ch));
                *num_define += 1;
                *name = alias.into();
            }
        }

        for i in 0..len {
            let ch = self.tree[current].children[i];
            match self.tree[ch].r#type {
                RelaxNGNodeType::Grammar => {
                    self.flatten_grammar(ch, in_scope_define, &mut HashMap::new(), num_define);
                }
                RelaxNGNodeType::Ref(ref mut name) => {
                    if let Some(define) = in_scope_define.get(name) {
                        *name = define.0.as_str().into();
                    } else {
                        error!(
                            self,
                            RngParseUnresolvableRefName,
                            "The 'name' attribute of 'ref' has a value '{}', but it is unresolvable.",
                            name
                        );
                    }
                }
                RelaxNGNodeType::ParentRef(ref name) => {
                    if let Some(define) = parent_define.get(name) {
                        // replace 'parentRef' to 'ref'
                        self.tree[ch].r#type = RelaxNGNodeType::Ref(define.0.as_str().into());
                    } else {
                        error!(
                            self,
                            RngParseUnresolvableRefName,
                            "The 'name' attribute of 'parentRef' has a value '{}', but it is unresolvable.",
                            name
                        );
                    }
                }
                _ => {
                    self.flatten_grammar(ch, parent_define, in_scope_define, num_define);
                }
            }
        }

        if matches!(self.tree[current].r#type, RelaxNGNodeType::Grammar) {
            for (_, (alias, define)) in in_scope_define.drain() {
                if current != 0 {
                    self.tree[0].children.push(define);
                }
                self.defines.insert(alias, define);
            }

            if current != 0 {
                let start = self.tree[current].children[0];
                let children = self.tree[start].children[0];
                self.tree.swap(current, children);
            }
        }
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 7.20 define and ref elements
    fn simplification_20(
        &mut self,
        current: usize,
        root_define: usize,
        num_define: &mut usize,
        used_define: &mut HashSet<usize>,
    ) {
        match self.tree[current].r#type {
            RelaxNGNodeType::Grammar => {
                let start = self.tree[current].children[0];
                self.simplification_20(start, root_define, num_define, used_define);
            }
            RelaxNGNodeType::Start(_) => {
                let ch = self.tree[current].children[0];
                self.simplification_20(ch, root_define, num_define, used_define);
            }
            RelaxNGNodeType::Define { .. } => {
                let ch = self.tree[current].children[0];
                if matches!(self.tree[ch].r#type, RelaxNGNodeType::Element(_)) {
                    // Since there's no need to process the `element`, which is a child of `define`,
                    // I will skip straight to processing the grandchild.
                    let gch = self.tree[ch].children[1];
                    self.simplification_20(gch, current, num_define, used_define);
                } else {
                    self.simplification_20(ch, current, num_define, used_define);
                }
            }
            RelaxNGNodeType::Ref(ref name) => {
                let &define = self.defines.get(name.as_ref()).unwrap();
                if used_define.insert(define) {
                    self.simplification_20(define, root_define, num_define, used_define);
                }

                let chdef = self.tree[define].children[0];
                if !matches!(self.tree[chdef].r#type, RelaxNGNodeType::Element(_)) {
                    if define == root_define {
                        error!(
                            self,
                            RngParseRefLoop, "A reference loop is detected at 'ref'.",
                        );
                        return;
                    }
                    // expand `ref`
                    self.tree[current] = self.tree[chdef].clone();
                    self.tree[current].children.clear();
                    if !self.expand_ref(current, chdef) {
                        error!(self, RngParseRefLoop, "A reference loop is detected.",);
                    }
                }
            }
            RelaxNGNodeType::Element(_) => {
                // Since the first child is a name class, only the second child needs to be processed.
                let ch = self.tree[current].children[1];
                self.simplification_20(ch, root_define, num_define, used_define);

                // The processing of `define` skips its child `element`, so when reaching this point,
                // only `element` that is not a child of `define` needs to be considered.
                let alias = format!("{}", num_define);
                *num_define += 1;
                let newdef = self.tree.len();
                self.defines.insert(alias.clone(), newdef);
                used_define.insert(newdef);
                // new `define` with `element` child
                self.tree.push(RelaxNGNode {
                    base_uri: None,
                    datatype_library: None,
                    ns: None,
                    xmlns: HashMap::new(),
                    r#type: RelaxNGNodeType::Define {
                        name: alias.as_str().into(),
                        combine: None,
                    },
                    children: vec![newdef + 1],
                });
                // new `ref`
                self.tree.push(RelaxNGNode {
                    base_uri: None,
                    datatype_library: None,
                    ns: None,
                    xmlns: HashMap::new(),
                    r#type: RelaxNGNodeType::Ref(alias.into()),
                    children: vec![],
                });
                self.tree[0].children.push(newdef);
                self.tree.swap(current, newdef + 1);
            }
            _ => {
                let len = self.tree[current].children.len();
                for i in 0..len {
                    let ch = self.tree[current].children[i];
                    self.simplification_20(ch, root_define, num_define, used_define);
                }
            }
        }
    }
    /// Return `true` if `ref` is successfully expanded, otherwise return `false`.
    fn expand_ref(&mut self, dest: usize, src: usize) -> bool {
        let len = self.tree[src].children.len();
        for i in 0..len {
            let ch = self.tree[src].children[i];
            if let RelaxNGNodeType::Ref(name) = &self.tree[ch].r#type {
                let &define = self.defines.get(name.as_ref()).unwrap();
                let chdef = self.tree[define].children[0];
                if !matches!(self.tree[chdef].r#type, RelaxNGNodeType::Element(_)) {
                    // If the reference graph is a DAG, then `ref` pointing to a `define`
                    // that is not an `element` should be already expanded.
                    // If such a `ref` is found, report a reference loop.
                    return false;
                }
            }
            let node = self.tree[ch].clone();
            let new = self.tree.len();
            self.tree.push(node);
            self.tree[dest].children.push(new);
            if !self.expand_ref(new, ch) {
                return false;
            }
        }
        true
    }

    /// # Reference
    /// - ISO/IEC 19757-2:2008 7.21 `notAllowed` element
    /// - ISO/IEC 19757-2:2008 7.22 `empty` element
    fn simplification_21_to_22(&mut self, parent: usize, current: usize) {
        let len = self.tree[current].children.len();
        for i in 0..len {
            let ch = self.tree[current].children[i];
            self.simplification_21_to_22(current, ch);
        }

        if self.tree[current].children.is_empty() {
            return;
        }

        match self.tree[current].r#type {
            RelaxNGNodeType::Attribute(_) => {
                let ch = self.tree[current].children[1];
                if matches!(self.tree[ch].r#type, RelaxNGNodeType::NotAllowed) {
                    self.tree.swap(current, ch);
                }
            }
            RelaxNGNodeType::List => {
                let ch = self.tree[current].children[0];
                if matches!(self.tree[ch].r#type, RelaxNGNodeType::NotAllowed) {
                    self.tree.swap(current, ch);
                }
            }
            RelaxNGNodeType::Group | RelaxNGNodeType::Interleave => {
                let ch1 = self.tree[current].children[0];
                let ch2 = self.tree[current].children[1];
                match (&self.tree[ch1].r#type, &self.tree[ch2].r#type) {
                    (RelaxNGNodeType::NotAllowed, _) => {
                        self.tree.swap(current, ch1);
                    }
                    (_, RelaxNGNodeType::NotAllowed) => {
                        self.tree.swap(current, ch2);
                    }
                    (RelaxNGNodeType::Empty, _) => {
                        self.tree.swap(current, ch2);
                    }
                    (_, RelaxNGNodeType::Empty) => {
                        self.tree.swap(current, ch1);
                    }
                    _ => {}
                }
            }
            RelaxNGNodeType::OneOrMore => {
                let ch = self.tree[current].children[0];
                match self.tree[ch].r#type {
                    RelaxNGNodeType::NotAllowed | RelaxNGNodeType::Empty => {
                        self.tree.swap(current, ch)
                    }
                    _ => {}
                }
            }
            RelaxNGNodeType::Choice(_) => {
                let ch1 = self.tree[current].children[0];
                let ch2 = self.tree[current].children[1];
                match (&self.tree[ch1].r#type, &self.tree[ch2].r#type) {
                    (RelaxNGNodeType::NotAllowed, _) => {
                        self.tree.swap(current, ch2);
                    }
                    (_, RelaxNGNodeType::NotAllowed) => {
                        self.tree.swap(current, ch1);
                    }
                    (RelaxNGNodeType::Empty, RelaxNGNodeType::Empty) => {
                        self.tree.swap(current, ch1);
                    }
                    (_, RelaxNGNodeType::Empty) => {
                        self.tree[current].children.swap(0, 1);
                    }
                    _ => {}
                }
            }
            RelaxNGNodeType::Except(_) => {
                let ch = self.tree[current].children[0];
                if matches!(self.tree[ch].r#type, RelaxNGNodeType::NotAllowed) {
                    // Since `except` only appears as the youngest sibling,
                    // simply remove the last element from the parent's `children`.
                    self.tree[parent].children.pop();
                }
            }
            _ => {}
        }
    }

    fn remove_unreachable_define(&mut self) {
        // start from `start`
        let start = self.tree[0].children[0];
        let mut stack = vec![start];
        let mut used_define = HashSet::new();
        while let Some(current) = stack.pop() {
            stack.extend(self.tree[current].children.iter().cloned());
            if let RelaxNGNodeType::Ref(name) = &self.tree[current].r#type
                && let Some(&define) = self.defines.get(name.as_ref())
                && used_define.insert(define)
            {
                stack.push(define);
            }
        }

        self.defines.retain(|_, def| used_define.contains(def));
        self.tree[0]
            .children
            .retain(|&ch| ch == start || used_define.contains(&ch));
    }

    /// # Reference
    /// - ISO/IEC 19757-2:2008 10.2 Prohibited paths
    fn check_prohibited_paths(
        &mut self,
        current: usize,
        mut attr: bool,
        mut one_or_more: bool,
        mut ogroup: bool,
        mut ointerleave: bool,
        mut list: bool,
        mut except: bool,
        mut start: bool,
    ) {
        match self.tree[current].r#type {
            RelaxNGNodeType::Attribute(_) => {
                self.report_prohibited_path(attr, "attribute//attribute");
                self.report_prohibited_path(ogroup, "oneOrMore//group//attribute");
                self.report_prohibited_path(ointerleave, "oneOrMore//interleave//attribute");
                self.report_prohibited_path(list, "list//attribute");
                self.report_prohibited_path(except, "data/except//attribute");
                self.report_prohibited_path(start, "start//attribute");
                attr = true;
            }
            RelaxNGNodeType::Data(_) => {
                self.report_prohibited_path(start, "start//data");
            }
            RelaxNGNodeType::Empty => {
                self.report_prohibited_path(except, "data/except//empty");
                self.report_prohibited_path(start, "start//empty");
            }
            RelaxNGNodeType::Except(ExceptType::Pattern) => except = true,
            RelaxNGNodeType::Group => {
                self.report_prohibited_path(except, "data/except//group");
                self.report_prohibited_path(start, "start//group");
                if one_or_more {
                    ogroup = true;
                }
            }
            RelaxNGNodeType::Interleave => {
                self.report_prohibited_path(list, "list//interleave");
                self.report_prohibited_path(except, "data/except//interleave");
                self.report_prohibited_path(start, "start//interleave");
                if one_or_more {
                    ointerleave = true;
                }
            }
            RelaxNGNodeType::List => {
                self.report_prohibited_path(list, "list//list");
                self.report_prohibited_path(except, "data/except//list");
                self.report_prohibited_path(start, "start//list");
                list = true;
            }
            RelaxNGNodeType::OneOrMore => {
                self.report_prohibited_path(except, "data/except//oneOrMore");
                self.report_prohibited_path(start, "start//oneOrMore");
                one_or_more = true;
            }
            RelaxNGNodeType::Ref(_) => {
                self.report_prohibited_path(attr, "attribute//ref");
                self.report_prohibited_path(list, "list//ref");
                self.report_prohibited_path(except, "data/except//ref");
            }
            RelaxNGNodeType::Start(_) => start = true,
            RelaxNGNodeType::Text => {
                self.report_prohibited_path(list, "list//text");
                self.report_prohibited_path(except, "data/except//text");
                self.report_prohibited_path(start, "start//text");
            }
            RelaxNGNodeType::Value { .. } => {
                self.report_prohibited_path(start, "start//value");
            }
            _ => {}
        }

        let len = self.tree[current].children.len();
        for i in 0..len {
            let ch = self.tree[current].children[i];
            self.check_prohibited_paths(
                ch,
                attr,
                one_or_more,
                ogroup,
                ointerleave,
                list,
                except,
                start,
            );
        }
    }
    fn report_prohibited_path(&mut self, cond: bool, path: &'static str) {
        if cond {
            error!(
                self,
                RngParseProhibitedPath, "'{}' path is not allowed.", path
            );
        }
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 10.3 String sequences
    fn check_content_type(&mut self, current: usize) -> Option<ContentType> {
        match self.tree[current].r#type {
            RelaxNGNodeType::Grammar => {
                let len = self.tree[current].children.len();
                // It is necessary to check only `define`.
                for i in 1..len {
                    let ch = self.tree[current].children[i];
                    self.check_content_type(ch);
                }
                None
            }
            RelaxNGNodeType::Define { .. } => {
                let element = self.tree[current].children[0];
                let top = self.tree[element].children[1];
                if self.check_content_type(top).is_none() {
                    error!(
                        self,
                        RngParseUngroupablePattern, "Ungropuable pattern is found."
                    );
                }
                None
            }
            RelaxNGNodeType::NotAllowed => None,
            RelaxNGNodeType::Empty => Some(ContentType::Empty),
            RelaxNGNodeType::Text | RelaxNGNodeType::Ref(_) => Some(ContentType::Complex),
            RelaxNGNodeType::Value { .. } | RelaxNGNodeType::List => Some(ContentType::Simple),
            RelaxNGNodeType::Data(_) => {
                if let Some(&except) = self.tree[current].children.first() {
                    let ch = self.tree[except].children[0];
                    self.check_content_type(ch).map(|_| ContentType::Simple)
                } else {
                    Some(ContentType::Simple)
                }
            }
            RelaxNGNodeType::Attribute(_) => {
                let ch = self.tree[current].children[1];
                self.check_content_type(ch).map(|_| ContentType::Empty)
            }
            RelaxNGNodeType::OneOrMore => {
                let ch = self.tree[current].children[0];
                self.check_content_type(ch)
                    .filter(|ct| !matches!(ct, ContentType::Simple))
            }
            RelaxNGNodeType::Choice(_) => {
                let ch1 = self.tree[current].children[0];
                let ch2 = self.tree[current].children[1];

                let ct1 = self.check_content_type(ch1)?;
                let ct2 = self.check_content_type(ch2)?;
                Some(ct1.max(ct2))
            }
            RelaxNGNodeType::Group | RelaxNGNodeType::Interleave => {
                let ch1 = self.tree[current].children[0];
                let ch2 = self.tree[current].children[1];

                let ct1 = self.check_content_type(ch1)?;
                let ct2 = self.check_content_type(ch2)?;
                ct1.groupable(ct2).then(|| ct1.max(ct2))
            }
            _ => unreachable!("typename: {}", self.tree[current].r#type.typename()),
        }
    }

    fn display_to(&self, f: &mut std::fmt::Formatter<'_>, current: usize) -> std::fmt::Result {
        write!(f, "<{}", self.tree[current].r#type.typename())?;
        if let Some(datatype) = self.tree[current].datatype_library.as_deref() {
            write!(f, " datatypeLibrary=\"{}\"", datatype)?;
        }
        if let Some(ns) = self.tree[current].ns.as_deref() {
            write!(f, " ns=\"{}\"", ns)?;
        }
        match &self.tree[current].r#type {
            RelaxNGNodeType::Attribute(Some(name))
            | RelaxNGNodeType::Element(Some(name))
            | RelaxNGNodeType::ParentRef(name)
            | RelaxNGNodeType::Ref(name) => write!(f, " name=\"{}\">", name)?,
            RelaxNGNodeType::Data(ty) => write!(f, " type=\"{}\">", ty)?,
            RelaxNGNodeType::Define { name, combine } => {
                if let Some(combine) = combine {
                    write!(f, " combine=\"{}\"", combine)?;
                }
                write!(f, " name=\"{}\">", name)?;
            }
            RelaxNGNodeType::ExternalRef(href) | RelaxNGNodeType::Include(href) => {
                write!(f, " href=\"{}\">", href)?
            }
            RelaxNGNodeType::Name(name) => write!(f, ">{}", name)?,
            RelaxNGNodeType::Param { name, value } => {
                write!(f, " name=\"{}\">", name)?;
                write!(f, "{}", value)?;
            }
            RelaxNGNodeType::Start(Some(combine)) => write!(f, " combine=\"{}\">", combine)?,
            RelaxNGNodeType::Value { r#type, value, .. } => {
                if let Some(ty) = r#type {
                    write!(f, " type=\"{}\">", ty)?;
                }
                write!(f, "{}", value)?;
            }
            _ => write!(f, ">")?,
        }

        for &ch in &self.tree[current].children {
            self.display_to(f, ch)?;
        }

        write!(f, "</{}>", self.tree[current].r#type.typename())
    }

    pub(super) fn build(&mut self) -> Grammar {
        let mut grammar = Grammar {
            root: 0,
            libraries: self.datatype_libraries.clone(),
            patterns: vec![],
            intern: HashMap::new(),
            nullable: vec![],
        };
        let root = self.do_build(0, &mut grammar, &mut HashMap::new());
        grammar.root = root;
        grammar
    }
    fn do_build<'a>(
        &'a self,
        current: usize,
        grammar: &mut Grammar,
        defines: &mut HashMap<&'a str, usize>,
    ) -> usize {
        match &self.tree[current].r#type {
            RelaxNGNodeType::Grammar => {
                let ch = self.tree[current].children[0];
                self.do_build(ch, grammar, defines)
            }
            RelaxNGNodeType::Start(_) => {
                self.do_build(self.tree[current].children[0], grammar, defines)
            }
            RelaxNGNodeType::Define { name, .. } => {
                if let Some(&index) = defines.get(name.as_ref()) {
                    return index;
                }
                let index = self.create_define_node(name, grammar, defines);
                let element = self.tree[current].children[0];
                let nc = self.tree[element].children[0];
                let pat = self.tree[element].children[1];

                let nc = self.create_name_class(nc);
                let pat = self.do_build(pat, grammar, defines);
                grammar.patterns[index] = Arc::new(Pattern::Element(nc, pat));
                index
            }
            RelaxNGNodeType::Attribute(_) => {
                let nc = self.tree[current].children[0];
                let pat = self.tree[current].children[1];

                let nc = self.create_name_class(nc);
                let pat = self.do_build(pat, grammar, defines);
                grammar.create_node(Pattern::Attribute(nc, pat))
            }
            RelaxNGNodeType::Empty => grammar.create_node(Pattern::Empty),
            RelaxNGNodeType::NotAllowed => grammar.create_node(Pattern::NotAllowed),
            RelaxNGNodeType::Text => grammar.create_node(Pattern::Text),
            RelaxNGNodeType::Ref(name) => {
                if let Some(&index) = defines.get(name.as_ref()) {
                    return index;
                }
                let &ch = self.defines.get(name.as_ref()).unwrap();
                self.do_build(ch, grammar, defines)
            }
            RelaxNGNodeType::Choice(ChoiceType::Pattern) => {
                let mut left = self.do_build(self.tree[current].children[0], grammar, defines);
                let mut right = self.do_build(self.tree[current].children[1], grammar, defines);
                if left > right {
                    (left, right) = (right, left);
                }
                grammar.create_node(Pattern::Choice(left, right))
            }
            RelaxNGNodeType::Interleave => {
                let mut left = self.do_build(self.tree[current].children[0], grammar, defines);
                let mut right = self.do_build(self.tree[current].children[1], grammar, defines);
                if left > right {
                    (left, right) = (right, left);
                }
                grammar.create_node(Pattern::Interleave(left, right))
            }
            RelaxNGNodeType::Group => {
                let left = self.do_build(self.tree[current].children[0], grammar, defines);
                let right = self.do_build(self.tree[current].children[1], grammar, defines);
                grammar.create_node(Pattern::Group(left, right))
            }
            RelaxNGNodeType::OneOrMore => {
                let p = self.do_build(self.tree[current].children[0], grammar, defines);
                grammar.create_node(Pattern::OneOrMore(p))
            }
            RelaxNGNodeType::List => {
                let p = self.do_build(self.tree[current].children[0], grammar, defines);
                grammar.create_node(Pattern::List(p))
            }
            RelaxNGNodeType::Data(r#type) => {
                let uri = self.tree[current]
                    .datatype_library
                    .clone()
                    .unwrap_or_default();
                let local_name = r#type.as_ref().into();
                let mut params = BTreeMap::new();
                let mut except = usize::MAX;
                let len = self.tree[current].children.len();
                for i in 0..len {
                    let ch = self.tree[current].children[i];
                    match &self.tree[ch].r#type {
                        RelaxNGNodeType::Param { name, value } => {
                            params.insert(name.as_ref().into(), value.as_ref().into());
                        }
                        RelaxNGNodeType::Except(ExceptType::Pattern) => {
                            let gch = self.tree[ch].children[0];
                            except = self.do_build(gch, grammar, defines);
                        }
                        _ => unreachable!(),
                    }
                }

                if except == usize::MAX {
                    grammar.create_node(Pattern::Data((uri, local_name), params))
                } else {
                    grammar.create_node(Pattern::DataExcept((uri, local_name), params, except))
                }
            }
            RelaxNGNodeType::Value {
                r#type,
                ns_map,
                value,
            } => {
                let uri = self.tree[current]
                    .datatype_library
                    .clone()
                    .unwrap_or_default();
                let local_name = r#type.as_deref().unwrap().into();
                let base_uri = self.tree[current].base_uri.clone().unwrap();
                grammar.create_node(Pattern::Value(
                    (uri, local_name),
                    value.as_ref().into(),
                    (base_uri, ns_map.clone()),
                ))
            }
            _ => unreachable!(),
        }
    }

    fn create_define_node<'a>(
        &'a self,
        name: &'a str,
        grammar: &mut Grammar,
        defines: &mut HashMap<&'a str, usize>,
    ) -> usize {
        let ret = grammar.patterns.len();
        defines.insert(name, ret);
        grammar.patterns.push(Arc::new(Pattern::Text));
        grammar.nullable.push(-1);
        ret
    }

    fn create_name_class(&self, current: usize) -> Arc<NameClass> {
        match &self.tree[current].r#type {
            RelaxNGNodeType::AnyName => {
                if !self.tree[current].children.is_empty() {
                    let ch = self.tree[current].children[0];
                    let gch = self.tree[ch].children[0];
                    Arc::new(NameClass::AnyNameExcept(self.create_name_class(gch)))
                } else {
                    Arc::new(NameClass::AnyName)
                }
            }
            RelaxNGNodeType::NsName => {
                let ns = self.tree[current].ns.clone().unwrap_or_default();
                if !self.tree[current].children.is_empty() {
                    let ch = self.tree[current].children[0];
                    let gch = self.tree[ch].children[0];
                    Arc::new(NameClass::NsNameExcept(ns, self.create_name_class(gch)))
                } else {
                    Arc::new(NameClass::NsName(ns))
                }
            }
            RelaxNGNodeType::Name(name) => {
                let ns = self.tree[current].ns.clone().unwrap_or_default();
                Arc::new(NameClass::Name(ns, name.as_ref().into()))
            }
            RelaxNGNodeType::Choice(ChoiceType::NameClass) => {
                let left = self.create_name_class(self.tree[current].children[0]);
                let right = self.create_name_class(self.tree[current].children[1]);
                Arc::new(NameClass::NameClassChoice(left, right))
            }
            _ => unreachable!(),
        }
    }

    /// # Reference
    /// - ISO/IEC 19757-2:2008 10.4 Restrictions on attributes
    fn check_attributes_restrictions(
        &mut self,
        current: usize,
        repeat: bool,
        grammar: &Grammar,
        names: &mut Vec<Arc<NameClass>>,
        memo: &mut HashSet<usize>,
    ) {
        match grammar.patterns[current].as_ref() {
            Pattern::Attribute(nc, pattern) => {
                if names.iter().any(|nc2| nc.overlap(nc2)) {
                    error!(
                        self,
                        RngParseConflictAttributeNameClass,
                        "The nameclass of attribute is duplicate."
                    );
                }
                if nc.is_infinite() {
                    if !repeat {
                        error!(
                            self,
                            RngParseUnrepeatedAttributeWithInfiniteNameClass,
                            "Attributes using infinite name classes shall be repeated."
                        );
                    }
                    if !matches!(grammar.patterns[*pattern].as_ref(), Pattern::Text) {
                        error!(
                            self,
                            RngParseUnacceptablePattern,
                            "Attributes using infinite name classes shall have 'text' as their value."
                        );
                    }
                }
                names.push(nc.clone());
            }
            Pattern::Element(_, pattern) => {
                if memo.insert(*pattern) {
                    self.check_attributes_restrictions(*pattern, false, grammar, &mut vec![], memo);
                }
            }
            Pattern::Choice(p1, p2) => {
                let len = names.len();
                self.check_attributes_restrictions(*p1, repeat, grammar, names, memo);
                let tmp = names.split_off(len);
                self.check_attributes_restrictions(*p2, repeat, grammar, names, memo);
                names.extend(tmp);
            }
            Pattern::Interleave(p1, p2) | Pattern::Group(p1, p2) => {
                self.check_attributes_restrictions(*p1, repeat, grammar, names, memo);
                self.check_attributes_restrictions(*p2, repeat, grammar, names, memo);
            }
            Pattern::OneOrMore(p) => {
                self.check_attributes_restrictions(*p, true, grammar, names, memo);
            }
            _ => {}
        }
    }

    /// # Reference
    /// - ISO/IEC 19757-2:2008 10.5 Restrictions on `interleave`
    fn check_interleave_restrictions(
        &mut self,
        current: usize,
        grammar: &Grammar,
        names: &mut Vec<Arc<NameClass>>,
        memo: &mut HashSet<usize>,
    ) {
        match grammar.patterns[current].as_ref() {
            Pattern::Element(nc, pattern) => {
                if names.iter().any(|nc2| nc.overlap(nc2)) {
                    error!(
                        self,
                        RngParseConflictElementNameClass, "The nameclass of element is duplicate."
                    );
                }
                names.push(nc.clone());
                if memo.insert(*pattern) {
                    self.check_interleave_restrictions(*pattern, grammar, &mut vec![], memo);
                }
            }
            Pattern::Text => {
                let dum = Arc::new(NameClass::Name("".into(), "#text".into()));
                if names.contains(&dum) {
                    error!(
                        self,
                        RngParseConflictElementNameClass,
                        "A text pattern shall not occur in both children of 'interleave'."
                    );
                } else {
                    names.push(dum);
                }
            }
            Pattern::Choice(p1, p2) | Pattern::Group(p1, p2) => {
                let len = names.len();
                self.check_interleave_restrictions(*p1, grammar, names, memo);
                let tmp = names.split_off(len);
                self.check_interleave_restrictions(*p2, grammar, names, memo);
                names.extend(tmp);
            }
            Pattern::Interleave(p1, p2) => {
                self.check_interleave_restrictions(*p1, grammar, names, memo);
                self.check_interleave_restrictions(*p2, grammar, names, memo);
            }
            Pattern::OneOrMore(p) => {
                self.check_interleave_restrictions(*p, grammar, names, memo);
            }
            _ => {}
        }
    }
}

impl<H: SAXHandler> std::fmt::Display for RelaxNGParseHandler<H> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.display_to(f, 0)
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
            self.defines.clear();
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
            if self.tree.is_empty() {
                error!(
                    self,
                    RngParseUnacceptablePattern, "The schema root is not RELAX NG element."
                );
            }
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
                            ns_map: BTreeMap::new(),
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
                            ns_map: BTreeMap::new(),
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
                    if value != "choice" && value != "interleave" {
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
                    if combine != "choice" && combine != "interleave" {
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
        if self.unrecoverable {
            return;
        }
        if self.ignore_depth > 0 {
            self.ignore_depth -= 1;
            return;
        }

        if !self.text.is_empty() {
            match &mut self.tree[self.cur].r#type {
                RelaxNGNodeType::Value { value, .. } | RelaxNGNodeType::Param { value, .. } => {
                    *value = self.text.as_str().into();
                }
                RelaxNGNodeType::Name(name) => {
                    let trimmed = self
                        .text
                        .trim_matches(|c| XMLVersion::default().is_whitespace(c));
                    *name = trimmed.into();
                    if !XMLVersion::default().validate_qname(trimmed) {
                        error!(
                            self,
                            RngParseInvalidQName,
                            "The text content '{}' of 'name' is not a QName.",
                            trimmed
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
        if self.ignore_depth > 0 {
            return;
        }
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

impl Default for RelaxNGParseHandler {
    fn default() -> Self {
        Self::with_handler(DefaultSAXHandler)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn full_syntax_parsing_tests() {
        let mut reader = XMLReaderBuilder::new()
            .set_handler(RelaxNGParseHandler::default())
            .build();

        reader.parse_str(r#"<element name="foo" xmlns="http://relaxng.org/ns/structure/1.0" ns=""><empty/></element>"#, None).unwrap();
        assert!(reader.handler.last_error.is_ok());
        assert!(reader.handler.simplification().is_ok());
        reader.parse_str(r#"<grammar xmlns="http://relaxng.org/ns/structure/1.0"><start><ref name="&#xE14;&#xE35;"/></start><define name="&#xE14;&#xE35;"><element name="foo"><empty/></element></define></grammar>"#, None).unwrap();
        assert!(reader.handler.last_error.is_ok());
        assert!(reader.handler.simplification().is_ok());
        reader.parse_str(r#"<element xmlns="http://relaxng.org/ns/structure/1.0" name="foo"><empty><ext xmlns="http://www.example.com"><element xmlns="http://relaxng.org/ns/structure/1.0"/></ext></empty></element>"#, None).unwrap();
        assert!(reader.handler.last_error.is_ok());
        assert!(reader.handler.simplification().is_ok());
        reader.parse_str(r#"<grammar xmlns="http://relaxng.org/ns/structure/1.0"  xmlns:eg="http://www.example.com" eg:comment=""><start eg:comment=""><element eg:comment=""><name eg:comment="">foo</name><data eg:comment="" type="string"/><empty eg:comment=""/></element></start></grammar>"#, None).unwrap();
        assert!(reader.handler.last_error.is_ok());
        assert!(reader.handler.simplification().is_ok());
        reader.parse_str(r#"<element xmlns="http://relaxng.org/ns/structure/1.0" name="elem"><data type="string" /></element>"#, None).unwrap();
        assert!(reader.handler.last_error.is_ok());
        assert!(reader.handler.simplification().is_ok());
        reader
            .parse_str(
                r#"<element xmlns="http://relaxng.org/ns/structure/1.0" name="elem1"><attribute><choice><name>att1</name><name>att2</name></choice></attribute></element>"#,
                None,
            )
            .unwrap();
        assert!(reader.handler.last_error.is_ok());
        assert!(reader.handler.simplification().is_ok());
        reader
            .parse_str(
                r#"<element xmlns="http://relaxng.org/ns/structure/1.0"><choice><name>elem1</name><name>elem2</name></choice><empty/></element>"#,
                None,
            )
            .unwrap();
        assert!(reader.handler.last_error.is_ok());
        assert!(reader.handler.simplification().is_ok());
    }

    #[test]
    fn simplification_tests() {
        let base_uri = URIString::parse_file_path(env!("CARGO_MANIFEST_DIR")).unwrap();

        let path =
            base_uri.resolve(&URIString::parse("resources/relaxng/spec-example.rng").unwrap());
        eprintln!("{}", path);
        let mut reader = XMLReaderBuilder::new()
            .set_handler(RelaxNGParseHandler::default())
            .build();
        reader.parse_uri(path, None).unwrap();
        assert!(reader.handler.last_error.is_ok());
        reader.handler.simplification().unwrap();
        assert!(reader.handler.last_error.is_ok());
        let grammar = reader.handler.build_grammar().unwrap();

        let schema = format!("{}", grammar);
        assert_eq!(
            schema,
            r#"<grammar xmlns="http://relaxng.org/ns/structure/1.0"><start><ref name="2"/></start><define name="0"><element><name ns="http://www.example.com/n1">bar1</name><empty/></element></define><define name="1"><element><name ns="http://www.example.com/n2">bar2</name><empty/></element></define><define name="2"><element><name ns="">foo</name><group><ref name="0"/><ref name="1"/></group></element></define></grammar>"#
        );

        reader.parse_str(r#"
            <element xmlns="http://relaxng.org/ns/structure/1.0" ns="http://www.example.com" name="foo">
                <attribute name="bar" />
            </element>"#, None).unwrap();
        assert!(reader.handler.last_error.is_ok());
        assert!(reader.handler.simplification().is_ok());
        let grammar = reader.handler.build_grammar().unwrap();
        let schema = format!("{}", grammar);
        assert_eq!(
            schema,
            r#"<grammar xmlns="http://relaxng.org/ns/structure/1.0"><start><ref name="0"/></start><define name="0"><element><name ns="http://www.example.com">foo</name><attribute><name ns="">bar</name><text/></attribute></element></define></grammar>"#
        );

        reader
            .parse_str(
                r#"
            <grammar xmlns="http://relaxng.org/ns/structure/1.0">
                <start><element name="foo"><ref name="bars" /></element></start>
                <define name="bars">
                    <element name="bar"><empty /></element>
                    <element name="bar"><empty /></element>
                    <element name="bar"><empty /></element>
                </define>
            </grammar>"#,
                None,
            )
            .unwrap();
        assert!(reader.handler.last_error.is_ok());
        assert!(reader.handler.simplification().is_ok());
        let grammar = reader.handler.build_grammar().unwrap();
        let schema = format!("{}", grammar);
        assert_eq!(
            schema,
            r#"<grammar xmlns="http://relaxng.org/ns/structure/1.0"><start><ref name="4"/></start><define name="1"><element><name ns="">bar</name><empty/></element></define><define name="2"><element><name ns="">bar</name><empty/></element></define><define name="3"><element><name ns="">bar</name><empty/></element></define><define name="4"><element><name ns="">foo</name><group><ref name="1"/><group><ref name="2"/><ref name="3"/></group></group></element></define></grammar>"#
        );

        reader
            .parse_str(
                r#"
                <element xmlns="http://relaxng.org/ns/structure/1.0" name="foo">
                    <value type="string" datatypeLibrary=""> x</value>
                </element>"#,
                None,
            )
            .unwrap();
        assert!(reader.handler.last_error.is_ok());
        assert!(reader.handler.simplification().is_ok());
        let grammar = reader.handler.build_grammar().unwrap();
        let schema = format!("{}", grammar);
        assert_eq!(
            schema,
            r#"<grammar xmlns="http://relaxng.org/ns/structure/1.0"><start><ref name="0"/></start><define name="0"><element><name ns="">foo</name><value datatypeLibrary="" type="string" ns=""> x</value></element></define></grammar>"#
        );

        reader
            .parse_str(
                r#"
                <choice xmlns="http://relaxng.org/ns/structure/1.0">
                    <element name="foo"><empty /></element>
                    <group>
                        <notAllowed />
                        <element name="bar">
                            <group>
                                <data type="token" />
                                <data type="token" />
                            </group>
                        </element>
                    </group>
                </choice>"#,
                None,
            )
            .unwrap();
        assert!(reader.handler.last_error.is_ok());
        assert!(reader.handler.simplification().is_ok());
        eprintln!("{}", reader.handler);
        let grammar = reader.handler.build_grammar().unwrap();
        let schema = format!("{}", grammar);
        assert_eq!(
            schema,
            r#"<grammar xmlns="http://relaxng.org/ns/structure/1.0"><start><ref name="0"/></start><define name="0"><element><name ns="">foo</name><empty/></element></define></grammar>"#
        );
    }
}
