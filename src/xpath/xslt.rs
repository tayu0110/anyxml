//! Implementation of the core function library added in
//! [Chapter 12 of XSLT 1.0](https://www.w3.org/TR/xslt-10/#add-func)
//! and [Chapter 15 of XSLT 1.0](https://www.w3.org/TR/xslt-10/#fallback).

use std::{borrow::Cow, collections::HashMap, sync::Arc};

use crate::{
    XMLVersion,
    error::XMLError,
    sax::XMLReader,
    tree::{Document, Node, TreeBuildHandler, node::NodeSpec},
    uri::URIString,
    xpath::{
        Axis, FunctionLibrary, NodeTest, XPathCompileError, XPathContext, XPathError,
        XPathExpression, XPathNodeSet, XPathObject, XPathSyntaxTree, compile, parse_literal,
        parse_node_test, parse_predicate, skip_whitespaces,
    },
};

pub const XML_XSLT_NAMESPACE: &str = "http://www.w3.org/1999/XSL/Transform";

/// The context required to execute an XSLT extension function.
#[derive(Clone)]
pub struct XSLTContext {
    /// [current node](https://www.w3.org/TR/xslt-10/#dt-current-node).  \
    /// It is usually equal to the context node at the start of the expression evaluation.
    ///
    /// # Reference
    /// - [4 Expressions](https://www.w3.org/TR/xslt-10/#section-Expressions)
    pub(super) current_node: Option<Node<dyn NodeSpec>>,
    /// Cache of generated document nodes.  \
    /// This is necessary to prevent a document node (and its descendant nodes) from being
    /// generated more than once from a given URI.
    pub doc_cache: HashMap<URIString, Document>,
    /// The base URI of the style sheet.  \
    /// If the second argument of the `document` function is omitted, it may
    /// be used to resolve the relative reference that is the first argument.
    pub stylesheet_base_uri: URIString,
    /// A set of defined `xsl:key` elements.  \
    /// Each key consists of a pair of namespace name and local name—the result of
    /// expanding the `name` attribute—and its value is a pair of `match` and `use`.
    pub keys: HashMap<(Option<Cow<'static, str>>, Cow<'static, str>), KeyDef>,
    /// A set of defined `xsl:decimal-format` elements.  \
    /// Each key consists of a pair of namespace name and local name—the result of
    /// expanding the `name` attribute—and its value is a pair of `match` and `use`.
    pub fmts: HashMap<(Option<Cow<'static, str>>, Cow<'static, str>), DecimalFormat>,
}

impl Default for XSLTContext {
    fn default() -> Self {
        let stylesheet_base_uri = std::env::current_dir()
            .map_err(XMLError::from)
            .and_then(|p| URIString::parse_file_path(p).map_err(XMLError::from))
            .unwrap_or_else(|_| URIString::parse("file:///").unwrap())
            .resolve(&URIString::parse("document.xml").unwrap());
        Self {
            current_node: None,
            doc_cache: HashMap::new(),
            stylesheet_base_uri,
            keys: HashMap::new(),
            fmts: HashMap::new(),
        }
    }
}

#[derive(Clone, Default)]
pub struct KeyDef {
    pub r#match: Box<str>,
    pub r#use: Box<str>,
}

pub(super) fn register_xslt_functions(library: &mut FunctionLibrary) {
    // 12.1 Multiple Source Documents
    library.register_function("document", document);
    // 12.2 Keys
    library.register_function("key", key);
    // 12.3 Number Formatting
    library.register_function("format-number", format_number);
    // 12.4 Miscellaneous Additional Functions
    library.register_function("current", current);
    library.register_function("unparsed-entity-uri", unparsed_entity_uri);
    library.register_function("generate-id", generate_id);
    library.register_function("system-property", system_property);
    // 15 Fallback
    library.register_function("element-available", element_available);
    library.register_function("function-available", function_available);
}

/// "document" function.
///
/// # Reference
/// - [Function: node-set document(object, node-set?)](https://www.w3.org/TR/xslt-10/#function-document)
fn document(context: &mut XPathContext, num_args: usize) -> Result<XPathObject, XPathError> {
    if num_args == 0 || num_args > 2 {
        return Err(XPathError::IncorrectNumberOfArgument);
    }

    let mut uris = vec![];
    if num_args == 2 {
        let mut second = context
            .pop_object()
            .ok_or(XPathError::IncorrectNumberOfArgument)?
            .as_nodeset()?;
        second.sort();
        let base_uri = if second.is_empty() {
            context.xslt.stylesheet_base_uri.clone()
        } else {
            second[0].base_uri().unwrap_or(
                second[0]
                    .owner_document()
                    .document_base_uri()
                    .as_ref()
                    .into(),
            )
        };
        let first = context
            .pop_object()
            .ok_or(XPathError::IncorrectNumberOfArgument)?;
        if let XPathObject::NodeSet(nodeset) = first {
            for node in &nodeset {
                let uri = URIString::parse(node.xpath_string_value())
                    .map_err(|_| XPathError::UnresolvableURI)?;
                uris.push(base_uri.resolve(&uri));
            }
        } else {
            let uri =
                URIString::parse(first.as_string()?).map_err(|_| XPathError::UnresolvableURI)?;
            uris.push(base_uri.resolve(&uri));
        }
    } else {
        let obj = context
            .pop_object()
            .ok_or(XPathError::IncorrectNumberOfArgument)?;
        if let XPathObject::NodeSet(nodeset) = obj {
            for node in &nodeset {
                let uri = URIString::parse(node.xpath_string_value())
                    .map_err(|_| XPathError::UnresolvableURI)?;
                let base_uri = node
                    .base_uri()
                    .unwrap_or(node.owner_document().document_base_uri().as_ref().into());
                uris.push(base_uri.resolve(&uri));
            }
        } else {
            let uri =
                URIString::parse(obj.as_string()?).map_err(|_| XPathError::UnresolvableURI)?;
            uris.push(context.xslt.stylesheet_base_uri.resolve(&uri));
        }
    }

    let mut handler = TreeBuildHandler::default();
    handler.coalescing = true;
    handler.expand_entity_reference = true;
    let mut reader = XMLReader::builder().set_handler(handler).build();
    let mut ret = XPathNodeSet::default();
    for uri in uris {
        if let Some(doc) = context.xslt.doc_cache.get(&uri) {
            ret.push(doc);
            continue;
        }

        if reader.parse_uri(uri.as_ref(), None).is_ok() {
            let doc = reader.handler.document.clone();
            ret.push(doc.clone());
            context.xslt.doc_cache.insert(uri, doc);
        }
    }
    Ok(ret.into())
}

/// "key" function.
///
/// # Reference
/// - [Function: node-set key(string, object)](https://www.w3.org/TR/xslt-10/#function-key)
fn key(context: &mut XPathContext, num_args: usize) -> Result<XPathObject, XPathError> {
    if num_args != 2 {
        return Err(XPathError::IncorrectNumberOfArgument);
    }

    let obj = context
        .pop_object()
        .ok_or(XPathError::IncorrectNumberOfArgument)?;
    let name = context
        .pop_object()
        .ok_or(XPathError::IncorrectNumberOfArgument)?
        .as_string()?;

    if !XMLVersion::XML10.validate_qname(&name) {
        return Err(XPathError::IncorrectArgumentType);
    }

    let (namespace, name) = if let Some((pre, loc)) = name.split_once(':') {
        let namespace = context
            .namespaces
            .get(Some(pre))
            .ok_or(XPathError::InvalidNamespacePrefix)?;
        (Some(Cow::Borrowed(namespace)), loc)
    } else {
        (None, name.as_ref())
    };

    let key = context
        .xslt
        .keys
        .get(&(namespace, Cow::Borrowed(name)))
        .ok_or(XPathError::UnresolvableKey)?;

    let mut r#match = compile_pattern(&key.r#match)?;
    let mut r#use = compile(&key.r#use)?;
    let document = context
        .node
        .clone()
        .ok_or(XPathError::InternalError)?
        .owner_document();
    let mut ret = XPathNodeSet::default();
    let matches = r#match.evaluate(document)?.as_nodeset()?;
    let mut values = match obj {
        XPathObject::NodeSet(ns) => ns
            .iter()
            .map(|n| n.xpath_string_value())
            .collect::<Vec<_>>(),
        object => {
            vec![object.as_string()?.into()]
        }
    };
    values.sort_unstable();
    values.dedup();
    for x in matches {
        let u = r#use.evaluate(x.clone())?;
        match u {
            XPathObject::NodeSet(u) => {
                if u.iter().map(|u| u.xpath_string_value()).any(|u| {
                    values
                        .binary_search_by_key(&u.as_ref(), |s| s.as_str())
                        .is_ok()
                }) {
                    ret.push(x);
                }
            }
            u => {
                let u = u.as_string()?;
                if values
                    .binary_search_by_key(&u.as_ref(), |s| s.as_str())
                    .is_ok()
                {
                    ret.push(x);
                }
            }
        }
    }
    ret.sort();
    Ok(ret.into())
}

/// "format-number" function.
///
/// # Reference
/// - [Function: string format-number(number, string, string?)](https://www.w3.org/TR/xslt-10/#function-format-number)
fn format_number(context: &mut XPathContext, num_args: usize) -> Result<XPathObject, XPathError> {
    if num_args != 2 && num_args != 3 {
        return Err(XPathError::IncorrectNumberOfArgument);
    }

    let name = (num_args == 3)
        .then(|| context.pop_object())
        .map(|obj| obj.ok_or(XPathError::IncorrectNumberOfArgument))
        .transpose()?
        .map(|obj| obj.as_string())
        .transpose()?;
    let name = name.as_deref().unwrap_or_default();
    let pattern = context
        .pop_object()
        .ok_or(XPathError::IncorrectNumberOfArgument)?
        .as_string()?;
    let number = context
        .pop_object()
        .ok_or(XPathError::IncorrectNumberOfArgument)?
        .as_number()?;

    if !name.is_empty() && !XMLVersion::XML10.validate_qname(name) {
        return Err(XPathError::IncorrectArgumentType);
    }

    let (namespace, name) = if let Some((pre, loc)) = name.split_once(':') {
        let namespace = context
            .namespaces
            .get(Some(pre))
            .ok_or(XPathError::InvalidNamespacePrefix)?;
        (Some(Cow::Borrowed(namespace)), loc)
    } else {
        (None, name)
    };
    let format = context
        .xslt
        .fmts
        .get(&(namespace, Cow::Borrowed(name)))
        .ok_or(XPathError::UnresolvableNumberFormat)?;

    let mut result = String::new();
    format_number_conversion(format, &pattern, number, &mut result)?;
    Ok(result.into())
}

/// "current" function.
///
/// # Reference
/// - [Function: node-set current()](https://www.w3.org/TR/xslt-10/#function-current)
fn current(context: &mut XPathContext, num_args: usize) -> Result<XPathObject, XPathError> {
    if num_args != 0 {
        return Err(XPathError::IncorrectNumberOfArgument);
    }
    let mut node_set = XPathNodeSet::default();
    let Some(node) = context.xslt.current_node.clone() else {
        return Err(XPathError::InvalidXSLTContext);
    };
    node_set.push(node);
    Ok(node_set.into())
}

/// "unparsed-entity-uri" function.
///
/// # Reference
/// - [Function: string unparsed-entity-uri(string)](https://www.w3.org/TR/xslt-10/#function-unparsed-entity-uri)
fn unparsed_entity_uri(
    context: &mut XPathContext,
    num_args: usize,
) -> Result<XPathObject, XPathError> {
    if num_args != 1 {
        return Err(XPathError::IncorrectNumberOfArgument);
    }
    let name = context
        .pop_object()
        .ok_or(XPathError::IncorrectNumberOfArgument)?
        .as_string()?;
    let Some(node) = context.node.clone() else {
        return Ok("".into());
    };
    let doc = node.owner_document();
    Ok(doc
        .document_type()
        .and_then(|doctype| doctype.get_entity_decl(&name))
        .filter(|ent| ent.notation_name().is_some())
        .and_then(|ent| ent.system_id())
        .map(|uri| uri.to_string())
        .unwrap_or_default()
        .into())
}

/// "generate-id" function.
///
/// # Reference
/// - [Function: string generate-id(node-set?)](https://www.w3.org/TR/xslt-10/#function-generate-id)
fn generate_id(context: &mut XPathContext, num_args: usize) -> Result<XPathObject, XPathError> {
    let node = match num_args {
        0 => context.node.clone(),
        1 => context
            .pop_object()
            .ok_or(XPathError::IncorrectNumberOfArgument)?
            .as_nodeset()?
            .first()
            .cloned(),
        _ => return Err(XPathError::IncorrectNumberOfArgument),
    };
    let Some(node) = node else {
        return Ok("".into());
    };
    Ok(format!("id{}", node.generate_id()).into())
}

/// "system-property" function.
///
/// # Reference
/// - [Function: object system-property(string)](https://www.w3.org/TR/xslt-10/#function-system-property)
fn system_property(context: &mut XPathContext, num_args: usize) -> Result<XPathObject, XPathError> {
    if num_args != 1 {
        return Err(XPathError::IncorrectNumberOfArgument);
    }

    let qname = context
        .pop_object()
        .ok_or(XPathError::IncorrectNumberOfArgument)?
        .as_string()?;
    if let Some((pre, loc)) = qname.split_once(':') {
        let Some(namespace) = context.namespaces.get(Some(pre)) else {
            return Ok("".into());
        };

        if namespace == XML_XSLT_NAMESPACE {
            // Since there is no vendor name or vendor URL, I'll return the
            // crate name and the crates.io URL that manages the crate for now.
            match loc {
                "version" => Ok("1.0".into()),
                "vendor" => Ok("anyxml".into()),
                "vendor-url" => Ok("https://crates.io/crates/anyxml/".into()),
                _ => Ok("".into()),
            }
        } else {
            Ok("".into())
        }
    } else {
        Ok("".into())
    }
}

/// "element-available" function.
///
/// # Reference
/// - [Function: boolean element-available(string)](https://www.w3.org/TR/xslt-10/#function-element-available)
fn element_available(
    context: &mut XPathContext,
    num_args: usize,
) -> Result<XPathObject, XPathError> {
    if num_args != 1 {
        return Err(XPathError::IncorrectNumberOfArgument);
    }

    let qname = context
        .pop_object()
        .ok_or(XPathError::IncorrectNumberOfArgument)?
        .as_string()?;
    let Some((pre, loc)) = qname.split_once(':') else {
        return Ok(false.into());
    };
    let Some(namespace) = context.namespaces.get(Some(pre)) else {
        return Ok(false.into());
    };

    if namespace == XML_XSLT_NAMESPACE {
        Ok(matches!(
            loc,
            "apply-imports"
                | "apply-templates"
                | "attribute"
                | "attribute-set"
                | "call-template"
                | "choose"
                | "comment"
                | "copy"
                | "copy-of"
                | "decimal-format"
                | "element"
                | "fallback"
                | "for-each"
                | "if"
                | "import"
                | "include"
                | "key"
                | "message"
                | "namespace-alias"
                | "number"
                | "otherwise"
                | "output"
                | "param"
                | "preserve-space"
                | "processing-instruction"
                | "sort"
                | "strip-space"
                | "stylesheet"
                | "template"
                | "text"
                | "transform"
                | "value-of"
                | "variable"
                | "when"
                | "with-param"
        )
        .into())
    } else {
        Ok(false.into())
    }
}

/// "function-available" function.
///
/// # Reference
/// - [Function: boolean function-available(string)](https://www.w3.org/TR/xslt-10/#function-function-available)
fn function_available(
    context: &mut XPathContext,
    num_args: usize,
) -> Result<XPathObject, XPathError> {
    if num_args != 1 {
        return Err(XPathError::IncorrectNumberOfArgument);
    }

    let qname = context
        .pop_object()
        .ok_or(XPathError::IncorrectNumberOfArgument)?
        .as_string()?;
    if qname.contains(':') {
        return Ok(false.into());
    }

    Ok(context.functions.get(&qname).is_ok().into())
}

/// Compile `pattern` as a XSLT pattern.
///
/// This function compiles a pattern according to the XSLT 1.0 specification and, if successful,
/// returns the compiled pattern. Since patterns are a subset of XPath expressions, the return
/// value is an [`XPathExpression`].
///
/// # Reference
/// - [5.2 Patterns](https://www.w3.org/TR/xslt-10/#patterns)
pub fn compile_pattern(mut pattern: &str) -> Result<XPathExpression, XPathCompileError> {
    let mut tree = vec![];
    let root = parse_pattern(&mut pattern, &mut tree)?;
    skip_whitespaces(&mut pattern);
    if !pattern.is_empty() {
        return Err(XPathCompileError::ExpressionNotTerminated);
    }

    let mut context = XPathContext::default();
    register_xslt_functions(&mut context.functions);
    Ok(XPathExpression {
        root,
        tree,
        context,
    })
}

/// [Pattern](https://www.w3.org/TR/xslt-10/#NT-Pattern)
fn parse_pattern(
    pattern: &mut &str,
    tree: &mut Vec<XPathSyntaxTree>,
) -> Result<usize, XPathCompileError> {
    let mut left = parse_location_path_pattern(pattern, tree)?;
    skip_whitespaces(pattern);
    while let Some(pat) = pattern.strip_prefix('|') {
        *pattern = pat;
        let right = parse_location_path_pattern(pattern, tree)?;
        let new = XPathSyntaxTree::Union(left, right);
        left = tree.len();
        tree.push(new);
        skip_whitespaces(pattern);
    }
    Ok(left)
}

/// [LocationPathPattern](https://www.w3.org/TR/xslt-10/#NT-LocationPathPattern)
fn parse_location_path_pattern(
    pattern: &mut &str,
    tree: &mut Vec<XPathSyntaxTree>,
) -> Result<usize, XPathCompileError> {
    skip_whitespaces(pattern);
    if pattern.starts_with('/') {
        parse_absolute_location_path_pattern(pattern, tree)
    } else if pattern.starts_with("id") || pattern.starts_with("key") {
        parse_id_key_pattern(pattern, tree)
    } else {
        parse_relative_path_pattern(pattern, tree, true)
    }
}
fn parse_absolute_location_path_pattern(
    pattern: &mut &str,
    tree: &mut Vec<XPathSyntaxTree>,
) -> Result<usize, XPathCompileError> {
    skip_whitespaces(pattern);
    *pattern = pattern
        .strip_prefix('/')
        .ok_or(XPathCompileError::InvalidAbsoluteLocationPath)?;
    let root = tree.len();
    tree.push(XPathSyntaxTree::LocationPathRoot);

    if let Some(rem) = pattern.strip_prefix('/') {
        // '//' is equal to '/descendant-or-self::node()'

        *pattern = rem;

        let right = tree.len();
        tree.push(XPathSyntaxTree::Step {
            first: false,
            axis: Axis::DescendantOrSelf,
            node_test: Arc::new(NodeTest::Node),
            predicate: usize::MAX,
        });

        let left = tree.len();
        tree.push(XPathSyntaxTree::Slash(root, right));

        let right = parse_relative_path_pattern(pattern, tree, false)?;
        let new = XPathSyntaxTree::Slash(left, right);
        let ret = tree.len();
        tree.push(new);
        return Ok(ret);
    }

    // RelativeLocationPath may not appear, so we need to check if RelativeLocationPath starts.
    // RelativeLocationPath starts with Step,
    // and Step starts with AxisSpecifier, NodeTest or AbbreviatedStep ('.', '..').

    skip_whitespaces(pattern);
    if pattern.starts_with(|c: char| {
        // '*'              : NameTest
        // '@'              : AbbreviatedAxisSpecifier
        // NameStartChar    : NameTest, AxisName
        matches!(c, '*' | '@') || (c != ':' && XMLVersion::default().is_name_start_char(c))
    }) {
        let right = parse_relative_path_pattern(pattern, tree, false)?;
        let new = XPathSyntaxTree::Slash(root, right);
        let ret = tree.len();
        tree.push(new);
        return Ok(ret);
    }

    Ok(root)
}

/// [IdKeyPattern](https://www.w3.org/TR/xslt-10/#NT-IdKeyPattern)
fn parse_id_key_pattern(
    pattern: &mut &str,
    tree: &mut Vec<XPathSyntaxTree>,
) -> Result<usize, XPathCompileError> {
    skip_whitespaces(pattern);
    let node = if let Some(pat) = pattern.strip_prefix("id") {
        *pattern = pat;
        skip_whitespaces(pattern);
        let Some(pat) = pattern.strip_prefix('(') else {
            return Err(XPathCompileError::InvalidFunctionCall);
        };
        *pattern = pat;
        let arg = parse_literal(pattern, tree)?;
        skip_whitespaces(pattern);
        let Some(pat) = pattern.strip_prefix(')') else {
            return Err(XPathCompileError::InvalidFunctionCall);
        };
        *pattern = pat;
        XPathSyntaxTree::FunctionCall {
            name: "id".into(),
            arguments: vec![arg],
        }
    } else if let Some(pat) = pattern.strip_prefix("key") {
        *pattern = pat;
        skip_whitespaces(pattern);
        let Some(pat) = pattern.strip_prefix('(') else {
            return Err(XPathCompileError::InvalidFunctionCall);
        };
        *pattern = pat;
        let left = parse_literal(pattern, tree)?;
        skip_whitespaces(pattern);
        let Some(pat) = pattern.strip_prefix(',') else {
            return Err(XPathCompileError::InvalidFunctionCall);
        };
        *pattern = pat;
        let right = parse_literal(pattern, tree)?;
        skip_whitespaces(pattern);
        let Some(pat) = pattern.strip_prefix(')') else {
            return Err(XPathCompileError::InvalidFunctionCall);
        };
        *pattern = pat;
        XPathSyntaxTree::FunctionCall {
            name: "key".into(),
            arguments: vec![left, right],
        }
    } else {
        return Err(XPathCompileError::InvalidFunctionName);
    };

    let mut left = tree.len();
    tree.push(node);

    let node = if let Some(pat) = pattern.strip_prefix("//") {
        // '//' is equal to '/descendant-or-self::node()'

        *pattern = pat;
        let right = tree.len();
        tree.push(XPathSyntaxTree::Step {
            first: false,
            axis: Axis::DescendantOrSelf,
            node_test: Arc::new(NodeTest::Node),
            predicate: usize::MAX,
        });
        let new = XPathSyntaxTree::Slash(left, right);
        left = tree.len();
        tree.push(new);
        let right = parse_step_pattern(pattern, tree, false)?;
        XPathSyntaxTree::Slash(left, right)
    } else if let Some(pat) = pattern.strip_prefix('/') {
        *pattern = pat;
        let right = parse_step_pattern(pattern, tree, false)?;
        XPathSyntaxTree::Slash(left, right)
    } else {
        return Ok(left);
    };

    let ret = tree.len();
    tree.push(node);
    Ok(ret)
}

/// [RelativePathPattern](https://www.w3.org/TR/xslt-10/#NT-RelativePathPattern)
fn parse_relative_path_pattern(
    pattern: &mut &str,
    tree: &mut Vec<XPathSyntaxTree>,
    is_first_step: bool,
) -> Result<usize, XPathCompileError> {
    let mut left = parse_step_pattern(pattern, tree, is_first_step)?;
    skip_whitespaces(pattern);
    while let Some(rem) = pattern.strip_prefix('/') {
        let new = if let Some(rem) = rem.strip_prefix('/') {
            // '//' is equal to '/descendant-or-self::node()'

            *pattern = rem;
            let right = tree.len();
            tree.push(XPathSyntaxTree::Step {
                first: false,
                axis: Axis::DescendantOrSelf,
                node_test: Arc::new(NodeTest::Node),
                predicate: usize::MAX,
            });
            let new = XPathSyntaxTree::Slash(left, right);
            left = tree.len();
            tree.push(new);
            let right = parse_step_pattern(pattern, tree, false)?;
            XPathSyntaxTree::Slash(left, right)
        } else {
            *pattern = rem;
            let right = parse_step_pattern(pattern, tree, false)?;
            XPathSyntaxTree::Slash(left, right)
        };
        left = tree.len();
        tree.push(new);
        skip_whitespaces(pattern);
    }
    Ok(left)
}

/// [StepPattern](https://www.w3.org/TR/xslt-10/#NT-StepPattern)
fn parse_step_pattern(
    pattern: &mut &str,
    tree: &mut Vec<XPathSyntaxTree>,
    is_first_step: bool,
) -> Result<usize, XPathCompileError> {
    skip_whitespaces(pattern);
    if let Some(rem) = pattern.strip_prefix('.') {
        // AbbreviatedStep

        let ret = tree.len();
        if let Some(rem) = rem.strip_prefix('.') {
            *pattern = rem;
            tree.push(XPathSyntaxTree::Step {
                first: is_first_step,
                axis: Axis::Parent,
                node_test: Arc::new(NodeTest::Node),
                predicate: usize::MAX,
            });
        } else {
            *pattern = rem;
            tree.push(XPathSyntaxTree::Step {
                first: is_first_step,
                axis: Axis::SelfNode,
                node_test: Arc::new(NodeTest::Node),
                predicate: usize::MAX,
            });
        };
        return Ok(ret);
    }

    // Step

    let axis = parse_child_or_attribute_axis_specifier(pattern)?;
    let node_test = parse_node_test(pattern, tree)?;

    let mut prev = tree.len();
    let ret = prev;
    tree.push(XPathSyntaxTree::Step {
        first: is_first_step,
        axis,
        node_test: Arc::new(node_test),
        predicate: usize::MAX,
    });

    skip_whitespaces(pattern);
    while pattern.starts_with('[') {
        let expression = parse_predicate(pattern, tree)?;
        let new = XPathSyntaxTree::Predicate {
            expression,
            next: usize::MAX,
        };
        let nt = tree.len();
        match &mut tree[prev] {
            XPathSyntaxTree::Step { predicate, .. } => *predicate = nt,
            XPathSyntaxTree::Predicate { next, .. } => *next = nt,
            _ => {}
        }
        prev = nt;
        tree.push(new);
        skip_whitespaces(pattern);
    }
    Ok(ret)
}

/// [ChildOrAttributeAxisSpecifier](https://www.w3.org/TR/xslt-10/#NT-ChildOrAttributeAxisSpecifier)
fn parse_child_or_attribute_axis_specifier(pattern: &mut &str) -> Result<Axis, XPathCompileError> {
    skip_whitespaces(pattern);
    let (mut rem, axis) = if let Some(rem) = pattern.strip_prefix('@') {
        *pattern = rem;
        return Ok(Axis::Attribute);
    } else if let Some(rem) = pattern.strip_prefix("attribute") {
        (rem, Axis::Attribute)
    } else if let Some(rem) = pattern.strip_prefix("child") {
        (rem, Axis::Child)
    } else {
        return Ok(Axis::Child);
    };

    skip_whitespaces(&mut rem);
    if let Some(rem) = rem.strip_prefix("::") {
        // Only when `::` follows an AxisName is it considered a match
        // for that specific AxisName.
        *pattern = rem;
        Ok(axis)
    } else {
        // If it is not a specific AxisName, `child` is considered omitted
        // (i.e., it is considered to match an AbbreviatedAxisSpecifier).
        Ok(Axis::Child)
    }
}

const SYMBOL_QUOTE: char = '\'';

struct FormatNumberInfo {
    integer_hash: i32,         // Number of '#' in integer part
    integer_digits: i32,       // Number of '0' in integer part
    frac_digits: i32,          // Number of '0' in fractional part
    frac_hash: i32,            // Number of '#' in fractional part
    group: i32,                // Number of chars per display 'group'
    multiplier: i32,           // Scaling for percent or permille
    add_decimal: bool,         // Flag for whether decimal point appears in pattern
    is_multiplier_set: bool,   // Flag to catch multiple occurences of percent/permille
    is_negative_pattern: bool, // Flag for processing -ve prefix/suffix
}

#[derive(Clone)]
pub struct DecimalFormat {
    pub decimal_separator: char,
    pub grouping_separator: char,
    pub infinity: Option<String>,
    pub minus_sign: char,
    pub nan: Option<String>,
    pub percent: char,
    pub permille: char,
    pub zero_digit: char,
    pub digit: char,
    pub pattern_separator: char,
}

impl DecimalFormat {
    fn is_special(&self, letter: char) -> bool {
        self.zero_digit == letter
            || self.digit == letter
            || self.decimal_separator == letter
            || self.grouping_separator == letter
            || self.pattern_separator == letter
    }
}

impl Default for DecimalFormat {
    fn default() -> Self {
        Self {
            decimal_separator: '.',
            grouping_separator: ',',
            infinity: Some("Infinity".into()),
            minus_sign: '-',
            nan: Some("NaN".into()),
            percent: '%',
            permille: '\u{2030}',
            zero_digit: '0',
            digit: '#',
            pattern_separator: ';',
        }
    }
}

fn number_format_decimal(
    buffer: &mut Vec<char>,
    mut number: f64,
    digit_zero: char,
    width: usize,
    digits_per_group: usize,
    grouping_character: char,
) {
    let mut temp_string = ['\0'; 500];
    let mut pointer = temp_string.len();
    let mut i = 0i32;
    while pointer > 0 {
        if i as usize >= width && number.abs() < 1.0 {
            break;
        }
        if i > 0
            && grouping_character != '\0'
            && digits_per_group > 0
            && i as usize % digits_per_group == 0
        {
            if pointer == 0 {
                i = -1;
                break;
            }
            pointer -= 1;
            temp_string[pointer] = grouping_character;
        }

        if pointer == 0 {
            i = -1;
            break;
        }
        let val = (digit_zero as u32) + (number % 10.0) as u32;
        let Some(val) = char::from_u32(val) else {
            i = -1;
            break;
        };
        pointer -= 1;
        temp_string[pointer] = val;
        number /= 10.0;
        i += 1;
    }
    if i < 0 {
        todo!("raise error");
    }
    buffer.extend(&temp_string[pointer..]);
}

fn format_number_pre_suffix(
    slf: &DecimalFormat,
    format: &mut &str,
    info: &mut FormatNumberInfo,
) -> Result<usize, XPathError> {
    let mut count = 0;

    loop {
        if format.is_empty() {
            break Ok(count);
        }
        if format.starts_with(SYMBOL_QUOTE) {
            *format = &format[SYMBOL_QUOTE.len_utf8()..];
            if format.is_empty() {
                break Err(XPathError::WrongNumberFormat);
            }
        } else if slf.is_special(format.chars().next().unwrap()) {
            break Ok(count);
        } else if format.starts_with(slf.percent) {
            if info.is_multiplier_set {
                break Err(XPathError::WrongNumberFormat);
            }
            info.multiplier = 100;
            info.is_multiplier_set = true;
        } else if format.starts_with(slf.permille) {
            if info.is_multiplier_set {
                break Err(XPathError::WrongNumberFormat);
            }
            info.multiplier = 1000;
            info.is_multiplier_set = true;
        }

        let c = format.chars().next().unwrap();
        let len = c.len_utf8();
        *format = &format[len..];
        count += len;
    }
}

fn format_number_conversion(
    slf: &DecimalFormat,
    format: &str,
    mut number: f64,
    result: &mut String,
) -> Result<(), XPathError> {
    let mut delayed_multiplier = 0;

    if format.is_empty() {
        return Err(XPathError::WrongNumberFormat);
    }
    result.clear();
    if number.is_nan() {
        if let Some(no_number) = slf.nan.as_deref() {
            *result = no_number.to_owned();
        } else {
            *result = "NaN".into();
        }
        return Ok(());
    }

    let mut format_info = FormatNumberInfo {
        integer_hash: 0,
        integer_digits: 0,
        frac_digits: 0,
        frac_hash: 0,
        group: -1,
        multiplier: 1,
        add_decimal: false,
        is_multiplier_set: false,
        is_negative_pattern: false,
    };

    let mut the_format = format;

    let mut prefix = the_format;
    let mut prefix_length = format_number_pre_suffix(slf, &mut the_format, &mut format_info)?;

    while !the_format.is_empty()
        && !the_format.starts_with(slf.decimal_separator)
        && !the_format.starts_with(slf.pattern_separator)
    {
        if delayed_multiplier != 0 {
            format_info.multiplier = delayed_multiplier;
            format_info.is_multiplier_set = true;
            delayed_multiplier = 0;
        }
        if the_format.starts_with(slf.digit) {
            if format_info.integer_digits > 0 {
                return Err(XPathError::WrongNumberFormat);
            }
            format_info.integer_hash += 1;
            if format_info.group >= 0 {
                format_info.group += 1;
            }
        } else if the_format.starts_with(slf.zero_digit) {
            format_info.integer_digits += 1;
            if format_info.group >= 0 {
                format_info.group += 1;
            }
        } else if the_format.starts_with(slf.grouping_separator) {
            format_info.group = 0;
            the_format = &the_format[slf.grouping_separator.len_utf8()..];
            continue;
        } else if the_format.starts_with(slf.percent) {
            if format_info.is_multiplier_set {
                return Err(XPathError::WrongNumberFormat);
            }
            delayed_multiplier = 100;
        } else if the_format.starts_with(slf.permille) {
            if format_info.is_multiplier_set {
                return Err(XPathError::WrongNumberFormat);
            }
            delayed_multiplier = 1000;
        } else {
            break;
        }
        let c = the_format.chars().next().unwrap();
        let len = c.len_utf8();
        the_format = &the_format[len..];
    }

    if !the_format.is_empty() && the_format.starts_with(slf.decimal_separator) {
        format_info.add_decimal = true;
        the_format = &the_format[slf.decimal_separator.len_utf8()..];
    }

    while !the_format.is_empty() {
        if the_format.starts_with(slf.zero_digit) {
            if format_info.frac_hash != 0 {
                return Err(XPathError::WrongNumberFormat);
            }
            format_info.frac_digits += 1;
        } else if the_format.starts_with(slf.digit) {
            format_info.frac_hash += 1;
        } else if the_format.starts_with(slf.percent) {
            if format_info.is_multiplier_set {
                return Err(XPathError::WrongNumberFormat);
            }
            delayed_multiplier = 100;
            the_format = &the_format[slf.percent.len_utf8()..];
            continue;
        } else if the_format.starts_with(slf.permille) {
            if format_info.is_multiplier_set {
                return Err(XPathError::WrongNumberFormat);
            }
            delayed_multiplier = 1000;
            the_format = &the_format[slf.permille.len_utf8()..];
            continue;
        } else if !the_format.starts_with(slf.grouping_separator) {
            break;
        }
        let c = the_format.chars().next().unwrap();
        the_format = &the_format[c.len_utf8()..];
        if delayed_multiplier != 0 {
            format_info.multiplier = delayed_multiplier;
            delayed_multiplier = 0;
            format_info.is_multiplier_set = true;
        }
    }

    if delayed_multiplier != 0 {
        let pre = &format[..format.len() - the_format.len()];
        let back = pre.chars().next_back().unwrap();
        the_format = &pre[pre.len() - back.len_utf8()..];
        delayed_multiplier = 0;
    }

    let mut suffix = the_format;
    let mut suffix_length = format_number_pre_suffix(slf, &mut the_format, &mut format_info)?;
    if !the_format.is_empty() && !the_format.starts_with(slf.pattern_separator) {
        return Err(XPathError::WrongNumberFormat);
    }

    let mut default_sign = number < 0.0;
    if default_sign && let Some((_, fmt)) = format.split_once(slf.pattern_separator) {
        the_format = fmt;
        format_info.is_negative_pattern = true;
        format_info.is_multiplier_set = false;

        let nprefix = the_format;
        let nprefix_length = format_number_pre_suffix(slf, &mut the_format, &mut format_info)?;
        while !the_format.is_empty() {
            if the_format.starts_with(slf.percent) || the_format.starts_with(slf.permille) {
                if format_info.is_multiplier_set {
                    return Err(XPathError::WrongNumberFormat);
                }
                format_info.is_multiplier_set = true;
                delayed_multiplier = 1;
            } else if slf.is_special(the_format.chars().next().unwrap()) {
                delayed_multiplier = 0;
            } else {
                break;
            }
            let c = the_format.chars().next().unwrap();
            the_format = &the_format[c.len_utf8()..];
        }
        if delayed_multiplier != 0 {
            format_info.is_multiplier_set = false;
            let pre = &format[..format.len() - the_format.len()];
            let back = pre.chars().next_back().unwrap();
            the_format = &pre[pre.len() - back.len_utf8()..];
        }

        let (nsuffix, nsuffix_length) = if !the_format.is_empty() {
            let nsuffix = the_format;
            let len = format_number_pre_suffix(slf, &mut the_format, &mut format_info)?;
            (&nsuffix[..len], len)
        } else {
            ("", 0)
        };
        if !the_format.is_empty() {
            return Err(XPathError::WrongNumberFormat);
        }
        if (nprefix_length != prefix_length)
            || (nsuffix_length != suffix_length)
            || (nprefix_length > 0 && nprefix[..prefix_length] != prefix[..prefix_length])
            || (nsuffix_length > 0 && nsuffix[..suffix_length] != suffix[..suffix_length])
        {
            prefix = nprefix;
            prefix_length = nprefix_length;
            suffix = nsuffix;
            suffix_length = nsuffix_length;
        }
        default_sign = prefix_length == 0;
    }

    number *= format_info.multiplier as f64;
    if number.is_infinite() {
        if number.is_sign_negative() {
            result.push(slf.minus_sign);
            if let Some(inf) = slf.infinity.as_deref() {
                result.push_str(inf);
            } else {
                result.push_str("Infinity");
            }
        } else if let Some(inf) = slf.infinity.as_deref() {
            result.push_str(inf);
        } else {
            result.push_str("Infinity");
        }
        return Ok(());
    }

    let mut buffer = vec![];
    if default_sign {
        buffer.push(slf.minus_sign);
    }

    buffer.extend(
        prefix[..prefix_length as usize]
            .chars()
            .filter(|&c| c != SYMBOL_QUOTE),
    );

    number = number.abs();
    let mut exp10 = format_info.frac_digits + format_info.frac_hash;
    if exp10 > f64::MAX_10_EXP {
        format_info.frac_digits = format_info.frac_digits.min(f64::MAX_10_EXP);
        format_info.frac_hash = f64::MAX_10_EXP - format_info.frac_digits;
        exp10 = f64::MAX_10_EXP;
    }
    let scale = 10.0f64.powf(exp10 as f64);
    number += 0.5 / scale;
    number -= number % (1.0 / scale);
    number_format_decimal(
        &mut buffer,
        number.floor(),
        slf.zero_digit,
        format_info.integer_digits as usize,
        format_info.group as usize,
        slf.grouping_separator,
    );

    // Special case: java treats '.#' like '.0', '.##' like '.0#', etc.
    if (format_info.integer_digits + format_info.integer_hash + format_info.frac_digits == 0)
        && (format_info.frac_hash > 0)
    {
        format_info.frac_digits += 1;
        format_info.frac_hash -= 1;
    }

    // Add leading zero, if required
    if number.floor() == 0.0 && format_info.integer_digits + format_info.frac_digits == 0 {
        buffer.push(slf.zero_digit);
    }

    // Next the fractional part, if required
    if format_info.frac_digits + format_info.frac_hash == 0 {
        if format_info.add_decimal {
            buffer.push(slf.decimal_separator);
        }
    } else {
        number -= number.floor();
        if number != 0.0 || format_info.frac_digits != 0 {
            buffer.push(slf.decimal_separator);
            number = (scale * number + 0.5).floor();
            let mut j = format_info.frac_hash;
            while j > 0 {
                if number % 10.0 >= 1.0 {
                    break;
                }
                number /= 10.0;
                j -= 1;
            }
            number_format_decimal(
                &mut buffer,
                number.floor(),
                slf.zero_digit,
                format_info.frac_digits as usize + j as usize,
                0,
                '\0',
            );
        }
    }
    buffer.extend(
        suffix[..suffix_length as usize]
            .chars()
            .filter(|&c| c != SYMBOL_QUOTE),
    );

    result.extend(buffer);
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::tree::NodeType;

    use super::*;

    #[test]
    fn current_function_tests() {
        const XML: &str = r#"<doc><child>text</child></doc>"#;

        let mut reader = XMLReader::builder()
            .set_handler(TreeBuildHandler::default())
            .build();
        reader.parse_str(XML, None).unwrap();
        let doc = reader.handler.document;

        let mut exp = crate::xpath::compile("current()").unwrap();
        let ret = exp.evaluate(doc.clone()).unwrap().as_nodeset().unwrap();

        assert_eq!(ret.len(), 1);
        let node = ret[0].clone();
        assert_eq!(node.node_type(), NodeType::Document);
        let node = node.as_document().unwrap();
        assert!(doc.is_same_node(node));
    }

    #[test]
    fn document_function_tests() {
        const BASE_URI: &str =
            concat!(env!("CARGO_MANIFEST_DIR"), "/resources/xpath/testsuite.xml");
        let uri = URIString::parse_file_path(BASE_URI).unwrap();

        let mut reader = XMLReader::builder()
            .set_handler(TreeBuildHandler::default())
            .build();
        reader.parse_uri(uri, None).unwrap();
        let doc = reader.handler.document;

        let mut exp = crate::xpath::compile("document('testsuite.xml',.)").unwrap();
        let ret = exp.evaluate(doc.clone()).unwrap().as_nodeset().unwrap();

        assert_eq!(ret.len(), 1);
        let node = ret[0].clone();
        assert_eq!(node.node_type(), NodeType::Document);
        let node = node.as_document().unwrap();
        assert!(doc.is_same_node(node));

        let mut exp = crate::xpath::compile("document('testsuite.xml',.)/*[1]").unwrap();
        let ret = exp.evaluate(doc.clone()).unwrap().as_nodeset().unwrap();

        assert_eq!(ret.len(), 1);
        let node = ret[0].clone();
        assert_eq!(node.node_type(), NodeType::Element);
        let node = node.as_element().unwrap();
        assert!(node.is_same_node(doc.document_element().unwrap()));
    }

    #[test]
    fn generate_id_function_tests() {
        const BASE_URI: &str =
            concat!(env!("CARGO_MANIFEST_DIR"), "/resources/xpath/testsuite.xml");
        let uri = URIString::parse_file_path(BASE_URI).unwrap();

        let mut reader = XMLReader::builder()
            .set_handler(TreeBuildHandler::default())
            .build();
        reader.parse_uri(uri, None).unwrap();
        let doc = reader.handler.document;

        let mut exp =
            crate::xpath::compile("generate-id(document('testsuite.xml',.))=generate-id(.)")
                .unwrap();
        assert!(exp.evaluate(doc.clone()).unwrap().as_boolean().unwrap());
    }

    #[test]
    fn format_number_function_tests() {
        let mut doc = Document::new();
        let elem = doc.create_element("dummy", None).unwrap();
        doc.append_child(elem).unwrap();

        macro_rules! execute {
            ($expr:literal, $( $field:ident : $value:expr ),*) => {{
                let mut exp = compile($expr).unwrap();
                #[allow(unused_mut)]
                let mut format = DecimalFormat::default();
                $( format.$field = $value; )*
                exp.xslt_context().fmts.insert((None, "".into()), format);
                exp.evaluate(doc.clone()).unwrap().as_string().unwrap()
            }};
            ($expr:literal) => {
                execute!($expr,)
            }
        }

        // numberformat1
        let ret = execute!("format-number(2392.14*36.58,'000,000.000000')");
        assert_eq!(&*ret, "087,504.481200");
        // numberformat2
        let ret = execute!("format-number(12792.14*96.58,'##,###,000.000###')");
        assert_eq!(&*ret, "1,235,464.8812");
        // numberformat3
        let ret = execute!("format-number(2792.14*(-36.58),'000,000.000###')");
        assert_eq!(&*ret, "-102,136.4812");
        // numberformat4
        let ret = execute!("format-number(2392.14*(-36.58),'###,###.000###;(###,###.000###)')");
        assert_eq!(&*ret, "(87,504.4812)");
        // numberformat5
        let ret = execute!("format-number(0.4857,'###.###%')");
        assert_eq!(&*ret, "48.57%");
        // numberformat6
        let ret = execute!("format-number(0.4857,'###.###\u{2030}')");
        assert_eq!(&*ret, "485.7\u{2030}");
        // numberformat7
        let ret = execute!("format-number(95.4857,'\u{A4}###.####')");
        assert_eq!(&*ret, "\u{A4}95.4857");
        // numberformat8
        let ret = execute!("format-number(2.14*86.58,'PREFIX##00.000###SUFFIX')");
        assert_eq!(&*ret, "PREFIX185.2812SUFFIX");
        // numberformat9
        let ret = execute!("format-number(931.4857,'000.000|###')", decimal_separator: '|', grouping_separator: '.');
        assert_eq!(&*ret, "000.931|486");
        // numberformat11
        let ret = execute!("format-number(26931.4,'+!!!,!!!.!!!\\-!!,!!!.!!!')", digit: '!', pattern_separator: '\\');
        assert_eq!(&*ret, "+26,931.4");
        // numberformat12
        let ret = execute!("format-number(-26931.4,'+!!,!!!.!!!\\-!!!,!!!.!!!')", digit: '!', pattern_separator: '\\');
        assert_eq!(&*ret, "-26,931.4");
        // numberformat13
        let ret =
            execute!("format-number(-26931.4,'!!!,!!!.!!!')", digit: '!', pattern_separator: '\\');
        assert_eq!(&*ret, "-26,931.4");
        // numberformat14
        let ret = execute!("format-number(1 div 0,'###############################')", infinity: Some("off-the-scale".into()));
        assert_eq!(&*ret, "off-the-scale");
        // numberformat15
        let ret = execute!("format-number('foo','#############')", nan: Some("non-numeric".into()));
        assert_eq!(&*ret, "non-numeric");
        // numberformat16
        let ret = execute!("format-number(0.4857,'###.###m')", permille: 'm');
        assert_eq!(&*ret, "485.7m");
        // numberformat17
        let ret = execute!("format-number(-26931.4,'+###,###.###;-###,###.###')", minus_sign: '_');
        assert_eq!(&*ret, "-26,931.4");
        // numberformat18
        let ret = execute!("format-number(-26931.4,'###,###.###')", minus_sign: '_');
        assert_eq!(&*ret, "_26,931.4");
        // numberformat19
        let ret = execute!("format-number(-26931.4,'###,###.###')", minus_sign: '_');
        assert_eq!(&*ret, "_26,931.4");
        let ret = execute!("format-number(-42857.1,'###,###.###')");
        assert_eq!(&*ret, "-42,857.1");
        // numberformat20
        let ret = execute!("format-number(1234.567,'#*###*###!###')", decimal_separator: '!', grouping_separator: '*');
        assert_eq!(&*ret, "1*234!567");
        // numberformat21
        let ret = execute!("format-number(-26931.4,'###.###,###')", decimal_separator: ',', grouping_separator: '.');
        assert_eq!(&*ret, "-26.931,4");
        let ret = execute!("format-number(-42857.1,'###,###.###')", minus_sign: '_');
        assert_eq!(&*ret, "_42,857.1");
        let ret = execute!("format-number(-73816.9,'###,###.###')");
        assert_eq!(&*ret, "-73,816.9");

        // The expected behavior of this test is questionable.
        // Neither the JDK specification nor the XSLT specification mentions how the system
        // should behave when symbols conflict. Since newer versions of the JDK state that
        // symbols must not conflict, it is unclear what the intended behavior is.
        // // numberformat25
        // let ret = execute!("format-number(931.4857,'###!###!###')", decimal_separator: '!', grouping_separator: '!');
        // assert_eq!(&*ret, "931");

        // numberformat26
        let ret = execute!("format-number(7654321.4857,'### ### ###,#####')", decimal_separator: ',', grouping_separator: ' ');
        assert_eq!(&*ret, "7 654 321,4857");
        // numberformat27
        let ret = execute!("format-number(2392.14*36.58,'000,000.000000')", minus_sign: '_');
        assert_eq!(&*ret, "087,504.481200");
        let ret = execute!("format-number(2392.14*36.58,'000,000.000000;###,###.000###')");
        assert_eq!(&*ret, "087,504.481200");
        let ret = execute!("format-number(2392.14*36.58,'000,000.000000;###,###.000###')", minus_sign: '_');
        assert_eq!(&*ret, "087,504.481200");
        // numberformat28
        let ret = execute!("format-number(2392.14*(-36.58),'+###,###.000###;-###,###.000###')");
        assert_eq!(&*ret, "-87,504.4812");
        // numberformat29
        let ret = execute!("format-number(-26931.4,'+###,###.###;_###,###.###')");
        assert_eq!(&*ret, "_26,931.4");
        // numberformat30
        let ret = execute!("format-number(-26931.4,'-###,###.###')");
        assert_eq!(&*ret, "--26,931.4");
        let ret = execute!("format-number(-26931.4,'-###,###.###')", minus_sign: '_');
        assert_eq!(&*ret, "_-26,931.4");
        let ret = execute!("format-number(-26931.4,'_###,###.###')", minus_sign: '_');
        assert_eq!(&*ret, "__26,931.4");
        // numberformat31
        let ret = execute!("format-number(-26931.4,'###,###.###;###,###.###')", minus_sign: '_');
        assert_eq!(&*ret, "_26,931.4");
        // numberformat32
        let ret = execute!("format-number(0.4857,'###.###c')", percent: 'c');
        assert_eq!(&*ret, "48.57c");
        // numberformat34
        let ret = execute!("format-number(4030201.0506,'#!!!,!!!,aaa.aaaaaa0')", digit: '!', zero_digit: 'a');
        assert_eq!(&*ret, "#e,ada,cab.afagaa0");
        // numberformat35
        let ret = execute!("format-number(987654321,'###,##0,00.00')");
        assert_eq!(&*ret, "9,87,65,43,21.00");
        // numberformat36
        let ret = execute!("format-number(239236.588,'00000.00')");
        assert_eq!(&*ret, "239236.59");
        // numberformat37
        let ret = execute!("format-number(1 div 0,'###############################')");
        assert_eq!(&*ret, "Infinity");
        // numberformat38
        let ret = execute!("format-number('foo','#############')");
        assert_eq!(&*ret, "NaN");
        // numberformat39
        let ret = execute!("format-number(-1 div 0,'###############################')");
        assert_eq!(&*ret, "-Infinity");
        // numberformat40
        let ret = execute!("format-number(-1 div 0,'###############################')", infinity: Some("huge".into()));
        assert_eq!(&*ret, "-huge");
        // numberformat41
        let ret = execute!("format-number('NaN','###')", minus_sign: '-', nan: Some("not a number".into()));
        assert_eq!(&*ret, "not a number");
        let ret = execute!("format-number(-13.2,'###.0')", minus_sign: '-', nan: Some("not a number".into()));
        assert_eq!(&*ret, "-13.2");
        let ret = execute!("format-number('NaN','###')", nan: Some("not a number".into()), decimal_separator: '.');
        assert_eq!(&*ret, "not a number");
        let ret = execute!("format-number(-13.2,'###.0')", nan: Some("not a number".into()), decimal_separator: '.');
        assert_eq!(&*ret, "-13.2");
        // numberformat42
        let ret = execute!("format-number('NaN','###')", nan: Some("not a number".into()), decimal_separator: '.');
        assert_eq!(&*ret, "not a number");
        let ret = execute!("format-number(3.2,'###.0')", nan: Some("not a number".into()), decimal_separator: '.');
        assert_eq!(&*ret, "3.2");
        // numberformat43
        let ret = execute!("format-number('NaN','###')", nan: Some("not a number".into()), decimal_separator: '.');
        assert_eq!(&*ret, "not a number");
        let ret = execute!("format-number(-13.2,'###.0')", nan: Some("not a number".into()), decimal_separator: '.');
        assert_eq!(&*ret, "-13.2");
        // numberformat44
        let ret =
            execute!("format-number('NaN','###')", digit: '#', nan: Some("not a number".into()));
        assert_eq!(&*ret, "not a number");
        let ret =
            execute!("format-number(-13.2,'###.0')", digit: '#', nan: Some("not a number".into()));
        assert_eq!(&*ret, "-13.2");
        // numberformat45
        let ret = execute!("format-number(12345.67,'000.000,###')", decimal_separator: ',', grouping_separator: '.');
        assert_eq!(&*ret, "012.345,67");
        let ret = execute!("format-number(-98765.4321,'##0.000,000')", decimal_separator: ',', grouping_separator: '.');
        assert_eq!(&*ret, "-98.765,432");
        // numberformat46
        let ret = execute!("format-number(12345.67,'000.000,###')", decimal_separator: ',', grouping_separator: '.');
        assert_eq!(&*ret, "012.345,67");
        let ret = execute!("format-number(-98765.4321,'##0.000,000')", decimal_separator: ',', grouping_separator: '.');
        assert_eq!(&*ret, "-98.765,432");
    }

    #[test]
    fn parse_pattern_tests() {
        // [5.2 Patterns](https://www.w3.org/TR/xslt-10/#patterns)
        compile_pattern("para").unwrap();
        compile_pattern("*").unwrap();
        compile_pattern("chapter|appendix").unwrap();
        compile_pattern("olist/item").unwrap();
        compile_pattern("appendix//para").unwrap();
        compile_pattern("/").unwrap();
        compile_pattern("text()").unwrap();
        compile_pattern("processing-instruction()").unwrap();
        compile_pattern("node()").unwrap();
        compile_pattern("id(\"W11\")").unwrap();
        compile_pattern("para[1]").unwrap();
        compile_pattern("*[position()=1 and self::para]").unwrap();
        compile_pattern("para[last()=1]").unwrap();
        compile_pattern("items/item[position()>1]").unwrap();
        compile_pattern("item[position() mod 2 = 1]").unwrap();
        compile_pattern("div[@class=\"appendix\"]//p").unwrap();
        compile_pattern("@class").unwrap();
        compile_pattern("@*").unwrap();
    }
}
