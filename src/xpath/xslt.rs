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
    // // 12.3 Number Formatting
    // library.register_function("format-number", format_number);
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
        let namespace = Cow::Borrowed(
            context
                .namespaces
                .get(Some(pre))
                .ok_or(XPathError::InvalidNamespacePrefix)?,
        );
        (Some(namespace), loc)
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

// This is planed to be implemented on a future version.
// /// "format-number" function.
// ///
// /// # Reference
// /// - [Function: string format-number(number, string, string?)](https://www.w3.org/TR/xslt-10/#function-format-number)
// fn format_number(context: &mut XPathContext, num_args: usize) -> Result<XPathObject, XPathError> {
//     todo!()
// }

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
