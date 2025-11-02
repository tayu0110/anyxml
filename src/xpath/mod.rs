mod compile;
mod function;
mod ops;
mod step;

use std::{borrow::Cow, collections::HashMap, io::Read, sync::Arc};

pub use compile::*;

use crate::{
    XML_NS_NAMESPACE, XML_XML_NAMESPACE,
    error::XMLError,
    sax::parser::XMLReaderBuilder,
    tree::{
        Attribute, CDATASection, Comment, Document, Element, Node, NodeType, ProcessingInstruction,
        Text, TreeBuildHandler, convert::NodeKind, namespace::Namespace, node::NodeSpec,
    },
    uri::URIStr,
    xpath::{function::FunctionLibrary, step::location_step},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum XPathError {
    IncorrectOperandType,
    IncorrectNumberOfArgument,
    IncorrectArgumentType,
    WrongTypeConversion,
    UnresolvableFunctionName,
    UnresolvableVariableName,
    CompileError(XPathCompileError),
    InternalError,
}

impl From<XPathCompileError> for XPathError {
    fn from(value: XPathCompileError) -> Self {
        XPathError::CompileError(value)
    }
}

/// Evaluate `xpath` as an XPath expression with `document` as the initial context node.
///
/// If successfully executed, it returns an [`XPathObject`] representing the evaluation
/// result of the XPath expression.  \
/// If the compilation or evaluation of the XPath expression fails, it returns [`Err`].
pub fn evaluate(xpath: &str, document: Document) -> Result<XPathObject, XPathError> {
    compile(xpath)?.evaluate(document)
}

/// Parse `xml` as the XML document and evaluate `xpath` as an XPath expression.
///
/// If successfully executed, it returns an [`XPathObject`] representing the evaluation
/// result of the XPath expression.  \
/// If the compilation or evaluation of the XPath expression fails,
/// or if the XML document cannot be parsed, it returns [`Err`].
///
/// `uri` is treated as the base URI for the XML document, and it is recommended
/// to specify it in documents that may retrieve external resources.
pub fn evaluate_str(xpath: &str, xml: &str, uri: Option<&URIStr>) -> Result<XPathObject, XMLError> {
    let mut expression = compile(xpath)?;
    let mut reader = XMLReaderBuilder::new()
        .set_handler(TreeBuildHandler::default())
        .build();
    reader.parse_str(xml, uri)?;

    let document = reader.handler.document;
    Ok(expression.evaluate(document)?)
}

/// Parse the XML document where `uri` is the base URI of the source, and evaluate `xpath`
/// as an XPath expression.
///
/// If successfully executed, it returns an [`XPathObject`] representing the evaluation
/// result of the XPath expression.  \
/// If the compilation or evaluation of the XPath expression fails,
/// or if the XML document cannot be parsed, it returns [`Err`].
///
/// `encoding` allows you to optionally specify the preferred encoding method to use.
pub fn evaluate_uri(
    xpath: &str,
    uri: impl AsRef<URIStr>,
    encoding: Option<&str>,
) -> Result<XPathObject, XMLError> {
    let mut expression = compile(xpath)?;
    let mut reader = XMLReaderBuilder::new()
        .set_handler(TreeBuildHandler::default())
        .build();
    reader.parse_uri(uri, encoding)?;

    let document = reader.handler.document;
    Ok(expression.evaluate(document)?)
}

/// Parse the XML documents using `reader` as the source and evaluate `xpath` as an XPath expression.
///
/// If successfully executed, it returns an [`XPathObject`] representing the evaluation
/// result of the XPath expression.  \
/// If the compilation or evaluation of the XPath expression fails,
/// or if the XML document cannot be parsed, it returns [`Err`].
///
/// `encoding` allows you to optionally specify the preferred encoding method to use.
///
/// `uri` is treated as the base URI for the XML document, and it is recommended
/// to specify it in documents that may retrieve external resources.
pub fn evaluate_reader<'a>(
    xpath: &str,
    reader: impl Read + 'a,
    encoding: Option<&str>,
    uri: Option<&URIStr>,
) -> Result<XPathObject, XMLError> {
    let mut expression = compile(xpath)?;
    let mut parser = XMLReaderBuilder::new()
        .set_handler(TreeBuildHandler::default())
        .build();
    parser.parse_reader(reader, encoding, uri)?;

    let document = parser.handler.document;
    Ok(expression.evaluate(document)?)
}

pub struct XPathExpression {
    root: usize,
    tree: Vec<XPathSyntaxTree>,
    context: XPathContext,
}

impl XPathExpression {
    /// Evaluate the precompiled XPath expression with `document` as the initial context node.
    ///
    /// If successfully executed, it returns an [`XPathObject`] representing the evaluation
    /// result of the XPath expression.  \
    /// If the evaluation of the XPath expression fails, it returns [`Err`].
    pub fn evaluate(&mut self, document: Document) -> Result<XPathObject, XPathError> {
        // clear context
        self.context.stack.clear();

        // initialize context
        self.context.node = Some(document.into());
        self.context.position = 1;
        self.context.size = 1;

        // start evaluation
        self.do_evaluate(self.root)?;
        assert_eq!(self.context.stack.len(), 1);
        self.context.stack.pop().ok_or(XPathError::InternalError)
    }

    fn do_evaluate(&mut self, op: usize) -> Result<(), XPathError> {
        match self.tree[op] {
            XPathSyntaxTree::Union(left, right) => {
                self.do_evaluate(left)?;
                self.do_evaluate(right)?;
                let right = self.context.stack.pop().unwrap();
                let left = self.context.stack.pop().unwrap();
                self.context.push_object(left.union(right)?);
            }
            XPathSyntaxTree::Slash(left, right) => {
                self.do_evaluate(left)?;
                self.do_evaluate(right)?;
            }
            XPathSyntaxTree::LocationPathRoot => {
                let mut node_set = XPathNodeSet::default();
                node_set.push(self.context.node.clone().unwrap().owner_document());
                self.context.push_object(node_set.into());
            }
            XPathSyntaxTree::Step {
                first,
                axis,
                ref node_test,
                predicate,
            } => {
                let current_node_set = if !first {
                    self.context
                        .stack
                        .pop()
                        .ok_or(XPathError::InternalError)?
                        .as_nodeset()
                        .map_err(|_| XPathError::IncorrectOperandType)?
                } else {
                    let mut node_set = XPathNodeSet::default();
                    node_set.push(self.context.node.clone().unwrap());
                    node_set
                };

                let node_test = node_test.clone();

                let old_context_node = self.context.node.take();
                let old_context_position = self.context.position;
                let old_context_size = self.context.size;

                self.context.size = current_node_set.len();
                let mut new_node_set = XPathNodeSet::default();
                for (i, node) in current_node_set.iter().enumerate() {
                    self.context.position = i + 1;
                    self.context.node = Some(node);
                    if predicate < usize::MAX {
                        let mut node_set = XPathNodeSet::default();
                        location_step(&self.context, axis, &node_test, &mut node_set);
                        if matches!(
                            axis,
                            Axis::Ancestor
                                | Axis::AncestorOrSelf
                                | Axis::Preceding
                                | Axis::PrecedingSibling
                        ) {
                            node_set.reverse_sort();
                        } else {
                            node_set.sort();
                        }
                        self.context.push_object(node_set.into());
                        self.do_evaluate(predicate)?;
                        let node_set = self.context.pop_object().unwrap().as_nodeset()?;
                        for node in &node_set {
                            new_node_set.push(node);
                        }
                    } else {
                        location_step(&self.context, axis, &node_test, &mut new_node_set);
                    }
                }

                self.context.node = old_context_node;
                self.context.position = old_context_position;
                self.context.size = old_context_size;

                if matches!(
                    axis,
                    Axis::Ancestor
                        | Axis::AncestorOrSelf
                        | Axis::Preceding
                        | Axis::PrecedingSibling
                ) {
                    new_node_set.reverse_sort();
                } else {
                    new_node_set.sort();
                }

                self.context.push_object(new_node_set.into());
            }
            XPathSyntaxTree::Predicate { expression, next } => {
                let XPathObject::NodeSet(node_set) = self.context.stack.pop().unwrap() else {
                    return Err(XPathError::IncorrectOperandType);
                };

                let old_context_node = self.context.node.take();
                let old_context_position = self.context.position;
                let old_context_size = self.context.size;
                self.context.size = node_set.len();
                let mut new = XPathNodeSet::default();
                let stack_depth = self.context.stack.len();
                for (i, node) in node_set.iter().enumerate() {
                    self.context.position = i + 1;
                    self.context.node = Some(node.clone());
                    self.do_evaluate(expression)?;
                    let ret = match self.context.stack.pop().unwrap() {
                        XPathObject::Number(number) => number == (i + 1) as f64,
                        object => object.as_boolean()?,
                    };
                    assert!(
                        stack_depth == self.context.stack.len(),
                        "stack_depth: {stack_depth}, context stack: {}",
                        self.context.stack.len(),
                    );
                    if ret {
                        new.push(node);
                    }
                }

                self.context.node = old_context_node;
                self.context.position = old_context_position;
                self.context.size = old_context_size;
                let is_empty = new.is_empty();
                self.context.push_object(new.into());

                if !is_empty && next < usize::MAX {
                    self.do_evaluate(next)?;
                }
            }
            XPathSyntaxTree::FilterExpr {
                expression,
                predicate,
            } => {
                self.do_evaluate(expression)?;
                let XPathObject::NodeSet(mut node_set) = self.context.stack.pop().unwrap() else {
                    return Err(XPathError::IncorrectOperandType);
                };
                // Since predicates for expressions are treated as filters on the child axis,
                // the node set is sorted in document order.
                //
                // # Reference
                // [3.3 Node-sets](https://www.w3.org/TR/1999/REC-xpath-19991116/#node-sets)
                // "Predicates are used to filter expressions in the same way that they are used
                // in location paths. It is an error if the expression to be filtered does not
                // evaluate to a node-set. The Predicate filters the node-set with respect to the
                // child axis."
                node_set.sort();

                let old_context_node = self.context.node.take();
                let old_context_position = self.context.position;
                let old_context_size = self.context.size;
                self.context.size = node_set.len();
                let mut new = XPathNodeSet::default();
                let stack_depth = self.context.stack.len();
                for (i, node) in node_set.iter().enumerate() {
                    self.context.position = i + 1;
                    self.context.node = Some(node.clone());
                    self.do_evaluate(predicate)?;
                    let ret = match self.context.stack.pop().unwrap() {
                        XPathObject::Number(number) => number == (i + 1) as f64,
                        object => object.as_boolean()?,
                    };
                    assert!(stack_depth == self.context.stack.len());
                    if ret {
                        new.push(node);
                    }
                }

                self.context.node = old_context_node;
                self.context.position = old_context_position;
                self.context.size = old_context_size;
                self.context.push_object(new.into());
            }
            XPathSyntaxTree::FunctionCall {
                ref name,
                ref arguments,
            } => {
                let func = self.context.functions.get(name)?;
                let num_args = arguments.len();
                let arguments = arguments.clone();
                for arg in arguments {
                    self.do_evaluate(arg)?;
                }
                let object = func(&mut self.context, num_args)?;
                self.context.push_object(object);
            }
            XPathSyntaxTree::Equal(left, right) => {
                self.do_evaluate(left)?;
                self.do_evaluate(right)?;
                let right = self.context.stack.pop().unwrap();
                let left = self.context.stack.pop().unwrap();
                self.context.push_object(left.eq(right)?);
            }
            XPathSyntaxTree::NotEqual(left, right) => {
                self.do_evaluate(left)?;
                self.do_evaluate(right)?;
                let right = self.context.stack.pop().unwrap();
                let left = self.context.stack.pop().unwrap();
                self.context.push_object(left.ne(right)?);
            }
            XPathSyntaxTree::Less(left, right) => {
                self.do_evaluate(left)?;
                self.do_evaluate(right)?;
                let right = self.context.stack.pop().unwrap();
                let left = self.context.stack.pop().unwrap();
                self.context.push_object(left.lt(right)?);
            }
            XPathSyntaxTree::LessOrEqual(left, right) => {
                self.do_evaluate(left)?;
                self.do_evaluate(right)?;
                let right = self.context.stack.pop().unwrap();
                let left = self.context.stack.pop().unwrap();
                self.context.push_object(left.le(right)?);
            }
            XPathSyntaxTree::Greater(left, right) => {
                self.do_evaluate(left)?;
                self.do_evaluate(right)?;
                let right = self.context.stack.pop().unwrap();
                let left = self.context.stack.pop().unwrap();
                self.context.push_object(left.gt(right)?);
            }
            XPathSyntaxTree::GreaterOrEqual(left, right) => {
                self.do_evaluate(left)?;
                self.do_evaluate(right)?;
                let right = self.context.stack.pop().unwrap();
                let left = self.context.stack.pop().unwrap();
                self.context.push_object(left.ge(right)?);
            }
            XPathSyntaxTree::And(left, right) => {
                self.do_evaluate(left)?;
                let mut ret = self.context.stack.pop().unwrap().as_boolean()?;
                if ret {
                    self.do_evaluate(right)?;
                    ret &= self.context.stack.pop().unwrap().as_boolean()?;
                }
                self.context.push_object(ret.into());
            }
            XPathSyntaxTree::Or(left, right) => {
                self.do_evaluate(left)?;
                let mut ret = self.context.stack.pop().unwrap().as_boolean()?;
                if !ret {
                    self.do_evaluate(right)?;
                    ret |= self.context.stack.pop().unwrap().as_boolean()?;
                }
                self.context.push_object(ret.into());
            }
            XPathSyntaxTree::Addition(left, right) => {
                self.do_evaluate(left)?;
                self.do_evaluate(right)?;
                let right = self.context.stack.pop().unwrap();
                let left = self.context.stack.pop().unwrap();
                let ret = left.add(right)?;
                self.context.push_object(ret);
            }
            XPathSyntaxTree::Subtraction(left, right) => {
                self.do_evaluate(left)?;
                self.do_evaluate(right)?;
                let right = self.context.stack.pop().unwrap();
                let left = self.context.stack.pop().unwrap();
                let ret = left.sub(right)?;
                self.context.push_object(ret);
            }
            XPathSyntaxTree::Multiplication(left, right) => {
                self.do_evaluate(left)?;
                self.do_evaluate(right)?;
                let right = self.context.stack.pop().unwrap();
                let left = self.context.stack.pop().unwrap();
                let ret = left.mul(right)?;
                self.context.push_object(ret);
            }
            XPathSyntaxTree::Division(left, right) => {
                self.do_evaluate(left)?;
                self.do_evaluate(right)?;
                let right = self.context.stack.pop().unwrap();
                let left = self.context.stack.pop().unwrap();
                let ret = left.div(right)?;
                self.context.push_object(ret);
            }
            XPathSyntaxTree::Remainder(left, right) => {
                self.do_evaluate(left)?;
                self.do_evaluate(right)?;
                let right = self.context.stack.pop().unwrap();
                let left = self.context.stack.pop().unwrap();
                let ret = left.rem(right)?;
                self.context.push_object(ret);
            }
            XPathSyntaxTree::Negation(expr) => {
                self.do_evaluate(expr)?;
                let neg = self.context.stack.pop().unwrap().neg()?;
                self.context.push_object(neg);
            }
            XPathSyntaxTree::Literal(ref literal) => {
                self.context.push_object(literal.clone().into());
            }
            XPathSyntaxTree::Number(ref number) => {
                self.context.push_object(
                    number
                        .parse::<f64>()
                        .map_err(|_| XPathError::WrongTypeConversion)?
                        .into(),
                );
            }
            XPathSyntaxTree::VariableReference(ref reference) => {
                self.context
                    .push_object(self.context.variables.get(reference)?);
            }
        }
        Ok(())
    }
}

#[derive(Default)]
pub struct XPathContext {
    node: Option<Node<dyn NodeSpec>>,
    position: usize,
    size: usize,
    variables: VariableSet,
    functions: FunctionLibrary,
    namespaces: NamespaceSet,
    stack: Vec<XPathObject>,
}

impl XPathContext {
    pub(super) fn push_object(&mut self, object: XPathObject) {
        self.stack.push(object);
    }

    pub(super) fn pop_object(&mut self) -> Option<XPathObject> {
        self.stack.pop()
    }
}

#[derive(Clone)]
pub enum XPathObject {
    Number(f64),
    String(Box<str>),
    Boolean(bool),
    NodeSet(XPathNodeSet),
}

impl XPathObject {
    pub fn cast_to_number(self) -> Result<Self, XPathError> {
        match self {
            XPathObject::Number(number) => Ok(XPathObject::Number(number)),
            XPathObject::Boolean(boolean) => Ok(XPathObject::Number(boolean as u8 as f64)),
            XPathObject::String(string) => Ok(XPathObject::Number(
                string.trim().parse::<f64>().unwrap_or(f64::NAN),
            )),
            node_set @ XPathObject::NodeSet(_) => node_set.cast_to_string()?.cast_to_number(),
        }
    }

    pub fn cast_to_string(self) -> Result<Self, XPathError> {
        match self {
            XPathObject::Number(number) => {
                if number.is_nan() {
                    Ok(XPathObject::String("NaN".into()))
                } else if number == 0.0 && number == -0.0 {
                    Ok(XPathObject::String("0".into()))
                } else if number == f64::INFINITY {
                    Ok(XPathObject::String("Infinity".into()))
                } else if number == f64::NEG_INFINITY {
                    Ok(XPathObject::String("-Infinity".into()))
                } else {
                    Ok(XPathObject::String(number.to_string().into()))
                }
            }
            XPathObject::Boolean(boolean) => Ok(XPathObject::String(boolean.to_string().into())),
            XPathObject::String(string) => Ok(XPathObject::String(string)),
            XPathObject::NodeSet(node_set) => Ok(node_set
                .first()
                .map(|node| node.xpath_string_value())
                .unwrap_or_default()
                .into()),
        }
    }

    pub fn cast_to_boolean(self) -> Result<Self, XPathError> {
        match self {
            XPathObject::Number(number) => Ok(XPathObject::Boolean(
                number != 0.0 && number != -0.0 && !number.is_nan(),
            )),
            XPathObject::Boolean(boolean) => Ok(XPathObject::Boolean(boolean)),
            XPathObject::String(string) => Ok(XPathObject::Boolean(!string.is_empty())),
            XPathObject::NodeSet(node_set) => Ok(XPathObject::Boolean(!node_set.is_empty())),
        }
    }

    pub fn as_number(self) -> Result<f64, XPathError> {
        match self {
            XPathObject::Number(number) => Ok(number),
            object => {
                let XPathObject::Number(number) = object.cast_to_number()? else {
                    unreachable!()
                };
                Ok(number)
            }
        }
    }

    pub fn as_string(self) -> Result<Box<str>, XPathError> {
        match self {
            XPathObject::String(string) => Ok(string),
            object => {
                let XPathObject::String(string) = object.cast_to_string()? else {
                    unreachable!()
                };
                Ok(string)
            }
        }
    }

    pub fn as_boolean(self) -> Result<bool, XPathError> {
        match self {
            XPathObject::Boolean(boolean) => Ok(boolean),
            object => {
                let XPathObject::Boolean(boolean) = object.cast_to_boolean()? else {
                    unreachable!()
                };
                Ok(boolean)
            }
        }
    }

    pub fn as_nodeset(self) -> Result<XPathNodeSet, XPathError> {
        if let XPathObject::NodeSet(node_set) = self {
            Ok(node_set)
        } else {
            Err(XPathError::WrongTypeConversion)
        }
    }
}

macro_rules! impl_number_to_xpath_object {
    ( $( $t:ty ),* ) => {
        $(
            impl From<$t> for XPathObject {
                fn from(value: $t) -> Self {
                    XPathObject::Number(value as f64)
                }
            }
        )*
    };
}
impl_number_to_xpath_object!(
    i8, i16, i32, i64, i128, isize, u8, u16, u32, u64, u128, usize, f32, f64
);

macro_rules! impl_string_to_xpath_object {
    ( $( $t:ty ),* ) => {
        $(
            impl From<$t> for XPathObject {
                fn from(value: $t) -> Self {
                    let s: &str = value.as_ref();
                    XPathObject::String(s.into())
                }
            }
        )*
    };
}
impl_string_to_xpath_object!(
    &str,
    String,
    Box<str>,
    std::rc::Rc<str>,
    std::sync::Arc<str>,
    std::borrow::Cow<'_, str>
);

impl From<bool> for XPathObject {
    fn from(value: bool) -> Self {
        XPathObject::Boolean(value)
    }
}

impl From<XPathNodeSet> for XPathObject {
    fn from(value: XPathNodeSet) -> Self {
        XPathObject::NodeSet(value)
    }
}

macro_rules! impl_node_to_xpath_object {
    ( $( $t:ty ),* ) => {
        $(
            impl From<$t> for XPathObject {
                fn from(value: $t) -> Self {
                    let mut nodeset = XPathNodeSet::default();
                    nodeset.push(value);
                    XPathObject::NodeSet(nodeset)
                }
            }
        )*
    };
}

impl_node_to_xpath_object!(
    Document,
    Element,
    Attribute,
    Namespace,
    ProcessingInstruction,
    Comment,
    Text,
    CDATASection
);

#[derive(Clone, Default)]
pub struct XPathNodeSet {
    nodes: Vec<Node<dyn NodeSpec>>,
}

impl XPathNodeSet {
    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = Node<dyn NodeSpec>> + '_ {
        self.nodes.iter().cloned()
    }

    pub fn first(&self) -> Option<&Node<dyn NodeSpec>> {
        self.nodes.first()
    }

    pub fn last(&self) -> Option<&Node<dyn NodeSpec>> {
        self.nodes.last()
    }

    pub fn contains(&self, node: impl Into<Node<dyn NodeSpec>>) -> bool {
        let node: Node<dyn NodeSpec> = node.into();
        self.iter().any(|n| node.is_same_node(n))
    }

    pub(crate) fn push(&mut self, node: impl Into<Node<dyn NodeSpec>>) {
        let node: Node<dyn NodeSpec> = node.into();
        if self.nodes.iter().any(|n| node.is_same_node(n)) {
            return;
        }
        self.nodes.push(node);
    }

    pub fn clear(&mut self) {
        self.nodes.clear();
    }

    pub(crate) fn sort(&mut self) {
        self.nodes
            .sort_unstable_by(|l, r| l.compare_document_order(r).unwrap());
    }

    pub(crate) fn reverse_sort(&mut self) {
        self.nodes
            .sort_unstable_by(|l, r| r.compare_document_order(l).unwrap());
    }

    pub fn union(&self, other: &Self) -> Self {
        let mut ret = self.clone();
        for node in other.iter() {
            ret.push(node);
        }
        ret.sort();
        ret
    }

    pub fn difference(&self, other: &Self) -> Self {
        let mut ret = Self::default();
        for node in other.iter() {
            if !self.contains(&node) {
                ret.push(node);
            }
        }
        for node in self.iter() {
            if !other.contains(&node) {
                ret.push(node);
            }
        }
        ret.sort();
        ret
    }

    pub fn intersection(&self, other: &Self) -> Self {
        let mut ret = Self::default();
        for node in other.iter() {
            if self.contains(&node) {
                ret.push(node);
            }
        }
        ret.sort();
        ret
    }
}

impl<'a> IntoIterator for &'a XPathNodeSet {
    type Item = &'a Node<dyn NodeSpec>;
    type IntoIter = std::slice::Iter<'a, Node<dyn NodeSpec>>;

    fn into_iter(self) -> Self::IntoIter {
        self.nodes.iter()
    }
}

#[derive(Default)]
struct VariableSet {
    map: HashMap<Cow<'static, str>, XPathObject>,
}

impl VariableSet {
    fn get(&self, name: &str) -> Result<XPathObject, XPathError> {
        self.map
            .get(name)
            .cloned()
            .ok_or(XPathError::UnresolvableVariableName)
    }
}

struct NamespaceSet {
    // (prefix, namespace name)
    prefix_map: HashMap<Cow<'static, str>, Cow<'static, str>>,
}

impl NamespaceSet {
    fn get_namespace_name(&self, prefix: Option<&str>) -> Option<&str> {
        let prefix = prefix.unwrap_or("");
        self.prefix_map
            .get(prefix)
            .map(|namespace_name| namespace_name.as_ref())
    }
}

impl Default for NamespaceSet {
    fn default() -> Self {
        Self {
            prefix_map: HashMap::from([
                (Cow::Borrowed("xml"), Cow::Borrowed(XML_XML_NAMESPACE)),
                (Cow::Borrowed("xmlns"), Cow::Borrowed(XML_NS_NAMESPACE)),
            ]),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum XPathCompileError {
    ExpressionNotTerminated,
    InvalidAbsoluteLocationPath,
    InvalidNodeTest,
    InvalidPredicate,
    InvalidFunctionCall,
    InvalidFunctionName,
    InvalidPrimaryExpr,
    InvalidLiteral,
    InvalidNumber,
    InvalidVariableReference,
    InvalidNCName,
    InvalidQName,
    UnknownError,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Axis {
    Ancestor,
    AncestorOrSelf,
    Attribute,
    Child,
    Descendant,
    DescendantOrSelf,
    Following,
    FollowingSibling,
    Namespace,
    Parent,
    Preceding,
    PrecedingSibling,
    SelfNode,
}

#[derive(Debug)]
enum NodeTest {
    Any,
    AnyLocalName(Box<str>),
    QName(Box<str>),
    Comment,
    Text,
    ProcessingInstruction(Option<Box<str>>),
    Node,
}

impl NodeTest {
    fn verify_node_type(&self, node_type: NodeType) -> bool {
        use NodeType::*;

        matches!(
            node_type,
            Document
                | Element
                | Attribute
                | Namespace
                | ProcessingInstruction
                | Comment
                | Text
                | CDATASection
        )
    }

    fn match_node(
        &self,
        node: &Node<dyn NodeSpec>,
        principal_node_type: NodeType,
        namespace_set: &NamespaceSet,
    ) -> bool {
        assert!(matches!(
            principal_node_type,
            NodeType::Attribute | NodeType::Element | NodeType::Namespace
        ));
        if !self.verify_node_type(node.node_type()) {
            return false;
        }
        match node.downcast() {
            NodeKind::Document(_) => self.match_document(),
            NodeKind::Element(element) => self.match_element(
                &element,
                matches!(principal_node_type, NodeType::Element),
                namespace_set,
            ),
            NodeKind::Attribute(attribute) => self.match_attribute(
                &attribute,
                matches!(principal_node_type, NodeType::Attribute),
                namespace_set,
            ),
            NodeKind::Namespace(namespace) => self.match_namespace(
                &namespace,
                matches!(principal_node_type, NodeType::Namespace),
            ),
            NodeKind::ProcessingInstruction(pi) => self.match_processing_instruction(&pi),
            NodeKind::Comment(_) => self.match_comment(),
            NodeKind::Text(_) | NodeKind::CDATASection(_) => self.match_text(),
            _ => false,
        }
    }

    fn match_document(&self) -> bool {
        matches!(self, NodeTest::Node)
    }

    fn match_element(
        &self,
        element: &Element,
        principal: bool,
        namespace_set: &NamespaceSet,
    ) -> bool {
        match self {
            NodeTest::Any => principal,
            NodeTest::Node => true,
            NodeTest::AnyLocalName(local) => local.as_ref() == element.local_name().as_ref(),
            NodeTest::QName(qname) => {
                if let Some((prefix, local_name)) = qname.split_once(':') {
                    element
                        .search_namespace_by_prefix(Some(prefix))
                        .map(|namespace| namespace.namespace_name())
                        .as_deref()
                        .or_else(|| namespace_set.get_namespace_name(Some(prefix)))
                        .is_some_and(|namespace_name| {
                            element.namespace_name().as_deref() == Some(namespace_name)
                                && local_name == element.local_name().as_ref()
                        })
                } else {
                    qname.as_ref() == element.name().as_ref() && element.namespace_name().is_none()
                }
            }
            NodeTest::Comment | NodeTest::ProcessingInstruction(_) | NodeTest::Text => false,
        }
    }

    fn match_attribute(
        &self,
        attribute: &Attribute,
        principal: bool,
        namespace_set: &NamespaceSet,
    ) -> bool {
        match self {
            NodeTest::Any => principal,
            NodeTest::Node => true,
            NodeTest::AnyLocalName(local) => local.as_ref() == attribute.local_name().as_ref(),
            NodeTest::QName(qname) => {
                if let Some((prefix, local_name)) = qname.split_once(':') {
                    attribute
                        .owner_element()
                        .and_then(|element| element.search_namespace_by_prefix(Some(prefix)))
                        .map(|namespace| namespace.namespace_name())
                        .as_deref()
                        .or_else(|| namespace_set.get_namespace_name(Some(prefix)))
                        .is_some_and(|namespace_name| {
                            attribute.namespace_name().as_deref() == Some(namespace_name)
                                && local_name == attribute.local_name().as_ref()
                        })
                } else {
                    qname.as_ref() == attribute.name().as_ref()
                        && attribute.namespace_name().is_none()
                }
            }
            NodeTest::Comment | NodeTest::ProcessingInstruction(_) | NodeTest::Text => false,
        }
    }

    fn match_namespace(&self, namespace: &Namespace, principal: bool) -> bool {
        match self {
            NodeTest::Any => principal,
            NodeTest::Node => true,
            NodeTest::QName(name) | NodeTest::AnyLocalName(name) => {
                name.as_ref() == namespace.prefix().as_deref().unwrap_or_default()
            }
            NodeTest::Comment | NodeTest::ProcessingInstruction(_) | NodeTest::Text => false,
        }
    }

    fn match_processing_instruction(&self, pi: &ProcessingInstruction) -> bool {
        match self {
            NodeTest::Node => true,
            NodeTest::QName(target) | NodeTest::AnyLocalName(target) => {
                target.as_ref() == pi.target().as_ref()
            }
            NodeTest::ProcessingInstruction(literal) => literal
                .as_deref()
                .is_none_or(|literal| literal == pi.target().as_ref()),
            NodeTest::Any | NodeTest::Comment | NodeTest::Text => false,
        }
    }

    fn match_comment(&self) -> bool {
        matches!(self, NodeTest::Comment | NodeTest::Node)
    }

    fn match_text(&self) -> bool {
        matches!(self, NodeTest::Text | NodeTest::Node)
    }
}

#[derive(Debug)]
enum XPathSyntaxTree {
    Union(usize, usize),
    Slash(usize, usize),
    LocationPathRoot,
    Step {
        first: bool,
        axis: Axis,
        node_test: Arc<NodeTest>,
        predicate: usize,
    },
    Predicate {
        expression: usize,
        next: usize,
    },
    FilterExpr {
        expression: usize,
        predicate: usize,
    },
    FunctionCall {
        name: Box<str>,
        arguments: Vec<usize>,
    },
    Equal(usize, usize),
    NotEqual(usize, usize),
    Less(usize, usize),
    LessOrEqual(usize, usize),
    Greater(usize, usize),
    GreaterOrEqual(usize, usize),
    And(usize, usize),
    Or(usize, usize),
    Addition(usize, usize),
    Subtraction(usize, usize),
    Multiplication(usize, usize),
    Division(usize, usize),
    Remainder(usize, usize),
    Negation(usize),
    Literal(Box<str>),
    Number(Box<str>),
    VariableReference(Box<str>),
}

impl Node<dyn NodeSpec> {
    fn xpath_string_value(&self) -> String {
        match self.downcast() {
            NodeKind::Document(document) => document.xpath_string_value(),
            NodeKind::Element(element) => element.xpath_string_value(),
            NodeKind::Attribute(attribute) => attribute.xpath_string_value(),
            NodeKind::Namespace(namespace) => namespace.xpath_string_value(),
            NodeKind::ProcessingInstruction(pi) => pi.xpath_string_value(),
            NodeKind::Comment(comment) => comment.xpath_string_value(),
            NodeKind::Text(text) => text.xpath_string_value(),
            _ => unimplemented!(),
        }
    }
}

impl Document {
    fn xpath_string_value(&self) -> String {
        let mut buf = String::new();
        let mut children = self.first_child();
        while let Some(child) = children {
            if let Some(element) = child.as_element() {
                buf.push_str(&element.xpath_string_value());
            }
            children = child.next_sibling();
        }
        buf
    }
}

impl Element {
    fn xpath_string_value(&self) -> String {
        let mut buf = String::new();
        let mut children = self.first_child();
        while let Some(child) = children {
            match child.downcast() {
                NodeKind::Text(text) => buf.push_str(&text.data()),
                NodeKind::CDATASection(cdata) => buf.push_str(&cdata.data()),
                _ => {}
            }

            if let Some(first) = child.first_child() {
                children = Some(first);
            } else if let Some(next) = child.next_sibling() {
                children = Some(next);
            } else {
                children = None;
                let mut par = child.parent_node();
                while let Some(now) = par {
                    if self.is_same_node(&now) {
                        break;
                    }
                    if let Some(next) = now.next_sibling() {
                        children = Some(next);
                        break;
                    }
                    par = now.parent_node();
                }
            }
        }
        buf
    }
}

impl Attribute {
    fn xpath_string_value(&self) -> String {
        self.value()
    }
}

impl Namespace {
    fn xpath_string_value(&self) -> String {
        self.namespace_name().to_string()
    }
}

impl ProcessingInstruction {
    fn xpath_string_value(&self) -> String {
        self.data().map(|data| data.to_string()).unwrap_or_default()
    }
}

impl Comment {
    fn xpath_string_value(&self) -> String {
        self.data().to_string()
    }
}

impl Text {
    fn xpath_string_value(&self) -> String {
        self.data().to_string()
    }
}
