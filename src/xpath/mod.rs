mod compile;
mod function;
mod ops;

use std::{borrow::Cow, collections::HashMap};

pub use compile::*;

use crate::{
    tree::{
        Attribute, Comment, Document, Element, Node, ProcessingInstruction, Text,
        convert::NodeKind, namespace::Namespace, node::NodeSpec,
    },
    xpath::function::FunctionLibrary,
};

pub enum XPathError {
    IncorrectOperandType,
    IncorrectNumberOfArgument,
    IncorrectArgumentType,
    WrongTypeConversion,
    UnresolvableFunctionName,
    UnresolvableVariableName,
}

pub struct XPathExpression {
    root: usize,
    tree: Vec<XPathSyntaxTree>,
    context: XPathContext,
}

impl XPathExpression {
    pub fn evaluate(&mut self, document: Document) -> Result<XPathObject, XPathError> {
        self.do_evaluate(self.root)?;
        todo!()
    }

    fn do_evaluate(&mut self, op: usize) -> Result<(), XPathError> {
        match self.tree[op] {
            XPathSyntaxTree::Union(left, right) => {
                self.do_evaluate(left)?;
                self.do_evaluate(right)?;
                let right = self.context.stack.pop().unwrap();
                let left = self.context.stack.pop().unwrap();
                todo!("Union")
            }
            XPathSyntaxTree::Slash(left, right) => {
                self.do_evaluate(left)?;
                self.do_evaluate(right)?;
                let right = self.context.stack.pop().unwrap();
                let left = self.context.stack.pop().unwrap();
                todo!("Slash")
            }
            XPathSyntaxTree::LocationPathRoot => todo!(),
            XPathSyntaxTree::Step {
                axis,
                ref node_test,
            } => todo!(),
            XPathSyntaxTree::Predicate {
                argument,
                expression,
            } => {
                self.do_evaluate(argument)?;
                todo!()
            }
            XPathSyntaxTree::FilterExpr {
                expression,
                predicate,
            } => {
                self.do_evaluate(expression)?;
                todo!()
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
                func(&mut self.context, num_args)?;
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

    pub(crate) fn push(&mut self, node: impl Into<Node<dyn NodeSpec>>) {
        let node: Node<dyn NodeSpec> = node.into();
        if self.nodes.iter().any(|n| node.is_same_node(n)) {
            return;
        }
        self.nodes.push(node);
    }

    pub(crate) fn clear(&mut self) {
        self.nodes.clear();
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

#[derive(Default)]
struct NamespaceSet;

pub enum XPathCompileError {
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

enum NodeTest {
    Any,
    AnyLocalName(Box<str>),
    QName(Box<str>),
    Comment,
    Text,
    ProcessingInstruction(usize),
    Node,
}

enum XPathSyntaxTree {
    Union(usize, usize),
    Slash(usize, usize),
    LocationPathRoot,
    Step {
        axis: Axis,
        node_test: NodeTest,
    },
    Predicate {
        argument: usize,
        expression: usize,
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
