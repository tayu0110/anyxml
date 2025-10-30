use std::sync::Arc;

use crate::{
    XMLVersion,
    xpath::{Axis, NodeTest, XPathCompileError, XPathContext, XPathExpression, XPathSyntaxTree},
};

pub fn compile(mut xpath: &str) -> Result<XPathExpression, XPathCompileError> {
    let mut tree = vec![];
    let root = parse_expr(&mut xpath, &mut tree)?;
    xpath = xpath.trim_start_matches(|c| XMLVersion::default().is_whitespace(c));
    if !xpath.is_empty() {
        return Err(XPathCompileError::ExpressionNotTerminated);
    }

    // Optimization will not be implemented yet.
    // For now, the priority is correct operation over performance.

    Ok(XPathExpression {
        root,
        tree,
        context: XPathContext::default(),
    })
}

/// ```text
/// [1] LocationPath ::= RelativeLocationPath | AbsoluteLocationPath
/// ```
fn parse_location_path(
    xpath: &mut &str,
    tree: &mut Vec<XPathSyntaxTree>,
) -> Result<usize, XPathCompileError> {
    skip_whitespaces(xpath);
    if xpath.starts_with('/') {
        parse_absolute_location_path(xpath, tree)
    } else {
        parse_relative_location_path(xpath, tree, true)
    }
}

/// ```text
/// [2]  AbsoluteLocationPath               ::= '/' RelativeLocationPath?
///                                             | AbbreviatedAbsoluteLocationPath
/// [10] AbbreviatedAbsoluteLocationPath    ::= '//' RelativeLocationPath
/// ```
fn parse_absolute_location_path(
    xpath: &mut &str,
    tree: &mut Vec<XPathSyntaxTree>,
) -> Result<usize, XPathCompileError> {
    skip_whitespaces(xpath);
    *xpath = xpath
        .strip_prefix('/')
        .ok_or(XPathCompileError::InvalidAbsoluteLocationPath)?;
    let root = tree.len();
    tree.push(XPathSyntaxTree::LocationPathRoot);

    if let Some(rem) = xpath.strip_prefix('/') {
        // '//' is equal to '/descendant-or-self::node()'

        *xpath = rem;

        let right = tree.len();
        tree.push(XPathSyntaxTree::Step {
            first: false,
            axis: Axis::DescendantOrSelf,
            node_test: Arc::new(NodeTest::Node),
            predicate: usize::MAX,
        });

        let left = tree.len();
        tree.push(XPathSyntaxTree::Slash(root, right));

        let right = parse_relative_location_path(xpath, tree, false)?;
        let new = XPathSyntaxTree::Slash(left, right);
        let ret = tree.len();
        tree.push(new);
        return Ok(ret);
    }

    // RelativeLocationPath may not appear, so we need to check if RelativeLocationPath starts.
    // RelativeLocationPath starts with Step,
    // and Step starts with AxisSpecifier, NodeTest or AbbreviatedStep ('.', '..').

    skip_whitespaces(xpath);
    if xpath.starts_with(|c: char| {
        // '.'              : AbbreviatedStep ('.', '..')
        // '*'              : NameTest
        // '@'              : AbbreviatedAxisSpecifier
        // NameStartChar    : NameTest, AxisName
        matches!(c, '.' | '*' | '@') || (c != ':' && XMLVersion::default().is_name_start_char(c))
    }) {
        let right = parse_relative_location_path(xpath, tree, false)?;
        let new = XPathSyntaxTree::Slash(root, right);
        let ret = tree.len();
        tree.push(new);
        return Ok(ret);
    }

    Ok(root)
}

/// ```text
/// [3] RelativeLocationPath                ::= Step
///                                             | RelativeLocationPath '/' Step
///                                             | AbbreviatedRelativeLocationPath
/// [11] AbbreviatedRelativeLocationPath    ::= RelativeLocationPath '//' Step
/// ```
fn parse_relative_location_path(
    xpath: &mut &str,
    tree: &mut Vec<XPathSyntaxTree>,
    is_first_step: bool,
) -> Result<usize, XPathCompileError> {
    let mut left = parse_step(xpath, tree, is_first_step)?;
    skip_whitespaces(xpath);
    while let Some(rem) = xpath.strip_prefix('/') {
        let new = if let Some(rem) = rem.strip_prefix('/') {
            // '//' is equal to '/descendant-or-self::node()'

            *xpath = rem;
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
            let right = parse_step(xpath, tree, false)?;
            XPathSyntaxTree::Slash(left, right)
        } else {
            *xpath = rem;
            let right = parse_step(xpath, tree, false)?;
            XPathSyntaxTree::Slash(left, right)
        };
        left = tree.len();
        tree.push(new);
        skip_whitespaces(xpath);
    }
    Ok(left)
}

/// ```text
/// [4]  Step               ::= AxisSpecifier NodeTest Predicate* | AbbreviatedStep
/// [12] AbbreviatedStep    ::= '.' | '..'
/// ```
fn parse_step(
    xpath: &mut &str,
    tree: &mut Vec<XPathSyntaxTree>,
    is_first_step: bool,
) -> Result<usize, XPathCompileError> {
    skip_whitespaces(xpath);
    if let Some(rem) = xpath.strip_prefix('.') {
        // AbbreviatedStep

        let ret = tree.len();
        if let Some(rem) = rem.strip_prefix('.') {
            *xpath = rem;
            tree.push(XPathSyntaxTree::Step {
                first: is_first_step,
                axis: Axis::Parent,
                node_test: Arc::new(NodeTest::Node),
                predicate: usize::MAX,
            });
        } else {
            *xpath = rem;
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

    let axis = parse_axis_specifier(xpath)?;
    let node_test = parse_node_test(xpath, tree)?;

    let mut prev = tree.len();
    let ret = prev;
    tree.push(XPathSyntaxTree::Step {
        first: is_first_step,
        axis,
        node_test: Arc::new(node_test),
        predicate: usize::MAX,
    });

    skip_whitespaces(xpath);
    while xpath.starts_with('[') {
        let expression = parse_predicate(xpath, tree)?;
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
        skip_whitespaces(xpath);
    }
    Ok(ret)
}

/// ```text
/// [5]  AxisSpecifier            ::= AxisName '::' | AbbreviatedAxisSpecifier
/// [6]  AxisName                 ::= 'ancestor'
///                                   | 'ancestor-or-self'
///                                   | 'attribute'
///                                   | 'child'
///                                   | 'descendant'
///                                   | 'descendant-or-self'
///                                   | 'following'
///                                   | 'following-sibling'
///                                   | 'namespace'
///                                   | 'parent'
///                                   | 'preceding'
///                                   | 'preceding-sibling'
///                                   | 'self'
/// [13] AbbreviatedAxisSpecifier ::= '@'?
/// ```
fn parse_axis_specifier(xpath: &mut &str) -> Result<Axis, XPathCompileError> {
    skip_whitespaces(xpath);
    let (rem, axis) = if let Some(rem) = xpath.strip_prefix('@') {
        *xpath = rem;
        return Ok(Axis::Attribute);
    } else if let Some(rem) = xpath.strip_prefix("ancestor") {
        if let Some(rem) = rem.strip_prefix("-or-self") {
            (rem, Axis::AncestorOrSelf)
        } else {
            (rem, Axis::Ancestor)
        }
    } else if let Some(rem) = xpath.strip_prefix("attribute") {
        (rem, Axis::Attribute)
    } else if let Some(rem) = xpath.strip_prefix("child") {
        (rem, Axis::Child)
    } else if let Some(rem) = xpath.strip_prefix("descendant") {
        if let Some(rem) = rem.strip_prefix("-or-self") {
            (rem, Axis::DescendantOrSelf)
        } else {
            (rem, Axis::Descendant)
        }
    } else if let Some(rem) = xpath.strip_prefix("following") {
        if let Some(rem) = rem.strip_prefix("-sibling") {
            (rem, Axis::FollowingSibling)
        } else {
            (rem, Axis::Following)
        }
    } else if let Some(rem) = xpath.strip_prefix("namespace") {
        (rem, Axis::Namespace)
    } else if let Some(rem) = xpath.strip_prefix("parent") {
        (rem, Axis::Parent)
    } else if let Some(rem) = xpath.strip_prefix("preceding") {
        if let Some(rem) = rem.strip_prefix("-sibling") {
            (rem, Axis::PrecedingSibling)
        } else {
            (rem, Axis::Preceding)
        }
    } else if let Some(rem) = xpath.strip_prefix("self") {
        (rem, Axis::SelfNode)
    } else {
        return Ok(Axis::Child);
    };

    skip_whitespaces(xpath);
    if let Some(rem) = rem.strip_prefix("::") {
        // Only when `::` follows an AxisName is it considered a match
        // for that specific AxisName.
        *xpath = rem;
        Ok(axis)
    } else {
        // If it is not a specific AxisName, `child` is considered omitted
        // (i.e., it is considered to match an AbbreviatedAxisSpecifier).
        Ok(Axis::Child)
    }
}

/// ```text
/// [7] NodeTest    ::= NameTest
///                     | NodeType '(' ')'
///                     | 'processing-instruction' '(' Literal ')'
/// [37] NameTest   ::= '*'
///                     | NCName ':' '*'
///                     | QName
/// [38] NodeType   ::= 'comment'
///                     | 'text'
///                     | 'processing-instruction'
///                     | 'node'
/// ```
fn parse_node_test(
    xpath: &mut &str,
    tree: &mut Vec<XPathSyntaxTree>,
) -> Result<NodeTest, XPathCompileError> {
    skip_whitespaces(xpath);
    if let Some(rem) = xpath.strip_prefix('*') {
        *xpath = rem;
        Ok(NodeTest::Any)
    } else if let Some(rem) = xpath.strip_prefix("text(") {
        *xpath = rem;
        skip_whitespaces(xpath);
        *xpath = xpath
            .strip_prefix(')')
            .ok_or(XPathCompileError::InvalidNodeTest)?;
        Ok(NodeTest::Text)
    } else if let Some(rem) = xpath.strip_prefix("comment(") {
        *xpath = rem;
        skip_whitespaces(xpath);
        *xpath = xpath
            .strip_prefix(')')
            .ok_or(XPathCompileError::InvalidNodeTest)?;
        Ok(NodeTest::Comment)
    } else if let Some(rem) = xpath.strip_prefix("node(") {
        *xpath = rem;
        skip_whitespaces(xpath);
        *xpath = xpath
            .strip_prefix(')')
            .ok_or(XPathCompileError::InvalidNodeTest)?;
        Ok(NodeTest::Node)
    } else if let Some(rem) = xpath.strip_prefix("processing-instruction(") {
        skip_whitespaces(xpath);
        if let Some(rem) = rem.strip_prefix(')') {
            *xpath = rem;
            Ok(NodeTest::ProcessingInstruction(None))
        } else {
            *xpath = rem;
            let literal = parse_literal(xpath, tree)?;
            skip_whitespaces(xpath);
            *xpath = xpath
                .strip_prefix(')')
                .ok_or(XPathCompileError::InvalidNodeTest)?;
            if let XPathSyntaxTree::Literal(literal) = &tree[literal] {
                Ok(NodeTest::ProcessingInstruction(Some(literal.clone())))
            } else {
                // is it correct ???
                Ok(NodeTest::ProcessingInstruction(None))
            }
        }
    } else {
        let (ncname, rem) = parse_ncname(xpath)?;
        if let Some(remove_colon) = rem.strip_prefix(':') {
            if let Some(rem) = remove_colon.strip_prefix('*') {
                // NCName:*
                *xpath = rem;
                Ok(NodeTest::AnyLocalName(ncname.into()))
            } else if let Ok((_, rem)) = parse_ncname(remove_colon) {
                // prefixed QName
                let (qname, rem) = xpath.split_at(xpath.len() - rem.len());
                *xpath = rem;
                Ok(NodeTest::QName(qname.into()))
            } else {
                // unprefixed QName followed colon
                *xpath = rem;
                Ok(NodeTest::QName(ncname.into()))
            }
        } else {
            // QName (also NCName)
            *xpath = rem;
            Ok(NodeTest::QName(ncname.into()))
        }
    }
}

/// ```text
/// [8] Predicate       ::= '[' PredicateExpr ']'
/// [9] PredicateExpr   ::= Expr
/// ```
fn parse_predicate(
    xpath: &mut &str,
    tree: &mut Vec<XPathSyntaxTree>,
) -> Result<usize, XPathCompileError> {
    skip_whitespaces(xpath);
    *xpath = xpath
        .strip_prefix('[')
        .ok_or(XPathCompileError::InvalidPredicate)?;
    let ret = parse_expr(xpath, tree)?;
    skip_whitespaces(xpath);
    *xpath = xpath
        .strip_prefix(']')
        .ok_or(XPathCompileError::InvalidPredicate)?;
    Ok(ret)
}

/// ```text
/// [14] Expr ::= OrExpr
/// ```
fn parse_expr(
    xpath: &mut &str,
    tree: &mut Vec<XPathSyntaxTree>,
) -> Result<usize, XPathCompileError> {
    parse_or_expr(xpath, tree)
}

/// ```text
/// [15] PrimaryExpr ::= VariableReference
///                      | '(' Expr ')'
///                      | Literal
///                      | Number
///                      | FunctionCall
/// ```
fn parse_primary_expr(
    xpath: &mut &str,
    tree: &mut Vec<XPathSyntaxTree>,
) -> Result<usize, XPathCompileError> {
    skip_whitespaces(xpath);
    if xpath.starts_with('$') {
        parse_variable_reference(xpath, tree)
    } else if let Some(rem) = xpath.strip_prefix('(') {
        *xpath = rem;
        let ret = parse_expr(xpath, tree)?;
        skip_whitespaces(xpath);
        *xpath = xpath
            .strip_prefix(')')
            .ok_or(XPathCompileError::InvalidPrimaryExpr)?;
        Ok(ret)
    } else if xpath.starts_with(['\'', '"']) {
        parse_literal(xpath, tree)
    } else if xpath.starts_with(|c: char| c.is_ascii_digit() || c == '.') {
        parse_number(xpath, tree)
    } else {
        parse_function_call(xpath, tree)
    }
}

/// ```text
/// [16] FunctionCall ::= FunctionName '(' ( Argument ( ',' Argument )* )? ')'
/// ```
fn parse_function_call(
    xpath: &mut &str,
    tree: &mut Vec<XPathSyntaxTree>,
) -> Result<usize, XPathCompileError> {
    skip_whitespaces(xpath);
    let (name, rem) = parse_function_name(xpath)?;
    *xpath = rem;
    skip_whitespaces(xpath);
    *xpath = xpath
        .strip_prefix('(')
        .ok_or(XPathCompileError::InvalidFunctionCall)?;
    skip_whitespaces(xpath);
    if let Some(rem) = xpath.strip_prefix(')') {
        *xpath = rem;
        let ret = tree.len();
        tree.push(XPathSyntaxTree::FunctionCall {
            name: name.into(),
            arguments: vec![],
        });
        return Ok(ret);
    }

    let mut arguments = vec![parse_argument(xpath, tree)?];
    skip_whitespaces(xpath);
    while let Some(rem) = xpath.strip_prefix(',') {
        *xpath = rem;
        arguments.push(parse_argument(xpath, tree)?);
        skip_whitespaces(xpath);
    }

    *xpath = xpath
        .strip_prefix(')')
        .ok_or(XPathCompileError::InvalidFunctionCall)?;

    let ret = tree.len();
    tree.push(XPathSyntaxTree::FunctionCall {
        name: name.into(),
        arguments,
    });
    Ok(ret)
}

/// ```text
/// [17] Argument ::= Expr
/// ```
fn parse_argument(
    xpath: &mut &str,
    tree: &mut Vec<XPathSyntaxTree>,
) -> Result<usize, XPathCompileError> {
    parse_expr(xpath, tree)
}

/// ```text
/// [18] UnionExpr ::= PathExpr | UnionExpr '|' PathExpr
/// ```
fn parse_union_expr(
    xpath: &mut &str,
    tree: &mut Vec<XPathSyntaxTree>,
) -> Result<usize, XPathCompileError> {
    let mut left = parse_path_expr(xpath, tree)?;
    skip_whitespaces(xpath);
    while let Some(rem) = xpath.strip_prefix('|') {
        *xpath = rem;
        let right = parse_path_expr(xpath, tree)?;
        let new = XPathSyntaxTree::Union(left, right);
        left = tree.len();
        tree.push(new);
        skip_whitespaces(xpath);
    }
    Ok(left)
}

/// ```text
/// [19] PathExpr ::= LocationPath
///                   | FilterExpr
///                   | FilterExpr '/' RelativeLocationPath
///                   | FilterExpr '//' RelativeLocationPath
/// ```
fn parse_path_expr(
    xpath: &mut &str,
    tree: &mut Vec<XPathSyntaxTree>,
) -> Result<usize, XPathCompileError> {
    skip_whitespaces(xpath);
    let filter_expr =
        if xpath.starts_with(|c: char| matches!(c, '$' | '(' | '"' | '\'') || c.is_ascii_digit()) {
            // VariableReference, grouped Expr, Literal, Number
            true
        } else if xpath.starts_with(['/', '.', '@', '*']) {
            // some LocationPath types
            false
        } else {
            // Check if `xpath` starts with FunctionCall.
            parse_function_name(xpath).is_ok_and(|(_, mut rem)| {
                skip_whitespaces(&mut rem);
                rem.starts_with('(')
            })
        };

    if filter_expr {
        let mut left = parse_filter_expr(xpath, tree)?;
        skip_whitespaces(xpath);
        if let Some(rem) = xpath.strip_prefix('/') {
            let new = if let Some(rem) = rem.strip_prefix('/') {
                // '//' is equal to '/descendant-or-self::node()'
                *xpath = rem;
                let right = tree.len();
                tree.push(XPathSyntaxTree::Step {
                    first: false,
                    axis: Axis::DescendantOrSelf,
                    node_test: Arc::new(NodeTest::Node),
                    predicate: usize::MAX,
                });

                let root = tree.len();
                tree.push(XPathSyntaxTree::Slash(left, right));

                let right = parse_relative_location_path(xpath, tree, false)?;
                XPathSyntaxTree::Slash(root, right)
            } else {
                *xpath = rem;
                let right = parse_relative_location_path(xpath, tree, false)?;
                XPathSyntaxTree::Slash(left, right)
            };
            left = tree.len();
            tree.push(new);
        }
        Ok(left)
    } else {
        parse_location_path(xpath, tree)
    }
}

/// ```text
/// [20] FilterExpr ::= PrimaryExpr | FilterExpr Predicate
/// ```
fn parse_filter_expr(
    xpath: &mut &str,
    tree: &mut Vec<XPathSyntaxTree>,
) -> Result<usize, XPathCompileError> {
    let mut expression = parse_primary_expr(xpath, tree)?;

    skip_whitespaces(xpath);
    while xpath.starts_with('[') {
        let predicate = parse_predicate(xpath, tree)?;
        let new = XPathSyntaxTree::FilterExpr {
            expression,
            predicate,
        };
        expression = tree.len();
        tree.push(new);
        skip_whitespaces(xpath);
    }
    Ok(expression)
}

/// ```text
/// [21] OrExpr ::= AndExpr | OrExpr 'or' AndExpr
/// ```
fn parse_or_expr(
    xpath: &mut &str,
    tree: &mut Vec<XPathSyntaxTree>,
) -> Result<usize, XPathCompileError> {
    let mut left = parse_and_expr(xpath, tree)?;
    skip_whitespaces(xpath);
    while let Some(rem) = xpath.strip_prefix("or") {
        *xpath = rem;
        let right = parse_and_expr(xpath, tree)?;
        let new = XPathSyntaxTree::Or(left, right);
        left = tree.len();
        tree.push(new);
        skip_whitespaces(xpath);
    }
    Ok(left)
}

/// ```text
/// [22] AndExpr ::= EqualityExpr | AndExpr 'and' EqualityExpr
/// ```
fn parse_and_expr(
    xpath: &mut &str,
    tree: &mut Vec<XPathSyntaxTree>,
) -> Result<usize, XPathCompileError> {
    let mut left = parse_equality_expr(xpath, tree)?;
    skip_whitespaces(xpath);
    while let Some(rem) = xpath.strip_prefix("and") {
        *xpath = rem;
        let right = parse_equality_expr(xpath, tree)?;
        let new = XPathSyntaxTree::And(left, right);
        left = tree.len();
        tree.push(new);
        skip_whitespaces(xpath);
    }
    Ok(left)
}

/// ```text
/// [23] EqualityExpr ::= RelationalExpr
///                       | EqualityExpr '=' RelationalExpr
///                       | EqualityExpr '!=' RelationalExpr
/// ```
fn parse_equality_expr(
    xpath: &mut &str,
    tree: &mut Vec<XPathSyntaxTree>,
) -> Result<usize, XPathCompileError> {
    let mut left = parse_relation_expr(xpath, tree)?;
    loop {
        skip_whitespaces(xpath);
        let new = if let Some(rem) = xpath.strip_prefix('=') {
            *xpath = rem;
            let right = parse_relation_expr(xpath, tree)?;
            XPathSyntaxTree::Equal(left, right)
        } else if let Some(rem) = xpath.strip_prefix("!=") {
            *xpath = rem;
            let right = parse_relation_expr(xpath, tree)?;
            XPathSyntaxTree::NotEqual(left, right)
        } else {
            break;
        };
        left = tree.len();
        tree.push(new);
    }
    Ok(left)
}

/// ```text
/// [24] RelationalExpr ::= AdditiveExpr
///                         | RelationalExpr '<' AdditiveExpr
///                         | RelationalExpr '>' AdditiveExpr
///                         | RelationalExpr '<=' AdditiveExpr
///                         | RelationalExpr '>=' AdditiveExpr
/// ```
fn parse_relation_expr(
    xpath: &mut &str,
    tree: &mut Vec<XPathSyntaxTree>,
) -> Result<usize, XPathCompileError> {
    let mut left = parse_additive_expr(xpath, tree)?;
    loop {
        skip_whitespaces(xpath);
        let new = if let Some(rem) = xpath.strip_prefix('<') {
            if let Some(rem) = rem.strip_prefix('=') {
                *xpath = rem;
                let right = parse_additive_expr(xpath, tree)?;
                XPathSyntaxTree::LessOrEqual(left, right)
            } else {
                *xpath = rem;
                let right = parse_additive_expr(xpath, tree)?;
                XPathSyntaxTree::Less(left, right)
            }
        } else if let Some(rem) = xpath.strip_prefix('>') {
            if let Some(rem) = rem.strip_prefix('=') {
                *xpath = rem;
                let right = parse_additive_expr(xpath, tree)?;
                XPathSyntaxTree::GreaterOrEqual(left, right)
            } else {
                *xpath = rem;
                let right = parse_additive_expr(xpath, tree)?;
                XPathSyntaxTree::Greater(left, right)
            }
        } else {
            break;
        };
        left = tree.len();
        tree.push(new);
    }
    Ok(left)
}

/// ```text
/// [25] AdditiveExpr ::= MultiplicativeExpr
///                       | AdditiveExpr '+' MultiplicativeExpr
///                       | AdditiveExpr '-' MultiplicativeExpr
/// ```
fn parse_additive_expr(
    xpath: &mut &str,
    tree: &mut Vec<XPathSyntaxTree>,
) -> Result<usize, XPathCompileError> {
    let mut left = parse_multiplicative_expr(xpath, tree)?;
    loop {
        skip_whitespaces(xpath);
        let new = if let Some(rem) = xpath.strip_prefix('+') {
            *xpath = rem;
            let rigth = parse_multiplicative_expr(xpath, tree)?;
            XPathSyntaxTree::Addition(left, rigth)
        } else if let Some(rem) = xpath.strip_prefix('-') {
            *xpath = rem;
            let right = parse_multiplicative_expr(xpath, tree)?;
            XPathSyntaxTree::Subtraction(left, right)
        } else {
            break;
        };
        left = tree.len();
        tree.push(new);
    }
    Ok(left)
}

/// ```text
/// [26] MultiplicativeExpr ::= UnaryExpr
///                             | MultiplicativeExpr MultiplyOperator UnaryExpr
///                             | MultiplicativeExpr 'div' UnaryExpr
///                             | MultiplicativeExpr 'mod' UnaryExpr
/// ```
fn parse_multiplicative_expr(
    xpath: &mut &str,
    tree: &mut Vec<XPathSyntaxTree>,
) -> Result<usize, XPathCompileError> {
    let mut left = parse_unary_expr(xpath, tree)?;
    loop {
        skip_whitespaces(xpath);
        let new = if let Some(rem) = xpath.strip_prefix('*') {
            *xpath = rem;
            let rigth = parse_unary_expr(xpath, tree)?;
            XPathSyntaxTree::Multiplication(left, rigth)
        } else if let Some(rem) = xpath.strip_prefix("div") {
            *xpath = rem;
            let right = parse_unary_expr(xpath, tree)?;
            XPathSyntaxTree::Division(left, right)
        } else if let Some(rem) = xpath.strip_prefix("mod") {
            *xpath = rem;
            let right = parse_unary_expr(xpath, tree)?;
            XPathSyntaxTree::Remainder(left, right)
        } else {
            break;
        };
        left = tree.len();
        tree.push(new);
    }
    Ok(left)
}

/// ```text
/// [27] UnaryExpr ::= UnionExpr | '-' UnaryExpr
/// ```
fn parse_unary_expr(
    xpath: &mut &str,
    tree: &mut Vec<XPathSyntaxTree>,
) -> Result<usize, XPathCompileError> {
    skip_whitespaces(xpath);
    if let Some(rem) = xpath.strip_prefix('-') {
        *xpath = rem;
        let child = parse_unary_expr(xpath, tree)?;
        let new = XPathSyntaxTree::Negation(child);
        let ret = tree.len();
        tree.push(new);
        Ok(ret)
    } else {
        parse_union_expr(xpath, tree)
    }
}

/// ```text
/// [29] Literal ::= '"' [^"]* '"' | "'" [^']* "'"
/// ```
fn parse_literal(
    xpath: &mut &str,
    tree: &mut Vec<XPathSyntaxTree>,
) -> Result<usize, XPathCompileError> {
    skip_whitespaces(xpath);
    let quote = xpath
        .chars()
        .next()
        .filter(|c| matches!(c, '\'' | '"'))
        .ok_or(XPathCompileError::InvalidLiteral)?;
    let (literal, rem) = xpath[1..]
        .split_once(quote)
        .ok_or(XPathCompileError::InvalidLiteral)?;
    *xpath = rem;
    let ret = tree.len();
    tree.push(XPathSyntaxTree::Literal(literal.into()));
    Ok(ret)
}

/// ```text
/// [30] Number ::= Digits ('.' Digits?)? | '.' Digits
/// ```
fn parse_number(
    xpath: &mut &str,
    tree: &mut Vec<XPathSyntaxTree>,
) -> Result<usize, XPathCompileError> {
    skip_whitespaces(xpath);
    if let Some(rem) = xpath.strip_prefix('.') {
        let rem = rem.trim_start_matches(|c: char| c.is_ascii_digit());
        if rem.len() + 1 == xpath.len() {
            return Err(XPathCompileError::InvalidNumber);
        }
        let number = &xpath[..xpath.len() - rem.len()];
        let ret = tree.len();
        tree.push(XPathSyntaxTree::Number(number.into()));
        *xpath = rem;
        Ok(ret)
    } else {
        let rem = xpath.trim_start_matches(|c: char| c.is_ascii_digit());
        if rem.len() == xpath.len() {
            return Err(XPathCompileError::InvalidNumber);
        }

        if let Some(rem) = rem.strip_prefix('.') {
            let rem = rem.trim_start_matches(|c: char| c.is_ascii_digit());
            let number = &xpath[..xpath.len() - rem.len()];
            let ret = tree.len();
            tree.push(XPathSyntaxTree::Number(number.into()));
            *xpath = rem;
            Ok(ret)
        } else {
            let number = &xpath[..xpath.len() - rem.len()];
            let ret = tree.len();
            tree.push(XPathSyntaxTree::Number(number.into()));
            *xpath = rem;
            Ok(ret)
        }
    }
}

/// ```text
/// [35] FunctionName   ::= QName - NodeType
/// [38] NodeType       ::= 'comment'
///                         | 'text'
///                         | 'processing-instruction'
///                         | 'node'
/// ```
fn parse_function_name(xpath: &str) -> Result<(&str, &str), XPathCompileError> {
    let (qname, rem) = parse_qname(xpath)?;
    if matches!(
        qname,
        "comment" | "text" | "processing-instruction" | "node"
    ) {
        return Err(XPathCompileError::InvalidFunctionName);
    }
    Ok((qname, rem))
}

/// ```text
/// [36] VariableReference ::= '$' QName
/// ```
fn parse_variable_reference(
    xpath: &mut &str,
    tree: &mut Vec<XPathSyntaxTree>,
) -> Result<usize, XPathCompileError> {
    skip_whitespaces(xpath);
    let rem = xpath
        .strip_prefix('$')
        .ok_or(XPathCompileError::InvalidVariableReference)?;
    let (qname, rem) = parse_qname(rem)?;
    *xpath = rem;
    let ret = tree.len();
    tree.push(XPathSyntaxTree::VariableReference(qname.into()));
    Ok(ret)
}

/// Try to parse NCName.
///
/// If successfully parsed, return (NCName, remainder string),
/// otherwise return `Err`
fn parse_ncname(xpath: &str) -> Result<(&str, &str), XPathCompileError> {
    let version = XMLVersion::default();
    if !xpath.starts_with(|c: char| c != ':' && version.is_name_start_char(c)) {
        return Err(XPathCompileError::InvalidNCName);
    }

    let pos = xpath
        .find(|c: char| c == ':' || !version.is_name_char(c))
        .unwrap_or(xpath.len());
    Ok(xpath.split_at(pos))
}

/// Try to parse QName.
///
/// If successfully parsed, return (QName, remainder string),
/// otherwise return `Err`
fn parse_qname(xpath: &str) -> Result<(&str, &str), XPathCompileError> {
    let (ncname, rem) = parse_ncname(xpath).map_err(|_| XPathCompileError::InvalidQName)?;
    let Some(rem) = rem.strip_prefix(':') else {
        // unprefixed qualified name
        return Ok((ncname, rem));
    };

    if let Ok((_, rem)) = parse_ncname(rem) {
        Ok(xpath.split_at(xpath.len() - rem.len()))
    } else {
        // Treat a colon as following an unprefixed qualified name
        Ok(xpath.split_at(ncname.len()))
    }
}

fn skip_whitespaces(xpath: &mut &str) {
    const XML_VERSION: XMLVersion = XMLVersion::XML10;
    *xpath = xpath.trim_start_matches(|c| XML_VERSION.is_whitespace(c));
}
