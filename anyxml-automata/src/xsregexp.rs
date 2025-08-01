//! Implement the regular expressions described in
//! [XML Schema Part 2: Datatypes Second Edition Appendix F Regular Expressions](https://www.w3.org/TR/xmlschema-2/#regexs).

use std::{iter::once, sync::LazyLock};

use crate::{
    ast::ASTNode,
    fa::{DFA, assemble},
    unicode::{
        GENERAL_CATEGORY_CC, GENERAL_CATEGORY_CF, GENERAL_CATEGORY_CO, GENERAL_CATEGORY_LL,
        GENERAL_CATEGORY_LM, GENERAL_CATEGORY_LO, GENERAL_CATEGORY_LT, GENERAL_CATEGORY_LU,
        GENERAL_CATEGORY_MC, GENERAL_CATEGORY_ME, GENERAL_CATEGORY_MN, GENERAL_CATEGORY_ND,
        GENERAL_CATEGORY_NL, GENERAL_CATEGORY_NO, GENERAL_CATEGORY_PC, GENERAL_CATEGORY_PD,
        GENERAL_CATEGORY_PE, GENERAL_CATEGORY_PF, GENERAL_CATEGORY_PI, GENERAL_CATEGORY_PO,
        GENERAL_CATEGORY_PS, GENERAL_CATEGORY_SC, GENERAL_CATEGORY_SK, GENERAL_CATEGORY_SM,
        GENERAL_CATEGORY_SO, GENERAL_CATEGORY_ZL, GENERAL_CATEGORY_ZP, GENERAL_CATEGORY_ZS,
        alternate_general_category_nd, iterate_general_category_c, iterate_general_category_l,
        iterate_general_category_m, iterate_general_category_n, iterate_general_category_p,
        iterate_general_category_s, iterate_general_category_z, negate_general_category_nd,
        seach_block_range,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RegexpError {
    SyntaxError,
    TooLargeQuantity,
    InvalidQuantifier,
    InvalidCharacter,
    InvalidCharProp,
    InvalidBlock,
}

impl std::fmt::Display for RegexpError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl std::error::Error for RegexpError {}

pub struct XSRegexp {
    fa: LazyLock<DFA<char>, Box<dyn Fn() -> DFA<char>>>,
}

impl XSRegexp {
    pub fn compile(mut regexp: &str) -> Result<XSRegexp, RegexpError> {
        let ast = parse_regexp(&mut regexp, false)?;
        Ok(Self {
            fa: LazyLock::new(Box::new(move || assemble(ast.as_ref()).unwrap())),
        })
    }
}

fn parse_regexp(regexp: &mut &str, inner: bool) -> Result<Option<ASTNode<char>>, RegexpError> {
    let mut res = parse_branch(regexp)?;
    while let Some(rem) = regexp.strip_prefix('|') {
        *regexp = rem;
        if let Some(right) = parse_branch(regexp)? {
            if let Some(left) = res {
                res = Some(ASTNode::Alternation(Box::new(left), Box::new(right)));
            } else {
                res = Some(right);
            }
        }
    }

    if inner && regexp.starts_with(')') {
        *regexp = &regexp[1..];
        Ok(res)
    } else if !regexp.is_empty() {
        Err(RegexpError::SyntaxError)
    } else {
        Ok(res)
    }
}

/// [2] branch ::= piece*
fn parse_branch(regexp: &mut &str) -> Result<Option<ASTNode<char>>, RegexpError> {
    let mut ret = None;
    while !regexp.starts_with('|') {
        if let Some(right) = parse_piece(regexp)? {
            if let Some(left) = ret {
                ret = Some(ASTNode::Alternation(Box::new(left), Box::new(right)));
            } else {
                ret = Some(right)
            }
        }
    }
    Ok(ret)
}

/// [3] piece ::= atom quantifier?
fn parse_piece(regexp: &mut &str) -> Result<Option<ASTNode<char>>, RegexpError> {
    if let Some(atom) = parse_atom(regexp)? {
        parse_quantifier(regexp, atom).map(Some)
    } else {
        Ok(None)
    }
}

/// [4] quantifier ::= [?*+] | ( '{' quantity '}' )
fn parse_quantifier(regexp: &mut &str, atom: ASTNode<char>) -> Result<ASTNode<char>, RegexpError> {
    match regexp.as_bytes() {
        [b'?', ..] => {
            *regexp = &regexp[1..];
            Ok(ASTNode::ZeroOrOne(Box::new(atom)))
        }
        [b'*', ..] => {
            *regexp = &regexp[1..];
            Ok(ASTNode::ZeroOrMore(Box::new(atom)))
        }
        [b'+', ..] => {
            *regexp = &regexp[1..];
            Ok(ASTNode::OneOrMore(Box::new(atom)))
        }
        [b'{', ..] => {
            *regexp = &regexp[1..];
            match parse_quantity(regexp)? {
                Quantity::QuantRange(at_least, at_most) => Ok(ASTNode::Repeat {
                    node: Box::new(atom),
                    at_least,
                    at_most,
                }),
                Quantity::QuantExact(exact) => Ok(ASTNode::RepeatExact(Box::new(atom), exact)),
            }
        }
        _ => Ok(atom),
    }
}

enum Quantity {
    QuantRange(usize, Option<usize>),
    QuantExact(usize),
}

/// [5] quantity ::= quantRange | quantMin | QuantExact
fn parse_quantity(regexp: &mut &str) -> Result<Quantity, RegexpError> {
    let pos = regexp
        .as_bytes()
        .iter()
        .position(|&c| !c.is_ascii_digit())
        .ok_or(RegexpError::SyntaxError)?;
    let p = regexp[..pos]
        .parse::<usize>()
        .or(Err(RegexpError::TooLargeQuantity))?;
    *regexp = &regexp[pos..];

    if regexp.is_empty() {
        return Err(RegexpError::SyntaxError);
    }

    let head = regexp.as_bytes()[0];
    if head == b'}' {
        Ok(Quantity::QuantExact(p))
    } else if head == b',' {
        *regexp = &regexp[1..];

        if regexp.is_empty() {
            return Err(RegexpError::SyntaxError);
        }

        let head = regexp.as_bytes()[0];
        if head == b'}' {
            Ok(Quantity::QuantRange(p, None))
        } else if head.is_ascii_digit() {
            let pos = regexp
                .as_bytes()
                .iter()
                .position(|&c| !c.is_ascii_digit())
                .ok_or(RegexpError::SyntaxError)?;
            let q = regexp[..pos]
                .parse::<usize>()
                .or(Err(RegexpError::TooLargeQuantity))?;

            if p > q {
                Err(RegexpError::InvalidQuantifier)
            } else {
                Ok(Quantity::QuantRange(p, Some(q)))
            }
        } else {
            Err(RegexpError::SyntaxError)
        }
    } else {
        Err(RegexpError::SyntaxError)
    }
}

/// [9] atom ::= Char | charClass | ( '(' regExp ')' )
fn parse_atom(regexp: &mut &str) -> Result<Option<ASTNode<char>>, RegexpError> {
    if regexp.is_empty() {
        return Err(RegexpError::SyntaxError);
    }

    match regexp.as_bytes() {
        [b'(', ..] => {
            // for 'regExp'
            *regexp = &regexp[1..];
            parse_regexp(regexp, true)
        }
        // [11] charClass         ::= charClassEsc | charClassExpr | WildcardEsc
        // [23] charClassEsc      ::= ( SingleCharEsc | MultiCharEsc | catEsc | complEsc )
        // [25] catEsc        ::= '\p{' charProp '}'
        [b'\\', b'p', ..] => parse_cat_esc(regexp).map(Some),
        // [26] complEsc      ::= '\P{' charProp '}'
        [b'\\', b'P', ..] => parse_compl_esc(regexp).map(Some),
        // [37] MultiCharEsc  ::= '\' [sSiIcCdDwW]
        [
            b'\\',
            b's' | b'S' | b'i' | b'I' | b'c' | b'C' | b'd' | b'D' | b'w' | b'W',
            ..,
        ] => parse_multi_char_esc(regexp).map(Some),
        // [24] SingleCharEsc ::= '\' [nrt\|.?*+(){}#x2D#x5B#x5D#x5E]
        [
            b'\\',
            b'n' | b'r' | b't' | b'\\' | b'|' | b'.' | b'?' | b'*' | b'+' | b'(' | b')' | b'{'
            | b'}' | b'\x2D' | b'\x5B' | b'\x5D' | b'\x5E',
            ..,
        ] => parse_single_char_esc(regexp, false).map(Some),
        // [12] charClassExpr ::= '[' charGroup ']'
        [b'[', ..] => {
            todo!()
        }
        // [37a] WildcardEsc  ::= '.'
        [b'.', ..] => {
            *regexp = &regexp[1..];
            Ok(ASTNode::negate_all(
                [('\n', '\n'), ('\r', '\r')].into_iter(),
            ))
        }
        // [10] Char          ::= [^.\?*+()|#x5B#x5D]
        _ => {
            todo!()
        }
    }
}

/// [13] charGroup ::= posCharGroup | negCharGroup | charClassSub
fn parse_char_group(regexp: &mut &str) -> Result<ASTNode<char>, RegexpError> {
    let mut negation = false;
    if let Some(rem) = regexp.strip_prefix('^') {
        *regexp = rem;
        negation = true;
    }

    todo!()
}

/// [14] posCharGroup ::= ( charRange | charClassEsc )+
/// [15] negCharGroup ::= '^' posCharGroup
fn parse_pos_char_group(regexp: &mut &str) -> Result<ASTNode<char>, RegexpError> {
    todo!()
}

/// [24] SingleCharEsc ::= '\' [nrt\|.?*+(){}#x2D#x5B#x5D#x5E] /* N.B.:  #x2D = '-', #x5B = '[', #x5D = ']', #x5E = '^' */
fn parse_single_char_esc(regexp: &mut &str, negation: bool) -> Result<ASTNode<char>, RegexpError> {
    if regexp.starts_with('\\') {
        return Err(RegexpError::SyntaxError);
    }
    *regexp = &regexp[1..];
    if regexp.is_empty() {
        return Err(RegexpError::SyntaxError);
    }
    let c = regexp.as_bytes()[0];
    let c = match c {
        b'n' => '\n',
        b'r' => '\r',
        b't' => '\t',
        c @ (b'\\' | b'|' | b'.' | b'?' | b'*' | b'+' | b'(' | b')' | b'{' | b'}' | b'-' | b'['
        | b']' | b'^') => c as char,
        _ => return Err(RegexpError::InvalidCharacter),
    };
    *regexp = &regexp[1..];
    Ok(ASTNode::Charcters {
        start: c,
        end: c,
        negation,
    })
}

/// [25] catEsc ::= '\p{' charProp '}'
fn parse_cat_esc(regexp: &mut &str) -> Result<ASTNode<char>, RegexpError> {
    if !regexp.starts_with("\\p{") {
        return Err(RegexpError::SyntaxError);
    }
    *regexp = &regexp[2..];
    let ret = parse_char_prop(regexp, false)?;
    if !regexp.starts_with("}") {
        return Err(RegexpError::SyntaxError);
    }
    *regexp = &regexp[1..];
    Ok(ret)
}

/// [26] complEsc ::= '\P{' charProp '}'
fn parse_compl_esc(regexp: &mut &str) -> Result<ASTNode<char>, RegexpError> {
    if !regexp.starts_with("\\P{") {
        return Err(RegexpError::SyntaxError);
    }
    *regexp = &regexp[2..];
    let ret = parse_char_prop(regexp, true)?;
    if !regexp.starts_with("}") {
        return Err(RegexpError::SyntaxError);
    }
    *regexp = &regexp[1..];
    Ok(ret)
}

/// [27] charProp ::= IsCategory | IsBlock
fn parse_char_prop(regexp: &mut &str, negation: bool) -> Result<ASTNode<char>, RegexpError> {
    if regexp.is_empty() {
        return Err(RegexpError::SyntaxError);
    }

    fn gen_ast(iter: impl Iterator<Item = (char, char)>, negation: bool) -> Option<ASTNode<char>> {
        if negation {
            ASTNode::negate_all(iter)
        } else {
            ASTNode::alternate_all(iter)
        }
    }

    let mut trim = 2;
    let ret = match regexp.as_bytes() {
        [b'L', b'u', ..] => Ok(gen_ast(GENERAL_CATEGORY_LU.iter().copied(), negation).unwrap()),
        [b'L', b'l', ..] => Ok(gen_ast(GENERAL_CATEGORY_LL.iter().copied(), negation).unwrap()),
        [b'L', b't', ..] => Ok(gen_ast(GENERAL_CATEGORY_LT.iter().copied(), negation).unwrap()),
        [b'L', b'm', ..] => Ok(gen_ast(GENERAL_CATEGORY_LM.iter().copied(), negation).unwrap()),
        [b'L', b'o', ..] => Ok(gen_ast(GENERAL_CATEGORY_LO.iter().copied(), negation).unwrap()),
        [b'L', ..] => {
            trim = 1;
            Ok(gen_ast(iterate_general_category_l(), negation).unwrap())
        }
        [b'M', b'n', ..] => Ok(gen_ast(GENERAL_CATEGORY_MN.iter().copied(), negation).unwrap()),
        [b'M', b'c', ..] => Ok(gen_ast(GENERAL_CATEGORY_MC.iter().copied(), negation).unwrap()),
        [b'M', b'e', ..] => Ok(gen_ast(GENERAL_CATEGORY_ME.iter().copied(), negation).unwrap()),
        [b'M', ..] => {
            trim = 1;
            Ok(gen_ast(iterate_general_category_m(), negation).unwrap())
        }
        [b'N', b'd', ..] => Ok(gen_ast(GENERAL_CATEGORY_ND.iter().copied(), negation).unwrap()),
        [b'N', b'l', ..] => Ok(gen_ast(GENERAL_CATEGORY_NL.iter().copied(), negation).unwrap()),
        [b'N', b'o', ..] => Ok(gen_ast(GENERAL_CATEGORY_NO.iter().copied(), negation).unwrap()),
        [b'N', ..] => {
            trim = 1;
            Ok(gen_ast(iterate_general_category_n(), negation).unwrap())
        }
        [b'P', b'c', ..] => Ok(gen_ast(GENERAL_CATEGORY_PC.iter().copied(), negation).unwrap()),
        [b'P', b'd', ..] => Ok(gen_ast(GENERAL_CATEGORY_PD.iter().copied(), negation).unwrap()),
        [b'P', b's', ..] => Ok(gen_ast(GENERAL_CATEGORY_PS.iter().copied(), negation).unwrap()),
        [b'P', b'e', ..] => Ok(gen_ast(GENERAL_CATEGORY_PE.iter().copied(), negation).unwrap()),
        [b'P', b'i', ..] => Ok(gen_ast(GENERAL_CATEGORY_PI.iter().copied(), negation).unwrap()),
        [b'P', b'f', ..] => Ok(gen_ast(GENERAL_CATEGORY_PF.iter().copied(), negation).unwrap()),
        [b'P', b'o', ..] => Ok(gen_ast(GENERAL_CATEGORY_PO.iter().copied(), negation).unwrap()),
        [b'P', ..] => {
            trim = 1;
            Ok(gen_ast(iterate_general_category_p(), negation).unwrap())
        }
        [b'Z', b's', ..] => Ok(gen_ast(GENERAL_CATEGORY_ZS.iter().copied(), negation).unwrap()),
        [b'Z', b'l', ..] => Ok(gen_ast(GENERAL_CATEGORY_ZL.iter().copied(), negation).unwrap()),
        [b'Z', b'p', ..] => Ok(gen_ast(GENERAL_CATEGORY_ZP.iter().copied(), negation).unwrap()),
        [b'Z', ..] => {
            trim = 1;
            Ok(gen_ast(iterate_general_category_z(), negation).unwrap())
        }
        [b'S', b'm', ..] => Ok(gen_ast(GENERAL_CATEGORY_SM.iter().copied(), negation).unwrap()),
        [b'S', b'c', ..] => Ok(gen_ast(GENERAL_CATEGORY_SC.iter().copied(), negation).unwrap()),
        [b'S', b'k', ..] => Ok(gen_ast(GENERAL_CATEGORY_SK.iter().copied(), negation).unwrap()),
        [b'S', b'o', ..] => Ok(gen_ast(GENERAL_CATEGORY_SO.iter().copied(), negation).unwrap()),
        [b'S', ..] => {
            trim = 1;
            Ok(gen_ast(iterate_general_category_s(), negation).unwrap())
        }
        [b'C', b'c', ..] => Ok(gen_ast(GENERAL_CATEGORY_CC.iter().copied(), negation).unwrap()),
        [b'C', b'f', ..] => Ok(gen_ast(GENERAL_CATEGORY_CF.iter().copied(), negation).unwrap()),
        [b'C', b'o', ..] => Ok(gen_ast(GENERAL_CATEGORY_CO.iter().copied(), negation).unwrap()),
        // [b'C', b'n', ..] => Ok(gen_ast(GENERAL_CATEGORY_CN.iter().copied(), negation).unwrap()),
        [b'C', ..] => {
            trim = 1;
            Ok(gen_ast(iterate_general_category_c(), negation).unwrap())
        }
        [b'I', b's', ..] => {
            while trim < regexp.len()
                && matches!(regexp.as_bytes()[trim], b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'\x2D')
            {
                trim += 1;
            }
            let Some((start, end)) = seach_block_range(&regexp[2..trim]) else {
                return Err(RegexpError::InvalidBlock);
            };
            Ok(gen_ast(once((start, end)), negation).unwrap())
        }
        _ => return Err(RegexpError::InvalidCharProp),
    };

    *regexp = &regexp[trim..];
    ret
}

/// [37] MultiCharEsc  ::= '\' [sSiIcCdDwW]
fn parse_multi_char_esc(regexp: &mut &str) -> Result<ASTNode<char>, RegexpError> {
    if regexp.starts_with('\\') {
        return Err(RegexpError::SyntaxError);
    }
    *regexp = &regexp[1..];
    if regexp.is_empty() {
        return Err(RegexpError::SyntaxError);
    }
    let res = match regexp.as_bytes()[0] {
        b's' => Ok(
            ASTNode::alternate_all([('\t', '\t'), ('\n', '\n'), ('\r', '\r')].into_iter()).unwrap(),
        ),
        b'S' => Ok(
            ASTNode::negate_all([('\t', '\t'), ('\n', '\n'), ('\r', '\r')].into_iter()).unwrap(),
        ),
        b'i' => Ok(ASTNode::alternate_all(
            [
                (':', ':'),
                ('A', 'Z'),
                ('_', '_'),
                ('a', 'z'),
                ('\u{C0}', '\u{D6}'),
                ('\u{D8}', '\u{F6}'),
                ('\u{C0}', '\u{D6}'),
                ('\u{D8}', '\u{F6}'),
                ('\u{F8}', '\u{2FF}'),
                ('\u{370}', '\u{37D}'),
                ('\u{37F}', '\u{1FFF}'),
                ('\u{200C}', '\u{200D}'),
                ('\u{2070}', '\u{218F}'),
                ('\u{2C00}', '\u{2FEF}'),
                ('\u{3001}', '\u{D7FF}'),
                ('\u{F900}', '\u{FDCF}'),
                ('\u{FDF0}', '\u{FFFD}'),
                ('\u{10000}', '\u{EFFFF}'),
            ]
            .into_iter(),
        )
        .unwrap()),
        b'I' => Ok(ASTNode::negate_all(
            [
                (':', ':'),
                ('A', 'Z'),
                ('_', '_'),
                ('a', 'z'),
                ('\u{C0}', '\u{D6}'),
                ('\u{D8}', '\u{F6}'),
                ('\u{C0}', '\u{D6}'),
                ('\u{D8}', '\u{F6}'),
                ('\u{F8}', '\u{2FF}'),
                ('\u{370}', '\u{37D}'),
                ('\u{37F}', '\u{1FFF}'),
                ('\u{200C}', '\u{200D}'),
                ('\u{2070}', '\u{218F}'),
                ('\u{2C00}', '\u{2FEF}'),
                ('\u{3001}', '\u{D7FF}'),
                ('\u{F900}', '\u{FDCF}'),
                ('\u{FDF0}', '\u{FFFD}'),
                ('\u{10000}', '\u{EFFFF}'),
            ]
            .into_iter(),
        )
        .unwrap()),
        b'c' => Ok(ASTNode::alternate_all(
            [
                ('-', '.'),
                ('0', '9'),
                (':', ':'),
                ('A', 'Z'),
                ('_', '_'),
                ('a', 'z'),
                ('\u{B7}', '\u{B7}'),
                ('\u{C0}', '\u{D6}'),
                ('\u{D8}', '\u{F6}'),
                ('\u{C0}', '\u{D6}'),
                ('\u{D8}', '\u{F6}'),
                ('\u{F8}', '\u{2FF}'),
                ('\u{300}', '\u{37D}'),
                ('\u{37F}', '\u{1FFF}'),
                ('\u{200C}', '\u{200D}'),
                ('\u{203F}', '\u{2040}'),
                ('\u{2070}', '\u{218F}'),
                ('\u{2C00}', '\u{2FEF}'),
                ('\u{3001}', '\u{D7FF}'),
                ('\u{F900}', '\u{FDCF}'),
                ('\u{FDF0}', '\u{FFFD}'),
                ('\u{10000}', '\u{EFFFF}'),
            ]
            .into_iter(),
        )
        .unwrap()),
        b'C' => Ok(ASTNode::negate_all(
            [
                ('-', '.'),
                ('0', '9'),
                (':', ':'),
                ('A', 'Z'),
                ('_', '_'),
                ('a', 'z'),
                ('\u{B7}', '\u{B7}'),
                ('\u{C0}', '\u{D6}'),
                ('\u{D8}', '\u{F6}'),
                ('\u{C0}', '\u{D6}'),
                ('\u{D8}', '\u{F6}'),
                ('\u{F8}', '\u{2FF}'),
                ('\u{300}', '\u{37D}'),
                ('\u{37F}', '\u{1FFF}'),
                ('\u{200C}', '\u{200D}'),
                ('\u{203F}', '\u{2040}'),
                ('\u{2070}', '\u{218F}'),
                ('\u{2C00}', '\u{2FEF}'),
                ('\u{3001}', '\u{D7FF}'),
                ('\u{F900}', '\u{FDCF}'),
                ('\u{FDF0}', '\u{FFFD}'),
                ('\u{10000}', '\u{EFFFF}'),
            ]
            .into_iter(),
        )
        .unwrap()),
        b'd' => Ok(alternate_general_category_nd()),
        b'D' => Ok(negate_general_category_nd()),
        b'w' => Ok(ASTNode::negate_all(
            iterate_general_category_p()
                .chain(iterate_general_category_z().chain(iterate_general_category_c())),
        )
        .unwrap()),
        b'W' => Ok(ASTNode::alternate_all(
            iterate_general_category_p()
                .chain(iterate_general_category_z().chain(iterate_general_category_c())),
        )
        .unwrap()),
        _ => Err(RegexpError::SyntaxError),
    };

    if res.is_ok() {
        *regexp = &regexp[1..];
    }
    res
}
