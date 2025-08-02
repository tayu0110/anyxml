//! Implement the regular expressions described in
//! [XML Schema Part 2: Datatypes Second Edition Appendix F Regular Expressions](https://www.w3.org/TR/xmlschema-2/#regexs).

use std::{
    iter::{empty, once},
    sync::LazyLock,
};

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
        iterate_general_category_c, iterate_general_category_l, iterate_general_category_m,
        iterate_general_category_n, iterate_general_category_p, iterate_general_category_s,
        iterate_general_category_z, seach_block_range,
    },
    util::{complement_ranges, difference_ranges, union_ranges},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RegexpError {
    SyntaxError,
    TooLargeQuantity,
    InvalidQuantifier,
    InvalidCharacter,
    InvalidCharRange,
    InvalidCharProp,
    InvalidBlock,
}

impl std::fmt::Display for RegexpError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl std::error::Error for RegexpError {}

#[derive(Debug)]
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

    pub fn is_match(&self, input: &str) -> bool {
        self.fa.is_match(input.chars())
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
    while !regexp.starts_with([')', '|']) && !regexp.is_empty() {
        if let Some(right) = parse_piece(regexp)? {
            if let Some(left) = ret {
                ret = Some(ASTNode::Catenation(Box::new(left), Box::new(right)));
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
        [b'\\', b'p', ..] => parse_cat_esc(regexp).map(ASTNode::alternate_all),
        // [26] complEsc      ::= '\P{' charProp '}'
        [b'\\', b'P', ..] => parse_compl_esc(regexp).map(ASTNode::alternate_all),
        // [37] MultiCharEsc  ::= '\' [sSiIcCdDwW]
        [
            b'\\',
            b's' | b'S' | b'i' | b'I' | b'c' | b'C' | b'd' | b'D' | b'w' | b'W',
            ..,
        ] => parse_multi_char_esc(regexp).map(ASTNode::alternate_all),
        // [24] SingleCharEsc ::= '\' [nrt\|.?*+(){}#x2D#x5B#x5D#x5E]
        [
            b'\\',
            b'n' | b'r' | b't' | b'\\' | b'|' | b'.' | b'?' | b'*' | b'+' | b'(' | b')' | b'{'
            | b'}' | b'\x2D' | b'\x5B' | b'\x5D' | b'\x5E',
            ..,
        ] => parse_single_char_esc(regexp).map(ASTNode::alternate_all),
        // [12] charClassExpr ::= '[' charGroup ']'
        [b'[', ..] => {
            *regexp = &regexp[1..];
            let ret = parse_char_group(regexp).map(ASTNode::alternate_all);
            if !regexp.starts_with(']') {
                return Err(RegexpError::SyntaxError);
            }
            *regexp = &regexp[1..];
            ret
        }
        // [37a] WildcardEsc  ::= '.'
        [b'.', ..] => {
            *regexp = &regexp[1..];
            Ok(ASTNode::negate_all(
                [('\n', '\n'), ('\r', '\r')].into_iter(),
            ))
        }
        // [10] Char          ::= [^.\?*+()|#x5B#x5D]
        [c, ..] => {
            if matches!(
                *c,
                b'.' | b'\\' | b'?' | b'*' | b'+' | b'(' | b')' | b'|' | b'\x5B' | b'\x5D'
            ) {
                return Err(RegexpError::InvalidCharacter);
            }
            let c = regexp.chars().next().unwrap();
            *regexp = &regexp[c.len_utf8()..];
            Ok(Some(ASTNode::Charcters {
                start: c,
                end: c,
                negation: false,
            }))
        }
        [] => Ok(None),
    }
}

/// [13] charGroup ::= posCharGroup | negCharGroup | charClassSub
fn parse_char_group(
    regexp: &mut &str,
) -> Result<Box<dyn Iterator<Item = (char, char)>>, RegexpError> {
    let mut negation = false;
    if let Some(rem) = regexp.strip_prefix('^') {
        *regexp = rem;
        negation = true;
    }

    let mut pos_char_group = parse_pos_char_group(regexp)?;
    if negation {
        pos_char_group = Box::new(complement_ranges(pos_char_group));
    }
    let ret = if regexp.starts_with("-[") {
        *regexp = &regexp[2..];
        let sub = parse_char_group(regexp)?;
        if !regexp.starts_with(']') {
            return Err(RegexpError::SyntaxError);
        }
        *regexp = &regexp[1..];
        Box::new(difference_ranges(pos_char_group, sub))
    } else {
        pos_char_group
    };
    Ok(ret)
}

/// [14] posCharGroup ::= ( charRange | charClassEsc )+
/// [15] negCharGroup ::= '^' posCharGroup
fn parse_pos_char_group(
    regexp: &mut &str,
) -> Result<Box<dyn Iterator<Item = (char, char)>>, RegexpError> {
    if regexp.is_empty() {
        return Err(RegexpError::SyntaxError);
    }

    fn parse_pos_char_group_once(
        regexp: &mut &str,
        hypen: &mut bool,
    ) -> Result<Box<dyn Iterator<Item = (char, char)>>, RegexpError> {
        if regexp.starts_with("-") {
            // A hyphen is only allowed at this position in XmlCharIncDash.
            //
            // [17] charRange      ::= seRange | XmlCharIncDash
            // [22] XmlCharIncDash ::= [^\#x5B#x5D]
            *regexp = &regexp[1..];
            *hypen = true;
            return Ok(Box::new(once(('-', '-'))));
        }

        let start = if regexp.starts_with('\\') {
            if regexp.len() < 2 {
                return Err(RegexpError::SyntaxError);
            }
            // If the first character is SingleCharEsc, it is necessary
            // to determine whether it is charRange or charClassEsc.
            if matches!(
                regexp.as_bytes()[1],
                b'n' | b'r'
                    | b't'
                    | b'\\'
                    | b'|'
                    | b'.'
                    | b'?'
                    | b'*'
                    | b'+'
                    | b'('
                    | b')'
                    | b'{'
                    | b'}'
                    | b'\x2D'
                    | b'\x5B'
                    | b'\x5D'
                    | b'\x5E'
            ) {
                parse_single_char_esc(regexp).unwrap().next().unwrap().0
            } else {
                return parse_char_class_esc(regexp);
            }
        } else {
            let start = regexp.chars().next().unwrap();
            *regexp = &regexp[start.len_utf8()..];
            if matches!(start, '\x2D' | '\x5B' | '\x5D') {
                return Err(RegexpError::InvalidCharacter);
            }
            start
        };
        if !regexp.starts_with('-') || regexp.starts_with("-[") {
            // If there are no consecutive a hyphen, this is XmlCharIncDash or charClassEsc.
            // If “-” continues, character class subtraction begins,
            // so it is still XmlCharIncDash or charClassEsc.
            return Ok(Box::new(once((start, start))));
        }
        *regexp = &regexp[1..];

        if regexp.is_empty() {
            return Err(RegexpError::SyntaxError);
        }

        // If there are consecutive a hyphen, this is seRange.
        // Furthermore, if there are consecutive backslashes, it is SingleCharEsc.
        let end = if regexp.starts_with('\\') {
            parse_single_char_esc(regexp)?.next().unwrap().0
        } else {
            let end = regexp.chars().next().unwrap();
            *regexp = &regexp[end.len_utf8()..];
            if matches!(end, '\x2D' | '\x5B' | '\x5D') {
                return Err(RegexpError::InvalidCharacter);
            }
            end
        };

        if start > end {
            return Err(RegexpError::InvalidCharRange);
        }
        Ok(Box::new(once((start, end))))
    }

    let mut res = Box::new(empty()) as Box<dyn Iterator<Item = (char, char)>>;
    let mut hypen = false;
    let mut init = false;
    let mut last_hyphen = false;
    while !regexp.starts_with("]") && !regexp.starts_with("-[") {
        res = Box::new(union_ranges(
            res,
            parse_pos_char_group_once(regexp, &mut hypen)?,
        ));
        if last_hyphen {
            // If the hyphen that should be at the end is already present, it is incorrect.
            return Err(RegexpError::SyntaxError);
        }
        if init && hypen {
            // If there is a hyphen at any point other than the start of a loop,
            // it must be the last hyphen.
            last_hyphen = true;
        }
        hypen = false;
        init = true;
    }

    Ok(res)
}

fn parse_char_class_esc<'a>(
    regexp: &mut &str,
) -> Result<Box<dyn Iterator<Item = (char, char)> + 'a>, RegexpError> {
    match regexp.as_bytes() {
        // [25] catEsc        ::= '\p{' charProp '}'
        [b'\\', b'p', ..] => parse_cat_esc(regexp).map(|iter| Box::new(iter) as _),
        // [26] complEsc      ::= '\P{' charProp '}'
        [b'\\', b'P', ..] => parse_compl_esc(regexp).map(|iter| Box::new(iter) as _),
        // [37] MultiCharEsc  ::= '\' [sSiIcCdDwW]
        [
            b'\\',
            b's' | b'S' | b'i' | b'I' | b'c' | b'C' | b'd' | b'D' | b'w' | b'W',
            ..,
        ] => parse_multi_char_esc(regexp),
        // [24] SingleCharEsc ::= '\' [nrt\|.?*+(){}#x2D#x5B#x5D#x5E]
        [
            b'\\',
            b'n' | b'r' | b't' | b'\\' | b'|' | b'.' | b'?' | b'*' | b'+' | b'(' | b')' | b'{'
            | b'}' | b'\x2D' | b'\x5B' | b'\x5D' | b'\x5E',
            ..,
        ] => parse_single_char_esc(regexp).map(|iter| Box::new(iter) as _),
        _ => Err(RegexpError::SyntaxError),
    }
}

/// [24] SingleCharEsc ::= '\' [nrt\|.?*+(){}#x2D#x5B#x5D#x5E] /* N.B.:  #x2D = '-', #x5B = '[', #x5D = ']', #x5E = '^' */
fn parse_single_char_esc<'a>(
    regexp: &mut &str,
) -> Result<impl Iterator<Item = (char, char)> + 'a, RegexpError> {
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
    Ok(once((c, c)))
}

/// [25] catEsc ::= '\p{' charProp '}'
fn parse_cat_esc<'a>(
    regexp: &mut &str,
) -> Result<impl Iterator<Item = (char, char)> + 'a, RegexpError> {
    if !regexp.starts_with("\\p{") {
        return Err(RegexpError::SyntaxError);
    }
    *regexp = &regexp[2..];
    let ret = parse_char_prop(regexp)?;
    if !regexp.starts_with("}") {
        return Err(RegexpError::SyntaxError);
    }
    *regexp = &regexp[1..];
    Ok(ret)
}

/// [26] complEsc ::= '\P{' charProp '}'
fn parse_compl_esc<'a>(
    regexp: &mut &str,
) -> Result<impl Iterator<Item = (char, char)> + 'a, RegexpError> {
    if !regexp.starts_with("\\P{") {
        return Err(RegexpError::SyntaxError);
    }
    *regexp = &regexp[2..];
    let ret = complement_ranges(parse_char_prop(regexp)?);
    if !regexp.starts_with("}") {
        return Err(RegexpError::SyntaxError);
    }
    *regexp = &regexp[1..];
    Ok(ret)
}

/// [27] charProp ::= IsCategory | IsBlock
fn parse_char_prop(
    regexp: &mut &str,
) -> Result<Box<dyn Iterator<Item = (char, char)>>, RegexpError> {
    if regexp.is_empty() {
        return Err(RegexpError::SyntaxError);
    }

    let mut trim = 2;
    let ret: Result<Box<dyn Iterator<Item = (char, char)>>, RegexpError> = match regexp.as_bytes() {
        [b'L', b'u', ..] => Ok(Box::new(GENERAL_CATEGORY_LU.iter().copied())),
        [b'L', b'l', ..] => Ok(Box::new(GENERAL_CATEGORY_LL.iter().copied())),
        [b'L', b't', ..] => Ok(Box::new(GENERAL_CATEGORY_LT.iter().copied())),
        [b'L', b'm', ..] => Ok(Box::new(GENERAL_CATEGORY_LM.iter().copied())),
        [b'L', b'o', ..] => Ok(Box::new(GENERAL_CATEGORY_LO.iter().copied())),
        [b'L', ..] => {
            trim = 1;
            Ok(Box::new(iterate_general_category_l()))
        }
        [b'M', b'n', ..] => Ok(Box::new(GENERAL_CATEGORY_MN.iter().copied())),
        [b'M', b'c', ..] => Ok(Box::new(GENERAL_CATEGORY_MC.iter().copied())),
        [b'M', b'e', ..] => Ok(Box::new(GENERAL_CATEGORY_ME.iter().copied())),
        [b'M', ..] => {
            trim = 1;
            Ok(Box::new(iterate_general_category_m()))
        }
        [b'N', b'd', ..] => Ok(Box::new(GENERAL_CATEGORY_ND.iter().copied())),
        [b'N', b'l', ..] => Ok(Box::new(GENERAL_CATEGORY_NL.iter().copied())),
        [b'N', b'o', ..] => Ok(Box::new(GENERAL_CATEGORY_NO.iter().copied())),
        [b'N', ..] => {
            trim = 1;
            Ok(Box::new(iterate_general_category_n()))
        }
        [b'P', b'c', ..] => Ok(Box::new(GENERAL_CATEGORY_PC.iter().copied())),
        [b'P', b'd', ..] => Ok(Box::new(GENERAL_CATEGORY_PD.iter().copied())),
        [b'P', b's', ..] => Ok(Box::new(GENERAL_CATEGORY_PS.iter().copied())),
        [b'P', b'e', ..] => Ok(Box::new(GENERAL_CATEGORY_PE.iter().copied())),
        [b'P', b'i', ..] => Ok(Box::new(GENERAL_CATEGORY_PI.iter().copied())),
        [b'P', b'f', ..] => Ok(Box::new(GENERAL_CATEGORY_PF.iter().copied())),
        [b'P', b'o', ..] => Ok(Box::new(GENERAL_CATEGORY_PO.iter().copied())),
        [b'P', ..] => {
            trim = 1;
            Ok(Box::new(iterate_general_category_p()))
        }
        [b'Z', b's', ..] => Ok(Box::new(GENERAL_CATEGORY_ZS.iter().copied())),
        [b'Z', b'l', ..] => Ok(Box::new(GENERAL_CATEGORY_ZL.iter().copied())),
        [b'Z', b'p', ..] => Ok(Box::new(GENERAL_CATEGORY_ZP.iter().copied())),
        [b'Z', ..] => {
            trim = 1;
            Ok(Box::new(iterate_general_category_z()))
        }
        [b'S', b'm', ..] => Ok(Box::new(GENERAL_CATEGORY_SM.iter().copied())),
        [b'S', b'c', ..] => Ok(Box::new(GENERAL_CATEGORY_SC.iter().copied())),
        [b'S', b'k', ..] => Ok(Box::new(GENERAL_CATEGORY_SK.iter().copied())),
        [b'S', b'o', ..] => Ok(Box::new(GENERAL_CATEGORY_SO.iter().copied())),
        [b'S', ..] => {
            trim = 1;
            Ok(Box::new(iterate_general_category_s()))
        }
        [b'C', b'c', ..] => Ok(Box::new(GENERAL_CATEGORY_CC.iter().copied())),
        [b'C', b'f', ..] => Ok(Box::new(GENERAL_CATEGORY_CF.iter().copied())),
        [b'C', b'o', ..] => Ok(Box::new(GENERAL_CATEGORY_CO.iter().copied())),
        // [b'C', b'n', ..] => Ok(GENERAL_CATEGORY_CN.iter().copied()),
        [b'C', ..] => {
            trim = 1;
            Ok(Box::new(iterate_general_category_c()))
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
            Ok(Box::new(once((start, end))))
        }
        _ => return Err(RegexpError::InvalidCharProp),
    };

    *regexp = &regexp[trim..];
    ret
}

/// [37] MultiCharEsc  ::= '\' [sSiIcCdDwW]
fn parse_multi_char_esc(
    regexp: &mut &str,
) -> Result<Box<dyn Iterator<Item = (char, char)>>, RegexpError> {
    if regexp.starts_with('\\') {
        return Err(RegexpError::SyntaxError);
    }
    *regexp = &regexp[1..];
    if regexp.is_empty() {
        return Err(RegexpError::SyntaxError);
    }
    let res: Result<Box<dyn Iterator<Item = (char, char)>>, RegexpError> =
        match regexp.as_bytes()[0] {
            c @ (b's' | b'S') => {
                let arr = [('\t', '\t'), ('\n', '\n'), ('\r', '\r')];
                if c.is_ascii_lowercase() {
                    Ok(Box::new(arr.into_iter()))
                } else {
                    Ok(Box::new(complement_ranges(arr.into_iter())))
                }
            }
            c @ (b'i' | b'I') => {
                let arr = [
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
                ];
                if c.is_ascii_lowercase() {
                    Ok(Box::new(arr.into_iter()))
                } else {
                    Ok(Box::new(complement_ranges(arr.into_iter())))
                }
            }
            c @ (b'c' | b'C') => {
                let arr = [
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
                ];
                if c.is_ascii_lowercase() {
                    Ok(Box::new(arr.into_iter()))
                } else {
                    Ok(Box::new(complement_ranges(arr.into_iter())))
                }
            }
            b'd' => Ok(Box::new(GENERAL_CATEGORY_ND.iter().copied())),
            b'D' => Ok(Box::new(complement_ranges(
                GENERAL_CATEGORY_ND.iter().copied(),
            ))),
            c @ (b'w' | b'W') => {
                let iter = iterate_general_category_p()
                    .chain(iterate_general_category_z().chain(iterate_general_category_c()));
                if c.is_ascii_lowercase() {
                    Ok(Box::new(iter))
                } else {
                    Ok(Box::new(complement_ranges(iter)))
                }
            }
            _ => Err(RegexpError::SyntaxError),
        };

    if res.is_ok() {
        *regexp = &regexp[1..];
    }
    res
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn regex_parse_tests() {
        let ast = parse_regexp(&mut "a", false).unwrap().unwrap();
        assert_eq!(format!("{ast}"), "a");

        let ast = parse_regexp(&mut "aa", false).unwrap().unwrap();
        assert_eq!(format!("{ast}"), "aa");

        // TODO: It is difficult to output correctly without adding `Group` to AST.
        // let mut test = "a+(c|b?)c";
        // let ast = parse_regexp(&mut test, false);
        // assert_eq!(test, "");
        // let ast = ast.unwrap().unwrap();
        // assert_eq!(format!("{ast}"), "a+(c|b?)c");
    }

    #[test]
    fn regex_matching_tests() {
        let re = XSRegexp::compile("").unwrap();
        assert!(re.is_match(""));
        assert!(!re.is_match("   "));
        assert!(!re.is_match("a"));

        let re = XSRegexp::compile("a").unwrap();
        assert!(re.is_match("a"));
        assert!(!re.is_match(""));
        assert!(!re.is_match("   "));
        assert!(!re.is_match("aa"));
        assert!(!re.is_match("A"));
        assert!(!re.is_match("b"));

        let re = XSRegexp::compile("aa").unwrap();
        assert!(re.is_match("aa"));
        assert!(!re.is_match("a"));
        assert!(!re.is_match("aaa"));
        assert!(!re.is_match(""));
        assert!(!re.is_match("   "));
        assert!(!re.is_match("AA"));
        assert!(!re.is_match("b"));

        let re = XSRegexp::compile("ab").unwrap();
        assert!(re.is_match("ab"));
        assert!(!re.is_match("aa"));
        assert!(!re.is_match("a"));
        assert!(!re.is_match("b"));
        assert!(!re.is_match(""));
        assert!(!re.is_match(" ab "));
        assert!(!re.is_match("AB"));

        let re = XSRegexp::compile("a*").unwrap();
        assert!(re.is_match(""));
        assert!(re.is_match("a"));
        assert!(re.is_match("aa"));
        assert!(re.is_match("aaa"));
        assert!(!re.is_match("ab"));
        assert!(!re.is_match("b"));
        assert!(!re.is_match(" aaa"));
        assert!(!re.is_match("aaa "));
        assert!(!re.is_match("aaA"));

        let re = XSRegexp::compile("a+").unwrap();
        assert!(re.is_match("a"));
        assert!(re.is_match("aa"));
        assert!(re.is_match("aaa"));
        assert!(!re.is_match(""));
        assert!(!re.is_match("ab"));
        assert!(!re.is_match("b"));
        assert!(!re.is_match(" aaa"));
        assert!(!re.is_match("aaa "));
        assert!(!re.is_match("aaA"));

        let re = XSRegexp::compile("a?").unwrap();
        assert!(re.is_match(""));
        assert!(re.is_match("a"));
        assert!(!re.is_match("aa"));
        assert!(!re.is_match("aaa"));
        assert!(!re.is_match("ab"));
        assert!(!re.is_match("b"));
        assert!(!re.is_match(" aaa"));
        assert!(!re.is_match("aaa "));
        assert!(!re.is_match("aaA"));

        let re = XSRegexp::compile("a|b").unwrap();
        assert!(re.is_match("a"));
        assert!(re.is_match("b"));
        assert!(!re.is_match(""));
        assert!(!re.is_match("aa"));
        assert!(!re.is_match("aaa"));
        assert!(!re.is_match("ab"));
        assert!(!re.is_match(" aaa"));
        assert!(!re.is_match("aaa "));
        assert!(!re.is_match("A"));
        assert!(!re.is_match("B"));

        let re = XSRegexp::compile("a+|b?").unwrap();
        assert!(re.is_match(""));
        assert!(re.is_match("a"));
        assert!(re.is_match("aa"));
        assert!(re.is_match("aaa"));
        assert!(re.is_match("b"));
        assert!(!re.is_match("bb"));
        assert!(!re.is_match("ab"));
        assert!(!re.is_match(" aaa"));
        assert!(!re.is_match("aaa "));
        assert!(!re.is_match("A"));
        assert!(!re.is_match("B"));

        let re = XSRegexp::compile("a+c|b?c").unwrap();
        assert!(re.is_match("ac"));
        assert!(re.is_match("aac"));
        assert!(re.is_match("bc"));
        assert!(re.is_match("c"));
        assert!(!re.is_match(""));
        assert!(!re.is_match("a"));
        assert!(!re.is_match("aa"));
        assert!(!re.is_match("aaa"));
        assert!(!re.is_match("b"));
        assert!(!re.is_match("bb"));
        assert!(!re.is_match("bbc"));
        assert!(!re.is_match("ab"));
        assert!(!re.is_match("abc"));
        assert!(!re.is_match(" aaa"));
        assert!(!re.is_match("aaa "));
        assert!(!re.is_match("A"));
        assert!(!re.is_match("B"));
        assert!(!re.is_match("C"));

        let re = XSRegexp::compile("a+(c|b?)c").unwrap();
        assert!(re.is_match("ac"));
        assert!(re.is_match("aac"));
        assert!(re.is_match("abc"));
        assert!(!re.is_match("bc"));
        assert!(!re.is_match("c"));
        assert!(!re.is_match(""));
        assert!(!re.is_match("a"));
        assert!(!re.is_match("aa"));
        assert!(!re.is_match("aaa"));
        assert!(!re.is_match("b"));
        assert!(!re.is_match("bb"));
        assert!(!re.is_match("bbc"));
        assert!(!re.is_match("ab"));
        assert!(!re.is_match(" aaa"));
        assert!(!re.is_match("aaa "));
        assert!(!re.is_match("A"));
        assert!(!re.is_match("B"));
        assert!(!re.is_match("C"));

        let re = XSRegexp::compile("[abde]").unwrap();
        assert!(re.is_match("a"));
        assert!(re.is_match("b"));
        assert!(re.is_match("d"));
        assert!(re.is_match("e"));
        assert!(!re.is_match(""));
        assert!(!re.is_match("c"));
        assert!(!re.is_match("ab"));
        assert!(!re.is_match("f"));

        let re = XSRegexp::compile("[^abde]").unwrap();
        assert!(re.is_match("c"));
        assert!(re.is_match("f"));
        assert!(!re.is_match(""));
        assert!(!re.is_match("a"));
        assert!(!re.is_match("b"));
        assert!(!re.is_match("d"));
        assert!(!re.is_match("e"));
        assert!(!re.is_match("ab"));
    }
}
