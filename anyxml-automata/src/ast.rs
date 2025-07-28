use std::ops::RangeInclusive;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ASTNodeType {
    Charcters,
    Catenation,
    Alternation,
    ZeroOrOne,
    ZeroOrMore,
    OneOrMore,
    Repeat,
    RepeatExact,
}

pub enum ASTNode {
    Charcters {
        range: RangeInclusive<char>,
        negation: bool,
    },
    Catenation(Box<ASTNode>, Box<ASTNode>),
    Alternation(Box<ASTNode>, Box<ASTNode>),
    ZeroOrOne(Box<ASTNode>),
    ZeroOrMore(Box<ASTNode>),
    OneOrMore(Box<ASTNode>),
    Repeat {
        node: Box<ASTNode>,
        at_least: usize,
        at_most: Option<usize>,
    },
    RepeatExact(Box<ASTNode>, usize),
}

impl ASTNode {
    pub fn node_type(&self) -> ASTNodeType {
        match self {
            Self::Charcters {
                range: _,
                negation: _,
            } => ASTNodeType::Charcters,
            Self::Catenation(_, _) => ASTNodeType::Catenation,
            Self::Alternation(_, _) => ASTNodeType::Alternation,
            Self::ZeroOrOne(_) => ASTNodeType::ZeroOrOne,
            Self::ZeroOrMore(_) => ASTNodeType::ZeroOrMore,
            Self::OneOrMore(_) => ASTNodeType::OneOrMore,
            Self::Repeat {
                node: _,
                at_least: _,
                at_most: _,
            } => ASTNodeType::Repeat,
            Self::RepeatExact(_, _) => ASTNodeType::RepeatExact,
        }
    }
}

impl std::fmt::Debug for ASTNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ASTNodeType::*;

        match self {
            Self::Charcters { range, negation } => {
                if !range.is_empty() {
                    let &start = range.start();
                    let &end = range.end();
                    if start == end {
                        if *negation {
                            write!(f, "[^{start}]")?;
                        } else {
                            write!(f, "{start}")?;
                        }
                    } else if *negation {
                        write!(f, "[^{start}-{end}]")?;
                    } else {
                        write!(f, "[{start}-{end}]")?;
                    }
                }
                Ok(())
            }
            Self::Catenation(front, back) => {
                write!(f, "{front:?}{back:?}")
            }
            Self::Alternation(left, right) => {
                write!(f, "{left:?}|{right:?}")
            }
            Self::ZeroOrOne(node) => {
                if matches!(node.node_type(), Charcters) {
                    write!(f, "{node:?}?")
                } else {
                    write!(f, "({node:?})?")
                }
            }
            Self::ZeroOrMore(node) => {
                if matches!(node.node_type(), Charcters) {
                    write!(f, "{node:?}*")
                } else {
                    write!(f, "({node:?})*")
                }
            }
            Self::OneOrMore(node) => {
                if matches!(node.node_type(), Charcters) {
                    write!(f, "{node:?}+")
                } else {
                    write!(f, "({node:?})+")
                }
            }
            Self::Repeat {
                node,
                at_least,
                at_most,
            } => {
                if matches!(node.node_type(), Charcters) {
                    write!(f, "{node:?}{{{at_least},")?;
                } else {
                    write!(f, "({node:?}){{{at_least},")?;
                }
                if let Some(at_most) = at_most {
                    write!(f, "{at_most}")?;
                }
                write!(f, "}}")
            }
            Self::RepeatExact(node, n) => {
                if matches!(node.node_type(), Charcters) {
                    write!(f, "{node:?}{{{n}}}")
                } else {
                    write!(f, "({node:?}){{{n}}}")
                }
            }
        }
    }
}
