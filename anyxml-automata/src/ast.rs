use std::fmt::{Debug, Display};

use crate::Atom;

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

pub enum ASTNode<A: Atom> {
    Charcters {
        start: A,
        end: A,
        negation: bool,
    },
    Catenation(Box<ASTNode<A>>, Box<ASTNode<A>>),
    Alternation(Box<ASTNode<A>>, Box<ASTNode<A>>),
    ZeroOrOne(Box<ASTNode<A>>),
    ZeroOrMore(Box<ASTNode<A>>),
    OneOrMore(Box<ASTNode<A>>),
    Repeat {
        node: Box<ASTNode<A>>,
        at_least: usize,
        at_most: Option<usize>,
    },
    RepeatExact(Box<ASTNode<A>>, usize),
}

impl<A: Atom> ASTNode<A> {
    pub(crate) fn alternate_all(mut iter: impl Iterator<Item = (A, A)>) -> Option<Self> {
        let (start, end) = iter.next()?;
        let mut ret = ASTNode::Charcters {
            start,
            end,
            negation: false,
        };
        for (start, end) in iter {
            assert!(start <= end);
            let alt = ASTNode::Charcters {
                start,
                end,
                negation: false,
            };
            ret = ASTNode::Alternation(Box::new(ret), Box::new(alt));
        }
        Some(ret)
    }

    pub(crate) fn negate_all(iter: impl Iterator<Item = (A, A)>) -> Option<Self> {
        let mut s = A::MIN;
        let mut ret = None::<Self>;
        for (start, end) in iter {
            assert!(s <= start);
            if let Some(end) = start.previous() {
                assert!(s <= end);
                let alt = ASTNode::Charcters {
                    start: s,
                    end,
                    negation: false,
                };
                if let Some(left) = ret {
                    ret = Some(ASTNode::Alternation(Box::new(left), Box::new(alt)));
                } else {
                    ret = Some(alt);
                }
            }
            if let Some(next) = end.next() {
                s = next;
            }
        }
        ret
    }

    pub fn node_type(&self) -> ASTNodeType {
        match self {
            Self::Charcters {
                start: _,
                end: _,
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

impl<A: Atom + Debug> std::fmt::Debug for ASTNode<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ASTNodeType::*;

        match self {
            &Self::Charcters {
                start,
                end,
                negation,
            } => {
                if start <= end {
                    if start == end {
                        if negation {
                            write!(f, "[^{start:?}]")?;
                        } else {
                            write!(f, "{start:?}")?;
                        }
                    } else if negation {
                        write!(f, "[^{start:?}-{end:?}]")?;
                    } else {
                        write!(f, "[{start:?}-{end:?}]")?;
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

impl<A: Atom + Display> std::fmt::Display for ASTNode<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ASTNodeType::*;

        match self {
            &Self::Charcters {
                start,
                end,
                negation,
            } => {
                if start <= end {
                    if start == end {
                        if negation {
                            write!(f, "[^{start}]")?;
                        } else {
                            write!(f, "{start}")?;
                        }
                    } else if negation {
                        write!(f, "[^{start}-{end}]")?;
                    } else {
                        write!(f, "[{start}-{end}]")?;
                    }
                }
                Ok(())
            }
            Self::Catenation(front, back) => {
                write!(f, "{front}{back}")
            }
            Self::Alternation(left, right) => {
                write!(f, "{left}|{right}")
            }
            Self::ZeroOrOne(node) => {
                if matches!(node.node_type(), Charcters) {
                    write!(f, "{node}?")
                } else {
                    write!(f, "({node})?")
                }
            }
            Self::ZeroOrMore(node) => {
                if matches!(node.node_type(), Charcters) {
                    write!(f, "{node}*")
                } else {
                    write!(f, "({node})*")
                }
            }
            Self::OneOrMore(node) => {
                if matches!(node.node_type(), Charcters) {
                    write!(f, "{node}+")
                } else {
                    write!(f, "({node})+")
                }
            }
            Self::Repeat {
                node,
                at_least,
                at_most,
            } => {
                if matches!(node.node_type(), Charcters) {
                    write!(f, "{node}{{{at_least},")?;
                } else {
                    write!(f, "({node}){{{at_least},")?;
                }
                if let Some(at_most) = at_most {
                    write!(f, "{at_most}")?;
                }
                write!(f, "}}")
            }
            Self::RepeatExact(node, n) => {
                if matches!(node.node_type(), Charcters) {
                    write!(f, "{node}{{{n}}}")
                } else {
                    write!(f, "({node}){{{n}}}")
                }
            }
        }
    }
}
