use std::{
    collections::{BTreeMap, HashMap, HashSet},
    sync::Arc,
};

use crate::{
    relaxng::{datatype_library::RelaxNGDatatypeLibraries, validate::QName},
    uri::URIStr,
};

pub(super) type Uri = Arc<str>;
pub(super) type LocalName = Arc<str>;
pub(super) type ParamList = BTreeMap<LocalName, Arc<str>>;
pub(super) type Prefix = Arc<str>;
pub(super) type Context = (Arc<URIStr>, BTreeMap<Prefix, Uri>);
pub(super) type Datatype = (Uri, LocalName);
pub(super) type PatternId = usize;

#[derive(Debug, PartialEq, Eq, Hash)]
pub(super) enum NameClass {
    AnyName,
    AnyNameExcept(Arc<NameClass>),
    Name(Uri, LocalName),
    NsName(Uri),
    NsNameExcept(Uri, Arc<NameClass>),
    NameClassChoice(Arc<NameClass>, Arc<NameClass>),
}

impl NameClass {
    pub(super) fn contains(&self, qname: &QName) -> bool {
        match (self, qname) {
            (Self::AnyName, _) => true,
            (Self::AnyNameExcept(nc), n) => !nc.contains(n),
            (Self::NsName(ns1), QName(ns2, _)) => ns1 == ns2,
            (Self::NsNameExcept(ns1, nc), n @ QName(ns2, _)) => ns1 == ns2 && !nc.contains(n),
            (Self::Name(ns1, ln1), QName(ns2, ln2)) => ns1 == ns2 && ln1 == ln2,
            (Self::NameClassChoice(nc1, nc2), n) => nc1.contains(n) || nc2.contains(n),
        }
    }

    pub(super) fn is_infinite(&self) -> bool {
        match self {
            Self::AnyName | Self::AnyNameExcept(_) | Self::NsName(_) | Self::NsNameExcept(_, _) => {
                true
            }
            Self::Name(_, _) => false,
            Self::NameClassChoice(nc1, nc2) => nc1.is_infinite() || nc2.is_infinite(),
        }
    }

    /// # Reference
    /// - [Name class analysis](https://relaxng.org/jclark/nameclass.html)
    pub(super) fn overlap(&self, other: &Self) -> bool {
        self.representatives(other)
            .map(|(ns, ln)| QName(ns, ln))
            .any(|qn| self.contains(&qn) && other.contains(&qn))
    }
    /// # Reference
    /// - [Name class analysis](https://relaxng.org/jclark/nameclass.html)
    fn representatives<'a>(
        &'a self,
        other: &'a Self,
    ) -> impl Iterator<Item = (Arc<str>, Arc<str>)> + 'a {
        let mut stack = vec![self, other];
        let mut seen = HashSet::new();
        let mut seen_anyname = false;
        let illegal_local_name = Arc::<str>::from("");
        let illegal_uri = Arc::<str>::from("\x01");
        std::iter::from_fn(move || {
            while let Some(now) = stack.pop() {
                match now {
                    NameClass::AnyName => {
                        if !seen_anyname {
                            seen_anyname = true;
                            return Some((illegal_uri.clone(), illegal_local_name.clone()));
                        }
                    }
                    NameClass::AnyNameExcept(nc) => {
                        stack.push(nc.as_ref());
                        if !seen_anyname {
                            seen_anyname = true;
                            return Some((illegal_uri.clone(), illegal_local_name.clone()));
                        }
                    }
                    NameClass::NsName(ns) => {
                        if seen.insert((ns.clone(), illegal_local_name.clone())) {
                            return Some((ns.clone(), illegal_local_name.clone()));
                        }
                    }
                    NameClass::NsNameExcept(ns, nc) => {
                        stack.push(nc.as_ref());
                        if seen.insert((ns.clone(), illegal_local_name.clone())) {
                            return Some((ns.clone(), illegal_local_name.clone()));
                        }
                    }
                    NameClass::Name(ns, ln) => {
                        if seen.insert((ns.clone(), ln.clone())) {
                            return Some((ns.clone(), ln.clone()));
                        }
                    }
                    NameClass::NameClassChoice(nc1, nc2) => {
                        stack.push(nc1.as_ref());
                        stack.push(nc2.as_ref());
                    }
                }
            }
            None
        })
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub(super) enum Pattern {
    Empty,
    NotAllowed,
    Text,
    Choice(PatternId, PatternId),
    Interleave(PatternId, PatternId),
    Group(PatternId, PatternId),
    OneOrMore(PatternId),
    List(PatternId),
    Data(Datatype, ParamList),
    DataExcept(Datatype, ParamList, PatternId),
    Value(Datatype, Arc<str>, Context),
    Attribute(Arc<NameClass>, PatternId),
    Element(Arc<NameClass>, PatternId),
    After(PatternId, PatternId),
}

pub(super) struct Grammar {
    pub(super) root: PatternId,
    pub(super) libraries: RelaxNGDatatypeLibraries,
    pub(super) patterns: Vec<Arc<Pattern>>,
    pub(super) intern: HashMap<Arc<Pattern>, PatternId>,
    /// -1: unknown, 0: false, 1: true
    pub(super) nullable: Vec<i8>,
}
