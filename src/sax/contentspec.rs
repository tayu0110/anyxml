use std::{
    collections::{HashMap, HashSet},
    iter::once,
    sync::{Arc, atomic::AtomicUsize},
};

use crate::automata::{
    Atom,
    ast::ASTNode,
    fa::{DFA, NFA, State},
};

/// # Reference
/// - [3.2 Element Type Declarations](https://www.w3.org/TR/xml/#elemdecls)
#[derive(Debug, Clone)]
pub enum ContentSpec {
    /// `'EMPTY'`
    EMPTY,
    /// `'ANY'`
    ANY,
    /// [Mixed Content](https://www.w3.org/TR/xml/#sec-mixed-content)
    Mixed(Arc<HashSet<Box<str>>>),
    /// [Element Content](https://www.w3.org/TR/xml/#sec-element-content)
    Children(ElementContent),
}

impl ContentSpec {
    pub fn new_validator(&mut self) -> ContentSpecValidationContext {
        let (validator, is_external_element_content) = match self {
            ContentSpec::EMPTY => (ContentSpecValidator::Empty, false),
            ContentSpec::ANY => (ContentSpecValidator::Any, false),
            ContentSpec::Mixed(set) => (ContentSpecValidator::Mixed(set.clone()), false),
            ContentSpec::Children(model) => {
                let validator = model.new_validator();
                (
                    ContentSpecValidator::Children {
                        unrecoverable: false,
                        state: vec![validator.initial_state()],
                        name_ids: model.name_ids.clone().unwrap(),
                        validator,
                    },
                    model.id >= MAX_ELEMENT_CONTENT_ID,
                )
            }
        };

        ContentSpecValidationContext {
            invalid: false,
            whitespace: false,
            is_external_element_content,
            validator,
        }
    }
}

impl std::fmt::Display for ContentSpec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EMPTY => write!(f, "EMPTY"),
            Self::ANY => write!(f, "ANY"),
            Self::Mixed(mixed) => {
                write!(f, "(#PCDATA")?;
                let mut mixed = mixed.iter().collect::<Vec<_>>();
                mixed.sort_unstable();
                for name in mixed.iter() {
                    write!(f, "|{name}")?;
                }
                if mixed.is_empty() {
                    write!(f, ")")
                } else {
                    write!(f, ")*")
                }
            }
            Self::Children(children) => write!(f, "{children}"),
        }
    }
}

const MAX_ELEMENT_CONTENT_ID: usize = 1 << (usize::BITS - 1);
static ELEMENT_CONTENT_ID: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug, Clone)]
enum ElementContentState {
    Name(Box<str>),
    Catenation { previous: usize, next: usize },
    Alternation(usize, usize),
    ZeroOrOne(usize),
    ZeroOrMore(usize),
    OneOrMore(usize),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ElementContentStateID {
    holder_id: usize,
    state_id: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]
pub(crate) struct NameID(usize);

impl Atom for NameID {
    const EPSILON: Self = Self(0);
    const MIN: Self = Self(1);
    const MAX: Self = Self(usize::MAX);

    fn previous(&self) -> Option<Self> {
        self.0.checked_sub(1).filter(|&c| c > 0).map(Self)
    }
    fn next(&self) -> Option<Self> {
        self.0.checked_add(1).map(Self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ElementContentCompileError {
    InvalidSyntaxTree,
}

#[derive(Debug, Clone)]
pub(crate) enum ElementContentValidator {
    Nfa(Arc<NFA<NameID>>),
    Dfa(Arc<DFA<NameID>>),
}

impl ElementContentValidator {
    fn initial_state(&self) -> State<NameID> {
        match self {
            ElementContentValidator::Nfa(nfa) => nfa.initial_state(),
            ElementContentValidator::Dfa(dfa) => dfa.initial_state(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ElementContent {
    // if id >= MAX_ELEMENT_CONTENT_ID, this is created from an external markup.
    id: usize,
    states: Vec<ElementContentState>,
    name_ids: Option<Arc<HashMap<Box<str>, usize>>>,
    compiled: Option<ElementContentValidator>,
}

impl ElementContent {
    pub(crate) fn new(is_external_markup: bool) -> Self {
        let mut id = ELEMENT_CONTENT_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        assert!(
            id < MAX_ELEMENT_CONTENT_ID,
            "Too many element content object."
        );
        if is_external_markup {
            id |= MAX_ELEMENT_CONTENT_ID;
        }
        Self {
            id,
            states: vec![],
            name_ids: None,
            compiled: None,
        }
    }

    pub(crate) fn create_name(&mut self, name: impl Into<Box<str>>) -> ElementContentStateID {
        let state_id = self.states.len();
        self.states.push(ElementContentState::Name(name.into()));
        ElementContentStateID {
            holder_id: self.id,
            state_id,
        }
    }

    pub(crate) fn create_catenation(
        &mut self,
        previous: ElementContentStateID,
        next: ElementContentStateID,
    ) -> ElementContentStateID {
        assert_eq!(self.id, previous.holder_id);
        assert_eq!(self.id, next.holder_id);
        let state_id = self.states.len();
        self.states.push(ElementContentState::Catenation {
            previous: previous.state_id,
            next: next.state_id,
        });
        ElementContentStateID {
            holder_id: self.id,
            state_id,
        }
    }

    pub(crate) fn create_alternation(
        &mut self,
        left: ElementContentStateID,
        right: ElementContentStateID,
    ) -> ElementContentStateID {
        assert_eq!(self.id, left.holder_id);
        assert_eq!(self.id, right.holder_id);
        let state_id = self.states.len();
        self.states.push(ElementContentState::Alternation(
            left.state_id,
            right.state_id,
        ));
        ElementContentStateID {
            holder_id: self.id,
            state_id,
        }
    }

    pub(crate) fn create_zero_or_one(
        &mut self,
        state: ElementContentStateID,
    ) -> ElementContentStateID {
        assert_eq!(self.id, state.holder_id);
        let state_id = self.states.len();
        self.states
            .push(ElementContentState::ZeroOrOne(state.state_id));
        ElementContentStateID {
            holder_id: self.id,
            state_id,
        }
    }

    pub(crate) fn create_zero_or_more(
        &mut self,
        state: ElementContentStateID,
    ) -> ElementContentStateID {
        assert_eq!(self.id, state.holder_id);
        let state_id = self.states.len();
        self.states
            .push(ElementContentState::ZeroOrMore(state.state_id));
        ElementContentStateID {
            holder_id: self.id,
            state_id,
        }
    }

    pub(crate) fn create_one_or_more(
        &mut self,
        state: ElementContentStateID,
    ) -> ElementContentStateID {
        assert_eq!(self.id, state.holder_id);
        let state_id = self.states.len();
        self.states
            .push(ElementContentState::OneOrMore(state.state_id));
        ElementContentStateID {
            holder_id: self.id,
            state_id,
        }
    }

    fn get_root_state(&self) -> Result<usize, ElementContentCompileError> {
        // check tree constraint
        let mut parent = vec![usize::MAX; self.states.len()];
        let mut num_edges = 0;
        for (i, state) in self.states.iter().enumerate() {
            match state {
                ElementContentState::Name(_) => {}
                ElementContentState::Catenation { previous, next }
                | ElementContentState::Alternation(previous, next) => {
                    if parent[*previous] != usize::MAX || parent[*next] != usize::MAX {
                        return Err(ElementContentCompileError::InvalidSyntaxTree);
                    }
                    parent[*previous] = i;
                    parent[*next] = i;
                    num_edges += 2;
                }
                ElementContentState::ZeroOrOne(child)
                | ElementContentState::ZeroOrMore(child)
                | ElementContentState::OneOrMore(child) => {
                    if parent[*child] != usize::MAX {
                        return Err(ElementContentCompileError::InvalidSyntaxTree);
                    }
                    parent[*child] = i;
                    num_edges += 1;
                }
            }
        }
        if num_edges != self.states.len() - 1
            || parent.iter().filter(|&&p| p == usize::MAX).count() != 1
        {
            return Err(ElementContentCompileError::InvalidSyntaxTree);
        }
        Ok(parent.into_iter().position(|p| p == usize::MAX).unwrap())
    }

    /// If successfully compiled or already compiled, return `Ok`.  \
    /// `Ok(false)` indicates that the state remained unchanged because it was already compiled
    /// or was unambiguous before compilation.  \
    /// `Ok(true)` indicates that the state changed because it was uncompiled and ambiguous.
    pub(crate) fn compile(&mut self) -> Result<bool, ElementContentCompileError> {
        // build ast
        let root = self.get_root_state()?;
        let mut memo = HashMap::new();
        let ast = self.build_syntax_tree(root, &mut memo);

        // build NFA and check if it is deterministic
        let nfa = NFA::assemble(Some(&ast)).unwrap();
        let is_deterministic = nfa.is_deterministic();
        self.compiled = Some(ElementContentValidator::Nfa(Arc::new(nfa)));
        self.name_ids = Some(Arc::new(memo));
        Ok(!is_deterministic)
    }

    fn build_syntax_tree(
        &self,
        node: usize,
        memo: &mut HashMap<Box<str>, usize>,
    ) -> ASTNode<NameID> {
        use ElementContentState::*;
        use std::collections::hash_map::Entry::*;

        let len = memo.len();
        match &self.states[node] {
            Name(name) => match memo.entry(name.clone()) {
                Occupied(entry) => {
                    let id = *entry.get();
                    ASTNode::Charcters {
                        start: NameID(id),
                        end: NameID(id),
                        negation: false,
                    }
                }
                Vacant(entry) => {
                    let id = len + 1;
                    entry.insert(id);
                    ASTNode::Charcters {
                        start: NameID(id),
                        end: NameID(id),
                        negation: false,
                    }
                }
            },
            Catenation { previous, next } => {
                let previous = self.build_syntax_tree(*previous, memo);
                let next = self.build_syntax_tree(*next, memo);
                ASTNode::Catenation(Box::new(previous), Box::new(next))
            }
            Alternation(left, right) => {
                let left = self.build_syntax_tree(*left, memo);
                let right = self.build_syntax_tree(*right, memo);
                ASTNode::Alternation(Box::new(left), Box::new(right))
            }
            ZeroOrOne(node) => {
                let node = self.build_syntax_tree(*node, memo);
                ASTNode::ZeroOrOne(Box::new(node))
            }
            ZeroOrMore(node) => {
                let node = self.build_syntax_tree(*node, memo);
                ASTNode::ZeroOrMore(Box::new(node))
            }
            OneOrMore(node) => {
                let node = self.build_syntax_tree(*node, memo);
                ASTNode::OneOrMore(Box::new(node))
            }
        }
    }

    fn new_validator(&mut self) -> ElementContentValidator {
        match &self.compiled {
            Some(ElementContentValidator::Nfa(nfa)) => {
                let dfa = Arc::new(nfa.build_dfa());
                self.compiled = Some(ElementContentValidator::Dfa(dfa.clone()));
                ElementContentValidator::Dfa(dfa.clone())
            }
            Some(ElementContentValidator::Dfa(dfa)) => ElementContentValidator::Dfa(dfa.clone()),
            None => {
                self.compile().ok();
                if self.compiled.is_none() {
                    panic!("internal error: element content cannot compiled.");
                }
                self.new_validator()
            }
        }
    }

    fn display_to(&self, to: &mut impl std::fmt::Write, state: usize) -> std::fmt::Result {
        use ElementContentState::*;

        match &self.states[state] {
            Name(name) => {
                write!(to, "{name}")
            }
            Catenation { previous, next } => {
                if matches!(self.states[*previous], Alternation(_, _)) {
                    write!(to, "(")?;
                    self.display_to(to, *previous)?;
                    write!(to, ")")?;
                } else {
                    self.display_to(to, *previous)?;
                }

                write!(to, ",")?;

                if matches!(self.states[*next], Alternation(_, _)) {
                    write!(to, "(")?;
                    self.display_to(to, *next)?;
                    write!(to, ")")
                } else {
                    self.display_to(to, *next)
                }
            }
            Alternation(left, right) => {
                if matches!(self.states[*left], Catenation { .. }) {
                    write!(to, "(")?;
                    self.display_to(to, *left)?;
                    write!(to, ")")?;
                } else {
                    self.display_to(to, *left)?;
                }

                write!(to, "|")?;

                if matches!(self.states[*right], Catenation { .. }) {
                    write!(to, "(")?;
                    self.display_to(to, *right)?;
                    write!(to, ")")
                } else {
                    self.display_to(to, *right)
                }
            }
            ZeroOrOne(state) => {
                if matches!(self.states[*state], Name(_)) {
                    self.display_to(to, *state)?;
                    write!(to, "?")
                } else {
                    write!(to, "(")?;
                    self.display_to(to, *state)?;
                    write!(to, ")?")
                }
            }
            ZeroOrMore(state) => {
                if matches!(self.states[*state], Name(_)) {
                    self.display_to(to, *state)?;
                    write!(to, "*")
                } else {
                    write!(to, "(")?;
                    self.display_to(to, *state)?;
                    write!(to, ")*")
                }
            }
            OneOrMore(state) => {
                if matches!(self.states[*state], Name(_)) {
                    self.display_to(to, *state)?;
                    write!(to, "+")
                } else {
                    write!(to, "(")?;
                    self.display_to(to, *state)?;
                    write!(to, ")+")
                }
            }
        }
    }
}

impl std::fmt::Display for ElementContent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ElementContentState::*;

        let Ok(root) = self.get_root_state() else {
            return write!(f, "invalid element content");
        };

        let outer = !matches!(
            self.states[root],
            OneOrMore(_) | ZeroOrMore(_) | ZeroOrOne(_)
        );
        if outer {
            write!(f, "(")?;
        }

        self.display_to(f, root)?;

        if outer {
            write!(f, ")")?;
        }
        Ok(())
    }
}

pub enum ContentSpecValidationError {
    UnacceptableElement,
    UnacceptablePCDATA,
    NotReachedAcceptedState,
}

#[derive(Debug)]
pub struct ContentSpecValidationContext {
    invalid: bool,
    whitespace: bool,
    is_external_element_content: bool,
    validator: ContentSpecValidator,
}

impl ContentSpecValidationContext {
    pub fn push_name(&mut self, name: &str) {
        self.invalid |= self.validator.push_name(name).is_err();
    }

    pub fn push_pcdata(&mut self) {
        self.invalid |= self.validator.push_pcdata().is_err();
    }

    pub fn push_whitespaces(&mut self) {
        self.whitespace = true;
        self.invalid |= self.validator.push_whitespaces().is_err();
    }

    /// In element content validation, it can be treated almost the same as whitespace.  \
    /// However, in standalone document validation, whitespace and other Miscs are treated differently.
    pub fn push_misc(&mut self) {
        self.invalid |= self.validator.push_whitespaces().is_err();
    }

    pub fn finish(&mut self) -> bool {
        self.invalid |= self.validator.finish().is_err();
        !self.invalid
    }

    pub(crate) fn whitespace(&self) -> bool {
        self.whitespace
    }

    pub(crate) fn is_element_content(&self) -> bool {
        matches!(self.validator, ContentSpecValidator::Children { .. })
    }

    pub(crate) fn is_external_element_content(&self) -> bool {
        self.is_external_element_content
    }
}

#[derive(Debug)]
pub(crate) enum ContentSpecValidator {
    Empty,
    Any,
    Mixed(Arc<HashSet<Box<str>>>),
    Children {
        unrecoverable: bool,
        state: Vec<State<NameID>>,
        name_ids: Arc<HashMap<Box<str>, usize>>,
        validator: ElementContentValidator,
    },
}

impl ContentSpecValidator {
    pub fn push_name(&mut self, name: &str) -> Result<(), ContentSpecValidationError> {
        match self {
            ContentSpecValidator::Empty => Err(ContentSpecValidationError::UnacceptableElement),
            ContentSpecValidator::Any => Ok(()),
            ContentSpecValidator::Mixed(allowed) => {
                if allowed.contains(name) {
                    Ok(())
                } else {
                    Err(ContentSpecValidationError::UnacceptableElement)
                }
            }
            ContentSpecValidator::Children {
                unrecoverable,
                state,
                name_ids,
                validator,
            } => {
                if *unrecoverable {
                    // Since it is no longer possible to validate correctly,
                    // it returns OK without performing any action.
                    return Ok(());
                }

                let Some(name_id) = name_ids.get(name).copied().map(NameID) else {
                    return Err(ContentSpecValidationError::UnacceptableElement);
                };
                match validator {
                    ElementContentValidator::Nfa(nfa) => {
                        if let Ok(new) = nfa.transition(state.iter().copied(), once(name_id)) {
                            *state = new;
                            Ok(())
                        } else {
                            *unrecoverable = true;
                            Err(ContentSpecValidationError::UnacceptableElement)
                        }
                    }
                    ElementContentValidator::Dfa(dfa) => {
                        if let Ok(new) = dfa.transition(state.pop().unwrap(), once(name_id)) {
                            state.push(new);
                            Ok(())
                        } else {
                            *unrecoverable = true;
                            Err(ContentSpecValidationError::UnacceptableElement)
                        }
                    }
                }
            }
        }
    }

    pub fn push_pcdata(&self) -> Result<(), ContentSpecValidationError> {
        match self {
            ContentSpecValidator::Any | ContentSpecValidator::Mixed(_) => Ok(()),
            ContentSpecValidator::Empty | ContentSpecValidator::Children { .. } => {
                Err(ContentSpecValidationError::UnacceptablePCDATA)
            }
        }
    }

    pub fn push_whitespaces(&self) -> Result<(), ContentSpecValidationError> {
        match self {
            ContentSpecValidator::Empty => Err(ContentSpecValidationError::UnacceptablePCDATA),
            _ => Ok(()),
        }
    }

    pub fn finish(&self) -> Result<(), ContentSpecValidationError> {
        match self {
            ContentSpecValidator::Children {
                unrecoverable,
                state,
                validator,
                ..
            } => {
                if *unrecoverable {
                    Ok(())
                } else {
                    match validator {
                        ElementContentValidator::Nfa(nfa) => {
                            if state.iter().any(|&state| nfa.is_accepted(state)) {
                                Ok(())
                            } else {
                                Err(ContentSpecValidationError::NotReachedAcceptedState)
                            }
                        }
                        ElementContentValidator::Dfa(dfa) => {
                            if dfa.is_accepted(state[0]) {
                                Ok(())
                            } else {
                                Err(ContentSpecValidationError::NotReachedAcceptedState)
                            }
                        }
                    }
                }
            }
            _ => Ok(()),
        }
    }
}
