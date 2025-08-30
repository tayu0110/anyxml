use std::{
    collections::{HashMap, HashSet},
    iter::once,
    sync::{Arc, atomic::AtomicUsize},
};

use anyxml_automata::{
    Atom,
    ast::ASTNode,
    fa::{DFA, NFA, State},
};

#[derive(Debug, Clone)]
pub enum ContentSpec {
    EMPTY,
    ANY,
    Mixed(Arc<HashSet<Box<str>>>),
    Children(ElementContent),
}

impl ContentSpec {
    pub fn new_validator(&mut self) -> ContentSpecValidator {
        match self {
            ContentSpec::EMPTY => ContentSpecValidator::EMPTY,
            ContentSpec::ANY => ContentSpecValidator::ANY,
            ContentSpec::Mixed(set) => ContentSpecValidator::Mixed(set.clone()),
            ContentSpec::Children(model) => {
                let validator = model.new_validator();
                ContentSpecValidator::Children {
                    unrecoverable: false,
                    state: vec![validator.initial_state()],
                    name_ids: model.name_ids.clone().unwrap(),
                    validator,
                }
            }
        }
    }
}

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
pub struct ElementContentStateID {
    holder_id: usize,
    state_id: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]
pub struct NameID(usize);

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
pub enum ElementContentValidator {
    NFA(Arc<NFA<NameID>>),
    DFA(Arc<DFA<NameID>>),
}

impl ElementContentValidator {
    fn initial_state(&self) -> State<NameID> {
        match self {
            ElementContentValidator::NFA(nfa) => nfa.initial_state(),
            ElementContentValidator::DFA(dfa) => dfa.initial_state(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ElementContent {
    id: usize,
    states: Vec<ElementContentState>,
    name_ids: Option<Arc<HashMap<Box<str>, usize>>>,
    compiled: Option<ElementContentValidator>,
}

impl ElementContent {
    pub(crate) fn new() -> Self {
        let id = ELEMENT_CONTENT_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
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

    /// If successfully compiled or already compiled, return `Ok`.  \
    /// `Ok(false)` indicates that the state remained unchanged because it was already compiled
    /// or was unambiguous before compilation.  \
    /// `Ok(true)` indicates that the state changed because it was uncompiled and ambiguous.
    pub(crate) fn compile(&mut self) -> Result<bool, ElementContentCompileError> {
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

        // build ast
        let root = parent.into_iter().position(|p| p == usize::MAX).unwrap();
        let mut memo = HashMap::new();
        let ast = self.build_syntax_tree(root, &mut memo);

        // build NFA and check if it is deterministic
        let nfa = NFA::assemble(Some(&ast)).unwrap();
        let is_deterministic = nfa.is_deterministic();
        self.compiled = Some(ElementContentValidator::NFA(Arc::new(nfa)));
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
            Some(ElementContentValidator::NFA(nfa)) => {
                let dfa = Arc::new(nfa.build_dfa());
                self.compiled = Some(ElementContentValidator::DFA(dfa.clone()));
                ElementContentValidator::DFA(dfa.clone())
            }
            Some(ElementContentValidator::DFA(dfa)) => ElementContentValidator::DFA(dfa.clone()),
            None => {
                self.compile().ok();
                if self.compiled.is_none() {
                    panic!("internal error: element content cannot compiled.");
                }
                self.new_validator()
            }
        }
    }
}

pub enum ContentSpecValidationError {
    UnacceptableElement,
    UnacceptablePCDATA,
    NotReachedAcceptedState,
}

#[derive(Debug)]
pub enum ContentSpecValidator {
    EMPTY,
    ANY,
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
            ContentSpecValidator::EMPTY => Err(ContentSpecValidationError::UnacceptableElement),
            ContentSpecValidator::ANY => Ok(()),
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
                    ElementContentValidator::NFA(nfa) => {
                        if let Ok(new) = nfa.transition(state.iter().copied(), once(name_id)) {
                            *state = new;
                            Ok(())
                        } else {
                            *unrecoverable = true;
                            Err(ContentSpecValidationError::UnacceptableElement)
                        }
                    }
                    ElementContentValidator::DFA(dfa) => {
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
            ContentSpecValidator::ANY | ContentSpecValidator::Mixed(_) => Ok(()),
            ContentSpecValidator::EMPTY | ContentSpecValidator::Children { .. } => {
                Err(ContentSpecValidationError::UnacceptablePCDATA)
            }
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
                        ElementContentValidator::NFA(nfa) => {
                            if state.iter().any(|&state| nfa.is_accepted(state)) {
                                Ok(())
                            } else {
                                Err(ContentSpecValidationError::NotReachedAcceptedState)
                            }
                        }
                        ElementContentValidator::DFA(dfa) => {
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
