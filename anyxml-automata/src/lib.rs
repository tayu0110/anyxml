//! Provides implementation of finite automata.
//!
//! One goal is to provide content models in DTD element type declarations
//! and regular expression implementations that conform to the XML Schema specification.  
//!
//! In addition, we provide an AST that expresses such regular expressions and a library
//! that constructs finite automata from the AST.  
//! Users of the library can use these features to implement their own regular expression parsers.

pub mod ast;
pub mod content_model;
pub mod fa;
pub mod unicode;
pub mod util;
pub mod xsregexp;

pub trait Atom: Clone + Copy + PartialOrd + Ord + PartialEq + Eq + Default {
    const MIN: Self;
    const MAX: Self;
    const EPSILON: Self;

    fn previous(&self) -> Option<Self>;
    fn next(&self) -> Option<Self>;
}

impl Atom for char {
    const MIN: Self = '\x01';
    const MAX: Self = char::MAX;
    // Since NULL characters are not used in XML, it should be fine for internal use...
    const EPSILON: Self = char::MIN;

    fn previous(&self) -> Option<Self> {
        (char::MIN..*self).next_back()
    }

    fn next(&self) -> Option<Self> {
        let mut range = *self..;
        range.next(); // current char
        range.next()
    }
}
