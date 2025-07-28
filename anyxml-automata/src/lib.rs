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
