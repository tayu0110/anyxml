pub mod encoding;
pub mod error;
pub mod sax;

use std::{borrow::Cow, marker::PhantomData};

use crate::sax::source::InputSource;

pub trait ParserSpec {
    type Reader;
}

pub struct DefaultParserSpec<'a> {
    _phantom: PhantomData<&'a ()>,
}

impl<'a> ParserSpec for DefaultParserSpec<'a> {
    type Reader = InputSource<'a>;
}

pub struct ProgressiveParserSpec;

impl ParserSpec for ProgressiveParserSpec {
    type Reader = InputSource<'static>;
}

pub struct Attribute<'a> {
    uri: Option<Cow<'a, str>>,
    local_name: Option<Cow<'a, str>>,
    qname: Cow<'a, str>,
    value: Cow<'a, str>,
    // 0: is declared in DTD
    // 1: is specified explicitly (in other words, `value` is not the default value provided by DTD)
    flag: u8,
}

pub struct Locator {
    // todo!!!!
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AttributeType {
    // todo!!!!
}

pub enum DefaultDecl {
    // todo!!!!
}

pub enum ContentSpec {
    // todo!!!!
}
