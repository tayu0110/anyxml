use std::{borrow::Cow, path::Path, sync::Arc};

use crate::error::{XMLError, XMLErrorLevel};

#[derive(Debug)]
pub struct SAXParseError {
    pub error: XMLError,
    pub level: XMLErrorLevel,
    pub line: usize,
    pub column: usize,
    pub system_id: Arc<Path>,
    pub public_id: Option<Arc<str>>,
    pub message: Cow<'static, str>,
}

impl std::fmt::Display for SAXParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "{}[line:{},column:{}]:{}:{}",
            self.system_id.display(),
            self.line,
            self.column,
            self.level,
            self.message,
        )
    }
}

impl std::error::Error for SAXParseError {}

macro_rules! generic_error {
    ($method:ident, $handler:expr, $code:expr, $level:expr, $locator:expr, $message:literal, $( $args:expr ),+) => {
        $handler.$method($crate::sax::error::SAXParseError {
            error: $code,
            level: $level,
            line: $locator.line(),
            column: $locator.column(),
            system_id: $locator.system_id(),
            public_id: $locator.public_id(),
            message: ::std::borrow::Cow::Owned(format!($message, $( $args ),+)),
        })
    };
    ($method:ident, $handler:expr, $code:expr, $level:expr, $locator:expr, $message:literal) => {
        $handler.$method($crate::sax::error::SAXParseError {
            error: $code,
            level: $level,
            line: $locator.line(),
            column: $locator.column(),
            system_id: $locator.system_id(),
            public_id: $locator.public_id(),
            message: ::std::borrow::Cow::Borrowed($message),
        })
    };
    ($method:ident, $handler:expr, $code:expr, $level:expr, $locator:expr, $message:expr) => {
        $handler.$method($crate::sax::error::SAXParseError {
            error: $code,
            level: $level,
            line: $locator.line(),
            column: $locator.column(),
            system_id: $locator.system_id(),
            public_id: $locator.public_id(),
            message: ::std::borrow::Cow::Owned($message.into()),
        })
    };
    ($method:ident, $handler:expr, $code:expr, $level:expr, $locator:expr) => {
        $handler.$method($crate::sax::error::SAXParseError {
            error: $code,
            level: $level,
            line: $locator.line(),
            column: $locator.column(),
            system_id: $locator.system_id(),
            public_id: $locator.public_id(),
            message: ::std::borrow::Cow::Borrowed("No messages"),
        })
    };
}

macro_rules! fatal_error {
    ($handler:expr, $code:ident, $locator:expr, $message:literal, $( $args:expr ),+) => {
        $crate::sax::error::generic_error!(fatal_error, $handler, $crate::error::XMLError::$code, $crate::error::XMLErrorLevel::FatalError, $locator, $message, $( $args ),+);
    };
    ($handler:expr, $code:ident, $locator:expr, $message:literal) => {
        $crate::sax::error::generic_error!(fatal_error, $handler, $crate::error::XMLError::$code, $crate::error::XMLErrorLevel::FatalError, $locator, $message);
    };
    ($handler:expr, $code:ident, $locator:expr, $message:expr) => {
        $crate::sax::error::generic_error!(fatal_error, $handler, $crate::error::XMLError::$code, $crate::error::XMLErrorLevel::FatalError, $locator, $message);
    };
    ($handler:expr, $code:ident, $locator:expr) => {
        $crate::sax::error::generic_error!(fatal_error, $handler, $crate::error::XMLError::$code, $crate::error::XMLErrorLevel::FatalError, $locator);
    };
}

macro_rules! error {
    ($handler:expr, $code:ident, $locator:expr, $message:literal, $( $args:expr ),+) => {
        #[allow(unused)]
        use $crate::error::XMLError::*;
        $crate::sax::error::generic_error!(error, $handler, $code, $crate::error::XMLErrorLevel::Error, $locator, $message, $( $args ),+);
    };
    ($handler:expr, $code:ident, $locator:expr, $message:literal) => {
        $crate::sax::error::generic_error!(error, $handler, $crate::error::XMLError::$code, $crate::error::XMLErrorLevel::Error, $locator, $message);
    };
    ($handler:expr, $code:ident, $locator:expr, $message:expr) => {
        $crate::sax::error::generic_error!(error, $handler, $crate::error::XMLError::$code, $crate::error::XMLErrorLevel::Error, $locator, $message);
    };
    ($handler:expr, $code:ident, $locator:expr) => {
        $crate::sax::error::generic_error!(error, $handler, $crate::error::XMLError::$code, $crate::error::XMLErrorLevel::Error, $locator);
    };
}

macro_rules! warning {
    ($handler:expr, $code:ident, $locator:expr, $message:literal, $( $args:expr ),+) => {
        $crate::sax::error::generic_error!(warning, $handler, $crate::error::XMLError::$code, $crate::error::XMLErrorLevel::Warning, $locator, $message, $( $args ),+);
    };
    ($handler:expr, $code:ident, $locator:expr, $message:literal) => {
        $crate::sax::error::generic_error!(warning, $handler, $crate::error::XMLError::$code, $crate::error::XMLErrorLevel::Warning, $locator, $message);
    };
    ($handler:expr, $code:ident, $locator:expr, $message:expr) => {
        $crate::sax::error::generic_error!(warning, $handler, $crate::error::XMLError::$code, $crate::error::XMLErrorLevel::Warning, $locator, $message);
    };
    ($handler:expr, $code:ident, $locator:expr) => {
        $crate::sax::error::generic_error!(warning, $handler, $crate::error::XMLError::$code, $crate::error::XMLErrorLevel::Warning, $locator);
    };
}

pub(crate) use {error, fatal_error, generic_error, warning};
