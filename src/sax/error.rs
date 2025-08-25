use std::{borrow::Cow, sync::Arc};

use anyxml_uri::uri::URIStr;

use crate::error::{XMLError, XMLErrorLevel};

#[derive(Debug)]
pub struct SAXParseError {
    pub error: XMLError,
    pub level: XMLErrorLevel,
    pub line: usize,
    pub column: usize,
    pub system_id: Arc<URIStr>,
    pub public_id: Option<Arc<str>>,
    pub message: Cow<'static, str>,
}

impl std::fmt::Display for SAXParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(unescaped) = self.system_id.as_unescaped_str() {
            write!(
                f,
                "{}[line:{},column:{}][{}] {}",
                unescaped, self.line, self.column, self.level, self.message,
            )
        } else {
            write!(
                f,
                "{}[line:{},column:{}]:{}:{}",
                self.system_id.as_escaped_str(),
                self.line,
                self.column,
                self.level,
                self.message,
            )
        }
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
    ($reader:expr, $code:ident, $message:literal, $( $args:expr ),+) => {
        $crate::sax::error::generic_error!(fatal_error, $reader.error_handler, $crate::error::XMLError::$code, $crate::error::XMLErrorLevel::FatalError, $reader.locator, $message, $( $args ),+);
        $reader.fatal_error_occurred = true;
    };
    ($reader:expr, $code:ident, $message:literal) => {
        $crate::sax::error::generic_error!(fatal_error, $reader.error_handler, $crate::error::XMLError::$code, $crate::error::XMLErrorLevel::FatalError, $reader.locator, $message);
        $reader.fatal_error_occurred = true;
    };
    ($reader:expr, $code:ident, $message:expr) => {
        $crate::sax::error::generic_error!(fatal_error, $reader.error_handler, $crate::error::XMLError::$code, $crate::error::XMLErrorLevel::FatalError, $reader.locator, $message);
        $reader.fatal_error_occurred = true;
    };
    ($reader:expr, $code:ident) => {
        $crate::sax::error::generic_error!(fatal_error, $reader.error_handler, $crate::error::XMLError::$code, $crate::error::XMLErrorLevel::FatalError, $reader.locator);
        $reader.fatal_error_occurred = true;
    };
}

macro_rules! error {
    ($reader:expr, $code:ident, $message:literal, $( $args:expr ),+) => {
        #[allow(unused)]
        use $crate::error::XMLError::*;
        $crate::sax::error::generic_error!(error, $reader.error_handler, $code, $crate::error::XMLErrorLevel::Error, $reader.locator, $message, $( $args ),+);
    };
    ($reader:expr, $code:ident, $message:literal) => {
        $crate::sax::error::generic_error!(error, $reader.error_handler, $crate::error::XMLError::$code, $crate::error::XMLErrorLevel::Error, $reader.locator, $message);
    };
    ($reader:expr, $code:ident, $message:expr) => {
        $crate::sax::error::generic_error!(error, $reader.error_handler, $crate::error::XMLError::$code, $crate::error::XMLErrorLevel::Error, $reader.locator, $message);
    };
    ($reader:expr, $code:ident) => {
        $crate::sax::error::generic_error!(error, $reader.error_handler, $crate::error::XMLError::$code, $crate::error::XMLErrorLevel::Error, $reader.locator);
    };
}

macro_rules! warning {
    ($reader:expr, $code:ident, $message:literal, $( $args:expr ),+) => {
        $crate::sax::error::generic_error!(warning, $reader.error_handler, $crate::error::XMLError::$code, $crate::error::XMLErrorLevel::Warning, $reader.locator, $message, $( $args ),+);
    };
    ($reader:expr, $code:ident, $message:literal) => {
        $crate::sax::error::generic_error!(warning, $reader.error_handler, $crate::error::XMLError::$code, $crate::error::XMLErrorLevel::Warning, $reader.locator, $message);
    };
    ($reader:expr, $code:ident, $message:expr) => {
        $crate::sax::error::generic_error!(warning, $reader.error_handler, $crate::error::XMLError::$code, $crate::error::XMLErrorLevel::Warning, $reader.locator, $message);
    };
    ($reader:expr, $code:ident) => {
        $crate::sax::error::generic_error!(warning, $reader.error_handler, $crate::error::XMLError::$code, $crate::error::XMLErrorLevel::Warning, $reader.locator);
    };
}

pub(crate) use {error, fatal_error, generic_error, warning};
