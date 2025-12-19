use std::{borrow::Cow, sync::Arc};

use crate::{
    error::{XMLError, XMLErrorDomain, XMLErrorLevel},
    uri::URIStr,
};

#[derive(Debug)]
pub struct SAXParseError {
    pub error: XMLError,
    pub level: XMLErrorLevel,
    pub domain: XMLErrorDomain,
    pub line: usize,
    pub column: usize,
    pub system_id: Arc<URIStr>,
    pub public_id: Option<Arc<str>>,
    pub message: Cow<'static, str>,
}

impl std::fmt::Display for SAXParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let system_id = self
            .system_id
            .as_unescaped_str()
            .unwrap_or(self.system_id.as_escaped_str().into());
        let last = system_id
            .rsplit_once('/')
            .map(|ret| ret.1)
            .unwrap_or(system_id.as_ref());
        write!(
            f,
            "{}[line:{},column:{}][{}] {}",
            last, self.line, self.column, self.level, self.message,
        )
    }
}

impl std::error::Error for SAXParseError {}

macro_rules! generic_error {
    ($method:ident, $handler:expr, $code:expr, $level:expr, $domain:expr, $locator:expr, $message:literal, $( $args:expr ),*) => {
        #[allow(unused)]
        use $crate::error::XMLError::*;
        $handler.$method($crate::sax::error::SAXParseError {
            error: $code,
            level: $level,
            domain: $domain,
            line: $locator.line(),
            column: $locator.column(),
            system_id: $locator.system_id(),
            public_id: $locator.public_id(),
            message: ::std::borrow::Cow::Owned(format!($message, $( $args ),*)),
        })
    };
    ($method:ident, $handler:expr, $code:expr, $level:expr, $domain:expr, $locator:expr, $message:literal) => {
        $handler.$method($crate::sax::error::SAXParseError {
            error: $code,
            level: $level,
            domain: $domain,
            line: $locator.line(),
            column: $locator.column(),
            system_id: $locator.system_id(),
            public_id: $locator.public_id(),
            message: ::std::borrow::Cow::Borrowed($message),
        })
    };
    ($method:ident, $handler:expr, $code:expr, $level:expr, $domain:expr, $locator:expr, $message:expr) => {
        $handler.$method($crate::sax::error::SAXParseError {
            error: $code,
            level: $level,
            domain: $domain,
            line: $locator.line(),
            column: $locator.column(),
            system_id: $locator.system_id(),
            public_id: $locator.public_id(),
            message: ::std::borrow::Cow::Owned($message.into()),
        })
    };
}

macro_rules! fatal_error {
    ($reader:expr, $code:ident, $message:literal, $( $args:expr ),*) => {
        #[allow(unused)]
        use $crate::error::XMLError::*;
        $crate::sax::error::generic_error!(
            fatal_error,
            $reader.handler,
            $code.clone(),
            $crate::error::XMLErrorLevel::FatalError,
            $crate::error::XMLErrorDomain::Parser,
            $reader.locator,
            $message,
            $( $args ),*
        );
        $reader.fatal_error_occurred = true;
    };
    ($reader:expr, $code:ident, $message:literal) => {
        $crate::sax::error::fatal_error!($reader, $code, $message, );
    };
    ($reader:expr, $code:ident, $message:expr) => {
        $crate::sax::error::generic_error!(
            fatal_error,
            $reader.handler,
            $crate::error::XMLError::$code,
            $crate::error::XMLErrorLevel::FatalError,
            $crate::error::XMLErrorDomain::Parser,
            $reader.locator,
            $message
        );
        $reader.fatal_error_occurred = true;
    };
}

macro_rules! error {
    ($reader:expr, $code:ident, $message:literal, $( $args:expr ),*) => {
        #[allow(unused)]
        use $crate::error::XMLError::*;
        $crate::sax::error::generic_error!(
            error,
            $reader.handler,
            $code,
            $crate::error::XMLErrorLevel::Error,
            $crate::error::XMLErrorDomain::Parser,
            $reader.locator,
            $message,
            $( $args ),*
        );
    };
    ($reader:expr, $code:ident, $message:literal) => {
        $crate::sax::error::error!($reader, $code, $message, );
    };
    ($reader:expr, $code:ident, $message:expr) => {
        $crate::sax::error::generic_error!(
            error,
            $reader.handler,
            $crate::error::XMLError::$code,
            $crate::error::XMLErrorLevel::Error,
            $crate::error::XMLErrorDomain::Parser,
            $reader.locator,
            $message
        );
    };
}

macro_rules! warning {
    ($reader:expr, $code:ident, $message:literal, $( $args:expr ),*) => {
        $crate::sax::error::generic_error!(
            warning,
            $reader.handler,
            $crate::error::XMLError::$code,
            $crate::error::XMLErrorLevel::Warning,
            $crate::error::XMLErrorDomain::Parser,
            $reader.locator,
            $message,
            $( $args ),*
        );
    };
    ($reader:expr, $code:ident, $message:literal) => {
        $crate::sax::error::warning!($reader, $code, $message, );
    };
    ($reader:expr, $code:ident, $message:expr) => {
        $crate::sax::error::generic_error!(
            warning,
            $reader.handler,
            $crate::error::XMLError::$code,
            $crate::error::XMLErrorLevel::Warning,
            $crate::error::XMLErrorDomain::Parser,
            $reader.locator,
            $message
        );
    };
}

macro_rules! ns_error {
    ($reader:expr, $code:ident, $message:literal, $( $args:expr ),*) => {
        #[allow(unused)]
        use $crate::error::XMLError::*;
        $crate::sax::error::generic_error!(
            error,
            $reader.handler,
            $code,
            $crate::error::XMLErrorLevel::Error,
            $crate::error::XMLErrorDomain::Namespace,
            $reader.locator,
            $message,
            $( $args ),*
        );
    };
    ($reader:expr, $code:ident, $message:literal) => {
        $crate::sax::error::ns_error!($reader, $code, $message, );
    };
    ($reader:expr, $code:ident, $message:expr) => {
        $crate::sax::error::generic_error!(
            error,
            $reader.handler,
            $crate::error::XMLError::$code,
            $crate::error::XMLErrorLevel::Error,
            $crate::error::XMLErrorDomain::Namespace,
            $reader.locator,
            $message
        );
    };
}

macro_rules! validity_error {
    ($reader:expr, $code:ident, $message:literal, $( $args:expr ),*) => {
        $crate::sax::error::generic_error!(
            error,
            $reader.handler,
            $code,
            $crate::error::XMLErrorLevel::Error,
            $crate::error::XMLErrorDomain::DTDValid,
            $reader.locator,
            $message,
            $( $args ),*
        );
    };
    ($reader:expr, $code:ident, $message:literal) => {
        $crate::sax::error::validity_error!($reader, $code, $message, );
    };
    ($reader:expr, $code:ident, $message:expr) => {
        $crate::sax::error::generic_error!(
            error,
            $reader.handler,
            $code,
            $crate::error::XMLErrorLevel::Error,
            $crate::error::XMLErrorDomain::DTDValid,
            $reader.locator,
            $message
        );
    };
}

pub(crate) use {error, fatal_error, generic_error, ns_error, validity_error, warning};
