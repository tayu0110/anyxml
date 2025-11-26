use crate::{
    encoding::{DecodeError, EncodeError},
    tree::XMLTreeError,
    uri::ParseRIError,
    xpath::{XPathCompileError, XPathError},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum XMLErrorLevel {
    FatalError,
    Error,
    Warning,
}

impl std::fmt::Display for XMLErrorLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Self::FatalError => write!(f, "fatal error"),
            Self::Error => write!(f, "error"),
            Self::Warning => write!(f, "warning"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum XMLErrorDomain {
    Parser,
    Namespace,
    DTDValid,
    RngParser,
}

impl std::fmt::Display for XMLErrorDomain {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Self::Parser => write!(f, "parser"),
            Self::Namespace => write!(f, "namespace"),
            Self::DTDValid => write!(f, "dtd-valid"),
            Self::RngParser => write!(f, "relaxng-parser"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum XMLError {
    // general errors
    InternalError,
    // parser errors
    ParserUnknownError,
    ParserUnsupportedEncoding,
    ParserUnsupportedXMLVersion,
    ParserTooLongXMLVersionNumber,
    PraserTooLongEncodingName,
    ParserEmptyNmtoken,
    ParserEmptyName,
    ParserEmptyNCName,
    ParserEmptyQName,
    ParserEmptyQNamePrefix,
    ParserEmptyQNameLocalPart,
    ParserEmptyNmtokens,
    ParserEmptyNames,
    ParserInvalidNameStartChar,
    ParserInvalidNameChar,
    ParserInvalidNCNameStartChar,
    ParserInvalidNCNameChar,
    ParserInvalidQNameSeparator,
    ParserIncorrectLiteralQuotation,
    ParserInvalidSystemLiteral,
    ParserSystemLiteralWithFragment,
    ParserInvalidPubidLiteral,
    ParserInvalidAttValue,
    ParserInvalidExternalID,
    ParserInvalidCharacter,
    ParserInvalidXMLDecl,
    ParserInvalidTextDecl,
    ParserInvalidXMLVersion,
    ParserInvalidEncodingDecl,
    ParserInvalidEncodingName,
    ParserInvalidSDDecl,
    ParserInvalidComment,
    ParserInvalidCDSect,
    ParserInvalidStandaloneDocument,
    ParserInvalidProcessingInstruction,
    ParserUnacceptableCatalogPIPosition,
    ParserInvalidCatalogPIAttribute,
    ParserUnacceptablePITarget,
    ParserUnacceptablePatternInCharData,
    ParserInvalidDoctypeDecl,
    ParserInvalidElementDecl,
    ParserDuplicateElementDecl,
    ParserAmbiguousElementContentModel,
    ParserDuplicateMixedContent,
    ParserInvalidAttlistDecl,
    ParserMismatchXMLIDAttributeType,
    ParserDuplicateAttlistDecl,
    ParserDuplicateTokensInAttlistDecl,
    ParserMultipleIDAttributePerElement,
    ParserInvalidIDAttributeValue,
    ParserInvalidIDREFAttributeValue,
    ParserNotationAttlistDeclOnEmptyElement,
    ParserSyntaxticallyIncorrectAttributeDefault,
    ParserInvalidEntityDecl,
    ParserDuplicateEntityDecl,
    ParserInvalidNotationDecl,
    ParserDuplicateNotationDecl,
    ParserUndeclaredNotation,
    ParserInvalidConditionalSect,
    ParserInvalidStartOrEmptyTag,
    ParserInvalidEndTag,
    ParserUnclosedStartTag,
    ParserMismatchElementType,
    ParserMismatchElementContentModel,
    ParserUndeclaredElement,
    ParserInvalidAttribute,
    ParserUndeclaredAttribute,
    ParserDuplicateAttributes,
    ParserDuplicateIDAttribute,
    ParserUnresolvableIDReference,
    ParserMultipleNotationAttributePerElement,
    ParserUnacceptableNotationAttribute,
    ParserUnacceptableEnumerationAttribute,
    ParserMismatchFixedDefaultAttributeValue,
    ParserRequiredAttributeNotFound,
    ParserUnacceptableXMLSpaceAttribute,
    ParserInvalidCharacterReference,
    ParserInvalidEntityReference,
    ParserEntityNotFound,
    ParserEntityRecursion,
    ParserEntityIncorrectNesting,
    ParserUndeclaredEntityReference,
    ParserNamespaceNameNotURI,
    ParserNamespaceNameNotAbsoluteURI,
    ParserUnacceptableNamespaceName,
    ParserUndefinedNamespace,
    ParserUnexpectedDocumentContent,
    ParserUnexpectedEOF,
    // I/O errors
    IOUnknownError,
    IONotFound,
    IOPermissionDenied,
    IOConnectionRefused,
    IOConnectionReset,
    IOHostUnreachable,
    IONetworkUnreachable,
    IOConnectionAborted,
    IONotConnected,
    IOAddrInUse,
    IOAddrNotAvailable,
    IONetworkDown,
    IOBrokenPipe,
    IOAlreadyExists,
    IOWouldBlock,
    IONotADirectory,
    IOIsADirectory,
    IODirectoryNotEmpty,
    IOReadOnlyFilesystem,
    IOFilesystemLoop,
    IOStaleNetworkFileHandle,
    IOInvalidInput,
    IOInvalidData,
    IOTimedOut,
    IOWriteZero,
    IOStorageFull,
    IONotSeekable,
    IOQuotaExceeded,
    IOFileTooLarge,
    IOResourceBusy,
    IOExecutableFileBusy,
    IODeadlock,
    IOCrossesDevices,
    IOTooManyLinks,
    IOInvalidFilename,
    IOArgumentListTooLong,
    IOInterrupted,
    IOUnsupported,
    IOUnexpectedEof,
    IOOutOfMemory,
    IOInProgress,
    // encoding errors
    EncoderInputIsEmpty,
    EncoderOutputTooShort,
    EncoderUnmappable {
        read: usize,
        write: usize,
        c: char,
    },
    EncoderUnknownError,
    DecoderInputIsEmpty,
    DecoderOutputTooShort,
    DecoderMalformed {
        read: usize,
        write: usize,
        length: usize,
        offset: usize,
    },
    DecoderUnknownError,
    // URI errors
    URIParseFailure,
    URIBaseURINotAbsolute,
    URIBaseURINotFound,
    // Tree errors
    TreeError(XMLTreeError),
    // XPath errors
    XPathError(XPathError),
    // Catalog errors
    CatalogInvalidPublicID,
    CatalogResourceFailure,
    // RELAX NG errors
    RngParseUnacceptablePattern,
    RngParseUnacceptableCombine,
    RngParseUnacceptableAttribute,
    RngParseUnacceptableString,
    RngParseInvalidNCName,
    RngParseInvalidQName,
    RngParseInvalidAnyURI,
    RngParseUnresolvableNamespacePrefix,
    RngParseUnresolvableDatatype,
    RngParseUnresolvableDatatypeLibrary,
    RngParseUnresolvableRefName,
    RngParseUnresolvableParentRefName,
    RngParseDatatypeLibraryURINotAbsolute,
    RngParseInsufficientAttribute,
    RngParseInsufficientDefineInInclude,
    RngParseExternalRefParseFailure,
    RngParseExternalRefLoop,
    RngParseIncludeParseFailure,
    RngParseIncludeLoop,
    RngParseHRefIncludeFragment,
    RngParseMultipleStartWithoutCombine,
    RngParseMultipleDefineWithoutCombine,
    RngParseStartNotFoundInGrammar,
    RngParseRefLoop,
    RngParseUnknownError,
}

impl std::fmt::Display for XMLError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl std::error::Error for XMLError {}

impl From<std::io::Error> for XMLError {
    fn from(value: std::io::Error) -> Self {
        use std::io::ErrorKind::*;
        match value.kind() {
            NotFound => XMLError::IONotFound,
            PermissionDenied => XMLError::IOPermissionDenied,
            ConnectionRefused => XMLError::IOConnectionRefused,
            ConnectionReset => XMLError::IOConnectionReset,
            HostUnreachable => XMLError::IOHostUnreachable,
            NetworkUnreachable => XMLError::IONetworkUnreachable,
            ConnectionAborted => XMLError::IOConnectionAborted,
            NotConnected => XMLError::IONotConnected,
            AddrInUse => XMLError::IOAddrInUse,
            AddrNotAvailable => XMLError::IOAddrNotAvailable,
            NetworkDown => XMLError::IONetworkDown,
            BrokenPipe => XMLError::IOBrokenPipe,
            AlreadyExists => XMLError::IOAlreadyExists,
            WouldBlock => XMLError::IOWouldBlock,
            NotADirectory => XMLError::IONotADirectory,
            IsADirectory => XMLError::IOIsADirectory,
            DirectoryNotEmpty => XMLError::IODirectoryNotEmpty,
            ReadOnlyFilesystem => XMLError::IOReadOnlyFilesystem,
            StaleNetworkFileHandle => XMLError::IOStaleNetworkFileHandle,
            InvalidInput => XMLError::IOInvalidInput,
            InvalidData => XMLError::IOInvalidData,
            TimedOut => XMLError::IOTimedOut,
            WriteZero => XMLError::IOWriteZero,
            StorageFull => XMLError::IOStorageFull,
            NotSeekable => XMLError::IONotSeekable,
            QuotaExceeded => XMLError::IOQuotaExceeded,
            FileTooLarge => XMLError::IOFileTooLarge,
            ResourceBusy => XMLError::IOResourceBusy,
            ExecutableFileBusy => XMLError::IOExecutableFileBusy,
            Deadlock => XMLError::IODeadlock,
            CrossesDevices => XMLError::IOCrossesDevices,
            TooManyLinks => XMLError::IOTooManyLinks,
            InvalidFilename => XMLError::IOInvalidFilename,
            ArgumentListTooLong => XMLError::IOArgumentListTooLong,
            Interrupted => XMLError::IOInterrupted,
            Unsupported => XMLError::IOUnsupported,
            UnexpectedEof => XMLError::IOUnexpectedEof,
            OutOfMemory => XMLError::IOOutOfMemory,
            Other => XMLError::IOUnknownError,
            _ => unimplemented!(),
        }
    }
}

impl From<EncodeError> for XMLError {
    fn from(value: EncodeError) -> Self {
        use EncodeError::*;
        match value {
            InputIsEmpty => XMLError::EncoderInputIsEmpty,
            OutputTooShort => XMLError::EncoderOutputTooShort,
            Unmappable { read, write, c } => XMLError::EncoderUnmappable { read, write, c },
            Other { msg: _ } => XMLError::EncoderUnknownError,
        }
    }
}

impl From<DecodeError> for XMLError {
    fn from(value: DecodeError) -> Self {
        use DecodeError::*;
        match value {
            InputIsEmpty => XMLError::DecoderInputIsEmpty,
            OutputTooShort => XMLError::DecoderOutputTooShort,
            Malformed {
                read,
                write,
                length,
                offset,
            } => XMLError::DecoderMalformed {
                read,
                write,
                length,
                offset,
            },
            Other { msg: _ } => XMLError::DecoderUnknownError,
        }
    }
}

impl From<ParseRIError> for XMLError {
    fn from(_: ParseRIError) -> Self {
        Self::URIParseFailure
    }
}

impl From<XMLTreeError> for XMLError {
    fn from(value: XMLTreeError) -> Self {
        Self::TreeError(value)
    }
}

impl From<XPathError> for XMLError {
    fn from(value: XPathError) -> Self {
        Self::XPathError(value)
    }
}

impl From<XPathCompileError> for XMLError {
    fn from(value: XPathCompileError) -> Self {
        XPathError::from(value).into()
    }
}
