use std::sync::Arc;

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

#[derive(Debug, Clone)]
pub enum XMLError {
    // general errors
    InternalError,
    UnsupportedError,
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
    IOError(Arc<std::io::Error>),
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
    RngParseInsufficientStartInInclude,
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
    RngParseProhibitedPath,
    RngParseUngroupablePattern,
    RngParseConflictAttributeNameClass,
    RngParseUnrepeatedAttributeWithInfiniteNameClass,
    RngValidNotAllowed,
    RngValidEmpty,
    RngValidText,
    RngValidData,
    RngValidValue,
    RngValidList,
    RngValidAttribute,
    RngValidGroup,
    RngValidInterleave,
    RngValidRef,
    RngValidElement,
    RngValidOneOrMore,
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
        Self::IOError(Arc::new(value))
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
