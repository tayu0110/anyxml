use std::sync::Arc;

use crate::{
    encoding::{DecodeError, EncodeError},
    tree::XMLTreeError,
    uri::ParseRIError,
    xinclude::XIncludeError,
    xpath::{XPathCompileError, XPathError},
    xpointer::XPointerParseError,
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
    C14N,
    XInclude,
}

impl std::fmt::Display for XMLErrorDomain {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Self::Parser => write!(f, "parser"),
            Self::Namespace => write!(f, "namespace"),
            Self::DTDValid => write!(f, "dtd-valid"),
            Self::RngParser => write!(f, "relaxng-parser"),
            Self::C14N => write!(f, "c14n"),
            Self::XInclude => write!(f, "xinclude"),
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
    EncodeError(EncodeError),
    EncoderUnknownError,
    DecodeError(DecodeError),
    DecoderUnknownError,
    // URI errors
    URIParseError(ParseRIError),
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
    RngParseUnknownError,
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
    RngValidUnknownError,
    // XPointer errors
    XPointerParseError(XPointerParseError),
    // XInclude errors
    XIncludeError(XIncludeError),
    // C14N errors
    C14NUnresolvableEntityReference,
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
        Self::EncodeError(value)
    }
}

impl From<DecodeError> for XMLError {
    fn from(value: DecodeError) -> Self {
        Self::DecodeError(value)
    }
}

impl From<ParseRIError> for XMLError {
    fn from(value: ParseRIError) -> Self {
        Self::URIParseError(value)
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

impl From<XPointerParseError> for XMLError {
    fn from(value: XPointerParseError) -> Self {
        XMLError::XPointerParseError(value)
    }
}

impl From<XIncludeError> for XMLError {
    fn from(value: XIncludeError) -> Self {
        XMLError::XIncludeError(value)
    }
}
