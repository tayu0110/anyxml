pub mod tokens;

use crate::{
    DefaultParserSpec, ENCODING_NAME_LIMIT_LENGTH, XML_VERSION_NUM_LIMIT_LENGTH, XMLVersion,
    error::XMLError,
    sax::{
        error::{fatal_error, warning},
        parser::{ParserState, XMLReader},
    },
};

impl XMLReader<DefaultParserSpec<'_>> {
    pub(crate) fn parse_document(&mut self) -> Result<(), XMLError> {
        self.content_handler
            .set_document_locator(self.locator.clone());
        self.content_handler.start_document();
        self.parse_prolog()?;
        self.parse_element()?;
        self.parse_misc()?;
        self.content_handler().end_document();
        todo!()
    }

    pub(crate) fn parse_prolog(&mut self) -> Result<(), XMLError> {
        self.parse_xmldecl()?;
        self.state = ParserState::Parsing;
        todo!()
    }

    pub(crate) fn parse_xmldecl(&mut self) -> Result<(), XMLError> {
        self.state = ParserState::InXMLDeclaration;
        self.grow()?;
        if !self.source.content_bytes().starts_with(b"<?xml") {
            return Ok(());
        }
        // skip '<?xml'
        self.source.advance(5)?;
        self.locator.update_column(|c| c + 5);

        // parse VersionInfo
        let s = self.skip_whitespaces()?;
        if s == 0 {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidXMLDecl,
                self.locator,
                "Whitespaces are required between '<?xml' and 'version'."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidXMLDecl);
        }

        if !self.source.content_bytes().starts_with(b"version") {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidXMLDecl,
                self.locator,
                "VersionInfo is not found in XMLDecl."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidXMLDecl);
        }
        // skip 'version'
        self.source.advance(7)?;
        self.locator.update_column(|c| c + 7);

        self.skip_whitespaces()?;
        if !self.source.content_bytes().starts_with(b"=") {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidXMLDecl,
                self.locator,
                "'=' is not found after 'version' in XMLDecl."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidXMLDecl);
        }
        // skip '='
        self.source.advance(1)?;
        self.locator.update_column(|c| c + 1);
        self.skip_whitespaces()?;

        let quote = match self.source.next_char()? {
            Some(c @ ('"' | '\'')) => c,
            _ => {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserInvalidXMLDecl,
                    self.locator,
                    "The quotation marks in the version number are incorrect."
                );
                self.state = ParserState::FatalErrorOccurred;
                return Err(XMLError::ParserInvalidXMLDecl);
            }
        };
        self.locator.update_column(|c| c + 1);

        self.grow()?;
        let content = self.source.content_bytes();
        let limit = content.len().min(XML_VERSION_NUM_LIMIT_LENGTH);
        let mut major = 0;
        while major < limit && content[major].is_ascii_digit() {
            major += 1;
        }
        if major > 1 || content[0] != b'1' {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidXMLVersion,
                self.locator,
                "XML major version number must be '1'."
            );
            self.state = ParserState::FatalErrorOccurred;
        } else if content[major] != b'.' {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidXMLVersion,
                self.locator,
                "Invalid XML version number is found."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidXMLVersion);
        }
        let mut minor = major + 1;
        while minor < limit && content[minor].is_ascii_digit() {
            minor += 1;
        }
        if minor == limit {
            fatal_error!(
                self.error_handler,
                XMLError::ParserTooLongXMLVersionNumber,
                self.locator,
                "Too long XML version number is found."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserTooLongXMLVersionNumber);
        }
        let version = if major + 1 < minor {
            match content[..minor] {
                [b'1', b'.', b'0'] => XMLVersion::XML10,
                _ => XMLVersion::Unknown,
            }
        } else {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidXMLVersion,
                self.locator,
                "XML minor version number is not found."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidXMLVersion);
        };
        if version == XMLVersion::Unknown {
            warning!(
                self.error_handler,
                XMLError::ParserUnsupportedXMLVersion,
                self.locator,
                "Unsupported XML version number is found. Fallback to XML 1.0."
            );
        }
        if content[minor] != quote as u8 {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidXMLDecl,
                self.locator,
                "The quotation marks in the version number are incorrect."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidXMLDecl);
        }
        let version_str = unsafe {
            // # Safety
            // `content[..minor]` contains only ASCII digit or '.',
            // so this operation is safe.
            String::from_utf8_unchecked(content[..minor].to_owned())
        };
        self.source.advance(minor + 1)?;
        self.locator.update_column(|c| c + minor + 1);

        // parse EncodingDecl
        let mut s = self.skip_whitespaces()?;
        self.grow()?;
        let mut encoding = None;
        if self.source.content_bytes().starts_with(b"encoding") {
            if s == 0 {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserInvalidXMLDecl,
                    self.locator,
                    "Whitespaces are required before 'encoding'."
                );
                self.state = ParserState::FatalErrorOccurred;
            }
            encoding = Some(self.parse_encoding_decl(false)?);
            s = self.skip_whitespaces()?;
            self.grow()?;
        }

        // parse SDDecl
        let mut standalone = None;
        if self.source.content_bytes().starts_with(b"standalone") {
            if s == 0 {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserInvalidXMLDecl,
                    self.locator,
                    "Whitespaces are required before 'standalone'."
                );
                self.state = ParserState::FatalErrorOccurred;
            }
            standalone = Some(self.parse_sddecl(false)?);
            self.skip_whitespaces()?;
            self.grow()?;
        }

        if !self.source.content_bytes().starts_with(b"?>") {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidXMLDecl,
                self.locator,
                "XMLDecl is not closed with '?>'."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidXMLDecl);
        }
        // skip '?>'
        self.source.advance(2)?;
        self.locator.update_column(|c| c + 2);

        if self.state != ParserState::FatalErrorOccurred {
            self.content_handler
                .declaration(&version_str, encoding.as_deref(), standalone);
        }
        self.version = version;
        self.standalone = standalone;
        // If an encoding is provided from an external source, it is used as a priority.
        // If not, `self.encoding` is `None`, so `self.encoding` is initialized with the value
        // obtained from the XML declaration, and the decoder for `self.source` is switched.
        if let Some(encoding) = encoding
            && self.encoding.is_none()
        {
            self.encoding = Some(encoding);
        }

        Ok(())
    }

    pub(crate) fn parse_encoding_decl(
        &mut self,
        need_trim_whitespace: bool,
    ) -> Result<String, XMLError> {
        if need_trim_whitespace && self.skip_whitespaces()? == 0 {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidEncodingDecl,
                self.locator,
                "Whitespaces are required before 'encoding' for EncodingDecl."
            );
            self.state = ParserState::FatalErrorOccurred;
        }

        if !self.source.content_bytes().starts_with(b"encoding") {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidEncodingDecl,
                self.locator,
                "'encoding' is not found for EncodingDecl."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidEncodingDecl);
        }
        // skip 'encoding'
        self.source.advance(8)?;
        self.locator.update_column(|c| c + 8);

        self.skip_whitespaces()?;
        if self.source.next_char()? != Some('=') {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidEncodingDecl,
                self.locator,
                "'=' is not found after 'encoding' in EncodingDecl."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidEncodingDecl);
        }
        self.locator.update_column(|c| c + 1);
        self.skip_whitespaces()?;

        let quote = match self.source.next_char()? {
            Some(c @ ('"' | '\'')) => c,
            _ => {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserInvalidEncodingDecl,
                    self.locator,
                    "The quotation marks in the encoding name are incorrect."
                );
                self.state = ParserState::FatalErrorOccurred;
                return Err(XMLError::ParserInvalidEncodingDecl);
            }
        };
        self.locator.update_column(|c| c + 1);

        let encoding = self.parse_enc_name()?;
        self.grow()?;

        match self.source.next_char()? {
            Some(c) if c == quote => {
                self.locator.update_column(|c| c + 1);
            }
            Some(_) => {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserInvalidEncodingDecl,
                    self.locator,
                    "The quotation marks in the encoding name are incorrect."
                );
                self.state = ParserState::FatalErrorOccurred;
                return Err(XMLError::ParserInvalidEncodingDecl);
            }
            _ => {
                self.state = ParserState::FatalErrorOccurred;
                // If we call `grow` just before and `source` is empty,
                // `source` has probably reached EOF.
                return if self.source.is_empty() {
                    fatal_error!(
                        self.error_handler,
                        XMLError::ParserUnexpectedEOF,
                        self.locator,
                        "Unexpected EOF."
                    );
                    Err(XMLError::ParserUnexpectedEOF)
                } else {
                    fatal_error!(
                        self.error_handler,
                        XMLError::ParserInvalidEncodingDecl,
                        self.locator,
                        "The quotation marks in the encoding name are incorrect."
                    );
                    Err(XMLError::ParserInvalidEncodingDecl)
                };
            }
        }

        Ok(encoding)
    }

    pub(crate) fn parse_sddecl(&mut self, need_trim_whitespace: bool) -> Result<bool, XMLError> {
        if need_trim_whitespace && self.skip_whitespaces()? == 0 {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidSDDecl,
                self.locator,
                "Whitespaces are required before 'standalone' for SDDecl."
            );
            self.state = ParserState::FatalErrorOccurred;
        }

        self.grow()?;
        let content = self.source.content_bytes();
        if !content.starts_with(b"standalone") {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidSDDecl,
                self.locator,
                "'standalone' is not found for SDDecl."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidSDDecl);
        }
        // skip 'standalone'
        self.source.advance(10)?;
        self.locator.update_column(|c| c + 10);

        self.skip_whitespaces()?;
        self.grow()?;
        if !self.source.content_bytes().starts_with(b"=") {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidSDDecl,
                self.locator,
                "'=' is not found for SDDecl."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidSDDecl);
        }
        // skip '='
        self.source.advance(1)?;
        self.locator.update_column(|c| c + 10);

        let quote = match self.source.next_char()? {
            Some(c @ ('"' | '\'')) => c,
            _ => {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserInvalidSDDecl,
                    self.locator,
                    "The quotation marks in the standalone declaration are incorrect."
                );
                self.state = ParserState::FatalErrorOccurred;
                return Err(XMLError::ParserInvalidSDDecl);
            }
        };
        self.locator.update_column(|c| c + 1);

        let ret = match self.source.content_bytes() {
            [b'y', b'e', b's', ..] => {
                self.source.advance(3)?;
                self.locator.update_column(|c| c + 3);
                true
            }
            [b'n', b'o', ..] => {
                self.source.advance(2)?;
                self.locator.update_column(|c| c + 2);
                false
            }
            _ => {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserInvalidSDDecl,
                    self.locator,
                    "The value of SDDecl must be either 'yes' or 'no'."
                );
                self.state = ParserState::FatalErrorOccurred;
                return Err(XMLError::ParserInvalidXMLDecl);
            }
        };

        if self.source.next_char()? != Some(quote) {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidSDDecl,
                self.locator,
                "The quotation marks in the standalone declaration are incorrect."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::ParserInvalidSDDecl);
        }
        self.locator.update_column(|c| c + 1);

        Ok(ret)
    }

    pub(crate) fn parse_enc_name(&mut self) -> Result<String, XMLError> {
        self.grow()?;

        let content = self.source.content_bytes();
        if content.is_empty() {
            self.state = ParserState::FatalErrorOccurred;
            // If we call `grow` just before and `source` is empty,
            // `source` has probably reached EOF.
            return if self.source.is_empty() {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserUnexpectedEOF,
                    self.locator,
                    "Unexpected EOF."
                );
                Err(XMLError::ParserUnexpectedEOF)
            } else {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserInvalidEncodingName,
                    self.locator,
                    "Data that may not be accepted as an encoding name has been detected."
                );
                Err(XMLError::ParserInvalidEncodingName)
            };
        }

        if !content[0].is_ascii_alphabetic() {
            fatal_error!(
                self.error_handler,
                XMLError::ParserInvalidEncodingName,
                self.locator,
                "The first character of an encoding name must be ASCII alphabetic."
            );
            self.state = ParserState::FatalErrorOccurred;
        }

        let limit = ENCODING_NAME_LIMIT_LENGTH.min(content.len());
        let mut cur = 0;
        while cur < limit
            && (content[cur].is_ascii_alphanumeric() || matches!(content[cur], b'.' | b'_' | b'-'))
        {
            cur += 1;
        }

        if cur == ENCODING_NAME_LIMIT_LENGTH {
            fatal_error!(
                self.error_handler,
                XMLError::PraserTooLongEncodingName,
                self.locator,
                "Too long encoding name is found."
            );
            self.state = ParserState::FatalErrorOccurred;
            return Err(XMLError::PraserTooLongEncodingName);
        } else if cur == content.len() {
            self.state = ParserState::FatalErrorOccurred;
            return if self.source.is_empty() {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserUnexpectedEOF,
                    self.locator,
                    "Unexpected EOF."
                );
                Err(XMLError::ParserUnexpectedEOF)
            } else {
                fatal_error!(
                    self.error_handler,
                    XMLError::ParserInvalidEncodingName,
                    self.locator,
                    "Data that may not be accepted as an encoding name has been detected."
                );
                Err(XMLError::ParserInvalidEncodingName)
            };
        }

        let ret = unsafe {
            // # Safety
            // `content[..cur]` contains only ASCII characters,
            // so this operation is safe.
            String::from_utf8_unchecked(content[..cur].to_owned())
        };
        self.source.advance(cur)?;
        self.locator.update_column(|c| c + cur);
        Ok(ret)
    }

    pub(crate) fn parse_element(&mut self) -> Result<(), XMLError> {
        todo!()
    }

    pub(crate) fn parse_misc(&mut self) -> Result<(), XMLError> {
        todo!()
    }
}
