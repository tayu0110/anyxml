use crate::{
    ENCODING_NAME_LIMIT_LENGTH, ParserSpec, XML_VERSION_NUM_LIMIT_LENGTH, XMLVersion,
    error::XMLError,
    sax::{
        error::{fatal_error, warning},
        handler::SAXHandler,
        parser::{ParserState, XMLReader},
        source::InputSource,
    },
};

impl<'a, Spec: ParserSpec<Reader = InputSource<'a>>, H: SAXHandler> XMLReader<Spec, H> {
    /// ```text
    /// [23] XMLDecl ::= '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
    /// ```
    pub(crate) fn parse_xml_decl(&mut self) -> Result<(), XMLError> {
        self.state = ParserState::InXMLDeclaration;
        self.grow()?;
        if !self.source.content_bytes().starts_with(b"<?xml") {
            fatal_error!(
                self,
                ParserInvalidXMLDecl,
                "XML declaration must start with '<?xml'."
            );
            return Err(XMLError::ParserInvalidXMLDecl);
        }
        // skip '<?xml'
        self.source.advance(5)?;
        self.locator.update_column(|c| c + 5);

        // parse VersionInfo
        let (version, version_str) = self.parse_version_info(true, false)?;

        // parse EncodingDecl if exists
        let mut s = self.skip_whitespaces()?;
        self.grow()?;
        let mut encoding = None;
        if self.source.content_bytes().starts_with(b"encoding") {
            if s == 0 {
                fatal_error!(
                    self,
                    ParserInvalidXMLDecl,
                    "Whitespaces are required before 'encoding'."
                );
            }
            encoding = Some(self.parse_encoding_decl(false)?);
            s = self.skip_whitespaces()?;
            self.grow()?;
        }

        // parse SDDecl if exists
        let mut standalone = None;
        if self.source.content_bytes().starts_with(b"standalone") {
            if s == 0 {
                fatal_error!(
                    self,
                    ParserInvalidXMLDecl,
                    "Whitespaces are required before 'standalone'."
                );
            }
            standalone = Some(self.parse_sddecl(false)?);
            self.skip_whitespaces()?;
            self.grow()?;
        }

        if !self.source.content_bytes().starts_with(b"?>") {
            fatal_error!(
                self,
                ParserInvalidXMLDecl,
                "XMLDecl is not closed with '?>'."
            );
            return Err(XMLError::ParserInvalidXMLDecl);
        }
        // skip '?>'
        self.source.advance(2)?;
        self.locator.update_column(|c| c + 2);

        // If an encoding is provided from an external source, it is used as a priority.
        // If not, `self.encoding` is `None`, so `self.encoding` is initialized with the value
        // obtained from the XML declaration, and the decoder for `self.source` is switched.
        if let Some(encoding) = encoding.as_deref()
            && self.encoding.is_none()
            && let Err(err) = self.source.switch_encoding(encoding)
        {
            fatal_error!(
                self,
                ParserUnsupportedEncoding,
                "The declared encoding '{}' is not supported.",
                encoding
            );
            return Err(err);
        }

        if !self.fatal_error_occurred {
            self.handler
                .declaration(&version_str, encoding.as_deref(), standalone);
        }
        self.version = version;
        self.standalone = standalone;
        if self.encoding.is_none() {
            self.encoding = encoding;
        }
        Ok(())
    }

    /// ```text
    /// [24] VersionInfo ::= S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
    /// ```
    pub(crate) fn parse_version_info(
        &mut self,
        need_trim_whitespace: bool,
        text_decl: bool,
    ) -> Result<(XMLVersion, String), XMLError> {
        if need_trim_whitespace && self.skip_whitespaces()? == 0 {
            fatal_error!(
                self,
                ParserInvalidEncodingDecl,
                "Whitespaces are required before 'version' for VersionDecl."
            );
        }

        if !self.source.content_bytes().starts_with(b"version") {
            fatal_error!(
                self,
                ParserInvalidXMLVersion,
                "VersionInfo must start with 'version'."
            );
            return Err(XMLError::ParserInvalidXMLVersion);
        }
        // skip 'version'
        self.source.advance(7)?;
        self.locator.update_column(|c| c + 7);

        self.skip_whitespaces()?;
        if !self.source.content_bytes().starts_with(b"=") {
            fatal_error!(
                self,
                ParserInvalidXMLDecl,
                "'=' is not found after 'version' in VersionInfo."
            );
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
                    self,
                    ParserInvalidXMLDecl,
                    "The quotation marks in the version number are incorrect."
                );
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
                self,
                ParserInvalidXMLVersion,
                "XML major version number must be '1'."
            );
        } else if content[major] != b'.' {
            fatal_error!(
                self,
                ParserInvalidXMLVersion,
                "Invalid XML version number is found."
            );
            return Err(XMLError::ParserInvalidXMLVersion);
        }
        let mut minor = major + 1;
        while minor < limit && content[minor].is_ascii_digit() {
            minor += 1;
        }
        if minor == limit {
            fatal_error!(
                self,
                ParserTooLongXMLVersionNumber,
                "Too long XML version number is found."
            );
            return Err(XMLError::ParserTooLongXMLVersionNumber);
        }
        let version = if major + 1 < minor {
            match content[..minor] {
                [b'1', b'.', b'0'] => XMLVersion::XML10,
                _ => XMLVersion::Unknown,
            }
        } else {
            fatal_error!(
                self,
                ParserInvalidXMLVersion,
                "XML minor version number is not found."
            );
            return Err(XMLError::ParserInvalidXMLVersion);
        };
        if text_decl && version != self.version {
            fatal_error!(
                self,
                ParserUnsupportedXMLVersion,
                "XML {} document must not refer to XML {} entity.",
                self.version,
                version
            );
        } else if version == XMLVersion::Unknown {
            warning!(
                self,
                ParserUnsupportedXMLVersion,
                "Unsupported XML version number is found. Fallback to XML 1.0."
            );
        }
        if content[minor] != quote as u8 {
            fatal_error!(
                self,
                ParserInvalidXMLDecl,
                "The quotation marks in the version number are incorrect."
            );
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

        Ok((version, version_str))
    }

    /// ```text
    /// [80] EncodingDecl ::= S 'encoding' Eq ('"' EncName '"' | "'" EncName "'" )
    /// ```
    pub(crate) fn parse_encoding_decl(
        &mut self,
        need_trim_whitespace: bool,
    ) -> Result<String, XMLError> {
        if need_trim_whitespace && self.skip_whitespaces()? == 0 {
            fatal_error!(
                self,
                ParserInvalidEncodingDecl,
                "Whitespaces are required before 'encoding' for EncodingDecl."
            );
        }

        if !self.source.content_bytes().starts_with(b"encoding") {
            fatal_error!(
                self,
                ParserInvalidEncodingDecl,
                "'encoding' is not found for EncodingDecl."
            );
            return Err(XMLError::ParserInvalidEncodingDecl);
        }
        // skip 'encoding'
        self.source.advance(8)?;
        self.locator.update_column(|c| c + 8);

        self.skip_whitespaces()?;
        if self.source.next_char()? != Some('=') {
            fatal_error!(
                self,
                ParserInvalidEncodingDecl,
                "'=' is not found after 'encoding' in EncodingDecl."
            );
            return Err(XMLError::ParserInvalidEncodingDecl);
        }
        self.locator.update_column(|c| c + 1);
        self.skip_whitespaces()?;

        let quote = match self.source.next_char()? {
            Some(c @ ('"' | '\'')) => c,
            _ => {
                fatal_error!(
                    self,
                    ParserInvalidEncodingDecl,
                    "The quotation marks in the encoding name are incorrect."
                );
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
                    self,
                    ParserInvalidEncodingDecl,
                    "The quotation marks in the encoding name are incorrect."
                );
                return Err(XMLError::ParserInvalidEncodingDecl);
            }
            _ => {
                // If we call `grow` just before and `source` is empty,
                // `source` has probably reached EOF.
                return if self.source.is_empty() {
                    Err(XMLError::ParserUnexpectedEOF)
                } else {
                    fatal_error!(
                        self,
                        ParserInvalidEncodingDecl,
                        "The quotation marks in the encoding name are incorrect."
                    );
                    Err(XMLError::ParserInvalidEncodingDecl)
                };
            }
        }

        Ok(encoding)
    }

    /// ```text
    /// [32] SDDecl ::= S 'standalone' Eq (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no') '"'))
    ///                                                 [VC: Standalone Document Declaration]
    /// ```
    pub(crate) fn parse_sddecl(&mut self, need_trim_whitespace: bool) -> Result<bool, XMLError> {
        if need_trim_whitespace && self.skip_whitespaces()? == 0 {
            fatal_error!(
                self,
                ParserInvalidSDDecl,
                "Whitespaces are required before 'standalone' for SDDecl."
            );
        }

        self.grow()?;
        let content = self.source.content_bytes();
        if !content.starts_with(b"standalone") {
            fatal_error!(
                self,
                ParserInvalidSDDecl,
                "'standalone' is not found for SDDecl."
            );
            return Err(XMLError::ParserInvalidSDDecl);
        }
        // skip 'standalone'
        self.source.advance(10)?;
        self.locator.update_column(|c| c + 10);

        self.skip_whitespaces()?;
        self.grow()?;
        if !self.source.content_bytes().starts_with(b"=") {
            fatal_error!(self, ParserInvalidSDDecl, "'=' is not found for SDDecl.");
            return Err(XMLError::ParserInvalidSDDecl);
        }
        // skip '='
        self.source.advance(1)?;
        self.locator.update_column(|c| c + 10);

        let quote = match self.source.next_char()? {
            Some(c @ ('"' | '\'')) => c,
            _ => {
                fatal_error!(
                    self,
                    ParserInvalidSDDecl,
                    "The quotation marks in the standalone declaration are incorrect."
                );
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
                    self,
                    ParserInvalidSDDecl,
                    "The value of SDDecl must be either 'yes' or 'no'."
                );
                return Err(XMLError::ParserInvalidXMLDecl);
            }
        };

        if self.source.next_char()? != Some(quote) {
            fatal_error!(
                self,
                ParserInvalidSDDecl,
                "The quotation marks in the standalone declaration are incorrect."
            );
            return Err(XMLError::ParserInvalidSDDecl);
        }
        self.locator.update_column(|c| c + 1);

        Ok(ret)
    }

    /// ```text
    /// [81] EncName ::= [A-Za-z] ([A-Za-z0-9._] | '-')*
    ///                                     /* Encoding name contains only Latin characters */
    /// ```
    pub(crate) fn parse_enc_name(&mut self) -> Result<String, XMLError> {
        self.grow()?;

        let content = self.source.content_bytes();
        if content.is_empty() {
            // If we call `grow` just before and `source` is empty,
            // `source` has probably reached EOF.
            return if self.source.is_empty() {
                Err(XMLError::ParserUnexpectedEOF)
            } else {
                fatal_error!(
                    self,
                    ParserInvalidEncodingName,
                    "Data that may not be accepted as an encoding name has been detected."
                );
                Err(XMLError::ParserInvalidEncodingName)
            };
        }

        if !content[0].is_ascii_alphabetic() {
            fatal_error!(
                self,
                ParserInvalidEncodingName,
                "The first character of an encoding name must be ASCII alphabetic."
            );
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
                self,
                PraserTooLongEncodingName,
                "Too long encoding name is found."
            );
            return Err(XMLError::PraserTooLongEncodingName);
        } else if cur == content.len() {
            return if self.source.is_empty() {
                Err(XMLError::ParserUnexpectedEOF)
            } else {
                fatal_error!(
                    self,
                    ParserInvalidEncodingName,
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

    /// ```text
    /// [77] TextDecl ::= '<?xml' VersionInfo? EncodingDecl S? '?>'
    /// ```
    pub(crate) fn parse_text_decl(&mut self) -> Result<(), XMLError> {
        self.grow()?;
        if !self.source.content_bytes().starts_with(b"<?xml") {
            fatal_error!(
                self,
                ParserInvalidTextDecl,
                "The text declaration does not start '<?xml'."
            );
            return Err(XMLError::ParserInvalidTextDecl);
        }
        // skip '<?xml'
        self.source.advance(5)?;
        self.locator.update_column(|c| c + 5);

        // parse VersionInfo if exists
        let mut s = self.skip_whitespaces()?;
        if self.source.content_bytes().starts_with(b"version") {
            if s == 0 {
                fatal_error!(
                    self,
                    ParserInvalidTextDecl,
                    "Whitespaces are required before 'encoding'."
                );
            }
            let (version, _) = self.parse_version_info(false, true)?;
            self.version = version;
            s = self.skip_whitespaces()?;
        }

        // parse EncodingDecl
        self.grow()?;
        if s == 0 {
            fatal_error!(
                self,
                ParserInvalidXMLDecl,
                "Whitespaces are required before 'encoding'."
            );
        }
        let encoding = self.parse_encoding_decl(false)?;
        if self.source.switch_encoding(&encoding).is_err() {
            fatal_error!(
                self,
                ParserUnsupportedEncoding,
                "Switching encoding to '{}' is failed.",
                encoding
            );
            return Err(XMLError::ParserUnsupportedEncoding);
        }
        self.encoding = Some(encoding);
        self.skip_whitespaces()?;
        self.grow()?;

        if !self.source.content_bytes().starts_with(b"?>") {
            fatal_error!(
                self,
                ParserInvalidTextDecl,
                "The text declaration does not end with '?>'."
            );
            return Err(XMLError::ParserInvalidTextDecl);
        }
        // skip '?>'
        self.source.advance(2)?;
        self.locator.update_column(|c| c + 2);

        Ok(())
    }
}
