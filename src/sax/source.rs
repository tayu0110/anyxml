use std::{io::Read, sync::atomic::AtomicUsize};

use crate::{
    encoding::{
        DecodeError, Decoder, UCS4Unusual2143Decoder, UCS4Unusual3412Decoder, UTF8Decoder,
        UTF16BEDecoder, UTF16LEDecoder, UTF32BEDecoder, UTF32LEDecoder, find_decoder,
    },
    error::XMLError,
    uri::{URIStr, URIString},
};

pub(crate) const INPUT_CHUNK: usize = 4096;
const GROW_THRESHOLD: usize = 64;

static SOURCE_ID: AtomicUsize = AtomicUsize::new(0);

pub struct InputSource<'a> {
    source: Box<dyn Read + 'a>,
    buffer: Vec<u8>,
    decoder: Box<dyn Decoder>,
    decoded: String,
    /// Start position of the undecoded range of `buffer`
    buffer_next: usize,
    /// End position of data read into `buffer`
    buffer_end: usize,
    /// Start position of unused data in `decoded`
    decoded_next: usize,
    /// Total number of bytes read from `source`
    total_read: usize,
    /// Whether `source` has reached EOF
    eof: bool,
    /// If `true`, keep `buffer` within the size specified by `INPUT_CHUNK` automatically.  \
    /// If `false`, the read byte sequence in `buffer` is retained.  
    ///
    /// Basically, this should be set to `true`, but if parsing is started with an unknown encoding,
    /// it is necessary to re-decode all byte sequences later, so it should be set to `false`.
    compact: bool,
    // If `true`, enable `push_bytes` and disable `grow`,
    // If `false`, enable `grow` and disable `push_bytes`.
    progressive: bool,
    /// A unique identifier for the source.
    ///
    /// To determine whether markup spans different sources,
    /// even sources created from the same file or character data are assigned different IDs.
    source_id: usize,

    system_id: Option<Box<URIStr>>,
    public_id: Option<Box<str>>,
}

impl<'a> InputSource<'a> {
    /// Construct an [`InputSource`] from the XML document resource `reader`.
    ///
    /// If the resource encoding is known, it can be specified using `encoding`.
    /// If not specified, the encoding is automatically inferred using the characteristics
    /// of the XML document.
    pub fn from_reader(reader: impl Read + 'a, encoding: Option<&str>) -> Result<Self, XMLError> {
        let mut ret = Self::default();
        ret.buffer.resize(INPUT_CHUNK, 0);
        ret.decoded
            .reserve(INPUT_CHUNK.saturating_sub(ret.decoded.capacity()));
        ret.source = Box::new(reader);

        if let Some(encoding) = encoding {
            ret.decoder = find_decoder(encoding).ok_or(XMLError::ParserUnsupportedEncoding)?;
            ret.buffer.shrink_to_fit();
            ret.compact = true;
        } else {
            // Handling strange implementations that write only one byte per read
            for _ in 0..INPUT_CHUNK {
                let read = ret.source.read(&mut ret.buffer[ret.buffer_end..])?;
                ret.buffer_end += read;
                ret.total_read += read;
                if read == 0 || ret.buffer_end == INPUT_CHUNK {
                    break;
                }
            }
            if ret.buffer_end < 4 {
                // The minimum byte count for well-formed XML is 4 bytes
                // (a document containing only an empty tag with a length of 1),
                // so if the number of bytes read is less than 4 bytes,
                // encoding detection is not possible.
                return Ok(ret);
            }

            ret.detect_encoding()?;
        }
        Ok(ret)
    }

    /// Construct an [`InputSource`] from the XML document resource `xml`.
    ///
    /// Resource encoding is always treated as UTF-8, and any encoding specified
    /// in the XML declaration is ignored.
    pub fn from_content(xml: &str) -> Self {
        Self {
            source: Box::new(std::io::empty()),
            buffer: vec![],
            decoder: Box::new(UTF8Decoder),
            decoded: xml.to_owned(),
            buffer_next: 0,
            buffer_end: 0,
            decoded_next: 0,
            total_read: xml.len(),
            eof: true,
            compact: true,
            progressive: false,
            source_id: SOURCE_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed),
            system_id: None,
            public_id: None,
        }
    }

    pub(crate) fn grow(&mut self) -> Result<(), XMLError> {
        if self.progressive {
            // ignore if this source is progressive mode.
            return Ok(());
        }

        if !self.eof {
            let rem = self.buffer_end - self.buffer_next;
            if rem < GROW_THRESHOLD {
                if self.compact {
                    // If compact mode, copy the remaining bytes to the top and leave a space.
                    self.buffer
                        .copy_within(self.buffer_next..self.buffer_end, 0);
                    self.buffer_next = 0;
                    self.buffer_end = rem;
                    if self.buffer.len() > INPUT_CHUNK {
                        debug_assert!(rem <= INPUT_CHUNK);
                        self.buffer.truncate(INPUT_CHUNK);
                        self.buffer.shrink_to_fit();
                    }
                } else {
                    self.buffer.resize(self.buffer.len() + INPUT_CHUNK, 0);
                }
                let mut read = 1;
                while self.buffer_end < self.buffer.len() && read != 0 {
                    read = self.source.read(&mut self.buffer[self.buffer_end..])?;
                    self.buffer_end += read;
                    self.total_read += read;
                }
                self.eof = read == 0;
            }
        }

        let rem = self.buffer_end - self.buffer_next;
        if rem > 0 {
            let cap = self.decoded.len() - self.decoded_next;
            if cap < GROW_THRESHOLD {
                if self.compact {
                    self.decoded.drain(..self.decoded_next);
                    self.decoded.shrink_to(INPUT_CHUNK);
                    self.decoded_next = 0;
                } else {
                    self.decoded.reserve_exact(INPUT_CHUNK);
                }
            }
            if self.decoded.capacity() - self.decoded.len() > GROW_THRESHOLD {
                match self.decoder.decode(
                    &self.buffer[self.buffer_next..self.buffer_end],
                    &mut self.decoded,
                    self.eof,
                ) {
                    Ok((read, _)) => {
                        self.buffer_next += read;
                    }
                    Err(e) => match e {
                        DecodeError::Malformed {
                            read,
                            write: _,
                            length,
                            offset,
                        } => {
                            let actual_read = read - offset - length;
                            // Since it may not be possible to set the decoder appropriately
                            // from the BOM or external encoding, no error is returned as long as
                            // some data can be decoded.
                            if actual_read > 0 {
                                self.buffer_next += actual_read;
                            } else {
                                return Err(From::from(e));
                            }
                        }
                        _ => return Err(From::from(e)),
                    },
                }
            }
        }
        Ok(())
    }

    pub fn content_bytes(&self) -> &[u8] {
        &self.decoded.as_bytes()[self.decoded_next..]
    }

    pub fn content_str(&self) -> &str {
        &self.decoded[self.decoded_next..]
    }

    pub fn total_read(&self) -> usize {
        self.total_read
    }

    pub(crate) fn next_char(&mut self) -> Result<Option<char>, XMLError> {
        Ok(self
            .peek_char()?
            .inspect(|c| self.decoded_next += c.len_utf8()))
    }

    pub(crate) fn next_char_if(
        &mut self,
        f: impl Fn(char) -> bool,
    ) -> Result<Option<char>, XMLError> {
        Ok(self
            .peek_char()?
            .filter(|c| f(*c))
            .inspect(|c| self.decoded_next += c.len_utf8()))
    }

    pub(crate) fn peek_char(&mut self) -> Result<Option<char>, XMLError> {
        if let Some(c) = self.decoded[self.decoded_next..].chars().next() {
            return Ok(Some(c));
        }
        self.grow()?;
        Ok(self.decoded[self.decoded_next..].chars().next())
    }

    pub(crate) fn advance(&mut self, mut len: usize) -> Result<(), XMLError> {
        while len > 0 {
            let l = len.min(self.decoded.len() - self.decoded_next);
            assert!(l > 0);
            assert!(self.decoded.is_char_boundary(self.decoded_next + l));
            self.decoded_next += l;
            len -= l;
            if self.decoded.len() - self.decoded_next == 0 {
                self.grow()?;
            }
        }
        Ok(())
    }

    /// Returns `true` if both the decoded but unused string
    /// and the read but undecoded data are 0 bytes.
    ///
    /// # Note
    /// Returning `true` does not mean that EOF has been reached.  
    /// If all of the read data has been decoded and you continue to consume the decoded strings
    /// without explicitly calling `grow`, this function may return `true` before reaching EOF.
    pub fn is_empty(&self) -> bool {
        self.decoded.len() - self.decoded_next == 0 && self.buffer_end - self.buffer_next == 0
    }

    /// The encoding name of the decoder currently in use.
    pub fn encoding_name(&self) -> &'static str {
        self.decoder.name()
    }

    /// A unique identifier for the source.
    ///
    /// To determine whether markup spans different sources,
    /// even sources created from the same file or character data are assigned different IDs.
    pub fn source_id(&self) -> usize {
        self.source_id
    }

    fn detect_encoding(&mut self) -> Result<(), XMLError> {
        match self.buffer[..4] {
            // Cases where BOM was found:
            // UCS-4, big-endian machine (1234 order)
            [0x00, 0x00, 0xFE, 0xFF] => {
                self.buffer_next = 4;
                self.decoder = Box::new(UTF32BEDecoder);
            }
            // UCS-4, little-endian machine (4321 order)
            [0xFF, 0xFE, 0x00, 0x00] => {
                self.buffer_next = 4;
                self.decoder = Box::new(UTF32LEDecoder);
            }
            // UCS-4, unusual octet order (2143)
            [0x00, 0x00, 0xFF, 0xFE] => {
                self.buffer_next = 4;
                self.decoder = Box::new(UCS4Unusual2143Decoder);
            }
            // UCS-4, unusual octet order (3412)
            [0xFE, 0xFF, 0x00, 0x00] => {
                self.buffer_next = 4;
                self.decoder = Box::new(UCS4Unusual3412Decoder);
            }
            // UTF-16, big-endian
            [0xFE, 0xFF, ..] => {
                self.buffer_next = 2;
                self.decoder = Box::new(UTF16BEDecoder);
            }
            // UTF-16, little-endian
            [0xFF, 0xFE, ..] => {
                self.buffer_next = 2;
                self.decoder = Box::new(UTF16LEDecoder);
            }
            // UTF-8
            [0xEF, 0xBB, 0xBF, ..] => {
                self.buffer_next = 3;
                self.decoder = Box::new(UTF8Decoder);
            }
            // Cases where BOM was not found:
            // UCS-4 or other 32-bit encoding, big-endian machine (1234 order)
            [0x00, 0x00, 0x00, 0x3C] => self.decoder = Box::new(UTF32BEDecoder),
            // UCS-4 or other 32-bit encoding, little-endian machine (4321 order)
            [0x3C, 0x00, 0x00, 0x00] => self.decoder = Box::new(UTF32LEDecoder),
            // UCS-4 or other 32-bit encoding, unusual octet order (2143)
            [0x00, 0x00, 0x3C, 0x00] => self.decoder = Box::new(UCS4Unusual2143Decoder),
            // UCS-4 or other 32-bit encoding, unusual octet order (3412)
            [0x00, 0x3C, 0x00, 0x00] => self.decoder = Box::new(UCS4Unusual3412Decoder),
            // UTF-16BE or big-endian ISO-10646-UCS-2 or other encoding  with a 16-bit
            // code unit in big-endian order and ASCII characters encoded as ASCII values
            // (the encoding declaration must be read to determine which)
            [0x00, 0x3C, 0x00, 0x3F] => self.decoder = Box::new(UTF16BEDecoder),
            // UTF-16LE or little-endian ISO-10646-UCS-2 or other encoding with a 16-bit
            // code unit in little-endian order and ASCII characters encoded as ASCII values
            // (the encoding declaration must be read to determine which)
            [0x3C, 0x00, 0x3F, 0x00] => self.decoder = Box::new(UTF16LEDecoder),
            // UTF-8, ISO 646, ASCII, some part of ISO 8859, Shift-JIS, EUC, or any other 7-bit,
            // 8-bit, or mixed-width encoding which ensures that the characters of ASCII have
            // their normal positions, width, and values; the actual encoding declaration must
            // be read to detect which of these applies, but since all of these encodings use
            // the same bit patterns for the relevant ASCII characters, the encoding declaration
            // itself may be read reliably
            [0x3C, 0x3F, 0x78, 0x6D] => self.decoder = Box::new(UTF8Decoder),
            // EBCDIC (in some flavor; the full encoding declaration must be read to tell
            // which code page is in use)
            [0x4C, 0x6F, 0xA7, 0x94] => return Err(XMLError::ParserUnsupportedEncoding),
            // cannot detect the specific encoding from the head of the content.
            // In this case, it is assumed that it is a UTF-8 document without an XML declaration.
            _ => {
                // Since it is either UTF-8 or an unknown encoding, the encoding is considered
                // to be fixed,  and the buffer control mode is also fixed.
                self.compact = true;
                self.decoder = Box::new(UTF8Decoder);
            }
        };
        Ok(())
    }

    pub(crate) fn switch_encoding(&mut self, to: &str) -> Result<(), XMLError> {
        // If compact mode, there may be data that has already been discarded,
        // so this is considered an error.
        if self.compact {
            return Err(XMLError::InternalError);
        }
        self.decoder = find_decoder(to).ok_or(XMLError::ParserUnsupportedEncoding)?;
        self.decoded.clear();
        self.buffer_next = 0;
        if self.decoded.capacity() < 4 {
            self.decoded.reserve(INPUT_CHUNK);
        }
        while self.buffer_next < self.buffer_end {
            match self.decoder.decode(
                &self.buffer[self.buffer_next..self.buffer_end],
                &mut self.decoded,
                false,
            ) {
                Ok((read, write)) => {
                    if read == 0 && write == 0 {
                        break;
                    }
                    self.buffer_next += read;
                    if write <= self.decoded_next {
                        self.decoded_next -= write;
                        self.decoded.clear();
                    } else if self.decoded_next > 0 {
                        self.decoded.drain(..self.decoded_next);
                        self.decoded_next = 0;
                    }
                }
                Err(err) => match err {
                    e @ DecodeError::Malformed {
                        read,
                        write,
                        length,
                        offset,
                    } => {
                        let actual_read = read - length - offset;
                        self.buffer_next += actual_read;
                        if write <= self.decoded_next {
                            self.decoded_next -= write;
                            self.decoded.clear();
                        } else if self.decoded_next > 0 {
                            self.decoded.drain(..self.decoded_next);
                            self.decoded_next = 0;
                        }
                        if !self.eof && (actual_read > 0 || !self.decoded.is_empty()) {
                            break;
                        }
                        return Err(From::from(e));
                    }
                    e => return Err(From::from(e)),
                },
            }
            if self.decoded.capacity() - self.decoded.len() < 4 {
                self.decoded.reserve(INPUT_CHUNK);
            }
        }

        if self.decoded_next > self.decoded.len() {
            return Err(XMLError::DecoderUnknownError);
        }

        self.decoded.shrink_to(INPUT_CHUNK);
        self.buffer
            .copy_within(self.buffer_next..self.buffer_end, 0);
        self.buffer_end -= self.buffer_next;
        if self.progressive {
            self.buffer.truncate(self.buffer_end);
        } else {
            self.buffer.truncate(self.buffer_end.max(INPUT_CHUNK));
        }
        self.buffer_next = 0;
        self.buffer.shrink_to(INPUT_CHUNK);

        // I don't think it's necessary to change the encoding twice,
        // so it should be fine to switch to compact mode...
        self.compact = true;
        Ok(())
    }

    /// Change buffer control to compact mode.  \
    /// If it was already in compact mode, nothing will happen.
    pub(crate) fn set_compact_mode(&mut self) {
        self.compact = true;
    }

    /// System identifier of the this source.
    pub fn system_id(&self) -> Option<&URIStr> {
        self.system_id.as_deref()
    }

    /// Public identifier of the this source.
    pub fn public_id(&self) -> Option<&str> {
        self.public_id.as_deref()
    }

    /// Set `system_id` as system identifier of this source.
    pub fn set_system_id(&mut self, system_id: impl Into<URIString>) {
        self.system_id = Some(system_id.into().into());
    }

    /// Set `public_id` as public identifier of this source.
    pub fn set_public_id(&mut self, public_id: impl Into<String>) {
        self.public_id = Some(public_id.into().into_boxed_str());
    }

    /// For progressive parser only
    pub(crate) fn push_bytes(
        &mut self,
        bytes: impl AsRef<[u8]>,
        finish: bool,
    ) -> Result<(), XMLError> {
        assert!(
            self.progressive,
            "system_id: {}",
            self.system_id().map_or("None", |uri| uri.as_escaped_str())
        );
        let bytes = bytes.as_ref();
        if bytes.is_empty() {
            return Ok(());
        }
        let init = self.total_read < 4;
        self.buffer.extend(bytes);
        self.buffer_end += bytes.len();
        self.total_read += bytes.len();
        if init {
            if self.total_read >= 4 {
                self.detect_encoding()?;
            } else {
                return Ok(());
            }
        }

        if self.compact && self.decoded_next * 2 >= self.decoded.len() {
            self.decoded.drain(..self.decoded_next);
            self.decoded_next = 0;
        }

        while self.buffer_next < self.buffer_end {
            if self.decoded.capacity() - self.decoded.len()
                < (self.buffer_end - self.buffer_next).max(4)
            {
                let additional = (self.decoded.capacity() * 2 - self.decoded.len()).max(4);
                self.decoded.reserve(additional);
            }
            match self.decoder.decode(
                &self.buffer[self.buffer_next..self.buffer_end],
                &mut self.decoded,
                finish,
            ) {
                Ok((read, _)) => {
                    if read == 0 {
                        break;
                    }
                    self.buffer_next += read;
                }
                Err(err) => match err {
                    err @ DecodeError::Malformed {
                        read,
                        length,
                        offset,
                        ..
                    } => {
                        let actual_read = read - length - offset;
                        if !finish || actual_read > 0 {
                            self.buffer_next += actual_read;
                            break;
                        }
                        return Err(From::from(err));
                    }
                    err => return Err(From::from(err)),
                },
            }
        }

        if self.compact && self.buffer_next * 2 > self.buffer_end {
            self.buffer.drain(..self.buffer_next);
            self.buffer_end -= self.buffer_next;
            self.buffer_next = 0;
        }
        Ok(())
    }

    pub(crate) fn set_progressive_mode(&mut self) {
        self.progressive = true;
    }

    /// For StAX reader
    pub(crate) fn decompose(mut self) -> (Box<dyn Read + 'a>, Vec<u8>) {
        assert_eq!(self.total_read, self.buffer_end);
        self.buffer.truncate(self.buffer_end);
        (self.source, self.buffer)
    }
}

impl Default for InputSource<'_> {
    fn default() -> Self {
        Self {
            source: Box::new(std::io::empty()),
            buffer: vec![],
            decoder: Box::new(UTF8Decoder),
            decoded: String::new(),
            buffer_next: 0,
            buffer_end: 0,
            decoded_next: 0,
            total_read: 0,
            eof: false,
            compact: false,
            progressive: false,
            source_id: SOURCE_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed),
            system_id: None,
            public_id: None,
        }
    }
}
